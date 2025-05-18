spec1 = "Amur Falcon"
spec2 = NULL
plot_min_long = -15
plot_min_lat = -33
plot_max_long = 180
plot_max_lat = 70


require(tidyverse)
require(sf)
require(magick)
# require(gridExtra)
# require(grid)
require(extrafont)
require(gganimate)
require(gifski) # faster renderer
require(transformr) # required to tween sf layers

# load maps  
dir_prefix <- "../india-maps/"
load(glue("{dir_prefix}outputs/maps_sf.RData"))


# default plotting settings unlikely to be changed

plot_res <- 150
plot_range <- 30
plot_step <- 10
plot_fps <- 10
plot_world <- FALSE

plot_yaxis <- c(-0.1, 1.2)

plot_col1 <- "#5e488a"
plot_col2 <- "#449966"
plot_cred1_col <- "black"
plot_cred2_col <- "black"

plot_pointsize <- 2.5 # if pelagic, 1.5


# photos for species

spec1_photo_info <- get_spec_photo() %>% filter(SPECIES == spec1)
if (!is.null(spec2)) {
  spec2_photo_info <- get_spec_photo() %>% filter(SPECIES == spec2)
}


data_cur <- if (is.null(spec2)) {
  data_spec %>% 
    filter(COMMON.NAME == spec1)
} else {
  data_spec %>% 
    filter(COMMON.NAME %in% c(spec1, spec2))
}


# spatialise species observation data
data_cur <- data_cur %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% 
  st_set_crs(st_crs(india_sf)) %>% 
  st_transform(crs = "ESRI:54030")


# plot limits 
plot_lims <- tibble(X = c(-30, -30, 145, 145),
                    Y = c(-35, 60, -35, 60)) %>% 
  st_as_sf(coords = c("X", "Y")) %>% 
  dplyr::summarise() %>% 
  st_cast("POLYGON") %>% 
  # important that limits are also projected to Robinson
  st_set_crs(st_crs(india_sf)) %>% 
  st_transform(crs = "ESRI:54030") %>% 
  st_bbox()

plot_base <- basemap +
  geom_sf(data = data_cur %>% 
            filter(YEAR > 2013) %>% 
            mutate(across(c("DAY.Y", "FORT.Y", "MONTH"),
                          ~ as.integer(.))), 
          aes(group = FORT.Y),
          alpha = 0.5, stroke = 0, size = 4, colour = plot_col1) +
  # need to set coord limits (plot zoom limits)
  coord_sf(xlim = c(plot_lims$xmin, plot_lims$xmax), 
           ylim = c(plot_lims$ymin, plot_lims$ymax)) +
  theme(legend.position = "none") +
  # gganimate code
  # ggtitle("{frame_time}") +
  transition_time(FORT.Y) +
  ease_aes("linear") +
  # enter_fade() +
  # exit_fade() 
  shadow_wake(0.1)

anim_save("outputs/test.gif", plot_base,
          # pass to animate()
          duration = 12, # chose based on old maps, but makes sense (12 months)
          # fps = plot_fps,
          res = plot_res, renderer = gifski_renderer(), 
          width = 10.5, height = 7, units = "in")


gg_repfreq(species1 = spec1, species2 = spec2) +
  transition_time(DAY.Y) +
  ease_aes("linear") +
  # enter_fade() +
  # exit_fade() 
  shadow_wake(0.1)





data_cur <- if (is.null(spec2)) {
  calc_repfreq_IN(data_IN, spec1)
} else {
  calc_repfreq_IN(data_IN, spec1) %>% 
    bind_rows(calc_repfreq_IN(data_IN, spec2))
} 

time_map <- data_cur %>% 
  filter(PERIOD == "DAY.Y") %>% # days are smoother
  distinct(NUMBER) %>% 
  rename(DAY.Y = NUMBER) %>% 
  left_join(data_IN %>% distinct(DAY.Y, MONTH), by = "DAY.Y") %>% 
  mutate(MONTH.LAB = month(MONTH, label = TRUE)) %>% 
  mutate(MONTH.LAB = str_sub(MONTH.LAB, start = 1, end = 1)) %>% 
  # only want one month for one fortnight
  group_by(DAY.Y) %>% 
  slice_sample(n = 1) %>% 
  ungroup()

data_cur <- data_cur %>% 
  filter(SPECIES %in% spec1,
         PERIOD == "DAY.Y") %>% 
  rename(DAY.Y = NUMBER) %>% 
  mutate(PERIOD = NULL) %>% 
  left_join(time_map, by = "DAY.Y") %>%
  # fractional scaling of day-month numbers
  group_by(MONTH) %>% 
  mutate(DAY.M = DAY.Y - (first(DAY.Y) - 1),
         MONTH.SCALED = (MONTH - 0.5) + (DAY.M - 1) / n_distinct(DAY.Y))

# precompute loess-smoothed line to remain static when vline animated
smoothed_data <- data_cur %>%
  group_by(SPECIES) %>%
  nest() %>%
  mutate(
    model = map(data, ~ loess(REP.FREQ ~ MONTH.SCALED, data = ., span = 0.4)),
    new_x = map(data, ~ seq(min(.$MONTH.SCALED), max(.$MONTH.SCALED), length.out = 200)),
    pred_y = map2(model, new_x, ~ predict(.x, newdata = data.frame(MONTH.SCALED = .y)))
  ) %>%
  unnest(c(new_x, pred_y)) %>%
  rename(MONTH.SCALED.STATIC = new_x, 
         REP.FREQ = pred_y) %>%
  select(SPECIES, MONTH.SCALED.STATIC, REP.FREQ)

ggplot() +
  geom_line(data = smoothed_data, inherit.aes = FALSE,
            aes(x = MONTH.SCALED.STATIC, y = REP.FREQ, colour = SPECIES), linewidth = 1.5) +
  geom_vline(data = data_cur,
             aes(xintercept = MONTH.SCALED, group = MONTH.SCALED)) +
  labs(title = glue("Frequency in India (max. {round(max(data_cur$REP.FREQ))}%)")) +
  scale_x_continuous(breaks = data_cur$MONTH, labels = data_cur$MONTH.LAB) +
  # colour scale different if two species 
  scale_colour_manual(values = if (is.null(spec2)) "black" else c(plot_col1, plot_col2)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face = "plain", size = 14),
        legend.position = "none") +
  transition_time(MONTH.SCALED) +
  ease_aes("linear") 




# checking fortnight ----------------------------------------------------------------

basemap +
  geom_sf(data = data_cur %>% 
            filter(YEAR > 2013, 
                   FORT.Y == 1) %>% 
            mutate(across(c("DAY.Y", "FORT.Y", "MONTH"),
                          ~ as.integer(.))), 
          alpha = 0.5, stroke = 0, size = 4, colour = plot_col1) +
  # need to set coord limits (plot zoom limits)
  coord_sf(xlim = c(plot_lims$xmin, plot_lims$xmax), 
           ylim = c(plot_lims$ymin, plot_lims$ymax))


basemap +
  geom_sf(data = data_cur %>% 
            filter(YEAR > 2013, 
                   FORT.Y == 2) %>% 
            mutate(across(c("DAY.Y", "FORT.Y", "MONTH"),
                          ~ as.integer(.))), 
          alpha = 0.5, stroke = 0, size = 4, colour = plot_col1) +
  # need to set coord limits (plot zoom limits)
  coord_sf(xlim = c(plot_lims$xmin, plot_lims$xmax), 
           ylim = c(plot_lims$ymin, plot_lims$ymax))

basemap +
  geom_sf(data = data_cur %>% 
            filter(YEAR > 2013, 
                   FORT.Y == 3) %>% 
            mutate(across(c("DAY.Y", "FORT.Y", "MONTH"),
                          ~ as.integer(.))), 
          alpha = 0.5, stroke = 0, size = 4, colour = plot_col1) +
  # need to set coord limits (plot zoom limits)
  coord_sf(xlim = c(plot_lims$xmin, plot_lims$xmax), 
           ylim = c(plot_lims$ymin, plot_lims$ymax))

basemap +
  geom_sf(data = data_cur %>% 
            filter(YEAR > 2013, 
                   FORT.Y == 4) %>% 
            mutate(across(c("DAY.Y", "FORT.Y", "MONTH"),
                          ~ as.integer(.))), 
          alpha = 0.5, stroke = 0, size = 4, colour = plot_col1) +
  # need to set coord limits (plot zoom limits)
  coord_sf(xlim = c(plot_lims$xmin, plot_lims$xmax), 
           ylim = c(plot_lims$ymin, plot_lims$ymax))

basemap +
  geom_sf(data = data_cur %>% 
            filter(YEAR > 2013, 
                   FORT.Y == 5) %>% 
            mutate(across(c("DAY.Y", "FORT.Y", "MONTH"),
                          ~ as.integer(.))), 
          alpha = 0.5, stroke = 0, size = 4, colour = plot_col1) +
  # need to set coord limits (plot zoom limits)
  coord_sf(xlim = c(plot_lims$xmin, plot_lims$xmax), 
           ylim = c(plot_lims$ymin, plot_lims$ymax))

basemap +
  geom_sf(data = data_cur %>% 
            filter(YEAR > 2013, 
                   FORT.Y == 6) %>% 
            mutate(across(c("DAY.Y", "FORT.Y", "MONTH"),
                          ~ as.integer(.))), 
          alpha = 0.5, stroke = 0, size = 4, colour = plot_col1) +
  # need to set coord limits (plot zoom limits)
  coord_sf(xlim = c(plot_lims$xmin, plot_lims$xmax), 
           ylim = c(plot_lims$ymin, plot_lims$ymax))

basemap +
  geom_sf(data = data_cur %>% 
            filter(YEAR > 2013, 
                   FORT.Y == 7) %>% 
            mutate(across(c("DAY.Y", "FORT.Y", "MONTH"),
                          ~ as.integer(.))), 
          alpha = 0.5, stroke = 0, size = 4, colour = plot_col1) +
  # need to set coord limits (plot zoom limits)
  coord_sf(xlim = c(plot_lims$xmin, plot_lims$xmax), 
           ylim = c(plot_lims$ymin, plot_lims$ymax))

basemap +
  geom_sf(data = data_cur %>% 
            filter(YEAR > 2013, 
                   FORT.Y == 8) %>% 
            mutate(across(c("DAY.Y", "FORT.Y", "MONTH"),
                          ~ as.integer(.))), 
          alpha = 0.5, stroke = 0, size = 4, colour = plot_col1) +
  # need to set coord limits (plot zoom limits)
  coord_sf(xlim = c(plot_lims$xmin, plot_lims$xmax), 
           ylim = c(plot_lims$ymin, plot_lims$ymax))

basemap +
  geom_sf(data = data_cur %>% 
            filter(YEAR > 2013, 
                   FORT.Y == 9) %>% 
            mutate(across(c("DAY.Y", "FORT.Y", "MONTH"),
                          ~ as.integer(.))), 
          alpha = 0.5, stroke = 0, size = 4, colour = plot_col1) +
  # need to set coord limits (plot zoom limits)
  coord_sf(xlim = c(plot_lims$xmin, plot_lims$xmax), 
           ylim = c(plot_lims$ymin, plot_lims$ymax))

basemap +
  geom_sf(data = data_cur %>% 
            filter(YEAR > 2013, 
                   FORT.Y == 10) %>% 
            mutate(across(c("DAY.Y", "FORT.Y", "MONTH"),
                          ~ as.integer(.))), 
          alpha = 0.5, stroke = 0, size = 4, colour = plot_col1) +
  # need to set coord limits (plot zoom limits)
  coord_sf(xlim = c(plot_lims$xmin, plot_lims$xmax), 
           ylim = c(plot_lims$ymin, plot_lims$ymax))

basemap +
  geom_sf(data = data_cur %>% 
            filter(YEAR > 2013, 
                   FORT.Y == 11) %>% 
            mutate(across(c("DAY.Y", "FORT.Y", "MONTH"),
                          ~ as.integer(.))), 
          alpha = 0.5, stroke = 0, size = 4, colour = plot_col1) +
  # need to set coord limits (plot zoom limits)
  coord_sf(xlim = c(plot_lims$xmin, plot_lims$xmax), 
           ylim = c(plot_lims$ymin, plot_lims$ymax))

# testing stuttering with dummy data ------------------------------------------------

# 10, 20 and 200 points all render in same time = 12 sec

data_test <- data.frame(NO = 1:200) %>% 
  group_by(NO) %>% 
  reframe(X = runif(10),
          Y = runif(10))

data_test %>% 
  ggplot(aes(x = X, y = Y)) +
  geom_point() +
  transition_time(NO) +
  ease_aes("linear") +
  shadow_wake(0.1, falloff = 'sine-in')

