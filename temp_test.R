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
  transition_time(FORT.Y) +
  ease_aes("linear") +
  exit_disappear() +
  shadow_wake(0.2, alpha = FALSE, size = NULL, falloff = 'sine-in')

plot_inset <- gg_repfreq(species1 = spec1, species2 = spec2)


plot_base + inset_element(plot_inset, 0, 0, 0.3, 0.25, align_to = "full")



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
  exit_disappear() +
  shadow_wake(0.2, alpha = FALSE, size = NULL, falloff = 'sine-in')

