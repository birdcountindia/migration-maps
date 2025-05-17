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

