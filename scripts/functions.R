# get species of interest -----------------------------------------------------------

get_spec_mig <- function() {
  require(readxl)
  read_xlsx("data/species_mapping.xlsx", sheet = 1)
}


# world basemap ---------------------------------------------------------------------

gg_world <- function(theme = "default") {
  
  require(rnaturalearth)
  require(rnaturalearthdata)
  require(ggplot2)
  require(sf)
  require(rmapshaper) # to simplify sf
  
  
  world_sf <- ne_countries(type = "countries", 
                           scale = "medium", returnclass = "sf") %>% 
    # selecting columns of interest
    reframe(NAME = sovereignt, 
            geometry = geometry) %>% 
    st_as_sf()
  
  # replacing default India with Indian India boundaries
  world_sf <- world_sf %>% 
    filter(NAME != "India") %>% 
    # since the row is at the end, will lay over other disputed territories
    bind_rows(india_sf %>% 
                dplyr::select(geometry) %>% 
                mutate(NAME = "India"))
  
  world_sf <- world_sf %>% 
    # converting to Robinson projection
    st_transform(crs = "ESRI:54030") %>% 
    # simplifying polygons
    ms_simplify(keep = 0.01, keep_shapes = TRUE) %>% 
    # fill colours and line widths
    { if (theme == "default") {
      mutate(., FILL = case_when(NAME == "India" ~ "#ffffff",
                              TRUE ~ "#e5d8ca"))
    } else if (theme == "green") {
      mutate(., FILL = case_when(NAME == "India" ~ "#008000",
                              TRUE ~ "#004d00"))
    }} %>% 
    mutate(LINEWIDTH = case_when(NAME == "India" ~ 0.65,
                                 TRUE ~ 0.5))
  
  
  world_gg <- ggplot(world_sf) +
    { if (theme == "default") {
      geom_sf(aes(fill = FILL, linewidth = LINEWIDTH))
    } else if (theme == "green") {
      geom_sf(aes(fill = FILL, linewidth = LINEWIDTH), colour = "black")
    }} +
    scale_fill_identity() +
    scale_linewidth_identity() + 
    theme_void() +
    theme(panel.background = element_rect(
      colour = NA, 
      fill = if (theme == "default") "#a9d5e0" else if (theme == "green") "#142952"
    ))
  
  return(world_gg)
  
}


# calculate reporting frequency in India as a whole ---------------------------------

# outputs list

calc_repfreq_IN <- function(data, species, mig_status) {
  
  # filter data to only those grids and seasons where species reported
  data_mig <- data %>%
    filter(COMMON.NAME == species) %>%
    { if (mig_status %in% c("S","W","P")){
      distinct(., GRID.G3) 
    } else if (mig_status == "LM") {
      distinct(., GRID.G3, SEASON)
    } else {
      .
    }} %>% 
    left_join(data)
  
  
  # daily freq
  totdays <- tibble(DAY.Y = 1:365)
  
  data_freq_day <- map_df(totdays$DAY.Y, ~ {
    
    # filtering data for day of interest
    data_day <- data_mig %>% filter(DAY.Y == .x)
    
    # calculate species-detected and total checklists for that day
    temp1 <- data_day %>% 
      filter(COMMON.NAME == species) %>% 
      reframe(DET.DAY = n_distinct(GROUP.ID))

    temp2 <- data_day %>% 
      reframe(LISTS.DAY = n_distinct(GROUP.ID)) %>% 
      # denominator cannot be 0
      mutate(LISTS.DAY = case_when(LISTS.DAY == 0 ~ 1,
                                  TRUE ~ LISTS.DAY))

    to_return <- tibble(DAY.Y = .x) %>% 
      bind_cols(temp1, temp2) %>% 
      mutate(REP.FREQ.DAY = 100*DET.DAY/LISTS.DAY)
    
    return(to_return)
    
  })
  
  
  # fortnightly freq
  totforts <- tibble(FORT.Y = 1:27)
  
  data_freq_fort <- map_df(totforts$FORT.Y, ~ {
    
    # filtering data for day of interest
    data_fort <- data_mig %>% filter(FORT.Y == .x)
    
    # calculate species-detected and total checklists for that day
    temp1 <- data_fort %>% 
      filter(COMMON.NAME == species) %>% 
      reframe(DET.FORT = n_distinct(GROUP.ID))
    
    temp2 <- data_fort %>% 
      reframe(LISTS.FORT = n_distinct(GROUP.ID)) %>% 
      # denominator cannot be 0
      mutate(LISTS.FORT = case_when(LISTS.FORT == 0 ~ 1,
                                  TRUE ~ LISTS.FORT))
    
    to_return <- tibble(FORT.Y = .x) %>% 
      bind_cols(temp1, temp2) %>% 
      mutate(REP.FREQ.FORT = 100*DET.FORT/LISTS.FORT)
    
    return(to_return)
    
  })
  
  
  # return both
  return(list(data_freq_day, data_freq_fort))

}




# create animated migration maps ----------------------------------------------------

# species names in eBird format

gg_migrate <- function(
    spec1, rawpath1, photopath1,
    spec2, rawpath2, photopath2,
    plot_world = FALSE, 
    plot_min_long = -15, plot_min_lat = -33, 
    plot_max_long = 180, plot_max_lat = 70,
    pos_im, pos_gr
    ) {
  
  # setup -----------------------------------------------------------------------------

  require(tidyverse)
  require(sf)
  require(magick)
  # require(gridExtra)
  # require(grid)
  require(extrafont)
  require(gganimate)
  require(transformr) # required to tween sf layers
  
  
  # default plotting settings unlikely to be changed
  
  plot_res <- 144
  plot_range <- 30
  plot_step <- 10
  plot_fps <- 2
  
  plot_col1 <- "#5e488a"
  plot_col2 <- "#449966"
  plot_cred1_col <- "black"
  plot_cred2_col <- "black"
  
  
  
  
  pointsize, yaxis,
  ggp,dataall,migstatus1,migstatus2,
  
  plot_cred1, plot_cred2,
  
  
  # output file name
  
  

  # load current data -----------------------------------------------------------------

  # import and filter data for current species
  
  
  
  if (n==1)
  {
    data = readcleanrawdata(rawpath = rawpath1)
  }
  
  if (n!=1)
  {
    data1 = readcleanrawdata(rawpath = rawpath1)
    data2 = readcleanrawdata(rawpath = rawpath2)
    data = rbind(data1,data2)
    data1 = data1 %>% select(COMMON.NAME)
    data2 = data2 %>% select(COMMON.NAME)
  }
  
  
  
  ###
  
  # spatialise species observation data
  data_cur <- data %>% 
    st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% 
    st_set_crs(st_crs(india_sf)) %>% 
    st_transform(crs = "ESRI:54030")
  

  # calculating reporting frequency (if applicable) -----------------------------------
  
  # below not needed in _S functions ###
  
  freq = create_freq(Species = Species1, data = dataall, migstatus = migstatus)
  
  freq1 = freq[[1]]
  freq4 = freq[[2]]
  freq1$checklists[freq1$checklists == 0] = 1
  freq4$checklists[freq4$checklists == 0] = 1
  freq1$perc = (freq1$detected/freq1$checklists)*100
  freq4$perc = (freq4$detected/freq4$checklists)*100
  mfort = rollmean(seq(0,365,14),2)
  freq4$day = c(mfort,mfort[1]+365)
  
  spl1 = smooth.spline(c(freq1$day,(freq1$day+365),(freq1$day+730)),rep(freq1$perc,3),nknots=30)
  spl4 = smooth.spline(c(freq4$day,(freq4$day+365),(freq4$day+730)),rep(freq4$perc,3),nknots=30)
  
  spl1a = predict(spl1,366:730)
  spl1a = as.data.frame(spl1a)
  spl1a$y[spl1a$y<0] = 0
  
  spl4a = predict(spl4,366:730)
  spl4a = as.data.frame(spl4a)
  spl4a$y[spl4a$y<0] = 0
  
  spl = spl4a
  spl$x = 1:365
  
  mx = max(na.omit(spl$y))
  yaxis = c(0,(mx+0.02))
  ybreaks = seq(0,yaxis[2],1)
  

  ### ###
  
    

  # 1. world base + points ------------------------------------------------------------

  
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
            res = 150, width = 10.5, height = 7, units = "in")
  

  # 2. repfreq spline (if applicable) -------------------------------------------------

 

  # 3. other overlays -----------------------------------------------------------------

  # empty image 
  img = image_graph(width = 1080, height = 810, res = plot_res)
  #datalist = split(data, data$fort)
  
  
  # other images to use in plot
  plot_mugshot <- image_read(photopath1) %>% 
    image_scale("300") %>% 
    image_border("#ffffff", "3x3") %>% 
    image_annotate(plot_cred1, 
                   font = "Gill Sans", size = 24, location = "+8+4", 
                   color = plot_cred1_col)
  
  plot_logo_bci <- image_read("birdcountindia logo.png") %>% 
    image_scale("300") %>% 
    image_background("#ffffff", flatten = TRUE)
  
  plot_logo_ebirdindia <- image_read("eBird India logo.png") %>% 
    image_scale("300") %>% 
    image_background("#ffffff", flatten = TRUE)
  

  # animate and export ----------------------------------------------------------------

  

}
