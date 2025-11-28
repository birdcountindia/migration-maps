# get species of interest -----------------------------------------------------------

get_spec_mig <- function() {
  require(readxl)
  read_xlsx("data/species_mapping.xlsx", sheet = 1)
}

get_spec_photo <- function() {
  require(readxl)
  read_xlsx("data/species_mapping.xlsx", sheet = 2)
}


# world basemap ---------------------------------------------------------------------

gg_world <- function(theme = "default") {
  
  require(rnaturalearth)
  require(rnaturalearthdata)
  require(ggplot2)
  require(sf)
  require(rmapshaper) # to simplify sf
  
  dir_prefix <- "../india-maps/"
  load(glue("{dir_prefix}outputs/maps_sf.RData"))
  
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

calc_repfreq_IN <- function(data, species) {
  
  # filter data to only those grids and seasons where species reported
  data_mig <- data %>%
    filter(COMMON.NAME == species) %>%
    # # this is the only step where mig status makes any difference
    # # and no harm in using SEASON for all species anyway
    # { if (mig_status %in% c("S","W","P")){
    #   distinct(., GRID.G3) 
    # } else if (mig_status == "LM") {
      distinct(., GRID.G3, SEASON) %>% 
    # } else {
    #   .
    # }} %>% 
    left_join(data, by = c("GRID.G3", "SEASON"))
  
  
  # daily freq
  totdays <- tibble(DAY.Y = 1:365)
  
  data_freq_day <- map_df(totdays$DAY.Y, ~ {
    
    # filtering data for day of interest
    data_day <- data_mig %>% filter(DAY.Y == .x)
    
    # calculate species-detected and total checklists for that day
    temp1 <- data_day %>% 
      filter(COMMON.NAME == species) %>% 
      reframe(DET = n_distinct(GROUP.ID))

    temp2 <- data_day %>% 
      reframe(LISTS = n_distinct(GROUP.ID)) %>% 
      # denominator cannot be 0
      mutate(LISTS = case_when(LISTS == 0 ~ 1,
                               TRUE ~ LISTS))

    to_return <- tibble(DAY.Y = .x) %>% 
      bind_cols(temp1, temp2) %>% 
      rename(NUMBER = DAY.Y) %>% 
      mutate(REP.FREQ = 100*DET/LISTS,
             PERIOD = "DAY.Y")
    
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
      reframe(DET = n_distinct(GROUP.ID))
    
    temp2 <- data_fort %>% 
      reframe(LISTS = n_distinct(GROUP.ID)) %>% 
      # denominator cannot be 0
      mutate(LISTS = case_when(LISTS == 0 ~ 1,
                                  TRUE ~ LISTS))
    
    to_return <- tibble(FORT.Y = .x) %>% 
      bind_cols(temp1, temp2) %>% 
      rename(NUMBER = FORT.Y) %>% 
      mutate(REP.FREQ = 100*DET/LISTS,
             PERIOD = "FORT.Y")
    
    return(to_return)
    
  })
  
  
  # return both
  return(bind_rows(data_freq_day, data_freq_fort) %>% 
           mutate(SPECIES = species) %>% 
           relocate(SPECIES, PERIOD, NUMBER))

}


# inset for rep freq ------------------------------------------------------

gg_repfreq <- function(species1, species2) {
  
  if (!exists("data_IN", envir = .GlobalEnv)) {
    error("India data is required for calculating repfeq!")
  }
  
  data_repfreq <- if (is.null(species2)) {
    calc_repfreq_IN(data_IN, species1)
  } else {
    calc_repfreq_IN(data_IN, species1) %>% 
      bind_rows(calc_repfreq_IN(data_IN, species2))
  }
  
  species <- if (is.null(species2)) species1 else c(species1, species2)
    
    
  # linking fortnights to months of year
  time_map <- data_repfreq %>% 
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
  
  data_cur <- data_repfreq %>% 
    filter(SPECIES %in% species,
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
  
  
  plot <- ggplot() +
    geom_line(data = smoothed_data, inherit.aes = FALSE,
              aes(x = MONTH.SCALED.STATIC, y = REP.FREQ, colour = SPECIES), linewidth = 1.5) +
    geom_vline(data = data_cur,
               aes(xintercept = MONTH.SCALED, group = MONTH.SCALED)) +
    labs(title = glue("Frequency in India (max.{round(max(data_cur$REP.FREQ))}%)")) +
    scale_x_continuous(breaks = data_cur$MONTH, labels = data_cur$MONTH.LAB) +
    # colour scale different if two species 
    scale_colour_manual(values = if (is.null(spec2)) "black" else c(plot_col1, plot_col2)) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.background = element_rect(fill = "white", colour = "black"),
          axis.text.x = element_text(face = "plain", size = 14),
          legend.position = "none") +
    transition_time(MONTH.SCALED) +
    ease_aes("linear") 
  
  return(plot)
  
}


# create animated migration maps ----------------------------------------------------

# species names in eBird format

gg_migrate <- function(
    spec1, spec2 = NULL, 
    plot_min_long = -15, plot_min_lat = -33, 
    plot_max_long = 180, plot_max_lat = 70,
    pos_im, pos_gr
) {
  # catches ---------------------------------------------------------------------------
  
  if (!exists("data_IN", envir = .GlobalEnv)) {
    error("India data is required for calculating reporting frequencies!")
  }
  
  if (!exists("data_spec", envir = .GlobalEnv)) {
    error("Object containing single-species eBird data is required!")
  }
  
  if (!exists("basemap", envir = .GlobalEnv)) {
    error("World basemap does not exist!")
  }
  
  # setup -----------------------------------------------------------------------------
  
  require(tidyverse)
  require(sf)
  require(magick)
  # require(gridExtra)
  # require(grid)
  require(extrafont)
  require(gganimate)
  require(gifski) # faster renderer
  require(transformr) # required to tween sf layers
  require(patchwork)
  
  # load maps  
  dir_prefix <- "../india-maps/"
  load(glue("{dir_prefix}outputs/maps_sf.RData"))
  
  
  # default plotting settings unlikely to be changed
  
  plot_res <- 150
  plot_range <- 30  #time window 
  plot_step <- 10 # increment value # 3/5
  plot_fps <- 12
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
  
  
  # # font for plot
  # windowsFonts("Gill Sans" = windowsFont("Gill Sans"))
  
  # load current species data -----------------------------------------------------------------
  
  # import and filter all occurrence data for current species
  
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
            aes(group = DAY.Y),
            colour = c(plot_col1), # different if more than two species
            alpha = 0.5, stroke = 0, size = 4) +
    # need to set coord limits (plot zoom limits)
    coord_sf(xlim = c(plot_lims$xmin, plot_lims$xmax), 
             ylim = c(plot_lims$ymin, plot_lims$ymax)) +
    theme(legend.position = "none") +
    transition_time(FORT.Y) + #change the transition here as per render requirements 
    ease_aes("linear") +
    exit_disappear() +
    shadow_wake(0.2, alpha = FALSE, size = NULL, falloff = 'sine-in')
  
  anim_save("outputs/test.gif", plot_base,
            # pass to animate()
            duration = 12, # chose based on old maps, but makes sense (12 months)
            fps = plot_fps,
            res = plot_res, renderer = gifski_renderer(), 
            width = 10.5, height = 7, units = "in")
  
  
  # 2. repfreq spline (if applicable) -------------------------------------------------
  
  # repfreq inset not required for pelagic species
  
  plot_inset <- if (is.null(spec2)) {
    gg_repfreq(species1 = spec1, species2 = NULL)
  } else {
    gg_repfreq(species1 = spec1, species2 = spec2)
  }
  
  # 3. other overlays -----------------------------------------------------------------
  
  # empty image 
  img = image_graph(width = 1080, height = 810, res = plot_res)
  #datalist = split(data, data$fort)
  
  
  # other images to use in plot
  plot_mugshot <- image_read(paste0("data/mugshots/", spec1_photo_info$PHOTO.FILE)) %>% 
    image_scale("300") %>% 
    image_border("#ffffff", "3x3") %>% 
    image_annotate(spec1_photo_info$PHOTO.CREDIT, 
                   font = "Gill Sans", size = 24, location = "+8+4", 
                   color = plot_cred1_col)
  
  plot_logo_bci <- image_read("birdcountindia_logo.png") %>% 
    image_scale("300") %>% 
    image_background("#ffffff", flatten = FALSE)
  
  plot_logo_ebirdindia <- image_read("eBird_India_Logo.png") %>% 
    image_scale("300") %>% 
    image_background("#ffffff", flatten = FALSE)
  
  plot_namecard <- image_blank(width = 600, height = 100, color = "none") %>% 
    image_annotate(
      text = spec1,
      font = "Gill Sans",         
      size = 60,
      color = plot_col1,
      gravity = "center",
      strokecolor = plot_col1,    
      strokewidth = 2           
    ) #for single species only, modify accordingly for 2 species
  
  
  # Animate and Combine --------------------------------------------------------------------
  
  anim_inset <- animate(
    plot_inset,
    duration = 12,
    fps = plot_fps,
    res = plot_res,
    width = 600, height = 300,
    renderer = gifski_renderer ()
  )  
  
  anim_save("outputs/inset.gif", animation = anim_inset)
  
  map_gif <- image_read("outputs/test.gif")
  inset_gif <- image_read("outputs/inset.gif")
  
  stopifnot(length(map_gif) == length(inset_gif))
  
  map_frames   <- as.list(map_gif)
  inset_frames <- as.list(inset_gif)
  
  # Apply frame by frame
  out_frames <- map2(map_frames, inset_frames, ~ {
    image_composite(.x, image_scale(.y, "300"), offset = "+1225+785") %>%
      image_composite(image_scale(plot_mugshot, "275"), offset = "+20+20") %>%
      image_composite(image_scale(plot_logo_bci, "250"), offset = "+20+900") %>%
      image_composite(image_scale(plot_logo_ebirdindia, "150"), offset = "+275 +900") %>% 
      image_composite(image_scale(plot_namecard, "500"), gravity = "north")
    
  })
  
  # Recombine into an animated GIF
  map_gif <- image_join(out_frames)
  
  filepath <- paste0("outputs/", spec1, "_migration_map.gif")
  image_write(image_join(map_gif), path = filepath)
  
  }

