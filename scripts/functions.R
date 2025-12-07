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
  #require(rmapshaper) # to simplify sf
  
  dir_prefix <- "../india-maps/"
  load(glue("{dir_prefix}outputs/maps_sf.RData"))
  
  world_sf <- ne_countries(type = "countries", 
                           scale = "large", returnclass = "sf") %>% 
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
    #ms_simplify(keep = 0.01, keep_shapes = TRUE) %>% 
    # fill colours and line widths
    { if (theme == "default") {
      mutate(., FILL = case_when(NAME == "India" ~ "#ffffff",
                              TRUE ~ "#e5d8ca"))
    } else if (theme == "green") {
      mutate(., FILL = case_when(NAME == "India" ~ "#008000",
                              TRUE ~ "#004d00"))
    }} %>% 
    mutate(LINEWIDTH = case_when(NAME == "India" ~ 0.5,
                                 TRUE ~ 0.4))
  
  
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
