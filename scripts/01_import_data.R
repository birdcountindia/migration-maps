library(skimmr)
library(lubridate)
library(sf)

# loading main data -----------------------------------------------------

get_param()
dir_prefix <- "../ebird-datasets/EBD/"
maindatapath <- glue("{dir_prefix}ebd_IN_rel{currel_month_lab}-{currel_year}.RData")

load(maindatapath)


# SoIB main datasheet
soib2023 <- read.csv(url("https://github.com/stateofindiasbirds/soib_2023/raw/master/01_analyses_full/results/SoIB_main.csv"))


# preparing data --------------------------------------------------------------------

data <- data %>%
  # filter for only approved observations & species
  filter(REVIEWED == 0 | APPROVED == 1) %>%
  # slice by GROUP.ID to remove duplicate checklists
  group_by(GROUP.ID, COMMON.NAME) %>% 
  slice_sample(1) %>% 
  group_by(GROUP.ID) %>% 
  ungroup() %>%
  # add more columns
  mutate(DAY.Y = yday(OBSERVATION.DATE),
         WEEK.Y = week(OBSERVATION.DATE),
         FORT.Y = ceiling(WEEK/2),
         SEASON = case_when(MONTH %in% 5:8 ~ "Summer",
                            MONTH %in% 9:11 ~ "Autumn",
                            MONTH %in% c(12, 1:2) ~ "Winter",
                            MONTH %in% 3:4 ~ "Spring")) 


# joining spatial data --------------------------------------------------------------

dir_prefix <- "../india-maps/"
load(glue("{dir_prefix}outputs/maps_sf.RData"))
load(glue("{dir_prefix}outputs/grids_sf_full.RData"))

sf_use_s2(FALSE)

# to later filter pelagics
india_buff_sf <- india_buff_sf %>% mutate(INLAND = 1)


temp = data %>%
  distinct(GROUP.ID, LONGITUDE, LATITUDE) %>% 
  group_by(GROUP.ID) %>% 
  slice_sample(1) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), remove = F) %>% 
  st_set_crs(st_crs(india_sf)) %>%
  # grid cells
  st_join(g3_sf %>% dplyr::select(GRID.G3)) %>% 
  st_join(india_buff_sf %>% dplyr::select(INLAND)) %>% 
  st_drop_geometry()

temp = temp %>% 
  distinct(GROUP.ID, GRID.G3, INLAND) %>% 
  group_by(GROUP.ID) %>% 
  slice_sample(1) %>% 
  ungroup()

data = data %>% 
  left_join(temp, by = "GROUP.ID")

