library(skimmr)
library(lubridate)
library(sf)


# species of interest for maps
spec_mig <- c(
  "Amur Falcon", "Arctic Warbler", "Ashy Drongo", "Asian Brown Flycatcher", "Bar-headed Goose",
  "Black Baza", "Black Redstart", "Blue Rock-Thrush", "Blue-tailed Bee-eater", "Blyth's Reed Warbler",
  "Brown-breasted Flycatcher", "Brown-headed Gull", 
  "Cattle Egret", "Gray Nightjar", "Greater Whitethroat", ### CHANGE NAME
  "Chestnut-headed Bee-eater", "Chestnut-winged Cuckoo", "Collared Pratincole", "Oriental Pratincole",
  "Common Crane", "Common Cuckoo", "Common Greenshank", "Common Rosefinch", "Crab-Plover", 
  "Demoiselle Crane", "Desert Wheatear", "Pied Wheatear", "Eurasian Wryneck", "European Bee-eater",
  "European Roller", "Forest Wagtail", "Garganey", "Gray-headed Lapwing", 
  "Great Cormorant", "Great White Pelican", "Green Warbler", "Greenish Warbler",
  "Gull-billed Tern", "Hypocolius", "Indian Blue-Robin", "Indian Golden Oriole",
  "Indian Paradise Flycatcher", "Indian Pitta", "Isabelline Shrike", "Brown Shrike",
  "Isabelline Wheatear", "Kashmir Flycatcher", "Little Tern", "Northern Wheatear",
  "Oriental Turtle-Dove", "Pacific Golden Plover", "Pallas's Fish-Eagle", "Pied Cuckoo",
  "Red-breasted Flycatcher", "Red-flanked Bluetail", "Red-headed Bunting", "Rosy Starling",
  "Rufous-tailed Scrub-Robin", "Sanderling", "Short-eared Owl", "Spot-winged Starling",
  "Spotted Flycatcher", "Tytler's Leaf Warbler", "Whimbrel", "White-eyed Buzzard", 
  "White-throated Needletail", "Willow Warbler", "Wilson's Storm-Petrel", 
  "Yellow-browed Warbler", "Yellow-eyed Pigeon", 
  # new additions 2024
  "Rusty-tailed Flycatcher", "Pied Thrush"
)


# loading main data -----------------------------------------------------

# india data to calculate repfreq (we show repfreq in India in gifs)

# get_param()
get_param(date_currel = "2022-01-01") # ### for tests without server and full data ###
dir_prefix <- "data/EBD/" ### this will change: directly use RData from ebird-datasets/EBD/ ###
maindatapath <- glue("{dir_prefix}ebd_IN_rel{currel_month_lab}-{currel_year}.RData")

load(maindatapath)


# preparing data 
data <- data %>%
  ### TEMP FILTER 
  filter(YEAR %in% 2020:2021) %>% 
  # filter for only approved observations & species
  filter(REVIEWED == 0 | APPROVED == 1) %>%
  # slice by GROUP.ID to remove duplicate checklists
  distinct(GROUP.ID, COMMON.NAME, .keep_all = TRUE) %>% 
  group_by(GROUP.ID, COMMON.NAME) %>% 
  mutate(NO.SP = n_distinct(COMMON.NAME)) %>% 
  ungroup() %>%
  # add more columns
  mutate(DAY.Y = yday(OBSERVATION.DATE),
         WEEK.Y = week(OBSERVATION.DATE),
         FORT.Y = ceiling(WEEK.Y/2),
         SEASON = case_when(MONTH %in% 5:8 ~ "Summer",
                            MONTH %in% 9:11 ~ "Autumn",
                            MONTH %in% c(12, 1:2) ~ "Winter",
                            MONTH %in% 3:4 ~ "Spring")) 


# loading single-species global data -----------------------------------------------------

# we also need individual species-level global datafiles (for the points)

# dir_prefix <- glue("data/EBD/{currel_year}")
dir_prefix <- glue("data/EBD/2024")
# ideally, the only .txt files in this directory should be single-species data
# country data should be RData and should be pulled from ebird-datasets/EBD/

# cols to import
preimp <- c("CATEGORY","EXOTIC.CODE","COMMON.NAME","OBSERVATION.COUNT",
            "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","LAST.EDITED.DATE",
            "STATE","STATE.CODE","COUNTY","COUNTY.CODE",
            "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
            "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","BREEDING.CODE",
            "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER",
            "TRIP.COMMENTS","SPECIES.COMMENTS", "HAS.MEDIA")


data_spec <- map_df(list.files(path = dir_prefix, pattern = ".txt"), ~{
  
  filepath <- glue("{dir_prefix}/{.x}")
  read.ebd(path = filepath, cols_sel = preimp)
  
}) %>% 
  mutate(GROUP.ID = ifelse(is.na(GROUP.IDENTIFIER), 
                           SAMPLING.EVENT.IDENTIFIER,
                           GROUP.IDENTIFIER), 
         OBSERVATION.DATE = as_date(OBSERVATION.DATE), 
         YEAR = year(OBSERVATION.DATE), 
         MONTH = month(OBSERVATION.DATE),
         DAY.M = day(OBSERVATION.DATE)) %>% 
  # migratory year and month information
  mutate(M.YEAR = if_else(MONTH > 5, YEAR, YEAR-1), # from June to May
         M.MONTH = if_else(MONTH > 5, MONTH-5, 12-(5-MONTH))) 


# loading other data -----------------------------------------------------

# SoIB main datasheet
soib <- read.csv(url("https://github.com/stateofindiasbirds/soib_2023/raw/master/01_analyses_full/results/SoIB_main.csv"))
### this will eventually come directly as data object in skimmr


# joining spatial data --------------------------------------------------------------

dir_prefix <- "../india-maps/"
load(glue("{dir_prefix}outputs/maps_sf.RData"))
load(glue("{dir_prefix}outputs/grids_sf_full.RData"))

sf_use_s2(FALSE)

# to later filter pelagics
india_buff_sf <- india_buff_sf %>% mutate(INLAND = 1)


# join grid and coastal boundary info 
temp = data %>%
  distinct(GROUP.ID, LONGITUDE, LATITUDE) %>% 
  group_by(GROUP.ID) %>% 
  slice_sample(n = 1) %>% 
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
  slice_sample(n = 1) %>% 
  ungroup()

data = data %>% 
  left_join(temp, by = "GROUP.ID")


# saving data for downstream use ----------------------------------------------------

# renaming here; input RData will always have "data" so cannot change that
data_IN <- data
rm(data)

save(data_IN, data_spec, soib,
     file = "data/01_import_data.RData")
