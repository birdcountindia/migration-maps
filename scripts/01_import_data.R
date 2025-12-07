library(skimmr)
library(lubridate)
library(sf)
library(dplyr)
library(purrr)


# species of interest for maps
spec_mig <- c(
  "Amur Falcon", "Arctic Warbler", "Ashy Drongo", "Asian Brown Flycatcher", "Bar-headed Goose",
  "Black Baza", "Black Redstart", "Blue Rock-Thrush", "Blue-tailed Bee-eater", "Blyth's Reed Warbler",
  "Brown-breasted Flycatcher", "Brown-headed Gull","Eastern Cattle Egret","Western Cattle Egret",
  "Gray Nightjar", "Greater Whitethroat", "Chestnut-headed Bee-eater", "Chestnut-winged Cuckoo", 
  "Collared Pratincole", "Oriental Pratincole",  "Common Crane", "Common Cuckoo", 
  "Common Greenshank", "Common Rosefinch", "Crab-Plover", "Demoiselle Crane", "Desert Wheatear",
  "Pied Wheatear", "Eurasian Wryneck", "European Bee-eater",
  "European Roller", "Forest Wagtail", "Garganey", "Gray-headed Lapwing",
  "Great Cormorant", "Great White Pelican", "Green Warbler", "Greenish Warbler",
  "Gull-billed Tern", "Hypocolius", "Indian Blue Robin", "Indian Golden Oriole",
  "Indian Paradise-Flycatcher", "Indian Pitta", "Isabelline Shrike", "Brown Shrike",
  "Isabelline Wheatear", "Kashmir Flycatcher", "Little Tern", "Northern Wheatear",
  "Oriental Turtle-Dove", "Pacific Golden-Plover", "Pallas's Fish-Eagle", "Pied Cuckoo",
  "Red-breasted Flycatcher", "Red-flanked Bluetail", "Red-headed Bunting", "Rosy Starling",
  "Rufous-tailed Scrub-Robin", "Sanderling", "Short-eared Owl", "Spot-winged Starling",
  "Spotted Flycatcher", "Tytler's Leaf Warbler", "Eurasian Whimbrel", "White-eyed Buzzard",
  "White-throated Needletail", "Willow Warbler", "Wilson's Storm-Petrel",
  "Yellow-browed Warbler", "Yellow-eyed Pigeon","Rusty-tailed Flycatcher", "Pied Thrush", 
  "Barn Swallow", "Dalmatian Pelican", "Taiga Flycatcher"
)

spec_mig <- c(get_spec_mig()$SPECIES1, get_spec_mig()$SPECIES2) %>% na.omit()

# loading main data -----------------------------------------------------
# india data to calculate repfreq (we show repfreq in India in gifs)
# paths to latest versions of user & GA info, and sensitive species data

load("../ebird-datasets/EBD/latest_non-EBD_paths.RData")
ebird_rel_param()

dir_prefix <- "data/EBD/" ### this will change: directly use RData from ebird-datasets/EBD/ ###
maindatapath <- glue("../ebird-datasets/EBD/ebd_IN_rel{currel_month_lab}-{currel_year}.RData")

load(maindatapath)

# cols to import
preimp <- c("COMMON.NAME","REVIEWED","APPROVED","LATITUDE","LONGITUDE",
            "OBSERVATION.DATE","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER",
            "SAMPLING.EVENT.IDENTIFIER", "YEAR", "MONTH", "DAY.M",
            "M.YEAR", "M.MONTH", "GROUP.ID")

data_IN <- data %>% select(all_of(preimp))
rm(data_sed, data)

# preparing data 
data_IN <- data_IN %>%
  #Because all sp data is till JUN 2025 only.
  filter(OBSERVATION.DATE >= as.Date("2020-01-01") &
           OBSERVATION.DATE <= as.Date("2025-06-30")) %>% 
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

dir_prefix <- glue("data/EBD/{currel_year}")
# ideally, the only .txt files in this directory should be single-species data
# country data should be RData and should be pulled from ebird-datasets/EBD/

preimp <- c("COMMON.NAME","REVIEWED","APPROVED","LATITUDE","LONGITUDE",
            "OBSERVATION.DATE","ALL.SPECIES.REPORTED", "EXOTIC.CODE","GROUP.IDENTIFIER",
            "SAMPLING.EVENT.IDENTIFIER")

data_spec <- map_df(list.files(path = dir_prefix, pattern = ".txt"), ~{
  
  filepath <- glue("{dir_prefix}/{.x}")
  read.ebd(filepath, cols_sel = preimp)
  
}) %>% 
  filter(!EXOTIC.CODE %in% c("P", "X")) %>%   
  mutate(GROUP.ID = ifelse(is.na(GROUP.IDENTIFIER), 
                           SAMPLING.EVENT.IDENTIFIER,
                           GROUP.IDENTIFIER), 
         OBSERVATION.DATE = as_date(OBSERVATION.DATE), 
         YEAR = year(OBSERVATION.DATE), 
         MONTH = month(OBSERVATION.DATE),
         DAY.M = day(OBSERVATION.DATE)) %>% 
  #Because 6 sps have data is till OCT 2025 only.
  filter(OBSERVATION.DATE >= as.Date("2020-01-01") &
           OBSERVATION.DATE <= as.Date("2025-06-30")) %>%   
    # migratory year and month information
  mutate(M.YEAR = if_else(MONTH > 5, YEAR, YEAR-1), # from June to May
         M.MONTH = if_else(MONTH > 5, MONTH-5, 12-(5-MONTH))) %>% 
  mutate(DAY.Y = yday(OBSERVATION.DATE),
         WEEK.Y = week(OBSERVATION.DATE),
         FORT.Y = ceiling(WEEK.Y/2),
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


# join grid and coastal boundary info 
temp = data_IN %>%
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

data_IN = data_IN %>% 
  left_join(temp, by = "GROUP.ID")

# saving data for downstream use ----------------------------------------------------

# renaming here; input RData will always have "data" so cannot change that
save(data_IN, data_spec, 
     file = "data/01_import_data.RData")
