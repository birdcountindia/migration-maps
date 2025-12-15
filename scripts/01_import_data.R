library(skimmr)
library(lubridate)
library(sf)
library(dplyr)
library(purrr)

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
  filter(OBSERVATION.DATE >= as.Date("2020-06-01") &
           OBSERVATION.DATE < as.Date("2025-06-01")) %>% 
    # filter for only approved observations & species
  filter(REVIEWED == 0 | APPROVED == 1) %>% 
  filter(ALL.SPECIES.REPORTED == 1) |> 
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

data_IN <- data_IN %>% select(-REVIEWED,-APPROVED,-ALL.SPECIES.REPORTED, 
                               -SAMPLING.EVENT.IDENTIFIER, -GROUP.IDENTIFIER)
save(data_IN, 
     file = "data/IN_data.RData")

# loading single-species global data -----------------------------------------------------

# species of interest for maps
#spec_mig <- sp_ls |>
#  mutate(SPECIES = if_else(is.na(SPECIES2),SPECIES1,
#                           paste(SPECIES1, SPECIES2, sep = " "))) |>
  #group_split(BATCH)
# we also need individual species-level global datafiles (for the points)
#batches = 1:3
#for (b in batches) {
#  message("Running batch: ", b)

#spec_mig <- unlist(lapply(seq_len(nrow(sp_ls)), function(i) {if (is.na(sp_ls$SPECIES2[i])) {sp_ls$SPECIES1[i]} else {c(sp_ls$SPECIES1[i], sp_ls$SPECIES2[i])}}))
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
  filter(OBSERVATION.DATE >= as.Date("2020-06-01") &
           OBSERVATION.DATE < as.Date("2025-06-01")) %>%   
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

# Some data has to be filtered out to remove exotic and 
# vagrant records so that the maps are not skewed.

data_spec <- data_spec |>
  filter(!(COMMON.NAME == "Asian Brown Flycatcher" & LONGITUDE > 140)) |> 
  filter(!(COMMON.NAME == "Bar-headed Goose" & LONGITUDE < 60)) |> 
  filter(!(COMMON.NAME == "Barn Swallow" & LONGITUDE < -154)) |> 
  filter(!(COMMON.NAME == "Blue Rock-Thrush" & LONGITUDE < -25)) |> 
  filter(!(COMMON.NAME == "Blue-tailed Bee-eater" & LONGITUDE > 140)) |> 
  filter(!(COMMON.NAME == "Blyth's Reed Warbler" & LONGITUDE < -25)) |> 
  filter(!(COMMON.NAME == "Common Crane" & LONGITUDE < -25)) |> 
  filter(!(COMMON.NAME == "Common Cuckoo" & LONGITUDE < -25)) |> 
  filter(!(COMMON.NAME == "Common Greenshank" & LONGITUDE < -30)) |> 
  filter(!(COMMON.NAME == "Common Rosefinch" & LONGITUDE < -30)) |> 
  filter(!(COMMON.NAME == "Demoiselle Crane" & LONGITUDE < 15)) |> 
  filter(!(COMMON.NAME == "Western Cattle-Egret" & LONGITUDE < -154)) |> 
  filter(!(COMMON.NAME == "Eurasian Whimbrel" & LONGITUDE < -30)) |> 
  filter(!(COMMON.NAME == "Garganey" & LONGITUDE < -30)) |> 
  filter(!(COMMON.NAME == "Gray-headed Lapwing" & LONGITUDE < 55)) |> 
  filter(!(COMMON.NAME == "Great Cormorant" & LONGITUDE < -125)) |> 
  filter(!(COMMON.NAME == "Little Tern" & LONGITUDE < -30)) |> 
  filter(!(COMMON.NAME == "Oriental Pratincole" & LONGITUDE < 35)) |> 
  filter(!(COMMON.NAME == "Oriental Turtle-Dove" & LONGITUDE < -30)) |> 
  filter(!(COMMON.NAME == "Red-breasted Flycatcher" & LONGITUDE < -30)) |>
  filter(!(COMMON.NAME == "Taiga Flycatcher" & LONGITUDE < -30)) |>
  filter(!(COMMON.NAME == "Red-flanked Bluetail" & LONGITUDE < -30)) |> 
  filter(!(COMMON.NAME == "Red-headed Bunting" & LONGITUDE < 43)) |> 
  filter(!(COMMON.NAME == "Willow Warbler" & LONGITUDE < -30)) |> 
  filter(!(COMMON.NAME == "Wilson's Storm-Petrel" & LONGITUDE < -154)) |> 
  filter(!(COMMON.NAME == "Yellow-browed Warbler" & LONGITUDE < -30))
 
# saving data for downstream use ----------------------------------------------------
data_spec <- data_spec %>% select(-REVIEWED,-APPROVED,-ALL.SPECIES.REPORTED,-EXOTIC.CODE, -SAMPLING.EVENT.IDENTIFIER, -GROUP.IDENTIFIER)

save(data_spec,
  file = paste0("data/spec_data_batch.RData"))

print("Done.")
#}
