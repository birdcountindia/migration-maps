library(skimmr)
library(lubridate)
library(sf)


# individual data files
get_param()
dir_prefix <- "data/EBD/"
rawpath <- glue("{dir_prefix}ebd_amufal1_rel{currel_month_lab}-{currel_year}.txt")
# rawpath <- glue("{dir_prefix}ebd_blrwar1_rel{currel_month_lab}-{currel_year}.txt")


preimp <- c("CATEGORY","EXOTIC.CODE","COMMON.NAME","OBSERVATION.COUNT",
            "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","LAST.EDITED.DATE",
            "STATE","STATE.CODE","COUNTY","COUNTY.CODE",
            "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
            "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY","BREEDING.CODE",
            "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER",
            "TRIP.COMMENTS","SPECIES.COMMENTS", "HAS.MEDIA")

data <- read.ebd(rawpath, preimp) %>% 
  mutate(GROUP.ID = ifelse(is.na(GROUP.IDENTIFIER), 
                           SAMPLING.EVENT.IDENTIFIER,
                           GROUP.IDENTIFIER), 
         OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         YEAR = year(OBSERVATION.DATE), 
         MONTH = month(OBSERVATION.DATE),
         DAY.M = day(OBSERVATION.DATE)) %>% 
  # migratory year and month information
  mutate(M.YEAR = if_else(MONTH > 5, YEAR, YEAR-1), # from June to May
         M.MONTH = if_else(MONTH > 5, MONTH-5, 12-(5-MONTH))) 
