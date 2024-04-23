library(skimmr)
library(lubridate)


# loading main EBD for all else

get_param()
dir_prefix <- "../ebird-datasets/EBD/"
maindatapath <- glue("{dir_prefix}ebd_IN_rel{currel_month_lab}-{currel_year}.RData")

load(maindatapath)


# preparing data

data <- data %>%
  # filter for only approved observations & species
  filter(APPROVED == 1) %>%
  # slice by GROUP.ID to remove duplicate checklists
  group_by(GROUP.ID, COMMON.NAME) %>% 
  slice_sample(1) %>% 
  group_by(GROUP.ID) %>% 
  # add NO.SP column
  mutate(NO.SP = n_distinct(COMMON.NAME)) %>% 
  ungroup() %>%
  # add WEEK & FORT columns
  mutate(WEEK.Y = week(OBSERVATION.DATE),
         FORT.Y = ceiling(WEEK/2)) 
