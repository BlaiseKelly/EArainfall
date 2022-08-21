library(sf)
library(raster)
library(lubridate)
library(dplyr)
library(openair)
library(stringr)
library(mapview)

s <- readRDS("dat/sites_w_names.RDS") %>% st_transform(27700)

## downloaded UK shp file from https://geoportal.statistics.gov.uk/datasets/ons::countries-december-2021-gb-bfc/explore?location=55.232621%2C-3.265847%2C6.88 and converted to RDS
eng_shp <- readRDS("dat/england.RDS") %>% 
  st_transform(27700) %>% 
  st_simplify(dTolerance = 2000) %>% 
  st_buffer(5000)

eng_line <- st_cast(eng_shp, "MULTILINESTRING")

dat_from_google <- "C:/Users/blais/Documents/EArainfall/dat/"

## data from Google Drive
all_filez <- list.files(dat_from_google, pattern = '.csv', full.names = FALSE)
site_list <- list()
all_mnths <- list()
for (a in all_filez){
  
  dat <- read.csv(paste0(dat_from_google, a))

  # jan_aug <- dat %>% 
  #   mutate(date = ymd_hms(date)) %>% 
  #   mutate(y = year(date),
  #          md = mday(date),
  #          m = month(date)) %>% 
  #   filter(m < 9 & md < 20) %>% 
  #   group_by(y) %>% 
  #   summarise(tot_rain = sum(value))
  
  jan_jul <- dat %>% 
    mutate(date = ymd_hms(date)) %>% 
    mutate(y = year(date),
           md = mday(date),
           m = month(date)) %>% 
    filter(m < 8)
  
  aug <- dat %>% 
    mutate(date = ymd_hms(date)) %>% 
    mutate(y = year(date),
           md = mday(date),
           m = month(date)) %>% 
    filter(m == 8 & md <20) 
  
  jan_aug <- jan_jul %>% 
    rbind(aug) %>% 
    group_by(y) %>% 
    summarise(tot_rain = sum(value))
  
  full_yr <- dat %>% 
    mutate(date = ymd_hms(date)) %>% 
    mutate(y = year(date),
           md = mday(date),
           m = month(date)) %>% 
    group_by(y) %>% 
    summarise(tot_rain = sum(value))
  
  jan_aug$station_number <- str_sub(a, 1, -5)
  full_yr$station_number <- str_sub(a, 1, -5)
  
  site_list[[a]] <- jan_aug
  all_mnths[[a]] <- full_yr
  print(a)
}

all_sites <- do.call(rbind, site_list)
full_yrz <- do.call(rbind, all_mnths)
saveRDS(all_sites, "dat/jan_aug_tots.RDS")
saveRDS(full_yrz, "dat/full_yr_tots.RDS")
