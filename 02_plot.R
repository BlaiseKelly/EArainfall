library(sf)
library(raster)
library(lubridate)
library(dplyr)
library(openair)
library(stringr)
library(mapview)
library(pals)
library(tmap)

s <- readRDS("dat/sites_w_names.RDS") %>% st_transform(27700)

## downloaded UK shp file from https://geoportal.statistics.gov.uk/datasets/ons::countries-december-2021-gb-bfc/explore?location=55.232621%2C-3.265847%2C6.88 and converted to RDS
eng_shp <- readRDS("dat/england.RDS") %>% 
  st_transform(27700) %>% 
  st_simplify(dTolerance = 2000) %>% 
  st_buffer(5000)

eng_line <- st_cast(eng_shp, "MULTILINESTRING")

all_sites <- readRDS("dat/jan_aug_tots.RDS")
full_yrz <- readRDS("dat/full_yr_tots.RDS")

yrz <- 2010:2022
poly_dat <- list()
rast_dat <- list()
for (yr in yrz){
  
  df <- all_sites %>% 
    filter(y == yr)
  s_in <- s %>% filter(station_number %in% df$station_number)
  ## combine sites
  d <- st_union(s_in)
  ## create voroni
  v <- st_voronoi(d)
  
  ## trim the plot to the country shapefile
  p1 <- st_as_sf(st_intersection(st_cast(v), eng_shp), crs = 27700) %>% 
    st_cast("MULTIPOLYGON")

  #p2 <- df[unlist(st_intersects(p1, df)),]
  p4 <- p1[unlist(st_intersects(s_in, p1)),]
  p3 <- s_in[unlist(st_intersects(p1, s_in)),]
  
  v_dat <- p4 %>%
    mutate(station_number = p3$station_number) %>% 
    left_join(df, by = "station_number")
  
  nam <- as.character(yr)
  poly_dat[[nam]] <- v_dat
  
  ## create raster
  xmin <- min(st_coordinates(p4)[,1])
  xmax <- max(st_coordinates(p4)[,1])
  ymin <- min(st_coordinates(p4)[,2])
  ymax <- max(st_coordinates(p4)[,2])
  
  ## calculate distance to make a 1km grid
  ny <- round((ymax-ymin)/1000)
  nx <- round((xmax-xmin)/1000)
  
  ## create 1km raster
  r <- raster(xmn = xmin, xmx = xmax, ymn = ymin, ymx = ymax, nrows = ny, ncols=nx)
  
  vr <- rasterize(v_dat, r, "tot_rain")

  rast_dat[[nam]] <- vr
print(nam)
}

all_rast <- brick(rast_dat)  


## define breaks for wind speeds
rain_bks <- c(seq(from = 0, to = 1000, by = 50), 5000)

## define palette for wind speed
rain_pal <- pals::jet(NROW(rain_bks))

## create tmap of raster and shape file
all_tmz <- list()
for (yr in yrz){
  n <- which(yrz==yr)
  p0 <- tm_shape(subset(all_rast,n)) +
    tm_raster(palette = rain_pal, breaks = rain_bks, title = '\nTotal rainfall (mm) between\n1st January and 20th August\nin England')+
    tm_layout(panel.labels = yr,legend.show = FALSE,
              panel.label.color = 'white', panel.label.bg.color = 'black', inner.margins = 0.01)+
    #tm_facets(nrow = 1, ncol = 1)+
    tm_shape(eng_line)+
    tm_lines()
  nam <-as.character(yr)
  all_tmz[[nam]] <- p0
}

all_tmz[["legend"]] <- tm_shape(subset(all_rast,13)) +
  tm_raster(palette = rain_pal, breaks = rain_bks, title = '\nTotal rainfall (mm) between\n1st January and 20th August\nin England')+
  tm_layout(legend.only = TRUE)

t1 <- tmap_arrange(all_tmz)

tmap_save(t1, "plots/jan_aug.png", width = 10000, height = 6000, dpi = 600)


poly_dat <- list()
rast_dat <- list()
for (yr in yrz){
  
  df <- full_yrz %>% 
    filter(y == yr)
  s_in <- s %>% filter(station_number %in% df$station_number)
  ## combine sites
  d <- st_union(s_in)
  ## create voroni
  v <- st_voronoi(d)
  
  ## trim the plot to the country shapefile
  p1 <- st_as_sf(st_intersection(st_cast(v), eng_shp), crs = 27700) %>% 
    st_cast("MULTIPOLYGON")
  
  #p2 <- df[unlist(st_intersects(p1, df)),]
  p4 <- p1[unlist(st_intersects(s_in, p1)),]
  p3 <- s_in[unlist(st_intersects(p1, s_in)),]
  
  v_dat <- p4 %>%
    mutate(station_number = p3$station_number) %>% 
    left_join(df, by = "station_number")
  
  nam <- as.character(yr)
  poly_dat[[nam]] <- v_dat
  
  ## create raster
  xmin <- min(st_coordinates(p4)[,1])
  xmax <- max(st_coordinates(p4)[,1])
  ymin <- min(st_coordinates(p4)[,2])
  ymax <- max(st_coordinates(p4)[,2])
  
  ## calculate distance to make a 1km grid
  ny <- round((ymax-ymin)/1000)
  nx <- round((xmax-xmin)/1000)
  
  ## create 1km raster
  r <- raster(xmn = xmin, xmx = xmax, ymn = ymin, ymx = ymax, nrows = ny, ncols=nx)
  
  vr <- rasterize(v_dat, r, "tot_rain")
  
  rast_dat[[nam]] <- vr
  print(nam)
}

full_yr_rast <- brick(rast_dat)  

## define breaks for wind speeds
rain_bks <- c(seq(from = 0, to = 2000, by = 100), 5000)

## define palette for wind speed
rain_pal <- pals::jet(NROW(rain_bks))

## create tmap of raster and shape file
all_tmz <- list()
for (yr in yrz){
  n <- which(yrz==yr)
  p1 <- tm_shape(subset(full_yr_rast,n)) +
    tm_raster(palette = rain_pal, breaks = rain_bks, title = '\nTotal rainfall (mm)\nfor each year\nin England')+
    tm_layout(panel.labels = yr,legend.show = FALSE,
              panel.label.color = 'white', panel.label.bg.color = 'black', inner.margins = 0.01)+
    #tm_facets(nrow = 1, ncol = 1)+
    tm_shape(eng_line)+
    tm_lines()
  nam <-as.character(yr)
  all_tmz[[nam]] <- p1
}

all_tmz[["legend"]] <- tm_shape(subset(full_yr_rast,13)) +
  tm_raster(palette = rain_pal, breaks = rain_bks, title = '\nTotal rainfall (mm)\nfor each year\nin England')+
  tm_layout(legend.only = TRUE)

t2 <- tmap_arrange(all_tmz)

tmap_save(t2, "plots/total.png", width = 10000, height = 6000, dpi = 1000)



# p_all <- tm_shape(all_rast) +
#   tm_raster(palette = rain_pal, breaks = rain_bks, title = '\nTotal rainfall between\n1st January and 20th August\n (mm)')+
#   tm_layout(legend.outside = FALSE, legend.position = c("left", "TOP"), panel.labels = 2010:2022,
#             panel.label.color = 'white', panel.label.bg.color = 'black', inner.margins = 0.01)+
#   tm_facets(nrow = 1, ncol = 1)+
#   tm_shape(eng_line)+
#   tm_lines()
# 
# tmap_animation(p_all, filename = paste0("plots/rainfall.gif"), delay = 120)


