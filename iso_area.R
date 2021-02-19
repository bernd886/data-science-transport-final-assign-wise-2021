update.packages()
# install.packages("gtfsrouter", "geodist", "alphahull", "mapview")
library(gtfsrouter)
# library(tidyverse)
library(sf)
library(tmap)
library(units)
tmap_mode("view")

##############################################################
#
#   READ GTFS DATA
#
##############################################################
# set work directions
setwd_gtfs <- function(){setwd("~/Documents/Uni/Master/DataScienceTransport/data/vbb-gtfs")}
setwd_data <- function(){setwd("~/Documents/Uni/Master/DataScienceTransport/data")}
setwd_work <- function(){setwd("~/Documents/Uni/Master/DataScienceTransport/assignment_2")}

setwd_work

# read gtfs data for monday
file <- file.path("~/Documents/Uni/Master/DataScienceTransport/data/vbb-gtfs/2020-12_2020-12-28.zip")
gtfs <- extract_gtfs(file) %>% gtfs_timetable(day = 2)

##############################################################
#
#   SET TIMES
#
##############################################################
start_time <- 7 * 3600 + 1200
end_time <- 8 * 3600

# create isochrone
# ic <- gtfs_isochrone (gtfs,
#                       from = from,
#                       start_time = start_time,
#                       end_time = end_time)

##############################################################
#
#   CREATE STOPS SF OBJECT
#
##############################################################
stops <- st_as_sf(gtfs$stops,
                   coords = c("stop_lon", "stop_lat"),
                   crs = 4326) %>% 
  st_transform(25833)

##############################################################
#
#   SHAPE DISTRICTS NEW (+ area)
#
##############################################################
setwd_data()
shape_districts_new <- read_sf(dsn = "LOR_SHP_2019-1", layer = "Planungsraum_EPSG_25833")
setwd_work()

shape_districts_new <- shape_districts_new %>% 
  group_by(BEZIRK) %>% 
  summarise() %>% 
  filter(!is.na(BEZIRK)) %>% 
  rename(NAME = BEZIRK) %>% 
  mutate(AREA = st_area(geometry)) %>% 
  select(NAME, AREA, everything()) %>% 
  mutate(AREA = (AREA / 1000000) * as_units("km2"))

# setting crs of polygons
st_crs(shape_districts_new$geometry) <- 25833

shape_berlin <- st_union(shape_districts_new)

##############################################################
#
#   SPECIFIC SHAPES AND STOPS
#
##############################################################

stops_in_berlin <- stops %>% 
  mutate(inside_berlin = st_within( geometry, shape_berlin )) %>% 
  mutate(inside_berlin = !is.na( as.numeric( inside_berlin ))) %>% 
  filter(inside_berlin == TRUE)

# get isochrone area
# ic = gtfs_isochrone (gtfs,
#                      from = "Berlin, Sowjetisches Ehrenmal",
#                      #from_is_id = TRUE,
#                      start_time = start_time,
#                      end_time = end_time)$hull$area

lichtenberg <- shape_districts_new %>% 
  filter(NAME == "Lichtenberg")
treptow <- shape_districts_new %>% 
  filter(NAME == "Treptow-Köpenick")

stops_one_district <- stops %>% 
  mutate(inside_district = st_within( geometry, treptow )) %>% 
  mutate(inside_district = as.numeric( inside_district )) %>% 
  filter(inside_district == 1) %>% 
  select(stop_id, stop_name)

##############################################################
#
#   CALCULATE ISOCHRONES
#
##############################################################

stops_ic_area <- vector(mode = "double")

# create isochrone areas for stops in 50 minutes
for (stop_name in stops$stop_name){

  tryCatch( {
    ic_area <- gtfs_isochrone (gtfs,
                               from = stop_name,
                               #from_is_id = TRUE,
                               start_time = start_time,
                               end_time = end_time)$hull$area
    if(is.null(ic_area)) {
      stops_ic_area <<- rbind(stops_ic_area, 0)
      print(paste(stop_name, ": ", ic_area, "!!!!!!!!!!"))
    } else {
      stops_ic_area <<- rbind(stops_ic_area, ic_area)
      print(paste(stop_name, ": ", ic_area))
    }
    },
    error = function(e) {
      stops_ic_area <<- rbind(stops_ic_area, 0)
      print(paste("ERROR!!!", stop_name))
      }
    )
}

##############################################################
#
#   CLEANING
#
##############################################################

# merge and clean
# https://r-spatial.github.io/sf/reference/bind.html
# https://cran.r-project.org/web/packages/units/vignettes/units.html
rownames(stops_ic_area) <- NULL
stops_area <- st_sf(data.frame(stops, stops_ic_area / 1000000)) %>%
  rename(ic_area = stops_ic_area.1e.06,
         id = stop_id,
         name = stop_name,
         parent = parent_station) %>% 
  select(id, name, parent, ic_area) %>% 
  mutate(ic_area = ic_area * as_units("km2"))

# save
# https://r-spatial.github.io/sf/reference/st_write.html
st_write(stops_area, "output_stops_ic_area.shp")
# st_read("output_stops_ic_area.shp")

# more cleaning for plot
# https://dplyr.tidyverse.org/reference/distinct.html
stops_area = 
  stops_area %>% 
  select(name, ic_area) %>% 
  distinct(name, .keep_all = TRUE)

stops_area_berlin <- stops_area %>% 
  mutate(inside_berlin = st_within( geometry, shape_berlin )) %>% 
  mutate(inside_berlin = !is.na( as.numeric( inside_berlin ))) %>% 
  filter(inside_berlin == TRUE) %>% 
  select(-inside_berlin) %>% 
  mutate(id = paste(name, ": ", round(ic_area)))

##############################################################
#
#   PLOT
#
##############################################################

tm_shape(shape_districts_new) +
  tm_polygons(alpha = 0,
              popup.vars = c("area" = "AREA")) +
  tm_shape(stops_area_berlin) +
  tm_dots(col = "ic_area",
          id = "name",
          popup.vars = c("area" = "ic_area"),
          size = 0.07,
          border.lwd = 0.3,
          legend.hist = TRUE,
          n = 15,
          title = "isochrone area [km^2]") +
  tm_view(bbox = shape_berlin)


ic_einstein <- gtfs_isochrone(gtfs,
                              from = "Berlin, Helmholtzstr.",
                              start_time = 18 * 3600,
                              end_time = 18 * 3600 + 1800)

tm_shape(ic_einstein$hull) + 
  tm_polygons(col = "red",
              alpha = 0.2,
              border.col = "red") +
  tm_shape(ic_einstein$routes) +
  tm_lines() +
  tm_shape(ic_einstein$end_points) +
  tm_dots(col = "red") + 
  tm_shape(ic_einstein$start_point) + 
  tm_dots(col = "green")

