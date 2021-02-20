# calculation tt to Zentrumsbereiche
# City West (Zoo/Kurfürstendamm)
# Mitte (Potsdamer Platz/Alexanderplatz)

update.packages()
# install.packages("gtfsrouter", "geodist", "alphahull", "mapview")
library(tidyverse)
library(tidytransit)
library(sf)
library(tmap)
library(units)
library(RColorBrewer)
library(leaflet)
tmap_mode("view")

# set work directions
setwd_gtfs <- function(){setwd("~/Documents/Uni/Master/DataScienceTransport/data/vbb-gtfs")}
setwd_data <- function(){setwd("~/Documents/Uni/Master/DataScienceTransport/data")}
setwd_work <- function(){setwd("~/Documents/Uni/Master/DataScienceTransport/assignment_2")}


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

shape_lichtenberg <- shape_districts_new %>% 
  filter(NAME == "Lichtenberg")
shape_spandau <- shape_districts_new %>% 
  filter(NAME == "Spandau")

##############################################################
#
#   SHAPE CENTER AREAS
#
##############################################################
# "Zentrentragender Stadtraum mit höchster / hoher Urbanität"
# of Zentrumsbereichskernen
# see page 39: https://www.stadtentwicklung.berlin.de/planen/stadtentwicklungsplanung/download/zentren/2011-07-31_StEP_Zentren3.pdf
# or page 45 (less detailed): https://www.stadtentwicklung.berlin.de/planen/stadtentwicklungsplanung/download/zentren/StEP_Zentren_2030.pdf
# recreated with QGis

shape_center <- read_sf(dsn = "shape_center_areas", layer = "center_areas") %>% 
  mutate(name = c("east", "west")) %>% 
  select(name)

shape_center_east <- shape_center %>% filter(name == "east")
shape_center_west <- shape_center %>% filter(name == "west")

##############################################################
#
#   READ GTFS DATA
#
##############################################################
# now we work with tidytransit
# calculation of shortest tt from all station to specific ones is more convinent

setwd_gtfs()
gtfs <- read_gtfs("2020-12_2020-12-28.zip")
setwd_work()

# http://tidytransit.r-transit.org/reference/filter_stop_times.html
stop_times_filtered <- filter_stop_times(gtfs, "2021-01-18", "06:00:00", "07:55:00")


##############################################################
#
#   GET STOPS
#
##############################################################
stops <- st_as_sf(gtfs$stops, coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
  st_transform(25833) %>% 
  select(stop_name) %>%
  rename(name = stop_name) %>%
  distinct(name)

stops_berlin <- stops %>% 
  mutate(inside_berlin = st_within( geometry, shape_berlin )) %>% 
  mutate(inside_berlin = !is.na( as.numeric( inside_berlin ))) %>% 
  filter(inside_berlin == TRUE) %>% 
  select(name)

stops_center <- stops %>% 
  mutate(inside_center = st_within( geometry, shape_center )) %>% 
  mutate(inside_center = !is.na( as.numeric( inside_center ))) %>% 
  filter(inside_center == TRUE) %>% 
  select(name)

stops_center_east <- stops %>% 
  mutate(inside_center_east = st_within( geometry, shape_center_east )) %>% 
  mutate(inside_center_east = !is.na( as.numeric( inside_center_east ))) %>% 
  filter(inside_center_east == TRUE) %>% 
  select(name)

stops_center_west <- stops %>% 
  mutate(inside_center_west = st_within( geometry, shape_center_west )) %>% 
  mutate(inside_center_west = !is.na( as.numeric( inside_center_west ))) %>% 
  filter(inside_center_west == TRUE) %>% 
  select(name)

# check if o.k.
tm_shape(shape_center_east) +
  tm_polygons() +
  tm_shape(stops_center_east) +
  tm_dots()

tm_shape(shape_center_west) +
  tm_polygons() +
  tm_shape(stops_center_west) +
  tm_dots()

# get stops for specific districts for testing
stops_lichtenberg <- stops %>% 
  mutate(inside_lichtenberg = st_within( geometry, shape_lichtenberg )) %>% 
  mutate(inside_lichtenberg = !is.na( as.numeric( inside_lichtenberg ))) %>% 
  filter(inside_lichtenberg == TRUE) %>% 
  select(name)

stops_spandau <- stops %>% 
  mutate(inside_spandau = st_within( geometry, shape_spandau )) %>% 
  mutate(inside_spandau = !is.na( as.numeric( inside_spandau ))) %>% 
  filter(inside_spandau == TRUE) %>% 
  select(name)

# check if o.k.
tm_shape(shape_districts_new) +
  tm_polygons(alpha = 0) +
  tm_shape(stops_lichtenberg) +
  tm_dots() +
  tm_shape(stops_spandau) +
  tm_dots()


##############################################################
#
#   TT calculation
#
##############################################################
# what are the tt to the center areas?
# according to Nahverkehrsplan Berlin 2019-2023: ANlage 1 - Monitoringbericht (p. 12)
# standard: tt_max = 3600, n_transfer_max = 2, n_realise_stations = 0.95

tt <- travel_times(
  stop_times_filtered,
  stops_center$name,
  time_range = 5400,
  arrival = TRUE,
  max_transfers = 2,
  # max_departure_time = NULL,
  return_coords = TRUE,
  return_DT = FALSE
)

# clean it for plot
tt <- tt %>% 
  rename(from = from_stop_name,
         to = to_stop_name,
         tt = travel_time,
         departure = journey_departure_time,
         arrival = journey_arrival_time
         ) %>% 
  select(-c(from_stop_id, to_stop_id, to_stop_lat, to_stop_lon)) %>% 
  st_as_sf(coords = c("from_stop_lon", "from_stop_lat"),
           crs = 4326) %>% 
  st_transform(25833) %>% 
  mutate(tt = set_units(round(tt/60, 2), "min"))

##############################################################
#
#   PLOT
#
##############################################################

# https://campus.datacamp.com/courses/visualizing-geospatial-data-in-r/raster-data-and-color?ex=9
rdylgn <- rev(brewer.pal(7, "RdYlGn"))

# https://leaflet-extras.github.io/leaflet-providers/preview/
# https://tlorusso.github.io/geodata_workshop/tmap_package
# https://www.rdocumentation.org/packages/tmap/versions/3.0/topics/tm_basemap
# https://rdrr.io/cran/tmap/man/tm_view.html
# https://leafletjs.com/reference-1.3.4.html#map-methods-for-modifying-map-state

tm_basemap(leaflet::providers$CartoDB.DarkMatter) +
  tm_shape(shape_districts_new) + 
  tm_polygons(alpha = 0,
              lwd = 1.5,
              border.col = "white",
              popup.vars = c("area" = "AREA")
              ) +
  tm_shape(shape_center) +
  tm_polygons(alpha = 0.2,
              col = "red",
              border.col = "red"
              ) + 
  tm_shape(tt) +
  tm_dots(col = "tt",
          style = "fixed",
          breaks = c(0, 10, 20, 30, 40, 50, 60, 120),
          labels = c("0 – 10", "10 – 20", "20 – 30", "30 – 40", "40 – 50", "50 – 60", "> 60"), 
          id = "from",
          palette = rdylgn,
          title = "traveltime [min]",
          popup.vars = c("to" = "to", 
                         "traveltime" = "tt",
                         "departure at" = "departure",
                         "arrival at" = "arrival",
                         "number of transfers" = "transfers")) +
  tm_view(bbox = shape_berlin)


##############################################################
#
#   DEGREE OF FULLFILMENT
#
##############################################################

n_of_stations <- tt %>%
  mutate(inside_berlin = st_within( geometry, shape_berlin )) %>% 
  mutate(inside_berlin = !is.na( as.numeric( inside_berlin ))) %>% 
  filter(inside_berlin == TRUE) %>% 
  mutate(outside_center = st_within( geometry, shape_center )) %>% 
  mutate(outside_center = is.na( as.numeric( outside_center ))) %>% 
  filter(outside_center == TRUE) %>%
  nrow()

n_of_stations_valid <- tt %>% 
  mutate(inside_berlin = st_within( geometry, shape_berlin )) %>% 
  mutate(inside_berlin = !is.na( as.numeric( inside_berlin ))) %>% 
  filter(inside_berlin == TRUE) %>% 
  mutate(outside_center = st_within( geometry, shape_center )) %>% 
  mutate(outside_center = is.na( as.numeric( outside_center ))) %>% 
  filter(outside_center == TRUE) %>%
  filter(tt <= 60 * as_units("min")) %>% 
  filter(transfers <= 2) %>% 
  nrow()

percent_stations_valid <- n_of_stations_valid / n_of_stations * 100
percent_stations_valid <- round(percent_stations_valid, 2)


























