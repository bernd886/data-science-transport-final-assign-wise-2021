# ----
library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")

berlin_bezirke <- st_read("~/Documents/Uni/Master/DataScienceTransport/assignment_2/Hao/shp-bezirke/bezirke_berlin.shp")

berlin_emissions <-read_delim("~/Documents/Uni/Master/DataScienceTransport/assignment_2/Hao/berlin-v5.5-1pct.emissionsgrid_Berlin_PlanA.csv", 
                            delim="\t",
                            # sep = "\t",
                            locale=locale(decimal_mark = "."),
                            col_types = cols(
                              x = col_double(),
                              y = col_double()
                              #person = col_double(),
                              #vehicleId = col_character(),
                              #fromLinkId = col_double(),
                              #start_x = col_double(),
                              #start_y = col_double()
                              #toLinkId = col_double(),
                              #toX = col_double(),
                              #toY = col_double(),
                              #waitTime = col_double(),
                              #arrivalTime = col_double(),
                              #travelTime = col_double(),
                              #travelDistance_m = col_double(),
                              #direcTravelDistance_m = col_double()
                            ))
# berlin_bezirke <- read.table("/Users/haowu/Workspace/R/DataScience_HA2/Output_MATSim_Emission/PlanA/berlin-v5.5-1pct.emissionsgrid_PlanA.csv",sep = "\t", dec=".", header = TRUE, encoding="UTF-8", stringsAsFactors = FALSE)

#view(berlin_emissions)
#berlin_emissions
#berlin_bezirke <- st_as_sf(berlin_bezirke, coords=c("x", "y"), crs=25833)





# ----
library(sf)
map = read_sf("~/Documents/Uni/Master/DataScienceTransport/assignment_2/Hao/shp-bezirke/bezirke_berlin.shp")

berlin_emissions_sf <- st_as_sf(berlin_emissions, coords = c('x', 'y'), crs = st_crs(map))
#view(berlin_emissions_sf)

berlin_emissions_bezirke <- berlin_emissions_sf %>% mutate(
  intersection = as.integer(st_intersects(geometry, map))
  , area = if_else(is.na(intersection), '', map$Name[intersection])
) 

#view(berlin_emissions_bezirke)




# ----
#tm_shape(berlin_bezirke) +
#  tm_polygons(col="berlin_emissions_bezirke") +
#  tm_shape(berlin_emissions_bezirke) +
#  tm_symbols(size=0.01, col="PM2_5_non_exhaust")
#  # tm_dots(size="count", col="count")




# ----
joined <- st_join(berlin_bezirke, berlin_emissions_bezirke)
joined

# ----
# joined_count <- joined %>%
#   group_by(Name) %>%
#   summarise(count=sum(PM2_5_non_exhaust))
# view(joined_count)
# 
# tm_shape(joined_count) +
#   # tm_borders() +
#   tm_polygons(col="Name") +
#   tm_dots(size="count", col="count")
#   # tm_symbols(size="count", col="count")

# ----
# joined_count <- joined %>%
#   group_by(Name) %>%
#   summarise(count_value=n()) %>%
#   summarise(sum_value_PM2_5_non_exhaust=sum(PM2_5_non_exhaust)) %>%
#   summarise(sum_value_NH3=sum(NH3)) %>%
#   summarise(size_value=sum_value_PM2_5_non_exhaust/count_value) %>%
#   summarise(col_value=sum_value_NH3/count_value)
# view(joined_count)
# 
# tm_shape(joined_count) +
#   # tm_borders() +
#   tm_polygons(col="Name") +
#   # tm_dots(size="size_value", col="col_value")
#   tm_symbols(size="size_value", col="col_value")

# Plot1: 只有PM2.5 ----
joined_count <- joined %>%
  group_by(Name) %>%
  summarise(sum=sum(PM2_5_non_exhaust)) %>%
  #summarise(sum=sum(N2O)) %>%
  #summarise(sum=sum(NH3)) %>%
  ungroup() %>%
  mutate(col_value=sum/n()) %>% 
  #mutate(col_value=1/sum*n())
  rename("Particular Matter" = "col_value")
  
tm_shape(joined_count) +
  # tm_borders() +
  tm_polygons(col="Name") +
  tm_shape(joined_count) +
  # tm_dots(size="size_value", col="col_value")
  tm_symbols(size="sum", col="Particular Matter")




# Plot2: 只有PM2.5 + N2O ----
joined_count <- joined %>%
  group_by(Name) %>%
  #summarise(sum=sum(PM2_5_non_exhaust)) %>%
  summarise(sum=sum(N2O)) %>%
  #summarise(sum=sum(NH3)) %>%
  ungroup() %>%
  #mutate(col_value=sum/n())
  mutate(col_value=1/sum*n())
# view(joined_count)

berlin_emissions_bezirke <- berlin_emissions_bezirke %>% 
  rename("Nitrous Oxide (N2O)" = "N2O")
  
tmap_mode("view")
tm_shape(joined_count) +
  # tm_borders() +
  tm_polygons() +
  # tm_polygons(col="col_value") +
  # tm_dots(size="size_value", col="col_value")
  tm_shape(berlin_emissions_bezirke) +
  tm_dots(size=0.01, col="Nitrous Oxide (N2O)", border.lwd=NA)
  # tm_dots(size=0.001, col="N2O", alpha=0.1)
tmap_mode("view")
