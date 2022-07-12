# get osm data??
#install.packages("osmdata")
library(osmdata)


# [out:json][timeout:10000];
# // gather results
# (
#   // query part for: “landuse=residential”
#   way["landuse"="residential"]({{bbox}});
#   relation["landuse"="residential"]({{bbox}});
# );
# // print results
# out body;
# >;
# out skel qt;
#
get_oms_ottawa_residential_polygons <- function(){
  ottawa_osm <- osmdata::opq(bbox = sf::st_bbox(onsr::ons_shp),
                    nodes_only = FALSE,
                    timeout = 10000,
                    memsize = 1000000000) %>%
    osmdata::add_osm_feature(key = "landuse", value = "residential") %>%
    osmdata::osmdata_sf(quiet = FALSE)

  # need both polygons and multipolygons or we miss barrhaven
  ottawa_osm_polygons <- dplyr::bind_rows(ottawa_osm$osm_polygons, ottawa_osm$osm_multipolygons) %>%
    dplyr::select(osm_id) %>%
    sf::st_transform(crs = 32189)


  return(ottawa_osm_polygons)
}




library(tidyverse)

test$osm_polygons %>% ggplot() + geom_sf()


ott_osm_res_polys %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addPolygons()

opq(bbox = 'Ottawa, Canada') %>%
  add_osm_feature(key = "landuse", value = "residential") %>%
  osmdata::opq_string()

osmdata::opq()

sf::st_bbox(onsr::ons_shp)


library(leaflet)
targets::tar_load(osm_res)
targets::tar_load()
osm_res %>% sf::st_transform(crs = "WGS84") %>%
leaflet() %>% addTiles() %>% addPolygons()


# get the DAs that are missing, don't intersect any residential zones
da_ott %>% filter(!DAUID %in% da_ons_intersect_raw$DAUID) %>%
  sf::st_transform(crs = "WGS84") %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addPolygons(label = ~ DAUID, color = "red", fill = "red") %>%
  leaflet::addPolygons(data = sf::st_transform(ons_trim, crs = "WGS84"), label = ~ ONS_ID, color = "green", fill = "green")
#  leaflet::addPolygons(data = sf::st_transform(ons_shp, crs = "WGS84"), label = ~ ONS_ID, color = "green", fill = "green")

test <- da_ott %>%
  dplyr::filter(!DAUID %in% da_ons_intersect_raw$DAUID) %>%
  get_da_ons_intersection(ons_trim = ons_shp) %>%
  dplyr::bind_rows(da_ons_intersect_raw) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), function(x) dplyr::if_else(is.na(x), 0, x)))

