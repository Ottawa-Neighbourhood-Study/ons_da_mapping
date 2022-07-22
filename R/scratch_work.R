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
targets::tar_load(da_ons_intersect_raw)
targets::tar_load(da_ons_intersect)
targets::tar_load(ons_trim)

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




# rounding but preserving sum to 1

# fails for c(28.54170, 17.46122, 53.99708)
# total rewrite of https://github.com/basilesimon/largeRem
largeRem <- function (values) {
  # can't use `==` because it will fail for some floating-point comparisons

  if (!all.equal(sum(values), 100)) {
    message(values)
    stop("The sum of the items in your column isn't equal to 100!!!!")
  }

  diffTo100 <- 100 - sum(as.integer(values))

  values_int <- floor(values)
  result <- values_int

  # get decimal values, and rank them in decreasing order of bigness
  decimal_values <- values %% 1

  ranked_indices <- sort(decimal_values, decreasing = TRUE,  index.return = TRUE)$ix

  # we need to add some integer value to get to 100, and we'll add 1 to as many
  # different values as we need using our ranking
  indices_to_update <- ranked_indices[1:diffTo100]

  # update those values
  result[indices_to_update] <- result[indices_to_update] +  sign(diffTo100)

  return(result)
}

test <- da_ons_intersect %>% pivot_longer(cols = -DAUID) %>% filter(value > 0) %>% group_by(DAUID)

test2 <- test %>%
  mutate(value_pct = value*100) %>%
  group_by(DAUID) %>%
  mutate(pct_largerem = largeRem(value_pct),
         pct_round = round(value_pct),
         same = pct_largerem == pct_round
         )

da_ons_intersect %>%
  pivot_longer(cols = -DAUID, names_to = "ONS_ID", values_to = "pct_overlap") %>%
  filter(pct_overlap > 0) %>%
  group_by(DAUID) %>%
  mutate(pct_overlap_rnd = largeRem(pct_overlap*100)/100)


test %>%
  filter(DAUID == "35060127") %>%
  pull(value) %>% `*`(100) %>% sum()

df <- 1

1/6
