# Functions for Ottawa Neighbourhood Study Dissemination Area to Neighbourhoods

# Download OpenStreetMaps residential zones for Ottawa, Ontario
get_osm_ottawa_residential_polygons <- function(){
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

  message("Running spatial union, this may take a minute...")
  result <- ottawa_osm_polygons %>% #ottawa_osm$osm_polygons %>%
    sf::st_union() %>%
    sf::st_transform(32189)

  return(result)
}


get_ottawa_das <- function(){
  url <-"https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lda_000a21a_e.zip"

  # download and unzip dissemination area files

  dir.create("temp")

  message ("Downloading file")
  download.file(url = url, destfile = "temp/das.zip")

  message("Unzipping file")
  unzip("temp/das.zip", exdir = "temp")

  # load all DAs
  message("Load DAs")
  das_all <- sf::read_sf("temp/lda_000a21a_e.shp")

  # filter for just Ottawa DAs
  message("Filter DAs")
  da_ott <- das_all %>%
    dplyr::filter(stringr::str_detect(DAUID, "^3506")) %>%
    sf::st_transform(crs = 32189)

  # remove temp files
  message("Remove temp files")
  file.remove(list.files("temp/", full.names = TRUE))
  file.remove("temp")

  return(da_ott)

}




get_da_ons_intersection <- function(da_ott, ons_trim){

  da_intersect <-  da_ott %>%
    #head(1) %>%
  #  ggplot() + geom_sf(data = ons_shp) + geom_sf(fill = "blue")
  #dplyr::mutate(geometry = purrr::map(geometry, st_buffer, dist = 0)) #%>%
  dplyr::mutate(results = purrr::map(geometry, function(x) {
    purrr::map(ons_trim$geometry, sf::st_intersection, y=x) %>%
      purrr::map_dbl(sf::st_area) %>%
      dplyr::tibble(ONS_ID = ons_trim$ONS_ID, Name = ons_trim$Name, intersection_area = .)
  })) %>%
  sf::st_set_geometry(NULL)



da_unnest <- da_intersect %>%
  tidyr::unnest(cols = results) %>%
#  dplyr::rename(intersection_area = results) %>%
  group_by(DAUID) %>%
  dplyr::mutate(total_intersection_area = sum(intersection_area),
         intersection_pct = intersection_area / total_intersection_area,
         intersection_pct = round(intersection_pct, digits=3 )) %>%
  dplyr::filter(intersection_pct > 0 ) %>%
  dplyr::select(DAUID, ONS_ID, intersection_pct) %>%
  tidyr::pivot_wider(names_from = ONS_ID, values_from = intersection_pct, values_fill = 0) %>%
  dplyr::select(DAUID, sort(current_vars()))

da_unnest
}
