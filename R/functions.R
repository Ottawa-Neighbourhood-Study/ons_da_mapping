# Functions for Ottawa Neighbourhood Study Dissemination Area to Neighbourhoods

# Download OpenStreetMaps residential zones for Ottawa, Ontario
get_osm_ottawa_residential_polygons <- function(ons_shp){

  ottawa_osm <- osmdata::opq(bbox = sf::st_bbox(sf::st_transform(ons_shp, crs = "WGS84")),
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


get_ottawa_das <- function(year = c(2016, 2021)){

  year <- as.character(year)
  year <- match.arg(year, year)

  if (year == "2021"){
    url <-"https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lda_000a21a_e.zip"
  }
  if (year == "2016"){
    # https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm
    url <- "https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lda_000a16a_e.zip"

  }

  # download and unzip dissemination area files

  dir.create("temp")

  message ("Downloading file")
  download.file(url = url, destfile = "temp/das.zip")

  message("Unzipping file")
  unzip("temp/das.zip", exdir = "temp")

  # load all DAs
  message("Load DAs")
  da_file <- list.files(path = "temp", pattern = "*.shp") %>%
   paste0("temp/", .)

  das_all <- sf::read_sf(da_file)

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
                  intersection_pct = intersection_area / total_intersection_area
                  #,intersection_pct = round(intersection_pct, digits=3 )
                  ) %>%
    dplyr::filter(intersection_pct > 0 ) %>%
    dplyr::select(DAUID, ONS_ID, intersection_pct) %>%
    tidyr::pivot_wider(names_from = ONS_ID, values_from = intersection_pct, values_fill = 0) %>%
    dplyr::select(DAUID, sort(current_vars()))

  da_unnest
}


# round percentages so tha tthey add to 100 using the largest remainders algorithm
# https://stackoverflow.com/questions/13483430/how-to-make-rounded-percentages-add-up-to-100
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





####
#'  Collect ONS data and DA-to-ONS data calculated using proportional assignment and single-link indicators
#'
#'  The Ottawa Neighbourhood Study (ONS) has custom geographies (neighbourhoods)
#'  that do not align with Statistics Canada's census regions. StatsCan has granular
#'  data and can run custom analyses to get census data for ONS's regions, but this
#'  is expensive and slow. ONS therefore wants to use public StatsCan data
#'
#'  This function creates a table that can be used to compare the Ottawa Neighbourhood
#'  Study's (ONS) "gold-standard" StatsCan-cut results to results computed using only public
#'  data. Two methods are performed: a one-to-many method where where DA-level data
#'  is assigned to a neighbourhood based on the relative extent to which it overlaps
#'  that neighbourhood's residential zones according to OpenStreetMaps (OSM); and
#'  a one-to-one method where each DA is assigned entirely to the single neighbourhood
#'  it most overlaps.
#'
#'  NOTE THAT THIS ONLY WORKS FOR COUNT DATA.
#'
#' @param sc_df A data frame containing Statistics Canada census data.
#' @param ons_data A data frame containing ONS's data.
#' @param da_ons_intersect A data frame containing DAUIDs and ONS_IDs and their proportional overlap.
#' @param sc_var_id The value of the column `TEXT_ID` in `sc_df` for the value of interest.
#' @param ons_var_id The value of the column `polygon_attribute` in `ons_data` for the value of interest.
#' @param var_type Unused; may be used if other types of data beyond count data are used.
#'
#' @return A data frame with the ONS gold-standard value, the value by method of
#'         proportional overlaps, and the single-link indicator value.
#' @export
get_values_for_comparison <- function(sc_df, ons_data, da_ons_intersect, sc_var_id, ons_var_id, var_type = c("count") ){

  var_type <- match.arg(var_type, var_type)
  #
  # # input census dataframe, here we're using labour
  # sc_df <- sc_labour2016
  #
  # # input ons dataframe, should always be ons_data, it's silly but i'm putting it here for absolute clarity
  # ons_data <- ons_data
  #
  # # statscan census TEXT_ID for  variable of interest, here we're using # unemployed
  # sc_var_id <- "31003"
  #
  # # ons_data polygon_attribute for variable of interest, again # unemployed
  # ons_var_id <- "CDP304"
  #
  # # dummy variable in case of future features: we're only doing simple counts for now
  # var_type <- "count"


  ## PREPARE: isolate the statscan variable of interest

  sc_var <- sc_df %>%
    dplyr::filter(TEXT_ID == sc_var_id) %>%
    dplyr::mutate(T_DATA_DONNEE = as.numeric(T_DATA_DONNEE)) %>%
    dplyr::select(TEXT_ID,
                  TEXT_NAME_NOM,
                  DAUID = GEO_ID,
                  T_DATA_DONNEE)



  ## FIRST: Compute city-wide differences

  sc_citywide <- sc_var %>%
    dplyr::summarise(total = sum(T_DATA_DONNEE, na.rm = TRUE)) %>%
    unlist()

  ons_citywide <- ons_data %>%
    dplyr::filter(polygon_attribute == ons_var_id,
                  ONS_ID == 0) %>%
    dplyr::pull(value)

  # get human-readable descriptions too
  sc_description <- stringr::str_squish(unique(sc_var$TEXT_NAME_NOM))

  ons_description <- ons_data %>%
    dplyr::filter(polygon_attribute == ons_var_id) %>%
    dplyr::mutate(ons_description = paste0(category1, " / ", category2, " / ", category3)) %>%
    dplyr::distinct(ons_description) %>%
    unlist()

  citywide <- dplyr::tibble(
    sc_var_id = sc_var_id,
    ons_var_id = ons_var_id,
    sc_description = sc_description,
    ons_description = ons_description,
    sc_citywide = sc_citywide,
    ons_citywide = ons_citywide)

  ## NEXT: compute neighbourhood-level differences

  # using proportional overlaps
  result_overlap <- da_ons_intersect %>%
    dplyr::left_join(sc_var, by = "DAUID") %>%
    dplyr::group_by(ONS_ID) %>%
    dplyr::summarise(value_overlap = sum(pct_overlap_rnd * T_DATA_DONNEE, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    # then create ottawa-wide value
    dplyr::bind_rows(
      dplyr::summarise(., value_overlap = sum(value_overlap, na.rm = TRUE)) %>%
        dplyr::mutate(ONS_ID = "0")
    ) %>%
    dplyr::arrange(ONS_ID)

  # using single-link indicator
  # create single-link indicator taking single ONS ID of maximum overlap
  # in cases of perfect tie, choose the first one listed
  da_ons_sli <- da_ons_intersect %>%
    dplyr::group_by(DAUID) %>%
    dplyr::arrange(dplyr::desc(pct_overlap)) %>%
    dplyr::slice_head(n = 1) %>%
    #  dplyr::filter(pct_overlap_rnd == max(pct_overlap_rnd)) %>%
    dplyr::mutate(pct_overlap_rnd = 1) %>%
    dplyr::select(DAUID, ONS_ID, pct_overlap_rnd)

  result_sli <- da_ons_sli %>%
    dplyr::left_join(sc_var, by = "DAUID") %>%
    dplyr::group_by(ONS_ID) %>%
    dplyr::summarise(value_sli = sum(pct_overlap_rnd * T_DATA_DONNEE, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(
      dplyr::summarise(., value_sli = sum(value_sli, na.rm = TRUE)) %>%
        dplyr::mutate(ONS_ID = "0")
    ) %>%
    dplyr::arrange(ONS_ID)

  ## Get ONS "gold standard"

  result_ons <- ons_data %>%
    dplyr::filter(polygon_attribute == ons_var_id) %>%
    dplyr::select(ONS_ID, value_ons = value) %>%
    dplyr::mutate(value_ons = dplyr::if_else(is.na(value_ons), 0, value_ons))


  ### PUT RESULTS TOGETHER, replace NAs with 0
  results <- result_ons %>%
    dplyr::left_join(result_overlap, by = "ONS_ID") %>%
    dplyr::left_join(result_sli, by = "ONS_ID") %>%
    dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) %>%
    dplyr::mutate(  sc_var_id = sc_var_id,
                    ons_var_id = ons_var_id,
                    sc_description = sc_description,
                    ons_description = ons_description,
                    .before = 1)

  return(results)
}


########### DO COMPARISONS
# mean/sd of differences? absolutes amd proportions
create_diff_table <- function(comp_table) {

  # get descriptions
  sc_description <- unique(comp_table$sc_description)
  ons_description <- unique(comp_table$ons_description)

  # get count differences and proportional (i.e. percent) diffs
  comp_table_diffs <- comp_table %>%
    filter(value_ons > 0) %>%
    #filter(as.numeric(ONS_ID) > 0) %>%
    mutate(overlap_diff_count = (value_overlap - value_ons),
           sli_diff_count = (value_sli- value_ons),
           overlap_diff_prop = overlap_diff_count/value_ons,
           sli_diff_prop = sli_diff_count/value_ons)

  # summarize across all neighbourhoods
  comp_table_summary_hoods <- comp_table_diffs %>%
    filter(as.numeric(ONS_ID) > 0) %>%
    summarise(overlap_diff_count_mean = mean(overlap_diff_count),
              overlap_diff_count_sd = sd(overlap_diff_count),
              overlap_diff_prop_mean = mean(overlap_diff_prop),
              overlap_diff_prop_sd = sd(overlap_diff_prop),
              sli_diff_count_mean = mean(sli_diff_count),
              sli_diff_count_sd = sd(sli_diff_count),
              sli_diff_prop_mean = mean(sli_diff_prop),
              sli_diff_prop_sd = sd(sli_diff_prop)
    ) %>%
    dplyr::mutate(scope = "neighbourhoods", .before = 1)

  # also create city-wide comparison
  comp_table_summary_city <- comp_table_diffs %>%
    filter(as.numeric(ONS_ID) == 0) %>%
    summarise(overlap_diff_count_mean = mean(overlap_diff_count),
              overlap_diff_count_sd = sd(overlap_diff_count),
              overlap_diff_prop_mean = mean(overlap_diff_prop),
              overlap_diff_prop_sd = sd(overlap_diff_prop),
              sli_diff_count_mean = mean(sli_diff_count),
              sli_diff_count_sd = sd(sli_diff_count),
              sli_diff_prop_mean = mean(sli_diff_prop),
              sli_diff_prop_sd = sd(sli_diff_prop)
    )  %>%
    dplyr::mutate(scope = "city", .before = 1)

  # put it all together and return it
  dplyr::bind_rows(comp_table_summary_city, comp_table_summary_hoods) %>%
    dplyr::bind_cols(dplyr::distinct(comp_table, sc_var_id, ons_var_id), .) %>%
    dplyr::mutate(sc_description = sc_description,
                  ons_description = ons_description,
                  .before = "scope")

}


# take all of the input data and, for variables defined in a csv file,
# estimate the values using proportional and SLI methods, then find average
# differences between the gold standard
run_comparison_analysis <- function(sc_labour2016, sc_pop2016, sc_immcitzn2016, sc_vismin2016, ons_data, da_ons_intersect) {
  vars_for_analysis <- readr::read_csv("inputs/vars_for_analysis.csv", col_types = readr::cols(.default = "c"))

  results <- dplyr::tibble()

  for (i in 1:nrow(vars_for_analysis)){
    sc_df <- get(vars_for_analysis$sc_df[[i]])
    #sc_df <- targets::tar_read(vars_for_analysis$sc_df[[i]])
    ons_df <- get(vars_for_analysis$ons_df[[i]])
    sc_var_id <- vars_for_analysis$sc_var_id[[i]]
    ons_var_id <- vars_for_analysis$ons_var_id[[i]]
    var_type <- vars_for_analysis$var_type[[i]]

    result <- get_values_for_comparison(sc_df = sc_df,
                                        ons_data = ons_df,
                                        da_ons_intersect = da_ons_intersect,
                                        sc_var_id = sc_var_id,
                                        ons_var_id = ons_var_id,
                                        var_type = "count") %>%
      create_diff_table()

    results <- dplyr::bind_rows(results, result)
  }


  results
}




# take all of the input data and, for variables defined in a csv file,
# estimate the values using proportional and SLI methods, then find average
# differences between the gold standard
analyze_variables_long <- function(sc_labour2016, sc_pop2016, sc_immcitzn2016, sc_vismin2016, ons_data, da_ons_intersect) {
  vars_for_analysis <- readr::read_csv("inputs/vars_for_analysis.csv", col_types = readr::cols(.default = "c"))

  results <- dplyr::tibble()

  for (i in 1:nrow(vars_for_analysis)){
    sc_df <- get(vars_for_analysis$sc_df[[i]])
    #sc_df <- targets::tar_read(vars_for_analysis$sc_df[[i]])
    ons_df <- get(vars_for_analysis$ons_df[[i]])
    sc_var_id <- vars_for_analysis$sc_var_id[[i]]
    ons_var_id <- vars_for_analysis$ons_var_id[[i]]
    var_type <- vars_for_analysis$var_type[[i]]

    result <- get_values_for_comparison(sc_df = sc_df,
                                        ons_data = ons_df,
                                        da_ons_intersect = da_ons_intersect,
                                        sc_var_id = sc_var_id,
                                        ons_var_id = ons_var_id,
                                        var_type = "count") %>%
      # get differences
      dplyr::mutate(overlap_diff_count = (value_overlap - value_ons),
                    sli_diff_count = (value_sli- value_ons),
                    overlap_diff_prop = overlap_diff_count/value_ons,
                    sli_diff_prop = sli_diff_count/value_ons)
    #%>% create_diff_table()

    results <- dplyr::bind_rows(results, result)
  }


  results
}


# summarize the results of analyze_variables_long into a table with one row per variable per scope

create_summary_table <- function(df_long){
  df_long %>%
    dplyr::mutate(scope = dplyr::if_else(ONS_ID == "0", "city", "neighbourhood")) %>%
    dplyr::group_by(sc_var_id, ons_var_id, ons_description, scope) %>%
    dplyr::mutate(across(where(is.numeric), function(x) dplyr::if_else(is.finite(x), x, NA_real_))) %>%
    tidyr::nest() %>%
    ungroup() %>%
    dplyr::mutate(data_summary = purrr::map(data, function(x) {
      x %>%
        summarise(overlap_diff_count_mean = mean(overlap_diff_count, na.rm = TRUE),
                  overlap_diff_count_sd = sd(overlap_diff_count, na.rm = TRUE),
                  overlap_diff_prop_mean = mean(overlap_diff_prop, na.rm = TRUE),
                  overlap_diff_prop_sd = sd(overlap_diff_prop, na.rm = TRUE),
                  sli_diff_count_mean = mean(sli_diff_count, na.rm = TRUE),
                  sli_diff_count_sd = sd(sli_diff_count, na.rm = TRUE),
                  sli_diff_prop_mean = mean(sli_diff_prop, na.rm = TRUE),
                  sli_diff_prop_sd = sd(sli_diff_prop, na.rm = TRUE)
        )
    })) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(cols = c(data_summary)) %>%
    dplyr::arrange(scope)

}
