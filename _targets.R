library(targets)
tar_option_set(packages = c("tidyverse",
                            "osmdata",
                            "onsr"))
source("R/functions.R")

list(
  targets::tar_target(ons_data,
                      onsr::get_ons_data()),

  targets::tar_target(ons_shp,
                      onsr::ons_shp %>%
                        sf::st_make_valid() %>%
                        sf::st_transform(32189)),

  targets::tar_target(ott_osm_res_poly,
                      get_osm_ottawa_residential_polygons(ons_shp = ons_shp)
  ),

  targets::tar_target(osm_res,
                      sf::st_make_valid(ott_osm_res_poly)),


  # get the DAs in Ottawa
  targets::tar_target(da_ott,
                      get_ottawa_das(year = 2016)),

  # trim the ONS neighbourhoods to the OSM residential zones, removing 3
  # neighbourhoods with no residents
  targets::tar_target(ons_trim,
                      ons_shp %>%
                        dplyr::filter(!ONS_ID %in% c(5, 17, 71)) %>%
                        sf::st_intersection(osm_res)),

  # get % overlaps of DAs with residential regions of ONS hoods.
  # Each DA % sums to 100%. Each % is the % of *overlapping* DA area in each
  # neighbourhood.
  # with 2021 DAs, 11 DAs are missing because they do not intersect any residential regions.
  # They are mostly forests/swamps, but a few are in Centretown.
  # DAUIDS:  "35061134" "35060338" "35061481" "35061501" "35061507" "35061125" "35061122" "35061142" "35061788" "35061792" "35061912"
  # With 2016 DAs, there are 12 that do not intersect any.
  targets::tar_target(da_ons_intersect_raw,
                      get_da_ons_intersection(da_ott, ons_trim)),

  # Deal with the 11 DAs that do not overlap any residential areas.
  # For them, run the same algorithm but using the *full* ONS boundaries.
  # Results will be %s that overlap the full ONS neighbourhoods.
  targets::tar_target(da_ons_intersect,
                      da_ott %>%
                        dplyr::filter(!DAUID %in% da_ons_intersect_raw$DAUID) %>%
                        get_da_ons_intersection(ons_trim = ons_shp) %>%
                        dplyr::bind_rows(da_ons_intersect_raw) %>%
                        dplyr::mutate(dplyr::across(dplyr::everything(), function(x) dplyr::if_else(is.na(x), 0, x))) %>%
                        pivot_longer(cols = -DAUID, names_to = "ONS_ID", values_to = "pct_overlap") %>%
                        filter(pct_overlap > 0) %>%
                        group_by(DAUID) %>%
                        mutate(pct_overlap_rnd = largeRem(pct_overlap*100)/100)
  ),

  # save in a few different formats, some more sensible than others
  targets::tar_target(save_outputs,
                      {
                        # readr::write_csv(da_ons_intersect, sprintf("outputs/da_ons_intersect_wide_%s.csv", Sys.Date()))

                        #     da_ons_long <- da_ons_intersect %>%
                        #   tidyr::pivot_longer(cols = -DAUID, names_to = "ONS_ID", values_to = "intersect_pct")

                        #  readr::write_csv(da_ons_long, sprintf("outputs/da_ons_intersect_long_all_%s.csv", Sys.Date()))

                        da_ons_intersect %>%
                          dplyr::filter(pct_overlap_rnd != 0) %>%
                          readr::write_csv(sprintf("outputs/da_ons_intersect_long_nonzero_%s.csv", Sys.Date()))

                        # return the time the last values were saved
                        Sys.time()
                      }),

  targets::tar_target(sc_pop2016,
                      onsr::census_make_dguid(data = da_ott$DAUID, geouid_type = "DAUID") %>%
                        onsr::census_get_data(topic = 13)),

  targets::tar_target(sc_labour2016,
                      onsr::census_make_dguid(data = da_ott$DAUID, geouid_type = "DAUID") %>%
                        onsr::census_get_data(topic = 9)),

  targets::tar_target(sc_housing2016,
                      onsr::census_make_dguid(data = da_ott$DAUID, geouid_type = "DAUID") %>%
                        onsr::census_get_data(topic = 5)),

  targets::tar_target(sc_vismin2016,
                      onsr::census_make_dguid(data = da_ott$DAUID, geouid_type = "DAUID") %>%
                        onsr::census_get_data(topic = 14)),


  targets::tar_target(sc_immcitzn2016,
                      onsr::census_make_dguid(data = da_ott$DAUID, geouid_type = "DAUID") %>%
                        onsr::census_get_data(topic = 6)),

  targets::tar_target(sc_pop2016total,
                      dplyr::filter(sc_pop2016, TEXT_ID == "1000")),


  # take all of the input data and, for variables defined in a csv file,
  # estimate the values using proportional and SLI methods, then find average
  # differences between the gold standard
  targets::tar_target(comparison_long, {

    #run_comparison_analysis(sc_labour2016 = sc_labour2016, sc_pop2016 = sc_pop2016, sc_immcitzn2016 = sc_immcitzn2016, sc_vismin2016 = sc_vismin2016, ons_data = ons_data, da_ons_intersect = da_ons_intersect)
    analyze_variables_long(sc_labour2016 = sc_labour2016, sc_pop2016 = sc_pop2016, sc_immcitzn2016 = sc_immcitzn2016, sc_vismin2016 = sc_vismin2016, ons_data = ons_data, da_ons_intersect = da_ons_intersect)
  }),

  targets::tar_target(comparison_summary_table,
                      create_summary_table(comparison_long))

)
