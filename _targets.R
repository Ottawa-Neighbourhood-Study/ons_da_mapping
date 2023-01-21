library(targets)
tar_option_set(packages = c("tidyverse",
                            "osmdata",
                            "sf",
                            "onsr"))
source("R/functions.R")
source("R/greedy_sli_search.R")

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

  targets::tar_target(da_ons_sli,

                      da_ons_intersect %>%
                        dplyr::group_by(DAUID) %>%
                        dplyr::arrange(dplyr::desc(pct_overlap_rnd)) %>%
                        dplyr::slice_head(n=1) %>%
                        dplyr::ungroup()
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
                      create_summary_table(comparison_long)),


  ## for greedy optimization of single-link indicator
  targets::tar_target(goldstandard_values,

                      readr::read_csv("inputs/Profile_P1.csv") %>%
                        dplyr::filter(data_ID == "A1702") %>%
                        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
                        tidyr::pivot_longer(cols = -c(data_ID, description), values_to = "value", names_to = "ONS_ID") %>%
                        dplyr::mutate(dwellings_ons = as.numeric(value))),

  targets::tar_target(da_values,
                      sc_housing2016 %>%
                        dplyr::filter(TEXT_ID == "27034") %>%
                        dplyr::select(DAUID = GEO_ID,
                                      TEXT_ID,
                                      description = TEXT_NAME_NOM,
                                      value = T_DATA_DONNEE) %>%
                        dplyr::mutate(value = as.numeric(value))),

  targets::tar_target(sli_results_orig,
                      measure_sli_performance(da_ons_sli, da_values, goldstandard_values)),

  # optimize for mean absolute percentage error, 3 iterations
  targets::tar_target(da_ons_sli_opt_mape,

                      greedy_sli_search(da_ons_sli = da_ons_sli, da_ons_intersect = da_ons_intersect, da_ott = da_ott, ons_shp = ons_shp, hoods_to_analyze = sample(ons_shp$ONS_ID), da_values = da_values, goldstandard_values = goldstandard_values, optimize_for = "mape") %>%
                        greedy_sli_search(da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = sample(ons_shp$ONS_ID), da_values, goldstandard_values, optimize_for = "mape") %>%
                        greedy_sli_search(da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = sample(ons_shp$ONS_ID), da_values, goldstandard_values, optimize_for = "mape")
  ),

  # optimize for mean absolute error, 3 iterations
  targets::tar_target(da_ons_sli_opt_mae,

                      greedy_sli_search(da_ons_sli, da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = sample(ons_shp$ONS_ID), da_values, goldstandard_values, optimize_for = "mae") %>%
                        greedy_sli_search(da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = sample(ons_shp$ONS_ID), da_values, goldstandard_values, optimize_for = "mae") %>%
                        greedy_sli_search(da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = sample(ons_shp$ONS_ID), da_values, goldstandard_values, optimize_for = "mae")
  ),

  # optimize for mean squared error, 3 iterations
  targets::tar_target(da_ons_sli_opt_mse,

                      greedy_sli_search(da_ons_sli, da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = sample(ons_shp$ONS_ID), da_values, goldstandard_values, optimize_for = "mse", verbose = FALSE) %>%
                        greedy_sli_search(da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = sample(ons_shp$ONS_ID), da_values, goldstandard_values, optimize_for = "mse") %>%
                        greedy_sli_search(da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = sample(ons_shp$ONS_ID), da_values, goldstandard_values, optimize_for = "mse")
  ),

  targets::tar_target(sli_performance,
                      create_sli_performance (da_ons_intersect, da_values, goldstandard_values, da_ons_sli, da_ons_sli_opt_mae, da_ons_sli_opt_mape, da_ons_sli_opt_mse)
  ),

  targets::tar_target(sli_performance_othervars,
                      compare_da_ons_links(sc_labour2016, sc_pop2016, sc_immcitzn2016, sc_vismin2016, ons_data, da_ons_intersect, da_ons_sli, da_ons_sli_opt_mae, da_ons_sli_opt_mape, da_ons_sli_opt_mse)
                      ),

  tarchetypes::tar_render(sli_performance_report,
                          "Rmd/sli_performance.Rmd"),

  tarchetypes::tar_render(sli_performance_report2,
                          "Rmd/sli_performance2.Rmd"),

  ### Run SLI analysis using revised function
  # optimize for mean absolute percentage error, 3 iterations
  targets::tar_target(da_shp,
                      dplyr::left_join(da_ott, da_values)),

  targets::tar_target(ons_goldstandard_shp,
                      goldstandard_values %>%
                        dplyr::select(ONS_ID, value) %>%
                        dplyr::mutate(ONS_ID = as.character(ONS_ID)) %>%
                        {dplyr::left_join(dplyr::mutate(ons_shp, ONS_ID = as.character(ONS_ID)), ., by = "ONS_ID")} %>%
                        dplyr::select(ONS_ID, value) %>%
                        dplyr::mutate(value = as.numeric(value))
  ),

  targets::tar_target(da_ons_sli_opt_mape2,

                      greedy_sli_search2(input_sli = da_ons_sli,
                                         from_shp = da_shp, from_idcol = "DAUID", from_valuecol ="value",
                                         to_shp = ons_goldstandard_shp, to_idcol = "ONS_ID", to_valuecol = "value",
                                         optimize_for = "mape", tolerance = 0.05, verbose = FALSE) %>%
                        greedy_sli_search2(from_shp = da_shp, from_idcol = "DAUID", from_valuecol ="value",
                                           to_shp = ons_goldstandard_shp, to_idcol = "ONS_ID", to_valuecol = "value",
                                           optimize_for = "mape", tolerance = 0.05, verbose = FALSE) %>%
                        greedy_sli_search2(from_shp = da_shp, from_idcol = "DAUID", from_valuecol ="value",
                                           to_shp = ons_goldstandard_shp, to_idcol = "ONS_ID", to_valuecol = "value",
                                           optimize_for = "mape", tolerance = 0.05, verbose = FALSE)
  ),

  # optimize for mean absolute error, 3 iterations
  targets::tar_target(da_ons_sli_opt_mae2,

                      greedy_sli_search2(input_sli = da_ons_sli,
                                         from_shp = da_shp, from_idcol = "DAUID", from_valuecol ="value",
                                         to_shp = ons_goldstandard_shp, to_idcol = "ONS_ID", to_valuecol = "value",
                                         optimize_for = "mae", tolerance = 0.05, verbose = FALSE) %>%
                        greedy_sli_search2(from_shp = da_shp, from_idcol = "DAUID", from_valuecol ="value",
                                           to_shp = ons_goldstandard_shp, to_idcol = "ONS_ID", to_valuecol = "value",
                                           optimize_for = "mae", tolerance = 0.05, verbose = FALSE) %>%
                        greedy_sli_search2(from_shp = da_shp, from_idcol = "DAUID", from_valuecol ="value",
                                           to_shp = ons_goldstandard_shp, to_idcol = "ONS_ID", to_valuecol = "value",
                                           optimize_for = "mae", tolerance = 0.05, verbose = FALSE)
  ),

  # optimize for mean squared error, 3 iterations
  targets::tar_target(da_ons_sli_opt_mse2,
                      greedy_sli_search2(input_sli = da_ons_sli,
                                         from_shp = da_shp, from_idcol = "DAUID", from_valuecol ="value",
                                         to_shp = ons_goldstandard_shp, to_idcol = "ONS_ID", to_valuecol = "value",
                                         optimize_for = "mse", tolerance = 0.05, verbose = FALSE) %>%
                        greedy_sli_search2(from_shp = da_shp, from_idcol = "DAUID", from_valuecol ="value",
                                           to_shp = ons_goldstandard_shp, to_idcol = "ONS_ID", to_valuecol = "value",
                                           optimize_for = "mse", tolerance = 0.05, verbose = FALSE) %>%
                        greedy_sli_search2(from_shp = da_shp, from_idcol = "DAUID", from_valuecol ="value",
                                           to_shp = ons_goldstandard_shp, to_idcol = "ONS_ID", to_valuecol = "value",
                                           optimize_for = "mse", tolerance = 0.05, verbose = FALSE)
  ),

  targets::tar_target(sli_performance2,
                      create_sli_performance2 (da_ons_intersect, da_values, goldstandard_values, da_ons_sli, da_ons_sli_opt_mae2, da_ons_sli_opt_mape2, da_ons_sli_opt_mse2)
  ),
  # report with new function
  tarchetypes::tar_render(sli_performance_report3,
                          "Rmd/sli_performance3.Rmd"),

  # save SLIs
  targets::tar_target(save_slis, {
    readr::write_csv(da_ons_sli_opt_mae2, sprintf("outputs/da_ons_sli_opt_mae2_%s.csv", Sys.Date()))
    readr::write_csv(da_ons_sli_opt_mape2, sprintf("outputs/da_ons_sli_opt_mape2_%s.csv", Sys.Date()))
    readr::write_csv(da_ons_sli_opt_mse2, sprintf("outputs/da_ons_sli_opt_mse2_%s.csv", Sys.Date()))


  }),


  ######## EXPERIMENT 3, OPTIMIZING BASED ON POPULATION COUNTS

  ## for greedy optimization of single-link indicator

  # extract the official 2016 ONS populations
  targets::tar_target(goldstandard_values_pop2016,{
                      tempdata <- ons_data %>%
                        dplyr::filter(polygon_attribute == "pop2016") %>%
                        dplyr::select(ONS_ID, ons_pop2016 = value) %>%
                        dplyr::mutate(ons_pop2016 = as.numeric(ons_pop2016))


                      ons_shp %>%
                        dplyr::mutate(ONS_ID = as.character(ONS_ID)) %>%
                        left_join(tempdata)%>%
                        dplyr::mutate(ons_pop2016 = dplyr::if_else(is.na(ons_pop2016), 0, ons_pop2016)) %>%
                        dplyr::filter( ons_pop2016 != 0) # NOTE! Removing rows with 0 so that we never assign them any values.
  }
                      ),

  targets::tar_target(da_values_pop2016,{
                        tempdata <- sc_pop2016 %>%
                          dplyr::filter(HIER_ID == "1.1.1") %>%
                          dplyr::mutate(T_DATA_DONNEE = as.numeric(T_DATA_DONNEE)) %>%
                          dplyr::select(DAUID = GEO_ID, da_pop2016 = T_DATA_DONNEE)

                        da_shp %>%
                          left_join(tempdata)
                        }
                        ),

  targets::tar_target(sli_results_orig_pop2016,
                      measure_sli_performance3(sli_for_test = da_ons_sli,
                                               da_values =  da_values_pop2016, da_values_column = "da_pop2016",
                                               goldstandard_values = goldstandard_values_pop2016, goldstandard_values_column =  "ons_pop2016")),


  targets::tar_target(da_ons_sli_opt_pop2016_mape,

                      greedy_sli_search2(input_sli = da_ons_sli,
                                         from_shp = da_values_pop2016, from_idcol = "DAUID", from_valuecol ="da_pop2016",
                                         to_shp = goldstandard_values_pop2016, to_idcol = "ONS_ID", to_valuecol = "ons_pop2016",
                                         optimize_for = "mape", tolerance = 0.05, verbose = FALSE) %>%
                        greedy_sli_search2(from_shp = da_values_pop2016, from_idcol = "DAUID", from_valuecol ="da_pop2016",
                                           to_shp = goldstandard_values_pop2016, to_idcol = "ONS_ID", to_valuecol = "ons_pop2016",
                                           optimize_for = "mape", tolerance = 0.05, verbose = FALSE) %>%
                        greedy_sli_search2(from_shp = da_values_pop2016, from_idcol = "DAUID", from_valuecol ="da_pop2016",
                                           to_shp = goldstandard_values_pop2016, to_idcol = "ONS_ID", to_valuecol = "ons_pop2016",
                                           optimize_for = "mape", tolerance = 0.05, verbose = FALSE)
  ),

  targets::tar_target(da_ons_sli_opt_pop2016_mae,

                      greedy_sli_search2(input_sli = da_ons_sli,
                                         from_shp = da_values_pop2016, from_idcol = "DAUID", from_valuecol ="da_pop2016",
                                         to_shp = goldstandard_values_pop2016, to_idcol = "ONS_ID", to_valuecol = "ons_pop2016",
                                         optimize_for = "mae", tolerance = 0.05, verbose = FALSE) %>%
                        greedy_sli_search2(from_shp = da_values_pop2016, from_idcol = "DAUID", from_valuecol ="da_pop2016",
                                           to_shp = goldstandard_values_pop2016, to_idcol = "ONS_ID", to_valuecol = "ons_pop2016",
                                           optimize_for = "mae", tolerance = 0.05, verbose = FALSE) %>%
                        greedy_sli_search2(from_shp = da_values_pop2016, from_idcol = "DAUID", from_valuecol ="da_pop2016",
                                           to_shp = goldstandard_values_pop2016, to_idcol = "ONS_ID", to_valuecol = "ons_pop2016",
                                           optimize_for = "mae", tolerance = 0.05, verbose = FALSE)
  ),
  targets::tar_target(da_ons_sli_opt_pop2016_mse,

                      greedy_sli_search2(input_sli = da_ons_sli,
                                         from_shp = da_values_pop2016, from_idcol = "DAUID", from_valuecol ="da_pop2016",
                                         to_shp = goldstandard_values_pop2016, to_idcol = "ONS_ID", to_valuecol = "ons_pop2016",
                                         optimize_for = "mse", tolerance = 0.05, verbose = FALSE) %>%
                        greedy_sli_search2(from_shp = da_values_pop2016, from_idcol = "DAUID", from_valuecol ="da_pop2016",
                                           to_shp = goldstandard_values_pop2016, to_idcol = "ONS_ID", to_valuecol = "ons_pop2016",
                                           optimize_for = "mse", tolerance = 0.05, verbose = FALSE) %>%
                        greedy_sli_search2(from_shp = da_values_pop2016, from_idcol = "DAUID", from_valuecol ="da_pop2016",
                                           to_shp = goldstandard_values_pop2016, to_idcol = "ONS_ID", to_valuecol = "ons_pop2016",
                                           optimize_for = "mse", tolerance = 0.05, verbose = FALSE)
  ),


  targets::tar_target(sli_performance_pop2016,
                      create_sli_performance3 (da_ons_intersect, da_values_pop2016, da_values_column = "da_pop2016", goldstandard_values_pop2016, "ons_pop2016", da_ons_sli, da_ons_sli_opt_pop2016_mae, da_ons_sli_opt_pop2016_mape, da_ons_sli_opt_pop2016_mse)
  ),
NULL
)
