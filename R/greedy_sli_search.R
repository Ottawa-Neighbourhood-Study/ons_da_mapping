# Starting with the most-error-on-average ONS neighbourhoods,
#   For each DA that intersects its residential area at all, NOTE!! should it just be intersection of undifferentiated area for this???
#     Compute mean errors FOR EACH assignment of that DA TO EACH ONS neighbourhood it intersects at all,
#     Assign that DA to the ONS neighbourhood that results in the lowest mean SLI error.

# targets::tar_load(da_ott)
# targets::tar_load(ons_shp)
# targets::tar_load(da_ons_intersect)



# hoods_to_analyze: character vector of ONS_IDs
# da_values and goldstandard_values must be tibbles wiht columns "value",
# and  DAUID or ONS_ID respectively
greedy_sli_search <- function(da_ons_sli, da_ons_intersect, da_ott, ons_shp, hoods_to_analyze, da_values, goldstandard_values, optimize_for = c("mape", "mae", "mse"), verbose = FALSE){

  optimize_for <- match.arg(optimize_for, optimize_for)

  # pre-compute hood intersections. this takes ~250ms up front and saves ~30s per DA..
  # we can then access results via indexing a named list
  da_ons_intersections <- da_hoods_precompute(ons_shp, da_ott)

  # keep track of iterations and results
  optimization_progress <- dplyr::tibble()

  # create SLI from input overlapping data
  # da_ons_sli <- da_ons_intersect %>%
  #   group_by(DAUID) %>%
  #   arrange(desc(pct_overlap_rnd)) %>%
  #   slice_head(n=1) %>%
  #   mutate(pct_overlap_rnd = 1)

  ## Get ONS "gold standard"
  result_ons <- goldstandard_values %>%
    dplyr::select(ONS_ID, value_ons = value) %>%
    dplyr::mutate(value_ons = as.numeric(value_ons))

  hoods_to_analyze <- as.character(hoods_to_analyze)

  # if we're sorting all of them, sort by error with original sli
  if (hoods_to_analyze == "sort"){
    sort_order <- get_sli_diffs(da_ons_sli, da_values, result_ons, do_not_summarize = TRUE)

    if (optimize_for == "mape") {
      hoods_to_analyze <- sort_order %>%
        dplyr::arrange(dplyr::desc(abs(sli_diff_prop))) %>%
        pull(ONS_ID)
    }

    if (optimize_for == "mae") {
      hoods_to_analyze <- sort_order %>%
        dplyr::arrange(dplyr::desc(abs(sli_diff_count))) %>%
        pull(ONS_ID)
    }
  }


  da_ons_sli_temp <- da_ons_sli

  da_ons_sli_new <- da_ons_sli

  # to quiet warning as per https://github.com/r-spatial/sf/issues/406
  sf::st_agr(ons_shp) <- "constant"
  sf::st_agr(da_ott) <- "constant"


  tictoc::tic()
  for (i in 1:length(hoods_to_analyze)){
    hood <- hoods_to_analyze[[i]]

    message(sprintf("%s/%s: Neighbourhood %s", i, length(hoods_to_analyze), hood))


    #if(verbose) message(paste0("Optimizing neighbourhood ", hood))

    # get all DAs that intersect this neighbourhood
    # FIXME this is also slow
    das_in_hood <- da_ons_intersect %>%
      dplyr::filter(ONS_ID == hood) %>%
      dplyr::pull(DAUID)


    for (da in das_in_hood){

      # check to see if DA has valid data, if it does not, skip it
      if (is.na(da_values[da_values$DAUID == da,]$value)) next

      if(verbose) message(paste0("  Checking all assignments for DA ", da))

      # get DA shape DON"T NEED THIS IS WE ARE INDEXING PRECOMPUTED LIST
      #da_shp <- dplyr::filter(da_ott, DAUID == da)

      # to quiet warning as per https://github.com/r-spatial/sf/issues/406
      #sf::st_agr(da_shp) <- "constant"

      # find ONS_IDs of neighbourhoods that intersect the DA
      da_hoods <- da_ons_intersections[da][[1]]

      # da_hoods <- sf::st_intersection(ons_shp, da_shp) %>%
      #   dplyr::pull(ONS_ID) %>%
      #   as.character()

      hood_results <- dplyr::tibble()

      for (da_hood in da_hoods){



        # Now uses adjusted SLI
        da_ons_sli_temp <- da_ons_sli_new

        # update the SLI for analysis to use this ONS ID
        da_ons_sli_temp[da_ons_sli_temp$DAUID == da,]$ONS_ID <- da_hood

        #var_results <- c()

        ## NEXT: compute neighbourhood-level differences

        result <- get_sli_diffs(da_ons_sli_temp, da_values, result_ons)

        # var_results <- c(var_results, result$sli_diff_prop_mean)


        if(verbose) if (optimize_for == "mape" ) message(sprintf("    %s: mean proportional difference %.2f%%", da_hood, 100*result$sli_diff_mape))
        if(verbose)  if (optimize_for == "mae"  ) message(sprintf("    %s: mean absolute error difference %.1f", da_hood, result$sli_diff_mae))


        hood_results <- dplyr::bind_rows(hood_results,
                                         dplyr::tibble(ONS_ID = da_hood) %>%
                                           dplyr::bind_cols(result)
        )
      } # end for da_hood in da_hoods






      # assign hood to DA with the best results
      # NOTE! Replacing ~3ms dplyr operation with ~222us base operation
      hood_results$abs_result <- unlist(abs(hood_results[paste0("sli_diff_", optimize_for)]))

      best_hood <- hood_results[hood_results$abs_result == min(hood_results$abs_result),]$ONS_ID

      # if (optimize_for == "mape"){
      #   #message ("proportion mena")
      #   best_hood <- hood_results %>%
      #     dplyr::mutate(abs_diff = abs(sli_diff_mape)) %>%
      #     dplyr::arrange(abs_diff) %>%
      #     dplyr::slice_head(n=1) %>%
      #     dplyr::pull(ONS_ID)
      #
      # }
      #
      #
      # if (optimize_for == "mae"){
      #   best_hood <- hood_results %>%
      #     dplyr::mutate(abs_diff = abs(sli_diff_mae)) %>%
      #     dplyr::arrange(abs_diff) %>%
      #     dplyr::slice_head(n=1) %>%
      #     dplyr::pull(ONS_ID)
      # }
      #
      # if (optimize_for == "mse"){
      #   best_hood <- hood_results %>%
      #     dplyr::mutate(abs_diff = abs(sli_diff_mse)) %>%
      #     dplyr::arrange(abs_diff) %>%
      #     dplyr::slice_head(n=1) %>%
      #     dplyr::pull(ONS_ID)
      # }

      if(verbose) message(paste0("     Best neighbourhood assignment for DAUID ", da, ": ", best_hood))


      # if we get one best unique answer, we use that one. otherwise don't update, it's probably an NA-valued DA
      if (length(best_hood) == 1) da_ons_sli_new[da_ons_sli_new$DAUID == da, ]$ONS_ID <- best_hood

    } # end for da in hood

    if (optimize_for == "mape" ) message(sprintf("    %s: best mean absolute proportional error %.2f%%", da_hood, 100*min(abs(hood_results$sli_diff_mape))))
    if (optimize_for == "mae" ) message(sprintf("    %s: best mean absolute error  %.1f", da_hood, min(abs(hood_results$sli_diff_mae))))
    if (optimize_for == "mse" ) message(sprintf("    %s: best mean squared error  %.1f", da_hood, min(abs(hood_results$sli_diff_mse))))

    # keep track of progress
    optimization_progress <- dplyr::bind_rows(optimization_progress,
                                              dplyr::tibble(iteration = i, metric = optimize_for, value =  min(abs(hood_results[paste0("sli_diff_", optimize_for)]))))

  } # end for hood in hoods to analyze

  # save optimization results to file
  readr::write_csv(optimization_progress, sprintf("outputs/optimization_trials/%s-%s.csv", optimize_for, Sys.time()))

  result <- da_ons_sli_new %>%
    dplyr::select(DAUID, ONS_ID)

  # set optimization progress as result metadata
  attr(result, "optimization_progress") <- optimization_progress

  # return the sli
  return (result)

} # end of function




# take an SLI and compare its results to the gold standard

measure_sli_performance <- function(sli_for_test, da_values, goldstandard_values){
  sli_created_results <-  da_values %>%
    left_join(sli_for_test) %>%
    group_by(ONS_ID) %>%
    summarise(dwellings_sli = sum(value, na.rm = TRUE))  %>%
    mutate(across(where(is.numeric), tidyr::replace_na, 0))

  compare_values <- goldstandard_values %>%
    left_join(sli_created_results, by = "ONS_ID") %>%
    mutate(diff_count = dwellings_sli - dwellings_ons,
           diff_prop = diff_count/dwellings_ons) %>%
    mutate(across(where(is.numeric), tidyr::replace_na, 0))

  compare_values %>%
    summarise(diff_count_mean = mean(diff_count),
              diff_count_sd = sd(diff_count),
              diff_count_abs_mean = mean(abs(diff_count)),
              diff_count_abs_sd = mean(abs(diff_count)),
              diff_prop_mean = mean(diff_prop),
              diff_prop_sd = sd(diff_prop),
              diff_prop_abs_mean = mean(abs(diff_prop)),
              diff_prop_abs_sd = sd(abs(diff_prop))
    ) %>%
    mutate(raw_comparison = list(compare_values))
}


measure_weighted_performance <- function(da_ons_intersect, da_values, goldstandard_values){
  sli_created_results <-  da_values %>%
    left_join(da_ons_intersect, by = "DAUID") %>%
    mutate(value = value * pct_overlap) %>%
    group_by(ONS_ID) %>%
    summarise(dwellings_sli = sum(value, na.rm = TRUE))  %>%
    mutate(across(where(is.numeric), tidyr::replace_na, 0))

  compare_values <- goldstandard_values %>%
    left_join(sli_created_results, by = "ONS_ID") %>%
    mutate(diff_count = dwellings_sli - dwellings_ons,
           diff_prop = diff_count/dwellings_ons) %>%
    mutate(across(where(is.numeric), tidyr::replace_na, 0))

  compare_values %>%
    summarise(mae = mean(abs(diff_count)),
              #     mae_sd = sd(abs(diff_count)),
              mape = mean(abs(diff_prop)),
              #     mape_sd = sd(abs(diff_prop)),
              mse = mean(diff_count^2)
    ) %>%
    mutate(raw_comparison = list(compare_values))
}

measure_sli_performance2 <- function(sli_for_test, da_values, goldstandard_values){
  sli_created_results <-  da_values %>%
    left_join(sli_for_test, by = "DAUID") %>%
    group_by(ONS_ID) %>%
    summarise(dwellings_sli = sum(value, na.rm = TRUE))  %>%
    mutate(across(where(is.numeric), tidyr::replace_na, 0))

  compare_values <- goldstandard_values %>%
    left_join(sli_created_results, by = "ONS_ID") %>%
    mutate(across(where(is.numeric), tidyr::replace_na, 0))%>%
    mutate(diff_count = dwellings_sli - dwellings_ons,
           diff_prop = diff_count/dwellings_ons)

  compare_values %>%
    summarise(mae = mean(abs(diff_count)),
              #  mae_sd = mean(abs(diff_count)),
              mape = mean(abs(diff_prop)),
              #   mape_sd = sd(abs(diff_prop)),
              mse = mean(diff_count^2)
    ) %>%
    mutate(raw_comparison = list(compare_values))
}


# function to compare SLI results with gold-standard results
get_sli_diffs <- function(da_ons_sli_temp, da_values, result_ons, do_not_summarize = FALSE) {
  # using single-link indicator
  result_sli <- da_ons_sli_temp %>%
    dplyr::left_join(da_values, by = "DAUID") %>%
    dplyr::group_by(ONS_ID) %>%
    dplyr::summarise(value_sli = sum(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ONS_ID)



  ### PUT RESULTS TOGETHER, replace NAs with 0
  result <- result_ons %>%
    dplyr::left_join(result_sli, by = "ONS_ID") %>%
    dplyr::mutate(value_sli = dplyr::if_else(is.na(value_sli), 0, value_sli)) %>%

    # calculate differences
    dplyr::mutate(sli_diff_count = (value_sli - value_ons),
                  sli_diff_prop = sli_diff_count/value_ons) %>%
    dplyr::mutate(sli_diff_prop = dplyr::if_else(value_ons == value_sli, 0, sli_diff_prop)) %>% # to handle 0/0 = NaN
    dplyr::mutate(sli_diff_prop = dplyr::if_else(sli_diff_prop > 5, 5, sli_diff_prop)) %>%      # to handle 0/n = Inf
    dplyr::mutate(sli_diff_prop = dplyr::if_else(sli_diff_prop < -5, -5, sli_diff_prop)) %>%      # to handle -0/n = -Inf
    dplyr::filter(ONS_ID != "0") %>%
    dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) %>%
    dplyr::arrange(dplyr::desc((ONS_ID)))

  if (!do_not_summarize) {
    result <- result %>%
      # summarise
      summarise(
        sli_diff_mae = mean(abs(sli_diff_count)),
        #sli_diff_count_mean = mean(abs(sli_diff_count)),
        #sli_diff_count_max = max(abs(sli_diff_count)),
        sli_diff_count_sd = sd(abs(sli_diff_count)),
        sli_diff_mape = mean(abs(sli_diff_prop)),
        #sli_diff_prop_mean = mean(abs(sli_diff_prop)),
        #sli_diff_prop_max = max(abs(sli_diff_prop)),
        sli_diff_prop_sd = sd(abs(sli_diff_prop)),
        sli_diff_mse = mean(sli_diff_count^2)
      )
  }

  return(result)
}



plot_sli_performance <- function(comparison, title = NA, plot_type = c("absolute", "proportional")) {
  plot_type <- match.arg(plot_type, plot_type)

  if (plot_type == "absolute"){
    result <- comparison %>%
      ggplot() +
      geom_point(aes(x=dwellings_ons, y=dwellings_sli)) +
      geom_abline(slope = 1, intercept = 0) +
      # scale_y_continuous(limits = c(-2000, 2000)) +
      ggtitle(title)
  }

  if (plot_type == "proportional"){
    result <- comparison %>%
      ggplot() +
      geom_point(aes(x=ONS_ID, y=diff_prop)) +
      geom_abline(slope = 0, intercept = 0) +
      scale_y_continuous(limits = c(-1, 1.1)) +
      ggtitle(title)
  }

  return(result)
}


leaflet_sli_performance <- function(comparison, ons_shp, var = c("diff_count", "diff_prop")){

  # set plot colour ranges based on what we know about inputs
  # so they'll be constant across maps
  if (var == "diff_count")  pal_domain <- c(-2000,2000)
  if (var == "diff_prop")  pal_domain <- c(-1.2,1.2)


  var <- match.arg(var, var)

  forplot <- ons_shp %>%
    sf::st_transform(crs = "WGS84") %>%
    dplyr::mutate(ONS_ID = as.character(ONS_ID)) %>%
    dplyr::left_join(comparison, by = "ONS_ID") %>%
    dplyr::rename(plot_var := {{var}})

  pal <- leaflet::colorNumeric(palette = "viridis",
                               domain = pal_domain #c(min(forplot$plot_var, na.rm = TRUE),#max(forplot$plot_var, na.rm = TRUE)))
  )

  if (var == "diff_prop"){
    forplot <- forplot %>%
      dplyr::mutate(labels = sprintf("%s (%s)<br>%% Difference: %.1f%%", Name, ONS_ID,  100*plot_var) %>%
                      purrr::map(htmltools::HTML))
  }


  if (var == "diff_count"){
    forplot <- forplot %>%
      dplyr::mutate(labels = sprintf("%s (%s)<br>Count Difference: %s", Name, ONS_ID,  plot_var) %>%
                      purrr::map(htmltools::HTML))
  }

  forplot %>%
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(weight = 1,
                         fillColor = ~ pal(plot_var),
                         fillOpacity = 0.9,
                         label = ~ labels
                         #label = ~ purrr::map(sprintf("%s (%s)<br>%s: %s", Name, ONS_ID, var, plot_var), htmltools::HTML)
    ) %>%
    leaflet::addScaleBar(position = "bottomright") %>%
    leaflet::addLegend(position = "bottomleft", pal = pal, values = pal_domain, # ~plot_var,
                       title = var)


}




# function to compare ons data with computed data using an arbitrary number of
# weighted or SLI data frames
compare_da_ons_links <- function(sc_labour2016, sc_pop2016, sc_immcitzn2016, sc_vismin2016, ons_data, ... ) {

  print(6)

  # https://stackoverflow.com/questions/51259346/how-to-get-names-of-dot-dot-dot-arguments-in-r
  # get the names of the links provided, put them into a column
  sli_names <- lapply(substitute(list(...))[-1], deparse)
  slis <- list(...) %>%
    purrr::map2(sli_names, function(x, y) dplyr::mutate(x, link_name = y ))

  # load the csv file that determines the variables
  vars_for_analysis <- readr::read_csv("inputs/vars_for_analysis.csv") #%>%    head(1)

  # extract the statscan and ons variables we're going to analyse
  sc_vars <- dplyr::bind_rows(sc_labour2016, sc_pop2016, sc_immcitzn2016, sc_vismin2016) %>%
    dplyr::filter(TEXT_ID %in% vars_for_analysis$sc_var_id)

  ons_vars <- ons_data %>%
    dplyr::filter(polygon_attribute %in% vars_for_analysis$ons_var_id)

  # results go here
  results <- dplyr::tibble()

  # for each sli, for each variable, compute results using the DA data and
  # compare it to the ONS data. return summaries and the raw comparisons
  for (sli in slis){

    sli_name <- unique(sli$link_name)

    message(sli_name)


    # set it up so that we can use same code for weighted and SLI
    # if sli has no weighted column, add one that's all 1s
    if (!"pct_overlap" %in% names(sli)) sli$pct_overlap <- 1

    for (i in 1:nrow(vars_for_analysis)){

      sc_var_id <- vars_for_analysis$sc_var_id[[i]]
      ons_var_id <- vars_for_analysis$ons_var_id[[i]]

      sc_var <- sc_vars %>%
        dplyr::filter(TEXT_ID == sc_var_id) %>%
        dplyr::rename(DAUID = GEO_ID)

      ons_var <- ons_vars %>%
        dplyr::filter(polygon_attribute == ons_var_id) %>%
        dplyr::filter(ONS_ID != "0") %>%
        dplyr::select(polygon_attribute, ONS_ID, category3, value_ons = value) %>%
        dplyr::mutate(value_ons = dplyr::if_else(is.na(value_ons), 0, value_ons))

      sli_prediction <- sli %>%
        dplyr::left_join(sc_var, by = "DAUID") %>%
        dplyr::select(DAUID, ONS_ID, pct_overlap, value = T_DATA_DONNEE, link_name) %>%
        dplyr::mutate(value_weighted = as.numeric(value) * pct_overlap) %>%
        dplyr::group_by(ONS_ID) %>%
        dplyr::summarise(value_sum = sum(value_weighted, na.rm = TRUE))

      comparison_hoods <- ons_var %>%
        dplyr::left_join(sli_prediction, by = "ONS_ID") %>%
        dplyr::mutate(value_sum = dplyr::if_else(is.na(value_sum), 0, value_sum)) %>%
        dplyr::mutate(diff_count = value_sum - value_ons,
                      diff_prop = diff_count / value_ons,
                      diff_prop = dplyr::if_else(!is.finite(diff_prop), 0, diff_prop)) %>%
        dplyr::mutate(sc_var_id = sc_var_id,
                      sc_var_name = unique(sc_var$TEXT_NAME_NOM),
                      ons_var_name = unique(ons_var$category3)) %>%
        dplyr::rename(ons_var_id = polygon_attribute)

      result <- comparison_hoods %>%
        group_by(ons_var_id, sc_var_id, sc_var_name, ons_var_name) %>%
        dplyr::summarise(.groups = "drop",
                         mae = mean(abs(diff_count)),
                         mape = mean(abs(diff_prop)),
                         mse = mean(diff_count^2)) %>%
        dplyr::mutate(link_name = sli_name, .before = 1) %>%
        dplyr::mutate(raw_comparison = list(comparison_hoods))


      results <- dplyr::bind_rows(results, result)
    } # end for var in analysis
  }# end for sli in slis

  results

}



# pre-compute the ONS hoods each DA intersects and return it as a named list
da_hoods_precompute <- function(ons_shp, da_ott){

  ons_ids <- ons_shp$ONS_ID

  da_ons_intersections <- sf::st_intersects(da_ott, ons_shp) %>%
    purrr::map(function(x) as.character(ons_ids[x]))

  names(da_ons_intersections) <- da_ott$DAUID

  # da_ons_intersections["35060145"]
  #
  # da_ott[1:5,] %>%
  #   sf::st_intersection(ons_shp) %>%
  #   select(DAUID, ONS_ID) %>%
  #   arrange(DAUID)

  return(da_ons_intersections)

}


#bench::mark(da_hoods_precompute(ons_shp, da_ott))

#bench::mark(da_ons_intersections["35060145"])




create_sli_performance <- function(da_ons_intersect, da_values, goldstandard_values, da_ons_sli, da_ons_sli_opt_mae, da_ons_sli_opt_mape, da_ons_sli_opt_mse) {

  measure_weighted_performance(da_ons_intersect, da_values, goldstandard_values) %>%
    dplyr::mutate(method = "Weighted Link", .before = 1) %>%
    dplyr::bind_rows(
      measure_sli_performance2(da_ons_sli, da_values, goldstandard_values) %>%
        dplyr::mutate(method = "SLI unoptimized", .before = 1)
    ) %>%
    dplyr::bind_rows(
      measure_sli_performance2(da_ons_sli_opt_mae, da_values, goldstandard_values) %>%
        dplyr::mutate(method = "SLI mean absolute error", .before = 1)
    ) %>%
    dplyr::bind_rows(
      measure_sli_performance2(da_ons_sli_opt_mape, da_values, goldstandard_values) %>%
        dplyr::mutate(method = "SLI mean absolute percentage error", .before = 1)
    ) %>%
    dplyr::bind_rows(
      measure_sli_performance2(da_ons_sli_opt_mse, da_values, goldstandard_values) %>%
        dplyr::mutate(method = "SLI mean squared error", .before = 1)
    )

}



#' Generate one-to-many weighted links between sets of spatial regions
#'
#' @param from_shp An `sf` object with the geometries we are linking from.
#' @param from_idcol A length-one character vector with the name of the `from_shp` column with unique region identifiers.
#' @param to_shp An `sf` object with the geometries we are linking to.
#' @param to_idcol A length-one character vector with the name of the `to_shp` column with unique region identifiers.
#'
#' @return A `tbl_df` with three columns, showing the linked-from region ids, the linked-to region ids, and the weights for each link. Each from-id set of weights should sum to 1.
#' @export
#'
#' @example
#' \dontrun{generate_weighted_links(da_ott, "DAUID", ons_shp, "ONS_ID")}
generate_weighted_links <- function(from_shp, from_idcol, to_shp, to_idcol){

  # Basic input validation
  # Are our input shapefiles really shapefiles?
  if (!"sf" %in% class(from_shp) | !"sf" %in% class(to_shp)) {
    stop("Input variables from_shp and to_shp must both be simple feature objects with class sf.")
  }

  # Do the id columns exist in the input files?
  if (!from_idcol %in% names(from_shp) | !to_idcol %in% names(to_shp)) {
    stop ("Input variables from_idcol and to_idcol must be names of columns in from_shp and to_shp.")
  }

  # Create our working data frames. Rename columns to make it clearer.
  from_shp <- from_shp %>%
    dplyr::rename(.from_idcol := {{from_idcol}}) %>%
    dplyr::select(.from_idcol)

  to_shp <- to_shp %>%
    dplyr::rename(.to_idcol := {{to_idcol}}) %>%
    dplyr::select(.to_idcol)

  # Set constant attributes so we don't get a warning
  sf::st_agr(from_shp) = "constant"
  sf::st_agr(to_shp) = "constant"

  # Get intersection area in temporary column
  result <- sf::st_intersection(from_shp, to_shp) %>%
    dplyr::mutate(.area = sf::st_area(geometry)) %>%
    sf::st_set_geometry(NULL) %>%

    # Get weights from area proportions
    dplyr::group_by(.from_idcol) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::arrange(.from_idcol) %>%
    dplyr::mutate(weight = as.numeric(.area/sum(.area))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.area) %>%

    # Set names back to originals
    dplyr::rename(!!sym(from_idcol) := .from_idcol,
                  !!sym(to_idcol) := .to_idcol)

  result
}



#' Created single-link indicator from weighted links using maximum weights
#'
#' @param weighted_links A `tbl_df` containing weighted links from one set of ids to another, such as created by `generate_weighted_links()`.
#' @param from_idcol Character. Name of column containing ids.
#' @param weights_col Character (default "weight"). Name of column containing weights.
#'
#' @return A `tbl_df` with two columns and one row for each unique value in input column `from_idcol`.
#' @export
#'
#' @examples
#' \dontrun{
#' generate_weighted_links(from_shp = dissemination_areas,
#' from_idcol = "DAUID",
#' to_shp = ons_neighbourhoods,
#' to_idcol = "ONS_ID") %>%
#' create_sli(from_idcol = "DAUID)
#' }
create_sli <- function(weighted_links, from_idcol, weights_col = "weight"){

  weighted_links %>%
    dplyr::rename(.weight := {{weights_col}}) %>%
    dplyr::group_by(!!sym(from_idcol)) %>%
    dplyr::filter(.weight == max(.weight)) %>%
    dplyr::select(-.weight) %>%
    dplyr::ungroup()

}





ons_goldstandard <- goldstandard_values %>% select(ONS_ID, value) %>% mutate(ONS_ID = as.numeric(ONS_ID)) %>% {left_join(ons_shp, .)} %>% select(ONS_ID, value) %>% mutate(value = as.numeric(value))

da_shp <- left_join(da_ott, da_values) %>% select(DAUID, value)
from_shp <- da_shp
to_shp <- ons_goldstandard
from_idcol = "DAUID"
to_idcol <- "ONS_ID"
from_valuecol <- to_valuecol <- "value"

# hoods_to_analyze: character vector of ONS_IDs
# da_values and goldstandard_values must be tibbles wiht columns "value",
# and  DAUID or ONS_ID respectively
greedy_sli_search2 <- function(input_sli, from_shp, from_idcol, from_valuecol, to_shp, to_idcol, to_valuecol, optimize_for = c("mape", "mae", "mse"), verbose = FALSE){

  optimize_for <- match.arg(optimize_for, optimize_for)

  # Create our working data frames. Rename columns to make it clearer.
  from_shp <- from_shp %>%
    dplyr::rename(.from_idcol := {{from_idcol}},
                  .from_valuecol := {{from_valuecol}}) %>%
    dplyr::select(.from_idcol, .from_valuecol) %>%
    dplyr::mutate(.from_idcol = as.character(.from_idcol))

  to_shp <- to_shp %>%
    dplyr::rename(.to_idcol := {{to_idcol}},
                  .to_valuecol := {{to_valuecol}}) %>%
    dplyr::select(.to_idcol, .to_valuecol) %>%
    dplyr::mutate(.to_idcol = as.character(.to_idcol))


  from_ids <- from_shp$.from_idcol
  to_ids <- to_shp$.to_idcol
  # pre-compute intersections in both directions. this takes ~250ms up front and saves ~30s per DA..
  # we can then access results via indexing a named list
  from_to_intersections <- precompute_intersections(from_shp = from_shp, from_idcol = ".from_idcol",
                                                    to_shp = to_shp, to_idcol = ".to_idcol")
  to_from_intersections <- precompute_intersections(from_shp= to_shp,
                                                    from_idcol = ".to_idcol",
                                                    to_shp = from_shp,
                                                    to_idcol = ".from_idcol")

  # keep track of iterations and results
  optimization_progress <- dplyr::tibble()


  # set up just the origin and destination values without geometry for faster operations
  from_values <- sf::st_set_geometry(from_shp , NULL)
  to_values <- sf::st_set_geometry(to_shp , NULL)

  ## Get  "gold standard" values
  # result_gold <- to_shp %>%
  #   dplyr::select(ONS_ID, value_ons = value) %>%
  #   dplyr::mutate(value_ons = as.numeric(value_ons))





  #sli_temp <- input_sli

  sli_new <- input_sli %>%
    dplyr::rename(.to_idcol := {{to_idcol}},
                  .from_idcol := {{from_idcol}}) %>%
    dplyr::mutate(.to_idcol = as.character(.to_idcol))

  # to quiet warning as per https://github.com/r-spatial/sf/issues/406
  sf::st_agr(from_shp) <- "constant"
  sf::st_agr(to_shp) <- "constant"


  tictoc::tic()
  for (i in 1:length(to_ids)){
    to_id <- to_ids[[i]]

    message(sprintf("%s/%s: Destination Region %s", i, length(to_ids), to_id))


    #if(verbose) message(paste0("Optimizing neighbourhood ", hood))

    # get all from_regions that intersect this to_region
    # FIXME this is also slow
    froms_in_to <- to_from_intersections[to_id][[1]]

    # for each originating region that's contained within the destination region...
    for (from in froms_in_to){

      # check to see if DA has valid data, if it does not, skip it
     # if (is.na(da_values[da_values$DAUID == da,]$value)) next



      # find ONS_IDs of neighbourhoods that intersect the DA
      from_possible_tos <- from_to_intersections[from][[1]]

      # if it can only go to one possible region, skip it
      if (length(from_possible_tos) == 1) next

      if(verbose) message(paste0("  Checking all assignments for originating region ", from))
      from_results <- dplyr::tibble()
      to_results <- dplyr::tibble()

      # for all possible destination regions this origin could possibly be assigned to...
      for (from_to in from_possible_tos){

        # Now uses adjusted SLI
        sli_temp <- sli_new

        # update the SLI for analysis to assign this origin to the current destination
        sli_temp[sli_temp$.from_idcol == from,]$.to_idcol <- from_to

        ## NEXT: compute differences between SLI predictions and true values

        result <- get_sli_diffs2(sli_temp, from_values, to_values)


        if(verbose) if (optimize_for == "mape" ) message(sprintf("    %s: mean proportional difference %.2f%%", from_to, 100*result$sli_diff_mape))
        if(verbose)  if (optimize_for == "mae"  ) message(sprintf("    %s: mean absolute error difference %.1f", from_to, result$sli_diff_mae))


        to_results <- dplyr::bind_rows(to_results,
                                         dplyr::tibble(.to_idcol = to_id) %>%
                                           dplyr::bind_cols(result)
        )
      } # end for da_hood in da_hoods


      # assign hood to DA with the best results
      # NOTE! Replacing ~3ms dplyr operation with ~222us base operation
      to_results$abs_result <- unlist(abs(to_results[paste0("sli_diff_", optimize_for)]))

      best_to <- to_results[to_results$abs_result == min(to_results$abs_result),]$.to_idcol


      if(verbose) message(paste0("     Best destination assignment for origin region ", from, ": ", best_to))


      # if we get one best unique answer, we use that one. otherwise don't update, it's probably an NA-valued DA
      if (length(best_to) == 1) sli_new <- sli_temp #sli_new[sli_new$DAUID == da, ]$ONS_ID <- best_hood

    } # end for da in hood

 #   if (optimize_for == "mape" ) message(sprintf("    %s: best mean absolute proportional error %.2f%%", da_hood, 100*min(abs(hood_results$sli_diff_mape))))
#    if (optimize_for == "mae" ) message(sprintf("    %s: best mean absolute error  %.1f", da_hood, min(abs(hood_results$sli_diff_mae))))
#    if (optimize_for == "mse" ) message(sprintf("    %s: best mean squared error  %.1f", da_hood, min(abs(hood_results$sli_diff_mse))))

    # keep track of progress
    optimization_progress <- dplyr::bind_rows(optimization_progress,
                                              dplyr::tibble(iteration = i, metric = optimize_for, value =  min(abs(to_results[paste0("sli_diff_", optimize_for)]))))

  } # end for hood in hoods to analyze

  # save optimization results to file
  readr::write_csv(optimization_progress, sprintf("outputs/optimization_trials/%s-%s.csv", optimize_for, Sys.time()))

  result <- da_ons_sli_new %>%
    dplyr::select(DAUID, ONS_ID)

  # set optimization progress as result metadata
  attr(result, "optimization_progress") <- optimization_progress

  # return the sli
  return (result)

} # end of function


# pre-compute the ONS hoods each DA intersects and return it as a named list
precompute_intersections <- function(from_shp, from_idcol, to_shp, to_idcol){

  to_ids <- to_shp[,to_idcol, drop=TRUE]
  #ons_ids <- ons_shp$ONS_ID

  from_to_intersections <- sf::st_intersects(from_shp, to_shp) %>%
    purrr::map(function(x) as.character(to_ids[x]))

  names(from_to_intersections) <- from_shp[,from_idcol, drop=TRUE]

  return(from_to_intersections)

}




# function to compare SLI results with gold-standard results
get_sli_diffs2 <- function(sli_temp, from_values, to_values, do_not_summarize = FALSE) {

  # using single-link indicator, estimate destination values
  result_sli <- sli_temp %>%
    dplyr::left_join(from_values, by = ".from_idcol") %>%
    dplyr::group_by(.to_idcol) %>%
    dplyr::summarise(value_sli = sum(.from_valuecol, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.to_idcol) %>%
    dplyr::mutate(.to_idcol = as.character(.to_idcol))

  ### PUT RESULTS TOGETHER, replace NAs with 0
  result <- to_values %>%
    dplyr::left_join(result_sli, by = ".to_idcol") %>%
    dplyr::mutate(value_sli = dplyr::if_else(is.na(value_sli), 0, value_sli)) %>%

    # calculate differences
    dplyr::mutate(sli_diff_count = (value_sli - .to_valuecol),
                  sli_diff_prop = sli_diff_count/.to_valuecol) %>%
    dplyr::mutate(sli_diff_prop = dplyr::if_else(.to_valuecol == value_sli, 0, sli_diff_prop)) %>% # to handle 0/0 = NaN
    dplyr::mutate(sli_diff_prop = dplyr::if_else(sli_diff_prop > 5, 5, sli_diff_prop)) %>%      # to handle 0/n = Inf
    dplyr::mutate(sli_diff_prop = dplyr::if_else(sli_diff_prop < -5, -5, sli_diff_prop)) %>%      # to handle -0/n = -Inf
    dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0))

  if (!do_not_summarize) {
    result <- result %>%
      # summarise
      dplyr::summarise(
        sli_diff_mae = mean(abs(sli_diff_count)),
        sli_diff_mape = mean(abs(sli_diff_prop)),
        sli_diff_mse = mean(sli_diff_count^2)
      )
  }

  return(result)
}
