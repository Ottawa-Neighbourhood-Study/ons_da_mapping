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
greedy_sli_search <- function(da_ons_sli, da_ons_intersect, da_ott, ons_shp, hoods_to_analyze, da_values, goldstandard_values, optimize_for = c("proportion", "count"), optimize_by = c("mean", "max"), verbose = FALSE){

  optimize_for <- match.arg(optimize_for, optimize_for)


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

    if (optimize_for == "proportion") {
      hoods_to_analyze <- sort_order %>%
        dplyr::arrange(dplyr::desc(abs(sli_diff_prop))) %>%
        pull(ONS_ID)
    }

    if (optimize_for == "count") {
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
    das_in_hood <- da_ons_intersect %>%
      dplyr::filter(ONS_ID == hood) %>%
      dplyr::pull(DAUID)


    for (da in das_in_hood){

      if(verbose) message(paste0("  Checking all assignments for DA ", da))
      # get DA shape
      da_shp <- dplyr::filter(da_ott, DAUID == da)

      # to quiet warning as per https://github.com/r-spatial/sf/issues/406
      sf::st_agr(da_shp) <- "constant"

      # find ONS_IDs of neighbourhoods that intersect the DA
      da_hoods <- sf::st_intersection(ons_shp, da_shp) %>%
        dplyr::pull(ONS_ID) %>%
        as.character()

      hood_results <- dplyr::tibble()

      for (da_hood in da_hoods){
        # FIXME TODO!! Now uses adjusted SLI, originally used original one always... this matters! Test!
        da_ons_sli_temp <- da_ons_sli_new

        # update the SLI for analysis to use this ONS ID
        da_ons_sli_temp[da_ons_sli_temp$DAUID == da,]$ONS_ID <- da_hood

        var_results <- c()

        # this should obviously be in a function
        # get mean neighbourhood proportional errors for SLI with this assignment

        ## NEXT: compute neighbourhood-level differences

        result <- get_sli_diffs(da_ons_sli_temp, da_values, result_ons)

        var_results <- c(var_results, result$sli_diff_prop_mean)


        if(verbose) if (optimize_for == "proportion" & optimize_by == "mean") message(sprintf("    %s: mean proportional difference %.2f%%", da_hood, 100*result$sli_diff_prop_mean))
        if(verbose)  if (optimize_for == "count"  & optimize_by == "mean") message(sprintf("    %s: mean count difference %.1f", da_hood, result$sli_diff_count_mean))

        if(verbose)  if (optimize_for == "proportion" & optimize_by == "max") message(sprintf("    %s: max proportional difference %.2f%%", da_hood, 100*result$sli_diff_prop_max))
        if(verbose)  if (optimize_for == "count"  & optimize_by == "max") message(sprintf("    %s: max count difference %.1f", da_hood, result$sli_diff_count_max))


        hood_results <- dplyr::bind_rows(hood_results,
                                         dplyr::tibble(ONS_ID = da_hood) %>%
                                           dplyr::bind_cols(result)
        )
      } # end for da_hood in da_hoods

      # assign hood to DA with the best results
      if (optimize_for == "proportion"){
        if (optimize_by == "mean"){
          #message ("proportion mena")
          best_hood <- hood_results %>%
            dplyr::mutate(abs_diff = abs(sli_diff_prop_mean)) %>%
            dplyr::arrange(abs_diff) %>%
            dplyr::slice_head(n=1) %>%
            dplyr::pull(ONS_ID)
        }

        if (optimize_by == "max"){
          best_hood <- hood_results %>%
            dplyr::mutate(abs_diff = abs(sli_diff_prop_max)) %>%
            dplyr::arrange(abs_diff) %>%
            dplyr::slice_head(n=1) %>%
            dplyr::pull(ONS_ID)
        }
      }

      if (optimize_for == "count"){
        if (optimize_by == "mean"){
          best_hood <- hood_results %>%
            dplyr::mutate(abs_diff = abs(sli_diff_count_mean)) %>%
            dplyr::arrange(abs_diff) %>%
            dplyr::slice_head(n=1) %>%
            dplyr::pull(ONS_ID)
        }
        if (optimize_by == "max"){
          best_hood <- hood_results %>%
            dplyr::mutate(abs_diff = abs(sli_diff_count_max)) %>%
            dplyr::arrange(abs_diff) %>%
            dplyr::slice_head(n=1) %>%
            dplyr::pull(ONS_ID)
        }

      }

      if(verbose) message(paste0("     Best neighbourhood assignment for DAUID ", da, ": ", best_hood))

      da_ons_sli_new[da_ons_sli_new$DAUID == da, ]$ONS_ID <- best_hood

    } # end for da in hood

    if (optimize_for == "proportion" & optimize_by == "mean") message(sprintf("    %s: best mean proportional difference %.2f%%", da_hood, 100*min(abs(hood_results$sli_diff_prop_mean))))
    if (optimize_for == "count"  & optimize_by == "mean") message(sprintf("    %s: best mean count difference %.1f", da_hood, min(abs(hood_results$sli_diff_count_mean))))

  } # end for hood in hoods to analyze

  da_ons_sli_new %>%
    dplyr::select(DAUID, ONS_ID) %>%
    return ()

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


measure_sli_performance2 <- function(sli_for_test, da_values, goldstandard_values){
  sli_created_results <-  da_values %>%
    left_join(sli_for_test, by = "DAUID") %>%
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
              mae_sd = mean(abs(diff_count)),
              mape = mean(abs(diff_prop)),
              mape_sd = sd(abs(diff_prop))
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

    # calculate differences
    dplyr::mutate(sli_diff_count = (value_sli - value_ons),
                  sli_diff_prop = sli_diff_count/value_ons) %>%
    dplyr::mutate(sli_diff_prop = dplyr::if_else(value_ons == value_sli, 0, sli_diff_prop)) %>% # to handle 0/0 = NaN
    dplyr::mutate(sli_diff_prop = dplyr::if_else(sli_diff_prop > 5, 5, sli_diff_prop)) %>%      # to handle n/0 = Inf
    dplyr::filter(ONS_ID != "0") %>%
    dplyr::mutate(dplyr::across(where(is.numeric), tidyr::replace_na, 0)) %>%
    dplyr::arrange(dplyr::desc((ONS_ID)))

  if (!do_not_summarize) {
    result <- result %>%
    # summarise
    # NOTE!!! HERE WE TAKE ABSOLUTE VALUES!!!! FIXME TODO!!
    # OTHERWISE IT OPTIMIZES FOR OVERALL POPULATION MEAN
    summarise(
      sli_diff_count_mean = mean(abs(sli_diff_count)),
      sli_diff_count_max = max(abs(sli_diff_count)),
      sli_diff_count_sd = sd(abs(sli_diff_count)),
      sli_diff_prop_mean = mean(abs(sli_diff_prop)),
      sli_diff_prop_max = max(abs(sli_diff_prop)),
      sli_diff_prop_sd = sd(abs(sli_diff_prop))
    )
  }

  return(result)
}


plot_sli_performance <- function(comparison, title = NA) {
  comparison %>%
    ggplot() +
    geom_point(aes(x=dwellings_ons, y=dwellings_sli)) +
    geom_abline(slope = 1, intercept = 0) +
    ggtitle(title)

}


leaflet_sli_performance <- function(comparison, ons_shp, var = c("diff_count", "diff_prop")){

  var <- match.arg(var, var)

  forplot <- ons_shp %>%
    sf::st_transform(crs = "WGS84") %>%
    dplyr::mutate(ONS_ID = as.character(ONS_ID)) %>%
    dplyr::left_join(comparison) %>%
    dplyr::rename(plot_var := {{var}})

  pal <- leaflet::colorNumeric(palette = "viridis",
                               domain = c(min(forplot$plot_var, na.rm = TRUE), max(forplot$plot_var, na.rm = TRUE)))

  forplot %>%
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(weight = 1,
      fillColor = ~ pal(plot_var),
                         fillOpacity = 0.9,
                         label = ~ purrr::map(sprintf("%s (%s)<br>%s: %s", Name, ONS_ID, var, plot_var), htmltools::HTML))


}
