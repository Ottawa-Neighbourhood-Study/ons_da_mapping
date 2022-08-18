# new attempt to compare statscan to ons data using new file from kady


# load ONS neighbourhood dwelling counts created by stats can: gold standard
ons_data2 <- readr::read_csv("inputs/Profile_P1.csv") %>%
  dplyr::filter(data_ID == "A1702") %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
  tidyr::pivot_longer(cols = -c(data_ID, description), values_to = "dwellings_ons", names_to = "ONS_ID") %>%
  dplyr::mutate(dwellings_ons = as.numeric(dwellings_ons))

# load DA-level data from statscan that we'll try to use to mimic the gold standard
sc_df <- sc_housing2016 %>%
  dplyr::filter(TEXT_ID == "27034") %>%
  dplyr::select(DAUID = GEO_ID,
                TEXT_ID,
                description = TEXT_NAME_NOM,
                dwellings = T_DATA_DONNEE) %>%
  dplyr::mutate(dwellings = as.numeric(dwellings))


# compare the city-wide ONS gold standard to the statscan DA sums: they're really close,
# 373,710 vs 373,755, so _overall_ the rounding errors are minimal

sc_df %>%
  summarise(dwellings_sc = sum(dwellings, na.rm = TRUE)) %>%
  bind_cols({
    ons_data2 %>%
      summarise(dwellings_ons = sum(dwellings, na.rm = TRUE))
  })




###### optimize with a greedy search algorithm
#test <- left_join(cvold, cvnew, by = c("data_ID", "description", "ONS_ID", "dwellings_ons")) %>% mutate(diff = diff_prop.x != diff_prop.y )

source("R/greedy_sli_search.R")


da_values <- sc_housing2016 %>%
  dplyr::filter(TEXT_ID == "27034") %>%
  dplyr::select(DAUID = GEO_ID,
                TEXT_ID,
                description = TEXT_NAME_NOM,
                value = T_DATA_DONNEE) %>%
  dplyr::mutate(value = as.numeric(value))

goldstandard_values <- readr::read_csv("inputs/Profile_P1.csv") %>%
  dplyr::filter(data_ID == "A1702") %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
  tidyr::pivot_longer(cols = -c(data_ID, description), values_to = "value", names_to = "ONS_ID") %>%
  dplyr::mutate(dwellings_ons = as.numeric(value))

# get results for the original sli

sli_results_orig <- measure_sli_performance(da_ons_sli, da_values, goldstandard_values)

# try optimizing something...
da_ons_sli_test <- greedy_sli_search(da_ons_sli, da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = "956", da_values, goldstandard_values)

# try optimizing everything...
da_ons_sli_test_all <- greedy_sli_search(da_ons_sli, da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = ons_shp$ONS_ID, da_values, goldstandard_values)


da_ons_sli_test_all_new <- greedy_sli_search(da_ons_sli, da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = ons_shp$ONS_ID, da_values, goldstandard_values)

da_ons_sli_test_all_new <- greedy_sli_search(da_ons_sli, da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = ons_shp$ONS_ID, da_values, goldstandard_values)

da_ons_sli_prop <- greedy_sli_search(da_ons_sli, da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = ons_shp$ONS_ID, da_values, goldstandard_values)

da_ons_sli_prop2 <- greedy_sli_search(da_ons_sli_prop, da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = sample(ons_shp$ONS_ID), da_values, goldstandard_values)

da_ons_sli_prop3 <- greedy_sli_search(da_ons_sli_prop2, da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = sample(ons_shp$ONS_ID), da_values, goldstandard_values)



da_ons_sli_test_all_count <- greedy_sli_search(da_ons_sli, da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = ons_shp$ONS_ID, da_values, goldstandard_values,
                                               optimize_for = "count", optimize_by = "mean")

da_ons_sli_test_all_count2 <- greedy_sli_search(da_ons_sli_test_all_count, da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = sample(ons_shp$ONS_ID), da_values, goldstandard_values,
                                                optimize_for = "count", optimize_by = "mean")

da_ons_sli_test_all_count_max <- greedy_sli_search(da_ons_sli, da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = "sort", da_values, goldstandard_values,
                                                   optimize_for = "count", optimize_by = "max")

# what if we do by count and then by proportion?
da_ons_sli_count_prop <- greedy_sli_search(da_ons_sli_test_all_count, da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = ons_shp$ONS_ID, da_values, goldstandard_values,
                                           optimize_for = "proportion", optimize_by = "mean")

da_ons_sli_count_prop2 <- greedy_sli_search(da_ons_sli_count_prop, da_ons_intersect, da_ott, ons_shp,
                                            hoods_to_analyze = sample(ons_shp$ONS_ID),
                                            da_values, goldstandard_values,
                                            optimize_for = "proportion", optimize_by = "mean")

# note! from experimenting, sorting by biggest doesn't help much for counts optimization

#### check error
sli_for_test <- da_ons_sli
sli_for_test <- da_ons_sli_new
sli_for_test <- da_ons_sli_test


sli_performance_orig <- measure_sli_performance(da_ons_sli, da_values, goldstandard_values)

sli_performance_one <- measure_sli_performance(da_ons_sli_test, da_values, goldstandard_values)

sli_performance_all <- measure_sli_performance(da_ons_sli_test_all, da_values, goldstandard_values)

sli_performance_all_new <- measure_sli_performance(da_ons_sli_test_all_new, da_values, goldstandard_values)


sli_performance_prop <- measure_sli_performance(da_ons_sli_prop, da_values, goldstandard_values)

sli_performance_prop2 <- measure_sli_performance(da_ons_sli_prop2, da_values, goldstandard_values)
sli_performance_prop3 <- measure_sli_performance(da_ons_sli_prop3, da_values, goldstandard_values)

sli_performance_all_count <- measure_sli_performance(da_ons_sli_test_all_count, da_values, goldstandard_values)

sli_performance_all_count_max <- measure_sli_performance(da_ons_sli_test_all_count_max, da_values, goldstandard_values)

sli_performance_all_count2  <- measure_sli_performance(da_ons_sli_test_all_count2, da_values, goldstandard_values)


sli_performance_count_prop <-  measure_sli_performance(da_ons_sli_count_prop, da_values, goldstandard_values)

# see below: most errors are small in absolute and relative terms, but there are
# long tails of a few extremely wrong values, more than 65% over or under the
# "gold-standard" value

sli_performance_orig$raw_comparison[[1]] %>%
  ggplot() +
  geom_point(aes(x=dwellings_ons, y=dwellings_sli)) +
  geom_abline(slope = 1, intercept = 0) +
  ggtitle("original SLI")


sli_performance_prop$raw_comparison[[1]] %>%
  ggplot() +
  geom_point(aes(x=dwellings_ons, y=dwellings_sli)) +
  geom_abline(slope = 1, intercept = 0) +
  ggtitle("optimized SLI: proportions - 1 iteration")


sli_performance_prop3$raw_comparison[[1]] %>%
  ggplot() +
  geom_point(aes(x=dwellings_ons, y=dwellings_sli)) +
  geom_abline(slope = 1, intercept = 0) +
  ggtitle("optimized SLI: proportions - 3 iterations")

sli_performance_all_count$raw_comparison[[1]] %>%
  ggplot() +
  geom_point(aes(x=dwellings_ons, y=dwellings_sli)) +
  geom_abline(slope = 1, intercept = 0) +
  ggtitle("optimized SLI: counts, unsorted")

sli_performance_all_count2$raw_comparison[[1]] %>%
  ggplot() +
  geom_point(aes(x=dwellings_ons, y=dwellings_sli)) +
  geom_abline(slope = 1, intercept = 0) +
  ggtitle("optimized SLI: counts, 2 iterations")


sli_performance_count_prop$raw_comparison[[1]] %>%
  ggplot() +
  geom_point(aes(x=dwellings_ons, y=dwellings_sli)) +
  geom_abline(slope = 1, intercept = 0) +
  ggtitle("optimized SLI: counts then proportion")

hist(compare_values$diff_prop, breaks = 11)

summary(compare_values$diff_count)
summary(compare_values$diff_prop)





mean(test$diff_prop.x)
mean(test$diff_prop.y)

da_ons_sli_test %>% rename(ONS_ID_new = ONS_ID) %>% left_join(da_ons_sli, by = "DAUID") %>% filter(ONS_ID_new != ONS_ID)




#### plotting differences from targets

targets::tar_load(sli_performance)

sli_performance

plot_sli_performance(sli_performance$raw_comparison[[1]], title = "no optimization")
plot_sli_performance(sli_performance$raw_comparison[[2]], title = paste0("Optimized for ", sli_performance$optimization[[2]]))
plot_sli_performance(sli_performance$raw_comparison[[3]], title = paste0("Optimized for ", sli_performance$optimization[[3]]))
plot_sli_performance(sli_performance$raw_comparison[[4]], title = paste0("Optimized for ", sli_performance$optimization[[4]]))


z <- leaflet_sli_performance(sli_performance$raw_comparison[[2]], ons_shp, "diff_prop")

z %>%
  leaflet::addPolygons(
    data = da_ott %>% sf::st_transform(crs = "WGS84") %>% filter(DAUID %in% test$DAUID) %>% left_join(test), fillColor = ~if_else(ONS_ID == "937", "blue", "purple"), weight = 1, color = "red")

z %>%
  leaflet::addPolygons(
    data = da_ott %>%
      sf::st_intersects(
        ons_shp %>% filter(ONS_ID %in% c("937","952"))
      ) %>% sf::st_transform(crs = "WGS84") %>%
      dplyr::left_join(da_values),
    weight = 1, color = "red",
    label = ~ sprintf("DAUID %s: pop %s",  DAUID, value)
  )

z %>%
  leaflet::addPolygons(
    data = filter(da_ott, DAUID == "35061841") %>% sf::st_transform(crs = "WGS84")
  )
leaflet_sli_performance(sli_performance$raw_comparison[[3]], ons_shp, "diff_count")
leaflet_sli_performance(sli_performance$raw_comparison[[3]], ons_shp, "diff_prop")

leaflet_sli_performance(sli_performance$raw_comparison[[4]], ons_shp, "diff_count")
leaflet_sli_performance(sli_performance$raw_comparison[[4]], ons_shp, "diff_prop")

targets::tar_load(da_ons_sli_opt_mse)
targets::tar_load(da_values)
targets::tar_load(da_ott)
targets::tar_load(ons_shp)
targets::tar_load(goldstandard_values)
targets::tar_load(da_ons_intersect)
test <- greedy_sli_search(da_ons_sli_opt_mse, da_ons_intersect, da_ott, ons_shp, hoods_to_analyze = c("956", "3", "93", "935", "948"), da_values, goldstandard_values, optimize_for = "mae")

test %>% left_join(da_ons_sli_opt_mse, by = "DAUID") %>% filter(ONS_ID.x != ONS_ID.y)




# comparing results for other variables

targets::tar_load(sc_labour2016)
targets::tar_load(sc_pop2016)
targets::tar_load(sc_immcitzn2016)
targets::tar_load(sc_vismin2016)
targets::tar_load(ons_data)
targets::tar_load(da_ons_intersect)
targets::tar_load(vars_to_analyze)
run_comparison_analysis



# function to compare ons data with computed data using an arbitrary number of
# weighted or SLI data frames
compare_da_ons_links <- function(sc_labour2016, sc_pop2016, sc_immcitzn2016, sc_vismin2016, ons_data, ... ) {

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


targets::tar_load(da_ons_sli_opt_mae)
targets::tar_load(da_ons_sli_opt_mse)
targets::tar_load(da_ons_sli_opt_mape)
z <- compare_da_ons_links(sc_labour2016, sc_pop2016, sc_immcitzn2016, sc_vismin2016, ons_data, da_ons_intersect, da_ons_sli, da_ons_sli_opt_mae, da_ons_sli_opt_mape, da_ons_sli_opt_mse)
z
