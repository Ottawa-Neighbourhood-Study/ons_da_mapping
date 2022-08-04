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




#### check error
sli_for_test <- da_ons_sli
sli_for_test <- da_ons_sli_new
sli_for_test <- da_ons_sli_test


sli_performance_orig <- measure_sli_performance(da_ons_sli, da_values, goldstandard_values)

sli_performance_one <- measure_sli_performance(da_ons_sli_test, da_values, goldstandard_values)

sli_performance_all <- measure_sli_performance(da_ons_sli_test_all, da_values, goldstandard_values)

sli_performance_all_new <- measure_sli_performance(da_ons_sli_test_all_new, da_values, goldstandard_values)
# see below: most errors are small in absolute and relative terms, but there are
# long tails of a few extremely wrong values, more than 65% over or under the
# "gold-standard" value

sli_performance_orig$raw_comparison[[1]] %>%
  ggplot() +
  geom_point(aes(x=dwellings_ons, y=dwellings_sli)) +
  geom_abline(slope = 1, intercept = 0) +
  ggtitle("original SLI")


sli_performance_all_new$raw_comparison[[1]] %>%
  ggplot() +
  geom_point(aes(x=dwellings_ons, y=dwellings_sli)) +
  geom_abline(slope = 1, intercept = 0) +
  ggtitle("original SLI")



hist(compare_values$diff_prop, breaks = 11)

summary(compare_values$diff_count)
summary(compare_values$diff_prop)





mean(test$diff_prop.x)
mean(test$diff_prop.y)

da_ons_sli_test %>% rename(ONS_ID_new = ONS_ID) %>% left_join(da_ons_sli, by = "DAUID") %>% filter(ONS_ID_new != ONS_ID)

