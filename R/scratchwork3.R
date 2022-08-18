
## run some tests

test_ons <- dplyr::filter(ons_shp, ONS_ID %in% c("3","912","956","93"))
test_ons <- dplyr::filter(ons_shp, ONS_ID %in% c("3","93"))
test_ons <- ons_shp

sli_input <- generate_weighted_links(da_ott, "DAUID", test_ons, "ONS_ID") %>%
  #mutate(weight = round(weight, digits =3))
  create_sli("DAUID")

ons_goldstandard <- goldstandard_values %>%
  select(ONS_ID, value) %>%
  mutate(ONS_ID = as.character(ONS_ID)) %>%
  {left_join(test_ons, .)} %>%
  select(ONS_ID, value) %>%
  mutate(value = as.numeric(value))

da_shp <- left_join(da_ott, da_values) %>% select(DAUID, value)
from_shp <- da_shp
to_shp <- ons_goldstandard
from_idcol = "DAUID"
to_idcol <- "ONS_ID"
from_valuecol <- to_valuecol <- "value"

sli_opt <- greedy_sli_search2(sli_input, da_shp, "DAUID", "value", ons_goldstandard, "ONS_ID", "value", "mape", tolerance = 0.05, verbose = TRUE) %>%
  greedy_sli_search2(da_shp, "DAUID", "value", ons_goldstandard, "ONS_ID", "value", "mape", tolerance = 0.05, verbose = TRUE) %>%
  greedy_sli_search2(da_shp, "DAUID", "value", ons_goldstandard, "ONS_ID", "value", "mape", tolerance = 0.05, verbose = TRUE)

sli_opt2 <- greedy_sli_search2(sli_input, da_shp, "DAUID", "value", ons_goldstandard, "ONS_ID", "value", "mse", tolerance = 0.05, verbose = TRUE) %>%
  greedy_sli_search2(da_shp, "DAUID", "value", ons_goldstandard, "ONS_ID", "value", "mse", tolerance = 0.05, verbose = TRUE) %>%
  greedy_sli_search2(da_shp, "DAUID", "value", ons_goldstandard, "ONS_ID", "value", "mse", tolerance = 0.05, verbose = TRUE)

attr(sli_opt, "optimization_progress") %>% rowid_to_column() %>% ggplot() + geom_line(aes(x=rowid, y=value))
attr(sli_opt2, "optimization_progress") %>% rowid_to_column() %>% ggplot() + geom_line(aes(x=rowid, y=value))



message('\r', i, appendLF = FALSE)
# see differences
left_join(sli_input, sli_opt, by = "DAUID") %>%
  filter(ONS_ID.x != ONS_ID.y) %>%
  pull(DAUID) %>%
  {filter(da_ott, DAUID %in% .)} %>%
  ggplot() + geom_sf()



#### plot them


ons_shp %>%
  sf::st_transform(crs = "WGS84") %>%
  leaflet() %>%
  addPolygons(label = ~ ONS_ID)

sliformap <- sli_opt
sliformap <- sli_input


formap <- da_ott %>%
  sf::st_transform(crs = "WGS84") %>%
  inner_join(sliformap) %>%
  select(DAUID, ONS_ID)

pal <- leaflet::colorFactor(palette = "YlOrRd", domain = formap$ONS_ID)

formap %>%
  leaflet() %>%
  addPolygons(data = sf::st_transform(filter(ons_shp, ONS_ID %in% sli_input$ONS_ID), crs = "WGS84"), color = "orange") %>%
  addPolygons(label = ~ paste0(DAUID, "\n\n", ONS_ID), fillColor = ~ pal(ONS_ID), weight = 1)


weighttest <- generate_weighted_links(from_shp, from_idcol = "DAUID")

sli_opt <- greedy_sli_search2(sli_input, da_shp, "DAUID", "value", ons_goldstandard, "ONS_ID", "value", "mape", tolerance = 0.05, verbose = FALSE) %>%
  greedy_sli_search2(da_shp, "DAUID", "value", ons_goldstandard, "ONS_ID", "value", "mape", tolerance = 0.05)  %>%
  greedy_sli_search2(da_shp, "DAUID", "value", ons_goldstandard, "ONS_ID", "value", "mape", tolerance = 0.05)

sli_opt2 <- greedy_sli_search2(sli_input, da_shp, "DAUID", "value", ons_goldstandard, "ONS_ID", "value", "mae", tolerance = 0.05, verbose = FALSE) %>%
  greedy_sli_search2(da_shp, "DAUID", "value", ons_goldstandard, "ONS_ID", "value", "mae", tolerance = 0.05)  %>%
  greedy_sli_search2(da_shp, "DAUID", "value", ons_goldstandard, "ONS_ID", "value", "mae", tolerance = 0.05)

sli_opt3 <- greedy_sli_search2(sli_input, da_shp, "DAUID", "value", ons_goldstandard, "ONS_ID", "value", "mse", tolerance = 0.05, verbose = FALSE) %>%
  greedy_sli_search2(da_shp, "DAUID", "value", ons_goldstandard, "ONS_ID", "value", "mse", tolerance = 0.05)  %>%
  greedy_sli_search2(da_shp, "DAUID", "value", ons_goldstandard, "ONS_ID", "value", "mse", tolerance = 0.05)

attr(sli_opt, "optimization_progress") %>% rowid_to_column() %>% ggplot() + geom_line(aes(x=rowid, y=value))
attr(sli_opt2, "optimization_progress") %>% rowid_to_column() %>% ggplot() + geom_line(aes(x=rowid, y=value))
attr(sli_opt3, "optimization_progress") %>% rowid_to_column() %>% ggplot() + geom_line(aes(x=rowid, y=value))

# see differences
left_join(sli_opt2, sli_opt, by = "DAUID") %>%
  filter(ONS_ID.x != ONS_ID.y) %>%
  pull(DAUID) %>%
  {filter(da_ott, DAUID %in% .)} %>%
  ggplot() + geom_sf()


inner_join(da_ott, sli_input) %>%
  ggplot() + geom_sf(aes(fill = ONS_ID))  + geom_sf(data = filter(to_shp, ONS_ID %in% sli_opt$ONS_ID), fill = NA,  colour = "blue")

inner_join(da_ott, sli_opt) %>%
  ggplot() + geom_sf(aes(fill = ONS_ID)) + geom_sf(data = filter(to_shp, ONS_ID %in% sli_opt$ONS_ID), fill = NA,  colour = "blue")
library(leaflet)

pal <- colorFactor("viridis", domain = sli_opt$ONS_ID)
inner_join(da_ott, sli_input) %>% sf::st_transform(crs = "WGS84") %>%
  leaflet()%>%
  addPolygons(data = sf::st_transform(filter(to_shp, ONS_ID %in% sli_opt$ONS_ID), crs = "WGS84"), fill = NA, color = "orange", weight = 5) %>% addPolygons(fill = ~pal(ONS_ID), label = ~paste0("DAUID: ", DAUID, "   ONS ID: ", ONS_ID),
                                                                                                                                                           weight = 1)













## testing greedy search because it won't work...

#take DA 35060224

da <- "35060224"


message(paste0("  Checking all assignments for DA ", da))

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

  ## Get ONS "gold standard"
  result_ons <- goldstandard_values %>%
    dplyr::select(ONS_ID, value_ons = value)

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
    dplyr::arrange(dplyr::desc((ONS_ID))) %>%

    # summarise
    summarise(
      sli_diff_count_mean = mean(sli_diff_count),
      sli_diff_count_sd = sd(sli_diff_count),
      sli_diff_prop_mean = mean(sli_diff_prop),
      sli_diff_prop_sd = sd(sli_diff_prop)
    )




  var_results <- c(var_results, result$sli_diff_prop_mean)

  message(sprintf("    %s: mean proportional difference %.2f%%", da_hood, 100*result$sli_diff_prop_mean))

  hood_results <- dplyr::bind_rows(hood_results,
                                   dplyr::tibble(ONS_ID = da_hood) %>%
                                     dplyr::bind_cols(result)
  )

} # end for da_hood in da_hoods

# assign hood to DA with the best results
best_hood <- hood_results %>%
  dplyr::mutate(abs_diff = abs(sli_diff_prop_mean)) %>%
  dplyr::arrange(abs_diff) %>%
  dplyr::slice_head(n=1) %>%
  dplyr::pull(ONS_ID)

message(paste0("     Best neighbourhood assignment for DAUID ", da, ": ", best_hood))

da_ons_sli_new[da_ons_sli_new$DAUID == da, ]$ONS_ID <- best_hood
