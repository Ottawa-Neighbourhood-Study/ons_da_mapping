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



####################################
# rounding but preserving sum to 100%

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


#######################################
# ONS STATSCAN DATA

ons_data <- onsr::get_ons_data()

# create single-link indicator taking single ONS ID of maximum overlap
# in cases of perfect tie, choose the first one listed
da_ons_sli <- da_ons_intersect %>%
  dplyr::group_by(DAUID) %>%
  dplyr::arrange(dplyr::desc(pct_overlap)) %>%
  dplyr::slice_head(n = 1) %>%
  #  dplyr::filter(pct_overlap_rnd == max(pct_overlap_rnd)) %>%
  dplyr::mutate(pct_overlap_rnd = 1) %>%
  dplyr::select(DAUID, ONS_ID, pct_overlap_rnd)

da_ott %>% filter(!DAUID %in% da_ons_intersect$DAUID)
da_ott %>% filter(!DAUID %in% da_ons_sli$DAUID)

da_ons_intersect %>%
  group_by(DAUID) %>%
  summarise(total = sum(pct_overlap_rnd)) %>% view()

ons_pop2016 <- dplyr::filter(ons_data, polygon_attribute == "pop2016")

sc_pop2016 <- onsr::census_get_data(dguids = onsr::census_make_dguid(da_ott$DAUID))

sc_pop2016total <- dplyr::filter(sc_pop2016, TEXT_ID == "1000") %>%
  dplyr::select(DAUID = GEO_ID, pop2016_sc = T_DATA_DONNEE)

# get ONS pops from statscan data using proportional assignment
pop_pct <- da_ott %>%
  sf::st_set_geometry(NULL) %>%
  left_join(sc_pop2016total) %>%
  left_join(da_ons_intersect) %>%
  filter(pct_overlap_rnd > 0) %>%
  mutate(pop2016_sc = as.numeric(pop2016_sc)) %>%
  group_by(DAUID) %>%
  mutate(pop2016_pct = pop2016_sc * pct_overlap_rnd) %>%
  select(DAUID, ONS_ID, pct_overlap_rnd, pop2016_sc, pop2016_pct) %>%
  ungroup() %>%
  group_by(ONS_ID) %>%
  summarise(pop2016_pct = sum(pop2016_pct))

# get ONS pops from statscan data using single-link indicator
pop_sli <- da_ott %>%
  sf::st_set_geometry(NULL) %>%
  left_join(sc_pop2016total) %>%
  left_join(da_ons_sli) %>%
  filter(pct_overlap_rnd > 0) %>%
  mutate(pop2016_sc = as.numeric(pop2016_sc)) %>%
  group_by(DAUID) %>%
  mutate(pop2016_pct = pop2016_sc * pct_overlap_rnd) %>%
  select(DAUID, ONS_ID, pct_overlap_rnd, pop2016_sc, pop2016_pct) %>%
  ungroup() %>%
  group_by(ONS_ID) %>%
  summarise(pop2016_pct = sum(pop2016_pct)) %>%
  rename(pop2016_sli = pop2016_pct)

# put ONS, pct, and sli results together
comp_table <- ons_pop2016 %>%
  select(ONS_ID, pop2016_ons = value) %>%
  left_join(pop_pct, by = "ONS_ID") %>%
  left_join(pop_sli, by = "ONS_ID") %>%
  mutate(across(everything(), replace_na, 0))

# mean/sd of differences? absolutes amd proportions
comp_table_diffs <- comp_table %>%
  filter(pop2016_ons > 0) %>%
  filter(ONS_ID > 0) %>%
  mutate(pct_diff_count = (pop2016_pct - pop2016_ons),
         sli_diff_count = (pop2016_sli- pop2016_ons),
         pct_diff_prop = pct_diff_count/pop2016_ons,
         sli_diff_prop = sli_diff_count/pop2016_ons)

comp_table_summary <- comp_table_diffs %>%
  summarise(pct_diff_count_mean = mean(pct_diff_count),
            pct_diff_count_sd = sd(pct_diff_count),
            pct_diff_prop_mean = mean(pct_diff_prop),
            pct_diff_pop_sd = sd(pct_diff_prop),
            sli_diff_count_mean = mean(sli_diff_count),
            sli_diff_count_sd = sd(sli_diff_count),
            sli_diff_prop_mean = mean(sli_diff_prop),
            sli_diff_pop_sd = sd(sli_diff_prop)
  )

# visualize?
comp_table %>%
  filter(ONS_ID > 0) %>%
  pivot_longer(cols = -ONS_ID, names_to = "data_source", values_to="pop_value") %>%
  ggplot(aes(x=pop_value, y=ONS_ID, colour = data_source )) +
  geom_point()


colours <- c("SLI Population" = "green", "Pct. Population" = "purple")

# proportional diffs
comp_table_diffs %>%
  filter(ONS_ID > 0) %>%
  mutate(ONS_ID = factor(ONS_ID, levels = ONS_ID)) %>%
  ggplot() +
  geom_point(aes(y=ONS_ID, x = sli_diff_prop, colour = names(colours)[1])) +
  geom_point(aes(y=ONS_ID, x = pct_diff_prop, colour = names(colours)[2])) +
  geom_vline(xintercept = 0)  +
  scale_colour_manual(values = colours) +
  theme(legend.position = "bottom") +
  labs(title = "Proportional Differences: ONS Neighbourhood 2016 Total Population using two DA-to-ONS methods",
       subtitle = "Comparing proportional assignment and single-link indicator to official StatsCan/ONS Data",
       x = "Calculated Value Compared to Ground Truth",
       colour = "Data Source") +
  scale_x_continuous(labels = scales::percent)

# absolute diffs
comp_table_diffs %>%
  filter(ONS_ID > 0) %>%
  mutate(ONS_ID = factor(ONS_ID, levels = ONS_ID)) %>%
  ggplot() +
  geom_point(aes(y=ONS_ID, x = sli_diff_count, colour = names(colours)[1])) +
  geom_point(aes(y=ONS_ID, x = pct_diff_count, colour = names(colours)[2])) +
  geom_vline(xintercept = 0)  +
  scale_colour_manual(values = colours) +
  theme(legend.position = "bottom") +
  labs(title = "Count Differences: ONS Neighbourhood 2016 Total Population using two DA-to-ONS methods",
       subtitle = "Comparing proportional assignment and single-link indicator to official StatsCan/ONS Data",
       x = "Calculated Value Compared to Ground Truth",
       colour = "Data Source")

# DT::datatable
comp_table_diffs %>%
  #filter(pop2016_ons > 0) %>%
  #filter(ONS_ID > 0) %>%
  #mutate(pct_diff = (pop2016_pct - pop2016_ons)/pop2016_ons,
  #       sli_diff = (pop2016_sli- pop2016_ons)/pop2016_ons)  %>%
  DT::datatable()




########## LOOKING AGAIN AT RESIDENTIAL OSM ZONES
da_ons_intersect %>% filter(ONS_ID == 64, pct_overlap_rnd > 0) %>% left_join(da_ott) %>% left_join(sc_pop2016total) %>% sf::st_as_sf() %>% sf::st_transform(crs = "WGS84") %>% leaflet() %>% addTiles() %>% addPolygons(data = filter(sf::st_transform(ons_trim, crs = "WGS84"), ONS_ID == 64), color = "green") %>%addPolygons(label = ~ paste0(DAUID, ": overlap ", pct_overlap_rnd, "; StatsCan pop = ", pop2016_sc))

ons_trim %>%
  sf::st_transform(crs = "WGS84") %>%
  leaflet() %>% addTiles() %>% addPolygons(label = ~ ONS_ID)




#### PREPARING PROGRAMMATIC WAY OF COMPARING SIMPLE COUNT VARIABLES
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

  citywide <- dplyr::tibble(
    sc_var_id = sc_var_id,
    ons_var_id = ons_var_id,
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
                    ons_var_id = ons_var_id, .before = 1)

  return(results)
}

# number employed
get_values_for_comparison(sc_df = sc_labour2016, ons_data = ons_data,
                          da_ons_intersect = da_ons_intersect,
                          sc_var_id = "31003", ons_var_id = "CDP304", var_type = "count")

# population
comp_table <- get_values_for_comparison(sc_df = sc_pop2016, ons_data = ons_data,
                                        da_ons_intersect = da_ons_intersect,
                                        sc_var_id = "1000", ons_var_id = "pop2016", var_type = "count")


# number employed
z <- get_values_for_comparison(sc_df = sc_labour2016, ons_data = ons_data,
                              da_ons_intersect = da_ons_intersect,
                              sc_var_id = "31003", ons_var_id = "CDP304", var_type = "count") %>%
  create_diff_table()

z
########### DO COMPARISONS
# mean/sd of differences? absolutes amd proportions
create_diff_table <- function(comp_table) {

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
              overlap_diff_pop_sd = sd(overlap_diff_prop),
              sli_diff_count_mean = mean(sli_diff_count),
              sli_diff_count_sd = sd(sli_diff_count),
              sli_diff_prop_mean = mean(sli_diff_prop),
              sli_diff_pop_sd = sd(sli_diff_prop)
    ) %>%
    dplyr::mutate(scope = "neighbourhoods", .before = 1)

  # also create city-wide comparison
  comp_table_summary_city <- comp_table_diffs %>%
    filter(as.numeric(ONS_ID) == 0) %>%
    summarise(overlap_diff_count_mean = mean(overlap_diff_count),
              overlap_diff_count_sd = sd(overlap_diff_count),
              overlap_diff_prop_mean = mean(overlap_diff_prop),
              overlap_diff_pop_sd = sd(overlap_diff_prop),
              sli_diff_count_mean = mean(sli_diff_count),
              sli_diff_count_sd = sd(sli_diff_count),
              sli_diff_prop_mean = mean(sli_diff_prop),
              sli_diff_pop_sd = sd(sli_diff_prop)
    )  %>%
    dplyr::mutate(scope = "city", .before = 1)

  # put it all together and return it
  dplyr::bind_rows(comp_table_summary_city, comp_table_summary_hoods) %>%
    dplyr::bind_cols(dplyr::distinct(comp_table, sc_var_id, ons_var_id), .)

}



############# NOW RUN SOME ANALYSES!!

vars_for_analysis <- readr::read_csv("inputs/vars_for_analysis.csv", col_types = readr::cols(.default = "c"))

results <- dplyr::tibble()

for (i in 1:nrow(vars_for_analysis)){
  sc_df <- get(vars_for_analysis$sc_df[[i]])
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
