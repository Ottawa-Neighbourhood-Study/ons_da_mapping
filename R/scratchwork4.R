#looking into possible issues:
#Do you happen to have a map of the DA-SLI file
#showing which DAs were assigned to which neighbourhoods? I'm asking because it
#look like the Riverview neighbourhood isn't getting a high enough pop assigned
#to it from DAs (about 50% the pop it should have). East Industrial (adjacent to
#Riverview) has over double the pop it should have. So I'm wondering if a quick
#fix could be to move some DAs that are currently allocated to East Industrial
#over to Riverview.

library(tidyverse)
library(leaflet)
targets::tar_load(c(da_shp, da_ons_sli_opt_mse2, ons_shp))

#make a big big plot

(da_shp %>% left_join(da_ons_sli_opt_mse2, by = "DAUID") %>%
  ggplot() + geom_sf(aes(fill=as.factor(ONS_ID)), size=0.2, color="grey") +
  theme(legend.position="none") + geom_sf(data=ons_shp, fill=NA) +
  labs(title="da_ons_sli_opt_mse2 to ONS Neighbourhoods")) %>%
  ggsave(plot = ., filename = "da_ons_sli_opt_mse2_check.png", dpi = 1000)


## which one is riverview??
ons_names <- ons_shp %>% sf::st_set_geometry(NULL) %>%
  mutate(ONS_ID = as.character(ONS_ID))

df <- (da_shp %>% left_join(da_ons_sli_opt_mse2, by = "DAUID") ) %>%
  left_join(ons_names, by = "ONS_ID") %>%
  sf::st_transform(crs = "WGS84")


df %>%
  filter(stringr::str_detect(Name, "Riverview|East Industrial")) %>%
  ggplot() + geom_sf(aes(fill=as.factor(ONS_ID)), size=0.2, color="grey") +
  geom_sf(data=filter(ons_shp, stringr::str_detect(Name, "Riverview|East Ind")), fill=NA)#, mapping=aes(fill=as.factor(ONS_ID)),alpha=0.5 )


leaflet() %>%
  addTiles() %>%
  addPolygons(data=filter(ons_shp, stringr::str_detect(Name, "Riverview|East Ind")) %>% sf::st_transform(crs="WGS84"), fill = "NA",opacity = 1, color="black", label=~Name) %>%
  addPolygons(data = filter(df, stringr::str_detect(Name, "Riverview|East Industrial")), label = ~ paste0(DAUID, ": ", Name), stroke=1)


theme(legend.position="none")


#### double-checking against ONS data
library(tidyverse)
library(leaflet)
targets::tar_load(c(da_shp, da_ons_sli_opt_mse2, ons_shp))
targets::tar_load(ons_data)
targets::tar_load(sc_pop2016)

# get 2016 ONS pop data by neighbourhood
ons_pop <- ons_data %>%
  filter(polygon_attribute == "pop2016") %>%
  select(ONS_ID,pop2016_official = value)

# get statscan 2016 pop data by DA
sc_dapop2016 <- sc_pop2016 %>%
  filter(TEXT_ID == "1000") %>%
  select(DAUID = GEO_ID, pop2016_sc = T_DATA_DONNEE) %>%
  mutate(pop2016_sc = as.numeric(pop2016_sc)) #%>% summarise(pop = sum(pop2016_sc))

# use SLI to group DAs into hoods, get hoodwise DA sum of population
ons_sc_pop <- da_ons_sli_opt_mse2 %>%
  left_join(sc_dapop2016, by = "DAUID") %>%
  group_by(ONS_ID) %>%
  summarise(pop2016_sli_mse2 = sum(pop2016_sc, na.rm = TRUE))

# combine ONS source of truth with computed DA-based values
pop_compare <- left_join(ons_pop, ons_sc_pop, by = "ONS_ID") %>%
  left_join(ons_names, by = "ONS_ID") %>%
  select(ONS_ID, Name, everything(), -Name_FR) %>%
  mutate(diff = pop2016_sli_mse2 - pop2016_official,
         diff_pct = diff/pop2016_official)

write_csv(pop_compare, "outputs/test_files/comparing_true_and_computed_populations.csv")
