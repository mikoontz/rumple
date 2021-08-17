# chloropleth map of US rumple index by county

library(dplyr)
library(sf)
library(tmap)
library(USAboundaries)
library(ggplot2)

# External sources for US state and county geometry info
us_states_lite <- USAboundaries::us_states(resolution = "low") %>% dplyr::select(state_name)
us_counties_lite <- USAboundaries::us_counties(resolution = "low") %>% dplyr::select(state_name, name, countyns)

### Using 90m DEM
# States
rumple_states <- sf::st_read(file.path("data", "out", "area-of-states.geojson")) %>% st_drop_geometry()
rumple_states$rumple <- rumple_states$sa / rumple_states$pa
rumple_states <- 
  rumple_states %>% 
  left_join(us_states_lite, by = c("NAME" = "state_name")) %>% 
  st_as_sf()

rumple_states %>% arrange(desc(rumple)) %>% pull(NAME)
rumple_states %>% arrange(rumple)

rumple_states_conus <-
  rumple_states %>% 
  dplyr::filter(!(NAME %in% c("United States Virgin Islands", "Commonwealth of the Northern Mariana Islands", "Alaska", "Hawaii", "Guam", "American Samoa", "Puerto Rico")))

tmap::tm_shape(rumple_states_conus) +
  tmap::tm_fill(col = "rumple") +
  tmap::tm_borders()

### Counties
rumple_counties <- 
  sf::st_read(file.path("data", "out", "area-of-counties.geojson")) %>% 
  dplyr::rename_all(.funs = list(tolower)) %>% 
  dplyr::mutate(rumple = sa / pa) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::left_join(us_counties_lite, by = c("name", "countyns")) %>% 
  sf::st_as_sf()

rumple_counties %>% arrange(desc(rumple))
rumple_counties %>% arrange(rumple)

rumple_counties_conus <-
  rumple_counties %>% 
  dplyr::filter(!is.na(state_name)) %>%
  dplyr::filter(!(state_name %in% c("United States Virgin Islands", "Commonwealth of the Northern Mariana Islands", "Alaska", "Hawaii", "Guam", "American Samoa", "Puerto Rico")))

tmap::tm_shape(rumple_counties_conus) +
  tmap::tm_fill(col = "rumple", n = 20, palette = viridisLite::cividis(20)) +
  tmap::tm_borders()

### Using 10m DEM
# States
rumple_states <- sf::st_read(file.path("data", "out", "area-of-states_10m.geojson")) %>% st_drop_geometry()
rumple_states$rumple <- rumple_states$sa / rumple_states$pa
rumple_states <- 
  rumple_states %>% 
  left_join(us_states_lite, by = c("NAME" = "state_name")) %>% 
  st_as_sf()

rumple_states %>% arrange(desc(rumple))
rumple_states %>% arrange(rumple)

rumple_states_conus <-
  rumple_states %>% 
  dplyr::filter(!(NAME %in% c("United States Virgin Islands", "Commonwealth of the Northern Mariana Islands", "Alaska", "Hawaii", "Guam", "American Samoa", "Puerto Rico")))

tmap::tm_shape(rumple_states_conus) +
  tmap::tm_fill(col = "rumple") +
  tmap::tm_borders()

### Counties
rumple_counties <- 
  sf::st_read(file.path("data", "out", "area-of-counties_10m.geojson")) %>% 
  dplyr::rename_all(.funs = list(tolower)) %>% 
  dplyr::mutate(rumple = sa / pa) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::left_join(us_counties_lite, by = c("name", "countyns")) %>% 
  sf::st_as_sf()

rumple_counties %>% arrange(desc(rumple))
rumple_counties %>% arrange(rumple)

rumple_counties_conus <-
  rumple_counties %>% 
  dplyr::filter(!is.na(state_name)) %>%
  dplyr::filter(!(state_name %in% c("United States Virgin Islands", "Commonwealth of the Northern Mariana Islands", "Alaska", "Hawaii", "Guam", "American Samoa", "Puerto Rico"))) %>% 
  dplyr::mutate(`Rumple index` = rumple)

county_rumple_tmap <-
  tmap::tm_shape(rumple_counties_conus) +
  tmap::tm_fill(col = "Rumple index", palette = viridisLite::cividis(20), n = 20) +
  tmap::tm_borders() +
  tmap::tm_layout(legend.position = c("right", "bottom"), frame = FALSE)

county_rumple_tmap

dir.create("figs", showWarnings = FALSE)
tmap::tmap_save(tm = county_rumple_tmap, filename = "figs/county-rumple-10m.png", height = 6, width = 10, units = "in")

ggplot(rumple_counties, aes(x = rumple)) +
  geom_histogram(bins = 100)

sum(rumple_counties$aland + rumple_counties$awater)
sum(rumple_counties$pa)
sum(rumple_counties$sa)

sum(rumple_counties$sa) / sum(rumple_counties$pa)
sum(rumple_states$sa)
sum(rumple_states$pa)

rc_low <- 
  rumple_counties_conus %>% 
  dplyr::mutate(rumple = ifelse(rumple > 1.05, NA, rumple))

tmap::tm_shape(rc_low) +
  tmap::tm_fill(col = "rumple", palette = viridisLite::viridis(20)) +
  tmap::tm_borders()

alaska_counties <-
  rumple_counties %>% filter(state_name == "Alaska")
tmap::tm_shape(alaska_counties) +
  tmap::tm_fill(col = "rumple", palette = viridisLite::viridis(20)) +
  tmap::tm_borders()


hawaii_counties <-
  rumple_counties %>% filter(state_name == "Hawaii")
tmap::tm_shape(hawaii_counties) +
  tmap::tm_fill(col = "rumple", palette = viridisLite::viridis(20)) +
  tmap::tm_borders()

co_counties <-
  rumple_counties %>% filter(state_name == "Colorado")
tmap::tm_shape(co_counties) +
  tmap::tm_fill(col = "rumple", palette = viridisLite::viridis(20)) +
  tmap::tm_borders()

hi_lo <-
  rumple_counties_conus %>% 
  mutate(rumple = ifelse(rumple > sum(rumple_counties$sa) / sum(rumple_counties$pa)
, 1, 0))
tmap::tm_shape(hi_lo) +
  tmap::tm_fill(col = "rumple", palette = viridisLite::viridis(20)) +
  tmap::tm_borders()
