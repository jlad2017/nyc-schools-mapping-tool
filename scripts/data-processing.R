# -------------
# DATA SOURCES
# -------------
# School demographic: https://infohub.nyced.org/reports/school-quality/information-and-data-overview
# School locations: https://data.cityofnewyork.us/Education/2019-2020-School-Locations/wg9x-4ke6
# UHF GeoJSONs: https://gist.github.com/miguelpaz/edbc79fc55447ae736704654b3b2ef90
# Air quality: https://data.cityofnewyork.us/Environment/Air-Quality/c3uy-2p5r
# ER visits: https://data.cityofnewyork.us/Health/Emergency-Department-Visits-and-Admissions-for-Inf/2nwg-uqyg
# -------------
library(tidyverse)
library(sf)
library(geojsonsf)
library(rgdal)

# Function to standardize column names 
clean_names <- function(.data) {
  n <- colnames(.data) 
  
  n <- gsub('%+', 'p', n)
  n <- gsub('#+', 'n', n)
  n <- gsub("[^a-zA-Z0-9_]+", "_", n)
  n <- tolower(trimws(n))
  
  colnames(.data) <- n
  .data
}

# Read data from csv
locations_raw <- as_tibble(read_csv('~/Desktop/life/nyc/raw-data/nyc-school-locations.csv'))
demographic_raw <- as_tibble(read_csv('~/Desktop/life/nyc/raw-data/nyc-school-demographic-snapshot.csv'))
air_quality_raw <- as_tibble(read_csv('~/Desktop/life/nyc/raw-data/nyc-air-quality.csv'))
er_visits_raw <- as_tibble(read_csv('~/Desktop/life/nyc/raw-data/nyc-er-visits-pne.csv'))
uhf_geo_raw <- geojson_sf("~/Desktop/life/nyc/raw-data/uhf42.geojson")
modzcta_raw <- as_tibble(read_csv('~/Desktop/life/nyc/raw-data/nyc-modified-zipcode-areas.csv'))

# --- Locations --- 
locations <- locations_raw %>% 
  select(system_code, Location_Category_Description, LONGITUDE, LATITUDE)
names(locations)[names(locations) == 'system_code'] <- 'DBN'
locations$LONGITUDE <- locations$LONGITUDE %>% as.double()
locations$LATITUDE <- locations$LATITUDE %>% as.double()

# --- Demographic --- 
demographic <- demographic_raw %>% 
  filter(Year == '2019-20') %>%
  select(-starts_with('Grade'), -'Year')

colnames_w_percent <- colnames(demographic)[grep('%', colnames(demographic))]
for (name in colnames_w_percent) {
  demographic[[name]] <- demographic[[name]] %>%
    str_replace('%', '') %>%
    str_replace('.*[ ]', '') %>%
    as.double()
}

demographic$`% Poverty` <- demographic$`% Poverty` %>%
  str_replace('%', '') %>%
  str_replace('.*[ ]', '') %>%
  as.double()

demographic$`Economic Need Index` <- demographic$`Economic Need Index` %>%
  str_replace('%', '') %>%
  str_replace('.*[ ]', '') %>%
  as.double()

# --- Geo --- 
uhf_geo <- uhf_geo_raw

# --- Air quality --- 
air_quality <- air_quality_raw %>%
  filter(Name == 'Fine Particulate Matter (PM2.5)') %>%
  filter(`Geo Type Name` == 'UHF42') %>%
  filter(`Time Period` == 'Annual Average 2018') %>% 
  filter(duplicated(`Geo Join ID`) == FALSE) %>%
  select(`Geo Join ID`, `Data Value`, `Geo Place Name`) 
names(air_quality)[names(air_quality) == 'Geo Join ID'] <- 'uhfcode'

# --- ER visits --- 
er_visits <- er_visits_raw %>%
  filter(grepl('2020$', date)) %>%
  select(mod_zcta, total_ed_visits, ili_pne_visits, ili_pne_admissions) %>%
  plyr::ddply('mod_zcta', plyr::numcolwise(sum))

# --- Modified zipcode areas --- 
modzcta <- modzcta_raw %>%
  select(MODZCTA, pop_est, the_geom) 
names(modzcta)[names(modzcta) == 'MODZCTA'] <- 'mod_zcta'

# Join data and clean column names
schools <- inner_join(locations, demographic, by='DBN') %>% drop_na() %>% clean_names()
schools$row_id <- 1:nrow(schools)

aq_by_uhf <- inner_join(air_quality, uhf_geo, by='uhfcode') %>% drop_na() %>% clean_names()
names(aq_by_uhf)[names(aq_by_uhf) == 'geo_join_id'] <- 'uhf_code'
names(aq_by_uhf)[names(aq_by_uhf) == 'data_value'] <- 'air_q'
aq_by_uhf$row_id <- 1:nrow(aq_by_uhf)

er_by_modzcta <- inner_join(er_visits, modzcta, by='mod_zcta') %>% drop_na() %>% clean_names() 
er_by_modzcta$mod_zcta <- er_by_modzcta$mod_zcta %>% as.character()
er_by_modzcta$pop_est <- er_by_modzcta$pop_est %>% as.integer()
names(er_by_modzcta)[names(er_by_modzcta) == 'total_ed_visits'] <- 't_ed_v'
names(er_by_modzcta)[names(er_by_modzcta) == 'ili_pne_visits'] <- 'n_pne_v'
names(er_by_modzcta)[names(er_by_modzcta) == 'ili_pne_admissions'] <- 'n_pne_a'
er_by_modzcta$p_pne_v <- er_by_modzcta$n_pne_v / er_by_modzcta$pop_est * 100 
er_by_modzcta$p_pne_a <- er_by_modzcta$n_pne_a / er_by_modzcta$pop_est * 100 
er_by_modzcta$p_pne_v <- round(er_by_modzcta$p_pne_v, 2)
er_by_modzcta$p_pne_a <- round(er_by_modzcta$p_pne_a, 2)
er_by_modzcta$row_id <- 1:nrow(er_by_modzcta)

# Turn data with geos into sp (SpatialDataFrame) objects
aq_by_uhf_sp <- st_as_sf(aq_by_uhf) %>% as_Spatial()
er_by_modzcta_sp <- st_as_sf(er_by_modzcta, wkt='the_geom') %>% as_Spatial()

# Write data 
write.csv(schools, '~/Desktop/life/nyc/data/schools.csv')
unlink('~/Desktop/life/nyc/data/aq_by_uhf/*')
unlink('~/Desktop/life/nyc/data/er_by_modzcta/*')
writeOGR(aq_by_uhf_sp, 
         dsn    = '~/Desktop/life/nyc/data/aq_by_uhf', 
         layer  = 'aq_by_uhf_sp', 
         driver = 'ESRI Shapefile')
writeOGR(er_by_modzcta_sp, 
         dsn    = '~/Desktop/life/nyc/data/er_by_modzcta', 
         layer  = 'er_by_modzcta_sp', 
         driver = 'ESRI Shapefile')

