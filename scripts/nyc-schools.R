# ---
# DATA SOURCES
# School demographic: https://infohub.nyced.org/reports/school-quality/information-and-data-overview
# School locations: https://data.cityofnewyork.us/Education/2019-2020-School-Locations/wg9x-4ke6
# UHF GeoJSONs: https://gist.github.com/miguelpaz/edbc79fc55447ae736704654b3b2ef90
# Air quality: https://data.cityofnewyork.us/Environment/Air-Quality/c3uy-2p5r
# ER visits: https://data.cityofnewyork.us/Health/Emergency-Department-Visits-and-Admissions-for-Inf/2nwg-uqyg
# --- 
library(tidyverse)
library(ggmap)
library(ggmapstyles)
library(sf)
library(geojsonsf)
library(leaflet)

# --- DATA PROCESSING --- 

# read data from csv
locations_raw <- as_tibble(read_csv('~/Desktop/life/nyc/data/nyc-school-locations.csv'))
demographic_raw <- as_tibble(read_csv('~/Desktop/life/nyc/data/nyc-school-demographic-snapshot.csv'))
air_quality_raw <- as_tibble(read_csv('~/Desktop/life/nyc/data/nyc-air-quality.csv'))
er_visits_raw <- as_tibble(read_csv('~/Desktop/life/nyc/data/nyc-er-visits-pne.csv'))
uhf_geo_raw <- geojson_sf("~/Desktop/life/nyc/data/uhf42.geojson")
modzcta_raw <- as_tibble(read_csv('~/Desktop/life/nyc/data/nyc-modified-zipcode-areas.csv'))

# Locations --- 
locations <- locations_raw %>% 
  select(system_code, location_name, Location_Category_Description, LONGITUDE, LATITUDE)
names(locations)[names(locations) == 'system_code'] <- 'DBN'
locations$LONGITUDE <- locations$LONGITUDE %>% as.double()
locations$LATITUDE <- locations$LATITUDE %>% as.double()

# Demographic --- 
demographic <- demographic_raw %>% 
  filter(Year == '2019-20') %>%
  select(-starts_with('Grade'), -'Year')
# select() %>%
# select(DBN, `School Name`, `% Poverty`, `Economic Need Index`, `Total Enrollment`) 

demographic$`% Poverty` <- demographic$`% Poverty` %>%
  str_replace('%', '') %>%
  str_replace('.*[ ]', '') %>%
  as.double()

demographic$`Economic Need Index` <- demographic$`Economic Need Index` %>%
  str_replace('%', '') %>%
  str_replace('.*[ ]', '') %>%
  as.double()

# Geo --- 
uhf_geo <- uhf_geo_raw

# Air quality --- 
air_quality <- air_quality_raw %>%
  filter(Name == 'Fine Particulate Matter (PM2.5)') %>%
  filter(`Geo Type Name` == 'UHF42') %>%
  filter(`Time Period` == 'Annual Average 2018') %>% 
  filter(duplicated(`Geo Join ID`) == FALSE) %>%
  select(`Geo Join ID`, `Data Value`, `Geo Place Name`) 
names(air_quality)[names(air_quality) == 'Geo Join ID'] <- 'uhfcode'

# ER visits --- 
er_visits <- er_visits_raw %>%
  filter(grepl('2020$', date)) %>%
  select(mod_zcta, total_ed_visits, ili_pne_visits, ili_pne_admissions) %>%
  plyr::ddply('mod_zcta', plyr::numcolwise(sum))

# Modified zipcode areas --- 
modzcta <- modzcta_raw %>%
  select(MODZCTA, pop_est, the_geom) 
names(modzcta)[names(modzcta) == 'MODZCTA'] <- 'mod_zcta'

# join location and demographic data
schools <- inner_join(locations, demographic, by='DBN') %>% drop_na()
glimpse(schools)

# join air quality and uhf geo data 
aq_by_geo <- inner_join(air_quality, uhf_geo, by='uhfcode') %>% drop_na()
names(aq_by_geo)[names(aq_by_geo) == 'Geo Join ID'] <- 'uhf_code'
names(aq_by_geo)[names(aq_by_geo) == 'Data Value'] <- 'air_quality_measure'

# join er visits and modzcta data
er_by_modzcta <- inner_join(er_visits, modzcta, by='mod_zcta') %>% drop_na()

# turn data into sp (SpatialDataFrame) object
aq_by_geo_sp <- st_as_sf(aq_by_geo) %>% as_Spatial()
er_by_modzcta_sp <- st_as_sf(er_by_modzcta, wkt='the_geom') %>% as_Spatial()

# --- MAP VISUALISATION --- 
gmaps_key <- 'AIzaSyAE0VgNlDTHIfZsYvfok2hbYo3efr-k1zU'
register_google(key=gmaps_key)

get_static_map <- function(school_category, center='new york, ny, usa', zoom=10) {
  # category = [Elementary, K-8, High school, Junior High-Intermediate-Middle, 
  # Secondary School, K-12 all grades, Early Childhood, Ungraded]
  map <- get_snazzymap(center  = center,
                       zoom    = zoom,
                       maptype = 'roadmap',
                       mapRef  = 'https://snazzymaps.com/style/15/subtle-grayscale')
  
  schools_vis <- schools %>% 
    filter(Location_Category_Description == school_category)
  
  ggmap(map) + 
    geom_point(data=schools_vis, 
               aes(x     = LONGITUDE, 
                   y     = LATITUDE,
                   color = `% Poverty`, 
                   alpha = 0.3,
                   size  = `Total Enrollment`)) + 
    scale_colour_gradient(high = "#132B43", low = "#56B1F7")
}

get_interactive_map <- function(school_category, area_category) {
  # area category: aq, er
  schools_vis <- schools %>% 
    filter(Location_Category_Description == school_category)
  schools_vis$popup <- paste('Name: ', schools_vis$`School Name`,  '<br/>',
                             'Economic Need Index: ', schools_vis$`Economic Need Index`, '% <br/>',
                             '% Asian: ', schools_vis$`% Asian`,  '<br/>',
                             '% Black: ', schools_vis$`% Black`,  '<br/>',
                             '% Hispanic: ', schools_vis$`% Hispanic`,  '<br/>',
                             '% Native American: ', schools_vis$`% Native American`,  '<br/>',
                             '% White: ', schools_vis$`% White`,  '<br/>',
                             '% Multi-Racial: ', schools_vis$`% Multi-Racial`,  '<br/>',
                             '% Students with Disabilities: ', schools_vis$`% Multi-Racial`,  '<br/>',
                             '% English Language Learners: ', schools_vis$`% English Language Learners`)
  
  schools_pal <- colorNumeric(palette='Blues', domain=schools_vis$`Economic Need Index`)
  map <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron)
  
  if (area_category == 'aq') {
    area_pal <- colorNumeric(palette='YlOrRd', domain=aq_by_geo_sp$air_quality_measure)
    map <- map %>% 
      addPolygons(data         = aq_by_geo_sp,
                  stroke       = TRUE,
                  color        = '#808080',
                  weight       = 1,
                  fillOpacity  = 0.5,
                  fillColor    = ~area_pal(aq_by_geo_sp$air_quality_measure),
                  popup        = ~paste('UHF Neighborhood: ', aq_by_geo_sp$uhf_neigh, '<br/>',
                                        'Air Quality Measure: ', aq_by_geo_sp$air_quality_measure)) %>%
      addLegend(data   = aq_by_geo_sp, 
                pal    = area_pal, 
                values = ~aq_by_geo_sp$air_quality_measure,
                title  = 'Air Quality (PM2.5 mcg/m3)')
  } else {
    area_pal <- colorNumeric(palette='YlOrRd', domain=er_by_modzcta_sp$ili_pne_visits/er_by_modzcta_sp$pop_est)
    map <- map %>% 
      addPolygons(data         = er_by_modzcta_sp,
                  stroke       = TRUE,
                  color        = '#808080',
                  weight       = 1,
                  fillOpacity  = 0.5,
                  fillColor    = ~area_pal(er_by_modzcta_sp$ili_pne_visits/er_by_modzcta_sp$pop_est),
                  popup        = ~paste('% Admissions ', er_by_modzcta_sp$ili_pne_visits/er_by_modzcta_sp$pop_est, '<br/>',
                                        'Total ED Visits: ', er_by_modzcta_sp$total_ed_visits, '<br/>',
                                        'Pneumonia Visits: ', er_by_modzcta_sp$ili_pne_visits, '<br/>',
                                        'Pneumonia Admissions: ', er_by_modzcta_sp$ili_pne_admissions, '<br/>',
                                        'Estimated Population: ', er_by_modzcta_sp$pop_est)) %>%
      addLegend(data   = er_by_modzcta_sp, 
                pal    = area_pal, 
                values = ~er_by_modzcta_sp$ili_pne_visits/er_by_modzcta_sp$pop_est,
                title  = 'Pneumonia Admissions (%)')
  }
  map <- map %>%
    addCircleMarkers(data   = schools_vis, 
                     lng    = ~LONGITUDE,
                     lat    = ~LATITUDE,
                     radius = ~logb(`Total Enrollment`/max(schools_vis$`Total Enrollment`), base=0.1) * 5,
                     color  = ~schools_pal(schools_vis$`Economic Need Index`),
                     stroke = FALSE,
                     fillOpacity = 0.8, 
                     popup  = ~schools_vis$popup) %>%
    addLegend(data   = schools_vis,
              pal    = schools_pal,
              values = ~schools_vis$`Economic Need Index`,
              title  = 'Economic Need Index (%)')
  map
}
# get_static_map('High school', zoom=13)
get_interactive_map('High school', 'er') 


