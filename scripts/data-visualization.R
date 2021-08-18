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
library(sf)
library(geojsonsf)
library(leaflet)

# Read data
schools <- as_tibble(read_csv('~/Desktop/life/nyc/data/schools.csv'))
aq_by_uhf_sp <- readOGR('~/Desktop/life/nyc/data/aq_by_uhf', layer='aq_by_uhf_sp')
er_by_modzcta_sp <- readOGR('~/Desktop/life/nyc/data/er_by_modzcta', layer='er_by_modzcta_sp')

# --- MAP VISUALISATION ---
f <- read.table("~/Desktop/life/nyc/gmaps-key.txt", header=F,nrows=1)
key <- f[,1]
register_google(key=key)

get_static_map <- function(school_category, center='new york, ny, usa', zoom=10) {
  # category = [Elementary, K-8, High school, Junior High-Intermediate-Middle, 
  # Secondary School, K-12 all grades, Early Childhood, Ungraded]
  map <- ggmapstyles::get_snazzymap(center  = center,
                                    zoom    = zoom,
                                    maptype = 'roadmap',
                                    mapRef  = 'https://snazzymaps.com/style/15/subtle-grayscale')
  
  schools_vis <- schools %>% 
    filter(Location_Category_Description == school_category)
  
  ggmap(map) + 
    geom_point(data=schools_vis, 
               aes(x     = longitude, 
                   y     = latitude,
                   color = p_poverty, 
                   alpha = 0.3,
                   size  = total_enrollment)) + 
    scale_colour_gradient(high = "#132B43", low = "#56B1F7")
}

get_interactive_map <- function(school_category, area_category) {
  # area category: aq, er
  schools_vis <- schools %>% 
    filter(location_category_description == school_category)
  schools_vis$popup <- paste('Name: ', schools_vis$school_name,  '<br/>',
                             'Economic Need Index: ', schools_vis$economic_need_index, '% <br/>',
                             '% Asian: ', schools_vis$p_asian,  '<br/>',
                             '% Black: ', schools_vis$p_black,  '<br/>',
                             '% Hispanic: ', schools_vis$p_hispanic,  '<br/>',
                             '% Native American: ', schools_vis$p_native_american,  '<br/>',
                             '% White: ', schools_vis$p_white,  '<br/>',
                             '% Multi-Racial: ', schools_vis$p_multi_racial,  '<br/>',
                             '% Students with Disabilities: ', schools_vis$p_students_with_disabilities,  '<br/>',
                             '% English Language Learners: ', schools_vis$p_english_language_learners)
  
  schools_pal <- colorNumeric(palette='Blues', domain=schools_vis$economic_need_index)
  map <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron)
  
  if (area_category == 'aq') {
    area_pal <- colorNumeric(palette='YlOrRd', domain=aq_by_uhf_sp$air_quality_measure)
    map <- map %>% 
      addPolygons(data         = aq_by_uhf_sp,
                  stroke       = TRUE,
                  color        = '#808080',
                  weight       = 1,
                  fillOpacity  = 0.5,
                  fillColor    = ~area_pal(aq_by_uhf_sp$air_quality_measure),
                  popup        = ~paste('UHF Neighborhood: ', aq_by_uhf_sp$uhf_neigh, '<br/>',
                                        'Air Quality Measure: ', aq_by_uhf_sp$air_quality_measure)) %>%
      addLegend(data   = aq_by_uhf_sp, 
                pal    = area_pal, 
                values = ~aq_by_uhf_sp$air_quality_measure,
                title  = 'Air Quality (PM2.5 mcg/m3)')
  } else {
    area_pal <- colorNumeric(palette='YlOrRd', domain=er_by_modzcta_sp$p_pne_a)
    map <- map %>% 
      addPolygons(data         = er_by_modzcta_sp,
                  stroke       = TRUE,
                  color        = '#808080',
                  weight       = 1,
                  fillOpacity  = 0.5,
                  fillColor    = ~area_pal(er_by_modzcta_sp$p_pne_a),
                  popup        = ~paste('Pneumonia Admissions per Capita: ', 
                                        format(round(er_by_modzcta_sp$p_pne_a, 2), nsmall=2), 
                                        '<br/>',
                                        '# Pneumonia Admissions: ', er_by_modzcta_sp$n_pne_a, '<br/>',
                                        'Total ED Visits: ', er_by_modzcta_sp$t_ed_v, '<br/>',
                                        'Estimated Population: ', er_by_modzcta_sp$pop_est)) %>%
      addLegend(data   = er_by_modzcta_sp, 
                pal    = area_pal, 
                values = ~er_by_modzcta_sp$p_pne_a,
                title  = 'Pneumonia Admissions <br> per Capita')
  }
  map <- map %>%
    addCircleMarkers(data   = schools_vis, 
                     lng    = ~longitude,
                     lat    = ~latitude,
                     radius = ~logb(total_enrollment/max(schools_vis$total_enrollment), base=0.1) * 5,
                     color  = ~schools_pal(schools_vis$economic_need_index),
                     stroke = FALSE,
                     fillOpacity = 0.8, 
                     popup  = ~schools_vis$popup) %>%
    addLegend(data   = schools_vis,
              pal    = schools_pal,
              values = ~schools_vis$economic_need_index,
              title  = 'Economic Need <br> Index (%)')
  map
}
# get_static_map('High school', zoom=13)
get_interactive_map('High school', 'er') 


 