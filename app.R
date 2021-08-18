#
# NYC school and environment mapping visualization
# Janice Lee, August 2021
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(htmlTable)
library(plotly)
library(RColorBrewer)

clean_name <- function(n) {
    n <- gsub('%+', 'p', n)
    n <- gsub('#+', 'n', n)
    n <- gsub("[^a-zA-Z0-9_]+", "_", n)
    n <- tolower(trimws(n))
    n
}

# Load data 
schools <- as_tibble(read_csv('./data/schools.csv'))
aq_by_uhf_sp <- rgdal::readOGR('./data/aq_by_uhf', layer='aq_by_uhf_sp')
er_by_modzcta_sp <- rgdal::readOGR('./data/er_by_modzcta', layer='er_by_modzcta_sp')

# Utility values and functions
school_category_options <- c(
    'Elementary', 
    'K-8', 
    'High school', 
    'Junior High-Intermediate-Middle', 
    'Secondary School', 
    'K-12 all grades', 
    'Early Childhood', 
    'Ungraded'
)
school_dem_display_options <- c(
    'Economic Need Index',
    '% Poverty',
    '% Female',
    '% Male',
    '% Asian',
    '% Black',
    '% Hispanic',
    '% Multi-Racial',
    '% Native American',
    '% White',
    '% Students with Disabilities',
    '% English Language Learners'
)
area_options <- c(
    'Air Quality (PM2.5 mcg/m3)' = 'air_q',
    '# Pneumonia Visits' = 'n_pne_v',
    '# Pneumonia Admissions' = 'n_pne_a',
    'Pneumonia Visits per Capita' = 'p_pne_v',
    'Pneumonia Admissions per Capita' = 'p_pne_a'
)

get_area_popup <- function(area_type=area_options) {
    if (area_type == 'air_q') {
        paste('UHF Neighborhood: ', aq_by_uhf_sp$uhf_ngh, '<br/>',
              'Air Quality (PM2.5 mcg/m3): ', aq_by_uhf_sp$air_q)
    } else {
        paste('Modified Zipcode:', er_by_modzcta_sp$mod_zcta)
    }
}



# --- UI ---
ui <- fluidPage(
    # Application title and navigation
    navbarPage('NYC Schools\' Demographic vs. Environment',
               tabPanel('Map',
                        div(class='outer',
                            # Map sidebar
                            sidebarLayout(
                                sidebarPanel(
                                    selectInput('school_dem', 
                                                label   = 'Select a demographic', 
                                                choices = school_dem_display_options),
                                    selectInput('school_category', 
                                                label   = 'Select a school type', 
                                                choices = school_category_options),
                                    prettyRadioButtons('area_type', 
                                                       label   = 'Select an environment data type', 
                                                       choices = area_options),
                                    tags$hr(),
                                    tags$h5('Notes:'),
                                    tags$ul(
                                        tags$li('Demographic data is from the 2019-2020 school year.'), 
                                        tags$li('Air quality data is the 2018 annual average of PM2.5 in milligrams / cubic meters (mg/m3) - higher is worse.'),
                                        tags$li('Pneumonia visits and admissions data are the total visits and admissions to NYC emergency departments for influenza-like illness and/or pneumonia in 2020.'),
                                        tags$li('School marker size corresponds to the total enrollment of the school.')
                                    )
                                ),
                                
                                # Map and plots
                                mainPanel(
                                    leafletOutput('map'),
                                    tags$br(),
                                    fluidRow(
                                        column(8, plotlyOutput('school_plot')),
                                        column(4, tableOutput('area_plot'))
                                    )
                                )
                            )
                        )
               ),
               tabPanel('About',
                    div(class='outer',
                        mainPanel(
                            tags$h3('Background'),
                            tags$br(),
                            'This mapping tool provides users with a way to explore the demographics of NYC schools and their geospatial/environmental contexts.',
                            'I created this visualization as a way to better understand NYC school data by placing it within a spatial framework, ',
                            'and because I was curious about how a school\'s environment may be associated with its demographics.',
                            tags$br(),
                            tags$br(),
                            'I was interested in 2 location-based factors: air quality and COVID-19 hospital visits.',
                            'I wasn\'t able to find data regarding COVID-19 specifically, but ED visits for influenza-like illness and pneuomia serves as a proxy for this data.',
                            'Hopefully this tool is interesting to you as well!',
                            tags$br(),
                            tags$br(),
                            
                            tags$h3('Code'),
                            tags$br(),
                            'The code and data for this site can be found on ',
                            tags$a(href='https://github.com/lee-janice/nyc-schools-mapping-tool', target="_blank", 'GitHub'),
                            '.',
                            tags$br(),
                            tags$br(),
                            
                            tags$h3('Data Sources'),
                            tags$br(),
                            tags$ul(
                                tags$li(
                                    tags$b('NYC school demographics: '), 'The ',
                                    tags$a(href='https://infohub.nyced.org/reports/school-quality/information-and-data-overview', 
                                           target="_blank",
                                           'demographic snapshot '), 'in NYC Department of Education\'s school quality report.'
                                ),
                                tags$li(
                                    tags$b('NYC school locations: '),
                                    tags$a(href='https://data.cityofnewyork.us/Education/2019-2020-School-Locations/wg9x-4ke6', 
                                           target="_blank",
                                           '2019-2020 school locations '), 'from the DOE.'
                                ),
                                tags$li(
                                    tags$b('Air quality measures: '),
                                    tags$a(href='https://data.cityofnewyork.us/Environment/Air-Quality/c3uy-2p5r', 
                                           target="_blank",
                                           'Air quality surveillance data '), 'from the Department of Health and Mental Hygiene (DOHMH).'
                                ),    
                                tags$li(
                                    tags$b('United Hospital Fund (UHF) neighborhood boundaries: '), 'Miguel Paz\'s ',
                                    tags$a(href='https://gist.github.com/miguelpaz/edbc79fc55447ae736704654b3b2ef90', 
                                           target="_blank",
                                           'GitHub repository, '), 'containing a GeoJSON converted from a Shapefile provided by the NYC Health Department.'
                                ), 
                                tags$li(
                                    tags$b('Emergency department data: '),
                                    tags$a(href='https://data.cityofnewyork.us/Health/Emergency-Department-Visits-and-Admissions-for-Inf/2nwg-uqyg', 
                                           target="_blank",
                                           'ED visits and admissions data '),  ' for influenza-like illness and/or pneumonia from the DOHMH.'
                                ),
                                tags$li(
                                    tags$b('Modified Zip Code Tabulation Areas (MODZCTA) boundaries: '),
                                    tags$a(href='https://data.cityofnewyork.us/Health/Modified-Zip-Code-Tabulation-Areas-MODZCTA-/pri4-ifjk', 
                                           target="_blank",
                                           'Shapefile '), 'for mapping MODZCTA in NYC from the DOHMH. MODZCTA are used by the DOHMH for mapping COVID-19 data.'
                                )
                            ),
                            
                            tags$h3('Author'),
                            tags$br(),
                            'This visualization was created by Janice Lee. You can contact me at janice.lee@pomona.edu!',
                            tags$br(),
                            tags$br()
                        ))
                )
        )
)



# --- SERVER ---
server <- function(input, output) {

    # Get visualization data
    area_vis <- reactive({
        if (input$area_type == 'air_q') {
            aq_by_uhf_sp
        } else {
            er_by_modzcta_sp
        }
    })
    schools_vis <- reactive({
        schools %>%
            filter(location_category_description == input$school_category)
    })
    
    # Get color palettes
    area_pal <- reactive({
        colorNumeric(palette='YlOrRd', domain=area_vis()[[input$area_type]])
    })
    schools_pal <- reactive({
        colorNumeric(palette='Blues', domain=schools_vis()[[clean_name(input$school_dem)]])
    })
    
    # Get column names from input 
    area_col_name <- reactive({
        input$area_type
    })
    school_col_name <- reactive({
        clean_name(input$school_dem)
    })
    
    # Get area legend title 
    area_legend_title <- reactive({
        switch(input$area_type,
               'air_q' = 'Air Quality <br> (PM2.5 mcg/m3)',
               'n_pne_v' = '# Pneumonia Visits',
               'n_pne_a' = '# Pneumonia Admissions',
               'p_pne_v' = 'Pneumonia Visits per Capita',
               'p_pne_a' = 'Pneumonia Admissions per Capita')
    })
    
    # Render map 
    output$map <- renderLeaflet({
        leaflet() %>%
            setView(lat=40.730610, lng=-73.935242, zoom = 10) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            # Fill in area polygons 
            addPolygons(data         = area_vis(),
                        stroke       = TRUE,
                        color        = '#808080',
                        weight       = 1,
                        fillOpacity  = 0.5,
                        fillColor    = ~area_pal()(area_vis()[[area_col_name()]]),
                        highlight    = highlightOptions(color        = "green",
                                                        weight       = 2, 
                                                        bringToFront = F, 
                                                        opacity      = 0.7),
                        layerId      = ~area_vis()@data$row_id,
                        popup        = ~get_area_popup(input$area_type)) %>%
            addLegend(data   = area_vis(),
                      pal    = area_pal(),
                      values = ~area_vis()[[area_col_name()]],
                      title  = area_legend_title()) %>%
            # Display schools on the map 
            addCircleMarkers(data        = schools_vis(),
                             lng         = ~longitude,
                             lat         = ~latitude,
                             radius      = ~logb(total_enrollment/max(schools_vis()$total_enrollment), base=0.1) * 5,
                             color       = ~schools_pal()(schools_vis()[[school_col_name()]]),
                             stroke      = TRUE,
                             weight      = 1,
                             fillOpacity = 0.75,
                             layerId     = ~schools_vis()$row_id,
                             popup  = ~paste(schools_vis()$school_name)) %>%
            addLegend(data   = schools_vis(),
                      pal    = schools_pal(),
                      values = ~schools_vis()[[clean_name(input$school_dem)]],
                      title  = input$school_dem)
            
    })
    
    # Render school plot 
    x <- school_dem_display_options
    y <- rep(0, length(x))
    school <- data.frame(x, y)
    
    xform <- list(categoryorder = "array",
                  categoryarray = school_dem_display_options,
                  tickangle     = 45,
                  title         = "")
    yform <- list(range = c(0, 100),
                  title = "")
    school_pal <- brewer.pal(3, name = "Blues")
    school_bar_colors <- list(color = c(school_pal[1], school_pal[1],
                                        school_pal[2], school_pal[2], school_pal[2], school_pal[2],
                                        school_pal[2], school_pal[2], school_pal[2], school_pal[2],
                                        school_pal[3], school_pal[3]))
    
    output$school_plot <- renderPlotly({
        plot_ly(data=school, type='bar', x=~x, y=~y, marker=school_bar_colors) %>% 
            layout(title = 'School Demographics',
                   xaxis = xform, 
                   yaxis = yform) 
    })
    
    # Change school plot when user clicks on a school 
    observeEvent(input$map_marker_click, {
        click <- input$map_marker_click
        
        y <- unlist(schools[clean_name(school_dem_display_options)][click$id,], use.names=FALSE)
        school <- data.frame(x, y)

        output$school_plot <- renderPlotly({
            plot_ly(data=school, type='bar', x=~x, y=~y, marker=school_bar_colors) %>% 
                layout(title = schools$school_name[click$id],
                       xaxis = xform, 
                       yaxis = yform) 
        })
    })
    
    # Render area plot 
    area_col_names <- reactive({
        if (input$area_type == 'air_q') {
            c('uhf_ngh', 'air_q', 'borough')
        } else {
            c('mod_zcta', 'p_pne_v', 'p_pne_a', 'n_pne_v', 'n_pne_a', 't_ed_v', 'pop_est')
        }
    })
    
    area_data <- reactive({
        if (input$area_type == 'air_q') {
            area_data_names <- c('UHF Neighborhood', 
                                 'Air Quality', 
                                 'Borough')
        } else {
            area_data_names <- c('Modified Zip Code',
                                 'Pneumonia Visits Per Capita',
                                 'Pneumonia Admissions Per Capita',
                                 '# Pneumonia Visits',
                                 '# Pneumonia Admissions',
                                 'Total Emergency Department Visits',
                                 'Estimated Population')
        }
        data.frame(Name = area_data_names, 
                   Value = double(length=length(area_data_names)))
    })
    
    output$area_plot <- renderTable(area_data(),
                                   caption  = 'Area Data',
                                   caption.placement = 'top',
                                   striped  = TRUE,
                                   hover    = TRUE,
                                   bordered = TRUE)
    
    # Change area plot when user clicks on an area 
    observeEvent(input$map_shape_click, {
        click <- input$map_shape_click
        
        area <- area_data()
        area$Value <- matrix(area_vis()@data[area_col_names()][click$id,])
        
        output$area_plot <- renderTable(area,
                                       caption  = 'Area Data',
                                       caption.placement = 'top',
                                       striped  = TRUE,
                                       hover    = TRUE,
                                       bordered = TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
# Deploy the application (run locally)
# rsconnect::deployApp(account="leejanice")
