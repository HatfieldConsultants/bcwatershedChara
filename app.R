# Copyright 2022 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.



# 1 Import the dependent libraries and data -----------------------------------

library(elevatr)
library(fasstr)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(raster)
library(rgdal)
library(sf)
library(sp)
library(terra)
library(tidyverse)
library(markdown)
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinythemes) # used themes
library(DT)

# how to add GIS data is explained in maintain folder
load("data/mapdata_watershed.RDa")
load("data/mapdata_hydat.RDa") 

# 2 Define UI -----------------------------------------------------------------
ui <- navbarPage(id = "menus", theme = "bcgov.css",
                 title = "British Columbia Watershed Characterization",
                 tabPanel(title = "Watershed Map",
                          shinyjs::useShinyjs(),          # make tab pages to right
                          fluidPage(
                              column(6, 
                                    "Select a watershed polygon or a hydata station on the map",
                                    shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="850px"), size=2, color="#38598a")
                              ),
                              column(6, "Select a watershed polygon or a hydata station in the table",
                                     tabsetPanel( 
                                         tabPanel("Watershed Selection Table",
                                                  DT::DTOutput("watershedtabledf")
                                         ),
                                         tabPanel("HYDAT Station Selection Table", 
                                                  DT::DTOutput("hydattable")
                                         )    
                                     ),  # end of 2 tab tables
                                     tabsetPanel( 
                                         tabPanel("Watershed Info Sheet",
                                                  DT::DTOutput("watershedreport")
                                         ),
                                         tabPanel("Hydat station flow plot",
                                                  plotlyOutput("plot_flow_data")  # haven't code yet
                                         )
                                     ) # end of bottom result tabserPanel
                                    ), # end of right column 6
                              column(width = 12,
                                     style = "background-color:#003366; border-top:2px solid #fcba19;",
                                     tags$footer(class="footer",
                                                 tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                          tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                                  tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                          )
                                                 )
                                     )
                              )
                          ) # end of fluidpage
                 ), # end 1st page
                 
                 tabPanel(title = "Watershed Data List",
                              navlistPanel("Watershed Characterization Data Source List", widths = c(2,10), 
                                  tabPanel("Overview",
                                           tags$iframe(src = './page2_20220225.html', # put test.html to /www
                                                       width = '100%', height = '854px', 
                                                       frameborder = 0, scrolling = 'auto')
                                  ),
                                  tabPanel("Water Quality",
                                           tags$iframe(src = './p2_WaterQuality.html', # put test.html to /www
                                                       width = '100%', height = '854px', 
                                                       frameborder = 0, scrolling = 'auto')
                                  ),
                                  tabPanel("Surface Water",
                                           tags$iframe(src = './p2_SurfaceWater.html', # put test.html to /www
                                                       width = '100%', height = '854px', 
                                                       frameborder = 0, scrolling = 'auto')
                                  ),
                                  tabPanel("Groundwater",
                                           tags$iframe(src = './p2_Groundwater.html', # put test.html to /www
                                                       width = '100%', height = '854px', 
                                                       frameborder = 0, scrolling = 'auto')
                                  ),
                                  tabPanel("Climate Change",
                                            tags$iframe(src = './p2_ClimateChange.html', # put test.html to /www
                                                        width = '100%', height = '854px', 
                                                        frameborder = 0, scrolling = 'auto')
                                           ),
                                  tabPanel("HydroClimate",
                                           tags$iframe(src = './p2_HydroClimate.html', # put test.html to /www
                                                       width = '100%', height = '854px', 
                                                       frameborder = 0, scrolling = 'auto')
                                           ),
                                  tabPanel("Watershed Land Use",
                                           tags$iframe(src = './p2_WatershedLandUse.html', # put test.html to /www
                                                       width = '100%', height = '854px', 
                                                       frameborder = 0, scrolling = 'auto')
                                           ),
                                  tabPanel("Watershed Disturbance",
                                           tags$iframe(src = './p2_WatershedDisturbance.html', # put test.html to /www
                                                       width = '100%', height = '854px', 
                                                       frameborder = 0, scrolling = 'auto')
                                  ),
                                  tabPanel("Geology Geomorphology",
                                           tags$iframe(src = './p2_GeologyGeomorphology.html', # put test.html to /www
                                                       width = '100%', height = '854px', 
                                                       frameborder = 0, scrolling = 'auto')
                                  ),
                                  tabPanel("Ecological Values",
                                           tags$iframe(src = './p2_EcologicalValues.html', # put test.html to /www
                                                       width = '100%', height = '854px', 
                                                       frameborder = 0, scrolling = 'auto')
                                  ),
                                  tabPanel("Cultural Values",
                                           tags$iframe(src = './p2_CulturalValues.html', # put test.html to /www
                                                       width = '100%', height = '854px', 
                                                       frameborder = 0, scrolling = 'auto')
                                  )
                          ), # end of navlistPanel
                             
                              column(width = 12,
                                     style = "background-color:#003366; border-top:2px solid #fcba19;",
                                     tags$footer(class="footer",
                                                 tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                          tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                                  tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                          )
                                                 )
                                     )
                              )
                 ),  # end of tabPanel page 2
                 
                 tabPanel(title = "User Guide",
                          fluidPage(
                              tags$iframe(src = './UserGuide.html',         # put myMarkdown.html to ~/www/
                                          width = '100%', height = '854px', 
                                          frameborder = 0, scrolling = 'auto'
                              ),
                              column(width = 12,
                                     style = "background-color:#003366; border-top:2px solid #fcba19;",
                                     tags$footer(class="footer",
                                                 tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                          tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                                  tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                          )
                                                 )
                                     )
                              )
                          ) # end of fluidPage in tabPanel 3 UserGuide
                 ) # end tabPanel page3
                 
) # end navbarPage


# 3 Define server -------------------------------------------------------------
server <- function(input, output, session) {
    shinyjs::addClass(id = "menus", class = "navbar-right")        # make tab pages to right

    
## 3.1 set up map and table ---------------------------------------------------

    ### 3.1.1 map -------------------------------------------------------------
    output$map <- leaflet::renderLeaflet({
                    leaflet() %>% 
                    setView(lng = -122.5,
                            lat = 51.5,
                            zoom = 6) %>%
                    setMaxBounds(lng1 = -113, lat1 = 60.5,
                                 lng2 = -141, lat2 = 47.8) %>%  
                    addProviderTiles(providers$OpenTopoMap, group='Topographic') %>%
                    addTiles(group = 'Basic map') %>%
                    addGraticule(interval = 1, style = list(color = 'lightgrey', weight = 0.5)) %>%
                    addCircleMarkers(data = hydattable, lng = ~longitude, lat = ~latitude, layerId = ~station_number, 
                               clusterOptions = markerClusterOptions(reezeAtZoom = 10), 
                               label = ~ str_to_title(paste(station_number, station_name))) %>% 
                    addPolygons(data = Strmrdr_4, color = 'black', weight = 2, layerId = ~GNIS_ID, 
                                popup = ~GNIS_NAME, group = 'StreamWatershed_4', fillOpacity = 0) %>%
                    addLayersControl(overlayGroups = c('StreamWatershed_4',  'Topographic', 'Basic map'),
                                     options = layersControlOptions(collapsed = F)) %>%
                    addResetMapButton()
                                        })
    
    ### 3.1.2 output$watershedtabledf -----------------------------------------
    output$watershedtabledf <- DT::renderDataTable({
                                    datatable(Strmrdr4df %>% 
                                    select(-c(STRMRDR, AREA_HA, FTRCD, AREA_SQM, STRMMGNTD)) %>%
                                    rename('Watershedkey_namestream' = 'WTRSHDK'),
                                selection = 'single', rownames = TRUE, filter="top",
                                options = list(scrollY = '180px', paging = FALSE, scrollX=TRUE, dom="ltipr")
                            ) # end of datatable
                        })
    # Note: Strmrdr_4 is sf, Strmrdr4df is dataframe, watershedtabledf is output ID
    
    ### 3.1.3 output$hydattable -----------------------------------------------
    output$hydattable <- DT::renderDataTable({
                                datatable(hydattable %>% 
                                mutate(across(c(latitude, longitude), round, 3)) , 
                        selection = 'single', rownames = TRUE, filter="top",
                        options = list(scrollY = '180px', paging = FALSE, scrollX=TRUE, dom="ltipr")
        ) # end of hydattable
    })
    
    
## 3.2 Empty reactive values object -------------------------------------------
    reactive_objects = reactiveValues()             
    
## 3.3 Select map marker Event ------------------------------------------------
    # scenario 1: user click a map marker, then the hydat table filter to the row
    ### Map marker click (to identify selected site) --------------------------
         ## Note: click maker event only returns: click$id, click$lng, click$lat
    observe({
            req(hydattable)
            site_click <- input$map_marker_click       
            if (is.null(site_click))
                return()
            siteid = site_click$id       
            reactive_objects$station_number <- siteid  
    })

    ### Filter hydattable to match clicked map marker -------------------------
    hydattable_proxy = DT::dataTableProxy('hydattable')
    observeEvent(input$map_marker_click, {
                hydattable_proxy %>% 
                DT::clearSearch() %>% 
                DT::updateSearch(keywords = list(global = reactive_objects$station_number, columns=NULL))
    })
    
    # scenario 2: user click a row on the hydat table, then the map zoom to that hydat station 
    ### active on hydat table row click ---------------------------------------
    observe({
            req(input$hydattable_rows_selected)
            hydatrow_click <- input$hydattable_rows_selected
            siteid         <- hydattable[hydatrow_click, "station_number"]$station_number          
            reactive_objects$station_number <- siteid 
    })
    
    ### map zoom on hydat table click -----------------------------------------
    map_proxy=leaflet::leafletProxy("map")
    observeEvent(input$hydattable_rows_selected,{
        lat=hydattable[hydattable$station_number==reactive_objects$station_number,"latitude"]$latitude
        long=hydattable[hydattable$station_number==reactive_objects$station_number,"longitude"]$longitude
        map_proxy %>% leaflet::setView(lng=long, lat=lat, zoom=12)
    })
    
   
## 3.4 Select polygon Event ---------------------------------------------------     
 
    # scenario 1: user click a watershed polygon on the map, then the table filterd to that row    
    ### map polygon select ----------------------------------------------------
    observe({
            req(Strmrdr_4)
            poly4_click <- input$map_shape_click
            if (is.null(poly4_click)){return()}
            poly4id = poly4_click$id             
            reactive_objects$watershed_id <- poly4id   
    })
    
    ### filter the watershedtable to that polygon row -------------------------
    watershedtabledf_proxy = DT::dataTableProxy('watershedtabledf')
    observeEvent(input$map_shape_click, {
                    watershedtabledf_proxy %>% 
                    DT::clearSearch() %>% 
                    DT::updateSearch(keywords = list(global = reactive_objects$watershed_id , columns = NULL))
    })
    
    # scenario 2: user click a row on the watershed table, then the map zoom to that watershed 
    ### active on watershed table row click -----------------------------------
    observe({
            req(input$watershedtabledf_rows_selected)                     
            poly4_rowclick <- input$watershedtabledf_rows_selected          
            poly4id <- Strmrdr4df[poly4_rowclick, "GNIS_ID"]               
            reactive_objects$watershed_id <- poly4id      
            reactive_objects$watershed_table_row <- Strmrdr4df[poly4_rowclick, ]
    })

    ### Change map zoom on watershed table click ------------------------------
    map_proxy=leaflet::leafletProxy("map")
    observeEvent(input$watershedtabledf_rows_selected, {
        if(!is.null(input$watershedtabledf_rows_selected)){
            poly4table_click <- input$watershedtabledf_rows_selected 
            map <- leafletProxy("map")
            zoombox <- st_bbox(Strmrdr_4[poly4table_click,]) %>% as.vector()
            map %>% fitBounds(zoombox[1], zoombox[2], zoombox[3], zoombox[4])
        }
    })
    

## 3.5 Result: watershed report table -----------------------------------------
 
    observe({
        req(reactive_objects$watershed_id)
        poly4_click         <- reactive_objects$watershed_id
        watershed_table_row <- subset(Strmrdr4df[Strmrdr4df$GNIS_ID==poly4_click, ])
        watershed_poly_row  <- subset(Strmrdr_4[Strmrdr_4$GNIS_ID==poly4_click, ])[1,]
        
        df           <- watershed_table_row %>% 
                        select(GNIS_ID, GNIS_NAME,STRMRDR, AREA_HA) %>%
                        rename("BCGNIS ID"       = "GNIS_ID",
                                "Watershed Name" = "GNIS_NAME",
                                "Stream No"      = "STRMRDR",
                                "Area (ha)"      = "AREA_HA" )
        df_rangebox  <- sf::st_bbox(watershed_poly_row) %>% 
                        as.vector() %>% t() %>% as.data.frame() %>%
                        relocate(V1, V3, V2, V4) %>%
                        rename('Longitude Min'='V1', 'Latitude Min'='V2', 'Longitude Max'='V3', 'Latitude Max'='V4') 
        ele_raster   <- elevatr::get_elev_raster( watershed_poly_row, z = 12) %>% crop(watershed_poly_row)
        df_elevation <- data.frame("Elevation Max (m)" = raster::cellStats(ele_raster, 'max'),
                                    "Elevation Min (m)" = raster::cellStats(ele_raster, 'min'),
                                    "Elevation Mean (m)" = raster::cellStats(ele_raster, 'mean')) 
        colnames(df_elevation) <- c("Elevation Max (m)", "Elevation Min (m)", "Elevation Mean (m)")
        df <- cbind(df, df_rangebox, df_elevation) %>% t() %>% as.data.frame()
        colnames(df) <- ("Value")
        df$'Item' <- rownames(df)
        df <- df[, c('Item', 'Value')]
        row.names(df) <- NULL
        
        if (is.null(poly4_click)){return()}
            output$watershedreport <- DT::renderDataTable({
                datatable(df, selection = 'single', rownames = F, extensions = 'Buttons',
                          options = list(scrollY = '260px', paging = FALSE, dom='Bfrtip', pageLength = 11, 
                                         autoWidth = TRUE, buttons=c('copy', 'csv', 'excel', 'pdf'))
                         )
               }) # end of DT
   
    }) # end of report observe
    
    
## 3.6 Result: hydat station flow plot ----------------------------------------
    output$plot_flow_data <- renderPlotly({
        req(reactive_objects$station_number)
        ggplotly(plot_flow_data(station_number = reactive_objects$station_number,
                       start_date = "1900-01-01",
                       end_date = "2021-12-31",
                       include_title = TRUE)$Daily_Flows)
    })

} # end of server


# Run the application 
shinyApp(ui = ui, server = server)

