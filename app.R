rm(list = ls())

# library -----------------------------------------------------------------


library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(rgdal)
library(zip)
library(raster)
library(leaflet.extras)
library(exactextractr)
library(stringr)
library(htmltools)


# read_data ---------------------------------------------------------------


admin_zero <-st_read("01_input/04_admin_boundary_without_pop/lbn_admbnda_adm0_cdr_20200810.shp") |> rmapshaper::ms_simplify()
cluster_sf <- st_read("01_input/05_cluster/lbn_one_km_hex.shp") |> rmapshaper::ms_simplify() %>% 
  rename(zone_sts=drft_s_,
         admin1Name=admn1Nm,
         admin2Name=admn2Nm,
         admin3Name=admn3Nm
         # ,population=pop
  )



admin1_boundary <- st_read("01_input/03_admin_boundary_with_pop/admin1.shp") |> rmapshaper::ms_simplify()
admin2_boundary <- st_read("01_input/03_admin_boundary_with_pop/admin2.shp") |> rmapshaper::ms_simplify()
admin3_boundary <- st_read("01_input/03_admin_boundary_with_pop/admin3.shp") |> rmapshaper::ms_simplify()



# leaflet_map -------------------------------------------------------------

base_map <- leaflet::leaflet() %>% leaflet::addProviderTiles(providers$CartoDB.Positron) %>% 
  leaflet::addPolygons(data = admin_zero,color = "#EE5859",fillColor = "transparent") %>% 
  leaflet::addPolygons(data = admin1_boundary,color = "#58585A",
                       # label = ~htmlEscape(admin1Name),
                       # labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize = "15px"),
                       popup = paste("Governorate:", admin1_boundary$admin1Name, "<br>",
                                     "Total population (WorldPop):", admin1_boundary$pop_wp, "<br>",
                                     "Total population (Facebook/Meta):", admin1_boundary$pop_fb),
                       weight = 2,dashArray = "12",fillColor = "transparent")



customized_cluster <- cluster_sf %>% filter(zone_sts != "NOT_OK")



# read population raster --------------------------------------------------

worldpop <- raster("01_input/01_population_raster/01_world_pop/lbn_ppp_2020_UNadj_constrained.tif") # %>% projectRaster(crs = crs(admin_zero))
facebook <- raster("01_input/01_population_raster/02_facebook/lbn_general_2020.tif")
jrc <- raster("01_input/01_population_raster/03_jrc/JRC_population_mask.tif")

base_map_for_pop <- leaflet::leaflet() %>% 
  leaflet::addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  leaflet::addPolygons(data = admin_zero,color = "#EE5859",fillColor = "transparent") %>%  
  # leaflet::addRasterImage(worldpop) %>% 
  leaflet::addPolygons(data = admin1_boundary,color = "#D2CBB8",
                       label = ~htmlEscape(admin1Name),
                       labelOptions = labelOptions(noHide = T,
                                                   direction = 'center',
                                                   textOnly = T,
                                                   style = list(
                                                     "font-family" = "serif",
                                                     "font-size" = "15px",
                                                     "font-weight" =  "bold"
                                                   )),
                       popup = paste("Governorate:", admin1_boundary$admin1Name, "<br>",
                                     "Total population (WorldPop):", admin1_boundary$pop_wp, "<br>",
                                     "Total population (Facebook/Meta):", admin1_boundary$pop_fb, "<br>",
                                     "Total population (JRC):", admin1_boundary$pop_jrc),
                       weight = 3,fillColor = "transparent") %>% 
  
  leaflet::addPolygons(data = admin2_boundary,
                       color = "#58585A",
                       label = ~htmlEscape(admin2Name),
                       labelOptions = labelOptions(noHide = T,
                                                   direction = 'center',
                                                   textOnly = T,
                                                   style = list(
                                                     "font-family" = "serif",
                                                     "font-size" = "12px"
                                                   )),
                       
                       popup = paste("Governorate:", admin2_boundary$admin1Name, "<br>",
                                     "District:", admin2_boundary$admin2Name, "<br>",
                                     "Total population (WorldPop):", admin2_boundary$pop_wp, "<br>",
                                     "Total population (Facebook/Meta):", admin2_boundary$pop_fb,"<br>",
                                     "Total population (JRC):", admin2_boundary$pop_jrc
                                     ),
                       
                       weight = 1,fillColor = "transparent",group = "District") %>% 
  
  leaflet::addPolygons(data = admin3_boundary,
                       color = "#F69E61",
                       label = ~htmlEscape(admin3Name),
                       labelOptions = labelOptions(noHide = T,
                                                   direction = 'center',
                                                   textOnly = T,
                                                   style = list(
                                                     "font-family" = "Arial Narrow",
                                                     "font-size" = "10px",
                                                     "font-style" = "italic"
                                                   )),
                       popup = paste("Governorate:", admin3_boundary$admin1Name, "<br>",
                                     "District:", admin3_boundary$admin2Name, "<br>",
                                     "Sub-District:", admin3_boundary$admin3Name, "<br>",
                                     "Total population (WorldPop):", admin3_boundary$pop_wp, "<br>",
                                     "Total population (Facebook/Meta):", admin3_boundary$pop_fb, "<br>",
                                     "Total population (JRC):", admin3_boundary$pop_jrc),
                       
                       weight = 1,dashArray = "9",
                       fillColor = "transparent",
                       group = "Sub-district") %>% 
  
  addLayersControl(
    overlayGroups = c("District", "Sub-district"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  # hideGroup(c("Sub-district")) %>% 
  groupOptions("District", zoomLevels = 10:12) %>% 
  groupOptions("Sub-district", zoomLevels = 12:20) %>% 
  
  addDrawToolbar(position = "topleft",
                 polylineOptions = F,
                 polygonOptions = T,
                 rectangleOptions = T,
                 markerOptions = F,circleOptions = F,
                 circleMarkerOptions = F,singleFeature = T
  ) %>% setView(lat = 33.8736,lng = 35.8637,zoom = 8) 



# ui ---------------------------------------------------------------------

ui <- 
  fluidPage(
    
    # Styling -----------------------------------------------------------------
    
    tags$head(
      HTML('<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />'), includeCSS("style.css")
    ),
    #   
    
    navbarPage(
      
      windowTitle = "LEBANON POPULATION DISTRIBUTION",
      HTML('<a style="padding-left:10px;" class = "navbar-brand" href = "https://www.reach-initiative.org" target="_blank"><img src = "reach_logo.png" height = "50"></a><span class="navbar-text" style="font-size: 16px; color: #FFFFFF"><strong>LEBANON POPULATION DISTRIBUTION</strong></span>'),

      
      tabPanel("Populated Area",

               
               column( width =3,
                       br(),
                       h4(strong("Background:"),tags$br(),
                          p(style="text-align: justify;","This app has been created to smoothen the host community sampling process all over the Lebanon. WorldPop, Facebook/Meta, and Joint Research Centre (JRC) data have been incorporated into the dashboard to compare the population figures from different sources. This will allow the user to choose the most appropriate dataset for a given area."
                          )),
                       
                       hr(),
                       
                       #########test ##########
                       # dropdown(
                       #   img(src = "Slide1.PNG", height = 720, width = 1280),
                       #   tooltip = tooltipOptions(title = "Click for more details on the app background"),
                       #   size = "xs",
                       #   up = F,right = F,
                       #   style = "jelly", label =  h4(style = "color: black;",strong(paste0("Methodology:"))),
                       #   animate = animateOptions(
                       #     enter = "fadeInDown",
                       #     exit  = "fadeOutUp",
                       #     duration = 0.5)
                       # ),
                       #################
                       
                       
                       
                        h4(strong("Methodology:"),tags$br(),
                          p(style="text-align: justify;", "Initially, whole of Lebanon was divided into hexagons, where each hexagon covers an area of one square kilometer. Non-livable areas such as government buildings, airports, schools, colleges, waterbodies, etc.. were then erased from the hexagons. OSM data was used to identify non-livable areas. Finally, these hexagons were used to calculate the population figures from the population rasters. The detailed script is available on request."
                          )
                       ),
                       
                       ##################
                       absolutePanel(id = "dropdown", top = 130, left = 440, width = 200, fixed=F, draggable = T, height = "auto",
                                     dropdown(
                                       img(src = "Slide1.PNG", height = 720, width = 1280),
                                       tooltip = tooltipOptions(title = "Click for more details on the app background"),
                                       size = "xs",
                                       up = F,right = F,
                                       style = "jelly", icon = icon("info"),
                                       animate = animateOptions(
                                         enter = "fadeInDown",
                                         exit  = "fadeOutUp",
                                         duration = 0.5)
                                     ),
                                     style = "opacity: 1; z-index: 10;"
                       ),
                       
                       ####################
                       
                       
                       hr(),

                       h4(strong("Uses:"),#tags$br(),
                          p("Although this app was initially focused on finding the populated area to make sample points for host community assessments but this app can-")), 
                       # br(),
                       
                       p(tags$ol(
                         tags$li(em("Calculate population by admin levels")), 
                         tags$li(em("Calculate population from customized shapefile")), 
                         tags$li(em("Explore population distribution over Lebanon"))
                       )),
                       
                       
                       # tags$br(),
                       
                       p("Note that the app data should be compared with other population data sources before calculating the sample size. The app data is based on remote sensing technology and might not be error-free. However, the app data can be used to prepare the sample points (kmls). The uses of this app are listed below:"
                         ),
                       
                       # tags$br(),
                       
                       p(tags$ol(
                         tags$li(em("Finding out the populated area.")), 
                         tags$li(em("Preparing the sample points.")), 
                         tags$li(em("Comparing the worldPop/Facebook/JRC population data with other sources.")),
                         tags$li(em("In case of the unavailability of other valid population data sources, the app data can be used in any assessment stage."))
                       )),
                       
                       
                       hr(),
                       
                       h4(strong(" Data Source:"),tags$br(),
                          p("Admin boundaries:", em(tags$a(href="https://data.humdata.org/dataset/Lebanon-admin-level-1-boundaries", "OCHA")),tags$br(),
                            "Population figures:", 
                             tags$ul(
                              tags$li(em(tags$a(href="https://hub.worldpop.org/geodata/summary?id=50007", "WorldPop,2020"))),
                              tags$li(em(tags$a(href="https://data.humdata.org/dataset/lebanon-high-resolution-population-density-maps-demographic-estimates?s", "Facebook/Meta,2020"))),
                              tags$li(em(tags$a(href="https://ghsl.jrc.ec.europa.eu/download.php?ds=pop", "JRC,2020")))
                              )
                       )),
                       
                       hr(),
                       # h4(strong("App Background:"),tags$br(),
                       # img(src = "Slide1.PNG", height = 300, width = 450)
                       # ),
                       # 
                       # hr(),
                       h4(strong("Contact:"),tags$br(),
                          p("Md. Mehedi Hasan Khan",br(),
                            "Data Specialist",br(),
                            "Email:", tags$a(href="mailto:mehedi.khan@impact-initiatives.org","mehedi.khan@impact-initiatives.org")),
                          p("Sally SASINE",br(),
                            "GIS Specialist",br(),
                            "Email:", tags$a(href="mailto:sally.sasine@reach-initiative.org","sally.sasine@reach-initiative.org"))
                       )
                       
               ),
               
               mainPanel(width = 9,
                 br(),
                 h5(strong("This tab will give you the population data by each sub-district. Here in this tab, you have five parameters-")),
                 tags$ol(p(
                   tags$li(strong("Select governorate:"), em("Here, you should select governorate(s) of interest")), 
                   tags$li(strong("Select district:"), em("Once you select the governorate(s), 
                                    you will have the district name of the selected governorate(s). 
                                    From the list you should choose the district(s) of interest")), 
                   tags$li(strong("Select sub-district:"), em("Once you select the district(s), you will have the district's sub-district(s) name. 
                                                               From the list, you should choose the sub-district(s) of interest")), 
                   tags$li(strong("Minimum allowable density:"), em("This parameter can help you to control the cluster. 
                              For example, If you do not want to consider the hexagon (cluster) with a population density below a specific number,
                              then you can just put the number in this parameter and the app will remove all the clusters with a population density 
                              below the number. The default is 667, which is Lebanon's population density. This means initially (unless you change anything),
                              all the clusters with a population below 667 will be removed. 
                              This parameter is necessary to define the populated areas.")),
                   
                   tags$li(strong("Select data source:"), em("Here, you should select the population dataset that you will use to find the populated area. Note that it will not affect the subdistrict-wise population data where 
                                                             you will receive the population figures for all available sources. However, it will affect in delineating the clusters. For example, 
                                                             if you select WorldPop, then the filter for minimum allowable density will be applied to the WorldPop population and if 
                                                             you choose Facebook/Meta, then the filter will be applied to the population figure which comes from Facabook/Meta."))
                 )),
                 
                 
                 hr(),
                 
                 ##########################
                 tags$div(pickerInput("select_governarate",
                                      label = "Select governorate:",
                                      choices = admin1_boundary$admin1Name %>% unique() %>% dput() |> sort(),
                                      selected = admin1_boundary$admin1Name %>% unique() %>% dput()|> sort(),
                                      multiple = T,
                                      options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                 ),style="display:inline-block"),
                 
                 tags$div(pickerInput("select_district",
                                      label = "Select district:",
                                      choices = NULL,
                                      selected = NULL,
                                      multiple = T,
                                      options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                 ),style="display:inline-block"),
                 
                 tags$div(pickerInput("select_sub_district",
                                      label = "Select sub district:",
                                      choices = NULL,
                                      selected = NULL,
                                      multiple = T,
                                      options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                 ),style="display:inline-block"),
                 
                 
                 tags$div(numericInput("minimum_allowable_density",
                                       label = "Minimum allowable density",
                                       value = 50,
                                       min = 0,
                                       max = 200,
                                       step = NA,
                                       width = NULL
                 ),style="display:inline-block"),
                 
                 
                 tags$div(pickerInput("select_data_source",
                                      label = "Select data source:",
                                      choices = c("WorldPop", "Facebook","JRC"),
                                      selected = "WorldPop",
                                      multiple = F,
                                      options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                 ),style="display:inline-block"),
                 
                 

                 
                 hr(),
                 
                 
                 actionButton("run", "Show result"), 
                 downloadButton("download_summary", label = "Download sub district population (as csv)", class = NULL),
                 downloadButton("download_csv", label = "Download cluster data (as csv)", class = NULL),
                 downloadButton("download_shp", label = "Download cluster data (as shapefile)", class = NULL),hr(),
                 
                 DTOutput("summary_table"),hr(),
                 
                 h5(em(strong("Clusters Location:"))), 
                 leafletOutput("map")
                 
               )
      ),
      
      
      
      
      tabPanel("Calculate Population from Input Shapefile",
               
               mainPanel(br(),width = 12,
                         
                         h5(strong("This tab is for calculating the population by an input shapefile.")),
                         h6(strong("Note:")),
                         tags$ol(
                           tags$li(em("The coordinate system of input shapefile must be WGS84.")),
                           tags$li(em("There should be no column named as",strong("pop"), "and",strong("pop_source"), "in the input shapefile."))),
                         
                         hr(),
                         fluidRow(
                           column(2, (pickerInput("select_data_source2",
                                                  label = "Select data source:",
                                                  choices = c("WorldPop", "Facebook","JRC"),
                                                  selected = "WorldPop",
                                                  multiple = F,
                                                  options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                           ))),
                           
                           column(2,(fileInput(inputId = "shp",
                                               label = "Upload shapefile",
                                               multiple = TRUE,
                                               accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj'))))),
                         
                         hr(),
                         
                         tags$div(actionButton("run_for_pop", "Calculate Population"), style="display:inline-block"),
                         downloadButton("download_table_cus_shp", label = "Download data with population (csv)", class = NULL),
                         downloadButton("download_cus_shp", label = "Download data with population(shapefile)", class = NULL),
                         
                         hr(),
                         
                         DTOutput("summary_table_cus_shp"), br(),
                         
                         h4(em(strong("Map::Input location"))),hr(),
                         
                         leafletOutput("cus_map"),
                         # div(class = "outer", tags$style(type = "text/css", ".outer {position: absolute;}"),
                         #     leafletOutput("cus_map", width = "100%", height = "100%"))
                         
                         
               )# end mainPanel 
               
      ), # END TAB 2
      
      tabPanel("Explore Population!",
               
               
               
               mainPanel( width = 12,
                          
                          
                          br(),
                          
                          h5(strong("This tab can calculate the population by user-defined polygon, which can be drawn using the top left button.")),
                          
                          hr(),
                          tags$div(pickerInput("select_data_source3",
                                               label = "Select data source:",
                                               choices = c("WorldPop", "Facebook","JRC"),
                                               selected = "WorldPop",
                                               multiple = F,
                                               options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                          ),style="display:inline-block"),
                          hr(),
                          tags$div(actionButton("a", "Calculate population"), style="display:inline-block"),
                          downloadButton("download_shp2", label = "Download data with population(shapefile)", class = NULL),
                          
                          br(),
                          h4(textOutput("population")), 
                          br(),br(),
                          div(class = "outer", tags$style(type = "text/css", ".outer {position: fixed; top: 270px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                              leafletOutput("base_map_for_pop",width = "100%", height = "100%"))
                          
                          # leafletOutput("P_MAP"),
               )
      ) # END TAB 3
      
      
    )  ########## navarpage
    
    
  ) ## fludpage


# server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output,session){
  

  ################### Filter governorate ###############################################3
  minimum_allowable_density_for_cluster <-  reactive({input$minimum_allowable_density})
  
  governorate_names <- reactive({input$select_governarate})
  
  customized_cluster_filter <- reactive({customized_cluster %>% 
      filter(admin1Name %in% governorate_names())})
  
  ####################### available district name in the selected governorate ############
  
  available_district <- reactive({customized_cluster_filter()$admin2Name %>% unique()|> sort()})
  
  observe({
    updatePickerInput(session, "select_district", choices = available_district())
  })
  
  
  ########################################## filter district #####################################
  
  district_names <- reactive({input$select_district})
  customized_cluster_filter_district <- reactive({customized_cluster_filter() %>% 
      filter(admin2Name %in% district_names())})
  
  
  ############################### available_sub_district ##############################
  available_subdistrict <- reactive({customized_cluster_filter_district()$admin3Name %>% unique()|> sort()})
  
  observe({
    updatePickerInput(session, "select_sub_district", choices = available_subdistrict())
  })
  
  
  ############################### Filter sub district #######################################
  
  subdistrict_names <- reactive({input$select_sub_district})
  customized_cluster_final_filter_2 <- reactive({customized_cluster_filter_district() %>% 
      filter(admin3Name %in% subdistrict_names())})
  
  
  ###################################### extent ############################################
  population <- eventReactive(input$run,{ case_when(input$select_data_source == "WorldPop"~"pop_wp",
                                                  input$select_data_source == "Facebook"~"pop_fb",
                                                  T~"pop_jrc")})
  
  customized_cluster_final_filter  <- eventReactive(input$run,{customized_cluster_final_filter_2() %>% 
      filter(!!sym(population())/as.numeric(area_km) >=minimum_allowable_density_for_cluster())})
  st_cord <- eventReactive(input$run,{customized_cluster_final_filter() %>% st_coordinates() %>% as.data.frame()})
  
  ############################################ MAP #########################################
  
  output$map <-  renderLeaflet({
    
    base_map %>%  leaflet::addPolygons(data = customized_cluster_final_filter(),
                                       fillOpacity = 0.5,
                                       smoothFactor = 0.5,
                                       color = ~colorQuantile("YlOrRd", customized_cluster_final_filter()[[population()]])(customized_cluster_final_filter()[[population()]]),
                                       stroke = F,
                                       popup = paste("Row_id:", customized_cluster_final_filter()$row_id, "<br>",
                                                     "Governorate:", customized_cluster_final_filter()$admin1Name, "<br>",
                                                     "District:", customized_cluster_final_filter()$admin2Name, "<br>",
                                                     "Sub district:", customized_cluster_final_filter()$admin3Name, "<br>",
                                                     "Cluster Character for sampling:", customized_cluster_final_filter()$zone_sts, "<br>",
                                                     "population(WorldPop):", customized_cluster_final_filter()$pop_wp,"<br>",
                                                     "population(Facebook/Meta):", customized_cluster_final_filter()$pop_fb,"<br>",
                                                     "population(JRC):", customized_cluster_final_filter()$pop_jrc,"<br>",
                                                     "Area (KM):", customized_cluster_final_filter()$area_km)
                                       
    )  %>% fitBounds(lng1 = min(st_cord()$X), 
                     lat1 = min(st_cord()$Y), 
                     lng2 = max(st_cord()$X), 
                     lat2 = max(st_cord()$Y))
    
  })
  
  
  ############################################################## Tab 2 map ########################### 
  output$base_map_for_pop <- renderLeaflet({base_map_for_pop})  
  
  
  polygn <- eventReactive(input$a,{
    feat <- input$base_map_for_pop_draw_new_feature
    coords <- unlist(feat$geometry$coordinates)
    coords <- matrix(coords, ncol = 2, byrow = T)
    polygn <- st_sf(st_sfc(st_polygon(list(coords))), crs = st_crs(4326))
    
  })
  
  
  
  ###################################### Tab3: Explore population ####################################################
  
  
  raster_source<- eventReactive(input$a,{ case_when(input$select_data_source3 == "WorldPop"~"worldpop",
                                                    input$select_data_source3 == "Facebook"~"facebook",
                                                    T ~ "jrc")})
  
  clip_pop_raster <-   eventReactive(input$a,{
    
    exact_extract(get(raster_source()), polygn(), function(values,coverage_fractions)
      round(sum(values*coverage_fractions,na.rm = T)))
    
  })
  
  pop_text <- eventReactive(input$a,{paste0("Population within selected area:",clip_pop_raster())})
  
  
  
  output$population <- renderText({
    pop_text()
  })
  
  ################### add new ##################
  
  polygn2 <- eventReactive(input$a,{
    polygn() %>% dplyr::mutate(
      popu = clip_pop_raster(),
      popu_src = raster_source()
    )})
  
  getGeoContent3 <- eventReactive(input$a,{polygn2() %>% as_Spatial()})
  
  output$download_shp2 <- downloadHandler(
    filename = 'population_polygon.zip',
    content = function(file) {
      if (length(Sys.glob("population_polygon.*"))>0){
        file.remove(Sys.glob("population_polygon.*"))
      }
      writeOGR(getGeoContent3(), dsn="population_polygon.shp", layer="population_polygon", driver="ESRI Shapefile")
      zip(zipfile='population_polygon.zip', files=Sys.glob("population_polygon.*"))
      file.copy("population_polygon.zip", file)
      if (length(Sys.glob("population_polygon.*"))>0){
        file.remove(Sys.glob("population_polygon.*"))
      }
    }
  )
  
  ###################end new ###################
  
  ######################################## Download csv and shapefile ##########################################
  
  cluster_data <- reactive({customized_cluster_final_filter() %>% as.data.frame() %>% dplyr::select(-geometry)})
  
  getGeoContent <- reactive({customized_cluster_final_filter() %>% as_Spatial()})
  
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste('Cluster_data_', Sys.Date(), '.csv',sep = '')
    },
    content = function(con) {
      write.csv(cluster_data(), con)
    }
  )
  
  output$download_shp <- downloadHandler(
    filename = 'cluster.zip',
    content = function(file) {
      if (length(Sys.glob("cluster.*"))>0){
        file.remove(Sys.glob("cluster.*"))
      }
      writeOGR(getGeoContent(), dsn="cluster.shp", layer="cluster", driver="ESRI Shapefile")
      zip(zipfile='cluster.zip', files=Sys.glob("cluster.*"))
      file.copy("cluster.zip", file)
      if (length(Sys.glob("cluster.*"))>0){
        file.remove(Sys.glob("cluster.*"))
      }
    }
  )
  
  ################################################# Pivot table ########################################
  
  admin_3_name <- eventReactive(input$run,{cluster_data()$admin3Name %>% unique()})
  
  
  
  df1 <- eventReactive(input$run,{
    admin3_boundary %>% as.data.frame() %>% dplyr::filter(admin2Name %in% district_names()) |> 
      dplyr::filter(admin3Name %in% admin_3_name()) %>% 
      dplyr::select(admin1Name,admin2Name,admin3Name,pop_wp,pop_fb,pop_jrc) |> group_by(admin1Name,admin2Name,admin3Name) |> 
      summarise(pop_wp = sum(pop_wp,na.rm = T),
                pop_fb = sum(pop_fb,na.rm = T),
                pop_jrc = sum(pop_jrc,na.rm = T)) |> ungroup()
  })
  df2 <- eventReactive(input$run,{
    cluster_data() %>% group_by(admin1Name,admin2Name,admin3Name) %>% summarise(
      pop_in_cluster_wp = sum(pop_wp,na.rm = T),
      pop_in_cluster_fb = sum(pop_fb,na.rm = T),
      pop_in_cluster_jrc = sum(pop_jrc,na.rm = T)
      
    )  |> ungroup()
  })
  
  pv_tb <- eventReactive(input$run,{df1() %>% left_join(df2())})
  
  output$summary_table <- renderDT({
    datatable(pv_tb(),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center;',
                'Table 1: ', htmltools::em('Population by sub district')
                
              ))
    
    
  })
  
  ######################### download summary table ####################
  output$download_summary <- downloadHandler(
    filename = function() {
      paste('population_by_sub_district_', Sys.Date(), '.csv',sep = '')
    },
    content = function(con) {
      write.csv(pv_tb(), con)
    }
  )
  
  
  ###################################### upload customized shapefile ################################
  
  
  
  # Read-in shapefile function
  Read_Shapefile <- function(shp_path) {
    infiles <- shp_path$datapath # get the location of files
    dir <- unique(dirname(infiles)) # get the directory
    outfiles <- file.path(dir, shp_path$name) # create new path name
    name <- strsplit(shp_path$name[1], "\\.")[[1]][1] # strip name 
    purrr::walk2(infiles, outfiles, ~file.rename(.x, .y)) # rename files
    x <- st_read(file.path(dir, paste0(name, ".shp"))) # read-in shapefile
    return(x)
  }
  
  # Read-shapefile once user submits files
  user_shp <-  eventReactive(input$run_for_pop, {
    user_shp <- Read_Shapefile(input$shp)
  })  
  
  
  
  
  
  
  output$cus_map <-  renderLeaflet({
    
    base_map %>% leaflet::addPolygons(data = user_shp(),
                                      fillOpacity = 0.5,
                                      smoothFactor = 0.5)
    
  })
  
  ################################# start::calculate population (tab2: Calculate population from input shapefile) ##############################################
  
  
  clip_pop_raster_cus_shp <-   eventReactive(input$run_for_pop,{
    
    user_shp_2 <- user_shp() %>% mutate(
      row_id = row.names(user_shp())
    )
    
    
    raster_source2 <- eventReactive(input$run_for_pop,{ case_when(input$select_data_source2 == "WorldPop"~"worldpop",
                                                                  input$select_data_source2 == "Facebook" ~"facebook",
                                                                  T~"jrc")})
    
    
    pop_cus_shp<- exact_extract(get(raster_source2()), user_shp_2, function(values,coverage_fractions)
      sum(values*coverage_fractions,na.rm = T))
    
    
    pop_cus_shp2 <- data.frame(row_id = row.names(user_shp_2),
                               pop= pop_cus_shp%>% as.integer(),
                               pop_source = raster_source2())
    
    
    
    
    
    
    # Zone_with_data$row_id <- paste0("ID",Zone_with_data$row_id)
    clip_pop_raster_cus_shp<-user_shp_2 %>% left_join(pop_cus_shp2) %>% dplyr::select(row_id,pop,pop_source,everything())
    
  })
  
  
  output$summary_table_cus_shp <- renderDT({
    datatable(clip_pop_raster_cus_shp(),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center;',
                'Table 1: ', htmltools::em('Population by customized shape')
                
              ))
    
  })
  
  ################################# End::calculate population ################################################
  
  
  data_cus_shp_df <- eventReactive(input$run_for_pop,{
    
    data_cus_shp_df <- clip_pop_raster_cus_shp() %>% as.data.frame() %>% dplyr::select(-"geometry")
    
  })
  
  ############################################# write #######################################################
  
  output$download_table_cus_shp <- downloadHandler(
    filename = function() {
      paste('population_by_input_area', str_replace_all(Sys.Date(),"-","_"),".csv",sep = "")
    },
    content = function(con) {
      write.csv(data_cus_shp_df(), con)
    }
  )
  
  
  getGeoContent2 <- reactive({clip_pop_raster_cus_shp() %>% as_Spatial()})
  
  
  
  output$download_cus_shp <- downloadHandler(
    filename = 'input_with_pop.zip',
    content = function(file) {
      if (length(Sys.glob("input_with_pop.*"))>0){
        file.remove(Sys.glob("input_with_pop.*"))
      }
      writeOGR(getGeoContent2(), dsn="input_with_pop.shp", layer="input_with_pop", driver="ESRI Shapefile")
      zip(zipfile='input_with_pop', files=Sys.glob("input_with_pop.*"))
      file.copy("input_with_pop", file)
      if (length(Sys.glob("input_with_popr.*"))>0){
        file.remove(Sys.glob("input_with_popr.*"))
      }
    }
  )
  
  
  
  
  #####################################################################################################
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
} ### end server




# Run the application 
shinyApp(ui = ui, server = server)
