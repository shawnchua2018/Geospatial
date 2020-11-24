
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)

packages = c('shiny', 'sp', 'rgdal', 'rgeos', 'sf', 'tidyverse', 'olsrr', 'corrplot', 'ggpubr', 'sf', 'spdep', 'GWmodel', 'tmap', 'tidyverse', 'raster','plotly')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

# Importing Datasets
resale_flat = st_read("data/aspatial/Resale_flats_compiled.csv")

resale_flat$latitude <- as.numeric(resale_flat$latitude)
resale_flat$longitude <- as.numeric(resale_flat$longitude)

cord.dec = SpatialPoints(cbind(resale_flat$longitude, resale_flat$latitude), proj4string=CRS("+proj=longlat"))

cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:3414"))
cord.UTM

new_coords <- data.frame(cord.UTM@coords)
summary(new_coords)
new_coords <- new_coords %>%
  rename(X = coords.x1, Y = coords.x2)

resale_flat <- cbind(resale_flat, "X" = new_coords[1], "Y" = new_coords[2])

resale_flat = resale_flat[,!(names(resale_flat) %in% c("flat_model","lease_commence_date","longitude","latitude"))]

sf_resale_flat <- st_as_sf(resale_flat, coords = c("X","Y"),crs= 3414)

#Pri school
pschool = st_read("data/aspatial/Primary_school.csv")

pschool$latitude <- as.numeric(pschool$latitude)
pschool$longitude <- as.numeric(pschool$longitude)

cord.dec.psch = SpatialPoints(cbind(pschool$longitude, pschool$latitude), proj4string=CRS("+proj=longlat"))

cord.UTM.psch <- spTransform(cord.dec.psch, CRS("+init=epsg:3414"))
cord.UTM.psch

new_coords_psch <- data.frame(cord.UTM.psch@coords)
summary(new_coords_psch)
new_coords_psch <- new_coords_psch %>%
  rename(X = coords.x1, Y = coords.x2)

pschool <- cbind(pschool, "X" = new_coords_psch[1], "Y" = new_coords_psch[2])

sf_pschool <- st_as_sf(pschool, coords = c("X","Y"),crs= 3414)
sf_pschool <- st_transform(sf_pschool, 3414)

#sec school
sschool = st_read("data/aspatial/Secondary_school.csv")

sschool$latitude <- as.numeric(sschool$latitude)
sschool$longitude <- as.numeric(sschool$longitude)

cord.dec.ssch = SpatialPoints(cbind(sschool$longitude, sschool$latitude), proj4string=CRS("+proj=longlat"))

cord.UTM.ssch <- spTransform(cord.dec.ssch, CRS("+init=epsg:3414"))
cord.UTM.ssch

new_coords_ssch <- data.frame(cord.UTM.ssch@coords)
summary(new_coords_ssch)
new_coords_ssch <- new_coords_ssch %>%
  rename(X = coords.x1, Y = coords.x2)

sschool <- cbind(sschool, "X" = new_coords_ssch[1], "Y" = new_coords_ssch[2])

sf_sschool <- st_as_sf(sschool, coords = c("X","Y"),crs= 3414)
sf_sschool <- st_transform(sf_sschool, 3414)

 

mall = st_read("data/aspatial/Shopping_Malls.csv")

mall$latitude <- as.numeric(mall$latitude)
mall$longitude <- as.numeric(mall$longitude)

cord.dec.mall = SpatialPoints(cbind(mall$longitude, mall$latitude), proj4string=CRS("+proj=longlat"))

cord.UTM.mall <- spTransform(cord.dec.mall, CRS("+init=epsg:3414"))
cord.UTM.mall

new_coords_mall <- data.frame(cord.UTM.mall@coords)
summary(new_coords_mall)
new_coords_mall <- new_coords_mall %>%
  rename(X = coords.x1, Y = coords.x2)

mall <- cbind(mall, "X" = new_coords_mall[1], "Y" = new_coords_mall[2])

sf_mall <- st_as_sf(mall, coords = c("X","Y"),crs= 3414)
sf_mall <- st_transform(sf_mall, 3414)
st_crs(sf_mall)


preschool = st_read("data/geospatial/pre-schools-location-kml.kml")

hawkercenter = st_read("data/geospatial/hawker-centres-kml.kml")

cc = st_read("data/geospatial/community-clubs-kml.kml")



supermarket = st_read("data/geospatial/supermarkets-kml.kml")

sport_facilities = st_read("data/geospatial/sportsg-sport-facilities-kml.kml")
sport_facilities = st_centroid(sport_facilities)

sf_mpsz2019 = st_read("data/geospatial/master-plan-2019-subzone-boundary-no-sea-kml.kml")




cc <- cc %>%
  st_cast("MULTIPOINT") %>%
  st_cast("POINT")

cc_coords <- data.frame(st_coordinates(cc))

cord.dec.cc = SpatialPoints(cbind(cc_coords$X, cc_coords$Y), proj4string=CRS("+proj=longlat"))

cord.UTM.cc <- spTransform(cord.dec.cc, CRS("+init=epsg:3414"))
new_cc <- data.frame(cord.UTM.cc)

new_cc <- new_cc %>%
  rename(X = coords.x1, Y = coords.x2)



preschool <- preschool %>%
  st_cast("MULTIPOINT") %>%
  st_cast("POINT")

preschool_coords <- data.frame(st_coordinates(preschool))

cord.dec.preschool = SpatialPoints(cbind(preschool_coords$X, preschool_coords$Y), proj4string=CRS("+proj=longlat"))

cord.UTM.preschool <- spTransform(cord.dec.preschool, CRS("+init=epsg:3414"))
new_preschool <- data.frame(cord.UTM.preschool)

new_preschool <- new_preschool %>%
  rename(X = coords.x1, Y = coords.x2)

sport_facilities <- sport_facilities %>%
  st_cast("MULTIPOINT") %>%
  st_cast("POINT")

sport_facilities_coords <- data.frame(st_coordinates(sport_facilities))

cord.dec.sport_facilities = SpatialPoints(cbind(sport_facilities_coords$X, sport_facilities_coords$Y), proj4string=CRS("+proj=longlat"))

cord.UTM.sport_facilities <- spTransform(cord.dec.sport_facilities, CRS("+init=epsg:3414"))
new_sport_facilities <- data.frame(cord.UTM.sport_facilities)

new_sport_facilities <- new_sport_facilities %>%
  rename(X = coords.x1, Y = coords.x2)

supermarket <- supermarket %>%
  st_cast("MULTIPOINT") %>%
  st_cast("POINT")

supermarket_coords <- data.frame(st_coordinates(supermarket))

cord.dec.supermarket = SpatialPoints(cbind(supermarket_coords$X, supermarket_coords$Y), proj4string=CRS("+proj=longlat"))

cord.UTM.supermarket <- spTransform(cord.dec.supermarket, CRS("+init=epsg:3414"))
new_supermarket <- data.frame(cord.UTM.supermarket)

new_supermarket <- new_supermarket %>%
  rename(X = coords.x1, Y = coords.x2)

hawkercenter <- hawkercenter %>%
  st_cast("MULTIPOINT") %>%
  st_cast("POINT")

hawkercenter_coords <- data.frame(st_coordinates(hawkercenter))

cord.dec.hawkercenter = SpatialPoints(cbind(hawkercenter_coords$X, hawkercenter_coords$Y), proj4string=CRS("+proj=longlat"))

cord.UTM.hawkercenter <- spTransform(cord.dec.hawkercenter, CRS("+init=epsg:3414"))
new_hawkercenter <- data.frame(cord.UTM.hawkercenter)

new_hawkercenter <- new_hawkercenter %>%
  rename(X = coords.x1, Y = coords.x2)


mrt = st_read("data/aspatial/MRT_new.csv")
sf_mrt <- st_as_sf(mrt, coords = c("coords.x1", "coords.x2"), crs=3414)
sf_mrt <- st_transform(sf_mrt,3414)




# Converting all to SF and changing CRS from WGS84 to SVY21


sf_hawkercenter <- st_as_sf(new_hawkercenter, coords = c("X","Y"),crs= 3414)
sf_hawkercenter<- st_transform(sf_hawkercenter, 3414)
st_crs(sf_hawkercenter)



sf_preschool <- st_as_sf(new_preschool, coords = c("X","Y"),crs= 3414)
sf_preschool <- st_transform(sf_preschool, 3414)
st_crs(sf_preschool)



sf_sport_facilities <- st_as_sf(new_sport_facilities, coords = c("X","Y"),crs= 3414)
sf_sport_facilities<- st_transform(sf_sport_facilities, 3414)
st_crs(sf_sport_facilities)



sf_cc <- st_as_sf(new_cc, coords = c("X","Y"),crs= 3414)
sf_cc <- st_transform(sf_cc, 3414)
st_crs(sf_cc)



sf_supermarket <- st_as_sf(new_supermarket, coords = c("X","Y"),crs= 3414)
sf_supermarket <- st_transform(sf_supermarket, 3414)
st_crs(sf_supermarket)





header <- dashboardHeader(title = "$patial")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Overview", 
                 tabName = "overview", icon= icon("building")), 
        menuItem("Feature Settings", 
                 tabName = "settings", icon = icon("sliders-h")),
        menuItem("Transform", 
                 tabName = "transform", icon= icon("chart-bar")),
        menuItem("GWR Modelling", 
                 tabName = "GWR", icon= icon("map-marked-alt"))
    )
)


body <- dashboardBody(tabItems(
  tabItem(tabName = "overview", 
          h1("Overview Of Application"), 
          h3("Introduction"), 
          p("Current resale flat prices are determined largely based on the past 
                  transaction price of similar flats in the neighbourhood with price 
                  adjustments made to account for differences in flat conditions.However, the hedonic pricing model fails to take into account spatial attributes 
                  that potentially determines the intrinsic value of the resale flats."), 
          p("This application seeks to allow users to visualise the effects of spatial autocorrelation between 
                    attributes used in determining resale price of flats"), 
          h3("Using the Application"), 
          p("Our application allows users to select the spatial features they would like to experiment with")),
  tabItem(tabName = "settings",
          tabsetPanel(tabPanel("Feature Selection", icon = icon("check-square"), 
                               h3("Radius Selection"),
                               p("This page allows you to select your desired radius to view the number of features within the proximity selected"),
                               checkboxInput(inputId = "mrt", label = "MRT", value= FALSE), 
                               conditionalPanel(condition = "input.mrt == true",
                                                sliderInput(inputId = 'mrtWidth', label = "MRT (includes all MRT stations in Singapore)",
                                                            min = 0, 
                                                            max = 1000, 
                                                            value = c(0), step = 5)),
                              
                               checkboxInput(inputId = "pschool", label = "Primary Schools", value= FALSE), 
                               conditionalPanel(condition = "input.pschool == true",
                                                sliderInput(inputId = 'pschoolWidth', label = "Primary Schools (includes all primary schools in Singapore)",
                                                            min = 0, 
                                                            max = 1000, 
                                                            value = c(0), step = 5)),
                               checkboxInput(inputId = "sschool", label = "Secondary Schools", value= FALSE), 
                               conditionalPanel(condition = "input.sschool == true",
                                                sliderInput(inputId = 'sschoolWidth', label = "Secondary Schools (includes all secondary schools in Singapore)",
                                                            min = 0, 
                                                            max = 1000, 
                                                            value = c(0), step = 5)),
                               checkboxInput(inputId = "cc", label = "Community Centers", value= FALSE), 
                               conditionalPanel(condition = "input.cc == true",
                                                sliderInput(inputId = 'ccWidth', label = "Community Centers (includes all community centers in Singapore)",
                                                            min = 0, 
                                                            max = 1000, 
                                                            value = c(0), step = 5)),
                               checkboxInput(inputId = "supermarket", label = "Supermarkets ", value= FALSE), 
                               conditionalPanel(condition = "input.supermarket == true",
                                                sliderInput(inputId = 'supermarketWidth', label = "Supermarkets (includes all licensed supermarkets in Singapore)",
                                                            min = 0, 
                                                            max = 1000, 
                                                            value = c(0), step = 5)),
                               checkboxInput(inputId = "sport", label = "Sports Facilities", value= FALSE), 
                               conditionalPanel(condition = "input.sport == true",
                                                sliderInput(inputId = 'sportWidth', label = "Sports (includes all sports facilities managed by Sport-SG)",
                                                            min = 0, 
                                                            max = 1000, 
                                                            value = c(0), step = 5)),
                               checkboxInput(inputId = "preschool", label = "Preschools", value= FALSE), 
                               conditionalPanel(condition = "input.preschool == true",
                                                sliderInput(inputId = 'preschoolWidth', label = "Preschools (includes both kindergarten and childcare centers)",
                                                            min = 0, 
                                                            max = 1000, 
                                                            value = c(0), step = 5)),
                              
                               checkboxInput(inputId = "hawker", label = "Hawkers", value= FALSE), 
                               conditionalPanel(condition = "input.hawker == true",
                                                sliderInput(inputId = 'hawkerWidth', label = "Hawkers (includes all hawker centers in Singapore)",
                                                            min = 0, 
                                                            max = 1000, 
                                                            value = c(0), step = 5)),
                               checkboxInput(inputId = "mall", label = "Shopping Malls", value= FALSE), 
                               conditionalPanel(condition = "input.mall == true",
                                                sliderInput(inputId = 'mallWidth', label = "Shopping Malls (includes all shopping malls in Singapore)",
                                                            min = 0, 
                                                            max = 1000, 
                                                            value = c(0), step = 5))),

                      tabPanel("View Data", icon = icon("database"), 
                               h3("Select variables to view in the HDB Resale Data"), 
                               div(selectInput("month", "Month", choices=c("2020-01", "2020-02", "2020-03" , "2020-04", "2020-05", "2020-06",
                                                                           "2020-07", "2020-08", "2020-09", "2020-10", "2020-11", "2020-12")), 
                                   style="display:inline-block"),
                               div(selectInput("flat", "Flat Type", choices=c("1 ROOM", "2 ROOM", "3 ROOM", "4 ROOM", "5 ROOM", "EXECUTIVE", "MULTI-GENERATION" )), 
                                   style="display:inline-block"),
                               div(selectInput("sample", "Sample Number", choices = c("100", "200", "300", "400", "500", "600", "700", "800", "900", "1000"))),
                               br(),
                               
                               DT::dataTableOutput(outputId = "popTab"), 
                               div(style = 'overflow-x: sschoocroll', tableOutput("Table"))), 
                      tabPanel("Analysis", icon = icon("chart-bar"),
                               selectInput("choice", "Choice of Town", choices = c(sf_resale_flat$town)),
                               conditionalPanel(condition = "input.mrt == true",
                               plotlyOutput("mrt_box")),
                               conditionalPanel("input.pschool == true",
                               plotlyOutput("pschool_box")),
                               conditionalPanel(condition = "input.sschool == true",
                               plotlyOutput("sschool_box")),
                               conditionalPanel(condition = "input.cc == true",
                               plotlyOutput("cc_box")),
                               conditionalPanel(condition = "input.supermarket == true",
                               plotlyOutput("supermarket_box")),
                               conditionalPanel(condition = "input.sport == true",
                               plotlyOutput("sport_box")),
                               conditionalPanel(condition = "input.preschool == true",
                               plotlyOutput("preschool_box")),
                               conditionalPanel(condition = "input.hawker == true",
                               plotlyOutput("hawker_box")),
                               conditionalPanel(condition = "input.mall == true",
                               plotlyOutput("mall_box"))
                               ))), 
  tabItem("transform",div(selectInput("var", "Select Variable to Transform", choices=c("resale_price", "remaining_lease", "floor_area_sqm")), 
                             style="display:inline-block"),
          div(selectInput("mode", "Select Transformation Mode", choices=c("Log", "Exp", "Sqrt")), 
              style="display:inline-block"),
          br(), 
          actionButton("trans", "Transform", icon = icon("exchange-alt"))),
  tabItem(tabName="GWR"))) 



ui <- dashboardPage(header, sidebar, body, skin = "red",)

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    

    new_resale_flat <- reactive({
      sf_resale_flat %>%
        mutate(mrt_buffer = st_buffer(sf_resale_flat, input$mrtWidth))
        #mutate(mrt_count = lengths(st_intersects(sf_resale_flat$mrt_buffer, mrt)))
    })
    
    output$new_resale_flat <- renderTable({
      new_resale_flat()
    })
    
    
  
    
    output$new_resale_flat_plot <- renderPlot({
    
      plot(new_resale_flat()$ds, new_resale_flat()$y)
  
    })
    
    output$popTab <-DT::renderDataTable({
      buffer_mrt <- st_buffer(sf_resale_flat, input$mrtWidth)
      buffer_psch <- st_buffer(sf_resale_flat, input$pschoolWidth)
      buffer_ssch <- st_buffer(sf_resale_flat, input$sschoolWidth)
      buffer_cc <- st_buffer(sf_resale_flat, input$ccWidth)
      buffer_supermarket <- st_buffer(sf_resale_flat, input$supermarketWidth)
      buffer_sport <- st_buffer(sf_resale_flat, input$sportWidth)
      buffer_preschool <- st_buffer(sf_resale_flat, input$preschoolWidth)
      
      buffer_hawker <- st_buffer(sf_resale_flat, input$hawkerWidth)
      buffer_mall <- st_buffer(sf_resale_flat, input$mallWidth)
      
      DT::datatable(data = sf_resale_flat %>% mutate(mrt_count = lengths(st_intersects(buffer_mrt, sf_mrt)))
                    %>% mutate(pri_school_count = lengths(st_intersects(buffer_psch, sf_pschool)))
                    %>% mutate(sec_school_count = lengths(st_intersects(buffer_ssch, sf_sschool)))
                    %>% mutate(community_center_count = lengths(st_intersects(buffer_cc, sf_cc)))
                    %>% mutate(supermarket_count = lengths(st_intersects(buffer_supermarket, sf_supermarket)))
                    %>% mutate(sport_count = lengths(st_intersects(buffer_sport, sf_sport_facilities)))
                    %>% mutate(preschool_count = lengths(st_intersects(buffer_preschool, sf_preschool)))
                    
                    %>% mutate(hawker_count = lengths(st_intersects(buffer_hawker, sf_hawkercenter)))
                    %>% mutate(mall_count = lengths(st_intersects(buffer_mall, sf_mall)))
                    %>% mutate(month = input$month)
                    %>% mutate(flat_type = input$flat),
                    extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                    options = list(
                      searching = TRUE,
                      autoWidth = TRUE,
                      rownames = FALSE,
                      scroller = TRUE,
                      scrollX = TRUE,
                      scrollY = "500px",
                      fixedHeader = FALSE,
                      class = 'cell-border stripe',
                      fixedColumns = list(
                        leftColumns = 3,
                        heightMatch = 'none'
                        
                      )
                    )
      )
      
      
    })

    
    
   
  


 
   
    
    sliderValues <- reactive({
      data.frame(Name = c("MRT", "Schools", "Supermarkets", "Sports","Preschools","Hawkers", "Shopping Malls"), 
                 Value = as.character(c(input$mrtWidth, input$schoolWidth, input$supermarketWidth, input$sportWidth, input$preschoolWidth, input$parkWidth, input$hawkerWidth, input$mallWidth)
                 ), 
                 stringsAsFactors = FALSE)
    })
      
    
    output$values <- renderTable({
        sliderValues()
      })
 
    
    
    
    output$mrt_box <- renderPlotly({
      buffer_mrt <- st_buffer(sf_resale_flat, input$mrtWidth)
      
      sf_resale_flat <- sf_resale_flat %>% mutate(mrt_count = lengths(st_intersects(buffer_mrt, sf_mrt)))
      
      ggplot(sf_resale_flat, aes(x = input$choice , y = mrt_count)) + geom_boxplot()
    })
    
    output$pschool_box <- renderPlotly({
      buffer_psch <- st_buffer(sf_resale_flat, input$pschoolWidth)
      
      sf_resale_flat <- sf_resale_flat %>% mutate(pri_school_count = lengths(st_intersects(buffer_psch, sf_pschool)))
      
      ggplot(sf_resale_flat, aes(x = input$choice , y = pri_school_count)) + geom_boxplot()
    })

    output$sschool_box <- renderPlotly({
      buffer_ssch <- st_buffer(sf_resale_flat, input$sschoolWidth)
      
      sf_resale_flat <- sf_resale_flat %>% mutate(sec_school_count = lengths(st_intersects(buffer_ssch, sf_sschool)))
      
      ggplot(sf_resale_flat, aes(x = input$choice , y = sec_school_count)) + geom_boxplot()
    })
  
    output$cc_box <- renderPlotly({
      buffer_cc <- st_buffer(sf_resale_flat, input$ccWidth)
      
      sf_resale_flat <- sf_resale_flat %>% mutate(community_center_count = lengths(st_intersects(buffer_cc, sf_cc)))
      
      ggplot(sf_resale_flat, aes(x = input$choice , y = community_center_count)) + geom_boxplot()
    })
  
    output$supermarket_box <- renderPlotly({
      buffer_supermarket <- st_buffer(sf_resale_flat, input$supermarketWidth)
      sf_resale_flat <- sf_resale_flat %>% mutate(supermarket_count = lengths(st_intersects(buffer_supermarket, sf_supermarket)))
      
      ggplot(sf_resale_flat, aes(x = input$choice , y = supermarket_count)) + geom_boxplot()
    })
    
    output$sport_box <- renderPlotly({
      buffer_sport <- st_buffer(sf_resale_flat, input$sportWidth)
      sf_resale_flat <- sf_resale_flat %>% mutate(sport_count = lengths(st_intersects(buffer_sport, sf_sport_facilities)))
      
      ggplot(sf_resale_flat, aes(x = input$choice , y = sport_count)) + geom_boxplot()
    })
    
    output$preschool_box <- renderPlotly({
      buffer_preschool <- st_buffer(sf_resale_flat, input$preschoolWidth)
      sf_resale_flat <- sf_resale_flat %>% mutate(preschool_count = lengths(st_intersects(buffer_preschool, sf_preschool)))
      
      ggplot(sf_resale_flat, aes(x = input$choice , y = preschool_count)) + geom_boxplot()
    })
    
    output$hawker_box <- renderPlotly({
      buffer_hawker <- st_buffer(sf_resale_flat, input$hawkerWidth)
      sf_resale_flat <- sf_resale_flat %>% mutate(hawker_count = lengths(st_intersects(buffer_hawker, sf_hawkercenter)))
      
      ggplot(sf_resale_flat, aes(x = input$choice , y = hawker_count)) + geom_boxplot()
    })
    
    output$mall_box <- renderPlotly({
      buffer_mall <- st_buffer(sf_resale_flat, input$mallWidth)
      sf_resale_flat <- sf_resale_flat %>% mutate(mall_count = lengths(st_intersects(buffer_mall, sf_mall)))
 
      ggplot(sf_resale_flat, aes(x = input$choice , y = mall_count)) + geom_boxplot()
    })
} 



shinyApp(ui, server)
