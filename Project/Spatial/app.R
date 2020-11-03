
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)

packages = c('shiny', 'sp', 'rgdal', 'rgeos', 'sf', 'tidyverse', 'olsrr', 'corrplot', 'ggpubr', 'sf', 'spdep', 'GWmodel', 'tmap', 'tidyverse', 'raster')
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

resale_flat = resale_flat[,!(names(resale_flat) %in% c("month","flat_model","lease_commence_date","longitude","latitude"))]
sf_resale_flat <- st_as_sf(resale_flat, coords = c("X","Y"),crs= 3414)



school = st_read("data/aspatial/Schools.csv")

school$latitude <- as.numeric(school$latitude)
school$longitude <- as.numeric(school$longitude)

cord.dec.sch = SpatialPoints(cbind(school$longitude, school$latitude), proj4string=CRS("+proj=longlat"))

cord.UTM.sch <- spTransform(cord.dec.sch, CRS("+init=epsg:3414"))
cord.UTM.sch

new_coords_sch <- data.frame(cord.UTM.sch@coords)
summary(new_coords_sch)
new_coords_sch <- new_coords_sch %>%
  rename(X = coords.x1, Y = coords.x2)

school <- cbind(school, "X" = new_coords_sch[1], "Y" = new_coords_sch[2])

sf_school <- st_as_sf(school, coords = c("X","Y"),crs= 3414)


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

preschool = st_read("data/geospatial/pre-schools-location-kml.kml")

hawkercenter = st_read("data/geospatial/hawker-centres-kml.kml")

cc = st_read("data/geospatial/community-clubs-kml.kml")

parks = st_read("data/geospatial/nparks-parks-kml.kml")

supermarket = st_read("data/geospatial/supermarkets-kml.kml")

sport_facilities = st_read("data/geospatial/sportsg-sport-facilities-kml.kml")

sf_mpsz2019 = st_read("data/geospatial/master-plan-2019-subzone-boundary-no-sea-kml.kml")

# Changing to X, Y
cc <- cc %>%
  st_cast("MULTIPOINT") %>%
  st_cast("POINT")

cc_coords <- data.frame(st_coordinates(cc))

cord.dec.cc = SpatialPoints(cbind(cc_coords$X, cc_coords$Y), proj4string=CRS("+proj=longlat"))

cord.UTM.cc <- spTransform(cord.dec.cc, CRS("+init=epsg:3414"))
new_cc <- data.frame(cord.UTM.cc)

new_cc <- new_cc %>%
  rename(X = coords.x1, Y = coords.x2)

parks <- parks %>%
  st_cast("MULTIPOINT") %>%
  st_cast("POINT")

parks_coords <- data.frame(st_coordinates(parks))

cord.dec.parks = SpatialPoints(cbind(parks_coords$X, parks_coords$Y), proj4string=CRS("+proj=longlat"))

cord.UTM.parks <- spTransform(cord.dec.parks, CRS("+init=epsg:3414"))
new_parks <- data.frame(cord.UTM.parks)

new_parks <- new_parks %>%
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

# Shapefile 
mrt <- shapefile("data/geospatial/MRTLRTStnPtt.shp")
mrt <- st_as_sf(mrt, coords = c("XCOORD", "YCOORD"), crs=3414)
mrt <- st_transform(mrt,3414)
st_crs(mrt)
st_is_longlat(mrt)


# Converting all to SF 
sf_parks <- st_as_sf(new_parks, coords = c("X","Y"),crs= 3414)
sf_hawkercenter <- st_as_sf(new_hawkercenter, coords = c("X","Y"),crs= 3414)
sf_preschool <- st_as_sf(new_preschool, coords = c("X","Y"),crs= 3414)
sf_sport_facilities <- st_as_sf(new_sport_facilities, coords = c("X","Y"),crs= 3414)
sf_cc <- st_as_sf(new_cc, coords = c("X","Y"),crs= 3414)
sf_supermarket <- st_as_sf(new_supermarket, coords = c("X","Y"),crs= 3414)




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
                               checkboxInput(inputId = "mrt", label = "MRT", value= FALSE), 
                               conditionalPanel(condition = "input.mrt == true",
                                                sliderInput(inputId = 'mrtWidth', label = "MRT",
                                                            min = 0, 
                                                            max = 1000, 
                                                            value = c(0), step = 5)),
                               checkboxInput(inputId = "school", label = "Schools", value= FALSE), 
                               conditionalPanel(condition = "input.school == true",
                                                sliderInput(inputId = 'schoolWidth', label = "Schools",
                                                            min = 0, 
                                                            max = 1000, 
                                                            value = c(0), step = 5)),
                               checkboxInput(inputId = "supermarket", label = "Supermarkets", value= FALSE), 
                               conditionalPanel(condition = "input.supermarket == true",
                                                sliderInput(inputId = 'supermarketWidth', label = "Supermarkets",
                                                            min = 0, 
                                                            max = 1000, 
                                                            value = c(0), step = 5)),
                               checkboxInput(inputId = "sport", label = "Sports Facilities", value= FALSE), 
                               conditionalPanel(condition = "input.sport == true",
                                                sliderInput(inputId = 'sportWidth', label = "Sports",
                                                            min = 0, 
                                                            max = 1000, 
                                                            value = c(0), step = 5)),
                               checkboxInput(inputId = "preschool", label = "Preschools", value= FALSE), 
                               conditionalPanel(condition = "input.preschool == true",
                                                sliderInput(inputId = 'preschoolWidth', label = "Preschools",
                                                            min = 0, 
                                                            max = 1000, 
                                                            value = c(0), step = 5)),
                               checkboxInput(inputId = "park", label = "Parks", value= FALSE), 
                               conditionalPanel(condition = "input.park == true",
                                                sliderInput(inputId = 'parkWidth', label = "Parks",
                                                            min = 0, 
                                                            max = 1000, 
                                                            value = c(0), step = 5)),
                               checkboxInput(inputId = "hawker", label = "Hawkers", value= FALSE), 
                               conditionalPanel(condition = "input.hawker == true",
                                                sliderInput(inputId = 'hawkerWidth', label = "Hawkers",
                                                            min = 0, 
                                                            max = 1000, 
                                                            value = c(0), step = 5)),
                               checkboxInput(inputId = "mall", label = "Shopping Malls", value= FALSE), 
                               conditionalPanel(condition = "input.mall == true",
                                                sliderInput(inputId = 'mallWidth', label = "Shopping Malls",
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
                               br(), 
                               actionButton("filter", "Filter", icon = icon("fil")),
                               DT::dataTableOutput(outputId = "popTab"), div(style = 'overflow-x: sschoocroll', tableOutput("Table"))), 
                      tabPanel("test", tableOutput("values"))
                      )), 
  tabItem("transform",div(selectInput("var", "Select Variable to Transform", choices=c("resale_price", "remaining_lease", "floor_area_sqm")), 
                             style="display:inline-block"),
          div(selectInput("mode", "Select Transformation Mode", choices=c("Log", "Exp", "Sqrt")), 
              style="display:inline-block"),
          br(), 
          actionButton("trans", "Transform", icon = icon("exchange-alt"))),
  tabItem(tabName="GWR"))) 

  



ui <- dashboardPage(header, sidebar, body, skin = "red")

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    

    popdata <- reactive({
      data
    })
  
      
    output$popTab <- DT::renderDataTable({
      DT::datatable(data = sf_resale_flat, 
                    extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                    options = list(
                      searching = TRUE,
                      autoWidth = TRUE,
                      rownames = FALSE,
                      scroller = TRUE,
                      scrollX = TRUE,
                      scrollY = "500px",
                      fixedHeader = TRUE,
                      class = 'cell-border stripe',
                      fixedColumns = list(
                        leftColumns = 3,
                        heightMatch = 'none', 
                      
                      )
                    )
      )
    })
    
    sliderValues <- reactive({
      data.frame(Name = c("MRT", "Schools", "Supermarkets", "Sports","Preschools","Parks","Hawkers" ), 
                 Value = as.character(c(buffer_zone <- st_buffer(sf_resale_flat, (input$mrtWidth)),
                                        sf_resale_flat$Num_of_MRT <- lengths(st_intersects(buffer_zone, mrt))
                 )), 
                 stringsAsFactors = FALSE)
    })
      
    output$values <- renderTable({
        sliderValues()
      })
    

    
}

shinyApp(ui, server)
