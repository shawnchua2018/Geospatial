library(sp)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(rgdal)
library(rgeos)
library(sf)
library(tidyverse)
library(olsrr)
library(corrplot)
library(ggpubr)
library(spdep)
library(GWmodel)
library(tmap)
library(raster)
library(plotly)
library(leaflet)
library(tmaptools)
#packages = c('shiny', 'sp', 'rgdal', 'rgeos', 'sf', 'tidyverse', 'olsrr', 'corrplot', 'ggpubr', 'sf', 'spdep', 'GWmodel', 'tmap', 'tidyverse', 'raster','plotly', 'leaflet', 'tmaptools')
#for (p in packages){
#  if(!require(p, character.only = T)){
#    install.packages(p)
#  }
#  library(p,character.only = T)
#}

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


mrt <- st_read("data/aspatial/MRT_new.csv")
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

#print(st_crs(sf_resale_flat))
#print(st_crs(sf_pschool))
#print(st_crs(sf_sschool))
#print(st_crs(sf_hawkercenter))
#print(st_crs(sf_mall))
#print(st_crs(sf_preschool))
#print(st_crs(sf_supermarket))
#print(st_crs(sf_cc))
#print(st_crs(sf_sport_facilities))


header <- dashboardHeader(title = "$patial")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Overview", 
                 tabName = "overview", icon= icon("building")), 
        menuItem("Feature Settings", 
                 tabName = "settings", icon = icon("sliders-h")),
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
          p("Our applications allows users to create and anaylze Geographically Regression Models to understand how spatial features affect property prices in Singapore."),
          p("In our current application, the main data imported in regards to HDB resale prices in Sigapore is updated as of the year 2020."),
          p("Our application mainly consist of two tabs: 1) Feature Selection 2) GWR modelling"),
          br(),
          #p("In the feature selection tab, users can select additional spatial features in Singapore that may affect resale prices, these features include: MRTs, Primary Schools, Secondary Schools, Community Centers, Supermarkets, Sports Facilities, Preschools, Hawkers and Shopping Malls."),
          p(strong("Feature Selection tab")),
          p("1) Select spatial features to include in GWR modelling."),
          p("2) View computed data table with features selected."),
          p("3) View Histograms and Boxplots for Exploratory Data Analysis."),
          #p("Users can click to add features they desire, choose a radius for computation of features around the properties int he analysis."),
          #p("Additionally in this tab, users are able to view Exploratory Data Analysis through several plots such as Histograms and Boxplots in the sub tabs."),
          br(),
          #p("In the GWR Modelling tab, users can select features they desire to include in the formula used for GWR."),
          #p("After doing so, they can select the configuration they would like for their modeelling and analysis needs."),
          p(strong("GWR Modelling Tab")),
          p("1) Select Independent variables to formulate GWR equation."),
          p("2) Configure GWR modelling parameters."),
          p("3) View Spatial autocorrelation plots and GWR plots with analysis summary.")),
  tabItem(tabName = "settings",
          tabsetPanel(tabPanel("Feature Selection", icon = icon("check-square"), 
                               h3("Proximity Selection"),
                               p("This page allows you to select your desired radius to view the number of features within the proximity selected (Slider scale is in meters)"),
                               checkboxInput(inputId = "mrt", label = "MRT", value= FALSE), 
                               conditionalPanel(condition = "input.mrt == true",
                                                sliderInput(inputId = 'mrtWidth', label = "MRT (includes all MRT stations in Singapore)",
                                                            min = 100, 
                                                            max = 1000, 
                                                            value = c(500), step = 10)),
                              
                               checkboxInput(inputId = "pschool", label = "Primary Schools", value= FALSE), 
                               conditionalPanel(condition = "input.pschool == true",
                                                sliderInput(inputId = 'pschoolWidth', label = "Primary Schools (includes all primary schools in Singapore)",
                                                            min = 100, 
                                                            max = 1000, 
                                                            value = c(500), step = 10)),
                               checkboxInput(inputId = "sschool", label = "Secondary Schools", value= FALSE), 
                               conditionalPanel(condition = "input.sschool == true",
                                                sliderInput(inputId = 'sschoolWidth', label = "Secondary Schools (includes all secondary schools in Singapore)",
                                                            min = 100, 
                                                            max = 1000, 
                                                            value = c(500), step = 10)),
                               checkboxInput(inputId = "cc", label = "Community Centers", value= FALSE), 
                               conditionalPanel(condition = "input.cc == true",
                                                sliderInput(inputId = 'ccWidth', label = "Community Centers (includes all community centers in Singapore)",
                                                            min = 100, 
                                                            max = 1000, 
                                                            value = c(500), step = 10)),
                               checkboxInput(inputId = "supermarket", label = "Supermarkets ", value= FALSE), 
                               conditionalPanel(condition = "input.supermarket == true",
                                                sliderInput(inputId = 'supermarketWidth', label = "Supermarkets (includes all licensed supermarkets in Singapore)",
                                                            min = 100, 
                                                            max = 1000, 
                                                            value = c(500), step = 10)),
                               checkboxInput(inputId = "sport", label = "Sports Facilities", value= FALSE), 
                               conditionalPanel(condition = "input.sport == true",
                                                sliderInput(inputId = 'sportWidth', label = "Sports (includes all sports facilities managed by Sport-SG)",
                                                            min = 300, 
                                                            max = 1000, 
                                                            value = c(500), step = 10)),
                               checkboxInput(inputId = "preschool", label = "Preschools", value= FALSE), 
                               conditionalPanel(condition = "input.preschool == true",
                                                sliderInput(inputId = 'preschoolWidth', label = "Preschools (includes both kindergarten and childcare centers)",
                                                            min = 100, 
                                                            max = 1000, 
                                                            value = c(500), step = 10)),
                              
                               checkboxInput(inputId = "hawker", label = "Hawkers", value= FALSE), 
                               conditionalPanel(condition = "input.hawker == true",
                                                sliderInput(inputId = 'hawkerWidth', label = "Hawkers (includes all hawker centers in Singapore)",
                                                            min = 200, 
                                                            max = 1000, 
                                                            value = c(500), step = 10)),
                               checkboxInput(inputId = "mall", label = "Shopping Malls", value= FALSE), 
                               conditionalPanel(condition = "input.mall == true",
                                                sliderInput(inputId = 'mallWidth', label = "Shopping Malls (includes all shopping malls in Singapore)",
                                                            min = 200, 
                                                            max = 1000, 
                                                            value = c(500), step = 10))),

                      tabPanel("View Data", icon = icon("database"), 
                               h3("Select variables to view in the HDB Resale Data"), 
                               p(strong("If error message appears, filter is too specific resulting in too little data for sampling which affects analysis, please filter by only town or flat type at a single time or choose a smaller sample size.", style='color:red')),
                               #div(selectInput("month", "Month", choices=c("All", "2020-01", "2020-02", "2020-03" , "2020-04", "2020-05", "2020-06",
                                                                           #"2020-07", "2020-08", "2020-09", "2020-10", "2020-11", "2020-12")), 
                                   #style="display:inline-block"),
                               div(selectInput("town", "Town", choices = c("All", sf_resale_flat$town))),
                               div(selectInput("flat", "Flat Type", choices=c("All", "1 ROOM", "2 ROOM", "3 ROOM", "4 ROOM", "5 ROOM", "EXECUTIVE", "MULTI-GENERATION" )), 
                                   style="display:inline-block"),
                               p(strong("Note: The larger the sample size, the longer it takes for the GWR model to run")),
                               div(selectInput("sample", "Sample Size", choices = c("100", "200", "300", "400", "500", "600", "700", "800", "900", "1000"))),
                               br(),
                               
                               DT::dataTableOutput(outputId = "popTab"), 
                               div(style = 'overflow-x: sschoocroll', tableOutput("Table"))), 
                      tabPanel("Analysis", icon = icon("chart-bar"), tabsetPanel(tabPanel("Histogram", icon = icon("poll"),
                                                                                          p("Histogram of Resale Flat Price", style = "font-size:30px;"),
                                                                                          plotlyOutput("resale"),
                                                                                          p("Histogram of Floor Area", style = "font-size:30px;"),
                                                                                          plotlyOutput("floorarea"),
                                                                                          p("Histogram of Flat Type", style = "font-size:30px;"),
                                                                                          p("Legend: 1 - 1 Room Flat, 2 - 2 Room Flat, 3 - 3 Room Flat , 4 - 4 Room Flat, 5 - 5 Room Flat, 6 - Executive, 7 - Multi-Generation"),
                                                                                          plotlyOutput("flattype"),
                                                                                          conditionalPanel(condition = "input.mrt == true", 
                                                                                                           p("Histogram of MRT Count", style = "font-size:30px;"),
                                                                                                           plotlyOutput("mrt_hist")),
                                                                                          conditionalPanel("input.pschool == true",
                                                                                                           p("Histogram of Primary School Count", style = "font-size:30px;"),
                                                                                                           plotlyOutput("pschool_hist")),
                                                                                          conditionalPanel(condition = "input.sschool == true",
                                                                                                           p("Histogram of Secondary School Count", style = "font-size:30px;"),
                                                                                                           plotlyOutput("sschool_hist")),
                                                                                          conditionalPanel(condition = "input.cc == true",
                                                                                                           p("Histogram of Community Center Count", style = "font-size:30px;"),
                                                                                                           plotlyOutput("cc_hist")),
                                                                                          conditionalPanel(condition = "input.supermarket == true",
                                                                                                           p("Histogram of Supermarket Count", style = "font-size:30px;"),
                                                                                                           plotlyOutput("supermarket_hist")),
                                                                                          conditionalPanel(condition = "input.sport == true",
                                                                                                           p("Histogram of Sports Facilities Count", style = "font-size:30px;"),
                                                                                                           plotlyOutput("sport_hist")),
                                                                                          conditionalPanel(condition = "input.preschool == true",
                                                                                                           p("Histogram of Preschool Count", style = "font-size:30px;"),
                                                                                                           plotlyOutput("preschool_hist")),
                                                                                          conditionalPanel(condition = "input.hawker == true",
                                                                                                           p("Histogram of Hawker Center Count", style = "font-size:30px;"),
                                                                                                           plotlyOutput("hawker_hist")),
                                                                                          conditionalPanel(condition = "input.mall == true",
                                                                                                           p("Histogram of Shopping Mall Count", style = "font-size:30px;"),
                                                                                                           plotlyOutput("mall_hist"))), 
                               tabPanel("Boxplot For Resale Price",
                               conditionalPanel(condition = "input.town == 'All'",
                                                selectInput("town2", "Please use this filter only if filter in previous tab for Town is 'All'", choices = c("All", sf_resale_flat$town))),
                               plotlyOutput("resale_box", width = "1400px", height = "2000px")),
        
                               tabPanel("Boxplot For Selected Features",
                                        p("This tab will be empty if nothing is selected in the feature selection tab", style='color:red'),
                               conditionalPanel(condition = "input.mrt == true",
                                                p("Boxplot of MRT Count", style = "font-size:30px;"),
                               plotlyOutput("mrt_box")),
                               conditionalPanel("input.pschool == true",
                                                p("Boxplot of Primary School Count", style = "font-size:30px;"),
                               plotlyOutput("pschool_box")),
                               conditionalPanel(condition = "input.sschool == true",
                                                p("Boxplot of Secondary School Count", style = "font-size:30px;"),
                               plotlyOutput("sschool_box")),
                               conditionalPanel(condition = "input.cc == true",
                                                p("Boxplot of Community Center Count", style = "font-size:30px;"),
                               plotlyOutput("cc_box")),
                               conditionalPanel(condition = "input.supermarket == true",
                                                p("Boxplot of Supermarket Count", style = "font-size:30px;"),
                               plotlyOutput("supermarket_box")),
                               conditionalPanel(condition = "input.sport == true",
                                                p("Boxplot of Sports Facility Count", style = "font-size:30px;"),
                               plotlyOutput("sport_box")),
                               conditionalPanel(condition = "input.preschool == true",
                                                p("Boxplot of Preschool Count", style = "font-size:30px;"),
                               plotlyOutput("preschool_box")),
                               conditionalPanel(condition = "input.hawker == true",
                                                p("Boxplot of Hawker Count", style = "font-size:30px;"),
                               plotlyOutput("hawker_box")),
                               conditionalPanel(condition = "input.mall == true",
                                                p("Boxplot of Shopping Mall Count", style = "font-size:30px;"),
                               plotlyOutput("mall_box")))
                               

                               
                               )))), 
          
  tabItem(tabName="GWR", 
          tabsetPanel(tabPanel("Settings", icon = icon("cogs"),
                               p("Below is the Correlation Plot to assist in picking independent variables for further analysis", style = "font-size:30px;"),
                               p("Note: If radius in feature selection is too small and counts of feature are 0, error in correlation plot may occur, hence please modify radius input in the feature selection tab accordingly.", style = "font-size:20px;color:red"),
                               verbatimTextOutput("corr"),
                               h3("GWR formula: y = b0 + b1x1 + e (where y is the dependent variable, x1 is the independent variable, b0 and b1, are the parameters to be estimated, and e is a random error term)"),
                               h3("Fixed Dependent Variable(y): Resale Price"), 
                               h3("Please pick at least 1 Independant Variable (x) below to formulate GWR formula"), 
                               checkboxInput(inputId = "floor_incl", label = "Floor_area_sqm", value= TRUE),
                               checkboxInput(inputId = "flat_incl", label = "Flat Type", value= FALSE),
                               conditionalPanel(condition = "input.mrt == true",
                                                checkboxInput(inputId = "mrt_incl", label = "MRT", value= FALSE)),
                               conditionalPanel(condition = "input.pschool == true",
                                                checkboxInput(inputId = "pschool_incl", label = "Primary School", value= FALSE)),
                               conditionalPanel(condition = "input.sschool == true",
                                                checkboxInput(inputId = "sschool_incl", label = "Secondary School", value= FALSE)),
                               conditionalPanel(condition = "input.cc == true",
                                                checkboxInput(inputId = "cc_incl", label = "Community Centre", value= FALSE)),
                               conditionalPanel(condition = "input.supermarket == true",
                                                checkboxInput(inputId = "supermarket_incl", label = "Supermarket", value= FALSE)),
                               conditionalPanel(condition = "input.sport == true",
                                                checkboxInput(inputId = "sport_incl", label = "Sport Facilities", value= FALSE)),
                               conditionalPanel(condition = "input.preschool == true",
                                                checkboxInput(inputId = "preschool_incl", label = "Preschool", value= FALSE)),
                               conditionalPanel(condition = "input.hawker == true",
                                                checkboxInput(inputId = "hawker_incl", label = "Hawker", value= FALSE)),
                               conditionalPanel(condition = "input.mall == true",
                                                checkboxInput(inputId = "mall_incl", label = "Shopping Mall", value= FALSE)),
                               br(),
                               
                               
                               #plotOutput("corr"),
                               #actionButton("plot", "Correlation Plot"),
                               #conditionalPanel(condition = "input.plot == true",
                                                #verbatimTextOutput("corr")),
                               
                               #bsModal("modalExample", "Your plot", "go", size = "large",plotOutput("corr")),
                               
                               #actionButton("go", "Go"),
                               
                               h3("Bandwidth and Kernel Selection for GWR modelling"),
                               materialSwitch(inputId = "fix", label = strong("Bandwidth Selection"), status = "danger", value= TRUE),
                               #checkboxInput(inputId = "auto", label = "Auto Fixed Bandwidth", value= FALSE),
                               conditionalPanel(condition = "input.fix == true",
                                                checkboxInput(inputId = "auto", label = "Auto Fixed Bandwidth", value= FALSE),
                                                conditionalPanel(condition = "input.auto == false",
                                                                 numericInput("bandwidth", "Manual Fixed Bandwidth: ", 1000,
                                                                              min = NA,
                                                                              max = NA,
                                                                              step = 100,
                                                                              width = NULL))),
                               #checkboxInput(inputId = "kadapt", label = "Adaptive Bandwidth", value= FALSE),
                               materialSwitch(inputId = "kadapt", label = strong("Adaptive Kernel"), status = "danger"), 
                               h3("Kernel Selection"), 
                               selectInput(inputId = "kernel", label = "Select Kernel type: ", 
                                           choices = c("Gaussian", "Exponential", "Bi-square", "Tricube", "Boxcar")),
                               #actionButton("plot", "Plot models"),
                               ),
                      
                      tabPanel("Spatial Autocorrelation",
                               #conditionalPanel(condition = "input.plot == false",
                                                #p("Please fill in information in previous tab to see plot on this page", style='color:red')),
                               #plotlyOutput("resplot", width = "1400px", height = "1400px")),
                               #conditionalPanel(condition = "input.plot == true",
                               p("Plot takes a moment to load"),
                               p("Data points can be clicked for more information"),
                               tmapOutput("res_plot", height=600),
                               verbatimTextOutput("morani")
                               ),
                      
                      tabPanel("Fixed Kernel GWR", icon = icon("ruler"),
                               tabsetPanel(tabPanel("GWR Map", icon= icon("map"),
                                                    #actionButton("plot", "Plot map"),
                                                    conditionalPanel(condition = "input.fix == false",
                                                    p("Please check the Bandwidth option to see plot on this page", style='color:red')),
                                                    
                                                    conditionalPanel(#condition = "input.plot == true",
                                                                     condition = "input.fix == true",
                                                                     p("Plot takes a moment to load"),
                                                                     p("Data points can be clicked for more information"),
                                                                     #plotOutput("fixed_plot"))), 
                                                                     tmapOutput("fixed_plot1", height=600))),
                                           tabPanel("GWR Data Output", icon= icon("file-excel"),
                                                    conditionalPanel(condition = "input.fix == false",
                                                                     p("Please check the Bandwidth option to see plot on this page", style='color:red')),
                                                    conditionalPanel(condition = "input.fix == true",
                                           verbatimTextOutput("fixsum"))))), 
                      tabPanel("Adaptive Kernel GWR",icon = icon("ruler"),
                               tabsetPanel(tabPanel("GWR Map", icon= icon("map"),
                                                    conditionalPanel(condition = "input.kadapt == false",
                                                                     p("Please check the Adaptive Kernel option to see plot on this page", style='color:red')),
                                                    conditionalPanel(condition = "input.kadapt == true",
                                                                     p("Plot takes a moment to load"),
                                                                     p("Data points can be clicked for more information"),
                                                                     #plotOutput("adapt_plot"))),
                                                                     tmapOutput("adapt_plot1", height=600))),
                                           tabPanel("GWR Data Output", icon = icon("file-excel"),
                                                    conditionalPanel(condition = "input.kadapt == false",
                                                                     p("Please check the Adaptive Kernel option to see plot on this page", style='color:red')),
                                                    conditionalPanel(condition = "input.kadapt == true",
                                           verbatimTextOutput("adaptsum")))))
                               
                               
                               ))))



ui <- dashboardPage(header, sidebar, body, skin = "red",)

server <- function(input, output) {
    set.seed(0)
    histdata <- rnorm(500)
    
   processed_table <- reactive({
       if (input$flat !="All") {
         sf_resale_flat <- sf_resale_flat %>% filter(flat_type == input$flat)
       }
       if (input$town !="All") {
         sf_resale_flat <- sf_resale_flat %>% filter(town == input$town)
       }
      testtest <- as_Spatial(sf_resale_flat)
      testtest <- testtest[sample(1:length(testtest),input$sample),]
      sf_resale_flat <- st_as_sf(testtest)
      sf_resale_flat <- st_transform(sf_resale_flat, 3414)
      if (input$mrt==TRUE) {
        buffer_mrt <- st_buffer(sf_resale_flat, input$mrtWidth)
        sf_resale_flat <- sf_resale_flat %>% mutate(mrt_count = lengths(st_intersects(buffer_mrt, sf_mrt)))
      }
      if (input$pschool=="TRUE") {
        buffer_psch <- st_buffer(sf_resale_flat, input$pschoolWidth)
        sf_resale_flat <- sf_resale_flat %>% mutate(pri_school_count = lengths(st_intersects(buffer_psch, sf_pschool)))
      }
      if (input$sschool=="TRUE") {
        buffer_ssch <- st_buffer(sf_resale_flat, input$sschoolWidth)
        sf_resale_flat <- sf_resale_flat %>% mutate(sec_school_count = lengths(st_intersects(buffer_ssch, sf_sschool)))
      }
      if (input$cc=="TRUE") {
        buffer_cc <- st_buffer(sf_resale_flat, input$ccWidth)
        sf_resale_flat <- sf_resale_flat %>% mutate(community_center_count = lengths(st_intersects(buffer_cc, sf_cc)))
      }
      if (input$supermarket=="TRUE") {
        buffer_supermarket <- st_buffer(sf_resale_flat, input$supermarketWidth)
        sf_resale_flat <- sf_resale_flat %>% mutate(supermarket_count = lengths(st_intersects(buffer_supermarket, sf_supermarket)))
      }
      if (input$sport=="TRUE") {
        buffer_sport <- st_buffer(sf_resale_flat, input$sportWidth)
        sf_resale_flat <- sf_resale_flat %>% mutate(sport_count = lengths(st_intersects(buffer_sport, sf_sport_facilities)))
      }
      if (input$preschool=="TRUE") {
        buffer_preschool <- st_buffer(sf_resale_flat, input$preschoolWidth)
        sf_resale_flat <- sf_resale_flat %>% mutate(preschool_count = lengths(st_intersects(buffer_preschool, sf_preschool)))
      }
      if (input$hawker=="TRUE") {
        buffer_hawker <- st_buffer(sf_resale_flat, input$hawkerWidth)
        sf_resale_flat <- sf_resale_flat %>% mutate(hawker_count = lengths(st_intersects(buffer_hawker, sf_hawkercenter)))
      }
      if (input$mall=="TRUE") {
        buffer_mall <- st_buffer(sf_resale_flat, input$mallWidth)
        sf_resale_flat <- sf_resale_flat %>% mutate(mall_count = lengths(st_intersects(buffer_mall, sf_mall)))
      }

        #buffer_mrt <- st_buffer(sf_resale_flat, input$mrtWidth)
        #buffer_psch <- st_buffer(sf_resale_flat, input$pschoolWidth)
        #buffer_ssch <- st_buffer(sf_resale_flat, input$sschoolWidth)
        #buffer_cc <- st_buffer(sf_resale_flat, input$ccWidth)
        #buffer_supermarket <- st_buffer(sf_resale_flat, input$supermarketWidth)
        #buffer_sport <- st_buffer(sf_resale_flat, input$sportWidth)
        #buffer_preschool <- st_buffer(sf_resale_flat, input$preschoolWidth)
        #buffer_hawker <- st_buffer(sf_resale_flat, input$hawkerWidth)
        #buffer_mall <- st_buffer(sf_resale_flat, input$mallWidth)
        
        #sf_resale_flat <- sf_resale_flat %>% mutate(mrt_count = lengths(st_intersects(buffer_mrt, sf_mrt)))
        #sf_resale_flat <- sf_resale_flat %>% mutate(pri_school_count = lengths(st_intersects(buffer_psch, sf_pschool)))
        #sf_resale_flat <- sf_resale_flat %>% mutate(sec_school_count = lengths(st_intersects(buffer_ssch, sf_sschool)))
        #sf_resale_flat <- sf_resale_flat %>% mutate(community_center_count = lengths(st_intersects(buffer_cc, sf_cc)))
        #sf_resale_flat <- sf_resale_flat %>% mutate(supermarket_count = lengths(st_intersects(buffer_supermarket, sf_supermarket)))
        #sf_resale_flat <- sf_resale_flat %>% mutate(sport_count = lengths(st_intersects(buffer_sport, sf_sport_facilities)))
        #sf_resale_flat <- sf_resale_flat %>% mutate(preschool_count = lengths(st_intersects(buffer_preschool, sf_preschool)))
        #sf_resale_flat <- sf_resale_flat %>% mutate(hawker_count = lengths(st_intersects(buffer_hawker, sf_hawkercenter)))
        #sf_resale_flat <- sf_resale_flat %>% mutate(mall_count = lengths(st_intersects(buffer_mall, sf_mall)))
        #sf_resale_flat <- sf_resale_flat %>% filter(month == input$month)
        #sf_resale_flat <- sf_resale_flat %>% filter(flat_type == input$flat)
      sf_resale_flat
    })
  
    output$popTab <-DT::renderDataTable({
      DT::datatable(data = processed_table(),
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
                        heightMatch = 'none')
                    )
      )
    })
    


    restable <- reactive({
      sf_resale_flat <- processed_table()
      mylist <- c()
      if (input$mrt==TRUE) {
        newelem <- 'mrt_count'
        mylist <- c(mylist, newelem)
      }
      if (input$pschool=="TRUE") {
        newelem <- 'pri_school_count'
        mylist <- c(mylist, newelem)
      }
      if (input$sschool=="TRUE") {
        newelem <- 'sec_school_count'
        mylist <- c(mylist, newelem)
      }
      if (input$cc=="TRUE") {
        newelem <- 'community_center_count'
        mylist <- c(mylist, newelem)
      }
      if (input$supermarket=="TRUE") {
        newelem <- 'supermarket_count'
        mylist <- c(mylist, newelem)
      }
      if (input$sport=="TRUE") {
        newelem <- 'sport_count'
        mylist <- c(mylist, newelem)
      }
      if (input$preschool=="TRUE") {
        newelem <- 'preschool_count'
        mylist <- c(mylist, newelem)
      }
      if (input$hawker=="TRUE") {
        newelem <- 'hawker_count'
        mylist <- c(mylist, newelem)
      }
      if (input$mall=="TRUE") {
        newelem <- 'mall_count'
        mylist <- c(mylist, newelem)
      }
      if (input$floor_incl=="TRUE") {
        newelem <- 'floor_area_sqm'
        mylist <- c(mylist, newelem)
      }
      if (input$flat_incl=="TRUE") {
        newelem <- 'flat_type_code'
        mylist <- c(mylist, newelem)
      }
      GwrFormula <- as.formula(paste('resale_price',paste(mylist, collapse="+"), sep="~"))
      resale_flat.mlr <- lm(formula = GwrFormula, data=sf_resale_flat)
      mlr.output <- as.data.frame(resale_flat.mlr$residuals)
      sf_resale_flat.res <- cbind(sf_resale_flat, mlr.output$`resale_flat.mlr$residuals`)%>%rename(`MLR_RES` = `mlr.output..resale_flat.mlr.residuals.`)
      sf_resale_flat.res <- sf_resale_flat.res%>%mutate(flat_type_code = as.numeric(flat_type_code))
      sf_resale_flat.res <- sf_resale_flat.res%>%mutate(resale_price = as.numeric(resale_price))
      })

    output$testTab <-DT::renderDataTable({
      DT::datatable(data = fixing()@data,
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
                        heightMatch = 'none')
                    )
      )
    })
    
    fixing <- reactive({
      sf_resale_flat <- restable()
      sp_resale_flat <- as_Spatial(sf_resale_flat)
      drop <- c("month", "town", "block", "street_name", "flat_type", "storey_range", "remaining_lease", "remaining_lease_nearest_year")
      sp_resale_flat <- sp_resale_flat[,!(names(sp_resale_flat@data) %in% drop)]
      sp_resale_flat@data <- sp_resale_flat@data %>% mutate(resale_price = as.numeric(resale_price))
      sp_resale_flat@data <- sp_resale_flat@data %>% mutate(flat_type_code = as.numeric(flat_type_code))
      sp_resale_flat@data <- sp_resale_flat@data %>% mutate(floor_area_sqm = as.numeric(floor_area_sqm))
      sp_resale_flat
    })
    
    moran <- reactive({
      sf_resale_flat <- processed_table()
      sp_resale_flat <- fixing()
      nb <- dnearneigh(coordinates(sp_resale_flat), 0, 2000, longlat = FALSE)
      cnt <- card(nb)
      ind <- match(0, cnt)
      if(is.na(ind) == FALSE){
        nearest <- knearneigh(coordinates(sp_resale_flat))$nn
        print(nb[[ind]])
        nb[[ind]] <- nearest[ind]
        nb[[nearest[ind]]] <- c(as.integer(100), as.integer(nb[[nearest[ind]]]))
      }
      nb_lw <- nb2listw(nb, style = 'W')
      mylist <- c()
      if (input$mrt==TRUE) {
        newelem <- 'mrt_count'
        mylist <- c(mylist, newelem)
      }

      if (input$pschool=="TRUE") {
        newelem <- 'pri_school_count'
        mylist <- c(mylist, newelem)
      }
      if (input$sschool=="TRUE") {
        newelem <- 'sec_school_count'
        mylist <- c(mylist, newelem)
      }
      if (input$cc=="TRUE") {
        newelem <- 'community_center_count'
        mylist <- c(mylist, newelem)
      }
      if (input$supermarket=="TRUE") {
        newelem <- 'supermarket_count'
        mylist <- c(mylist, newelem)
      }
      if (input$sport=="TRUE") {
        newelem <- 'sport_count'
        mylist <- c(mylist, newelem)
      }
      if (input$preschool=="TRUE") {
        newelem <- 'preschool_count'
        mylist <- c(mylist, newelem)
      }
      if (input$hawker=="TRUE") {
        newelem <- 'hawker_count'
        mylist <- c(mylist, newelem)
      }
      if (input$mall=="TRUE") {
        newelem <- 'mall_count'
        mylist <- c(mylist, newelem)
      }
      if (input$floor_incl=="TRUE") {
        newelem <- 'floor_area_sqm'
        mylist <- c(mylist, newelem)
      }
      if (input$flat_incl=="TRUE") {
        newelem <- 'flat_type_code'
        mylist <- c(mylist, newelem)
      }
      #GwrFormula <- as.formula(paste('resale_price',paste(mylist, collapse="+"), sep="~"))
      GwrFormula <- formula()
      resale_flat.mlr <- lm(formula = GwrFormula, data=sf_resale_flat)
      lm.morantest(resale_flat.mlr, nb_lw)
    })
    
    corr <- reactive({
      sf_resale_flat <- processed_table()
      test = st_drop_geometry(sf_resale_flat)
      drop <- c("resale_price", "town", "month", "block", "street_name", "Address", "flat_type", "storey_range", "remaining_lease", "remaining_lease_nearest_year")
      test = test[,!(names(test) %in% drop)]
      test$flat_type_code <- as.numeric(test$flat_type_code)
      #test$resale_price <- as.numeric(test$resale_price)
      test$floor_area_sqm <- as.numeric(test$floor_area_sqm)
      #print(test)
      #test$remaining_lease_nearest_year <- as.numeric(test$remaining_lease_nearest_year)
      #test[10:18]
      corrplot(cor(test), diag = FALSE, order = "AOE", tl.pos = "td", tl.cex = 0.5, method = "number", type = "upper")
    })
    
    kernel_check <- reactive({
      if (input$kernel == "Gaussian"){
        kernel <- "gaussian"
      } else if (input$kernel == "Exponential"){
        kernel <- "exponential"
      } else if (input$kernel == "Bi-square"){
        kernel <- "bisquare"
      } else if (input$kernel == "Tricube"){
        kernel <- "tricube"
      } else if (input$kernel == "Boxcar"){
        kernel <- "boxcar"
      } 
    })

    formula<-reactive({
      mylist <- c()
      if (input$mrt==TRUE) {
        if (input$mrt_incl==TRUE) {
          newelem <- 'mrt_count'
          mylist <- c(mylist, newelem)
          }
      }
      if (input$pschool=="TRUE") {
        if (input$pschool_incl=="TRUE") {
          newelem <- 'pri_school_count'
          mylist <- c(mylist, newelem)
        }
      }
      if (input$sschool=="TRUE") {
        if (input$sschool_incl=="TRUE") {
          newelem <- 'sec_school_count'
          mylist <- c(mylist, newelem)
        }
      }
      if (input$cc=="TRUE") {
        if (input$cc_incl=="TRUE") {
          newelem <- 'community_center_count'
          mylist <- c(mylist, newelem)
        }
      }
      if (input$supermarket_incl=="TRUE") {
        if (input$supermarket_incl=="TRUE") {
          newelem <- 'supermarket_count'
          mylist <- c(mylist, newelem)
        }
      }
      if (input$sport=="TRUE") {
        if (input$sport_incl=="TRUE") {
          newelem <- 'sport_count'
          mylist <- c(mylist, newelem)
        }
      }
      if (input$preschool=="TRUE") {
        if (input$preschool_incl=="TRUE") {
          newelem <- 'preschool_count'
          mylist <- c(mylist, newelem)
        }
      }
      if (input$hawker=="TRUE") {
        if (input$hawker_incl=="TRUE") {
          newelem <- 'hawker_count'
          mylist <- c(mylist, newelem)
        }
      }
      if (input$mall_incl=="TRUE") {
        if (input$mall_incl=="TRUE") {
          newelem <- 'mall_count'
          mylist <- c(mylist, newelem)
        }
      }
      if (input$floor_incl=="TRUE") {
        newelem <- 'floor_area_sqm'
        mylist <- c(mylist, newelem)
      }
      if (input$flat_incl=="TRUE") {
        newelem <- 'flat_type_code'
        mylist <- c(mylist, newelem)
      }
      GwrFormula <- as.formula(paste('resale_price',paste(mylist, collapse="+"), sep="~"))
    })
    
    GWR_fixed <- reactive({
      sp_resale_flat <- fixing()
      kernel = kernel_check()
      GwrFormula <- formula()
      bw.fixed <- bw.gwr(formula=GwrFormula, data=sp_resale_flat, approach="CV", kernel=kernel, adaptive=FALSE, longlat=FALSE)
      set.seed(0)
      if (input$auto == "TRUE"){
        gwr.fixed <- gwr.basic(formula=GwrFormula, data=sp_resale_flat, bw=bw.fixed, kernel = kernel, longlat = FALSE)
      } 
      else {
        gwr.fixed <- gwr.basic(formula=GwrFormula, data=sp_resale_flat, bw=input$bandwidth, kernel = kernel, longlat = FALSE)
      }
      })

    GWR_adapt <- reactive({
      sp_resale_flat <- fixing()
      mylist <- c()
      if (input$mrt_incl==TRUE) {
        newelem <- 'mrt_count'
        mylist <- c(mylist, newelem)
      }
      if (input$pschool_incl=="TRUE") {
        newelem <- 'pri_school_count'
        mylist <- c(mylist, newelem)
      }
      if (input$sschool_incl=="TRUE") {
        newelem <- 'sec_school_count'
        mylist <- c(mylist, newelem)
      }
      if (input$cc_incl=="TRUE") {
        newelem <- 'community_center_count'
        mylist <- c(mylist, newelem)
      }
      if (input$supermarket_incl=="TRUE") {
        newelem <- 'supermarket_count'
        mylist <- c(mylist, newelem)
      }
      if (input$sport_incl=="TRUE") {
        newelem <- 'sport_count'
        mylist <- c(mylist, newelem)
      }
      if (input$preschool_incl=="TRUE") {
        newelem <- 'preschool_count'
        mylist <- c(mylist, newelem)
      }
      if (input$hawker_incl=="TRUE") {
        newelem <- 'hawker_count'
        mylist <- c(mylist, newelem)
      }
      if (input$mall_incl=="TRUE") {
        newelem <- 'mall_count'
        mylist <- c(mylist, newelem)
      }
      if (input$floor_incl=="TRUE") {
        newelem <- 'floor_area_sqm'
        mylist <- c(mylist, newelem)
      }
      if (input$flat_incl=="TRUE") {
        newelem <- 'flat_type_code'
        mylist <- c(mylist, newelem)
      }
      kernel = kernel_check()
      GwrFormula <- formula()
      #GwrFormula <- as.formula(paste('resale_price',paste(mylist, collapse="+"), sep="~"))
      bw.adaptive <- bw.gwr(formula=GwrFormula, data=sp_resale_flat, approach="CV", kernel=kernel, adaptive=TRUE, longlat=FALSE)
      set.seed(0)
      if (input$auto == "TRUE"){
        gwr.adaptive <- gwr.basic(formula = GwrFormula, data=sp_resale_flat, bw=bw.adaptive, kernel = kernel, adaptive=TRUE, longlat = FALSE)
      } 
      else {
        gwr.adaptive <- gwr.basic(formula = GwrFormula, data=sp_resale_flat, bw=input$bandwidth, kernel = kernel, adaptive=TRUE, longlat = FALSE)
      }
      
    })
    
    
    plot_fix <- reactive({
      gwr.fixed <- GWR_fixed()
      resale.sf.fixed <- st_as_sf(gwr.fixed$SDF)%>%st_transform(crs=3414)
      resale.sf.fixed.svy21 <- st_transform(resale.sf.fixed, 3414)
      gwr.fixed.output <- as.data.frame(gwr.fixed$SDF)
      resale.sf.fixed <- cbind(restable(), as.matrix(gwr.fixed.output))
      })
    
    plot_adapt <- reactive({
      gwr.adapt <- GWR_adapt()
      resale.sf.fixed <- st_as_sf(gwr.adapt$SDF)%>%st_transform(crs=3414)
      resale.sf.fixed.svy21 <- st_transform(resale.sf.fixed, 3414)
      gwr.adapt.output <- as.data.frame(gwr.adapt$SDF)
      resale.sf.fixed <- cbind(restable(), as.matrix(gwr.adapt.output))
    })
    
    output$res_plot <- renderTmap({
      tm_shape(sf_mpsz2019)+
        #tm_polygons()+
        tm_borders(lwd = 1, alpha = 1) +
        tm_shape(restable()) +  
        tm_dots(col = "MLR_RES",
                id = "Address",
                popup.vars = c("town", "Address", "resale_price"),
                alpha = 0.6,
                size = 0.2,
                style="quantile") +
        tm_view(set.zoom.limits = c(11,14))
    })
    
    #output$res_plot <- renderTmap({
      #tm_shape(sf_mpsz2019)+
        #tm_fill()+
        #tm_borders(lwd = 1, alpha = 1) +
        #tm_shape(restable()) +  
        #tm_dots(col = "MLR_RES",
                #alpha = 0.6,
                #style="quantile") +
        #tm_view(set.zoom.limits = c(11,14))
    #})
      output$fixed_plot1 <- renderTmap({
        tm_shape(sf_mpsz2019)+
          #tm_fill()+
          tm_borders(lwd = 1, alpha = 1) +
          tm_shape(plot_fix()) +  
          tm_dots(col = "Local_R2",
                  id = "Address",
                  popup.vars = c("town", "Address", "resale_price"),
                  alpha = 0.6,
                  size = 0.2,
                  border.col = "gray60",
                  border.lwd = 1) +
          tm_view(set.zoom.limits = c(11,14))
    })


      output$adapt_plot1 <- renderTmap({
        tm_shape(sf_mpsz2019)+
          #tm_fill()+
          tm_borders(lwd = 1, alpha = 1) +
          tm_shape(plot_adapt()) +  
          tm_dots(col = "Local_R2",
                  id = "Address",
                  popup.vars = c("town", "Address", "resale_price"),
                  alpha = 0.6,
                  size = 0.2,
                  border.col = "gray60",
                  border.lwd = 1) +
          tm_view(set.zoom.limits = c(11,14))
      })

    
    output$fixed_plot <- renderPlot({
      tm_shape(sf_mpsz2019)+
        tm_fill()+
        tm_borders(lwd = 1, alpha = 1) +
        tm_shape(plot_fix()) +  
        tm_dots(col = "Local_R2",
                border.col = "gray60",
                border.lwd = 1) +
        tm_view(set.zoom.limits = c(11,14))
    })
    
    output$adapt_plot <- renderPlot({
      tm_shape(sf_mpsz2019)+
        tm_fill()+
        tm_borders(lwd = 1, alpha = 1) +
        tm_shape(plot_adapt()) +  
        tm_dots(col = "Local_R2",
                border.col = "gray60",
                border.lwd = 1) +
        tm_view(set.zoom.limits = c(11,14))
    })
    
    #output$resplot <- renderTmap({
      #tm_shape(sf_mpsz2019)+
        #tm_polygons(alpha = 0.4) +
        #tm_shape(restable()) +  
        #tm_dots(col = "MLR_RES",
                #alpha = 0.6,
                #style="quantile") +
        #tm_view(set.zoom.limits = c(11,14))
    #})
    
    output$fixsum <- renderPrint({
      GWR<-GWR_fixed()
      GWR
    })
    
    output$adaptsum <- renderPrint({
      GWR<-GWR_adapt()
      GWR
    })
    
    output$morani <- renderPrint({
      test<-moran()
      test
    })
    
      
      output$corr <- renderPrint({
        corr<-corr()
        corr
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
 
    
    output$resale <- renderPlotly({
      sf_resale_flat <- processed_table()
      ggplot(data= sf_resale_flat, aes(x=as.numeric(`resale_price`))) +geom_histogram(bins=20, color="black", fill="light blue")
    })
    

    output$floorarea <- renderPlotly({
      sf_resale_flat <- processed_table()
      ggplot(data= sf_resale_flat, aes(x=as.numeric(`floor_area_sqm`))) +geom_histogram(bins=20, color="black", fill="light blue")
    })
    
    output$flattype <- renderPlotly({
      sf_resale_flat <- processed_table()
      ggplot(data= sf_resale_flat, aes(x=as.numeric(`flat_type_code`))) +geom_histogram(bins=20, color="black", fill="light blue")
    }) 
    
    output$mrt_hist <- renderPlotly({
      sf_resale_flat <- processed_table()
      ggplot(data= sf_resale_flat, aes(x=as.numeric(`mrt_count`))) +geom_histogram(bins=20, color="black", fill="light blue")
    })
    
    output$pschool_hist <- renderPlotly({
      sf_resale_flat <- processed_table()
      ggplot(data= sf_resale_flat, aes(x=as.numeric(`pri_school_count`))) +geom_histogram(bins=20, color="black", fill="light blue")
    })
    
    output$sschool_hist <- renderPlotly({
      sf_resale_flat <- processed_table()
      ggplot(data= sf_resale_flat, aes(x=as.numeric(`sec_school_count`))) +geom_histogram(bins=20, color="black", fill="light blue")
    })
    
    output$cc_hist <- renderPlotly({
      sf_resale_flat <- processed_table()
      ggplot(data= sf_resale_flat, aes(x=as.numeric(`community_center_count`))) +geom_histogram(bins=20, color="black", fill="light blue")
    })
    
    output$supermarket_hist <- renderPlotly({
      sf_resale_flat <- processed_table()
      ggplot(data= sf_resale_flat, aes(x=as.numeric(`supermarket_count`))) +geom_histogram(bins=20, color="black", fill="light blue")
    })
    
    output$sport_hist <- renderPlotly({
      sf_resale_flat <- processed_table()
      ggplot(data= sf_resale_flat, aes(x=as.numeric(`sport_count`))) +geom_histogram(bins=20, color="black", fill="light blue")
    })
    
    output$preschool_hist <- renderPlotly({
      sf_resale_flat <- processed_table()
      ggplot(data= sf_resale_flat, aes(x=as.numeric(`preschool_count`))) +geom_histogram(bins=20, color="black", fill="light blue")
    })
    
    output$hawker_hist <- renderPlotly({
      sf_resale_flat <- processed_table()
      ggplot(data= sf_resale_flat, aes(x=as.numeric(`hawker_count`))) +geom_histogram(bins=20, color="black", fill="light blue")
    })
    
    output$mall_hist <- renderPlotly({
      sf_resale_flat <- processed_table()
      ggplot(data= sf_resale_flat, aes(x=as.numeric(`mall_count`))) +geom_histogram(bins=20, color="black", fill="light blue")
    })
    
    #output$resale_box <- renderPlotly({
      #sf_resale_flat <- processed_table()
      #ggplot(sf_resale_flat, aes(x = flat_type, y = resale_price)) + geom_boxplot()
    #})
    
    output$resale_box <- renderPlotly({
      sf_resale_flat <- processed_table()
      if(input$town == "All"){
        if(input$town2 != "All"){
          sf_resale_flat <- sf_resale_flat%>%filter(town == input$town2)
        }
        ggplot(sf_resale_flat, aes(x = flat_type, y = resale_price, fill = flat_type)) + geom_boxplot()
      }
      else{
        ggplot(sf_resale_flat, aes(x = flat_type, y = resale_price, fill = flat_type)) + geom_boxplot()
      }
    })
    
    #output$resale_box2 <- renderPlotly({
      #sf_resale_flat <- processed_table()
      #if(input$town == "All"){
        #sf_resale_flat <- sf_resale_flat%>%filter(town == input$town2)
        #ggplot(sf_resale_flat, aes(x = flat_type, y = resale_price)) + geom_boxplot()
      #}
    #})
    
    
    output$mrt_box <- renderPlotly({
      sf_resale_flat <- processed_table()
      buffer_mrt <- st_buffer(sf_resale_flat, input$mrtWidth)
      sf_resale_flat <- sf_resale_flat %>% mutate(mrt_count = lengths(st_intersects(buffer_mrt, sf_mrt)))
      ggplot(sf_resale_flat, aes(x = input$choice , y = mrt_count)) + geom_boxplot()
      #ggplot(sf_resale_flat, aes(x = town , y = mrt_count, fill = town)) + geom_boxplot()
    })
    
    output$pschool_box <- renderPlotly({
      sf_resale_flat <- processed_table()
      buffer_psch <- st_buffer(sf_resale_flat, input$pschoolWidth)
      sf_resale_flat <- sf_resale_flat %>% mutate(pri_school_count = lengths(st_intersects(buffer_psch, sf_pschool)))
      ggplot(sf_resale_flat, aes(x = input$choice , y = pri_school_count)) + geom_boxplot()
    })

    output$sschool_box <- renderPlotly({
      sf_resale_flat <- processed_table()
      buffer_ssch <- st_buffer(sf_resale_flat, input$sschoolWidth)
      sf_resale_flat <- sf_resale_flat %>% mutate(sec_school_count = lengths(st_intersects(buffer_ssch, sf_sschool)))
      ggplot(sf_resale_flat, aes(x = input$choice , y = sec_school_count)) + geom_boxplot()
    })
  
    output$cc_box <- renderPlotly({
      sf_resale_flat <- processed_table()
      buffer_cc <- st_buffer(sf_resale_flat, input$ccWidth)
      sf_resale_flat <- sf_resale_flat %>% mutate(community_center_count = lengths(st_intersects(buffer_cc, sf_cc)))
      ggplot(sf_resale_flat, aes(x = input$choice , y = community_center_count)) + geom_boxplot()
    })
  
    output$supermarket_box <- renderPlotly({
      sf_resale_flat <- processed_table()
      buffer_supermarket <- st_buffer(sf_resale_flat, input$supermarketWidth)
      sf_resale_flat <- sf_resale_flat %>% mutate(supermarket_count = lengths(st_intersects(buffer_supermarket, sf_supermarket)))
      ggplot(sf_resale_flat, aes(x = input$choice , y = supermarket_count)) + geom_boxplot()
    })
    
    output$sport_box <- renderPlotly({
      sf_resale_flat <- processed_table()
      buffer_sport <- st_buffer(sf_resale_flat, input$sportWidth)
      sf_resale_flat <- sf_resale_flat %>% mutate(sport_count = lengths(st_intersects(buffer_sport, sf_sport_facilities)))
      ggplot(sf_resale_flat, aes(x = input$choice , y = sport_count)) + geom_boxplot()
    })
    
    output$preschool_box <- renderPlotly({
      sf_resale_flat <- processed_table()
      buffer_preschool <- st_buffer(sf_resale_flat, input$preschoolWidth)
      sf_resale_flat <- sf_resale_flat %>% mutate(preschool_count = lengths(st_intersects(buffer_preschool, sf_preschool)))
      ggplot(sf_resale_flat, aes(x = input$choice , y = preschool_count)) + geom_boxplot()
    })
    
    output$hawker_box <- renderPlotly({
      sf_resale_flat <- processed_table()
      buffer_hawker <- st_buffer(sf_resale_flat, input$hawkerWidth)
      sf_resale_flat <- sf_resale_flat %>% mutate(hawker_count = lengths(st_intersects(buffer_hawker, sf_hawkercenter)))
      ggplot(sf_resale_flat, aes(x = input$choice , y = hawker_count)) + geom_boxplot()
    })
    
    output$mall_box <- renderPlotly({
      sf_resale_flat <- processed_table()
      buffer_mall <- st_buffer(sf_resale_flat, input$mallWidth)
      sf_resale_flat <- sf_resale_flat %>% mutate(mall_count = lengths(st_intersects(buffer_mall, sf_mall)))
      ggplot(sf_resale_flat, aes(x = input$choice , y = mall_count)) + geom_boxplot()
    })
} 



shinyApp(ui, server)
