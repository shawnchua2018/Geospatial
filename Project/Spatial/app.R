
library(shiny)
library(shinydashboard)
library(shinyWidgets)

packages = c('shiny', 'sp', 'rgdal', 'rgeos', 'sf', 'tidyverse', 'olsrr', 'corrplot', 'ggpubr', 'sf', 'spdep', 'GWmodel', 'tmap', 'tidyverse', 'raster')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

resale_flat = st_read("data/aspatial/HDB_resale_price.csv")
#print(resale_flat)
preschool = st_read("data/geospatial/pre-schools-location-kml.kml")
hawkercenter = st_read("data/geospatial/hawker-centres-kml.kml")
cc = st_read("data/geospatial/community-clubs-kml.kml")
parks = st_read("data/geospatial/nparks-parks-kml.kml")
supermarket = st_read("data/geospatial/supermarkets-kml.kml")
sport_facilities = st_read("data/geospatial/sportsg-sport-facilities-kml.kml")
sf_mpsz2019 = st_read("data/geospatial/master-plan-2019-subzone-boundary-no-sea-kml.kml")

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

body <- dashboardBody(
    tabItems(
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
                tabsetPanel(tabPanel("Feature Selection", 
                                     icon = icon("check-square"),
                                     h1("Pick features included in model"),
                                     sliderInput(inputId = "MRT", 
                                                 label = "MRT",
                                                 min = 250, 
                                                 max = 1000,
                                                 value = c(50)), 
                                     prettyCheckbox("mrt_check", 
                                                    "To be included in model?", 
                                                    value = FALSE, shape="curve", 
                                                    outline = TRUE),
                                     sliderInput(inputId = "school", 
                                                 label = "Schools",
                                                 min = 250, 
                                                 max = 1000, 
                                                 value = c(50)),  
                                     prettyCheckbox("school_check", 
                                                    "To be included in model?", 
                                                    value = FALSE, 
                                                    shape="curve", 
                                                    outline = TRUE), 
                                     sliderInput(inputId = "supermarket", 
                                                 label="Supermarkets", 
                                                 min = 250, 
                                                 max = 1000,
                                                 value = c(50)), 
                                     prettyCheckbox("supermarket_check", 
                                                    "To be included in model?", 
                                                    value = FALSE, 
                                                    shape="curve", 
                                                    outline = TRUE),  
                                     sliderInput(inputId = "sports", 
                                                 label="Sports Facilities", 
                                                 min = 250, 
                                                 max = 1000,
                                                 value = c(50)),
                                     prettyCheckbox("sports_check", 
                                                    "To be included in model?", 
                                                    value = FALSE, 
                                                    shape="curve", 
                                                    outline = TRUE),
                                     sliderInput(inputId = "preschool", 
                                                 label="Preschools", 
                                                 min = 250, 
                                                 max = 1000,
                                                 value = c(50)), 
                                     prettyCheckbox("preschool_check", 
                                                    "To be included in model?", 
                                                    value = FALSE, 
                                                    shape="curve", 
                                                    outline = TRUE), 
                                     sliderInput(inputId = "parks", 
                                                 label= "Parks", 
                                                 min = 250,
                                                 max = 1000,
                                                 value = c(50)), 
                                     prettyCheckbox("parks_check", 
                                                    "To be included in model?", 
                                                    value = FALSE, 
                                                    shape="curve", 
                                                    outline = TRUE),
                                     sliderInput(inputId = "hawker", 
                                                 label="Hawkers", 
                                                 min = 250, 
                                                 max = 1000,
                                                 value = c(50)), 
                                     prettyCheckbox("hawker_check", 
                                                    "To be included in model?", 
                                                    value = FALSE, 
                                                    shape="curve", 
                                                    outline = TRUE)), 
                            tabPanel("View Features", icon = icon("database"),
                                     DT::dataTableOutput(outputId = "popTab")))), 
        tabItem(tabName = "variables"), 
        tabItem(tabName = "GWR")
    ))


ui <- dashboardPage(header, sidebar, body, skin = "red")

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$popTab <- DT::renderDataTable({
      DT::datatable(data = resale_flat)
    })
}

shinyApp(ui, server)
