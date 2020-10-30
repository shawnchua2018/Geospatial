
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
        
        tabItem(tabName = "settings", tabsetPanel(tabPanel("Feature Selection", 
                                                           icon = icon("check-square"),
                                                           h3("Bandwidth Setting"),
                                                           checkboxInput(inputId = "mrt", label = "MRT", value = FALSE ),
                                                           conditionalPanel(condition = "input.mrt == true", 
                                                             sliderInput(inputId = "mrtWidth", 
                                                                         label = "MRT",
                                                                         min = 0, 
                                                                         max = 1000,
                                                                         value = c(0))),
                                                           checkboxInput(inputId = "school", label = "Schools", value = FALSE ),
                                                           conditionalPanel(condition = "input.school == true",
                                                                            sliderInput(inputId = "schoolWidth", 
                                                                                        label = "Schools",
                                                                                        min = 0, 
                                                                                        max = 1000,
                                                                                        value = c(0), step =5)),
                                                           checkboxInput(inputId = "supermarket", label = "Supermarkets", value = FALSE ),
                                                           conditionalPanel(condition = "input.supermarket == true",
                                                                            sliderInput(inputId = "supermarketWidth", 
                                                                                        label = "Supermarkets",
                                                                                        min = 0, 
                                                                                        max = 1000,
                                                                                        value = c(0), step = 5)),
                                                           checkboxInput(inputId = "sport", label = "Sports Facilities", value = FALSE ),
                                                           conditionalPanel(condition = "input.sport == true",
                                                                            sliderInput(inputId = "sportWidth", 
                                                                                        label = "Sports",
                                                                                        min = 0, 
                                                                                        max = 1000,
                                                                                        value = c(0), step = 5)),
                                                           checkboxInput(inputId = "preschool", label = "Preschools", value = FALSE ),
                                                           conditionalPanel(condition = "input.preschool == true",
                                                                            sliderInput(inputId = "preschoolWidth", 
                                                                                        label = "Preschools",
                                                                                        min = 0, 
                                                                                        max = 1000,
                                                                                        value = c(0), step = 5)),
                                                           checkboxInput(inputId = "park", label = "Parks", value = FALSE ),
                                                           conditionalPanel(condition = "input.park == true",
                                                                            sliderInput(inputId = "parkWidth", 
                                                                                        label = "Parks",
                                                                                        min = 0, 
                                                                                        max = 1000,
                                                                                        value = c(0), step = 5)),
                                                           checkboxInput(inputId = "hawker", label = "Hawkers", value = FALSE)),
                                                            conditionalPanel(condition = "input.hawker == true",
                                                                              sliderInput(inputId = "hawkerWidth", 
                                                                                          label = "Hawkers",
                                                                                          min = 0, 
                                                                                          max = 1000,
                                                                                          value = c(0), step = 5)),
 
                                                  tabPanel("View Features", icon = icon("database"),
                                                           DT::dataTableOutput(outputId = "popTab")), 
                                                  tabPanel("test", tableOutput("values")))),
        tabItem(tabName = "variables"), 
        tabItem(tabName = "GWR")))



ui <- dashboardPage(header, sidebar, body, skin = "red")

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$popTab <- DT::renderDataTable({
      DT::datatable(data = resale_flat)
    })
    
    # tried to link here but couldnt work
    sliderValues <- reactive({
      data.frame(Name = c("MRT", "Schools", "Supermarkets", "Sports","Preschools","Parks","Hawkers" ), 
                 Value = as.character(c(input$mrtWidth, 
                                        input$schoolWidth, 
                                        input$supermarketWidth,
                                        input$sportWidth,
                                        input$preschoolWidth,
                                        input$parkWidth,
                                        input$hawkerWidth)), 
                 stringsAsFactors = FALSE)
    })
      
    output$values <- renderTable({
        sliderValues()
      })
    

    
}

shinyApp(ui, server)
