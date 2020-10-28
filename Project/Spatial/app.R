
library(shiny)
library(shinydashboard)
library(shinyWidgets)


header <- dashboardHeader(title = "$patial")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Overview", 
                 tabName = "overview", icon= icon("building")), 
        menuItem("Feature Settings", 
                 tabName = "settings", icon = icon("sliders-h")),
        menuItem("View Data", 
                 tabName = "view", icon = icon("database")),
        menuItem("Transform", 
                 tabName = "transform", icon= icon("chart-bar")),
        menuItem("GWR Modelling", 
                 tabName = "GWR", icon= icon("map-marked-alt"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "overview"), 
        tabItem(tabName = "settings", tabsetPanel(tabPanel("Feature Selection",
          h1("Pick features included in model"),
          sliderInput(
            inputId = "MRT", 
            label = "MRT", 
            min = 250, 
            max = 1000,
            value = c(50)
          ), prettyCheckbox("mrt_check", "To be included in model?", value = FALSE, shape="curve", outline = TRUE),
          sliderInput(inputId = "school", 
                      label = "Schools", 
                      min = 250, 
                      max = 1000, 
                      value = c(50)),  prettyCheckbox("school_check", "To be included in model?", value = FALSE, shape="curve", outline = TRUE), 
          sliderInput(inputId = "supermarket", 
                      label="Supermarkets", 
                      min = 250, 
                      max = 1000,
                      value = c(50)), prettyCheckbox("supermarket_check", "To be included in model?", value = FALSE, shape="curve", outline = TRUE),  
          sliderInput(inputId = "sports", 
                      label="Sports Facilities", 
                      min = 250, 
                      max = 1000,
                      value = c(50)),prettyCheckbox("sports_check", "To be included in model?", value = FALSE, shape="curve", outline = TRUE),
          sliderInput(inputId = "preschool", 
                      label="Preschools", 
                      min = 250, 
                      max = 1000,
                      value = c(50)), prettyCheckbox("preschool_check", "To be included in model?", value = FALSE, shape="curve", outline = TRUE), 
          sliderInput(inputId = "parks", 
                      label= "Parks", 
                      min = 250, 
                      max = 1000,
                      value = c(50)), prettyCheckbox("parks_check", "To be included in model?", value = FALSE, shape="curve", outline = TRUE),
          sliderInput(inputId = "hawker", 
                      label="Hawkers", 
                      min = 250, 
                      max = 1000,
                      value = c(50)), prettyCheckbox("hawker_check", "To be included in model?", value = FALSE, shape="curve", outline = TRUE))),tags$style(".fullwidth { width: 100% !important; }")),
        tabItem(tabName = "view", 
                "Display selected features"), 
        tabItem(tabName = "variables"), 
        tabItem(tabName = "GWR")
    )
)

ui <- dashboardPage(header, sidebar, body, skin = "red")

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
}

shinyApp(ui, server)
