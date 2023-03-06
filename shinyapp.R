library(tidyverse)
library(shiny)
library(rsconnect)
setwd("C:/Users/ajsko/OneDrive/Desktop/UW/WI 2023/Info201/ps06-shiny-app")
  #rsconnect::deployApp('OneDrive/desktop/uw/WI 2023/info201/ps06-shiny-app/shinyapp.R')
  
  #
  # This is a Shiny web application. You can run the application by clicking
  # the 'Run App' button above.
  #
  # Find out more about building applications with Shiny here:
  #
  #    http://shiny.rstudio.com/
  #
  
  # Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    mainPanel("UAH Lower Troposphere Temperatures"),
    
    # Tab Panels
    tabsetPanel(
      tabPanel("About",
        mainPanel(
          h2("Satellite Temperature Data from",
            strong("UAH")),
          p("Temperature temp is measured as deviation (deg C) from 1991-2020 baseline"),
          p("The dataset contains 14310 observations and 5 variables 
            \nHere is a small (random) sample of data:"),
          tableOutput("smallsample")
        )
      ),
      tabPanel("Plots", 
        sidebarLayout(
          sidebarPanel(
            p("You can analyze the global temperature for different regions.
              Select the regions you are interested in.
              You see a monthly scatterplot and the corresponding trend lines."),
            checkboxInput("trend", "Display trend(s)"),
            #radioButtons("colorpalette", "Palette:", )
            
            uiOutput("checkboxes")
          ),
          mainPanel(
            plotOutput("plot")
          )
        )
      ),
              
      tabPanel("Tables", 
        sidebarLayout(
          sidebarPanel(
            uiOutput("tablechoices")
          ),
          mainPanel(
            tableOutput("tables")
          )
        )
      )
    )
)
  
  # Server
server <- function(input, output) {
    #temperatures <- read_delim("UAH-lower-troposphere-long.csv")
    temperatures <- temperatures %>% 
      mutate(time = year + month/12)

    regions <- unique(temperatures$region)
    
    output$smallsample <- renderTable(
      temperatures %>% 
        sample_n(5)
    )
    
    output$checkboxes <- renderUI({
      checkboxGroupInput(
        "regions", "Select the region(s) to display",
        choices = regions,
        selected = "globe"
      )
    })
    
    temps <- reactive({
      temperatures %>% 
        filter(region %in% input$selected)
    })
    
    output$plot <- renderPlot({
      p <- temps() %>% 
        ggplot(aes(x=time, y=temp)) + geom_point()
        
    })
}
  # Run the application 
  shinyApp(ui = ui, server = server)
  