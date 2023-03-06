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
          p("The dataset contains 14310 observations and 5 variables.", 
            strong("\nHere is a small (random) sample of data:")),
          tableOutput("smallsample")
        )
      ),
      tabPanel("Plots", 
        sidebarLayout(
          sidebarPanel(
            p("Global temperatures for different regions!
              Select the regions you want to see on the graph.
              There will be a monthly scatterplot, and trend lines if the option is checked."),
            checkboxInput("trend", "Display trend(s)"),
            uiOutput("checkboxes")
          ),
          mainPanel(
            plotOutput("plot"),
            textOutput("plotsum"),
            textOutput("min"),
            textOutput("max")
          )
        )
      ),
      tabPanel("Tables", 
        sidebarLayout(
          sidebarPanel(
            p("This panel displays average temperature by:\n",
              em("months, years, decades")),
            uiOutput("tablechoices")
          ),
          mainPanel(
            textOutput("tablesum"),
            tableOutput("tables")
          )
        )
      )
    )
)
  
  # Server
server <- function(input, output) {
    temperatures <- read_delim("UAH-lower-troposphere-long.csv")
    temperatures <- temperatures %>% 
      mutate(time = year + month/12)
    regions <- unique(temperatures$region)
    output$smallsample <- renderTable(
      temperatures %>% 
        sample_n(5)
    )
    output$checkboxes <- renderUI({
      checkboxGroupInput(
        "checkedregion", "Select the region(s) to display",
        choices = regions,
        selected = "globe"
      )
    })
    temps <- reactive({
      temperatures %>% 
        filter(region %in% input$checkedregion)
    })
    output$plot <- renderPlot({
      p <- ggplot(temps(), aes(x=time, y=temp, col = region)) + 
        geom_point() +
        labs(x = "Year", y = "Temperatures, Deviation from Baseline, Deg C", col = "Region")
      if (input$trend) {
        p <- p + geom_smooth()
      }
      p
    })
    output$plotsum <- renderText({
      temps() %>% 
        nrow() %>% 
        paste("Total observations:", .)
    })
    output$min <- renderText({
      min <- temps()$temp %>% 
        min()
      if(!is.infinite(min))
        paste("Minimum temperature:", min)
      else
        ""
    })
    output$max <- renderText({
      max <- temps()$temp %>% 
        max()
      if(!is.infinite(max))
        paste("Minimum temperature:", max)
      else
        ""
    })
    output$tablechoices <- renderUI({
      radioButtons("timeframe", strong("Average over:"), c("month", "year", "decade"), selected = "month")
    })
    output$tables <- renderTable({
      if (input$timeframe == "month") {
        monthtemps <- temperatures %>% 
          group_by(month) %>% 
          summarize(avgtemp = mean(temp))
      } else if (input$timeframe == "year") {
        temperatures %>% 
          group_by(year) %>% 
          summarize(avgtemp = mean(temp))
      } else if (input$timeframe == "decade") {
        temperatures %>% 
          mutate(decade = year - year %% 10) %>% 
          group_by(decade) %>% 
          summarize(avgtemp = mean(temp))
      }
    })
    output$tablesum <- renderText({
      if (input$timeframe == "month") {
        monthtemps <- temperatures %>% 
          group_by(month) %>% 
          summarize(avgtemp = mean(temp)) %>% 
          nrow() %>% 
          paste("Number of observations:", .)
      } else if (input$timeframe == "year") {
        temperatures %>% 
          group_by(year) %>% 
          summarize(avgtemp = mean(temp)) %>% 
          nrow() %>% 
          paste("Number of observations:", .)
      } else if (input$timeframe == "decade") {
        temperatures %>% 
          mutate(decade = year - year %% 10) %>% 
          group_by(decade) %>% 
          summarize(avgtemp = mean(temp)) %>% 
          nrow() %>% 
          paste("Number of observations:", .)
      }
    })
}
  # Run the application 
  shinyApp(ui = ui, server = server)
  