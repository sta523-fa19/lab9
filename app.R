library(shiny)
library(tidyverse)
library(shinythemes)

sites <- read.csv("mi_park_camps.csv")

ui <- shinyUI(
  fluidPage(theme = shinytheme("flatly"),
    titlePanel("Michigan Campsite Search"),
    
    sidebarLayout(
      sidebarPanel(width = 3,
        sliderInput("rangeNum",
                    label = h3("Number of campsites:"),
                    min = 0,
                    max = 420,
                    value = c(0,420),
                    step=20
        ),
        
        selectInput("type",
                    label = "Type of campsites:",
                    levels(sites$Camp_type)
        ),
        
        checkboxInput("ada",
                      label = "ADA Sites Available:",
                      FALSE),

        checkboxGroupInput("districts",
                           label = "Choose district(s): ",
                           choices = sort(unique(sites$DISTRICT)),
                           selected = unique(sites$DISTRICT)),
        
        tags$div(align="right", actionButton("search", "Search"))
        
        
      ), # sidebarPanel
      
      mainPanel(
          tabsetPanel(type = "tabs",
            tabPanel(title = "Map", plotOutput("plot1")),
            tabPanel(title = "List", htmlOutput("text1"))
        )
      )
      
    ) #sidebarLayout
  )# fluidPage
) # shinyUI

server <- function(input, output) {
  
  site_search <- eventReactive(input$search, {
    sites %>%
      filter(TOTAL_SITE >= input$rangeNum[1], TOTAL_SITE <= input$rangeNum[2],
             Camp_type == input$type, DISTRICT %in% input$districts) %>%
      filter(if(input$ada){ ADA_SITES > 0 } else {ADA_SITES >= 0})
  })
  
  output$text1 <- renderText({

    sites1 <- site_search()
    
    if(nrow(sites1) > 0){
      outStr <- "<ul>"
  
      for(index in 1: length(sites1$FACILITY)){
          outStr <- paste0(outStr,"<li>",sites1$FACILITY[index], ": ", sites1$TOTAL_SITE[index], "</li>")
        }
      outStr <- paste0(outStr,"</ul>")
      
    } else {
      outStr <- ""
    }
    
    paste("<p>There are",
          nrow(sites1), 
          "campgrounds that match your search:</p>", 
          outStr)
    
  })
  
  output$plot1 <- renderPlot({


    miMap <- map_data("state", region = "michigan")
    
    plt <- ggplot(site_search()) +
      geom_polygon(data=miMap, aes(x = long, y = lat, group = group), 
                   colour="black", fill="gray") +
      coord_fixed(ratio = 1) +
      labs(x = "Longitude", y = "Latitude")
      theme_bw()

    if(nrow(site_search()) > 0){
      plt <- plt + 
        geom_point(data = site_search(), 
                   aes(x = long, y = lat, size = TOTAL_SITE, colour = COUNTY)) +
        labs(size = "Camp size", colour = "County")
    }

    plot(plt)
  })
}

shinyApp(ui = ui, server = server)
