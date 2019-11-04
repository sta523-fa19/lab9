library(shiny)
library(tidyverse)
library(shinythemes)

ui <- shinyUI(
  fluidPage(theme = shinytheme("flatly"),
    titlePanel("Michigan Campsite Search"),
    

  )# fluidPage
) # shinyUI

server <- function(input, output) {
  
  
}

shinyApp(ui = ui, server = server)
