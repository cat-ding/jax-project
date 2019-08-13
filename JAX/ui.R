
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source("Variables.R")
source("Plot.R")
library(shiny)


shinyUI(fluidPage(
  
  titlePanel(title = "Network Graph"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("genename", "Select a gene", node.list), 
      
      sliderInput(inputId = "num",
                             label = "Choose a number", 
                             min = 1,
                             max = 9,
                             step = 1,
                             value = 1),
                 

                 h5("Select coefficients to display"),
                 
                 checkboxInput("ER", "ER", value = TRUE, width = NULL),
                 checkboxInput("p53", "p53", value = TRUE, width = NULL),
                 checkboxInput("Grade", "Grade", value = TRUE, width = NULL)
    ),
    mainPanel(plotOutput("network")) 
  )
)
)