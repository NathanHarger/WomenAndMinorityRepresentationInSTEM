
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
library(readxl)


shinyUI(fluidPage(

  # Application title
  titlePanel("Representation of Marginalized Groups in STEM Careers"),

  # Sidebar with a slider input for number of bins

    
   
    

    # Show a plot of the generated distribution
     tabsetPanel(
        tabPanel("Overview",  fluidRow(
          column(6,plotlyOutput("overviewBar")),
          column(6,plotlyOutput("overviewSum")))
     ),
        tabPanel("Minorites in STEM",  
                 fluidRow(
                   column(7,plotlyOutput("pie")),
                   column(5,plotlyOutput("pieSum"))),     sliderInput("When", "Choose a year to display", sep = "", animate=T,
                                                                      min = 2006, max = 2015,value="2006")),
        tabPanel("Genders in STEM",column(8,plotlyOutput("plot")),column(4,plotlyOutput("gender")),sliderInput("WhenGender", "Choose a year to display", sep = "", animate=T,
                                                                                                               min = 2006, max = 2015,value="2006"))),
      
      
  


  textOutput("copyright") 



)
)