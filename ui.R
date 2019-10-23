#Load Libraries
library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(shinythemes)
library(sf)
library(raster)
library(spData)
library(tmap) 
library(mapview)
library(ggplot2)


fluidPage(theme = shinytheme("lumen"), 
navbarPage(    
  "MODELS:",
  tabPanel("Template",             
           sidebarPanel(
             fileInput("file", "File input:"),
             textInput("txt", "Text input:", "general"),
             sliderInput("slider", "Slider input:", 1, 100, 30),
             tags$h5("Deafult actionButton:"),
             actionButton("action", "Search"),
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Tab 1",
                        h4("Table"),
                        tableOutput("table"),
                        h4("Verbatim text output"),
                        verbatimTextOutput("txtout"),
                        h1("Header 1"),
                        h2("Header 2"),
                        h3("Header 3"),
                        h4("Header 4"),
                        h5("Header 5")
               ),
               tabPanel("Tab 2", "This panel is intentionally left blank"),
               tabPanel("Tab 3", "This panel is intentionally left blank")
             )
           )
  ), ###End "tabPanel=Template"
  tabPanel("Probability", "This panel is intentionally left blank"),
  tabPanel("Something", "This panel is intentionally left blank"),
  tabPanel("Simulated", "This panel is intentionally left blank"),
  tabPanel("PETERSON",             
           sidebarPanel(
             fileInput("file", "File input:"),
             textInput("txt", "Text input:", "general"),
             sliderInput("slider", "Slider input:", 1, 100, 30),
             tags$h5("Deafult actionButton:"),
             actionButton("action", "Search"),
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Interactive Map",
                        h4("Table"),
                        tableOutput("table"),
                        h4("Verbatim text output"),
                        verbatimTextOutput("txtout"),
                        h1("Header 1")
               ),
               
               tabPanel("Bar Graph", 
                        "This panel is intentionally left blank"),
               tabPanel("Data Analisys", 
                        "This panel is intentionally left blank")
             )
           )
  )  ###End "tabPanel=PETERSON"
           
)
)
