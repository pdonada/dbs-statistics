#Loading Libraries
library(shiny)
library(shinythemes)
library(dplyr)
library(gganimate)
library(ggplot2)
library(gifski)
library(leaflet)
library(leaflet.extras)
library(mapview)
library(raster)
library(sf)
library(spData)
library(tidyverse)
library(tmap) 
library(DT)

########################################
######Template
########################################

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
            
########################################
######PETERSON
########################################
            
            tabPanel("PETERSON",             
                     sidebarPanel(
                       
                       fileInput('file1', 
                                 'Choose file to upload (csv)',
                                 accept = ".csv"),
                       checkboxInput('header', 'Header', TRUE),
                       radioButtons("sep", "Separator",
                                    choices = c(Comma = ",",
                                                Semicolon = ";"),
                                    selected = ","),
                       radioButtons('quote', 'Quote',
                                   c(None='',
                                     'Double Quote'='"',
                                     'Single Quote'="'"),
                                   '"'),
                       tags$hr(),
                       
                       sliderInput("slider", "Slider input:", 1, 100, 30),
                       tags$h5("Deafult actionButton:"),
                       actionButton("action", "Search"),
                     ),
                     
                     mainPanel(
                       tabsetPanel(
                         tabPanel("Data Sample",
                                  h5("Generate an Extructured Table from your file"),
                                  DTOutput("contents")
                         ),
                         
                         tabPanel("Interactive Map",
                                  h5("Generate Maps"),
                                  leafletOutput(outputId = "mymap")
                                    )
                         )
                       )
                          
            )  ###End "tabPanel=PETERSON"
          )   ###End "navbarPage"
)   ###End "fluidPage"
