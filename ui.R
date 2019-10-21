library(shiny)

#library(shinythemes) #need to install.packages("shinythemes") to use. 

#fluidPage(theme -= shinytheme("lumen"), #if using shinythemes, starts "ui.R" here and don't forget to close ")" in the end of the code).

navbarPage(    
  "MODELS:",
  tabPanel("Exponential",             
           sidebarPanel(
             fileInput("file", "File input:"),
             textInput("txt", "Text input:", "general"),
             sliderInput("slider", "Slider input:", 1, 100, 30),
             tags$h5("Deafult actionButton:"),
             actionButton("action", "Search"),
             tags$h5("actionButton with CSS class:"),
             actionButton("action2", "Action button", class = "btn-primary")
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
  ),
  tabPanel("Probability", "This panel is intentionally left blank"),
  tabPanel("Simulated", "This panel is intentionally left blank")
)
#) #If using fluidpage uncomment this ")".
