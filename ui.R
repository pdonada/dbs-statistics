###############################################
##
##    Development of Statistical Analytics App
##
##    ui.R - User interface defintion
##
###############################################

fluidPage(theme = shinytheme("lumen"), 
          
          navbarPage( "MODELS:", id = "tabs",
                      
                      tabPanel("Probability", "",
                               
                               sidebarPanel( 
                                 
                                 selectInput("distype", "Select type of distribution", 
                                             choices = c('Discrete' = 'discrete'
                                                         ,'Continuous' = 'continuous'), 
                                             selected = "discrete" 
                                 ),
                                 
                                 # hr(),
                                 HTML("<hr style='height: 2px; color: #F3F3F3; background-color: #F3F3F3; border: none;'>"),
                                 
                                 selectInput("dismodel", "Select Model", 
                                             choices = '', 
                                             selected = "binomial" 
                                 ),
                                 
                                 # hr(),
                                 HTML("<hr style='height: 2px; color: #F3F3F3; background-color: #F3F3F3; border: none;'>"),
                                 
                                 h5("Parameters"),
                                 
                                 ###############################################
                                 ##          binomial distribution            ##
                                 conditionalPanel( 
                                   condition = "input.dismodel == 'binomial'", 
                                   
                                   radioButtons("dataset", "Data set",
                                                choices = c("Gender statistics" 
                                                            , "diamonds"  # source: 
                                                )
                                                , inline = TRUE),
                                   
                                   conditionalPanel( 
                                     condition = "input.dataset == 'Gender statistics'", 
                                     
                                     selectInput("bgender_country", "Select country", 
                                                 choices = bdbgender_country
                                     ),
                                     
                                     selectInput("bgender_year", "Select year", 
                                                 choices = '' 
                                     ),
                                     
                                     selectInput("bgender_series", "Select serie", 
                                                 choices = bdbgender_series
                                     )
                                   ),
                                   
                                   #  hr(),
                                   
                                   numericInput("n", "Number of trials (n)" , value = 10, min = 0)
                                 ), 
                                 
                                 ###############################################
                                 ##           poisson distribution            ##
                                 conditionalPanel(     
                                   condition = "input.dismodel == 'poisson'", 
                                   numericInput("lam", "Estimated rate of events (lambda)" 
                                                , value = 1
                                                , min = 0 ),
                                   
                                   h5("Inform other lambda values for comparison"),
                                   numericInput("lam2", "Estimated rate of events (lambda 2)" 
                                                , value = 0 , min = 0 ),
                                   numericInput("lam3", "Estimated rate of events (lambda 3)" 
                                                , value = 0 , min = 0 )
                                 ), 
                                 
                                 conditionalPanel(     
                                   condition = "input.dismodel == 'geometric'", 
                                   numericInput("probg", "Probability of sucess (p)" 
                                                , value = 0.5  # initial value
                                                , min = 0      # Minimum allowed value
                                                , max = 1      # Maximum allowed value
                                                , step = 0.1   # Interval to use when stepping between min and max
                                   ) 
                                   
                                 ), 
                                 
                                 conditionalPanel(     
                                   condition = "input.dismodel == 'normal'", 
                                   numericInput("mu", "Mean (mu)" , value = 0), 
                                   numericInput("sigma", "Standard deviation (sigma)" , value = 1 , min = 0)
                                 ),
                                 
                                 conditionalPanel(     
                                   condition = "input.dismodel == 'exponential'", 
                                   numericInput("lambda", "Estimated rate of events (lambda)" , value = 1 , min = 0 )
                                   
                                 ),
                                 
                                 
                                 conditionalPanel(     
                                   condition = "input.dismodel !== 'binomial' & input.dismodel !== 'exponential' ",
                                   numericInput("max", "Upper limit for x" , value = 5, min = 0)
                                 ),
                                 
                                 conditionalPanel(     
                                   condition = "input.dismodel !== 'normal' ",
                                   sliderInput("s", "Number of simulated data" ,min=1, max=1000, value = 500) 
                                 )
                                 
                               ), # sidebarPanel
                               
                               
                               mainPanel( 
                                 
                                 tabsetPanel(type = "tabs", 
                                             tabPanel("Plot", textOutput("txtplot")
                                                      , hr()
                                                      , plotOutput("plot")),
                                             tabPanel("Table", DT::dataTableOutput("tab")) 
                                 )
                                 
                               )  # mainPanel
                               
                      ), # tabPanel probability
                      
                      tabPanel("Describing Data",
                               sidebarPanel(
                                 fileInput("file1", "Choose file to upload (csv)", accept = ".csv"),
                                 checkboxInput('header', 'Header', TRUE),
                                 radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";"), selected = ","),
                                 radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'),
                                 tags$hr(),
                                 selectInput("gdp_country_pd", "Select country", 
                                             choices = ""
                                             ),  
                                 selectInput("gdp_year_pd", "Select Year:",
                                             choices = ""
                                             )
                               )
                      ),
                      
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Data Table",
                                   h5("Generate an Extructured Table from your file"),
                                   DT::dataTableOutput("view1_pd")
                          ),
                          
                          tabPanel("Data Analysis",
                                   # Output: Header + summary of distribution ----
                                   h4("Summary"),
                                   verbatimTextOutput("view2_pd"),
                                   
                                   # Output: Header + table of distribution ----
                                   h4("Observations"),
                                   tableOutput("view3_pd")
                          ),
                          
                          tabPanel("Mean by Country", 
                                   h4("Observations"),
                                   "This panel is intentionally left blank",
                                   DT::dataTableOutput("view4_pd")),
                          
                          tabPanel("Top 10", 
                                   h4("Observations"),
                                   "This panel is intentionally left blank",
                                   DT::dataTableOutput("view5_pd")),
                          
                          tabPanel("By Country",
                                   h5("Generate an Extructured Table from your file"),
                                   DT::dataTableOutput("view6_pd")
                          )
                        )
                      ),
                      tabPanel("OtherTopic", "This panel is intentionally left blank"),
                      tabPanel("OtherTopic", "This panel is intentionally left blank")
                      
          )
          
)
