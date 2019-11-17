###############################################
##
##    Development of Statistical Analytics App
##
##    ui.R - User interface defintion
##
###############################################

fluidPage(theme = shinytheme("lumen"), 
          
          navbarPage( "MODELS:", id = "tabs",
                      
                      ####################################################################################################
                      ##                                      PROBABILITY MODELS                                        ##
                      ####################################################################################################
                      tabPanel("Probability", "",
                               
                               sidebarPanel( 
                                 
                                 selectInput("distype", "Select type of distribution", 
                                             choices = c('Discrete' = 'discrete'
                                                         ,'Continuous' = 'continuous'), 
                                             selected = "discrete" 
                                 ),
                                 
                                 HTML("<hr style='height: 2px; color: #F3F3F3; background-color: #F3F3F3; border: none;'>"),
                                 
                                 selectInput("dismodel", "Select Model"
                                             , choices = selectDiscreteDist
                                             , selected = "binomial" 
                                 ),

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
                                     
                                     selectInput("bgender_series", "Select a serie"
                                                 , choices = bdbgender_series
                                                 , selected = " "
                                     ),
                                     
                                     selectInput("bgender_country", "Select a country"
                                                 , choices = '' 
                                     ),
                                     
                                     selectInput("bgender_year", "Select a year"
                                                 , choices = '' 
                                     )
                                   ),
                                   
                                   
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
                                             tabPanel("Plot"
                                                      , textOutput("txtplotProb")
                                                      , hr()
                                                      , plotOutput("plotProb")
                                                     ),
                                             tabPanel("Table", DT::dataTableOutput("tabProb")) 
                                 )
                                 
                               )  # mainPanel
                               
                      ), # tabPanel probability
                      
                      
                      ####################################################################################################
                      ##                                  DESCRIPTIVE ANALYSIS                                          ##
                      ####################################################################################################
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
                               ),  # sidebarPanel
                               
                               mainPanel(
									tabsetPanel(
									  tabPanel("Data Table",
											   h5("Extructured table from your file (Millions U$)"),
											   h6("Source: https://databank.worldbank.org/GDP_by_Country/id/bba0f640"),
											   DT::dataTableOutput("view1_pd")
									  ),
									  
									  tabPanel("Summary",
											   # Output: Header + summary of distribution ----
											   h5("Summary"),
											   verbatimTextOutput("view2_pd"),
											   
											   # Output: Header + table of distribution ----
											   h5("Observations"),
											   tableOutput("view3_pd")
									  ),
									  
									  tabPanel("Mean by Country", 
											   h5("Mean of GDP grouped by country"),
											   DT::dataTableOutput("view4_pd")),
									  
									  tabPanel("Top 10", 
											   h5("Mean of GDP grouped by country showing TOP 10 GDP"),
											   DT::dataTableOutput("view5_pd")),

									  tabPanel("Table Config",
											   h5("Generate a table considering arguments"),
											   DT::dataTableOutput("view6_pd")),
											   
  								  tabPanel("Boxplot", 
  								          h5("Boxplot of TOP 10 GDP"),
  								          plotOutput("view7_pd"))	   
                                   
                                 )
                               ) # mainPanel
                               
                      ),  # tabPanel Describing Data
                      

                      ####################################################################################################
                      ##                                  HYPOTHESIS TESTING                                            ##
                      ####################################################################################################        
                      tabPanel("OtherTopic", "This panel is intentionally left blank"),
                      
                      
                      ####################################################################################################
                      ##                                  GENERALIZED LINEAR MODELS                                     ##
                      ####################################################################################################                      
                      tabPanel("OtherTopic", "This panel is intentionally left blank")
                      
                      
                      
          ) # navbarPage
          
) # fluidPage
