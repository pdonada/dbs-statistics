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
                      ##                                  DESCRIPTIVE ANALYSIS                                          ##
                      ####################################################################################################
                      tabPanel("Describing Data",
                               sidebarPanel(
                                 fileInput("file1", "Choose file to upload (csv)", accept = ".csv"),
                                 checkboxInput('header', 'Header', TRUE),
                                 radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";"), selected = ","),
                                 radioButtons('quote', 'Quote', c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'),
                                 tags$hr(),
                                 selectInput("gdp_country_pd", "Select a country", 
                                             choices = ""
                                 ),  
                                 selectInput("gdp_year_pd", "Select a year",
                                             choices = ""
                                 )
                               ),  # sidebarPanel
                               
                               mainPanel(
                                 tabsetPanel(
                                   tabPanel("Data Table",
                                            h4("Please, select country and year to filter"),
                                            h6("Source: https://databank.worldbank.org/GDP_by_Country/id/bba0f640 | ! Please, upload file: gdp_database_tidy.csv"),
                                            DT::dataTableOutput("view6_pd")),
                                   
                                   #tabPanel("Data Table",
                                   #	   h5("Extructured table from your file (Millions U$)"),
                                   #	   h6("Source: https://databank.worldbank.org/GDP_by_Country/id/bba0f640"),
                                   #	   DT::dataTableOutput("view1_pd")
                                   #),
                                   
                                   tabPanel("Summary",
                                            # Output: Header + summary of distribution ----
                                            h4("Summary"),
                                            verbatimTextOutput("view2_pd"),
                                            
                                            # Output: Header + table of distribution ----
                                            h4("Observations"),
                                            tableOutput("view3_pd")),
                                   
                                   tabPanel("Mean by Country", 
                                            h4("GDP - Mean grouped by country in Millions U$"),
                                            DT::dataTableOutput("view4_pd")),
                                   
                                   tabPanel("Top 10",
                                      tabsetPanel(
                                        tabPanel("Table",
                                                 h4("TOP 10 GDP in Millions U$"),
                                                 DT::dataTableOutput("view5_pd")),
                                        
                                        tabPanel("Pie Chart", 
                                                 h4("TOP 10 countries by GDP in Trillions of U$"),
                                                 plotOutput("view7_pd")),
                                        
                                        tabPanel("Bar Chart", 
                                                 h4("TOP 10 countries by GDP in Trillions of U$"),
                                                 plotOutput("view8_pd"))
                                        )
                                      ),
                                   
                                   tabPanel("Heat Map", 
                                            h5("GDP-Mean by country representativity"),
                                            leafletOutput(outputId = "view9_pd"))
                                   
                                 )
                                 
                               ) # mainPanel
                               
                      ),  # tabPanel Describing Data
                      
                      
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
                                   
                                   radioButtons("bdataset", "Data set",
                                                choices = c("Gender statistics" = "gender"
                                                          , "Population statistics" = "population"
                                                )
                                                , inline = TRUE),
                                   
                                   selectInput("bgender_series", "Select a serie"
                                               , choices = ""
                                               , selected = " "
                                   ),
                                     
                                   selectInput("bgender_country", "Select a country"
                                               , choices = '' 
                                   ),
                                     
                                   selectInput("bgender_year", "Select a year"
                                               , choices = '' 
                                   ),
                                   
                                   
                                   numericInput("n", "Number of trials (n)" , value = 10, min = 0)
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
                                                      , conditionalPanel(     
                                                            condition = "input.dismodel == 'binomial' & input.bdataset == 'gender' & input.bgender_series == 'access'",
                                                            h5('Lets check the probability of accessing anti-retroviral drugs by gender.')
                                                        )
                                                      , conditionalPanel(     
                                                        condition = "input.dismodel == 'binomial' & input.bdataset == 'gender' & input.bgender_series == 'progression'",
                                                        h5('Lets check the probability of progressing to secondary school by gender.')
                                                      )
                                                      , conditionalPanel(     
                                                        condition = "input.dismodel == 'binomial' & input.bdataset == 'gender' & input.bgender_series == 'cause'",
                                                        h5('Lets check the probability that the cause of death is injury by gender (ages 15-34).')
                                                      )
                                                      , conditionalPanel(     
                                                        condition = "input.dismodel == 'binomial' & input.bdataset == 'population' & input.bgender_series == 'measles'",
                                                        h5('Lets check the probability of children aged 12-23 months being immunized to measles.')
                                                      )
                                                      , conditionalPanel(     
                                                        condition = "input.dismodel == 'binomial' & input.bdataset == 'population' & input.bgender_series == 'sanitation'",
                                                        h5('Lets check the probability of people using safely managed sanitation services on urban and rural areas.')
                                                      )                                              
                                                      
                                                      , conditionalPanel(     
                                                        condition = "input.dismodel == 'binomial'",
                                                        h6( paste('Data source:', binomial_source) )
                                                      )
                                                      , hr()
                                                      , plotOutput("plotProb")
                                                     ),
                                             tabPanel("Data table", DT::dataTableOutput("tabProbBy")),
                                             tabPanel("Data table (full)", DT::dataTableOutput("tabProb")) 
                                 )
                                 
                               )  # mainPanel
                               
                      ), # tabPanel probability
                      


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
