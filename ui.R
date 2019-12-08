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
                                            h6("Source: https://databank.worldbank.org/GDP_by_Country/id/bba0f640"),
                                            h5("!Please, upload file: gdp_database_tidy.csv (folder Display on GitHub)"),
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
                      tabPanel("Probability",
                               
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
                                 
                                 ###############################################
                                 ##          normal   distribution            ##
                                 conditionalPanel(     
                                   condition = "input.dismodel == 'normal'", 
                                   radioButtons("ndataset", "Data set",
                                                choices = c("Blood pressure" = "bp"
                                                )
                                                , inline = TRUE),
                                                
                                   numericInput("norm_mu", "Mean (mu)" , value = 112), 
                                   numericInput("norm_sigma", "Standard deviation (sigma)" , value = 10 , min = 0)
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
                                                            condition = "input.dismodel == 'binomial' & input.bdataset == 'gender' & input.bgender_series == 'access'"
                                                            ,h5('Lets check the probability of accessing anti-retroviral drugs by gender.')
                                                            ,hr()
                                                      )
                                                      
                                                      , conditionalPanel(     
                                                        condition = "input.dismodel == 'binomial' & input.bdataset == 'gender' & input.bgender_series == 'progression'"
                                                        ,h5('Lets check the probability of progressing to secondary school by gender.')
                                                        ,hr()
                                                      )
                                                      , conditionalPanel(     
                                                        condition = "input.dismodel == 'binomial' & input.bdataset == 'gender' & input.bgender_series == 'cause'"
                                                        ,h5('Lets check the probability that the cause of death is injury by gender (ages 15-34).')
                                                        ,hr()
                                                      )
                                                      , conditionalPanel(     
                                                        condition = "input.dismodel == 'binomial' & input.bdataset == 'population' & input.bgender_series == 'measles'"
                                                        ,h5('Lets check the probability of children aged 12-23 months being immunized to measles.')
                                                        ,hr()
                                                      )
                                                      , conditionalPanel(     
                                                        condition = "input.dismodel == 'binomial' & input.bdataset == 'population' & input.bgender_series == 'sanitation'"
                                                        ,h5('Lets check the probability of people using safely managed sanitation services on urban and rural areas.')
                                                        ,hr()
                                                      )                                              
                                                      
                                                      , conditionalPanel(     
                                                        condition = "input.dismodel == 'binomial'"
                                                        ,h6( paste('Data source:', binomial_source) )
                                                        ,hr()
                                                      )
                                                      
                                                      , conditionalPanel(     
                                                        condition = "input.dismodel == 'normal'"
                                                        ,h5('This dataset contains information on the records of 100 adults from a small cross-sectional survey in 2001 investigating blood pressure and its determinants in a community.')
                                                        ,hr()
                                                        ,h6(paste('Data source:', normal_source))
                                                        ,hr()
                                                      )
                                                      
                                                      , plotOutput("plotProb")
                                                      
                                                     ), # plot
                                             
                                             tabPanel("Data table", 
                                                      conditionalPanel(     
                                                        condition = "input.dismodel == 'binomial'", 
                                                        h5('Information from the dataset by Country/Year selected.'),
                                                        DT::dataTableOutput("tabProbBy")
                                                      )
                                                      , conditionalPanel(     
                                                        condition = "input.dismodel == 'normal'", 
                                                        h5('Not available for this model.')
                                                      )
                                             ),
                                            
                                             tabPanel("Data table (full)", 
                                                      h5('All information from the dataset selected.'),
                                                      DT::dataTableOutput("tabProb")
                                             ) 
                                 )  # tabsetPanel tabs
                                 
                               )  # mainPanel
                               
                      ), # tabPanel probability
                      


                      ####################################################################################################
                      ##                                  HYPOTHESIS TESTING                                            ##
                      ####################################################################################################        
                      tabPanel("Hypothesis Testing",

                               mainPanel(

                                 tabsetPanel(

                                   tabPanel("t-Test",

                                            column(1),
                                            withMathJax(p(),p(),p("A t-test is any ", strong("hypothesis test"), " in which the ", strong("test statistic"), " follows ", strong("Student's t distribution"),
                                                                  "if the ", strong("null hypothesis"), "is true. Some common t-tests are:")),br(),

                                            code("One-sample t-test:",style="color:f8f8f8"),
                                            p(HTML("<ul> <li type=square> the parameter of interest is the population mean, &mu;<li type=square>"),p(),
                                              p("t-statistic = \\(\\frac{\\bar x -\\mu_0}{s_{x}/\\sqrt{n}}\\)"), HTML("</ul>")),br(),

                                            code("Two-sample t-test:",style="color:f8f8f8"),
                                            p(HTML("<ul> <li type=square> the parameter of interest is the difference between the two population means, &mu;<sub>1</sub>-&mu;<sub>2</sub> <li type=square>"),p(),
                                              p("t-statistic = \\(\\frac{(\\bar x_1 - \\bar x_2) -(\\mu_1-\\mu_2)}{\\sqrt{\\frac{s_{1}^2}{n_1} + \\frac{s_{2}^2}{n_2}}}\\)"), HTML("</ul>")),
                                            column(1)

                                   ),

                                   tabPanel("Data Exploration",

                                            tabsetPanel(

                                              tabPanel("Sample Data",

                                                       fluidRow(

                                                         column(3,
                                                                wellPanel(
                                                                  selectInput("sampdat", "Choose a data set:", choices=list("One-sample"=1,"Two-sample"=2), selected=1),
                                                                  bsPopover(id="sampdat", title="Data set information", content="Please chose the type of t-test.",trigger="hover",placement="right"),
                                                                  checkboxInput("usedata", "Use sample data", TRUE),
                                                                  bsTooltip("usedata","Uncheck this when not using sample data!","right"),
                                                                  tags$hr(),
                                                                  p(strong("One-sample data:"), " Quarterly UK gas consumption from 1960Q1 to 1986Q4, in millions of therms."),
                                                                  br(),
                                                                  p(strong("Two-sample data:"), " On-time data for UA (United Airlines) and DL (Delta Airlines) flights that departed from NYC in 2013. Arrival delays, in minutes. Negative times represent early arrivals."),
                                                                  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                                                  )),

                                                         column(9,
                                                                conditionalPanel(
                                                                  condition="input.usedata",
                                                                  h5("All information from the dataset selected."),br(),
                                                                  dataTableOutput("data.tab1")))

                                                       )

                                              ),

                                              tabPanel("Upload Data",

                                                       fluidRow(

                                                         column(3,
                                                                wellPanel(
                                                                  fileInput("file","Choose file to upload (csv):",accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                                                  bsPopover("file","Note", "Remember to select the correct data format after uploading file!  Hover over the Select data format panel for more information.",
                                                                            trigger="hover",placement="right"),
                                                                  p("Remember to uncheck sample data to use uploaded data!"),
                                                                  tags$hr(),
                                                                  radioButtons("datformat", strong("Select data format:"), choices=c("1-sample"=1,Stacked=2,Unstacked=3), selected=1),
                                                                  bsPopover("datformat","Data format", "Select Stacked for 2-sample with explanatory and response variables in two columns.  Select Unstacked with explanatory variable as column names and response variable in two columns",
                                                                            trigger="hover",placement="right"),
                                                                  tags$hr(),
                                                                  strong("Customize file format:"),
                                                                  checkboxInput("header", "Header", TRUE),
                                                                  radioButtons("sep", "Separator:", choices=c(Comma=",",Semicolon=";",Tab="\t"), selected=","),
                                                                  radioButtons("quote", "Quote", choices=c(None="","Double Quote"='"',"Single Quote"="'"),selected=""),
                                                                  br(),br(),br())),

                                                         column(9,
                                                                conditionalPanel(
                                                                  condition="input.file!='NULL'",
                                                                  dataTableOutput("data.tab"))))

                                              ),

                                              tabPanel("Visualize Data",
                                                       
                                                       fluidRow(
                                                         column(7,
                                                              br(),
                                                              plotOutput("datagraph")),
                                                         column(7,
                                                                br(),br(),
                                                                p(strong("Summary")),
                                                                tableOutput("summarystats"))))

                                            )),
                                   

                                   tabPanel("Hypothesis Test",
                                            
                                            fluidRow(
                                              column(3,
                                                     wellPanel(
                                                       conditionalPanel(
                                                         condition="input.datformat==1 && input.sampdat==1",
                                                         h4("Hypotheses:"),
                                                         uiOutput("hypo1"),
                                                         tags$hr(),
                                                         numericInput("null1", label="Hypothesized value:", value=0),
                                                         selectInput("alt1", "Select a direction for Ha:", choices=list("two-sided","less than","greater than"),selected="two-sided")),
                                                       conditionalPanel(
                                                         condition="input.datformat!=1 || input.sampdat==2",
                                                         h4("Hypotheses:"),
                                                         uiOutput("hypo2"),
                                                         tags$hr(),
                                                         numericInput("null2", label="Hypothesized value:", value=0),
                                                         selectInput("alt2", label="Select a direction for Ha:", choices=list("two-sided","less than","greater than"),selected="two-sided")),
                                                       sliderInput("alpha", label=HTML("Significance level &alpha;:"), value=.05, max=1, min=0, step=.01),
                                                       tags$hr(),
                                                       actionButton("teststart", "Execute t-Test"),
                                                       br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                                       )),
                                              
                                              column(9,
                                                     br(),br(),
                                                     conditionalPanel(
                                                       condition="input.teststart>0",
                                                       column(10,
                                                              plotOutput("tdist"),
                                                              bsPopover("tdist","p-value","The p-value is the shaded region. A large p-value indicates to fail to reject Ho and no evidence for Ha.  A small p-value indicates to reject the Ho and evidence for Ha.",
                                                                        trigger="hover",placement="left"),br()),
                                                       column(5,br(),
                                                              strong("Test output:"),
                                                              tableOutput("test"),br(),
                                                              strong("Point estimate(s):"),
                                                              uiOutput("est"),br(),br(),
                                                              strong("Confidence interval:"),
                                                              tableOutput("citab"),
                                                              bsPopover("citab","Confidence interval sample interpretation","We are 95% confident that the true parameter is anywhere between the lower bound to the upper bound.",
                                                                        trigger="hover",placement="bottom"))))))
                                            
                                            

                                 ) # tabsetPanel

                               ) # mainPanel


                       ), # tabPanel Hipothesis Testing
                      
                      
                      
                      ####################################################################################################
                      ##                                  GENERALIZED LINEAR MODELS                                     ##
                      ####################################################################################################                      
                      tabPanel("OtherTopic", "This panel is intentionally left blank")
                      
                      
                      
          ) # navbarPage
          
) # fluidPage
