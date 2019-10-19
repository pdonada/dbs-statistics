library(shiny) 



shinyUI(pageWithSidebar( 
  
  headerPanel("Interactive plots"), 
  
  sidebarPanel( 
    
    sliderInput("s", "number of simulated data" ,min=1, max=1000, value = 10), 
    
    numericInput("lam", "parameter lambda in exponential" , value = 1), 
    
    
    
    numericInput("mu", "parameter mu in Normal" , value = 0), 
    
    numericInput("sigma", "parameter sigma in Normal" , value = 1),  
    
    numericInput("i", "support" , value = 0),  
    
    numericInput("j1", "j in exponential" , value = 0), 
    
    numericInput("j2", "j in Normal" , value = 0) 
    
  ), 
  
  mainPanel( 
    
    tabsetPanel(type = "tabs", 
                
                tabPanel("Plot", plotOutput("plot")), 
                
                tabPanel("Exp prob",verbatimTextOutput("prob")), 
                
                tabPanel("Table", tableOutput("tab")) 
                
                
                
                
                
    )  
    
    
    
  ) 
  
)) 