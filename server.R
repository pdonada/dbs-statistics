###############################################
##
##    Development of Statistical Analytics App
##
##    server.R - Server logic definition
##
###############################################

binomial_plot <- function (i_population, i_trials, i_prob, i_gender) {
  d <- density( rbinom( n = i_population, size = i_trials, prob = i_prob) )  # simulation  (s = number of observations ) 
  x <- 0:i_trials  
  y <- dbinom(x,i_trials,i_prob)  # pmf
  
  plot(d, main="Kernel Density of generated data", sub = i_gender) 
  polygon(d, col="red", border="blue") 
  
  plot(x, y , main = "Probability mass function", sub = i_gender, xlab= "Number of trials", ylab= "Probability")  # pmf
  
}


function(input, output, session) {
  
  observe({ 
    switch(input$distype,
           discrete = {  
             updateSelectInput(session, "dismodel", 
                               choices = selectDiscreteDist  # from global.R
             ) 
           }, 
           
           continuous = {  
             updateSelectInput(session, "dismodel", 
                               choices = selectContinuousDist # from global.R
             )
           }
    ) 
  })
  
  
  observe({ 
    
    # list of unique year in the dataset
    bdbgender_year <- bdbgender[ bdbgender$Country.Name == input$bgender_country, ]$Time %>% unique()
    
    updateSelectInput(session, "bgender_year", 
                      choices = bdbgender_year )
    
  })
  
  
  # library(shinysky)
  #  observe({
  #    if(input$dismodel == "binomial"){
  #      if(input$n <= 0 ){
  #        showshinyalert(session,"shinyalert6",
  #                       "Looks like something is wrong in your parameters, please check them and then hit submit again",
  #                       "danger")
  #      }else return()
  #    }else return()
  #  })
  
  
  # table tab
  output$tab <- DT::renderDataTable( {
    
    if (input$dismodel == 'binomial') { 
      if (input$dataset == 'Gender statistics'){
        
        bdbgender  # from global.R
      }
      
    }else { "Not available"}
    
  }) 
  
  output$txtplot <- renderPrint({
    
    # binomial  - discrete model
    if (input$dismodel == 'binomial') { 
      if (input$dataset == 'Gender statistics'){ 
        
        switch(input$bgender_series,  
               
               access = { print('Lets check the probability of accessing anti-retroviral drugs by gender.') }, 
               
               progression =  { print('Lets check the probability of progressing to secondary school by gender.') },  
               
               cause =   { print('Lets check the probability that the cause of death is injury by gender.') }
        )
        
      }
    }
    
  } )
  
  
  # plot tab
  output$plot <- renderPlot({ 
    
    # binomial  - discrete model
    if (input$dismodel == 'binomial') { 
      
      validate(
        probability_validation(input$n, 'n', input$dismodel)
      )
      
      par(mfrow=c(2,2)) 
      
      if (input$dataset == 'Gender statistics'){ 
        
        
        switch(input$bgender_series,  
               
               access = { colFemale = 5
               colMale   = 6 }, 
               
               progression =  { colFemale = 7
               colMale   = 8 },  
               
               cause =   { colFemale = 9
               colMale   = 10 }
        )
        
        
        #### female
        probf <- bdbgender[ bdbgender$Country.Name == input$bgender_country & bdbgender$Time == input$bgender_year, ][colFemale]
        if ( !is.null(probf) ){ 
          probf <- as.numeric(probf / 100)
          binomial_plot (input$s,input$n,probf,'Female')
        }
        
        #### male
        probm <- bdbgender[ bdbgender$Country.Name == input$bgender_country & bdbgender$Time == input$bgender_year, ][colMale]
        if ( !is.null(probm) ){ 
          probm <- as.numeric(probm / 100)
          binomial_plot (input$s,input$n,probm,'Male') 
        }
        
      }
      
    } 
    
    # poisson - discrete model
    if (input$dismodel == 'poisson') { 
      
      validate(
        probability_validation(input$lam, 'lambda', input$dismodel),
        probability_validation(input$lam2, 'lambda 2', input$dismodel),
        probability_validation(input$lam3, 'lambda 3', input$dismodel),
        probability_validation(input$max, 'x', input$dismodel)
        
      )      
      
      par(mfrow=c(1,2))   
      # simulation (model the number of expected events concurring within a specific time window)
      D = rpois(input$s    # Number of observations you want to see
                , input$lam  # Estimated rate of events for the distribution; this is expressed as average events per period
      )    
      tab = table(D)  
      barplot(tab, col = 'blue', main="Distribution", xlab="Events" , ylab= "Count")  
      
      x1 = 0:input$max  
      y1 = dpois(x1,input$lam)          # pmf
      plot(x1, y1, type='b', main = "Probability mass function" , ylab = "Probability" , xlab = "Events", col="blue")  
      
      # example https://towardsdatascience.com/the-poisson-distribution-and-poisson-process-explained-4e2cb17d459
      
      if (input$lam2 > 0 ){
        # Add second curve to the same plot by calling points() and lines()
        # Use symbol '*' for points.
        y2 = dpois(x1,input$lam2)          # pmf
        points(x1, y2, col="red", pch="*")
        lines (x1, y2, col="red", lty=2)
      }
      
      if (input$lam3 > 0 ){
        # Add Third curve to the same plot by calling points() and lines()
        # Use symbol '+' for points.
        y3 = dpois(x1,input$lam3)          # pmf
        points(x1, y3, col="green",pch="+")
        lines (x1, y3, col="green", lty=3)
      }
      
    } 
    
    # geometric - discrete model
    if (input$dismodel == 'geometric') { 
      
      validate(
        probability_validation(input$probg, 'p', input$dismodel),
        probability_validation(input$max, 'x', input$dismodel)
      )        
      
      
      par(mfrow=c(1,2)) 
      D=rgeom(input$s, input$probg)   # simulation
      tab=table(D)  
      barplot(tab, col='blue', main="Distribution", xlab="Events" , ylab= "Count")
      
      x2=0:input$max  
      y2=dgeom(x2,input$probg)      # pmf
      plot(x2, y2, type='b', main = "Probability mass function" , ylab = "Probability" , xlab = "Number of trials", col="blue" )  
    }
    
    # normal  - continuous model
    if (input$dismodel == 'normal') { 
      
      validate(
        probability_validation(input$mu, 'mu', input$dismodel),
        probability_validation(input$sigma, 'sigma', input$dismodel),
        probability_validation(input$max, 'x', input$dismodel)
      )       
      
      par(mfrow=c(1,2))  
      x= seq(-input$max,input$max,0.1)
      
      y= dnorm(x,input$mu,input$sigma)  # pdf 
      plot(x, y, type='l', main = "Probability density function" , xlab = "" , ylab = "Density", col='red') 
      
      
      y1= pnorm(x,input$mu,input$sigma)  # cdf 
      plot(x, y1, type='l', main = 'Cumulative density function' , xlab = "" , ylab = "Cumulative probability", col='red')  
    } 
    
    # exponential  - continuous model
    if (input$dismodel == 'exponential') { 
      
      validate(
        probability_validation(input$lambda, 'lambda', input$dismodel)
      )   
      
      par(mfrow=c(1,2))  
      x=seq(0,input$s,0.1)
      
      y=  dexp(x,input$lambda)  # pdf 
      plot(x, y, type='l', main = "Probability density function" , xlab = "" , ylab = "Density", col='red') 
      
      y1= pexp(x,input$lambda)  # cdf 
      plot(x, y1, type='l', main = "Cumulative density function" , xlab = "" , ylab = "Cumulative probability",col='red')  
    } 
    
  })   
  
  #############################
  ###Render Data Display navtab
  output$view1_pd <- DT::renderDataTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })  
  
  # Generate a summary of the dataset ----
  output$view2_pd <- renderPrint({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    dataset <- read.csv(inFile$datapath, header = input$header,
                        sep = input$sep, quote = input$quote)
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  output$view3_pd <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    dataset <- read.csv(inFile$datapath, header = input$header,
                        sep = input$sep, quote = input$quote)
    head(dataset)
  })
  
  # Show the first "n" observations ----
  output$view4_pd <- DT::renderDataTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    v_file1 <- read.csv(inFile$datapath, header = input$header,
                        sep = input$sep, quote = input$quote)
    
    #exclude NA from dataset
    v_data <- na.omit(v_file1)
    
    #grouping by country/mean
    v_countrym <- aggregate(v_data[, 4], list(v_data$country_name), mean, 0)
  })
  
  output$view5_pd <- DT::renderDataTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    v_file1 <- read.csv(inFile$datapath, header = input$header,
                        sep = input$sep, quote = input$quote)
    
    #exclude NA from dataset
    v_data <- na.omit(v_file1)
    
    #grouping by country/mean
    v_countrym <- aggregate(v_data[, 4], list(v_data$country_name), mean, 0)
    
    v_countryorder <- v_countrym[with(v_countrym,order(-x)),]
    v_countryorder <- v_countryorder[1:10,]
  })
  
  output$view6_pd <- DT::renderDataTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    v_file1 <- read.csv(inFile$datapath, header = input$header,
                        sep = input$sep, quote = input$quote)
    
    #exclude NA from dataset
    v_data <- na.omit(v_file1)
    
    # Change values for input$inSelect
    s_options <- list()
    v_year <- input$year
    updateSelectInput(session, "inSelect",
                      choices = s_options,
                      selected = paste0("option-", v_year, "-A")
    )
    ?list
    
  })
  

}
