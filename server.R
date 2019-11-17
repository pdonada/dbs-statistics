###############################################
##
##    Development of Statistical Analytics App
##
##    server.R - Server logic definition
##
###############################################

function(input, output, session) {
  
  ##########################################################################################################
  ###Render Probability navtab 
  ##########################################################################################################
  
  bdbgender <- reactive({ 
    print( paste('start reactive', input$bgender_series))  
    
    withProgress(message = 'Uploading data', value = 0, {
      
      switch(input$bgender_series,  
             
             access = { 
               indicatorFE = 'SH.HIV.ARTC.FE.ZS'  # Access to anti-retroviral drugs - Male 
               indicatorMA = 'SH.HIV.ARTC.MA.ZS'  # Access to anti-retroviral drugs - Female 
             }, 
             
             progression =  { 
               indicatorFE = 'SE.SEC.PROG.FE.ZS'  # Progression to secondary school - Male 
               indicatorMA = 'SE.SEC.PROG.MA.ZS'  # Progression to secondary school - Female 
             },  
             
             cause =   { 
               indicatorFE = 'SH.DTH.INJR.1534.FE.ZS'  # Cause of death by injury - ages 15-34 - Male 
               indicatorMA = 'SH.DTH.INJR.1534.MA.ZS'  # Cause of death by injury - ages 15-34 - Female 
             }
      )
      
      print(paste('indicator',indicatorMA,'syear', startYear, 'eyear'=endYear))     
      bdataset_genderM =  WDI(indicator = indicatorMA, country = countries , start = startYear, end = endYear)
      bdataset_genderM <- bdataset_genderM %>% na.omit()  # ignore lines with missing information
      print(paste('nro col M', nrow(bdataset_genderM) ))    
      bdataset_genderF =  WDI(indicator = indicatorFE, country = countries , start = startYear, end = endYear)
      bdataset_genderF <- bdataset_genderF %>%  na.omit()  # ignore lines with missing information
      print(paste('nro col F', nrow(bdataset_genderF))) 
      
      print( paste('end reactive', nrow( data.frame(bdataset_genderM, bdataset_genderF) )))
      # group both datasets
      data.frame(bdataset_genderM, bdataset_genderF)
    })
    
  })  # reactive function - series
  
  
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
  }) # observe type of model
  
  
  observe({ 
    
    if (input$bgender_series == "") {
      return (NULL)
    } 
    print(paste( 'oberserve - list of countries', input$bgender_series)) 
    
    dfgender <- data.frame(bdbgender())
    
    switch(input$bgender_series,  
           
           access = { 
             # list of unique countries in the dataset
             bdbgender_country <- dfgender$country %>% unique()
           }, 
           
           progression =  { 
             # list of unique countries in the dataset
             bdbgender_country <- dfgender$country %>% unique()
           },  
           
           cause =   { 
             # list of unique countries in the dataset
             bdbgender_country <- dfgender$country %>% unique()
           }
    )
    
    updateSelectInput(session, "bgender_country"
                      , choices = bdbgender_country )
    
    print('end oberserve - list of countries')  
    
  }) # observe list of countries
  
  
  observe({ 
    
    if (input$bgender_country == "") {
      return (NULL)
    } 
    print(paste('oberserve - list of years', input$bgender_series , 'country', input$bgender_country))  
    
    dfgender <- data.frame(bdbgender())
    
    switch(input$bgender_series,  
           
           access = {
             print('*** access')
             # list of unique year in the dataset
             bdbgender_year <- dfgender[ dfgender$country == input$bgender_country, ]$year %>% unique()
           }, 
           
           progression =  { 
             print('*** progression')
             # list of unique year in the dataset
             bdbgender_year <- dfgender[ dfgender$country == input$bgender_country, ]$year %>% unique()
           },  
           
           cause =   { 
             print('*** cause')
             # list of unique year in the dataset
             bdbgender_year <- dfgender[ dfgender$country == input$bgender_country, ]$year %>% unique()
           }
    )    
    
    updateSelectInput(session, "bgender_year"
                      , choices = bdbgender_year )
    print('end oberserve - list of years') 
    
  })  # observe list of years
  
  
  # table tab
  output$tabProb <- DT::renderDataTable( {
    
    if (input$dismodel == 'binomial') { 
      if (input$dataset == 'Gender statistics'){
        
        print('Generating table')        
        switch(input$bgender_series,  
               
               access = { 
                 dfgender <- data.frame(bdbgender()) %>% 
                   select(country, year, SH.HIV.ARTC.MA.ZS, SH.HIV.ARTC.FE.ZS)
                 #  %>%   rename( replace = c('SH.HIV.ARTC.MA.ZS' = 'Male(%)') ) #, SH.HIV.ARTC.FE.ZS = Female)
               }, 
               
               progression =  { 
                 dfgender <- data.frame(bdbgender()) %>% 
                   select(country, year, SE.SEC.PROG.MA.ZS, SE.SEC.PROG.FE.ZS)
                 #  %>%   rename( replace = c('SH.HIV.ARTC.MA.ZS' = 'Male(%)') ) #, SH.HIV.ARTC.FE.ZS = Female)                 
               },  
               
               cause =   { 
                 dfgender <- data.frame(bdbgender()) %>% 
                   select(country, year, SH.DTH.INJR.1534.MA.ZS, SH.DTH.INJR.1534.FE.ZS)
                 #  %>%   rename( replace = c('SH.HIV.ARTC.MA.ZS' = 'Male(%)') ) #, SH.HIV.ARTC.FE.ZS = Female)                 
               }
        )
        
      }
      
    }else { "Not available"}
    
  }) # output$tabProb 
  
  output$txtplotProb <- renderPrint({
    
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
    
  } )  # output$txtplotProb
  
  
  # plot tab
  output$plotProb <- renderPlot({ 
    print('Starting PLOT')    
    # binomial  - discrete model
    if (input$dismodel == 'binomial') { 
      
      validate(
        probability_validation(input$n, 'n', input$dismodel)
      )
      
      par(mfrow=c(2,2)) 
      
      if (input$dataset == 'Gender statistics'){ 
        
        dfgender <- data.frame(bdbgender())
        
        switch(input$bgender_series,  
               
               access = { 
                 #### female
                 probf <- dfgender[ dfgender$country == input$bgender_country & dfgender$year == input$bgender_year, ]$SH.HIV.ARTC.FE.ZS
                 #### male
                 probm <- dfgender[ dfgender$country == input$bgender_country & dfgender$year == input$bgender_year, ]$SH.HIV.ARTC.MA.ZS
               }, 
               
               progression =  { 
                 #### female
                 probf <- dfgender[ dfgender$country == input$bgender_country & dfgender$year == input$bgender_year, ]$SE.SEC.PROG.FE.ZS
                 #### male
                 probm <- dfgender[ dfgender$country == input$bgender_country & dfgender$year == input$bgender_year, ]$SE.SEC.PROG.MA.ZS
               },  
               
               cause =   { 
                 #### female
                 probf <- dfgender[ dfgender$country == input$bgender_country & dfgender$year == input$bgender_year, ]$SH.DTH.INJR.1534.FE.ZS
                 #### male
                 probm <- dfgender[ dfgender$country == input$bgender_country & dfgender$year == input$bgender_year, ]$SH.DTH.INJR.1534.MA.ZS
               }
        )
        print('Calculating FEMALE probability')            
        #### female
        if ( !is.null(probf) ){ 
          probf <- as.numeric(probf / 100)
          binomial_plot (input$s,input$n,probf,'Female')
        }
        print('Calculating MALE probability')            
        #### male
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
    
  })    # output$plotProb
  
  
  ##########################################################################################################
  ###Render Data Display navtab
  ##########################################################################################################

  output$view1_pd <- DT::renderDataTable({
    inFile_pd <- input$file1
    
    if (is.null(inFile_pd))
      return(NULL)
    
    file_read_pd <- read.csv(inFile_pd$datapath, header = input$header,
                              sep = input$sep, quote = input$quote)
    v_data_pd <- na.omit(file_read_pd)
  })  
  
  #TAB: Summary - Summary
  output$view2_pd <- renderPrint({
    inFile_pd <- input$file1
    
    if (is.null(inFile_pd))
      return(NULL)
    
    file_read_pd <- read.csv(inFile_pd$datapath, header = input$header,
                              sep = input$sep, quote = input$quote)
    v_data_pd <- na.omit(file_read_pd)
    
    summary(v_data_pd)
  })
  
  #TAB:  Summary - Head
  output$view3_pd <- renderTable({
    inFile_pd <- input$file1
    
    if (is.null(inFile_pd))
      return(NULL)
    
    file_read_pd <- read.csv(inFile_pd$datapath, header = input$header,
                             sep = input$sep, quote = input$quote)
    v_data_pd <- na.omit(file_read_pd)
    
    head(v_data_pd)
  })
  
  #TAB: Country by mean
  output$view4_pd <- DT::renderDataTable({
    inFile_pd <- input$file1
    
    if (is.null(inFile_pd))
      return(NULL)
    
    file_read_pd <- read.csv(inFile_pd$datapath, header = input$header,
                             sep = input$sep, quote = input$quote)
    v_data_pd <- na.omit(file_read_pd)
    
    #grouping by country/mean
    v_countrym_pd <- aggregate(v_data_pd[, 4], list(v_data_pd$country_name), mean, 0)
  })
  
  #TAB: Top 10
  output$view5_pd <- DT::renderDataTable({
    inFile_pd <- input$file1
    
    if (is.null(inFile_pd))
      return(NULL)
    
    file_read_pd <- read.csv(inFile_pd$datapath, header = input$header,
                             sep = input$sep, quote = input$quote)
    v_data_pd <- na.omit(file_read_pd)
    
    #grouping by country/mean
    v_countrym_pd <- aggregate(v_data_pd[, 4], list(v_data_pd$country_name), mean, 0)
    v_countryorder_pd <- v_countrym_pd[with(v_countrym_pd,order(-x)),]
    v_countryorder_pd <- v_countryorder_pd[1:10,]
  })
  
  #TAB: Table Config
  output$view6_pd <- DT::renderDataTable({
    inFile_pd <- input$file1
    
    if (is.null(inFile_pd))
      return(NULL)
    
    file_read_pd <- read.csv(inFile_pd$datapath, header = input$header,
                             sep = input$sep, quote = input$quote)
    v_data_pd <- na.omit(file_read_pd)
    
    if (input$gdp_country_pd != "All" & input$gdp_year_pd != "All") {
      gdp_filter_pd <- v_data_pd[v_data_pd$country_name == input$gdp_country_pd & 
                                   v_data_pd$year == input$gdp_year_pd,]
    }else if (input$gdp_country_pd != "All" & input$gdp_year_pd == "All") {
      gdp_filter_pd <- v_data_pd[v_data_pd$country_name == input$gdp_country_pd,]
    }else if (input$gdp_country_pd == "All" & input$gdp_year_pd != "All") {
      gdp_filter_pd <- v_data_pd[v_data_pd$year == input$gdp_year_pd,]
    }else {gdp_filter_pd <- v_data_pd
    }
    
})
  
  ##TAB: Top Boxplot
  output$view7_pd <- renderPlot({
    inFile_pd <- input$file1
    
    if (is.null(inFile_pd))
      return(NULL)
    
    file_read_pd <- read.csv(inFile_pd$datapath, header = input$header,
                             sep = input$sep, quote = input$quote)
    v_data_pd <- na.omit(file_read_pd)
    
    #grouping by country/mean
    v_countrym_pd <- aggregate(v_data_pd[, 4], list(v_data_pd$country_name), mean, 0)
    v_countryorder_pd <- v_countrym_pd[with(v_countrym_pd,order(-x)),]
    v_countryorder_pd <- v_countryorder_pd[1:10,]

})
  
  #Creating choices for Country and Year
  observe({ 
    inFile_pd <- input$file1
    if (is.null(inFile_pd))
      return(NULL)
    file_read_pd <- read.csv(inFile_pd$datapath, header = input$header,
                             sep = input$sep, quote = input$quote)
    v_data_pd <- na.omit(file_read_pd)
    
    countries_pd <- data.frame(v_data_pd$country_name %>% unique())
    all_c_pd <- data.frame("All")
    names(all_c_pd)<-names(countries_pd)
    countries_pd2 <- rbind(all_c_pd, countries_pd)
    
    years_pd <- data.frame(v_data_pd$year %>% unique())
    years_pd2 <- rbind(c("All"), years_pd)
    
    updateSelectInput(session, "gdp_country_pd", 
                      choices = countries_pd2)
    updateSelectInput(session, "gdp_year_pd", 
                      choices = years_pd2)  
  })
  
  ##########################################################################################################
  ###Render HYPOTHESIS TESTING navtab
  ##########################################################################################################
  
  
  
  ##########################################################################################################
  ###Render GENERALIZED LINEAR MODELS navtab
  ##########################################################################################################  
  
  
  
}