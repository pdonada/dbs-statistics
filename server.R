###############################################
##
##    Development of Statistical Analytics App
##
##    server.R - Server logic definition
##
###############################################

function(input, output, session) {
  
  ##########################################################################################################
  ### Render Probability navtab 
  ###load the database selected 
  ##########################################################################################################
  bdbgender <- reactive({ 
    print( paste('**start reactive', input$bgender_series,'tab -', input$tabs))  
    
    if (input$dismodel != 'binomial'){
      print('end reactive')
      return(NULL)
    }
    
    if (input$bgender_series == ''){
      print('end reactive')
      return(NULL)
    }
    
    if (input$tabs != 'Probability'){
      print('end reactive')
      return(NULL)
    }
    
    withProgress(message = 'Uploading data', value = 0, {
      
      switch(input$bgender_series,  
             
             access = { 
               
               # Increment the progress bar, and update the detail text.
               incProgress(1/4)
               
               indicatorFE = 'SH.HIV.ARTC.FE.ZS'  # Access to anti-retroviral drugs - Male 
               indicatorMA = 'SH.HIV.ARTC.MA.ZS'  # Access to anti-retroviral drugs - Female 
             }, 
             
             progression =  { 
               
               # Increment the progress bar, and update the detail text.
               incProgress(1/4)
               
               indicatorFE = 'SE.SEC.PROG.FE.ZS'  # Progression to secondary school - Male 
               indicatorMA = 'SE.SEC.PROG.MA.ZS'  # Progression to secondary school - Female 
             },  
             
             cause =   { 
               
               # Increment the progress bar, and update the detail text.
               incProgress(1/4)
               
               indicatorFE = 'SH.DTH.INJR.1534.FE.ZS'  # Cause of death by injury - ages 15-34 - Male 
               indicatorMA = 'SH.DTH.INJR.1534.MA.ZS'  # Cause of death by injury - ages 15-34 - Female 
             },
             
             measles =   { 
               
               # Increment the progress bar, and update the detail text.
               incProgress(1/2)
               
               indicatorMEAS = 'SH.IMM.MEAS'  # Immunization, measles (% of children ages 12-23 months)
               print(paste('indicator MEAS',indicatorMEAS,'syear', startYear, 'eyear'=endYear))     
               
               # Increment the progress bar, and update the detail text.
               incProgress(1/2)
               
               bdataset_populationMEAS =  WDI(indicator = indicatorMEAS, country = countries , start = startYear, end = endYear)
               bdataset_populationMEAS <- bdataset_populationMEAS %>% na.omit()  # ignore lines with missing information
               print(paste('nro col MEAS', nrow(bdataset_populationMEAS) ))  
               
               print( 'end reactive (population 1)')
               return(bdataset_populationMEAS)
             },
             
             sanitation =   { 
               
               # Increment the progress bar, and update the detail text.
               incProgress(1/5)
               
               indicatorUR = 'SH.STA.SMSS.UR.ZS'  # People using safely managed sanitation services, urban (% of urban population)
               indicatorRU = 'SH.STA.SMSS.RU.ZS'  # People using safely managed sanitation services, rural (% of rural population)
               
               print(paste('indicator UR',indicatorUR,'syear', startYear, 'eyear'=endYear))     
               
               # Increment the progress bar, and update the detail text.
               incProgress(1/5)
               
               bdataset_populationUR =  WDI(indicator = indicatorUR, country = countries , start = startYear, end = endYear)
               bdataset_populationUR <- bdataset_populationUR %>% na.omit()  # ignore lines with missing information
               bdataset_populationUR <- bdataset_populationUR %>% 
                 dplyr::select(iso2c, country, year, SH.STA.SMSS.UR.ZS)
               print(paste('nro col UR', nrow(bdataset_populationUR) ))   
               
               print(paste('indicator RU',indicatorRU,'syear', startYear, 'eyear'=endYear)) 
               
               # Increment the progress bar, and update the detail text.
               incProgress(1/5)
               
               bdataset_populationRU =  WDI(indicator = indicatorRU, country = countries , start = startYear, end = endYear)
               bdataset_populationRU <- bdataset_populationRU %>%  na.omit()  # ignore lines with missing information
               bdataset_populationRU <- bdataset_populationRU %>% 
                 dplyr::select(iso2c, country, year, SH.STA.SMSS.RU.ZS)
               print(paste('nro col RU', nrow(bdataset_populationRU))) 
               
               # Increment the progress bar, and update the detail text.
               incProgress(1/5)
               
               # group both datasets
               bdataset <- NULL
               for (row in 1:nrow(bdataset_populationRU)) {
                 flag <- FALSE
                 row_ur <- 1
                 while (!flag & row_ur <= nrow(bdataset_populationUR) ) {
                   if (bdataset_populationUR[row_ur, "iso2c"] == bdataset_populationRU[row, "iso2c"] &
                       bdataset_populationUR[row_ur, "year"] == bdataset_populationRU[row, "year"] ) {
                     flag <- TRUE
                     
                   }else { row_ur <- row_ur + 1 }
                 }
                 # if found the country on the Urban database add to the list
                 if (flag) {
                   bdataset <- rbind(bdataset,
                                     c("iso2c" = bdataset_populationRU[row, "iso2c"]
                                       ,"country" = bdataset_populationRU[row, "country"]
                                       ,"year" = bdataset_populationRU[row, "year"]
                                       ,"SH.STA.SMSS.UR.ZS" = bdataset_populationUR[row_ur, "SH.STA.SMSS.UR.ZS"]
                                       ,"SH.STA.SMSS.RU.ZS" = bdataset_populationRU[row, "SH.STA.SMSS.RU.ZS"] ))
                   
                 }
               } # loop
               
               # Increment the progress bar, and update the detail text.
               incProgress(1/5)
               
               print( paste('end reactive (population 2)', nrow(bdataset) ))
               return (bdataset)  # return the grouped dataset
             }  # sanitation services
      )  # switch
      
      if ( input$bdataset == 'gender' ){
             
          # Increment the progress bar, and update the detail text.
          incProgress(1/4)
        
          print(paste('indicator MA',indicatorMA,'syear', startYear, 'eyear'=endYear))     
                  
          bdataset_genderM =  WDI(indicator = indicatorMA, country = countries , start = startYear, end = endYear)
          bdataset_genderM <- bdataset_genderM %>% na.omit()  # ignore lines with missing information
          print(paste('nro col M', nrow(bdataset_genderM) ))   
                  
          # Increment the progress bar, and update the detail text.
          incProgress(1/4)
          
          print(paste('indicator FE',indicatorFE,'syear', startYear, 'eyear'=endYear)) 
          bdataset_genderF =  WDI(indicator = indicatorFE, country = countries , start = startYear, end = endYear)
          bdataset_genderF <- bdataset_genderF %>%  na.omit()  # ignore lines with missing information
          print(paste('nro col F', nrow(bdataset_genderF))) 
            
          # Increment the progress bar, and update the detail text.
          incProgress(1/4)      
          print( paste('end reactive (gender)', nrow( data.frame(bdataset_genderM, bdataset_genderF) )))
          # group both datasets
          return (data.frame(bdataset_genderM, bdataset_genderF))
      }
      
    })
    
  })  # reactive function - series
  
  ##################################################
  ## PROBABILITY MODELS
  ## Show the list of models available as per
  ## type of distribution selected
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
  
  ##################################################
  ## PROBABILITY MODELS
  ## Show the list of series available as per
  ## model selected
  observe({ 
 
    print(paste( 'observe - list of series', input$bdataset)) 
    
    switch(input$bdataset,  
           
           gender = { 
             bdbseries <- bdbgender_series
           }, 
           
           population =  { 
             bdbseries <- bdbpopulation_series
           }
    )
    
    updateSelectInput(session, "bgender_series"
                     , choices = bdbseries )
    
    print('end observe - list of series')  
    
  }) # observe list of series
  
  
  ##################################################
  ## PROBABILITY MODELS
  ## Show the list of countries available as per
  ## model/serie selected  
  observe({ 
    
    if (input$bgender_series == "") {
      return (NULL)
    } 
    print(paste( 'observe - list of countries', input$bgender_series)) 
    
    dfgender <- data.frame(bdbgender())
    # list of unique countries in the dataset
    bdbgender_country <- dfgender$country %>% unique()
    
    updateSelectInput(session, "bgender_country"
                      , choices = bdbgender_country )
    
    print('end observe - list of countries')  
    
  }) # observe list of countries
  
  ##################################################
  ## PROBABILITY MODELS
  ## Show the list of years available as per
  ## model/serie/country selected
  observe({ 
    
    if (input$bgender_country == "") {
      return (NULL)
    } 
    print(paste('observe - list of years', input$bgender_series , 'country', input$bgender_country))  
    
    dfgender <- data.frame(bdbgender())
    # list of unique year in the dataset
    bdbgender_year <- dfgender[ dfgender$country == input$bgender_country, ]$year %>% unique()
    
    updateSelectInput(session, "bgender_year"
                      , choices = bdbgender_year )
    print('end observe - list of years') 
    
  })  # observe list of years
  
  
  ##################################################
  ## PROBABILITY MODELS
  ## build a table to show the full dataset selected
  output$tabProb <- DT::renderDataTable( {
  
    if (input$dismodel == 'binomial') { 
      print(paste('Generating table -', input$bgender_series)) 
      
      if (input$bdataset == 'gender'){
        
        switch(input$bgender_series,  
               
               access = { 
                 dfgender <- data.frame(bdbgender()) %>% 
                   dplyr::select(country, year, SH.HIV.ARTC.MA.ZS, SH.HIV.ARTC.FE.ZS)  %>%   
                   rename( c("country" = "Country", "year" = "Year","SH.HIV.ARTC.MA.ZS" = "Male(%)", "SH.HIV.ARTC.FE.ZS" = "Female(%)") )
               }, 
               
               progression =  { 
                 dfgender <- data.frame(bdbgender()) %>% 
                   dplyr::select(country, year, SE.SEC.PROG.MA.ZS, SE.SEC.PROG.FE.ZS) %>%   
                   rename( c("country" = "Country", "year" = "Year","SE.SEC.PROG.MA.ZS" = "Male(%)", "SE.SEC.PROG.FE.ZS" = "Female(%)") )                
               },  
               
               cause =   { 
                 dfgender <- data.frame(bdbgender()) %>% 
                   dplyr::select(country, year, SH.DTH.INJR.1534.MA.ZS, SH.DTH.INJR.1534.FE.ZS) %>%   
                   rename( c("country" = "Country", "year" = "Year","SH.DTH.INJR.1534.MA.ZS" = "Male(%)", "SH.DTH.INJR.1534.FE.ZS" = "Female(%)") )               
               }
        )
        
      }else {
        
        switch(input$bgender_series,  
               
               measles = { 
                 dfgender <- data.frame(bdbgender()) %>% 
                   dplyr::select(country, year, SH.IMM.MEAS) %>%   
                   rename( c("country" = "Country", "year" = "Year","SH.IMM.MEAS" = "(% of children ages 12-23 months)") )                 
                 
               },
               
               sanitation = {
                 dfgender <- data.frame(bdbgender()) %>% 
                   dplyr::select(country, year, SH.STA.SMSS.UR.ZS, SH.STA.SMSS.RU.ZS) %>%   
                   rename( c("country" = "Country", "year" = "Year","SH.STA.SMSS.UR.ZS" = "(% of urban population)", "SH.STA.SMSS.RU.ZS" = "(% of rural population)") )                  
               }
            ) 
      }
      
    }else { 
      
      data(BP)
      dfgender <- data.frame(BP)
      dfgender %>% 
        dplyr::select(sex, sbp, dbp, saltadd, birthdate)  %>%   
        rename( c("sex" = "Gender"
                  ,"sbp" = "Systolic"
                  ,"dbp" = "Diastolic"
                  ,"saltadd" = "Salt added on table" 
                  ,"birthdate" = "Birth date")
             )
      
    }
    
  }) # output$tabProb 
  
  
  ##################################################
  ## PROBABILITY MODELS
  ## build a table to show the data used to build 
  ## the graphs
  output$tabProbBy <- DT::renderDataTable( {
    
    if (input$dismodel == 'binomial') { 
        print( paste('Generating table by',input$bgender_country, input$bgender_year))
      
        if (input$bgender_country == "" || input$bgender_year == "") {
          return (NULL)
        } 
        
        df <- data.frame(bdbgender())
        df <- df[ df$country == input$bgender_country & df$year == input$bgender_year, ]
   
        switch(input$bgender_series,  
               
               access = { 
                   df %>% 
                   dplyr::select(country, year, SH.HIV.ARTC.MA.ZS, SH.HIV.ARTC.FE.ZS)  %>%   
                   rename( c("country" = "Country", "year" = "Year","SH.HIV.ARTC.MA.ZS" = "Male(%)", "SH.HIV.ARTC.FE.ZS" = "Female(%)") )
               }, 
               
               progression =  { 
                   df %>% 
                   dplyr::select(country, year, SE.SEC.PROG.MA.ZS, SE.SEC.PROG.FE.ZS) %>%   
                   rename( c("country" = "Country", "year" = "Year","SE.SEC.PROG.MA.ZS" = "Male(%)", "SE.SEC.PROG.FE.ZS" = "Female(%)") )                
               },  
               
               cause =   { 
                   df %>% 
                   dplyr::select(country, year, SH.DTH.INJR.1534.MA.ZS, SH.DTH.INJR.1534.FE.ZS) %>%   
                   rename( c("country" = "Country", "year" = "Year","SH.DTH.INJR.1534.MA.ZS" = "Male(%)", "SH.DTH.INJR.1534.FE.ZS" = "Female(%)") )               
               },
               
               measles =   { 
                 df %>% 
                   dplyr::select(country, year, SH.IMM.MEAS) %>%   
                   rename( c("country" = "Country", "year" = "Year", "SH.IMM.MEAS" = "(% of children ages 12-23 months)" ) )               
               },
               
               sanitation =   { 
                 df %>% 
                   dplyr::select(country, year, SH.STA.SMSS.UR.ZS, SH.STA.SMSS.RU.ZS) %>%   
                   rename( c("country" = "Country", "year" = "Year","SH.STA.SMSS.UR.ZS" = "(% of urban population)", "SH.STA.SMSS.RU.ZS" = "(% of rural population)") )               
               }
        )
        
    }else { 
      
      return(NULL)
      
    }
    
  }) # output$tabProbBy 

  ##################################################
  ## PROBABILITY MODELS
  ## build the graphs based on the dataset selected
  output$plotProb <- renderPlot({ 
    
    ####################################################################################    
    # binomial  - discrete model
    if (input$dismodel == 'binomial') { 
      
      print(paste('Starting BINOMIAL PLOT -',input$bgender_series, input$bdataset,input$bgender_country, input$bgender_year))  
      
      if (input$bgender_series == '' || input$bgender_country =='' || input$bgender_year == ''){
        print('end PLOT')
        return(NULL)
      }
      
      validate(
        probability_validation(input$n, 'n', input$dismodel)
      )
      
      dfgender <- data.frame(bdbgender())

      if (input$bdataset == 'gender'){ 
        
        par(mfrow=c(2,2)) 
        probf <- NULL
        probm <- NULL
        
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
        print( paste('Calculating FEMALE probability',probf))            
        #### female
        if ( !is.null(probf) ){ 
          probf <- as.numeric(probf / 100)
          binomial_plot (input$s,input$n,probf,'Female')
        }
        print(paste('Calculating MALE probability',probm))            
        #### male
        if ( !is.null(probm) ){ 
          probm <- as.numeric(probm / 100)
          binomial_plot (input$s,input$n,probm,'Male') 
        }
        
      }else if(input$bdataset == 'population'){
        switch(input$bgender_series,  
               
               measles = { 
                 
                 par(mfrow=c(1,2)) 
                 # Immunization, measles (% of children ages 12-23 months)
                 probMeas <- dfgender[ dfgender$country == input$bgender_country & dfgender$year == input$bgender_year, ]$SH.IMM.MEAS
                 print(paste('Calculating MEASLES probability',probMeas))  
                 if ( !is.null(probMeas) ){ 
                   probMeas <- as.numeric(probMeas) / 100
                   binomial_plot (input$s,input$n,probMeas,'')
                 }
                 
               },
               
               sanitation = {
                 
                 par(mfrow=c(2,2)) 
                 
                 # People using safely managed sanitation services, urban (% of urban population)
                 probUR <- dfgender[ dfgender$country == input$bgender_country & dfgender$year == input$bgender_year, ]$SH.STA.SMSS.UR.ZS
                 # People using safely managed sanitation services, rural (% of rural population)
                 probRU <- dfgender[ dfgender$country == input$bgender_country & dfgender$year == input$bgender_year, ]$SH.STA.SMSS.RU.ZS
                 
   
                 print( paste('Calculating URBAN probability',probUR)) 
                 # urban population
                 if ( !is.null(probUR) ){ 
                   probUR <- as.numeric(as.character(probUR))  # factor to numeric
                   probUR <- round(probUR, digits = 2) /100
                   binomial_plot (input$s,input$n,probUR,'Urban population')
                 }
                 print( paste('Calculating RURAL probability',probRU))       
                 # rural population
                 if ( !is.null(probRU) ){ 
                   probRU <- as.numeric(as.character(probRU))  # factor to numeric
                   probRU <- round(probRU, digits = 2) /100
                   binomial_plot (input$s,input$n,probRU,'Rural population') 
                 }
                 
               }
        )
      }
      
      print('end BINOMIAL PLOT')   
    } 
    
    ####################################################################################
    # normal  - continuous model
    if (input$dismodel == 'normal') { 
      
      print(paste('Starting NORMAL PLOT -',input$norm_mu, input$norm_sigma))  
      
      if (input$norm_sigma == '' || is.na(input$norm_sigma) || input$norm_mu =='' || is.na(input$norm_mu)){
        print('end PLOT')
        return(NULL)
      }
      
      validate(
        probability_validation(input$norm_mu, 'mu', input$dismodel),
        probability_validation(input$norm_sigma, 'sigma', input$dismodel)
      )  
      
      data(BP)
      dfgender <- data.frame(BP)
      
      par(mfrow=c(2,3))
      
      # Systolic blood pressure
      norm_sbp = dfgender$sbp
      
      print('Printing Histogram - Systolic')
      
      hist(norm_sbp, probability=TRUE, main='Histogram of Systolic', xlab = 'x')   # histogram
      norm_seq <- seq( from = min(norm_sbp), to = max(norm_sbp), length = 100)
      lines(norm_seq, dnorm(norm_seq, mean = input$norm_mu, sd = input$norm_sigma), col="blue")
      
      print('Printing PDF')
      
      norm_y= dnorm(norm_seq, mean = input$norm_mu, sd = input$norm_sigma)   # pdf 
      plot(norm_seq, norm_y, type='l', main = "Probability density function - Systolic" , xlab = "" , ylab = "Density", col='red') 
      
      print('Printing CDF')
      norm_y1= pnorm(norm_seq, mean = input$norm_mu, sd = input$norm_sigma)  # cdf 
      plot(norm_seq, norm_y1, type='l', main = 'Cumulative density function - Systolic' , xlab = "" , ylab = "Cumulative probability", col='red')  
    
      # Diastolic blood pressure
      norm_sbp = dfgender$dbp
      
      print('Printing Histogram - Diastolic')
      
      hist(norm_sbp, probability=TRUE, main = 'Histogram of Diastolic')   # histogram
      norm_seq <- seq( from = min(norm_sbp), to = max(norm_sbp), length = 100)
      lines(norm_seq, dnorm(norm_seq, mean = input$norm_mu, sd = input$norm_sigma), col="blue")
      
      print('Printing PDF')
      
      norm_y= dnorm(norm_seq, mean = input$norm_mu, sd = input$norm_sigma)   # pdf 
      plot(norm_seq, norm_y, type='l', main = "Probability density function - Diastolic" , xlab = "" , ylab = "Density", col='red') 
      
      print('Printing CDF')
      norm_y1= pnorm(norm_seq, mean = input$norm_mu, sd = input$norm_sigma)  # cdf 
      plot(norm_seq, norm_y1, type='l', main = 'Cumulative density function - Diastolic' , xlab = "" , ylab = "Cumulative probability", col='red')  
      
      
      print('end NORMAL PLOT')  
    } 
    
    
  })    # output$plotProb
  
  
  ##########################################################################################################
  ###Render Data Display navtab
  ##########################################################################################################
  
  #TAB: Table
#  output$view1_pd <- DT::renderDataTable({
#    inFile_pd <- input$file1
#    
#    if (is.null(inFile_pd))
#      return(NULL)
#    
#    file_read_pd <- read.csv(inFile_pd$datapath, header = input$header,
#                              sep = input$sep, quote = input$quote)
#    v_data_pd <- na.omit(file_read_pd)
#    v_data_pd  %>% 
#       rename ( c("country_name" = "Country" 
#                  , "country_code" = "Country code"
#                  , "year" = "Year"
#                  , "gdp_usd" = "GDP (Millions U$)"
#                  , "latitude" = "Latitude"
#                  , "longitude" = "Longitude") )
#  })  
  
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
    
    print('grouping by country/mean')
    #grouping by country/mean
    v_countrym_pd <- aggregate(v_data_pd[, 4], list(v_data_pd$country_name), mean, 0)
    
    v_countrym_pd <- v_countrym_pd  %>% 
                      rename ( c("Group.1" = "Country"
                                 ,"mean.v_data_pd[, 4]" = "Mean")) 
    
    DT::datatable(v_countrym_pd)  %>% 
      formatCurrency( c('Mean'), currency = '', interval = 3, mark = ',', before = FALSE) 
  })
  
  #TAB: Top 10
  output$view5_pd <- DT::renderDataTable({
    inFile_pd <- input$file1
    
    if (is.null(inFile_pd))
      return(NULL)
    
    file_read_pd <- read.csv(inFile_pd$datapath, header = input$header,
                             sep = input$sep, quote = input$quote)
    v_data_pd <- na.omit(file_read_pd)
    print('top 10 - grouping by country/mean')
    #grouping by country/mean
    v_countrym_pd <- aggregate(v_data_pd[, 4], list(v_data_pd$country_name), mean, 0)
    
    v_countrym_pd <- v_countrym_pd  %>% 
      rename ( c("Group.1" = "Country"
                 ,"mean.v_data_pd[, 4]" = "Mean")) 
    
    v_countryorder_pd <- v_countrym_pd[with(v_countrym_pd,order(- Mean)),]
    v_countryorder_pd <- v_countryorder_pd[1:10,]
    
    DT::datatable(v_countryorder_pd)  %>% 
      formatCurrency(c('Mean'), currency = '', interval = 3, mark = ',', before = FALSE) 
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
    
    gdp_filter_pd  %>% 
      rename ( c("country_name" = "Country" 
                 , "country_code" = "Country code"
                 , "year" = "Year"
                 , "gdp_usd" = "GDP (Millions U$)"
                 , "latitude" = "Latitude"
                 , "longitude" = "Longitude") )
})
  
  ##TAB: Pie Chart
  output$view7_pd <- renderPlot({
    inFile_pd <- input$file1
    
    if (is.null(inFile_pd))
      return(NULL)
    
    file_read_pd <- read.csv(inFile_pd$datapath, header = input$header,
                             sep = input$sep, quote = input$quote)
    v_data_pd <- na.omit(file_read_pd)
    print('top 10 - pie chart')
    #grouping by country/mean
    v_countrym_pd <- aggregate(v_data_pd[, 4], list(v_data_pd$country_name), mean, 0)
    
    v_countrym_pd <- v_countrym_pd  %>% 
      rename ( c("mean.v_data_pd[, 4]" = "x")) 
    
    v_countryorder_pd <- v_countrym_pd[with(v_countrym_pd,order(-x)),]
    v_countryorder_pd <- v_countryorder_pd[1:10,]
    
    lbls <- factor(v_countryorder_pd[ ,1])
    slices <- v_countryorder_pd$x
    slicestril <- prettyNum(c(round(v_countryorder_pd$x/1000)), big.mark = ",", decimal.mark = ".")
    lbls <- paste(lbls, slicestril) 
    #lbls <- paste(lbls,"T",sep="") # ad % to labels
    
    pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Top 10 GDP Countries in Trillions of U$", radius = 1.1)
})
  
  ##TAB: Graphs
  output$view8_pd <- renderPlot({
    inFile_pd <- input$file1
    
    if (is.null(inFile_pd))
      return(NULL)
    
    file_read_pd <- read.csv(inFile_pd$datapath, header = input$header,
                             sep = input$sep, quote = input$quote)
    v_data_pd <- na.omit(file_read_pd)
    print('top 10 - bar chart')
    #grouping by country/mean
    v_countrym_pd <- aggregate(v_data_pd[, 4], list(v_data_pd$country_name), mean, 0)
    
    v_countrym_pd <- v_countrym_pd  %>% 
      rename ( c("mean.v_data_pd[, 4]" = "x")) 
    
    v_countryorder_pd <- v_countrym_pd[with(v_countrym_pd,order(-x)),]
    v_countryorder_pd <- v_countryorder_pd[1:10,]
    
    lbls <- factor(v_countryorder_pd[ ,1])
    slices <- round(v_countryorder_pd$x/1000)
    #slicestril <- prettyNum(c(round(slices)), big.mark = ",", decimal.mark = ".")
    #lbls <- paste(lbls, slicestril) 
    #lbls <- paste(lbls,"T",sep="") # ad % to labels
    head(slices)
    par(las=2) # make label text perpendicular to axis
    par(mar=c(5,9,4,2)) # increase y-axis margin.
    barplot(slices, names.arg = lbls, col=rainbow(length(lbls)), horiz=TRUE, 
            legend=slices, args.legend = list(x = "topright"))
    })    
  
  ##TAB: Heat Map
  output$view9_pd <- renderLeaflet({
    
    inFile_pd <- input$file1
    
    if (is.null(inFile_pd))
      return(NULL)
    
    file_read_pd <- read.csv(inFile_pd$datapath, header = input$header,
                             sep = input$sep, quote = input$quote)
    v_data_pd <- na.omit(file_read_pd)
    data_map <- aggregate(v_data_pd[, 4], by=list(v_data_pd$country_name, v_data_pd$latitude, v_data_pd$longitude), mean, 0)
    colnames(data_map) <- c("country_name", "latitude", "longitude", "gdp_usd")
    
    caz <- data_map$gdp_usd / 40
    qpal <- colorQuantile("YlOrRd", data_map$gdp_usd, n = 4)
    
    leaflet(data_map) %>% 
      setView(lng = -6.2489, lat = 53.3331, zoom = 2)  %>% #setting the view over ~ center of North America
      addTiles() %>% 
      addCircles(data = data_map, lat = data_map$latitude, lng = data_map$longitude, 
                 popup = ~data_map$country_name, radius = 100000, weight = 10,
                 color = ~qpal(gdp_usd), fillOpacity = 1) %>%
      addLegend("bottomright", pal = qpal, values = ~gdp_usd, 
                title = "GDP - Mean", opacity = 1)
    
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
    colnames(countries_pd2) <- c("Countries")
    
    years_pd <- data.frame(v_data_pd$year %>% unique())
    years_pd2 <- rbind(c("All"), years_pd)
    colnames(years_pd2) <- c("Years")
    
    updateSelectInput(session, "gdp_country_pd", 
                      choices = countries_pd2)
    updateSelectInput(session, "gdp_year_pd", 
                      choices = years_pd2)  
  })
  
 ##########################################################################################################
  ###Render HYPOTHESIS TESTING navtab
  ##########################################################################################################
  

  data <- reactive({
    if(is.null(input$file) & !input$usedata)
    {
      return(NULL)
      
    } else if(!is.null(input$file) & !input$usedata)
    {
      file = read.csv(input$file$datapath, header=input$header, sep=input$sep, quote=input$quote)
      return(file)
      
    } else if(input$sampdat==1 & input$usedata)
    {
      return(data.frame(level=UKgas))
    
    } else if(input$sampdat==2 & input$usedata)
    {
      
      pop_data = na.omit(flights) %>%
        dplyr::filter(carrier == 'UA' | carrier == 'DL', arr_delay >= 0) %>%
        dplyr::select(carrier, arr_delay) %>%
        dplyr::group_by(carrier) %>%
        dplyr::sample_n(10000) %>%
        dplyr::ungroup()

      pop_data$carriercoded[which(pop_data$carrier=='UA')] = "Delta Airlines"
      pop_data$carriercoded[which(pop_data$carrier=='DL')] = "United Airlines"
      return(data.frame(carrier=pop_data$carriercoded, arrived_delay_in_min=pop_data$arr_delay))

    }
  })

  output$data.tab = renderDataTable({
    if(!input$usedata) data()
  })
  
  output$data.tab1 = renderDataTable({
    if(input$usedata) data()
  })
  
  ####
  output$datagraph = renderPlot({
    if((input$datformat==1 & !input$usedata) | (input$sampdat!=2 & input$usedata)) 
    {
      dat=unlist(data())
      dat1=data.frame(x=as.numeric(as.character(dat)))
      
      if(input$usedata)
        lab = "Gas consumption in millions of therms"
      else
        lab = paste(names(data())[1])
      
      ggplot(data=dat1) + geom_histogram(aes(x=x), fill="red", alpha=.5) +
        xlab(lab) + ylab("Frequency") +
        ggtitle(paste("Histogram of",lab)) + theme_bw()
    } else if((input$datformat==2 & !input$usedata) | (input$sampdat!=1 & input$usedata))
    {
      dat=data()
      dat1=data.frame(x=dat[[1]],y=dat[[2]])
      if(length(unique(dat1$x))>length(unique(dat1$y)))
      {
        dat1$x = as.numeric(as.character(dat1$x))
        ggplot(data=dat1) + geom_boxplot(aes(x=factor(y),y=x,fill=factor(y)),alpha=.5) + 
          xlab(paste(names(dat)[2])) + ylab(paste(names(dat)[1])) + theme_bw() +
          ggtitle(paste("Boxplots of",paste(names(dat)[1])," by",paste(names(dat)[2]))) +
          scale_fill_manual(name=paste(names(dat)[1]),values=c("blue1","cyan1")) +
          theme(legend.position="bottom")
      } else
      { 
        dat1$y = as.numeric(as.character(dat1$y))
        ggplot(data=dat1) + geom_boxplot(aes(x=factor(x),y=y,fill=factor(x)),alpha=.5) +
          xlab(paste(names(dat)[1])) + ylab(paste(names(dat)[2])) + theme_bw() +
          ggtitle(paste("Boxplots of",paste(names(dat)[2]),"by",paste(names(dat)[1]))) +
          scale_fill_manual(name=paste(names(dat)[2]),values=c("blue1","cyan1")) +
          theme(legend.position="bottom")
      }
    } else if((input$datformat==3 & !input$usedata) | (input$sampdat!=1 & input$usedata))
    {
      dat=data()
      dat1=data.frame(x=c(as.numeric(as.character(dat[[1]])),as.numeric(as.character(dat[[2]]))),
                      y=c(rep(names(dat)[1],length(dat[[1]])),rep(names(dat)[2],length(dat[[2]]))))
      ggplot(data=dat1) + geom_boxplot(aes(x=factor(y),y=x,fill=factor(y)),alpha=.5) + 
        xlab("Explanatory variable") + ylab("Response variable") +
        scale_fill_manual(name="",values=c("blue1","cyan1")) +
        ggtitle("Boxplots") + theme_bw() + theme(legend.position="bottom")
    }
  })
  
  output$summarystats = renderTable({
    if((input$datformat==1 & !input$usedata) | (input$sampdat!=2 & input$usedata))
    {
      vec = as.numeric(as.character(data()[[1]]))
      table = t(matrix(c((as.matrix(summary(vec)[1:6])),
                         round(sd(vec,na.rm=TRUE)))))
      
      if(input$usedata)
        rownames(table) = "Eruption times"
      else
        rownames(table) = names(data())[[1]]
      
      colnames(table) = c("Min","Q1","Median","Mean","Q3","Max","SD")
      return(table)
    } else if((input$datformat==2 & !input$usedata) | (input$sampdat!=1 & input$usedata))
    {
      dat=data()
      dat1=data.frame(x=dat[[1]],y=dat[[2]])
      if(length(unique(dat1$x)) > length(unique(dat1$y)))
      {
        dat1$y = factor(dat1$y)
        dat1$x = as.numeric(as.character(dat1$x))
        dat1 = dat1[which(complete.cases(dat1)),]
        sum = tapply(dat1$x,dat1$y,summary)
        table = data.frame(matrix(c(sum[[1]],sum[[2]]),nrow=2,ncol=6,byrow=TRUE))
        std = tapply(dat1$x,dat1$y,sd,na.rm=TRUE)
        table$sd[1] = round(std[1],digits=2)
        table$sd[2] = round(std[2],digits=2)
        table = as.matrix(table)
        colnames(table) = c("Min","Q1","Median","Mean","Q3","Max","SD")
        rownames(table) = c(levels(dat1$y)[1],levels(dat1$y)[2])
        return(table)        
      } else if(length(unique(dat1$x)) < length(unique(dat1$y)))
      {
        dat1$x = factor(dat1$x)
        dat1$y = as.numeric(as.character(dat1$y))
        dat1 = dat1[which(complete.cases(dat1)),]
        sum = tapply(dat1$y,dat1$x,summary)
        table = data.frame(matrix(c(sum[[1]],sum[[2]]),nrow=2,ncol=6,byrow=TRUE))
        std = tapply(dat1$y,dat1$x,sd)
        table$sd[1] = round(std[1],digits=2)
        table$sd[2] = round(std[2],digits=2)
        table = as.matrix(table)
        colnames(table) = c("Min","Q1","Median","Mean","Q3","Max","SD")
        rownames(table) = c(levels(dat1$x)[1],levels(dat1$x)[2])
        return(table)         
      }
    } else if((input$datformat==3 & !input$usedata) | (input$sampdat!=1 & input$usedata))
    {
      dat = data()
      dat[,1] = as.numeric(as.character(dat[,1]))
      dat[,2] = as.numeric(as.character(dat[,2]))
      table = data.frame(t(as.matrix(apply(dat,2,summary)[-7,])))
      table$sd[1] = round(sd(dat[,1],na.rm=TRUE),digits=2)
      table$sd[2] = round(sd(dat[,2],na.rm=TRUE),digits=2)
      table = as.matrix(table)
      colnames(table) = c("Min","Q1","Median","Mean","Q3","Max","SD")
      return(table)
    }
  })
  

  #### T-test Panel
  output$info = renderUI({
    HTML(as.character(code("Click here for hypothesis test information.")))
  })
  
  output$onesample = renderUI({
    HTML(as.character(code("Click here for one-sample t-test information.")))
  })
  
  output$twosample = renderUI({
    HTML(as.character(code("Click here for two-sample t-test information.")))
  })
  
  output$hypo1 = renderUI({
    if((input$datformat==1 & !input$usedata) | (input$sampdat!=2 & input$usedata))
    {
      if(input$alt1=="less than") 
        HTML("Ho: &mu; =", input$null1,"<p> Ha: &mu; <",input$null1)
      else if(input$alt1=="greater than")
        HTML("Ho: &mu; =", input$null1,"<p> Ha: &mu; >",input$null1)
      else 
        HTML("Ho: &mu; =", input$null1,"<p> Ha: &mu; &ne;",input$null1)
    } 
  })
  
  output$hypo2 = renderUI({
    if((input$datformat!=1 & !input$usedata) | (input$sampdat!=1 & input$usedata))
    {
      if(input$alt2=="less than") 
        HTML("Ho: &mu;<sub>1</sub>-&mu;<sub>2</sub> =",input$null2,
             "<p> Ha: &mu;<sub>1</sub>-&mu;<sub>2</sub> <",input$null2)
      else if(input$alt2=="greater than")
        HTML("Ho: &mu;<sub>1</sub>-&mu;<sub>2</sub> =",input$null2,
             "<p> Ha: &mu;<sub>1</sub>-&mu;<sub>2</sub> >",input$null2)
      else 
        HTML("Ho: &mu;<sub>1</sub>-&mu;<sub>2</sub> =",input$null2,
             "<p> Ha: &mu;<sub>1</sub>-&mu;<sub>2</sub> &ne;",input$null2)
    } 
  })
  
  mod = reactive({
    input$teststart
    isolate({
      if(input$teststart>0)
      {
        if((input$datformat==1 & !input$usedata) | (input$sampdat!=2 & input$usedata))
        {
          if(input$alt1=="less than") 
            mod = t.test(x=as.numeric(as.character(unlist(data()))),alternative="less",mu=input$null1,conf.level=1-input$alpha)
          else if(input$alt1=="greater than") 
            mod = t.test(x=as.numeric(as.character(unlist(data()))),alternative="greater",mu=input$null1,conf.level=1-input$alpha)
          else 
            mod = t.test(x=as.numeric(as.character(unlist(data()))),alternative="two.sided",mu=input$null1,conf.level=1-input$alpha)
        } else if((input$datformat==2 & !input$usedata) | (input$sampdat!=1 & input$usedata))
        {
          dat=data()
          if(length(unique(dat[[1]])) > length(unique(dat[[2]])))
          {
            if(input$alt2=="less than")
              mod = t.test(as.numeric(as.character(dat[[1]]))~dat[[2]],
                           alternative="less",mu=input$null2,conf.level=1-input$alpha)
            else if(input$alt2=="greater than")
              mod = t.test(as.numeric(as.character(dat[[1]]))~dat[[2]],
                           alternative="greater",mu=input$null2,conf.level=1-input$alpha)
            else 
              mod = t.test(as.numeric(as.character(dat[[1]]))~dat[[2]],
                           alternative="two.sided",mu=input$null2,conf.level=1-input$alpha)
          } else
          {
            if(input$alt2=="less than")
              mod = t.test(as.numeric(as.character(dat[[2]]))~dat[[1]],
                           alternative="less",mu=input$null2,conf.level=1-input$alpha)
            else if(input$alt2=="greater than")
              mod = t.test(as.numeric(as.character(dat[[2]]))~dat[[1]], 
                           alternative="greater",mu=input$null2,conf.level=1-input$alpha)
            else 
              mod = t.test(as.numeric(as.character(dat[[2]]))~dat[[1]],
                           alternative="two.sided",mu=input$null2,conf.level=1-input$alpha)
          }
        } else if((input$datformat==3 & !input$usedata) | (input$sampdat!=1 & input$usedata))
        {
          dat=data()
          if(input$alt2=="less than")
            mod = t.test(x=as.numeric(as.character(dat[[1]])),y=as.numeric(as.character(dat[[2]])),
                         alternative="less",mu=input$null2,conf.level=1-input$alpha)
          else if(input$alt2=="greater than")
            mod = t.test(x=as.numeric(as.character(dat[[1]])),y=as.numeric(as.character(dat[[2]])),
                         alternative="greater",mu=input$null2,conf.level=1-input$alpha)
          else 
            mod = t.test(x=as.numeric(as.character(dat[[1]])),y=as.numeric(as.character(dat[[2]])),
                         alternative="two.sided",mu=input$null2,conf.level=1-input$alpha)
        }
      }
    })
  })
  
  output$est=renderUI({
    if(input$teststart>0 & ((input$datformat==1 & !input$usedata) | (input$sampdat!=2 & input$usedata)))
    {
      HTML("x&#773; =",round(mod()$estimate[1],2))
    } else if(input$teststart>0 & ((input$datformat!=1 & !input$usedata) | (input$sampdat!=1 & input$usedata)))
    {
      HTML("x&#773<sub>1</sub> =",round(mod()$estimate[1],2),"<p> x&#773<sub>2</sub> =",round(mod()$estimate[2],2),
           "<p> x&#773<sub>1</sub> - x&#773<sub>2</sub> =",round(mod()$estimate[1]-mod()$estimate[2],2))
    }
  })
  
  output$test = renderTable({
    input$teststart
    isolate({
      if(input$teststart>0)
      {
        tab = matrix(c(mod()$parameter,mod()$statistic,mod()$p.value),nrow=1)
        colnames(tab) = c("df","t-statistic","p-value")
        rownames(tab) = "Values"
        tab
      } 
    })
  })
  
  output$tdist = renderPlot({
    input$teststart
    isolate({
      if(input$alt1=="less than" | input$alt2=="less than")
      {
        tail="left"
      } else if(input$alt1=="greater than" | input$alt2=="greater than")
      {
        tail="right"
      } else if(input$alt1=="two-sided" | input$alt2=="two-sided")
      {
        tail="both"
      } 
      
      return(t.dist.area(mod()$statistic,tail=tail,mod()$parameter))
    })
  })
  
  output$citab = renderTable({
    if(input$teststart>0)
    {
      tab = matrix(c(mod()$conf.int[1],mod()$conf.int[2]),nrow=1)
      colnames(tab) = c("Lower bound","Upper bound")
      rownames(tab) = paste(round(1-input$alpha, digits=3)*100,"% CI",sep="")
      tab
    }
  })
  
  
  
  ##########################################################################################################
  ###Render GENERALIZED LINEAR MODELS navtab
  ##########################################################################################################  
  
  
  
}
