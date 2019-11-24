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
  
  
  # table tab
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
  
  
  # table tab
  output$tabProbBy <- DT::renderDataTable( {
    
    if (input$dismodel == 'binomial') { 
        print( paste('Generating table by',input$bgender_country, input$bgender_year))
      
  #    if (input$bdataset == 'gender'){
        
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
        
  #    }
      
    }else { 
      
      return(NULL)
      
    }
    
  }) # output$tabProbBy 

  # plot tab
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
  
  
  
  ##########################################################################################################
  ###Render GENERALIZED LINEAR MODELS navtab
  ##########################################################################################################  
  
  
  
}