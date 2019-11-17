###############################################
##
##    Development of Statistical Analytics App
##
##    global.R - loading and defining variables 
##    for the global environment
##
###############################################
library(shiny)
library(shinythemes) 
library(dplyr)

###############################################
#        main path to find the datasets       #
path = "C:/Users/Casa/Documents/DBS/ShinyAPP2/"

###############################################
##    set models that are available         ##
##    by type of distribuiton               ##
selectDiscreteDist <- c('Binomial' = 'binomial'
                        ,'Poisson' = 'poisson')

selectContinuousDist <- c('Normal' = 'normal'
                          , 'Exponential' = 'exponential')

###############################################
##          binomial datasets                ##
bdataset_gender = "binomial/Gender_Statistics.csv" # source: https://databank.worldbank.org/source/gender-statistics/

# dataset 1: Gender Statistics
# read csv file
bdataset <- read.csv(paste(path, bdataset_gender, sep = ""))
bdbgender <- bdataset %>%  na.omit()  # ignore lines with missing information
# list of unique countries in the dataset
bdbgender_country <- bdbgender$Country.Name %>% unique()
# list of series available
bdbgender_series <- c('Access.to.anti.retroviral.drugs' = 'access'
                      , 'Progression.to.secondary.school' = 'progression'
                      , 'Cause.of.death..by.injury..ages.15.34' = 'cause')

#######################################


###############################################
##   Function to validate input parameters   ##
probability_validation <- function(value, parameter, model) {
  
  if ( model == 'binomial') {
    
    if ( !( is.numeric(value) & !is.na(value) ) ){
      paste('*** Invalid', parameter, 'value (Not numeric) ***') 
      
    }else{
      
      if ( parameter == 'p' & (value<0 || value>1) ) { 
        paste('*** Invalid',parameter,'value (min = 0 / max = 1) ***') 
        
      }else if ( parameter == 'n' &  value <0 ) { 
        paste('*** Invalid', parameter,'value (min = 0) ***') 
      }
    }
    
  }else if ( model == 'poisson' || model == 'geometric') {
    if ( !( is.numeric(value) & !is.na(value) ) ){
      paste('*** Invalid', parameter,'value (Not numeric) ***') 
      
    }else{
      if ( value <0 ){ 
        paste('*** Invalid',parameter,'value (min = 0) ***') 
      }
    }    
    
  }else if ( model == 'normal' ){
    if ( !( is.numeric(value) & !is.na(value) ) ){
      paste('*** Invalid', parameter,'value (Not numeric) ***') 
      
    }else{
      if (( value <0 ) && parameter != 'mu' ) {
        paste('*** Invalid',parameter,'value (min = 0) ***') 
      }
    }
    
  }else if ( model == 'exponential' ){
    if ( !( is.numeric(value) & !is.na(value) ) ){
      paste('*** Invalid', parameter,'value (Not numeric) ***') 
      
    }else{
      if ( value <0 ) {
        paste('*** Invalid',parameter,'value (min = 0) ***') 
      }
    }
  }
  
}