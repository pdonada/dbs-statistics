###############################################
##
##    Development of Statistical Analytics App
##
##    global.R - loading and defining variables 
##    for the global environment
##
###############################################
if (!require("shiny")) install.packages("shiny")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("dplyr")) install.packages("dplyr")
if (!require("plyr")) install.packages("plyr")
if (!require("WDI")) install.packages("WDI")  # World Bank Development indicators
if (!require("DT")) install.packages("DT")
if (!require("leaflet")) install.packages("leaflet")
if (!require("epiDisplay")) install.packages("epiDisplay")
if (!require("ggplot2")) install.packages("ggplot2") # walbert
if (!require("shinyBS")) install.packages("shinyBS") # walbert
if (!require("nycflights13")) install.packages("nycflights13") # walbert

library(shiny)
library(shinythemes) 
library(dplyr)
library(plyr)
library(WDI)
library(DT)
library(leaflet)
library(epiDisplay)
library(ggplot2) # walbert
library(shinyBS) # walbert
library(nycflights13) # walbert

data(longley)  # GLM dataset

###############################################
#      list of countries available            #
countries <- c( 
  'Albania'='AL'
  ,'Algeria'='DZ'
  ,'Angola'='AO'
  ,'Argentina'='AR'
  ,'Armenia'='AM'
  ,'Australia'='AU'
  ,'Austria'='AT'
  ,'Azerbaijan'='AZ'
  ,'Bahamas, The'='BS'
  ,'Bahrain'='BH'
  ,'Bangladesh'='BD'
  ,'Barbados'='BB'
  ,'Belarus'='BY'
  ,'Belize'='BZ'
  ,'Benin'='BJ'
  ,'Bolivia'='BO'
  ,'Botswana'='BW'
  ,'Brazil'='BR'
  ,'Bulgaria'='BG'
  ,'Burkina Faso'='BF'
  ,'Burundi'='BI'
  ,'Cabo Verde'='CV'
  ,'Cambodia'='KH'
  ,'Cameroon'='CM'
  ,'Central African Republic'='CF'
  ,'Chad'='TD'
  ,'Chile'='CL'
  ,'Colombia'='CO'
  ,'Comoros'='KM'
  ,'Congo, Dem. Rep.'='CD'
  ,'Congo, Rep.'='CG'
  ,'Costa Rica'='CR'
  ,'Cote dIvoire'='CI'
  ,'Cuba'='CU'
  ,'Cyprus'='CY'
  ,'Czech Republic'='CZ'
  ,'Denmark'='DK'
  ,'Djibouti'='DJ'
  ,'Dominican Republic'='DO'
  ,'Ecuador'='EC'
  ,'Egypt, Arab Rep.'='EG'
  ,'El Salvador'='SV'
  ,'Equatorial Guinea'='GQ'
  ,'Eritrea'='ER'
  ,'Estonia'='EE'
  ,'Eswatini'='SZ'
  ,'Ethiopia'='ET'
  ,'France'='FR'
  ,'Gabon'='GA'
  ,'Gambia, The'='GM'
  ,'Georgia'='GE'
  ,'Ghana'='GH'
  ,'Greece'='GR'
  ,'Guatemala'='GT'
  ,'Guinea'='GN'
  ,'Guinea-Bissau'='GW'
  ,'Guyana'='GY'
  ,'Haiti'='HT'
  ,'Honduras'='HN'
  ,'Hungary'='HU'
  ,'India'='IN'
  ,'Indonesia'='ID'
  ,'Iran, Islamic Rep.'='IR'
  ,'Ireland'='IE'
  ,'Italy'='IT'
  ,'Japan'='JP'
  ,'Kazakhstan'='KZ'
  ,'Kenya'='KE'
  ,'Kuwait'='KW'
  ,'Kyrgyz Republic'='KG'
  ,'Lao PDR'='LA'
  ,'Lebanon'='LB'
  ,'Lesotho'='LS'
  ,'Liberia'='LR'
  ,'Lithuania'='LT'
  ,'Luxembourg'='LU'
  ,'Madagascar'='MG'
  ,'Malawi'='MW'
  ,'Malaysia'='MY'
  ,'Mali'='ML'
  ,'Mauritania'='MR'
  ,'Mexico'='MX'
  ,'Moldova'='MD'
  ,'Mongolia'='MN'
  ,'Montenegro'='ME'
  ,'Morocco'='MA'
  ,'Mozambique'='MZ'
  ,'Myanmar'='MM'
  ,'Namibia'='NA'
  ,'Nepal'='NP'
  ,'Netherlands'='NL'
  ,'New Zealand'='NZ'
  ,'Nicaragua'='NI'
  ,'Niger'='NE'
  ,'Nigeria'='NG'
  ,'North Macedonia'='MK'
  ,'Norway'='NO'
  ,'Pakistan'='PK'
  ,'Panama'='PA'
  ,'Papua New Guinea'='PG'
  ,'Paraguay'='PY'
  ,'Peru'='PE'
  ,'Philippines'='PH'
  ,'Portugal'='PT'
  ,'Qatar'='QA'
  ,'Romania'='RO'
  ,'Russian Federation'='RU'
  ,'Rwanda'='RW'
  ,'Senegal'='SN'
  ,'Serbia'='RS'
  ,'Sierra Leone'='SL'
  ,'Singapore'='SG'
  ,'Slovak Republic'='SK'
  ,'Slovenia'='SI'
  ,'Somalia'='SO'
  ,'South Africa'='ZA'
  ,'South Sudan'='SS'
  ,'Spain'='ES'
  ,'Sri Lanka'='LK'
  ,'Sudan'='SD'
  ,'Suriname'='SR'
  ,'Tajikistan'='TJ'
  ,'Tanzania'='TZ'
  ,'Thailand'='TH'
  ,'Togo'='TG'
  ,'Trinidad and Tobago'='TT'
  ,'Tunisia'='TN'
  ,'Uganda'='UG'
  ,'Ukraine'='UA'
  ,'Uruguay'='UY'
  ,'Uzbekistan'='UZ'
  ,'Vietnam'='VN'
  ,'Zambia'='ZM'
  ,'Zimbabwe'='ZW'
)

###############################################
##    set models that are available         ##
##    by type of distribuiton               ##
selectDiscreteDist <- c('Binomial' = 'binomial'
                        )

selectContinuousDist <- c('Normal' = 'normal'
                        )

###############################################
##          binomial datasets                ##
###############################################

###############################################
# dataset 1: Gender Statistics               ##

# list of series available
bdbgender_series <- c('Access to anti-retroviral drugs' = 'access'
                    , 'Progression to secondary school' = 'progression'
                    , 'Cause of death by injury (ages 15-34)' = 'cause')
                    
                    
###############################################
# dataset 2: Population Statistics           ##

# list of series available
bdbpopulation_series <- c('Immunization - measles (children ages 12-23 months)' = 'measles'
                         ,'People using safely managed sanitation services' = 'sanitation')


startYear = 2008
endYear = 2018
binomial_source = 'https://www.r-project.org/nosvn/pandoc/WDI.html'
normal_source = 'Epidemiological Data Display Package in R (epiDisplay)'


###############################################
##   Function to plot binomial distribution  ##
###############################################
binomial_plot <- function (i_population, i_trials, i_prob, i_gender) {
  
  print(paste('Generating plot data',i_population,i_trials,i_prob,i_gender))
  
  if( (is.null(i_prob) || i_prob == '' || length(i_prob) == 0)  ){
    return (NULL)
  }
  
  d <- density( rbinom( n = i_population, size = i_trials, prob = i_prob) )  # simulation  (s = number of observations ) 
  x <- 0:i_trials  
  y <- dbinom(x,i_trials,i_prob)  # pmf
  
  plot(d, main="Kernel Density of generated data", sub = i_gender) 
  polygon(d, col="red", border="blue") 
  
  plot(x, y , main = "Probability mass function", sub = i_gender, xlab= "Number of trials", ylab= "Probability")  # pmf
  
}

###############################################
##   Function to validate input parameters   ##
###############################################
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
    
  }else if ( model == 'normal' ){
    if ( !( is.numeric(value) & !is.na(value) ) ){
      paste('*** Invalid', parameter,'value (Not numeric) ***') 
      
    }else{
      if (( value <0 ) && parameter != 'mu' ) {
        paste('*** Invalid',parameter,'value (min = 0) ***') 
      }
    }
    
  }
  
}

#walbert
###############################################
##       t-distribution  Function            ##
###############################################

t.dist.area = function(tstat,tail,df)
{
  x = seq(-5,5,length.out=200)
  df = round(df, digits=3)
  
  if(tail=="right")
  {
    xmin=tstat
    xmax=5
    
    area = seq(xmin,xmax,length.out=300)
    dat = data.frame(x=area,ymin=0,ymax=dt(area,df=df))
    
    graph = ggplot() + geom_line(data.frame(x=x, y=dt(x,df=df)), mapping=aes(x=x, y=y)) + 
      geom_ribbon(data=dat, mapping=aes(x=x, ymin=ymin, ymax=ymax), fill="blue1") + 
      ggtitle(paste("t-distribution with", df, "degrees of freedom")) +
      xlab("t-values") + ylab("Relative frequency") + theme_gray  ()
  } else if(tail=="left")
  {
    xmin=-5
    xmax=tstat
    
    area = seq(xmin,xmax,length.out=300)
    dat = data.frame(x=area,ymin=0,ymax=dt(area,df=df))
    
    graph = ggplot() + geom_line(data.frame(x=x, y=dt(x,df=df)), mapping=aes(x=x, y=y)) + 
      geom_ribbon(data=dat, mapping=aes(x=x, ymin=ymin, ymax=ymax), fill="blue1") +
      ggtitle(paste("t-distribution with", df, "degrees of freedom")) +
      xlab("t-values") + ylab("Relative frequency") + theme_gray ()
  } else if(tail=="both")
  {
    xmin1=abs(tstat)
    xmax1=5
    area1 = seq(xmin1,xmax1,length.out=300)
    dat1 = data.frame(x=area1,ymin1=0,ymax1=dt(area1,df=df))
    
    xmin2=-5
    xmax2=-abs(tstat)
    area2 = seq(xmin2,xmax2,length.out=300)
    dat2 = data.frame(x=area2,ymin2=0,ymax2=dt(area2,df=df))
    
    graph = ggplot() + geom_line(data.frame(x=x, y=dt(x,df=df)), mapping=aes(x=x, y=y)) + 
      geom_ribbon(data=dat1, mapping=aes(x=x, ymin=ymin1, ymax=ymax1),fill="blue1") +
      geom_ribbon(data=dat2, mapping=aes(x=x, ymin=ymin2, ymax=ymax2),fill="blue1") +
      ggtitle(paste("t-distribution with", df, "degrees of freedom")) +
      xlab("t-values") + ylab("Relative frequency") + theme_gray ()
  }
  return(graph)
}


###############################################
##       glm function                        ##
###############################################
glm_apply_model <- function(pi_input, pi_output, pi_model, pi_dataset, pi_trainset) {
  
  if ( length(pi_input) == 1 ){
    
    fit <- glm(pi_dataset[,pi_output] ~ pi_dataset[,pi_input] , data = pi_trainset , family = pi_model)
    names( fit$coefficients) <- c("Intercept", pi_input)
    
  }else if ( length(pi_input) == 2 ){
    fit <- glm(pi_dataset[,pi_output] ~ pi_dataset[,pi_input[1]] + pi_dataset[,pi_input[2]] , data = pi_trainset , family = pi_model)
    names( fit$coefficients) <- c("Intercept", pi_input[1], pi_input[2])
    
  }else if ( length(pi_input) == 3 ){
    fit <- glm(pi_dataset[,pi_output] ~ pi_dataset[,pi_input[1]] + pi_dataset[,pi_input[2]] + pi_dataset[,pi_input[3]], data = pi_trainset , family = pi_model)
    names( fit$coefficients) <- c("Intercept", pi_input[1], pi_input[2], pi_input[3])
    
  }else if ( length(pi_input) == 4 ){
    fit <- glm(pi_dataset[,pi_output] ~ pi_dataset[,pi_input[1]] + pi_dataset[,pi_input[2]] + pi_dataset[,pi_input[3]] + pi_dataset[,pi_input[4]], data = pi_trainset , family = pi_model)
    names( fit$coefficients) <- c("Intercept", pi_input[1], pi_input[2], pi_input[3], pi_input[4])
  }
  
  return (fit)
}
