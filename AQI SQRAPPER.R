



##------loading library----------------

library(lubridate)
library(RSelenium)
library(tidyverse)
library(zoo)

##------ User Details  Entry----------------------

##--enter date to begin scrapping(yyyy-mm-dd)
staD<-as.Date("2020/11/02")

##--enter end date(yyyy-mm-dd)-
endD<-as.Date("2020/11/13")

##--enter time(24 hour format)-



##------sit back and relax------------------------ 
startime<-Sys.time()

data <- data.frame(matrix(ncol = 10,nrow=0))
d<-c("DATE","TIME","MONTH","PM2.5" , "PM10" ,"NO2","NH3","SO2","CO","OZONE")
colnames(data)<-d
one <- data.frame(matrix(ncol = 10,nrow=0))
d<-c("DATE","TIME","MONTH","PM2.5" , "PM10" ,"NO2","NH3","SO2","CO","OZONE")
colnames(one)<-d
rm(d)

ver<-binman::list_versions("chromedriver")
version<-ver$win32[4]
port<-sample(1000:5000,1)
driver <- rsDriver(browser=c("chrome"),port,chromever=version)
remote_driver <- driver[["client"]]

remote_driver$navigate("https://app.cpcbccr.com/AQI_India/")
Sys.sleep(5)




date_element<-remote_driver$findElement(value='//*[(@id = "date")]//*[contains(concat( " ", @class, " " ), concat( " ", "form-control", " " ))]')
time_element<-remote_driver$findElement(using='id', value="time")

for( i in as.numeric(staD):as.numeric(endD)){
  
  
  
  iasdate <- as.Date(i)
  d<-day(iasdate)
  m<-month(iasdate)
  y<-year(iasdate)
  
  
  date<-paste0(d,"/",m,"/",y)
  date_element$clearElement()
  
  date_element$sendKeysToElement(list(date))
  Sys.sleep(1)
  date_element$sendKeysToElement(list(key="enter"))
  state_element<-remote_driver$findElement(value='//*[(@id = "states")]')
  station_element<-remote_driver$findElement(value='//*[(@id = "cities")]')
  loc_element<-remote_driver$findElement(value='//*[(@id = "stations")]')
  state_element$sendKeysToElement(list("DELHI",key="enter"))
  station_element$sendKeysToElement(list("DELHI",key="enter"))
  loc_element$sendKeysToElement(list("Dwark",key="enter"))
  
  for(j in 00:23){
    
    time<-paste0(sprintf("%02d",j),":00") 
    time_element$clearElement()
    time_element$sendKeysToElement(list(time))
    Sys.sleep(1)
    time_element$sendKeysToElement(list(key="enter")) 
    date_element$clickElement()
    date_element$sendKeysToElement(list(key="enter"))
    Sys.sleep(2.7)
    
    
    pmdhaiavg<-remote_driver$findElements(value='//*[contains(concat( " ", @class, " " ), concat( " ", "metrics-row", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "avg-value", " " ))]')
    pmdhai<-as.numeric(sapply(pmdhaiavg, function(x) x$getElementText()))
    
    
    
    
    pmdusavg<-remote_driver$findElements(value='//*[contains(concat( " ", @class, " " ), concat( " ", "metrics-row", " " )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "avg-value", " " ))]')
    pmdus<-as.numeric(sapply(pmdusavg, function(x) x$getElementText()))
    
    
    notwoavg<-remote_driver$findElements(value='//*[contains(concat( " ", @class, " " ), concat( " ", "metrics-row", " " )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "avg-value", " " ))]')
    notwo<-as.numeric(sapply(notwoavg , function(x) x$getElementText()))
    
    nhthreeavg<-remote_driver$findElements(value='//*[contains(concat( " ", @class, " " ), concat( " ", "metrics-row", " " )) and (((count(preceding-sibling::*) + 1) = 4) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "avg-value", " " ))]')
    nhthree<-as.numeric(sapply(nhthreeavg, function(x) x$getElementText()))
    
    sotwoavg<-remote_driver$findElements(value='//*[contains(concat( " ", @class, " " ), concat( " ", "metrics-row", " " )) and (((count(preceding-sibling::*) + 1) = 5) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "avg-value", " " ))]')
    sotwo<-as.numeric(sapply(sotwoavg, function(x) x$getElementText()))
    
    
    coavg<-remote_driver$findElements(value='//*[contains(concat( " ", @class, " " ), concat( " ", "metrics-row", " " )) and (((count(preceding-sibling::*) + 1) = 6) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "avg-value", " " ))]')
    co<-as.numeric(sapply(coavg, function(x) x$getElementText()))
    
    ozoneavg<-remote_driver$findElements(value='//*[contains(concat( " ", @class, " " ), concat( " ", "metrics-row", " " )) and (((count(preceding-sibling::*) + 1) = 7) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "avg-value", " " ))]')
    ozone<-as.numeric(sapply(ozoneavg , function(x) x$getElementText()))
    
    
    
    
    
    
    one[1,]<-c(date,time,month.abb[m],pmdhai[1],pmdus[1],notwo[1],nhthree[1],sotwo[1],co[1],ozone[1])
    data<-rbind(data,one)
    rm(pmdhai,pmdus,notwo,nhthree,sotwo,co,ozone,pmdhaiavg,nhthreeavg,notwoavg,sotwoavg,coavg,ozoneavg,pmdusavg)
    #for loop for missing will start afterwards
    remote_driver$deleteAllCookies()
    if(j==23){	remote_driver$close()
      remote_driver$open()
      remote_driver$navigate("https://app.cpcbccr.com/AQI_India/")
      date_element<-remote_driver$findElement(value='//*[(@id = "date")]//*[contains(concat( " ", @class, " " ), concat( " ", "form-control", " " ))]')
      time_element<-remote_driver$findElement(using='id', value="time")
      
      
      state_element<-remote_driver$findElement(value='//*[(@id = "states")]')
      station_element<-remote_driver$findElement(value='//*[(@id = "cities")]')
      loc_element<-remote_driver$findElement(value='//*[(@id = "stations")]')
      state_element$sendKeysToElement(list("DELHI",key="enter"))
      station_element$sendKeysToElement(list("DELHI",key="enter"))
      loc_element$sendKeysToElement(list("Anand",key="enter"))
      
      date_element$clearElement()
      
      date_element$sendKeysToElement(list(date))
      
      
    }
    
    
  }
}

##----------------------------------------------------------------------------------------------------------------------------------------------------------------------










