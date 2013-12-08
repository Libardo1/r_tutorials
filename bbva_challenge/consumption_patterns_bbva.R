# Consumption pattern
# Data: Noviembre 20012 to Abril 2013
library(RJSONIO)
library(RCurl)
library(rjson)
library(ggplot2)
library(reshape)
library(plyr)
library(maps)
library(geosphere)
library(ggmap)

# Request = HttpRequest(SERVICIO,PARAMETROS.header={'Authorization':auth})

# Parámetros de autenticación API CIBBVA
APP_ID = "your_id"
APP_KEY = "your_key"
strToEncode = paste(APP_ID,APP_KEY,sep=":")
auth = base64Encode(strToEncode)
auth

# URLS base servicios API BBVA
CONSUMPTION_PATTERN = "https://api.bbva.com/apidatos/zones/consumption_pattern.json"

# Parámetros de la petición
datemin = 201211
datemax = 201304
zipcode = 28004
catego = "es_barsandrestaurants"
groupby = "month"

# Peticion a customer_zipcodes
# total
gg<-getForm(CONSUMPTION_PATTERN, .opts=list(userpwd=strToEncode, httpauth=1L,verbose=T),date_min = datemin,date_max = datemax, group_by = groupby, zipcode = zipcode)
# categories
gg<-getForm(CONSUMPTION_PATTERN, .opts=list(userpwd=strToEncode, httpauth=1L,verbose=T),date_min = datemin,date_max = datemax, group_by = groupby,zipcode = zipcode, category=catego)

# Convert to JSON
jj<-fromJSON(gg) 

# Function to extract days and build a data frame
get_datadays<-function(days) {
  dfdays<-data.frame(
    day = character(),
    avg = numeric(),
    max = numeric(),
    min = numeric(),
    std = numeric(),
    mode = numeric(),
    n_cards = numeric(),
    n_payments = numeric(),
    stringsAsFactors=F
  )
  i = 1;
  for (day in days) {
    data<-list(days$day,days$avg,days$max,days$min,days$std,days$mode,days$num_cards,days$num_payments)
    dfdays[i,]<-data
    i = i+1;
  }
  return(dfdays)
}

# Funcion para extraer datos de horas
foo <- function(x){
  tmp <- data.frame(do.call(rbind,x$hours))
  tmp$day <- x$day
  tmp
}

# Aplicar funcion horas a lista de meses
nov <- jj$data$stats[[1]]$days
nov <- lapply(nov,foo)
nov <- do.call(rbind,nov)
nov <- lapply(nov,unlist)
nov <- as.data.frame(nov)
nov$month <- "112012"

dic <- jj$data$stats[[2]]$days
dic <- lapply(dic,foo)
dic <- do.call(rbind,dic)
dic <- lapply(dic,unlist)
dic <- as.data.frame(dic)
dic$month <- "122012"

ene <- jj$data$stats[[3]]$days
ene <- lapply(ene,foo)
ene <- do.call(rbind,ene)
ene <- lapply(ene,unlist)
ene <- as.data.frame(ene)
ene$month <- "012013"

feb <- jj$data$stats[[4]]$days
feb <- lapply(feb,foo)
feb <- do.call(rbind,feb)
feb <- lapply(feb,unlist)
feb <- as.data.frame(feb)
feb$month <- "022013"

mar <- jj$data$stats[[5]]$days
mar <- lapply(mar,foo)
mar <- do.call(rbind,mar)
mar <- lapply(mar,unlist)
mar <- as.data.frame(mar)
mar$month <- "032013"

abr <- jj$data$stats[[6]]$days
abr <- lapply(abr,foo)
abr <- do.call(rbind,abr)
abr <- lapply(abr,unlist)
abr <- as.data.frame(abr)
abr$month <- "042013"

df <- rbind(nov,dic,ene,feb,mar,abr)
df <- df[order(df$num_payments,decreasing=TRUE),]
df$zipcode <- zipcode
head(df)

# Total category 
df$category <- "total"

# Category
df$category <- catego

str(df)
head(df,50)
a <- ggplot(df,aes(hour,num_cards))
a + geom_point()

# Export to CSV
write.csv(df,file="28001_consumption_patterns_total.csv")



