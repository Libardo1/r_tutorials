### Innova data BBVA ###
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
CARDS_CUBE="https://api.bbva.com/apidatos/zones/cards_cube.json"
CATEGORIES_INFO="https://api.bbva.com/apidatos/info/merchants_categories.json"
CUSTOMER_ZIPCODES="https://api.bbva.com/apidatos/zones/customer_zipcodes.json"
CONSUMPTION_PATTERN = "https://api.bbva.com/apidatos/zones/consumption_pattern.json"

##### CUSTOMER_ZIPCODES:  #####
### Mandatory fields: date_min, date_max, group_by, zipcode, by
# Selected zipcodes: 28028,28001,28013,28006,28004,28005,28008,28010,28015,28660
# Parámetros de la petición
datemin = 201211
datemax = 201304
zipcode = 28660
catego = "es_wellnessandbeauty"
# group by: week, month
groupby = "month"
# by: cards, payments, incomes
by = "incomes"

# Peticion a customer_zipcodes
# total
gg<-getForm(CUSTOMER_ZIPCODES, .opts=list(userpwd=strToEncode, httpauth=1L,verbose=T),date_min = datemin,date_max = datemax, group_by = groupby,zipcode = zipcode, by=by)
# categories
gg<-getForm(CUSTOMER_ZIPCODES, .opts=list(userpwd=strToEncode, httpauth=1L,verbose=T),date_min = datemin,date_max = datemax, group_by = groupby,zipcode = zipcode, by=by,category=catego)


# Convert to JSON
jj<-fromJSON(gg)

# Function to extract zipcodes and build a data frame
get_datazips<-function(zipcodes) {
  dfzips<-data.frame(
    zipcode = character(),
    num_cards = integer(),
    num_payments = integer(),
    incomes = numeric(),
    stringsAsFactors=F
  )
  i = 1;
  for (zipcode in zipcodes) {
    data<-list(zipcode$label,zipcode$num_cards, zipcode$num_payments, zipcode$incomes)
    dfzips[i,]<-data
    i = i+1;
  }
  return(dfzips)
}

# Run zipcodes extract function
datazips_nov<-get_datazips(jj$data$stats[[1]]$zipcodes)
datazips_nov$time <- "112012"
datazips_dic<-get_datazips(jj$data$stats[[2]]$zipcodes)
datazips_dic$time <- "122012"
datazips_ene<-get_datazips(jj$data$stats[[3]]$zipcodes)
datazips_ene$time <- "012013"
datazips_feb<-get_datazips(jj$data$stats[[4]]$zipcodes)
datazips_feb$time <- "022013"
datazips_mar<-get_datazips(jj$data$stats[[5]]$zipcodes)
datazips_mar$time <- "032013"
datazips_abr<-get_datazips(jj$data$stats[[6]]$zipcodes)
datazips_abr$time <- "042013"
datazips <- rbind(datazips_nov,datazips_dic,datazips_ene,datazips_feb,datazips_mar,datazips_abr)
str(datazips)
head(datazips)

# Add destination zipcode
datazips$dest_zipcode <- zipcode

# Add zipcodes geographic coodrinates
# Load and prepare data
ES_postal_codes <- read.csv("~/Desktop/innova_BBVA/ES_postal_codes.csv")
postal <- ES_postal_codes
unique_postal <- postal[!duplicated(postal$Postal.Code),]
names(unique_postal) <- c("zipcode","place","state","county","city","lat","lon")
head(unique_postal)
zipcode_geo_1 <- unique_postal[1]
zipcode_geo_2 <- unique_postal[6:7]
zipcode_geo <- cbind(zipcode_geo_1,zipcode_geo_2)
head(zipcode_geo)

# Merge API result with geo data
datazips_merge_1 <- merge(datazips,zipcode_geo,by="zipcode")
names(datazips_merge_1) <- c("orig_zipcode","num_cards","num_payments","incomes","time","zipcode","orig_lat","orig_lon")
datazips_merge_2 <- merge(datazips_merge_1,zipcode_geo,by="zipcode")
names(datazips_merge_2) <- c("dest_zipcode","orig_zipcode","num_cards","num_payments","incomes","time","orig_lat","orig_lon","dest_lat","dest_lon")
datazips_merge_2 <- datazips_merge_2[order(datazips_merge_2$incomes,decreasing=TRUE),]
head(datazips_merge_2)


# total
datazips_merge_2$cat <- "total"
head(datazips_merge_2)

# categories
datazips_merge_2$cat <- catego
head(datazips_merge_2)

# CSV output
write.csv(datazips_merge_2,file="customer_zipcodes_28660_es_wellnessandbeauty.csv")

# Mapping connections
madrid <- geocode('Madrid,Spain')
madrid
center <- as.numeric(madrid)
ggmap(get_map(location=center,scale=2,color='bw',zoom=11))
ggmap(get_map(location=center,scale=2,color='bw',zoom=11)) + geom_point(aes(x=orig_lon,y=orig_lat),data=datazips_merge_2)
ggmap(get_map(location=center,scale=2,color='bw',zoom=11)) + geom_point(aes(x=orig_lon,y=orig_lat,size=num_cards),data=datazips_merge_2)
ggmap(get_map(location=center,scale=2,color='bw',zoom=11)) + geom_point(aes(x=orig_lon,y=orig_lat,size=num_payments),data=datazips_merge_2)
ggmap(get_map(location=center,scale=2,color='bw',zoom=11)) + geom_point(aes(x=orig_lon,y=orig_lat,size=incomes),data=datazips_merge_2)
qmap(center,zoom=13,maprange=TRUE,maptype='terrain') + geom_leg(aes(x=orig_lon,y=orig_lat,xend=dest_lon,yend=dest_lat,size=incomes,colour="blue"),data=datazips_merge_2)
qmap(center,zoom=13,maprange=TRUE,maptype='terrain') + geom_segment(aes(x=orig_lon,y=orig_lat,xend=dest_lon,yend=dest_lat,size=num_cards,alpha=0.2),data=datazips_merge_2) + facet_wrap(~time)


