# Innova BBVA bank + opinions

# Load data
df <- read.delim("~/Desktop/innova_BBVA/finalData.tsv")
str(df)
df <- subset(df,valueAvg!="NA")

# Normalize to 1km area
df$payments_area <- (df$numPayments/df$area)
df$cards_area <- (df$numCards/df$area)
str(df)

# Linear transformation to valueAvg (g.places)
# Calculate valueAvg range
range_min <- min(df$valueAvg)
range_max <- max(df$valueAvg)

# Calculate domain
domain_min <- min(df$payments_area)
domain_max <- max(df$payments_area)

# Cards domain
domain_min <- min(df$cards_area)
domain_max <- max(df$cards_area)


# Transformation
# y = y1 + (x-x1) * (y2-y1)/(x2-x1)
# x1,y1 => domain
# x2,y2 => range

foo <- function(x){
  temp <-range_min + (x-domain_min) * ((range_max-range_min)/(domain_max-domain_min))
  temp
}

payments_area_data <- df$payments_area
payments_area_new <- lapply(payments_area_data,foo)
payments_area_new <- as.data.frame(payments_area_new)
payments_area_new <- t(payments_area_new)

cards_area_data <- df$cards_area
cards_area_new <- lapply(cards_area_data,foo)
cards_area_new <- as.data.frame(cards_area_new)
cards_area_new <- t(cards_area_new)

# Add transformed payments & cards data to data frame

df <- cbind(df,payments_area_new)
df <- cbind(df,cards_area_new)
str(df)

# Plotting

qplot(payments_area_new,valueAvg,data=df,colour=category,size=numPlaces) + stat_smooth()
qplot(payments_area_new,valueAvg,data=df) + stat_smooth() 
qplot(payments_area_new,valueAvg,data=df,colour=category) + facet_wrap(~district)
qplot(payments_area_new,valueAvg,data=df,colour=category,size=numPlaces) + facet_wrap(~district)
qplot(lat,long,data=df,colour=category,size=valueAvg/payments_area_new)
qplot(lat,long,data=df,colour=category,size=valueAvg/payments_area_new) + facet_wrap(~district)
qplot(lat,long,data=df,size=valueAvg/payments_area_new,colour=district) + facet_wrap(~category)
qplot(lat,long,data=df,size=valueAvg/payments_area_new) + facet_wrap(~category)
qplot(payments_area_new,valueAvg,data=su,colour=category) + stat_smooth(se = FALSE)


# Export to CSV
df$geometry <- NULL 
write.csv(df,file="bank_opinions.csv")











