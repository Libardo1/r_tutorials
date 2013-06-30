# vlookup in R >> original source: http://stackoverflow.com/questions/5539407/similar-to-excel-vlookup
# and : http://stackoverflow.com/questions/15303283/how-to-do-vlookup-and-fill-down-like-in-excel-in-r
european_flights <- read.csv("~/Desktop/rstats/data/european_flights.csv")
table_large <- european_flights
str(table_large)
dep_co <- as.data.frame(table(table_large$departure_country))
dep_co$eur <- TRUE
str(dep_co)
table_equiv <- dep_co
str(table_equiv)
table_equiv
names(table_equiv) <- c("departure_country","freq","dep_european")
merge1 <- merge(table_large,table_equiv,by="departure_country",all.y=TRUE)
str(merge1)
from <- subset(merge1,departure_country=="Austria")
to <- as.data.frame(table(from$arrival_country))
ord1 <- to[order(to$Freq,decreasing=TRUE),]
ord1
