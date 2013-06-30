# maping connections
library(maps)
library(geosphere)
map("state")
map("world")
# Limiting boundaries
xlim <- c(-171.738281, -56.601563)
ylim <- c(12.039321, 71.856229)
map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05, xlim=xlim, ylim=ylim)
# Drawing connecting lines
lat_ca <- 39.164141
lon_ca <- -121.640625
lat_me <- 45.213004
lon_me <- -68.906250
inter <- gcIntermediate(c(lon_ca, lat_ca), c(lon_me, lat_me), n=50, addStartEnd=TRUE)
lines(inter)
lat_tx <- 29.954935
lon_tx <- -98.701172
inter2 <- gcIntermediate(c(lon_ca, lat_ca), c(lon_tx, lat_tx), n=50, addStartEnd=TRUE)
lines(inter2, col="red")
# Load flight data
airports <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/airports.csv", header=TRUE)
flights <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/flights.csv", header=TRUE, as.is=TRUE)
# Draw multiple connections
map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05, xlim=xlim, ylim=ylim)
fsub <- flights[flights$airport1 == "ORD" & flights$cnt>=20,]
for (j in 1:length(fsub$airline)) {
  air1 <- airports[airports$iata == fsub[j,]$airport1,]
  air2 <- airports[airports$iata == fsub[j,]$airport2,]
  inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat), n=100, addStartEnd=TRUE)
  lines(inter, col="black", lwd=0.2)
}
# Color for clarity
pal <- colorRampPalette(c("#f2f2f2", "black"))
colors <- pal(100)
map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05, xlim=xlim, ylim=ylim)
fsub <- flights[flights$airline == "AA",]
maxcnt <- max(fsub$cnt)
for (j in 1:length(fsub$airline)) {
  air1 <- airports[airports$iata == fsub[j,]$airport1,]
  air2 <- airports[airports$iata == fsub[j,]$airport2,]
  inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat), n=100, addStartEnd=TRUE)
  colindex <- round( (fsub[j,]$cnt / maxcnt) * length(colors) )
  lines(inter, col=colors[colindex], lwd=0.8)
}
# refining color
pal <- colorRampPalette(c("#f2f2f2", "black"))
pal <- colorRampPalette(c("#f2f2f2", "red"))
colors <- pal(100)
map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05, xlim=xlim, ylim=ylim)
fsub <- flights[flights$airline == "AA",]
fsub <- fsub[order(fsub$cnt),]
maxcnt <- max(fsub$cnt)
for (j in 1:length(fsub$airline)) {
  air1 <- airports[airports$iata == fsub[j,]$airport1,]
  air2 <- airports[airports$iata == fsub[j,]$airport2,]
  inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat), n=100, addStartEnd=TRUE)
  colindex <- round( (fsub[j,]$cnt / maxcnt) * length(colors) )
  lines(inter, col=colors[colindex], lwd=0.8)
}


