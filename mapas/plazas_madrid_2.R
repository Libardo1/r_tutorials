################################################
# Plazas de Madrid, distancias con Plaza del Sol
################################################

# URL del callejero en Dropbox: https://dl.dropboxusercontent.com/u/31748747/callejero_all.csv

library(xlsx)

# Loading complete callejero
raw <- read.csv("~/Desktop/rstats/data/callejero_all.csv")
str(raw)

# Puerta del Sol
# Items de raw con Puerta del Sol
sol <- subset(raw, DSVIAL_NOR == "PUERTA SOL" & DSMUNI == "Madrid")
# Coordenadas X/Y unica de Puerta del Sol
sol_x <- sol$COORD_X[1]
sol_y <- sol$COORD_Y[1]

# Searching for plazas
table(raw$CDTVIA)
plazaraw <- subset(raw, CDTVIA == "Plaza" | CDTVIA == "Plzla")
str(plazaraw)
dim(plazaraw)

# Looking for unique plazas
plazarefined <- plazaraw[ ! duplicated( plazaraw$DSVIAL), ]
dim(plazarefined)
largo <- length(plazarefined$X)

# Construir una columna con coordenadas de Sol y largo de plazarefined
sol_x <- rep(sol_x,largo)
sol_y <- rep(sol_y,largo)

# Extraer coodenadas X/Y de Plazas
plazas_x <- plazarefined[18]
plazas_y <- plazarefined[19]

# Fusionar plaza sol X y plazas unicas X
distdf <- cbind(sol_x,sol_y,plazas_x,plazas_y)

# Distancias
distancias <- sqrt((distdf$COORD_X - distdf$sol_x)^2 + (distdf$COORD_Y - distdf$sol_y)^2 )
distancias <- round(distancias,digits=0)

# Asocias distancias a resto de informacion
plazarefined$distancias <- distancias
str(plazarefined)

# Exporting data for geocoding
write.csv(plazarefined,file="distancias_plazas_madrid.csv")

# Importing refined data
refined <- read.xlsx("~/Desktop/rstats/data/distancias_plazas_madrid.xls",1)
str(refined)

geoinfo <- read.csv("~/Desktop/rstats/data/plaza_madrid_geo_info.csv")
str(geoinfo)

# Merging refined + geo info
finaldf <- cbind(refined,geoinfo)
str(finaldf)

# Subsetting those with valid geocode
finaldf <- subset(finaldf,status.code != 602)
str(finaldf)

# Exporting final data
write.csv(finaldf,file="plazas_madrid_geocoded_final.csv",sep=";")



















