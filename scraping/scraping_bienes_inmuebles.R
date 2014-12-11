# Scraping Bienes Inmuebles de Admon Española #

#---------------------------------------------#

# Packages
library(plyr)
library(XML)

# First URL
url <- "http://transparencia.gob.es/es_ES/buscar/contenido/cibi/CIBI_DPTO12"
raw.data <- readHTMLTable(url, warn="F")

# List of URLs
se <- seq(12,20)
base_url <- "http://transparencia.gob.es/es_ES/buscar/contenido/cibi/CIBI_DPTO"
list_of_urls <- paste(base_url,se,sep="")

# Scraping function
scrafoo <- function(uri){
  re <- readHTMLTable(uri,warn="F")
  return(re)
}

# Single test
t <- scrafoo("http://transparencia.gob.es/es_ES/buscar/contenido/cibi/CIBI_DPTO12")

# Batch test, from dpto 12 to dpto 20
batch <- all_tables <- lapply(list_of_urls, scrafoo)
df <- ldply (all_tables, data.frame)
names(df) <- c("Tipo","Localizacion","Uso","Ministerio")

# Other dptos
a <- scrafoo("http://transparencia.gob.es/es_ES/buscar/contenido/cibi/CIBI_DPTO23")
b <- scrafoo("http://transparencia.gob.es/es_ES/buscar/contenido/cibi/CIBI_DPTO25")
c <- scrafoo("http://transparencia.gob.es/es_ES/buscar/contenido/cibi/CIBI_DPTO26")
d <- scrafoo("http://transparencia.gob.es/es_ES/buscar/contenido/cibi/CIBI_DPTO27")
other <- c(a,b,c,d)
df2 <- ldply (other, data.frame)
df2[1] <- NULL

str(df2)


# Mergind results
dftot <- rbind(df,df2)
head(dftot)
str(dftot)

# Exporting results
write.csv(dftot,file="edificios_publicos_2014.csv")
















