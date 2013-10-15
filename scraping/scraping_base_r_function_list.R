# Scraping List of R packages functionalities
library(XML)
library(RCurl)
library(tm)
library(plyr)

# Reading Source code
url <- "http://stat.ethz.ch/R-manual/R-devel/library/utils/html/00Index.html"
url <- htmlParse(url)

# Funtion texts
text <- getNodeSet(url,"//table//tr/td/text()")
text <- unlist(text)
text <- sapply(text,xmlValue)
head(text,10)
length(text)

name <- getNodeSet(url,"//table/tr/td/a/text()")
name <- unlist(name)
name <- sapply(name,xmlValue)
head(name,10)
length(name)

# Enlaces a páginas individuales
# URL de referencia: http://stat.ethz.ch/R-manual/R-devel/library/base/html/abbreviate.html
url_root <- "http://stat.ethz.ch/R-manual/R-devel/library/utils/html/"

link <- getNodeSet(url,"//table/tr/td/a/@href")
link <- unlist(link)

link <- paste(url_root,link,sep="")
head(link,5)

# Merging all data into a single data frame
df <- cbind(text,name,link)
head(df)
dim(df)

# Write CSV to export results
write.csv(df,file="R_functions_utils.csv")

