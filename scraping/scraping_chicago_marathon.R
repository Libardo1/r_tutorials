# Scraping Chicago marathon
# Init URL: http://chicago-history.r.mikatiming.de/2012/?page=1&event=ALL_HISTORY&event_main_group=ALL&lang=EN_CAP&num_results=1000&pid=search&search%5Bnation%5D=%25&search_sort=name
# Last URL: http://chicago-history.r.mikatiming.de/2012/?page=485&event=ALL_HISTORY&event_main_group=ALL&lang=EN_CAP&num_results=1000&pid=search&search%5Bnation%5D=%25&search_sort=name
# Good stacked overflow url: http://stackoverflow.com/questions/1395528/scraping-html-tables-into-r-data-frames-using-the-xml-package

library(plyr)
library(XML)

base_url_1 <- "http://chicago-history.r.mikatiming.de/2012/?page="
base_url_2 <- seq(1:485)
base_url_3 <- "&event=ALL_HISTORY&event_main_group=ALL&lang=EN_CAP&num_results=1000&pid=search&search%5Bnation%5D=%25&search_sort=name"
list_of_urls <- paste(base_url_1,base_url_2,base_url_3,sep="")
head(list_of_urls)

# One URL
first_url <- "http://chicago-history.r.mikatiming.de/2012/?page=1&event=ALL_HISTORY&event_main_group=ALL&lang=EN_CAP&num_results=1000&pid=search&search%5Bnation%5D=%25&search_sort=name" 
raw.data <- readHTMLTable(first_url, warn="F")
df <- as.data.frame(raw.data)
str(df)
head(df)

# Batch Scraping
all_tables <- lapply(list_of_urls, readHTMLTable, stringsAsFactors = FALSE)
df <- ldply (all_tables, data.frame)
str(df)

# Working with output df
colnames(df) <- c("year","event","place.overall","place.gender","place.division","name","city.state","bib","division","age","half.time","finish.time")
df[is.na(df)] <- 0

# Numeric columns
num.cols <- c(1,3,4,5,8,10)
df.num <- df[num.cols]
df.num <- data.frame(lapply(df.num,as.numeric))
str(df.num)

# Non numeric columns
no.num.cols <- c(2,6,7,9)
df.no.num <- df[no.num.cols]
str(df.no.num)

# Time columns
time.cols <- c(11,12)
df.time <- df[time.cols]
str(df.time)
half.time.df <- as.POSIXct(df.time$half.time,format="%H:%M:%S")
finish.time.df <- as.POSIXct(df.time$finish.time,format="%H:%M:%S")


# Merging numeric, non numeric and time
df.tot <- cbind(df.num,df.no.num,half.time.df,finish.time.df)
str(df.tot)
head(df.tot)
df.tot[df.tot==""]  <- NA

# Exporting output
write.csv(df.tot,file="chicago_marathon_historic_results.csv")









