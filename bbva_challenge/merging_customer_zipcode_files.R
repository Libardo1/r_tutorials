# Consolidacion de ficheros de customer_zipcodes

# Set working directory
setwd("~/Desktop/innova_BBVA/customer_zipcodes_files")
getwd()

# Read files
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)
df <- ldply(myfiles, data.frame)
df <- df[2:12]
str(df)
head(df)

# Export resulting data frame
write.csv(df,file="customer_zipcodes_total.csv")

