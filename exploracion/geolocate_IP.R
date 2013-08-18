# Geolocate IP addresses
# http://heuristically.wordpress.com/2013/05/20/geolocate-ip-addresses-in-r/
# Define the function
freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
  if (1 == length(ip))
  {
    # a single IP address
    require(rjson)
    url <- paste(c("http://freegeoip.net/json/", ip), collapse='')
    ret <- fromJSON(readLines(url, warn=FALSE))
    if (format == 'dataframe')
      ret <- data.frame(t(unlist(ret)))
    return(ret)
  } else {
    ret <- data.frame()
    for (i in 1:length(ip))
    {
      r <- freegeoip(ip[i], format="dataframe")
      ret <- rbind(ret, r)
    }
    return(ret)
  }
}
# See how it works
freegeoip("184.26.100.110")
# Multiples IP addresses
freegeoip(c('106.78.232.100','174.6.153.88'))
