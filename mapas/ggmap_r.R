Library ggmap
=======================

```{r}
library(ggmap)
```
```{r}
data(package="ggmap")
```
```{r}
str(crime)
```
```{r}
map <- get_cloudmademap(api_key = '')
ggmap(map)
```
```{r}
distQueryCheck()
geocodeQueryCheck()
```
```{r}
gc <- geocode('waco,texas')
center <- as.numeric(gc)
ggmap(get_map(location=center,color='bw',scale=2))
```
```{r}
gc <- geocode('duncan hall, rice university')
google <- get_googlemap('rice university', zoom = 15)
ggmap(google) + geom_point(aes(x = lon, y = lat), data = gc, colour = 'red', size = 2)
```
```{r}
bbox <- as.numeric(attr(google, 'bb'))[c(2,1,4,3)]
names(bbox) <- c('left','bottom','right','top')
stamen <- get_stamenmap(bbox, zoom = 15)
ggmap(stamen) + geom_point(aes(x = lon, y = lat), data = gc, colour = 'red', size = 2)
```
```{r}
osm <- get_openstreetmap(bbox, scale = OSM_scale_lookup(15))
ggmap(osm) + geom_point(aes(x = lon, y = lat), data = gc, colour = 'red', size = 2)
```
```{r}
ggmap(get_stamenmap(bbox, zoom = 15, maptype = 'watercolor'))+ geom_point(aes(x = lon, y = lat), data = gc, colour = 'red', size = 2)
```
```{r}
ggmap(get_stamenmap(bbox, zoom = 15, maptype = 'toner'))+ geom_point(aes(x = lon, y = lat), data = gc, colour = 'red', size = 2)
```
```{r}
municatlonlat2 <- read.csv("~/Desktop/Docs/CAT/municatlonlat2.csv")
lonlat <- municatlonlat2
```
```{r}
barcelona <- geocode('Barcelona,Spain')
barcelona
```
```{r}
center <- as.numeric(barcelona)
ggmap(get_map(location=center,scale=2,color='bw',zoom=8))
 ggmap(get_map(location=center,scale=2,color='bw',zoom=8))+ geom_point(aes(x=longitude,y=latitude),data=lonlat)
```
```{r}
eleccions2012 <- read.csv("~/Desktop/Docs/CAT/elecat2012.csv")
ele1 <- eleccions2012
str(ele1)
str(lonlat)
```
```{r}
ele2 <- cbind(ele1,lonlat$longitude,lonlat$latitude)
str(ele2)
```
```{r}
ggmap(get_map(location=center,scale=2,color='bw',zoom=8))+ geom_point(aes(x=lonlat$longitude,y=lonlat$latitude,size=total.votos),data=ele2)
```







