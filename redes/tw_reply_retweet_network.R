# Network of Replies and Retweets on Twitter
# Follow this tutorial to manage Twitter API authentication process: https://github.com/pablobarbera/workshop 
library(twitteR)
library(ROAuth)
library(igraph)

# load library and OAuth
load("my_oauth")
registerTwitterOAuth(my_oauth)

# Search Tweets
tweets <- searchTwitter("Spanair", n=1000,language="es")
class(tweets)
head(tweets)

# Converting to data frame
df_tweets <- twListToDF(tweets)
str(df_tweets)

# Extracting and merging reply columns
replyto <- df_tweets[3]
user <- df_tweets[10]
df_reply <- cbind(replyto,user)
str(df_reply)
head(df_reply)

# Filtering reply tweets
replies <- subset(df_reply,replyToSN!="<NA>")
str(replies)
head(replies)
text_1 <- subset(df_tweets,replyToSN!="<NA>")
str(text_1)
ind <- c(1,3,10)
replies_def <- text_1[ind]
str(replies_def)
head(replies_def,10)

# Building and plotting a graph object
df.g <- graph.data.frame(d = replies, directed = FALSE)
plot(df.g, vertex.label = V(df.g)$name)
plot(df.g, vertex.size=4, vertex.label=V(df.g)$name,vertex.color="orange",vertex.label.color="black", vertex.frame.color="white", edge.color="grey",edge.arrow.size=0.01, rescale=TRUE,vertex.label=NA, vertex.label.dist=0.0,vertex.label.cex=0.5, add=FALSE, vertex.label.font=.001)


# Retweets
rts <- grep("^rt @[a-z0-9_]{1,15}", tolower(df_tweets$text), perl=T, value=T)
rt.sender <- tolower(as.character(df_tweets$screenName[grep("^rt @[a-z0-9_]{1,15}", tolower(df_tweets$text), perl=T)]))
rt.receiver <- gsub("^rt @([a-z0-9_]{1,15})[^a-z0-9_]+.*$", "\\1", rts, perl=T)

rt.sender[rt.sender==""] <- "<NA>"
rt.receiver[rt.receiver==""] <- "<NA>"

# Reweets data frame
rts.df <- data.frame(rts,rt.sender, rt.receiver)
str(rts.df)
ddf <- rts.df[c(2:3)]
str(ddf)
m <- as.matrix(ddf)
head(m,10)
rts.df.2 <- data.frame(rt.sender,rt.receiver) 

# Retweets plotting
rts.g <- graph.data.frame(rts.df.2, directed=T)
plot(rts.g, vertex.label = V(rts.g)$name)
plot(rts.g ,vertex.size=4, vertex.label=V(rts.g)$name,vertex.color="red",vertex.label.color="black", vertex.frame.color="white", edge.color="grey",edge.arrow.size=0.01, rescale=TRUE, vertex.label=NA, vertex.label.dist=0.0,vertex.label.cex=0.5, add=FALSE, vertex.label.font=.001)


# Exporting to graphml file
write.graph(rts.g, file="rts.graphml", format="graphml")
