# Mining Twitter data with R
# https://sites.google.com/site/miningtwitter/home
# Getting Data
# XML
library(XML)
# define base twitter search url (following the atom standard)
twitter_url = "http://search.twitter.com/search.atom?"
# create twitter search query to be parsed
twitter_search = paste(twitter_url, "q=datamining", sep="")
# let's parse with xmlParseDoc
results = xmlParseDoc(twitter_search, asText=FALSE)
# search term "datamining"
http://search.twitter.com/search.atom?q=datamining
# search term "datamining" in spanish
http://search.twitter.com/search.atom?q=datamining&lang=es
# search term "data mining OR analytics" and display 50 results per page
http://search.twitter.com/search.atom?q=data%20mining%20OR%20anaytics&rpp=50
# Processing
# define base twitter search url (following the atom standard)
twitter_url = "http://search.twitter.com/search.atom?"
# create twitter search query to be parsed
twitter_search = paste(twitter_url, "q=jpd13", sep="")
# let's parse with xmlParseDoc
tweets = xmlParseDoc(twitter_search, asText=FALSE)
# extracting titles
titles = xpathSApply(tweets, "//s:entry/s:title", xmlValue,namespaces = c('s'='http://www.w3.org/2005/Atom'))
authors = xpathSApply(tweets, "//s:entry/s:author/s:name", xmlValue,namespaces = c('s'='http://www.w3.org/2005/Atom'))
# Talking about
# Two terms network
library(XML)
library(tm)
library(igraph)
library(RColorBrewer)
# define twitter search url (following the atom standard)
twitter_url = "http://search.twitter.com/search.atom?"
# encode query
query = URLencode("jpd13")
# vector to store results
tweets = character(0)
# paginate 17 times to harvest tweets
for (page in 1:17)
{
  # create twitter search query to be parsed
  twitter_search = paste(twitter_url, "q=", query,"&rpp=100&lang=es&page", page, sep="")
  
  # let's parse with xmlParseDoc
  tmp = xmlParseDoc(twitter_search, asText=FALSE)
  
  # extract titles
  tweets = c(tweets, xpathSApply(tmp, "//s:entry/s:title",xmlValue, namespaces=c('s'='http://www.w3.org/2005/Atom')))
}
results = tweets
# remove retweet entities
results = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", results)
# remove at people
results = gsub("@\\w+", "", results)
# remove punctuation
results = gsub("[[:punct:]]", "", results)
# remove numbers
results = gsub("[[:digit:]]", "", results)
# remove html links
results = gsub("http\\w+", "", results)
# remove unnecessary spaces
results = gsub("[ \t]{2,}", "", results)
results = gsub("^\\s+|\\s+$", "", results)
# define "tolower error handling" function 
tryTolower = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using tryTolower with sapply 
results = sapply(results, tryTolower)
names(results) = NULL
# remove empty results (if any)
results = results[results != ""]
# create corpus
corpus = Corpus(VectorSource(results))
# remove stopwords
skipwords = c(stopwords("spanish"))
corpus = tm_map(corpus, removeWords, skipwords)
# term-document matrix
tdm = TermDocumentMatrix(corpus)
# convert tdm to matrix
m = as.matrix(tdm)
# word counts
wc = rowSums(m)
# get those words above the 3rd quantile
lim = quantile(wc, probs=0.5)
good = m[wc > lim,]
# remove columns (docs) with zeroes
good = good[,colSums(good)!=0]
# adjacency matrix
M = good %*% t(good)
# set zeroes in diagonal
diag(M) = 0
# graph
g = graph.adjacency(M, weighted=TRUE, mode="undirected",add.rownames=TRUE)
# layout
glay = layout.fruchterman.reingold(g)
# let's superimpose a cluster structure with k-means clustering
kmg = kmeans(M, centers=8)
gk = kmg$cluster
# create nice colors for each cluster
gbrew = c("red", brewer.pal(8, "Dark2"))
gpal = rgb2hsv(col2rgb(gbrew))
gcols = rep("", length(gk))
for (k in 1:8) {
  gcols[gk == k] = hsv(gpal[1,k], gpal[2,k], gpal[3,k], alpha=0.5)
}
# prepare ingredients for plot
V(g)$size = 10
V(g)$label = V(g)$name
V(g)$degree = degree(g)
V(g)$label.cex = 1.5 * log10(V(g)$degree)
V(g)$label.color = hsv(0, 0, 0.2, 0.55)
V(g)$frame.color = NA
V(g)$color = gcols
E(g)$color = hsv(0, 0, 0.7, 0.3)
#  plot
plot(g, layout=glay)
title("\nGraph of tweets about jpd13 and okfn",col.main="gray40", cex.main=1.5, family="serif")
# Frequencies
# Frequency analysis - word - "dataviz"
# load packages
library(XML)
library(tm)
library(ggplot2)
# define twitter search url (following the atom standard)
twitter_url = "http://search.twitter.com/search.atom?"
# vector to store results
results = character(0)
# paginate 20 times to harvest tweets
for (page in 1:10)
{
  # create twitter search query to be parsed
  # tweets in english containing 'dataviz'
  twitter_search = paste(twitter_url, "q=turkey","&rpp=100&lang=en&page", page, sep="")
  
  # let's parse with xmlParseDoc
  tmp = xmlParseDoc(twitter_search, asText=FALSE)
  
  # extract titles
  results = c(results, xpathSApply(tmp, "//s:entry/s:title", xmlValue,namespaces=c('s'='http://www.w3.org/2005/Atom')))
}
# how many tweets
length(results)
# characters per tweet
chars_per_tweet = sapply(results, nchar)
summary(chars_per_tweet)
# split words
words_list = strsplit(results, " ")
# words per tweet
words_per_tweet = sapply(words_list, length)
# barplot
barplot(table(words_per_tweet), border=NA,main="Distribution of words per tweet", cex.main=1)
# length of words per tweet
wsize_per_tweet = sapply(words_list, function(x) mean(nchar(x)))
# barplot
barplot(table(round(wsize_per_tweet)), border=NA,xlab = "word length in number of characters",main="Distribution of words length per tweet", cex.main=1)
# how many unique words per tweet
uniq_words_per_tweet = sapply(words_list, function(x) length(unique(x)))
# barplot
barplot(table(uniq_words_per_tweet), border=NA,main="Distribution of unique words per tweet", cex.main=1)
# how many hashtags per tweet
hash_per_tweet = sapply(words_list, function(x) length(grep("#", x)))
table(hash_per_tweet)
prop.table(table(hash_per_tweet))
# how many @mentions per tweet
ats_per_tweet = sapply(words_list, function(x) length(grep("@", x)))
table(ats_per_tweet)
prop.table(table(ats_per_tweet))
# how many http links per tweet
links_per_tweet = sapply(words_list, function(x) length(grep("http", x)))
table(links_per_tweet)
prop.table(table(links_per_tweet))
# data frame
icedf = data.frame(chars=chars_per_tweet,words = words_per_tweet,lengths = wsize_per_tweet,uniqs = uniq_words_per_tweet,hashs = hash_per_tweet,ats = ats_per_tweet,links = links_per_tweet)
# words -vs- chars
ggplot(icedf, aes(x=words, y=chars)) + geom_point(colour="gray20", alpha=0.2) + stat_smooth(method="lm") + labs(x="number of words per tweet", y="number of characters per tweet") + opts(title = "Tweets about 'icecream' \nNumber of words -vs- Number of characters", plot.title = theme_text(size=12))
# words -vs- word length
ggplot(icedf, aes(x=words, y=lengths)) + geom_point(colour="gray20", alpha=0.2) + stat_smooth(method="lm") + labs(x="number of words per tweet", y="size of words per tweet") + opts(title = "Tweets about 'icecream' \nNumber of words -vs- Length of words", plot.title = theme_text(size=12))
# unique words in total
uniq_words = unique(unlist(words_list))
# lexical diversity
length(uniq_words) / length(unlist(words_list))
# most frequent words
mfw = sort(table(unlist(words_list)), decreasing=TRUE)
# top-20 most frequent
top20 = head(mfw, 20)
# barplot
barplot(top20, border=NA, las=2, main="Top 20 most frequent terms", cex.main=1)
