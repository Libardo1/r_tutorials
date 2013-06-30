# Network of terms
library(XML)
library(tm)
library(igraph)
library(RColorBrewer)
# define twitter search url (following the atom standard)
twitter_url = "http://search.twitter.com/search.atom?"
# encode query
query = URLencode("turkey")
# vector to store results
tweets = character(0)
# paginate 17 times to harvest tweets
for (page in 1:15)
{
  # create twitter search query to be parsed
  twitter_search = paste(twitter_url, "q=turkey", query,"&rpp=100&lang=en&page", page, sep="")
  
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
skipwords = c(stopwords("english"))
corpus = tm_map(corpus, removeWords, skipwords)
# term-document matrix
tdm = TermDocumentMatrix(corpus)
# convert tdm to matrix
termDocMatrix = as.matrix(tdm)
termDocMatrix[1:5,1:5]
# change it to a Boolean matrix
termDocMatrix[termDocMatrix>=1] <- 1
# transform into a term-term adjacency matrix
termMatrix <- termDocMatrix %*% t(termDocMatrix)
# inspect terms numbered 5 to 10
termMatrix[5:10,5:10]
# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)
plot(g, layout=layout.kamada.kawai)
V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
# plot the graph in layout1
plot(g, layout=layout1)
