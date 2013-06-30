# Introduction to tm package
library(tm)
data(acq)
inspect(acq[1:2])
# Eliminating extra whitespace
acq <- tm_map(acq, stripWhitespace)
# Convert to lower case
acq <- tm_map(acq, tolower)
summary(acq)
# Dictionaries
Dictionary(c("some", "tokens"))
data(crude)
Dictionary(TermDocumentMatrix(crude))
# Dissimilarity
data(crude)
tdm <- TermDocumentMatrix(crude)
dissimilarity(tdm, method = "cosine")
dissimilarity(crude[[1]], crude[[2]], method = "eJaccard")
# Find associations in a term document matrix
findAssocs(tdm,"oil",0.7)
# Find frequent Terms
findFreqTerms(tdm,2,3)
# Inspect objects
inspect(crude[1:2])
inspect(tdm[1:1])
# Metadata
data("crude")
meta(crude[[1]])
DublinCore(crude[[1]])
meta(crude)
# Names
data("crude")
tdm <- TermDocumentMatrix(crude)[1:10,1:20]
rownames(tdm)
colnames(tdm)
dimnames(tdm)
Docs(tdm)
Terms(tdm)
# Number
data("crude")
tdm <- TermDocumentMatrix(crude)[1:10,1:20]
ncol(tdm)
nrow(tdm)
dim(tdm)
nDocs(tdm)
nTerms(tdm)
# Remove punctuation
removePunctuation(crude[[14]])
# Remove sparseterms
removeSparseTerms(tdm, 0.2)
# Remove words
removeWords(crude[[1]], stopwords("english"))
# Search full text
searchFullText(crude[[1]], "co[m]?pany")
# Stem completion
stemCompletion(c("compan", "entit", "suppl"), crude)
(s <- stemDocument(crude[[1]]))
stemCompletion(s, crude)
# Stem words
stemDocument(crude[[1]])
# Strip white space
stripWhitespace(crude[[1]])
# inspect tdm
inspect(tdm[c("price", "texas"),c("127","144","191","194")])
# term frequency
termFreq(crude[[14]])
# filter and indexing
tm_filter(crude, FUN = searchFullText, "company")
tm_index(crude, FUN = searchFullText, "company")
# Intersection between docs and words
tm_intersect(crude[[1]], c("crude", "oil"))
