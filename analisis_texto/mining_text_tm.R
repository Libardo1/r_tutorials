# Mining text data, package tm
library(tm)
data(package="tm")
data(crude)
data(reuters)
inspect(crude[1:2])
dtm <- DocumentTermMatrix(crude)
inspect(dtm[1:5,100:105])
findFreqTerms(dtm, 5)
findAssocs(dtm, "opec", 0.8)
inspect(removeSparseTerms(dtm, 0.4))
# Dictionary
(d <- Dictionary(c("prices", "crude", "oil")))
inspect(DocumentTermMatrix(crude, list(dictionary = d)))
