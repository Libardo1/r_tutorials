# Text Analysis with R: Transparency Law in Spain
# Objective: Perform text analysis on a set of 24 texts with independent analysts appearances in the Spanish Congress of Deputies in 2013

# libraries

library(tm)
library(Snowball)
library(ggplot2)
library(reshape)

# Load text files and build a corpus (text document collection)

directory <- DirSource(directory="~/Desktop/rstats/transp_texts")
corpus <- Corpus(directory)

# Inspect the corpus

class(corpus)
length(corpus)
corpus[[1]]

# Refine the corpus

corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removeNumbers)
corpus <- tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus,removeWords,stopwords("spanish"))
corpus <- tm_map(corpus,removePunctuation,preserve_intra_word_dashes = FALSE)

# Build Term Document Matrix and Document Term Matrix

tdm <- TermDocumentMatrix(corpus)
class(tdm)
dim(tdm)
dtm <- DocumentTermMatrix(corpus)
class(dtm)
dim(dtm)

# Word Frequency
# Terms with a minimum frecuency

min_word_freq <- findFreqTerms(tdm, lowfreq=50)
length(min_word_freq)
min_word_freq

# Word associations: Find words correlated with a word

word_aso <- findAssocs(tdm, "transparencia",0.6)
length(word_aso)
word_aso
word_aso_names <- attributes(word_aso)$names
df_word_aso <- as.data.frame(word_aso)
df_word_aso <- cbind(df_word_aso,word_aso_names)
df_word_aso
df_word_aso_t <- transform(df_word_aso,word_aso_names=reorder(word_aso_names,word_aso))
qplot(word_aso,word_aso_names,data=df_word_aso_t)

# Frequency order
# Most frequent terms

top_freq_terms <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
head(top_freq_terms,20)
df_top_freq_terms <- as.data.frame(top_freq_terms)
top_freq_terms_names <- attributes(top_freq_terms)$names
df_top_freq_terms <- cbind(df_top_freq_terms,top_freq_terms_names)
df_top_freq_terms_20 <-head(df_top_freq_terms,20)
df_top_freq_terms_t <- transform(df_top_freq_terms_20,top_freq_terms_names=reorder(top_freq_terms_names,top_freq_terms))
qplot(top_freq_terms,top_freq_terms_names,data=df_top_freq_terms_t)

# Total words per author

total_word_per_author <- sort(colSums(as.matrix(tdm)), decreasing=TRUE)
head(total_word_per_author)
df_total_word_per_author <- as.data.frame(total_word_per_author)
total_word_per_author_names <- attributes(total_word_per_author)$names
df_total_word_per_author <- cbind(df_total_word_per_author,total_word_per_author_names)
le <- length(total_word_per_author_names)
head(df_total_word_per_author,le)
df_total_word_per_author_t <- transform(df_total_word_per_author,total_word_per_author_names=reorder(total_word_per_author_names,total_word_per_author))
qplot(total_word_per_author,total_word_per_author_names,data=df_total_word_per_author_t)


# Word matrices per author 

matriz <- as.matrix(tdm[1:6414,0:24])
matriz_row_names <- row.names(matriz)
matriz_dframe <- as.data.frame(matriz)
matriz_dframe <- cbind(matriz_dframe,matriz_row_names)
str(matriz_dframe)
t(head(matriz_dframe))

# Sort matrices by one author (e.g Soledad Becerril)

df_order_by_author <- matriz_dframe[order(matriz_dframe$soledadbecerril.txt,decreasing=TRUE),]
head(df_order_by_author)
dim(df_order_by_author)
df_order_by_author_melt <- melt(df_order_by_author,id="matriz_row_names")
str(df_order_by_author_melt)
head(df_order_by_author_melt)

# Top words

df_order_by_author_top <- head(df_order_by_author,20)
str(df_order_by_author_top)
df_order_by_author_top_melt <- melt(df_order_by_author_top, id="matriz_row_names")
str(df_order_by_author_top_melt)
head(df_order_by_author_top_melt)
qplot(value,matriz_row_names,data=df_order_by_author_top_melt) + facet_wrap(~ variable)

# Dictionaries

dic <- Dictionary(c("transparencia","ley","información","derecho","acceso","datos"))
ma <- DocumentTermMatrix(corpus, list(dictionary = dic))
inspect(ma)
df <- as.data.frame((as.matrix(ma))) 
str(df)
df
sort(total_word_per_author_names)
name_author <- sort(total_word_per_author_names)
df <- cbind(df,name_author)
df
df_total_word_per_author
names(df_total_word_per_author) <- c("total_word_per_author","name_author")
merge <- merge(df,df_total_word_per_author,by="name_author")
merge

# Plotting

qplot(acceso,name_author,data=merge)
qplot(datos,name_author,data=merge)
qplot(derecho,name_author,data=merge)
qplot(información,name_author,data=merge)
qplot(ley,name_author,data=merge)
qplot(transparencia,name_author,data=merge)
qplot(acceso/total_word_per_author,name_author,data=merge)
qplot(datos/total_word_per_author,name_author,data=merge)
qplot(derecho/total_word_per_author,name_author,data=merge)
qplot(información/total_word_per_author,name_author,data=merge)
qplot(ley/total_word_per_author,name_author,data=merge)
qplot(transparencia/total_word_per_author,name_author,data=merge)

