# Install
#install.packages(c("tm", "SnowballC", "wordcloud", "RColorBrewer"))

# Load Libraries
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

getwd()

text <- readLines("AppleIphone11Pro.txt")

docs <- Corpus(VectorSource(text))

docs <- tm_map(docs, content_transformer(tolower))

docs <- tm_map(docs, removePunctuation)

docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, removeWords, stopwords("english"))

docs <- tm_map(docs, removeWords, c("big", "small")) 

docs <- tm_map(docs, stemDocument)

#Replacing "/", "@" and "|" with space

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")

docs <- tm_map(docs, toSpace, "@")

docs <- tm_map(docs, toSpace, "\\|")

docs <- tm_map(docs, stripWhitespace)

#Build a Term-document Matrix

doc_mat <- TermDocumentMatrix(docs)

m <- as.matrix(doc_mat)

v <- sort(rowSums(m), decreasing = TRUE)

d_BioC <- data.frame(word = names(v), freq = v)

head(d_BioC, 5)

#Generate first word cloud

wordcloud(docs, random.order=F,max.words=100, col=rainbow(5), scale=c(2.5,0.5))

Rcran_list <- findFreqTerms(doc_mat, lowfreq = 13)
findFreqTerms(doc_mat, lowfreq = 13)

findAssocs(doc_mat, terms = "model", corlimit = 0.05)

head(d_BioC, 10)

barplot(d_BioC[1:25,]$freq, 
        las = 2, 
        names.arg = d_BioC[1:25,]$word,
        col ="lightyellow", 
        main ="Most Frequent Words From R-cran Packages",
        ylab = "Word Count")

# Sentiment analysis


opinion.pos <- scan('positive-words.txt', what ='character', comment.char = ";")
opinion.neg <- scan('negative-words.txt', what = 'character', comment.char = ";")

library("qdapRegex")

textbag <- str_split(docs, pattern = "\\s+")

class(textbag)

textbag <- unlist(textbag)

class(textbag)

pos <- sum(!is.na(match(textbag, opinion.pos))) # 315 positive words

neg <- sum(!is.na(match(textbag, opinion.neg))) #135 negative words

Sent_score = pos - neg
Sent_score #180

sd(Sent_score)
wordcloud(textbag, min.freq = 1, random.order = FALSE, scale = c(3,0.5), col= rainbow(5))

#which says there is positive reviews on the respective phone
