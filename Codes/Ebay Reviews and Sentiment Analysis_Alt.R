#ebay reviews

eurl <- "https://www.ebay.com/urw/Apple-MacBook-Pro-13-3-128GB-SSD-Intel-Core-i5-8th-Gen-3-90-GHz-8GB-Laptop-Space-Gray-MUHN2LL-A-July-2019-/product-reviews/14032813598?pgn="
Ebay_reviews <- NULL
for (i in 1:5){
  murl <- read_html(as.character(paste(furl,i,sep="")))
  rev <- murl %>%
    html_nodes(".ebay-review-section-r") %>%
    html_text()
  Ebay_reviews <- c(Ebay_reviews,rev)
}
write.table(Ebay_reviews,"Apple-MacBook.txt",row.names=FALSE)

#install.packages(c("tm", "SnowballC", "wordcloud", "RColorBrewer"))

# Load Libraries
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

getwd()

text <- readLines("iphonef.txt")

text1 <- paste("\\", text, sep ="")

corpus <- lapply(text1, FUN = paste, collapse ="")

corpus2 <- gsub(pattern ="\\W", replace ="", corpus)

corpus2 <- gsub(pattern ="\\d", replace ="", corpus2)

corpus2 <- tolower(corpus2)

corpus2 <- removeWords(corpus2, stopwords("english"))

corpus2 <- gsub(pattern = "\\b[A-z]\\b[1]", replace =" ", corpus2)

corpus2 <- stripWhitespace(corpus2)

corpus2 <- removeWords(corpus2, c('using','also','the','is','its'))
corpus2

text2 <- corpus2

##################################################################################

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
library("stringr")

textbag <- str_split(docs, pattern = "\\s+")

class(textbag)

textbag <- unlist(textbag)

class(textbag)

pos <- sum(!is.na(match(textbag, opinion.pos))) # 16 positive words

neg <- sum(!is.na(match(textbag, opinion.neg))) #0 negative words

Sent_score = pos - neg
Sent_score #16

sd(Sent_score)
wordcloud(textbag, min.freq = 1, random.order = FALSE, scale = c(2,0.5), col= rainbow(5))

#which says there is positive reviews on the respective phone

## Bigram ##
install.packages("RWeka")
library(RWeka)
library(wordcloud)
install.packages("ngram")
library(ngram)
install.packages("memuse")
library ( memuse )


text2 <- as.String(text2)
preprocess(text2)

ng <- ngram (corpus2 , n =2)

print (ng , output =" full ")



minfreq_bigram <- 2
bitoken <- NGramTokenizer(corpus2,Weak_control(min = 2 , max = 2))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq, decreasing = True),]

wordcloud(sort_two$bitoken, sort_two$Freq, random.order = F, scale = c(2, 0.35), min.freq = minfreq_bigram , colors = brewer.pal(8, "Dark2"), max.words = 150)

