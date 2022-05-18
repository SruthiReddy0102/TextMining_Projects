# install socialmedialab package to load the youtube data

install.packages("vosonSML")
library(vosonSML)

# Google API KEY

apikey <- "XXXXXXXXXXXXXXXXX"
youtubeAuth <- Authenticate("youtube",apikey)

video <- c('5DW0ixWjn7U','8K8N1Hw6IMg')
ytdata <- Collect(youtubeAuth, video, writeToFile =TRUE)

str(ytdata)

getwd()

write.csv(ytdata, "C:\\Users\\pradu\\Documents\\hb.csv", row.names = F)

#read youtube data

data <- read.csv(file.choose(), header = TRUE)
View(data)
str(data)

#Building corpus

library(tm)

corpus <- iconv(data$Comment)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#cleansing the data

corpus <- tm_map(corpus, tolower)

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeNumbers)

corpus <- tm_map(corpus, removeWords, stopwords('english'))

corpus <- tm_map(corpus, stripWhitespace)

#Term document Matrix

tdm <- TermDocumentMatrix(corpus)
tdm

tdm <- as.matrix(tdm)
tdm[1:10,1:10] #first 10 rows and 10 columns

#Bar plot

w <- rowSums(tdm) # to check each word term frequency

w <- subset(w, w>=5)

barplot(w, las = 2 , col = rainbow(50))

#wordcloud

library(wordcloud)

w <- sort(rowSums(tdm),decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w), freq = w, colors = rainbow(5), max.words = 65, random.order = F, min.freq = 5, scale = c(5,0.3),rot.per = 0.3)


opinion.pos <- scan('positive-words.txt', what ='character', comment.char = ";")
opinion.neg <- scan('negative-words.txt', what = 'character', comment.char = ";")

library("qdapRegex")
library("stringr")

textbag <- str_split(corpus, pattern = "\\s+")

class(textbag)

textbag <- unlist(textbag)

class(textbag)

pos <- sum(!is.na(match(textbag, opinion.pos))) # 315 positive words

neg <- sum(!is.na(match(textbag, opinion.neg))) #135 negative words

Sent_score = pos - neg
Sent_score #222

sd(Sent_score)
wordcloud(textbag, min.freq = 1, random.order = FALSE, scale = c(3,0.5), col= rainbow(5))


