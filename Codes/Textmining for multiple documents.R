# Building a Model with using multiple documents
# Using Amazon and Flipkart Reviews for iphone and oneplus mobile

# Importing Required Libraries
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("stringr")

# Importing the folder of the text files
file.choose()
folder <- "C:\\Data Science\\textmining"
folder

# Flitering the folder if there are any unwanted files
# list.files(path=folder)
# filelist <- list.files(path = folder, pattern = ".txt") # Takes only .txt files and drops other unwanted files like excel,words r ppt
# paste(folder,"\\", filelist) # Gives a folder with only .txt files but spaces at "\\"
# paste(folder,"\\", filelist, sep="") # Removes spaces

# Combining the text files
filelist <- list.files(path = folder)
filelist
paste(folder,"\\", filelist, sep="")
typeof(filelist)

a <- lapply(folder, FUN=readLines)

corpus <- lapply(a, FUN=paste, collapse="")
corpus

corpus2 <- gsub(pattern = "\\W", replace="",corpus) # Removing punctutaions
corpus2 <- gsub(pattern = "\\d", replace="",corpus2) # Removing digits
corpus2 <- tolower(corpus2) # Converting all to lower case letters
removeWords(corpus2, stopwords("english")) 
corpus2

corpus2 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", corpus2)
corpus2 <- stripWhitespace(corpus2)
corpus2

# Word Cloud
wordcloud(corpus2)
wordcloud(corpus2, random.order = FALSE)
wordcloud(corpus2, random.order = FALSE, col = rainbow(3))

# Comparison of the text mining file
corpus3 <- Corpus(VectorSource(corpus2))
corpus3
tdm <- TermDocumentMatrix(corpus3)
tdm

m <- as.matrix(tdm)
colnames(m)
colnames(m) <- ("iphone","oneplus")
comparison.cloud(m)

# Sentiment Analysis
pos <- scan('positive-words.txt', what = 'character', comment = ';')
neg <- scan('negative-words.txt', what = 'character', comment = ';')

words <- str_split(corpus2, pattern = "\\s+")
positive <- lapply(words,function(x){sum(!is.na(match(x,pos)))}) # Checking positive words in each document
negative <- lapply(words,function(x){sum(!is.na(match(x,neg)))}) # Checking negative words in each document

Analysis <- lapply(words,function(x){sum(!is.na(match(x,pos))) - sum(!is.na(match(x,neg)))})
mean(Analysis)
sd(Analysis)
