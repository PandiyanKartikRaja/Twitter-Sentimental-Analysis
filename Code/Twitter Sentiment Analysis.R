#Connecting_Libarires
library(twitteR)
library(ROAuth)
require(RCurl)

#Connecting_to_Twitter_api
api_key = "GStibPIrmlNybfNajlIacNBDn"
api_secret<-"jUaT7Bf09FP8HpmCdQicsUwPKpKcKdtOFzaV0b3Uc09IZEhiSh"
access_token<-"212500992-Mrjm52E3BLc8gaoqGXLui8hgLkrezw2gpn58IocI"
access_token_secret<-"VfQhQXYIrCdrWhrFJrD0VZdBbk4DtfSfRc7TXZ1sj0rSb"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#Extracting_Tweets_containg_bitcoin_keyword
bitcoin<-searchTwitter("bitcoin", n=1500)
library(stringr)
library(tm)
library(ggmap)
library(plyr)
library(dplyr)
library(wordcloud)

#Cleanning_Data
df1 <- twListToDF(bitcoin)
myCorpus <- Corpus(VectorSource(df1$text))
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpusCopy <- myCorpus
myCorpus <- tm_map(myCorpus, stemDocument)
myCorpus <- Corpus(VectorSource(myCorpus))

#Creating_Document_matrix
wordFreq <- function(corpus, word) {
  results <- lapply(corpus,
                    function(x) { grep(as.character(x), pattern=paste0("\\<",word)) }
  )
  sum(unlist(results))
}
tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
tdm

#Finding out the most frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq = 100))


#Removing stop words
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),"and", "when", "what", "to", "this","the","that","so","of","it","is","in","at","a","be","by","for","have","on","our","are","i","will","with","you")

#Creating Wordcloud
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
library(wordcloud)
wordcloud(myCorpus ,max.words =150,min.freq=3,scale=c(4,.5),colors=palette())

tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
tdm

#Most frequent words after removing stop words
(freq.terms <- findFreqTerms(tdm, lowfreq = 50))

#Plot of most frequent words
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 50)
df2 <- data.frame(term = names(term.freq), freq = term.freq)

#Calculating the sentiment score
df <- twListToDF(bitcoin)
df <- df[, order(names(df))]
df$created <- strftime(df$created, '%Y-%m-%d')
if (file.exists(paste("bitcoin", '_stack.csv'))==FALSE) write.csv(df, file=paste("bitcoin", '_stack.csv'), row.names=F) 
stack <- read.csv(file=paste("bitcoin", '_stack.csv'))
stack <- rbind(stack, df)
stack <- subset(stack, !duplicated(stack$text))
write.csv(stack, file=paste("bitcoin", '_stack.csv'), row.names=F)

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

pos <- scan('C:\\Program Files\\R\\positivewords.txt', what='character', comment.char=';')
neg <- scan('C:\\Program Files\\R\\negativewords.txt', what='character', comment.char=';')

Dataset <- stack
Dataset$text <- as.factor(Dataset$text)
Dataset$text<- str_replace_all(Dataset$text,"ν ½ν²Έν ½ν²°' "," ")
scores <- score.sentiment(Dataset$text, pos, neg, .progress='text')

write.csv(scores, file=paste("bitcoin", '_scores.csv'), row.names=TRUE)
stat <- scores
stat$created <- stack$created
stat$created <- as.Date(stat$created)
stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))

by.tweet <- group_by(stat, tweet, created)
by.tweet <- summarise(by.tweet, number=n())
write.csv(by.tweet, file=paste("bitcoin", '_opin.csv'), row.names=TRUE)

#Plot of the sentiment analysis
ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
  geom_point(aes(group=tweet, color=tweet), size=4) +
  theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
  
  ggtitle(bitcoin)
