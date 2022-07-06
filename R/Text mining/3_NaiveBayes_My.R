install.packages('e1071')

library(e1071)
library(tm)
library(NLP)

rm(list=ls()) ## Clear the environment

getwd()
setwd("D:/Learning/Analytics Path_R/Text_Mining")

sms_raw = read.csv('sms_spam.csv', stringsAsFactors = F)

## Structure of SMS Data

str(sms_raw) # 2 columns. 1 is text and other is type(ham, spam)
head(sms_raw)

# convert spam/ham to factor.
sms_raw$type <- factor(sms_raw$type)

prop.table(table(sms_raw$type))

# examine the type variable more carefully
str(sms_raw$type)
prop.table(table(sms_raw$type))

# build a corpus using the text mining (tm) package
sms_corpus = Corpus(VectorSource(sms_raw$text))

# examine the sms corpus
print(sms_corpus) ## 5559 documnets

# clean up the corpus using tm_map()

corpus_clean <- tm_map(sms_corpus, tolower)
##corpus_clean =  tm_map(corpus_clean, PlainTextDocument)
corpus_clean = tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("english"))
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

inspect(sms_corpus[1:3])
head(corpus_clean, 3)
class(corpus_clean)

# create a document-term sparse matrix

sms_dtm <- DocumentTermMatrix(corpus_clean)
length(sms_raw$type) # total 5559 rows

## Let's divide data into test and train

sms_raw_train <- sms_raw[1:4169, ]    ## Dividing the raw data
sms_raw_test  <- sms_raw[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]    ## Dividing the document Term Matrix
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169] ## Dividing the Corpus
sms_corpus_test  <- corpus_clean[4170:5559]

library(e1071)
# word cloud visualization

library(wordcloud)

wordcloud(sms_corpus_train, min.freq = 50, random.order = FALSE)

# subset the training data into spam and ham groups
spam <- subset(sms_raw_train, type == "spam")
ham  <- subset(sms_raw_train, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(4,0.2), color = brewer.pal(8, 'Dark2'))
wordcloud(ham$text, max.words = 40)


## only spam records corpus 

head(spam)


spam_corpus <- Corpus(VectorSource(spam$text))

spam_clean <- tm_map(spam_corpus, tolower)
##spam_clean =  tm_map(spam_clean, PlainTextDocument)
spam_clean <- tm_map(spam_clean, removeNumbers)
spam_clean <- tm_map(spam_clean, removeWords, stopwords("english"))

spam_clean <- tm_map(spam_clean, removePunctuation)
spam_clean <- tm_map(spam_clean, stripWhitespace)

inspect(spam_clean)

spam_dtm = TermDocumentMatrix(spam_clean)

m=as.matrix(spam_dtm)
v = sort(rowSums(m), decreasing = T)
d=data.frame(word=names(v), freq=v)
head(d,25)

## Making wordcloud on spam Data to check the most frequent words

wordcloud(words = d$word, freq = d$freq, min.freq = 40,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(6, "Dark2"))

mywords = as.character(d[1:20,1])
mywords

findAssocs(spam_dtm, terms = "call",corlimit = 0.2 )

findAssocs(spam_dtm, terms = "free",corlimit = 0.2 )

findAssocs(spam_dtm, terms = c("free","prize"),corlimit = 0.2 )
mywords

# indicator features for frequent words

myTerms <- c("amount", "free", "claim", "prize", "won", "draw", "latest", "win", "stop",  "contact", "send")

sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = myTerms))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = myTerms))

