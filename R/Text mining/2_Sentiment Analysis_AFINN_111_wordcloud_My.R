install.packages('NLP')
install.packages('tm')
install.packages('wordcloud')

library(plyr)
library(dplyr)
library(stringr)
library(NLP)  ## Natural Language Processing
library(tm) ## Text Mining
library(ggplot2)
library(wordcloud)
library(RColorBrewer)


setwd('D:\\Learning\\Analytics Path_R\\Text_Mining')
getwd()

posText = read.delim(file = 'polarity_pos.txt', header = FALSE, stringsAsFactors = FALSE) # text document to analyze
#View(posText)
posText = posText$V1
class(posText)
head(posText)

#########################################################################################################
## This unlisting might not be needed
posText[1:2]
posText = unlist(lapply(posText, function(x) {str_split(x, "\n")}))
class(posText)
posText[1:2]
#########################################################################################################

## Define function score.sentiment
# Parameters
# sentences: vector of text to score
# pos.words: vector of words of postive sentiment
# neg.words: vector of words of negative sentiment
# .progress: passed to laply() to control of progress bar
score.sentiment <- function(sentences, posterms, negterms, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, posterms, negterms)
  {
    #1.Remove punctuation(the marks, such as full stop, comma, and brackets, used in writing to separate sentences)
    ## Using R’s regex-driven global substitute, gsub()
    sentence <- gsub('[[:punct:]]', "", sentence) 
    #2.Remove control characters(a non-printing charactera that does not represent a written symbol)
    sentence <- gsub('[[:cntrl:]]', "", sentence) 
    
    #3. Remove Digits
    sentence <- gsub('\\d+', "", sentence)  
    sentence <- tolower(sentence)
    
    #4. split sentence into words with str_split (stringr package)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    
    
    #5. compare words to the dictionaries of positive & negative terms
    pos.matches = match(words, posterms)
    neg.matches = match(words, negterms)
    
    # Few words won't match either to pos.words or neg.words. They will return NA. We don't need NA. We just need True/False
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # final score
    score = sum(pos.matches)  - sum(neg.matches)
    return(score)
  }, posterms, negterms, .progress = .progress)
  # The above will return score for each sentence taking all the sentences one by one
  
  # data frame with scores for each sentence(Combining Scores and Sentences)
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
} 

############################Now load the positive and ngative terms for sentiment analysis#############################
afinn_list = read.delim('AFINN-111.txt', header = FALSE, stringsAsFactors = FALSE)
head(afinn_list)
names(afinn_list) = c('word', 'score')
afinn_list$word = tolower(afinn_list$word)

#categorize words as very negative to very positive and add some movie-specific words
#vNegTerms = afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
#negTerms = c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1], "second-rate", "moronic", "third-rate", "flawed", "juvenile", "boring", "distasteful", "ordinary", "disgusting", "senseless", "static", "brutal", "confused", "disappointing", "bloody", "silly", "tired", "predictable", "stupid", "uninteresting", "trite", "uneven", "outdated", "dreadful", "bland")
#posTerms = c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1], "first-rate", "insightful", "clever", "charming", "comical", "charismatic", "enjoyable", "absorbing", "sensitive", "intriguing", "powerful", "pleasant", "surprising", "thought-provoking", "imaginative", "unpretentious")
#vPosTerms = c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4], "uproarious", "riveting", "fascinating", "dazzling", "legendary")

negterms = c(afinn_list$word[afinn_list$score < 0 ] )
posterms = c(afinn_list$word[afinn_list$score > 0 ] )

posterms[1:2]
negterms[80:90]

# apply function(score.sentiment) on the text data(posText) under analysis to get scores for each sentence
posResult = score.sentiment(posText, posterms,   negterms)

View(posResult)
summary(posResult$score)

## Histogram of scores

hist(posResult$score,col ="yellow")
write.csv(posResult,"results.csv", row.names = F)
##View(result$score)
##View(result)

##Count of sentences as per scores
length(posText) ## Total 10663 sentences
aggregate(posResult$text, list(posResult$score), FUN = length )

count(result[posResult$score>0,]) ## 4624 +ve Comments
count(result[posResult$score<0,]) ## 2291 -ve Comments
count(result[posResult$score==0,])## 3748 Neutral Comments

##############################################################################################################################

## Individual Sentence Testing

result1 = score.sentiment("this is an awesome phone, but poor battery", posterms, negterms)
result1$score ## Neutral Sentence

result2 = score.sentiment("this is an awesome phone, but very poor poor battery", posterms, negterms)
result2$score ## Negative Sentence 

###############################################################################################################################

## Data Exploration on Negative Reviews

table(posResult$score)

### subset all the reviews where the score is less than -2
negativereviews = posResult[posResult$score <= -1,]

table(negativereviews$score)

library(tm)

pos_corpus <- Corpus(VectorSource(negativereviews$text))
##head(pos_corpus)
##View(pos_corpus)

list_stop = c("movie", "film") ## 2 stop words introduced

# clean up the corpus using tm_map()
corpus_clean <- tm_map(pos_corpus, tolower)
##corpus_clean = tm_map(corpus_clean, PlainTextDocument)
## When I included the above term , then at the time of DTM formation  it threw error
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords('english'))
corpus_clean <- tm_map(corpus_clean, removeWords, list_stop) ## additional stopwords introduced manually
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

### creating Term Document matrix 

library(tm)
dtm_pos = DocumentTermMatrix(corpus_clean, control = list(removePunctuation = TRUE,
                                                          stopwords = TRUE))
?DocumentTermMatrix
tdm_pos = TermDocumentMatrix(corpus_clean)
View(tdm_pos)

m <- as.matrix(tdm_pos) ## Will convert the tdm(term document matrix) into 
##View(m)
m2 = rowSums(m)  ##Counting the occurances of each word e.g. 'one' appeared 173 times, bad appeared 154 times etc
class(m2)
#View(m2)

v <- sort(rowSums(m),decreasing=TRUE) ## sorting m2
head(v)
class(v)
##View(names(v))
d <- data.frame(word = names(v),freq=v) # converting v to a dataframe
head(d, 25)

library(wordcloud)
wordcloud(words=d$word, 
          freq = d$freq, 
          min.freq = 20,
          max.words = 30,
          random.order = F,
          rot.per = 0.35,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(4,0.2))




### word correlations 
library(tm)
library(NLP)

findAssocs(dtm_pos, terms = "characters",corlimit = 0.1)

findAssocs(dtm_pos, terms = "story",corlimit = 0.1)

?wordcloud

??findAssocs
###### check the same on another file 

scores <- score.sentiment(negText, posTerms, negTerms, .progress='text')

table(scores$score)
write.csv(scores, 'scores_sentiment.csv', row.names=TRUE) #save evaluation results into the file













