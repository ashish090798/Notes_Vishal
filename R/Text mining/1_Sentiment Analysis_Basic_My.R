install.packages('NLP')
install.packages('tm')

library(plyr)
library(dplyr)
library(stringr)
library(NLP)  ## Natural Language Processing
library(tm) ## Text Mining
library(ggplot2)

setwd('D:\\Learning\\Analytics Path_R\\Text_Mining')
getwd()

posText = read.delim(file = 'polarity_pos.txt', header = FALSE, stringsAsFactors = FALSE) # text document to analyze
pos.words = read.delim(file = 'positive-words.txt', header = FALSE, stringsAsFactors = FALSE ) # a document of positive words
neg.words = read.delim(file = 'negative-words.txt', header = FALSE, stringsAsFactors = FALSE ) # A document of Negative Words

#View(posText)
#View(pos.words)
#View(neg.words)

posText = posText$V1
pos.words = pos.words$V1 # Taking only text
neg.words = neg.words$V1

## We can add our own positive or negative words as 
##pos.words = c(pos.words, ‘new’,’nice’ ,’good’, ‘horizon’)
##neg.words = c(neg.words, ‘wtf’, ‘behind’,’feels’, ‘ugly’, ‘back’ , ‘shitty’, ‘bad’, ‘no’,’freaking’,’sucks’,’horrible’)

class(posText)
class(pos.words)
class(neg.words)
head(posText)

#########################################################################################################
## This unlisting might not be needed
posText[1:2]
posText = unlist(lapply(posText, function(x) {str_split(x, "\n")}))
class(posText)
posText[1:2]

#########################################################################################################

## Now, We will classify the sentiment of a the text based on the polarity of the individual words. 
## Each word will be given a score of +1 if classified as positive, -1 if negative, and 0 if classified as neutral

## Define function score.sentiment

# Parameters
# sentences: vector of text to score
# pos.words: vector of words of postive sentiment
# neg.words: vector of words of negative sentiment
# .progress: passed to laply() to control of progress bar
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words)
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
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # Few words won't match either to pos.words or neg.words. They will return NA. We don't need NA. We just need True/False
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # final score
    score = sum(pos.matches)  - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress = .progress)
  # The above will return score for each sentence taking all the sentences one by one
  
  # data frame with scores for each sentence(Combining Scores and Sentences)
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
} 

# apply function(score.sentiment) on the text data(posText) under analysis to get scores for each sentence
result = score.sentiment(posText,pos.words,neg.words, .progress='text')


summary(result$score)

## Histogram of scores

hist(result$score,col ="yellow")
##View(result$score)
##View(result)

##Count of sentences as per scores
length(posText) ## Total 10663 sentences
aggregate(result$text, list(result$score), FUN = length )

count(result[result$score>0,]) ## 4396 +ve Comments
count(result[result$score<0,]) ## 3406 -ve Comments
count(result[result$score==0,])## 2861 Neutral Comments

##############################################################################################################################

##https://medium.com/@rohitnair_94843/analysis-of-twitter-data-using-r-part-3-sentiment-analysis-53d0e5359cb8
##https://github.com/gastonstat/Mining_Twitter/blob/master/Rscripts/Sentiment_Analysis_with_Drinks.R





