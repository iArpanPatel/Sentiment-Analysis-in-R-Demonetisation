#Load Libraries
library(twitteR)
library(ROAuth)
library(ggplot2)

#Set Twitter Application Authantication

api_key <- "VdzN4TuGEbusw30z2rGj863bn"
api_secret <- "TyXwN4ggbhzukkLwHPygZCqYviWBZ5Hcj7ZFWsHMh6JLritvcQ"
access_key <- "311872394-FrUf8T6T1vQr8djpkKLdw8Feyd1Iq9dlvVBj8PCK"
access_secret <- "zUaPUA5Kgm2rIXdeQREHYNzKUZ9IQXRluSWZFYzhSVPRP"
setup_twitter_oauth(api_key, api_secret, access_key, access_secret)

#Search Twitter for the Tweets
Demonetisation.list <- searchTwitter('#DeMonetisation', n=1000)  
Demonetisation.df = twListToDF(Demonetisation.list) 

summary(Demonetisation.df)
setwd("C:/R/01Sentiment_analysis/Demonetisation")
getwd()

write.csv(tweets.df, "Demonetisation_tweets.csv")

# Import all other libraries
library (plyr)
library (stringr)

#Generate the function
score.sentiment = function(sentences, pos.words, neg.words,.progress='none')  
{  
  require(plyr)  
  require(stringr)       

  good.smiley <- c(":)")
  bad.smiley <- c(":(",";)",":'",":P") 
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {  
    
    # clean up sentences with R's regex-driven global substitute, gsub():  
    
    sentence = gsub(":)", 'awsum', sentence)
    
    sentence = gsub('[[:punct:]]', '', sentence)  
    
    sentence = gsub('[[:cntrl:]]', '', sentence)  
    
    sentence = gsub('\\d+', '', sentence)  
    
    sentence = tolower(sentence)  
    
    
    word.list = str_split(sentence, '\\s+')  
    
    words = unlist(word.list)  
    
    pos.matches = match(words, pos.words)  
    neg.matches = match(words, neg.words)  
    
    pos.matches = !is.na(pos.matches)  
    
    neg.matches = !is.na(neg.matches)  
    
    score = sum(pos.matches) - sum(neg.matches)  
    
    return(score)  
    
  }, pos.words, neg.words, .progress=.progress )  
  scores.df = data.frame(score=scores, text=sentences)  
  return(scores.df)  
} 


#Load sentiment word lists
hu.liu.pos = scan('C:/R/01Sentiment_analysis/positive-words.txt', what='character', comment.char=';')
hu.liu.neg = scan('C:/R/01Sentiment_analysis/negative-words.txt', what='character', comment.char=';')

#Add words to list
pos.words = c(hu.liu.pos, 'upgrade', 'awsum')
neg.words = c(hu.liu.neg, 'wtf', 'wait','waiting', 'epicfail', 'mechanical',"suspension","no")

#convert text to factor
Demonetisation.df$text<-as.factor(Demonetisation.df$text)

#calculate all the scores
Demonetisation.score = score.sentiment(Demonetisation.df$text, pos.words,neg.words, .progress='text')

# Final output
hist(Demonetisation.score$score)
table(Demonetisation.score$score)


