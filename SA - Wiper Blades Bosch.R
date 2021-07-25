library(xml2) #pull html data
library(rvest) #get html nodes
library(purrr) #for map functions
library(stringr) #for str functions
library(ggplot2)
library(readxl)
library(dplyr)
library(stringr)
library(tibble)
library(tidytext)
library(textdata)

#Webscrapping
url <-"https://www.amazon.in/Bosch-3397010057-Performance-Replacement-Wiper/product-reviews/B00P21LI9O/ref=cm_cr_arp_d_viewopt_sr?ie=UTF8&reviewerType=all_reviews&pageNumber=%d"

map_df(1:69,function(i){
  
  page <- read_html(sprintf(url,i))
  data.frame(Reviews = html_text(html_nodes(page,".review-text-content")),
             stringsAsFactors = FALSE)
}) -> review

#Saving it as new .csv file
write.csv(review, "Amazon-Reviews.csv")

#Preprocessing
review <- read.csv("Amazon-Reviews.csv")
review_df <- as_tibble(review)

review_df$Reviews <- review_df$Reviews %>%
  str_split("\\n") %>%
  map(14) %>%
  str_trim()

#Tokenization
review_df %>%
  unnest_tokens(output = "word",
                token = "words",
                input = Reviews) %>%
  count(word, sort=TRUE) -> data_by_word

#Removing Stop Words
review.data <- data_by_word
review.data <- review.data %>%
  anti_join(stop_words)

#Visualization for Frequently occuring words, here more that 50 times
filtered_data <- filter(review.data, n >50)

filtered_data %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_col()+
  coord_flip()

#Dataset for positive and negative words
sentiments <- review.data %>%
  inner_join(get_sentiments("nrc")) %>%
  inner_join(get_sentiments("bing")) %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word,sentiment,sort = TRUE)

posText <- subset(sentiments, sentiment == "positive")
negText <- subset(sentiments, sentiment == "negative")
review.wiper <- review.data

pos.word=c(posText,"upgrade")
neg.word=c(negText,"wtf","wait","waiting","epicfail","mechanical","misleading")

#posText<-unlist(lapply(posText, function(x){str_split(x,"\n")}))
#negText<-unlist(lapply(negText, function(x){ str_split(x,"\n")}))

#Function to calculate sentiment scores
score.sentiment=function(sentences, pos.words,neg.words){
  
  sent.scores=sapply(sentences, function(sentence,pos.words,neg.words){
    # removing punctuations
    sentences=gsub("[[:punct:]]","",sentence)
    # removing control charaters
    sentences=gsub("[[:cntrl:]]","",sentence)
    # removing digits
    sentences=gsub("//d+ ","",sentence)
    # error handling function when trying to convert lower case
    tryTolower=function(x){
      y=NA
      try_error=tryCatch(tolower(x),error=function(e) e)
      if(!inherits(try_error,"error")){
        y=tolower(x)
      }
      return(y)
    }
    sentence=sapply(sentence,tryTolower)
    # split sentence into words with str_split (stringr package)
    word.list = str_split(sentence, "\\s+")
    words = unlist(word.list)
    # compare words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # get the position of the matched term or NA
    # we just want a TRUE/FALSE
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # final score
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words )
  # data frame with sent.scores for each sentence
  sent.scores.datafrm = data.frame(text=sentences, score=sent.scores)
  return(sent.scores.datafrm)
}
  
#Start processing the reviews to calculate the sentiment score.  
  sent.scores = score.sentiment(review.wiper, pos.word,neg.word)
  
  #1. Calculate positive, neutral and negative sentiments.
  sent.scores$positive <- as.numeric(sent.scores$score >0)
  sent.scores$negative <- as.numeric(sent.scores$score <0)
  sent.scores$neutral <- as.numeric(sent.scores$score ==0)
  
  #2. Create polarity variable for each data frame.
  sent.scores$polarity <- ifelse(sent.scores$score >=0,"positive",ifelse(sent.scores$score <= -1,"negative",ifelse(sent.scores$score==0,"Neutral",0)))

qplot(factor(polarity), data=sent.scores, geom="bar",
fill=factor(polarity))+xlab("Polarity Categories") +ylab("Frequency") +
ggtitle("Customer Sentiments - Wiper Blades - Bosch")

qplot(factor(score), data=sent.scores, geom="bar", fill=factor(score))+xlab("Sentiment
Score") + ylab("Frequency") + ggtitle("Customer Sentiment Scores - Wiper Blades Bosch")

#Postive To Negative Ratio
pos_score <- aggregate(n ~ sentiment, data=posText,sum)
neg_score <- aggregate(n ~ sentiment, data=negText,sum)

ratioPosNeg <- (pos_score$n/neg_score$n)
ratioPosNeg

