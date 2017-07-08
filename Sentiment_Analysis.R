install.packages('data.table')
install.packages("tm")
install.packages('wordcloud')
install.packages('SnowballC')
library('data.table')
library('tm')
library(SnowballC)
library("syuzhet")

data1<-fread('/Sentiment_Analysis_Dataset/Sentiments.csv')

#Loading the text data
textdata <-  data1$SentimentText

#Cleaning the data with unusual characters
textdata = gsub("[[:punct:]]", "", textdata)


#Removing digits
textdata = gsub("[[:digit:]]", "", textdata)

#removing urls
textdata = gsub("http\\w+", "", textdata)

#removing extra spaces
textdata = gsub("[ \t]{2,}", "", textdata)

#removing special characters
textdata = gsub("^\\s+|\\s+$+", "", textdata)

change_lower = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

textdata = sapply(textdata, change_lower)
textdata = textdata[!is.na(textdata)]


a=get_sentiment(as.character(textdata),method="bing")

data1$sentiment_Score<-NULL
data1$sentiment_Score<-a


#To create a wordcloud
library('wordcloud')
x<-data.frame("Text"=data1$SentimentText,stringsAsFactors = FALSE)

corp <- Corpus(DataframeSource(x))
corp <- tm_map(lords, stripWhitespace)
corp <- tm_map(lords, content_transformer(tolower))
corp <- tm_map(lords, removeWords, stopwords('english'))
corp <- tm_map(lords, stemDocument)
wordcloud(lords, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))
