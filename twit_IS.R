library(twitteR)
# retrieve the first 200 tweets (or all tweets if fewer than 200) from the
# user timeline of @rdatammining
rdmTweets <- userTimeline("rdatamining", n=200)
(nDocs <- length(rdmTweets))
rdmTweets[11:15]

for (i in 11:15) {
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(rdmTweets[[i]]$getText(), width=73)) +}

# convert tweets to a data frame
df <- do.call("rbind", lapply(rdmTweets, as.data.frame))
dim(df)

library(tm)
# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(df$text))
# convert to lower case
myCorpus <- tm_map(myCorpus, tolower)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, removeURL)
# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords('english'), "available", "via")
# remove "r" and "big" from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)
# inspect documents (tweets) numbered 11 to 15
# inspect(myCorpus[11:15])
# The code below is used for to make text fit for paper width
for (i in 11:15) {
    cat(paste("[[", i, "]] ", sep=""))
    writeLines(strwrap(myCorpus[[i]], width=73)) 
}

inspect(myCorpus[11:15])

# stem completion
myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=myCorpusCopy)

#####################################################
install.packages('streamR')
install.packages('ROAuth')
library(ROAuth)
library(streamR)

#create your OAuth credential
credential <- OAuthFactory$new(consumerKey='########',
                               consumerSecret='##########',
                               requestURL='https://api.twitter.com/oauth/request_token',
                               accessURL='https://api.twitter.com/oauth/access_token',
                               authURL='https://api.twitter.com/oauth/authorize')

#authentication process
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
credential$handshake(cainfo="cacert.pem")

#function to actually scrape Twitter
#The track parameter tells Twitter want you want to ‘search’ for. It’s technically 
#not really a search since you are filtering the Twitter stream and not searching…
#technically. Twitter’s dev site has a nice explanation of all the Streaming APIs 
#parameters. For example, the track parameter is not case sensitive, it will treat 
#hashtags and regular words the same, and it will find tweets with any of the words 
#you specify, not just when all the words are present. The track parameter ‘apple, 
#twitter’ will find tweets with ‘apple’, tweets with ‘twitter’, and tweets with both.
#The filterStream() function will stay open as long as you tell it to in the timeout 
#parameter [in seconds], so don’t set it too long if you want your data quickly. The 
#data Twitter returns to you is a .json file, which is a JavaScript data file.
filterStream( file.name="tweets_test.json",
              track="isis", tweets=1000, oauth=credential, timeout=60, lang='en' )

#There are a few different ways to parse the data into something useful. The most 
#basic [and easiest] is to use the parseTweets() function that is also in streamR.
#Parses the tweets
tweet_df <- parseTweets(tweets='tweets_test.json')

#This means that R can only read basic A-Z characters and can’t translate emoji, 
#foreign languages, and some punctuation. I’d recommend using something like MongoDB 
#to store tweets or create your own parser if you want be able to use these features 
#of the text.
tweet_df$created_at
tweet_df$text

plot(tweet_df$friends_count, tweet_df$followers_count) #plots scatterplot
cor(tweet_df$friends_count, tweet_df$followers_count) #returns the correlation coefficient

###mapping tweets
library(ggplot2)
library(grid)
map.data <- map_data("world")
points <- data.frame(x = as.numeric(tweet_df$lon), y = as.numeric(tweet_df$lat))
points <- points[points$y > 25, ]
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", 
                            color = "grey20", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                 aes(x = x, y = y), size = 10, alpha = 1/2, color = "darkblue")
