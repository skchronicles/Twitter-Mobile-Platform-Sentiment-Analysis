#Skyler Kuhn 
#Project6: Android/iPhone Twitter code
#For this project you will use twitter data to determine if two phone platforms have different "like" and "hate" ratings.  
#The first platform is "Android" and the other is "iPhone".  Complete the following tasks.

#################################################################################

# *** START Hightlighting text from here: ***

#Using the package we have downloaded
library(httr) 
library(twitteR)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(twitteR, sentiment, plyr, ggplot2, wordcloud, RColorBrewer, httpuv, RCurl, base64enc)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

#Must first create a twitter application to get this information
api_key = "GsYm2mzsuIZbsWxpwZL7k1Yc4"
api_secret = "ccenJqNP3U8KSWTGndD5TdB6eoYTfza8A5mwHn0aunrYxDqk3O"
access_token = "4219122813-7WryAjedOdx1QfKQZgSHhNzDQchhGbGuueweDG0"
access_token_secret = "QJHhv7C1nPewgDBh4uGXjqCMpaP3l1bUrkhg1TRZ7QadD"


setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)   #gaining access to enter twitter

###1.)Create two dictionaries with at least 10 words each. 
#One dictionary is for "positive" statements for the "like" category 
#and the other dictionary is for "negative" statements for the "hate" category.  
#Clearly comment your code on which dictionary is which.
# setup_twitter_oauth(api_key,api_secret)

some_tweets = searchTwitter("Android", n=500, lang="en")  #searching twitter for keyword
head(some_tweets)                                         #prints off a few android related tweets

some_tweets2 <- twListToDF(some_tweets) #puts information in a dataframe, can be used to grab stuff (like usernames, gps coordinates of user, etc.)    
head(some_tweets2)

someApple_tweets = searchTwitter("iphone", n=500, lang="en")  #searching twitter for keyword
head(someApple_tweets)                                       # prints off a few apple related tweets :)

someApple_tweets2 <- twListToDF(someApple_tweets) #puts information in a dataframe, can be used to grab stuff (like usernames)
head(someApple_tweets2)


#Search the text terms
apple_tweet_text <- someApple_tweets2$text #grabbing the text only from apple related tweets
tweet_text <- some_tweets2$text #grabbing the text only from android related tweets
#tweet_source <- some_tweets2$statusSource #grabbing information on which platform the tweets are coming from (android or iPhone)

#Variables used to store the counter
androidBad1 <- 0  #Container to COUNT the bad android responses
androidGood1 <- 0   #Container to COUNT the good android responses
iphoneBad1 <- 0  #Container to COUNT the bad apple responses
iphoneGood1 <- 0   #Container to COUNT the good apple responses

#Create  a dictionary 
GW1 <- c("love", "like", "happy", "great", "excellent", "awesome","positive", "best", "pleased", "perfect") #Goodword Dictionary
BW1 <- c("hate", "horrible", "stupid", "sucks", "negative", "shitty", "crappy", "worst", "dislike", "resent") #Badword Dictionary

#####2.) & 4.)Pull 10,000 tweets for iPhone & Android and determine the number of "like" and "hate" ratings (using looping)

for(i in 1:10000)
{
  if(!is.na(tweet_text[i]))    # avoids empty tweets (tweets with no text)
  {
    for(goodword in GW1)  #Iterating through our keywords and grabbing there username if their tweet contains one of our words
    {
      goodflag <- regexpr(goodword, tweet_text[i])[1] 
      if(goodflag  >  0) #if the flag is greater than 1 that means it found a badword in the tweet (-1 will be returned if not found)
      {
        androidGood1 <- androidGood1 + 1 
      }
    }
    for(badword in BW1)
    {
      badflag <- regexpr(badword, tweet_text[i])[1] 
      if(badflag  >  0) #if the flag is greater than 1 that means it found a badword in the tweet (-1 will be returned if not found)
      {
        androidBad1 <- androidBad1 +1
      }
    }
  }
  if(!is.na(apple_tweet_text[i]))    # avoids apple empty tweets (tweets with no text)
  {
    for(applegoodword in GW1)  #Iterating through our keywords and grabbing there username if their tweet contains one of our words
    {
      goodflag <- regexpr(applegoodword, apple_tweet_text[i])[1] 
      if(goodflag  >  0) #if the flag is greater than 1 that means it found a badword in the tweet (-1 will be returned if not found)
      {
        iphoneGood1 <- iphoneGood1 + 1 
      }
    }
    for(applebadword in BW1)
    {
      badflag <- regexpr(applebadword, apple_tweet_text[i])[1] #badflag is a local variable to the loop, can be used in other loops
      if(badflag  >  0) #if the flag is greater than 1 that means it found a badword in the tweet (-1 will be returned if not found)
      {
        iphoneBad1 <- iphoneBad1 + 1
      }
    }
  }
}

#Printing the results of the coutner
print(androidGood1) #android goodword counter
print(androidBad1) #android badword counter
print(iphoneGood1) #iphone goodword counter
print(iphoneBad1) #iphone badword counter


###3.)For the android tweets use the R function prop.test() to create
#    a confidence interval for the proportion who "like" and another confidence interval 
#prop.test(x=c(androidGood1,androidBad1), n=c(10000, 10000), alternative = "greater") (ignore/delete later)
prop.test(x=androidGood1, n=10000)
#    for the proportion who "hate" android.
prop.test(x=androidBad1, n=10000)


###5.)For the iPhone tweets use the R function prop.test() to create 
#   a confidence interval for the proportion who "like" and another confidence interval 
prop.test(x=iphoneGood1, n=10000)
#   for  the proportion who "hate" iphone.
prop.test(x=iphoneBad1, n=10000)


###6.)Use the prop.test() function in R to perform a two sample test on proportions
#     to see if the proportion who "like" is the same across the two platforms
prop.test(x=c(androidGood1,iphoneGood1), n=c(10000,10000))


###7.)Use the prop.test() function in R to perform a two sample test on proportions 
#     to see if the proportion who "hate" is the same across the two platforms

prop.test(x=c(androidBad1,iphoneBad1), n=c(10000,10000))


# *** END HIGHTLIGHTING HERE and RUN :D ***





