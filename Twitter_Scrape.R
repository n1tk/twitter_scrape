####################################################################################
##################              Install and Load Packages         ##################
####################################################################################
#-----------------------------------------------#
#---   Select TN mirror (Near the bottom)   ----#
#-----------------------------------------------#
install.packages('twitteR')
install.packages('streamR')
install.packages('reshape2')
install.packages('ggplot2')
install.packages('wordcloud')
install.packages('RColorBrewer')
install.packages('plyr')
install.packages('ggmap')
install.packages('sqldf')
library(sqldf)
library(twitteR)
library(streamR)
library(ROAuth)
library(reshape2)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(plyr)

install.packages('devtools')
require(devtools)
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
require(sentiment)
require(Rstem)

####################################################################################
##################                 Start Editing Here             ##################
####################################################################################
#------------------------------------------------------#
#---   Copy and Paste Keys From Twitter to Here   -----#
#--- Notice that the key values are inside quotes! ----#
#------------------------------------------------------#
consumerKey <- "*****"
consumerSecret <- "*****"
accessToken <- '*****'
secretToken <- '*****'
#------------------------------#
#---   Leave these as is   ----#
#------------------------------#
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
twitCred <- OAuthFactory$new(consumerKey=consumerKey,
  		consumerSecret=consumerSecret, requestURL=reqURL,
  		accessURL=accessURL, authURL=authURL)

#----------------------------------------------------------------#
#---   Copy and paste the URL from here into your browser   -----#
#---        then paste pin from browser into R console      -----#
#----------------------------------------------------------------#
twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

####################################################################################
##################             Saving/Loading Credentials         ##################
####################################################################################
#-----------------------------------------------------------#
#---   Replace path with a location on your computer   -----#
#-----------------------------------------------------------#
setwd("/Users/users/Twitter_Scrape")

#-- Verfiy file is in your location --#
#-------------------------------------#
save(twitCred, file="/Users/users/Twitter_Scrape/Caregiver_Strain_1.xlsx")

#-- Run this line as well --#
#---------------------------#
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#-----------------------------------------------------------#
#---       If reloading during another session         -----#
#---     This prevents you from handshaking again      -----#
#-----------------------------------------------------------#
# After you exist R and reopen, remeber to run all libary and
# require statments from the first section then set your work 
# directory and run these two uncommented lines
#-----------------------------------------------------------#

####################################################################################
##################               Getting Twitter Feeds            ##################
####################################################################################
#-----------------------------------#
#----    Register Credentials   ----#
#-----------------------------------#
registerTwitterOAuth(twitCred)
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,secretToken)


#---------------------------------------------------------------#
#------                 Search Twitter                    ------#
#---------------------------------------------------------------#
#----        Replace Christmas with desired keyword       ------#
#- timeout = 300: 5min(300sec) worth of tweets with #Christmas -#
#- lang = 'en': English							----#
#- locations= longitude, lattitude pairs				----#
#---------------------------------------------------------------#
#-- Working with R code to find User ID's---------------------#
q = '82282572, 123853341, 2982168555, 73303753, 2546833752, 231744293, 87951215, 49615362, 9624742, 211955846, 172814396, 33669915, 44499925, 258972713, 252952750, 251553556, 15007149, 19394188, 188591875, 270132205, 44498753, 49276934, 297037755, 14845376, 148733376, 557177242, 138787319, 12858902, 146471779, 18695134, 97190495, 293121508, 1615463502, 17658786, 475210759, 97261425, 143568410, 495328039, 19674502, 317650146, 28305267, 87914827, 398289794, 178978113, 254120931, 70955722, 555005172, 23946671, 62081232, 855807462, 19006707, 96354656, 238197721, 2767724844, 1286202108, 271024433, 1482535734, 561447826'

#--- 5min of Twitter data with #Christmas from the United States (general area) ---#
filterStream(file.name="test1.xls", track = NULL, follow = NULL,
timeout=20, oauth=twitCred,
lang = 'en',locations=c(-121.087140,25.034454 ,-61.497299,50.084363))

#-- Create data.frame --#
tweetsDF <- parseTweets("PoliticalTweets.xls", verbose = TRUE)

###################################################################################
##################        Cleaning Up Feed / Transformations     ##################
###################################################################################
# No editing required, run entire section
#----------------------------------------#
tweetsDF$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "",tweetsDF$text)
tweetsDF<-cbind(tweetsDF,colsplit(tweetsDF$created," ",c("date","time")))
tweetsDF<-cbind(tweetsDF,colsplit(tweetsDF$date,"-",c("year","month","day")))
tweetsDF<-cbind(tweetsDF,colsplit(tweetsDF$time,":",c("hour","min","sec")))
tweetsDF$text = gsub("@\\w+", "",tweetsDF$text)
tweetsDF$text = gsub("[[:punct:]]", "", tweetsDF$text)
tweetsDF$text = gsub("[[:digit:]]", "", tweetsDF$text)
tweetsDF$text = gsub("http\\w+", "", tweetsDF$text)
tweetsDF$text = gsub("[ \t]{2,}", "", tweetsDF$text)
tweetsDF$text = gsub("^\\s+|\\s+$", "", tweetsDF$text)
tweetsDF$text <- gsub("\n","",tweetsDF$text)
start <- lapply(strsplit(tweetsDF$source, ''), function(x) which(x == '>'))
temp <-data.frame(start)
temp <-temp[1,]
temp <- melt(temp)
tweetsDF$start <- temp[2]
end <- lapply(strsplit(tweetsDF$source, ''), function(x) which(x == '<'))
temp <-data.frame(end)
temp <-temp[2,]
temp <- melt(temp)
tweetsDF$end <- temp[2]
status <- vector()
for (i in 1:NROW(tweetsDF)){
	status[i] <- substr(tweetsDF$source[i], tweetsDF$start[i,], tweetsDF$end[i,])
	status1 <-data.frame(cbind(status,status[i]))
	status1<-status1
}
status1$status <- as.character(status1$status)
status <- data.frame(substr(status1$status, 2, nchar(status1$status)-1))
names(status) <- "status"
num <- NCOL(tweetsDF)-2
tweetsDF <- tweetsDF[,1:num]
tweetsDF$source <- status
names(tweetsDF$source)<- NULL
names(tweetsDF$source)<- "source"
catch.error <- function(x)
{
y = NA
catch_error = tryCatch(tolower(x), error=function(e) e)
if (!inherits(catch_error, "error"))
y = tolower(x)
return(y)
}
tweetsDF$text <- sapply(tweetsDF$text, catch.error)
tweetsDF$text <- tweetsDF$text[!is.na(tweetsDF$text)]
names(tweetsDF$text) = NULL
tweetsDF_geo<-tweetsDF[!is.na(tweetsDF$lat) & !is.na(tweetsDF$lon),]

class_emo <- classify_emotion(tweetsDF$text, algorithm="bayes", prior=1.0)
tweetsDF$emotion = class_emo[,7]
tweetsDF$emotion[is.na(tweetsDF$emotion)] = "unknown"
class_pol = classify_polarity(tweetsDF$text, algorithm="bayes")
tweetsDF$polarity = class_pol[,4]

sentiment_dataframe = data.frame(text=tweetsDF$text, emotion=tweetsDF$emotion, polarity=tweetsDF$polarity, stringsAsFactors=FALSE)


###################################################################################
##################                Sentiment Analysis             ##################
###################################################################################

#----------------------------------#
#----   Emotion with Unknown   ----#
#----------------------------------#
ggplot(sentiment_dataframe, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
	scale_fill_brewer(palette="Dark2") +
	#-----------------------------------------#
	#----   Replace With Your Title Below ----#
	#-----------------------------------------#
	ggtitle('Emotion of Tweets on Twitter about Christmas') +
	theme(legend.position='right') + ylab('Number of Tweets') + xlab('Emotion Categories')

#------------------------------------#
#----   Emotion Without Unknown  ----#
#------------------------------------#
sentiment_dataframe1 <- sentiment_dataframe[sentiment_dataframe[,2]!="unknown",]

ggplot(sentiment_dataframe1, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
	scale_fill_brewer(palette="Dark2") +
	#-----------------------------------------#
	#----   Replace With Your Title Below ----#
	#-----------------------------------------#
	ggtitle('Emotion of Tweets on Twitter about Christmas') +
	theme(legend.position='right') + ylab('Number of Tweets') + xlab('Emotion Categories')

#----------------------#
#----   Polarity   ----#
#----------------------#
ggplot(sentiment_dataframe, aes(x=polarity)) +
	geom_bar(aes(y=..count.., fill=polarity)) +
	scale_fill_brewer(palette="RdGy") +
	#-----------------------------------------#
	#----   Replace With Your Title Below ----#
	#-----------------------------------------#
	ggtitle('Polarity of Tweets on Twitter about Christmas') +
	theme(legend.position='right') + ylab('Number of Tweets') + xlab('Polarity Categories')

#-------------------------#
#----   Geolocation   ----#
#-------------------------#
detach("package:ggplot2", unload=TRUE)
library(ggplot2)
library(ggmap)

map <- get_map(location = "united states", zoom = 4,
	source= "google",maptype = "roadmap", scale = 4)

ggmap(map) +
 	geom_point(data = tweetsDF_geo, aes(x = lon, y = lat, alpha = 0.8), size = 2, shape = 21, fill= "red") +
 	guides(fill=FALSE, alpha=FALSE, size=FALSE)


#---------------------#
#----   Devices   ----#
#---------------------#
mysources <- tweetsDF$source
unlist(mysources)
temp <- sqldf("SELECT source FROM sources WHERE source like '%Twitter%'")
factor(mysources)

mar.default <- c(9,3,2,2) + 0.1
par(mar = mar.default + c(0, 0, 0, 0)) 
col<- brewer.pal(12, "Paired")
barplot(table(temp), las=3, 
	#-----------------------------------------#
	#----   Replace With Your Title Below ----#
	#-----------------------------------------#
	main = "Barplot of Devices", ylab = "Number of Tweets",
	col=col, cex.names= .8)

