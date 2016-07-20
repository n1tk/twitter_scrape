library(twitteR) # install.packages("twitteR")

# Authenticate 
api_key <- "a"
api_secret <- "b"
access_token <- "c"
access_token_secret <- "d"

# This creates an auth file in your working directory, so you will need to run this at least once before your first ever load.

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret) 

# Get the page information
yourUser <- getUser("KSU")

# Get that user's Twitter followers.
yourUser.Followers <- lookupUsers(yourUser$getFollowerIDs())
yourUser.Followers <- twListToDF(yourUser.Followers)