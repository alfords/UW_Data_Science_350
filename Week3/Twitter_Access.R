library(twitteR)
library(devtools)
library(httr)

# Read credential file (two rows: Headers and value, comma separated, no spaces)
twit_cred <- read.csv("twitter_cred.csv", stringsAsFactors = FALSE)

# Store twitter consumer/access key, token, and secrets in variables
TWITTER_CONSUMER_KEY = twit_cred$TWITTER_CONSUMER_KEY
TWITTER_CONSUMER_SECRET = twit_cred$TWITTER_CONSUMER_SECRET
TWITTER_ACCESS_TOKEN = twit_cred$TWITTER_ACCESS_TOKEN
TWITTER_ACCESS_SECRET = twit_cred$TWITTER_ACCESS_SECRET

# Authenticate with twiter, if prompted, select 1 (yes) to run in cache
setup_twitter_oauth(TWITTER_CONSUMER_KEY, TWITTER_CONSUMER_SECRET, TWITTER_ACCESS_TOKEN, TWITTER_ACCESS_SECRET)

# Obtain twitter data
ds <- searchTwitter('#datascience', locale=NULL, geocode=NULL,n=50)