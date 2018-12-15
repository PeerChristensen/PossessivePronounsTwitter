
library(rtweet)
library(tidyverse)

token <- create_token(
    app = "textR",
    consumer_key = "hrztENrNCQ5FpXKK1RdeN8zFf",
    consumer_secret = "GE9WUVCt4mDpmT6u2SNif5rxbzG3DPK0gBZpu7tz1TopOlQYnE")

######### HIS #############

his <- search_tweets2(
    "his", n = 100000, include_rts = FALSE,
    retryonratelimit = TRUE
  )
  
his[sapply(his, is.list)] <- lapply(his[sapply(his, is.list)], 
                                    as.character)

write.csv(his,"hisTweets.csv")

######### HER #############

her <- search_tweets2(
    "her", n = 100000, include_rts = FALSE,
    retryonratelimit = TRUE
  )

her[sapply(her, is.list)] <- lapply(her[sapply(her, is.list)], 
                                    as.character)

write_csv(her,"herTweets.csv")
 