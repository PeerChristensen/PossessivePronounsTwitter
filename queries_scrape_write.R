
library(rtweet)
library(tidyverse)
library(beepr)

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

######### MEN and WOMEN #############

mw <- search_tweets2(
  "men OR women", n = 200000, include_rts = FALSE,
  retryonratelimit = TRUE, lang = "en"
)

mw[sapply(mw, is.list)] <- lapply(mw[sapply(mw, is.list)], 
                                    as.character)

write.csv(mw,"mwTweets.csv")
beep("mario")

######### HE IS #############

he <- search_tweets2(
  "\"he's\" OR \"he is\"", n = 100000, include_rts = FALSE,
  retryonratelimit = TRUE, lang = "en"
)

he[sapply(he, is.list)] <- lapply(he[sapply(he, is.list)], 
                                  as.character)

write.csv(he,"heTweets.csv")
beep("mario")

######### SHE IS #############

she <- search_tweets2(
  "\"she's\" OR \"she is\"", n = 100000, include_rts = FALSE,
  retryonratelimit = TRUE, lang = "en"
)

she[sapply(she, is.list)] <- lapply(she[sapply(she, is.list)], 
                                  as.character)

write.csv(she,"sheTweets.csv")
beep("mario")

######### CHRISTMAS ######

xmas <- search_tweets2(
  "All I want for christmas is", n = 100000, include_rts = FALSE,
  retryonratelimit = TRUE, lang = "en"
)

xmas[sapply(xmas, is.list)] <- lapply(xmas[sapply(xmas, is.list)], 
                                  as.character)

write.csv(xmas,"xmasTweets.csv")

 