# 20 requests every 1 seconds
# 100 requests every 2 minutes
# Note that rate limits are enforced per region. For example, with the above rate limit, you could make 20 requests every 1 seconds to both NA and EUW endpoints simultaneously.
library(jsonlite)
library(tidyverse)

# api_key stored in .Rprofile.site
Region <- "euw1"
Summoner_Name <- "Proudfeet"

Summoner_Info_Link <- paste0("https://",Region,".api.riotgames.com/lol/summoner/v3/summoners/by-name/",Summoner_Name,"?api_key=",api_key)
Summoner_Info_data <- fromJSON(testLink, simplifyDataFrame = TRUE)

Summoner_ID <- Summoner_Info_data$accountId

Match_His_Link <- paste("https://",Region,".api.riotgames.com/lol/match/v3/matchlists/by-account/",Summoner_ID,"?api_key=",api_key,sep = "")
Match_history <- fromJSON(Match_His_Link)
nGames <- Match_history$totalGames

# nLoops <- 1:ceiling((nGames-100)/100)
# extractMatches <- function(x) {
#   start <- (x*100)+1
#   end <- min((start+100)-1,nGames)
#   linkText <- paste0("https://",Region,".api.riotgames.com/lol/match/v3/matchlists/by-account/",Summoner_ID,"?beginIndex=",start,"&endIndex=",end,"&api_key=",api_key)
#   mathes <- fromJSON(linkText, flatten = TRUE)
#   mathes$matches[[1]]
# }
# map(nLoops,extractMatches)

matchDF <- Match_history$matches %>% 
             mutate(date = lubridate::as_datetime(timestamp/1000, origin = lubridate::as_datetime("1970-01-01")))

matchRefs <- pull(matchDF,gameId)

getMatchStats <- function(x){
  matchLink<- paste0("https://",Region,".api.riotgames.com/lol/match/v3/matches/",x,"?api_key=",api_key)
  mathesIn <- fromJSON(matchLink)
  mathesIn
}

matchList <- map(matchRefs,getMatchStats)
