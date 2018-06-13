# 20 requests every 1 seconds
# 100 requests every 2 minutes
# Note that rate limits are enforced per region. For example, with the above rate limit, you could make 20 requests every 1 seconds to both NA and EUW endpoints simultaneously.
library(jsonlite)
library(tidyverse)
library(lubridate)
library(httr)
library(stringr)
#Get functions, eventually should be a package?
source("functions.R")
# api_key stored in .Rprofile.site
Region <- "euw1"
Summoner_Name <- "Proudfeet"

Summoner_Info_Link <- paste0("https://",Region,".api.riotgames.com/lol/summoner/v3/summoners/by-name/",Summoner_Name,"?api_key=",api_key)
Summoner_Info_data <- fromJSON(Summoner_Info_Link, simplifyDataFrame = TRUE)

Summoner_ID <- Summoner_Info_data$accountId

Match_His_Link <- paste("https://",Region,".api.riotgames.com/lol/match/v3/matchlists/by-account/",Summoner_ID,"?api_key=",api_key,sep = "")
Match_history <- fromJSON(Match_His_Link)

# Bulk extract match meta data --------------------------------------------

nGames <- 300
matchHistDF<- tibble(nGamesEnd = as.integer(seq(100,nGames,by=100)),
                     nGamesStart = as.integer(lag(nGamesEnd, default = 0L))) %>% 
                mutate(link=map2(nGamesStart, nGamesEnd,~extractMatches(account = account,start = .x,end = .y)),
                       rawJson=map(link,fromJSON),
                       df = map(rawJson,"matches")) %>% 
                  unnest(df) %>% 
                    mutate(date = as_datetime(timestamp/1000, origin = as_datetime("1970-01-01")))

# Get match stats ---------------------------------------------------------

matchHistDFExt<- matchHistDF %>% 
  select(-nGamesStart,-nGamesEnd) %>% 
  mutate(matchStats = map(gameId,~getMatchStats(matchId=as.numeric(.x),region=Region,apiKey=api_key)))


