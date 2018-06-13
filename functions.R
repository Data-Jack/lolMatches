
# function for getting link for match meta data ---------------------------

extractMatches <- function(account,start=NULL,end=NULL,region = Region,apiKey=api_key){
  paste0("https://",region,".api.riotgames.com/lol/match/v3/matchlists/by-account/",Summoner_ID,"?beginIndex=",start,"&endIndex=",end,"&api_key=",apiKey)
}
# Function for getting match stats ----------------------------------------

getMatchStats <- function(matchId,region=Region,apiKey=api_key){
  #Make link and get response
  matchLink<- paste0("https://",region,".api.riotgames.com/lol/match/v3/matches/",matchId,"?api_key=",apiKey)
  
  apiResponse<- httr::GET(matchLink)
  
  #Process API headers and handle rate limits
  heads <- headers(apiResponse)
  processLimits(heads)
  #extracting match data
  cont<- fromJSON(content(test2, type = "text"))
  # print("Sleeping for 2:05 mins zzz...")
  return(cont)
}

#Function for processing apiResponse header----
processLimits <- function(heads){
  rateLimit <- heads[str_detect(names(heads),"rate-limit")]
  rateLimit2 <- as_tibble(rateLimit) %>% 
    gather(key = HeaderName) %>% 
    mutate(end = str_locate(HeaderName,"rate.limit")[,"start"]-1,
           Name = str_sub(HeaderName,1,end),
           measure = case_when(str_detect(HeaderName,"count")~"count",TRUE~"limit")) %>% 
    # group_by(HeaderName) %>% nest() %>% 
    mutate(rates = map(value, function(x){unlist(str_split(x,","))})) %>% unnest(rates) %>% 
    separate(rates, c("count","seconds"), ":") %>% 
    spread(measure, count) %>% 
    arrange(Name,seconds) %>% 
    select(-HeaderName,-value,-end) %>% 
    group_by(Name,seconds) %>% 
    nest() %>% 
    mutate(data2 =map(data,function(x){ tibble(count = pull(filter(x,!is.na(count)),count),
                                               limit = pull(filter(x,!is.na(limit)),limit))})) %>% 
    unnest(data2,.drop = TRUE) %>% 
    mutate_at(c("seconds","count","limit"),as.integer) %>% 
    filter(count>=limit)
  #if limit reached sleep for maximum time required
  if(nrow(rateLimit2)==0){
    return(invisible(NULL))
  }else{
    sleepLen <- max(pull(rateLimit2,seconds))
    warning(paste("Sleeping for",sleepLen,"seconds zzz..."))
    Sys.sleep(sleepLen)
    return(invisible(NULL))
  }
}