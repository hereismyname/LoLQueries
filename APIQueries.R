getplayerid <- function(username, key) {
    
    summonerquery <- "https://na.api.pvp.net/api/lol/na/v1.4/summoner/by-name/"
    apikey <- paste("?api_key=", key, sep = "")
    url <- paste(summonerquery, username, apikey, sep = "")
    content(GET(url))[[1]][[1]]
    
}

getmatchhist <- function(id, key, queuetype = "none", mingame, maxgame) {
    
    matchhistquery <- "https://na.api.pvp.net/api/lol/na/v2.2/matchhistory/"
    apikey <- paste("api_key=", key, sep = "")
    
    # indexes <- paste("&beginIndex=", mingame, "&endIndex=", maxgame, sep = "")
    rankorno <- ""
    
    if (queuetype == "RANKED_SOLO_5x5") {
        rankorno <- "?rankedQueues=RANKED_SOLO_5x5&"
    } else if (queuetype == "RANKED_TEAM_3x3") {
        rankorno <- "?rankedQueues=RANKED_TEAM_3x3&"
    } else if (queuetype == "RANKED_TEAM_5x5") {
        rankorno <- "?rankedQueues=RANKED_TEAM_5x5&"
    } else if (queuetype == "none") {
        rankorno <- "?"
    }
    
    
    finalquery <- paste(matchhistquery, id, rankorno, apikey, sep = "")
    
    yepyep <- GET(finalquery)
    toJSON(content(yepyep))
}