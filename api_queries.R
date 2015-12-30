# Extract player ID number via a player-username
# 
# Args:
#   username: username to query
#   key: *your* personal API key
#
# Returns:
#   the player ID as a numeric vector
getplayerid <- function(username, key) {
    summonerquery <- "https://na.api.pvp.net/api/lol/na/v1.4/summoner/by-name/"
    apikey <- paste("?api_key=", key, sep = "")
    
    url <- paste(summonerquery, username, apikey, sep = "")
    content(GET(url))[[1]][[1]]
}

## uncertain whether you can have control over extracting game history from
## certain periods of time
## for example, if you set mingame/maxgame to 1, does this just extract the player's
## most recent game? needs to be tested.

# Extract match history from a given player ID
#
# Args:
#   id: player ID to be queried
#   key: *your* personal API key
#   queuetype: which type of matchmaking
#   mingame: minimum number of games to be extracted
#   maxgame: maximum number of games to be extracted
#
# Returns:
#   player match history in JSON format
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
