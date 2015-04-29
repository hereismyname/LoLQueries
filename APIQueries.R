library(httr) 
library(dplyr)
library(jsonlite)

keyfile <- readLines("keyfile.txt", n = 1)

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

buildplayerstats <- function(playerhist) {
    playerhist <- fromJSON(playerhist)
    
    matchIDs <- playerhist[[1]][1]
    
    playermatchstats <- playerhist$matches[[12]]
    
    
    playerstats <- data.frame()
    
    for (i in 1:length(playermatchstats)) {
        if (length(playermatchstats[[i]]) == 10) {
            playerstats[i, 1] <- myplayer                     # player ID
            playerstats[i, 2] <- playermatchstats[[i]][[5]]   # highest season Tier
            playerstats[i, 3] <- matchIDs[i, 1]               # Match ID
            playerstats[i, 4] <- playermatchstats[[i]][[4]]   # Champ ID
            playerstats[i, 5] <- playerhist$matches$matchDuration[[1]]
            playerstats[i, 6] <- playerhist$matches$matchVersion[[1]]
            playerstats[i, 7] <- playerhist$matches$queueType[[1]]
            playerstats[i, 8] <- playerhist$matches$season[[1]]
            
            stats <- playermatchstats[[i]][[8]]
            playerstats[i, 5] <- stats$winner[[1]]              # Win or loss
            playerstats[i, 6] <- stats$champLevel[[1]]              # champion level
            playerstats[i, 7] <- stats$kills[[1]]             # kills
            playerstats[i, 8] <- stats$deaths[[1]]             # deaths
            playerstats[i, 9] <- stats$assists[[1]]             # assists
            playerstats[i, 10] <- stats$minionsKilled[[1]]            # minions killed-- TOTAL
            playerstats[i, 11] <- stats$neutralMinionsKilled[[1]]            # neutral minions killed-- TOTAL
            playerstats[i, 12] <- stats$neutralMinionsKilledTeamJungle[1]
            playerstats[i, 13] <- stats$neutralMinionsKilledEnemyJungle[1]
            playerstats[i, 14] <- stats$goldEarned[1]
            playerstats[i, 15] <- stats$goldSpent[[1]]
            playerstats[i, 16] <- stats$totalDamageDealt[[1]]
            playerstats[i, 17] <- stats$totalDamageDealtToChampions[[1]]
            playerstats[i, 18] <- stats$totalDamageTaken[[1]]
            playerstats[i, 19] <- stats$totalHeal[[1]]
            playerstats[i, 20] <- stats$wardsPlaced[[1]]            # wards placed
            playerstats[i, 21] <- stats$wardsKilled[[1]]
            playerstats[i, 22] <- stats$killingSprees[[1]]
            
        } else if (length(playermatchstats[[i]]) != 10) {
            playerstats[i, 1] <- myplayer                     # player ID
            playerstats[i, 2] <- playermatchstats[[i]][[5]]   # highest season Tier
            playerstats[i, 3] <- matchIDs[i, 1]               # Match ID
            playerstats[i, 4] <- playermatchstats[[i]][[4]]   # Champ ID
            
            stats <- playermatchstats[[i]][[7]]
            playerstats[i, 5] <- stats$winner[[1]]              # Win or loss
            playerstats[i, 6] <- stats$champLevel[[1]]              # champion level
            playerstats[i, 7] <- stats$kills[[1]]             # kills
            playerstats[i, 8] <- stats$deaths[[1]]             # deaths
            playerstats[i, 9] <- stats$assists[[1]]             # assists
            playerstats[i, 10] <- stats$minionsKilled[[1]]            # minions killed-- TOTAL
            playerstats[i, 11] <- stats$neutralMinionsKilled[[1]]            # neutral minions killed-- TOTAL
            playerstats[i, 12] <- stats$neutralMinionsKilledTeamJungle[1]
            playerstats[i, 13] <- stats$neutralMinionsKilledEnemyJungle[1]
            playerstats[i, 14] <- stats$goldEarned[1]
            playerstats[i, 15] <- stats$goldSpent[[1]]
            playerstats[i, 16] <- stats$totalDamageDealt[[1]]
            playerstats[i, 17] <- stats$totalDamageDealtToChampions[[1]]
            playerstats[i, 18] <- stats$totalDamageTaken[[1]]
            playerstats[i, 19] <- stats$totalHeal[[1]]
            playerstats[i, 20] <- stats$wardsPlaced[[1]]            # wards placed
            playerstats[i, 21] <- stats$wardsKilled[[1]]
            playerstats[i, 22] <- stats$killingSprees[[1]]
            
        }
        
    }
    names(playerstats) <- c("player.id", "season", "matchID", "champID", "winorloss",
                            "champlvl", "kills", "deaths", "assists", "minions.killed.total",
                            "neutral.minions.killed", "neutmin.killed.team", "neutmin.killed.enemy",
                            "goldearned", "goldspent", "totalDamageDealt", "dmgDealttoChamp", 
                            "totaldmgtaken", "totalheal", "wardsplaced", "wardskilled", "killingsprees")
    
    role.lane <- data.frame("role" = 1, "lane" = 1)
    
    for (i in 1:length(playermatchstats)) {
        timeline <- playermatchstats[[i]][[6]]
        role.lane[i, 1] <- timeline$role           # Role
        role.lane[i, 2] <- timeline$lane           # Lane
        
    }
    
    items <- data.frame()
    
    
    #### FIX MEEEEEEEEEEEEEE #### Maybe done?
    for (i in 1:length(playermatchstats)) {
        stats <- playermatchstats[[i]]$stats
        items[i, 1] <- stats[[3]][[1]]
        items[i, 2] <- stats[[4]][[1]]
        items[i, 3] <- stats[[5]][[1]]
        items[i, 4] <- stats[[6]][[1]]
        items[i, 5] <- stats[[7]][[1]]
        items[i, 6] <- stats[[8]][[1]]
        items[i, 7] <- stats[[9]][[1]]
    }
    names(items) <- paste("itemslot", 1:ncol(items), sep = ".")
    
    runedata <- data.frame()
    
    for (i in 1:length(playermatchstats)) {
        runes <- data.frame(playermatchstats[[i]]$runes)
        runesused <- paste(unlist(runes[1]), unlist(runes[2]), sep = "")
        runesused <- as.numeric(runesused)
        ## would choke here if different assignments of runes were used...
        runedata <- rbind(runedata, runesused)
    }
    names(runedata) <- paste("rune", 1:ncol(runedata), sep = ".")
    
    masterydata <- data.frame()
    
    for (i in 1:length(playermatchstats)) {
        masteries <- data.frame(playermatchstats[[i]]$masteries)
        masteriesused <- paste(unlist(masteries[1]), unlist(masteries[2]), sep = "")
        masteriesused <- as.numeric(masteriesused)
        masterydata <- rbind(masterydata, masteriesused)
    }
    names(masterydata) <- paste("mastery", 1:ncol(masterydata), sep = ".")
    
    cbind(playerstats, role.lane, items, runedata, masterydata)
}