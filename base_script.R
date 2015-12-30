library(httr) 
library(dplyr)
library(jsonlite)

source("APIQueries.R")

## Sys.sleep(seconds)
## This can throttle the loop in case of overquerying

# Pull down individual player stats for a given user (or set of users)
#
# Args:
#   list: list of summoner names
#   outputpath: where the player stats should be written out
#
# Returns: Nothing, files are written out. Files will be saved in JSON format.
script <- function(list, outputpath) {
    for(user in mylist) {
        
        playerid <- getplayerid(user, keyfile)
        
        playerhist <- getmatchhist(playerid, keyfile)
        
        filename <- paste(outputpath, user, Sys.Date(), ".json", sep = "")
        
        write(playerhist, filename)
    }
    NULL
}

# this object should be a single character vector containing *your* personal API key
keyfile <- readLines("keyfile.txt", n = 1)

mylist <- c("highronic", "suntso")
path <- "./json-files/"

if(file.exists("json-files") == FALSE) { dir.create("json-files") }

script(mylist, path)
