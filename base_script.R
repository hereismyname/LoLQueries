library(httr) 
library(dplyr)
library(jsonlite)

source("APIQueries.R")

## Sys.sleep(seconds)
## This can throttle the loop in case of overquerying

script <- function(list, outputpath) {
    for(user in mylist) {
        
        playerid <- getplayerid(user, keyfile)
        
        playerhist <- getmatchhist(playerid, keyfile)
        
        filename <- paste(outputpath, user, Sys.Date(), ".json", sep = "")
        
        write(playerhist, filename)
    }
}

keyfile <- readLines("keyfile.txt", n = 1)

mylist <- c("highronic", "suntso")
path <- "./json-files/"

script(mylist, path)