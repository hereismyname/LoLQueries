library(httr) 
library(dplyr)
library(jsonlite)
library(lubridate)

## Sys.sleep(seconds)
## This can throttle the loop in case of overquerying

keyfile <- readLines("keyfile.txt", n = 1)

mylist <- c("highronic", "suntso")
path <- "./json-files/"

script <- function(list, outputpath) {
    for(user in mylist) {
        
       playerid <- getplayerid(user, keyfile)
       
       playerhist <- getmatchhist(playerid, keyfile)
       
       filename <- paste(outputpath, user, Sys.Date(), ".json", sep = "")
       
       write(playerhist, filename)
    }
}

script(mylist, path)