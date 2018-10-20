library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(data.table)
# install.packages("rjson")
library(rjson)
library(stringr)
library(tidyr)

df <- read.csv("retail-food-stores.csv")
names(df)
LocTxt <- c()
Loc <- df$Location
for (i in 1: length(Loc)){
    LocTxt[i] <- gsub("\\'", '"', toString(Loc[i]))
}
Location<- as.data.frame(LocTxt, row.names = NULL, colnames = names("Location"))

df2 <- function(keyword, Lst){
    if (Lst != ""){
        str <- strsplit(Lst, ':|,')[[1]]
        for (i in 1: length(str)){
        
                if (grepl(keyword, str[i]) == TRUE){ 
                    value <- gsub('\\"|\\"|\\{|\\}| ', "", str[i+1])
                    
                    return (value)
                } 
            }
        }        
    else {
        value <- "N/A"
        return (value)
    }           
}     


split <- sapply(Location[,1], toString)
Logi <- lapply(split, df2, keyword = 'longitude')
Lati <- lapply(split, df2, keyword = 'latitude')
Address <- lapply(split, df2, keyword = '"address"')
City <- lapply(split, df2, keyword = 'city')
State <- lapply(split, df2, keyword = 'state')
Zip <- lapply(split, df2, keyword = 'zip')

# df2('longitude', split )
# sapply(split,  df2('longitude', split ))
newDf <- mutate(df, address = unlist(Address), city = unlist(City), state= unlist(State),  zip= unlist(Zip) ,longitute = unlist(lapply(Logi, toString))
, latitude = unlist(lapply(Lati, toString)))
write.table(newDf, "New_retail-food-stores.csv", sep = ",")
# ndf <- read.csv("New_retail-food-stores.csv", sep = ",")

clean <- function(fname) {
    d <- read.csv("New_retail-food-stores.csv")

    ## subset out the col no needed 
    df <- d[, c(-9,-10,-11,-12,-13, -15, -16)]
    ## rename all the col
    df <- rename(df, City =city, State = state, ZipCode = zip, Longitude = longitute, Latitude = latitude)
    ## parse all spaces on left and right of strings
    for (i in names(df)){
        df[, c(i)] <- sapply(df[, c(i)], trimws, which =c("both") )
    }
    df$Address <-  paste(df$Street.Number, df$Street.Name, sep = " ")
    df <- df[,c(2,3,4,5,6,9, 15,7,8,10,11,12,1, 13,14)]
    df <- df[, c(-9,-10)]
    ## to parse number out of a string :
    # regexp <- "[[:digit:]]+"
    # regexb <- "[[:alpha:]]+"
    # df$AddressNumber <- lapply(df$Address, str_extract, pattern = regexp) 
    # df$AddressStreet <- lapply(df$Address, str_extract, pattern = regexb)
    # df <- df[, c(-8)]
    # df <- transform(df,  Address = paste(df$AddressNumber, df$AddressStreet, sep = " "))

    return(df)
}
df <- clean("New_retail-food-stores.csv")
write.table(df, "retail-food-stores_tidyData.csv", sep = ",")