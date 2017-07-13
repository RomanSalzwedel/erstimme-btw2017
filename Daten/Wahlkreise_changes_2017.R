### Making a file to track Wahlkreis changes for 2017
### Final goal: Create file for 2002-2017 Wahlkreise and changes for use in models



library(R2HTML)
library(rgdal)
library(spdep)
library(rgeos)
library(plyr)
library(readstata13)

wkrnamen2017 <- read.csv2("btw17_wahlkreisnamen.csv", sep = ";", skip = 5, stringsAsFactors = FALSE, encoding="latin1")
View(wkrnamen2017)

districts <- read.dta13("districts_2002_2013.dta")
View(districts)


vars <- c("wkr_nr2013", 
          "change2013", 
          "new2013", 
          "wkr_name_2013"
)

districts2013 <- districts[, vars]
View(districts2013)

districts2013[districts2013$new2013==1, ]


# wkrnamen2017
change2013 <- vector(mode="numeric", length=299)
new2013 <- vector(mode="numeric", length=299)
wkr_nr2013 <- vector(mode="numeric", length=299)
wkrnamen2017<- cbind(wkrnamen2017, change2013, new2013, wkr_nr2013)


# change2017: 
