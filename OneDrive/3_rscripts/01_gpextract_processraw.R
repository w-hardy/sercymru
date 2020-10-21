
## Name: 01_Processraw.R
## Date: 16/09/2020

## Clear
rm(list=ls())

## Packages
library(dplyr)
library(dbplyr)

##
## PART 1: Preliminaries
##

## Directory for source files
dir_data <- "C:/Users/Dan/Documents/BCUHB"

## Month number and date name lookups
monthlookup <- read.csv("2_interimdata/_misc/monthlookup.csv", stringsAsFactors = F)
head(monthlookup)

## BNF chemical code to chemical name lookup
chemlookup <- read.csv("1_data/bnfchemsubs_lookup.csv", stringsAsFactors = F)
head(chemlookup)

## Function: Read single GP extract
readfile <- function(x, dir) {
  
  ## Define path and read data
  fname <- paste(dir,x,"/",x,".csv",sep="")
  fulldata <- read.csv(fname,stringsAsFactors = F)
  
  ## If columns = 12 add two blanks
  if(length(fulldata)==12) {
    fulldata <- fulldata %>%
      mutate(DDD=NA) %>%
      mutate(ADQ=NA)
  }
  return(fulldata)
}  

##
## PART 2: Read prescribing data
##

## NHS Shared Services Partnership data directory
nhs.ssp.dir <- paste(dir_data,"/nhswssp/GP Data Extract/",sep="")

## List file names
filenames_all <- list.files(nhs.ssp.dir)

## Subset years 2016-20
filenames_set1 <- c(filenames_all[grepl("2020", filenames_all)],
                    filenames_all[grepl("2019", filenames_all)],
                    filenames_all[grepl("2018", filenames_all)],
                    filenames_all[grepl("2017", filenames_all)],
                    filenames_all[grepl("2016", filenames_all)])


## Create list GP extracts
lgpdata <- filenames_set1 %>%
  purrr::map(readfile, dir=nhs.ssp.dir)

## Convert GP data list to DF
gppresdata <- do.call(rbind.data.frame, lgpdata)
rm(lgpdata)

## Checks
str(gppresdata)
sum(is.na(gppresdata$BNFCode)==T)

## Merge in DDD multipliers
gppresdata2 <- gppresdata %>% 
  mutate(year = as.numeric(substr(Period,1,4))) %>% # extract year
  mutate(year2 = as.numeric(year)-2012) %>% # year relative to 2012 baseline
  mutate(month = as.numeric(substr(Period,5,6))) %>% # extract month
  mutate(month2 = month + (year2*12)) %>% # number of months since baseline
  left_join(monthlookup,by="month2") %>% # merge month numberes and names
  mutate(bnfchem = substr(BNFCode, 0, 9)) %>%
  mutate(bnfsubpara = substr(BNFCode, 0, 7)) %>%
  left_join(chemlookup, by="bnfchem") %>% # merge chemical codes and names
  mutate(chemname = ifelse(is.na(chemname)==T, paste(BNFName," [BNFName]"), chemname))

## Checks and tidy
head(gppresdata2)
rm(gppresdata)

## 
allchems <- unique(gppresdata2$bnfchem)
write.csv(allchems, "2_interimdata/allchems.csv")

## Save dataset
#saveRDS(gppresdata2,"C:/Users/Dan/Documents/SERCYMRU/3_data/allpresc_wales_16-20.rds")

##
## PART 3: Create SQLite
##

## Database location
my_db_file <- paste(dir_data,"db-gpextract.sqlite",sep="/")
#my_db <- src_sqlite(my_db_file, create = TRUE)

## Connect and write data
con <- DBI::dbConnect(RSQLite::SQLite(), my_db_file)
DBI::dbWriteTable(conn = con, 
                  name = "gpextract", 
                  gppresdata2, 
                  overwrite=T,
                  row.names=FALSE)
src_dbi(con)
DBI::dbDisconnect(con)

##
## END
##
