
## Name: 01_gpsummary.R
## Date: 08/12/19

## Clear
rm(list=ls())

## Packages
library(readr)
library(dplyr)
library(VIM)

##
## PART 1: GP PRACTICE ANALYSIS
##

## Local data directory
dir_data <- "C:/Users/Dan/Documents/BCUHB"

## NHS Shared Services Partnership data directory
nhssspdir <- paste(dir_data,"/nhswssp/GP Practice Analysis/",sep="")

## List file names
filenames <- list.files(nhssspdir)[-9] # removes 'original' folder

## Function: Read single GP extract
readfile <- function(x) {
  
  ## year
  year <- substr(x,nchar(x)-7,nchar(x)-4)
  
  ## Define path and read data
  fname <- paste(nhssspdir,x,sep="")
  fulldata <- read.csv(fname,stringsAsFactors = F)
  fulldata$year <- as.numeric(year)
  return(fulldata)
}  

## Create list GP extracts
gpdata <- filenames %>%
  purrr::map(readfile)

## Convert GP data list to DF
dfgpdata <- do.call(rbind.data.frame, gpdata) %>%
  dplyr::select(PracticeID,LHBName,year,ngp,postcode,dispensing,nprespats,ndisppats) %>%
  arrange(year,PracticeID)

##
## PART 2: CHECK DUPLICATES
##

## Inspect
head(dfgpdata)
table(dfgpdata$year) # How much data by year
str(dfgpdata)

## Check for duplicate ID codes
dfgpdata %>%
  group_by(year) %>%
  dplyr::summarise(n.unique = length(unique(PracticeID)),
            n.all = length(PracticeID))
# There are 17 duplicates for 2019

## Obtain duplicate practice IDs
dup.ids <- dfgpdata %>% 
  filter(year==2019) %>%
  mutate(dup.flag = duplicated(PracticeID)) %>%
  filter(dup.flag==T) %>%
  select(PracticeID, year, dup.flag)

## View duplicate IDs
dfgpdata %>% 
  filter(year==2019) %>%
  filter(PracticeID%in%dup.ids$PracticeID) %>% 
  arrange(PracticeID)

## Count replicated practice IDs
gpcount1 <- dfgpdata %>%
  group_by(year,PracticeID) %>%
  dplyr::summarise(n = n(),
            maxgp = max(ngp))
table(gpcount1$n) # Check repeated IDs

## Combine nGP data
dfgpdata2 <- dfgpdata %>%
  left_join(gpcount1,by=c("PracticeID","year")) %>%
  mutate(flag = ifelse((n>1 & ngp!=maxgp),1,0)) %>% # If more than one, but has max GPs, flag
  filter(flag == 0) 

## Check for duplicate ID codes
dfgpdata2 %>%
  group_by(year) %>%
  summarise(n.unique = length(unique(PracticeID)),
            n.all = length(PracticeID),
            diff = n.all - n.unique)
# There are 0 duplicates 

##
## MERGE GEOGRAPHY
##

## LSOA WIMD Data (2019)
lsoa_wimd_2019 <- read_csv("2_interimdata/3_wimd/lsoa_wimd_2019.csv", col_names = TRUE) 

## Read post code - location lookup
nspl <- read_csv(paste(dir_data,"/pclookup/Data","NSPL_NOV_2019_UK.csv",sep="/"), col_names = TRUE) %>%
  filter(ctry=="W92000004") %>% # filter Wales
  select(pcd,pcd2,pcds,lat,long,oseast1m,osnrth1m,hlthau,lsoa11) %>%
  `colnames<-`(c("pcd","pcd2","postcode","lat","long","easting","northing","hlthau","lsoacode")) %>%
  mutate_at(vars(northing), as.numeric)
head(nspl)

##
dfgpdata3 <- dfgpdata2 %>%
  select(-n,-maxgp,-flag) %>%
  left_join(nspl %>% select(-pcd,-pcd2,-lat,-long),by="postcode") %>% # get easting northing hlthau lsoa
  mutate(lsoacode = ifelse(postcode=="GL15 6TN","W01001599",lsoacode)) %>% # Manual fix 1
  mutate(lsoacode = ifelse(postcode=="LD6  5ED","W01000487",lsoacode)) %>% # Manual fix 2
  mutate(lsoacode = ifelse(postcode=="LD3 OAW","W01000494",lsoacode)) %>%  # Manual fix 3
  mutate(hlthau = ifelse(is.na(hlthau)==T&LHBName=="Powys","W11000024",hlthau)) %>%
  mutate(hlthau = ifelse(is.na(hlthau)==T&LHBName=="Monmouth","W11000028",hlthau)) %>%
  mutate_at(vars(ngp, nprespats), ~replace(., .==0, NA)) %>%
  left_join(lsoa_wimd_2019 %>% select(lsoacode,wimd2019), by="lsoacode") # get WIMD 2019 

##  
dfgpdata3 %>% aggr(numbers = TRUE, prop = c(FALSE, FALSE),bars=F,combined=T)

## Save dataset
saveRDS(dfgpdata3, "2_interimdata/2_gpanalysis/gpanalysis1.rds")

##
## END
##
