
## Name: 02_splitcases.R
## Date: 16/09/2020

## Clear
rm(list=ls())

## Packages
library(dplyr)
library(VIM)
library(ggplot2)

## Plotting functions
source("3_rscripts/99_UtilityFunctions.R")

##
## PART 1: LOAD DATA
##

## Connect to database
con <- DBI::dbConnect(RSQLite::SQLite(), "C:/Users/Dan/Documents/BCUHB/db-gpextract.sqlite")
db.gpextract <- tbl(con, "gpextract")

## Month number and date name lookups
monthlookup <- read.csv("C:/Users/Dan/OneDrive/PROJECTS/SERCYMRU/2_interimdata/_misc/monthlookup.csv", stringsAsFactors = F)
head(monthlookup); tail(monthlookup)

## Observation window
startmonth <- 73 # Jan-18
len <- length(73:103) # Jan-18 - 	Jul-20

## Create a vector of month names for plots
months <- monthlookup %>%
  filter(month2%in%c(startmonth:103)) %>%
  arrange(month2) %>%
  select(datename)

##
## PART 2: SUBSET CASES
##

##
## Chloroquine
##

## Treatment name
name <- "Chloroquine"

## Subset 1
chlor <- c("0504010F0", "1001030C0", "0504010G0", "0504010Z0")
subdf_chlor <- filter(db.gpextract, bnfchem%in%chlor) %>% collect()
fn_plotbnf(subdf_chlor, name) # Focus on 1001030C0

## Subset 2
chlor <- c("1001030C0")
subdf_chlor <- filter(db.gpextract, bnfchem%in%chlor) %>% collect()
fn_plotbnf(subdf_chlor, name)

## Outputs
ggsave(paste("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot4/trends/plot_", name, ".tiff",sep=""), dpi=200, width = 16, height = 6, units = "cm")
saveRDS(subdf_chlor, paste("C:/Users/Dan/Documents/SERCYMRU/3_data/presc_wales_",name,".rds",sep=""))

##
## Azithromycin & Famotidine 
## 

## Azithromycin
name <- "Azithromycin"
azith <- c("0501050A0")
subdf_azith <- filter(db.gpextract, bnfchem%in%azith) %>% collect()
fn_plotbnf(subdf_azith,name)
ggsave(paste("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot4/trends/plot_", name, ".tiff",sep=""), dpi=200, width = 16, height = 6, units = "cm")
saveRDS(subdf_azith, paste("C:/Users/Dan/Documents/SERCYMRU/3_data/presc_wales_",name,".rds",sep=""))

## Famotidine
name <- "Famotidine"
famot <- c("0103010H0")
subdf_famot <- filter(db.gpextract, bnfchem%in%famot) %>% collect()
fn_plotbnf(subdf_famot,name)
ggsave(paste("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot4/trends/plot_", name, ".tiff",sep=""), dpi=200, width = 16, height = 6, units = "cm")
saveRDS(subdf_famot, paste("C:/Users/Dan/Documents/SERCYMRU/3_data/presc_wales_",name,".rds",sep=""))

##
## Vitamin D
##

## Treatment name
name <- "Vitamin D"

## Subset 1
vitmd <- c("0906040B0","0906040C0","0906040G0","0906040K0","0906040N0","090604800","0906040P0")
subdf_vitmd <- filter(db.gpextract, bnfchem%in%vitmd) %>% collect()
fn_plotbnf(subdf_vitmd, name) # Focus on 0906040G0, 0906040N0

## Subset 2
vitmd <- c("0906040G0", "0906040N0")
subdf_vitmd <- filter(db.gpextract, bnfchem%in%vitmd) %>% collect()
fn_plotbnf(subdf_vitmd, name)

## Outputs
ggsave(paste("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot4/trends/plot_", name, ".tiff",sep=""), dpi=200, width = 16, height = 6, units = "cm")
saveRDS(subdf_vitmd, paste("C:/Users/Dan/Documents/SERCYMRU/3_data/presc_wales_",name,".rds",sep=""))

##
## Corticosteroids
##

## Treatment name
name <- "Corticosteroids (respiratory)"

## Subset 1
ctico_subpara <- c("0302000")
subdf_ctico <- filter(db.gpextract, bnfsubpara%in%ctico_subpara) %>% collect()
fn_plotbnf(subdf_ctico,name) # Focus on 1001022K0 and 1001022U0
subdf_ctico %>% group_by(chemname,bnfchem) %>% summarise(sumi = sum(Items)) %>% arrange(-sumi) %>% print(n=100)

## Subset 2
ctico <- c("0302000C0","0302000N0","0302000K0")
subdf_ctico <- filter(db.gpextract, bnfchem%in%ctico) %>% collect()
fn_plotbnf(subdf_ctico,name) 

## Outputs
ggsave(paste("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot4/trends/plot_", name, ".tiff",sep=""), dpi=200, width = 16, height = 6, units = "cm")
saveRDS(subdf_ctico, paste("C:/Users/Dan/Documents/SERCYMRU/3_data/presc_wales_",name,".rds",sep=""))

##
## Nicotine
##

## Treatment name
name <- "Nicotine"

## Subset 1
nicot <- c(	"0410000P0","0410020A0","0410020B0","0410020D0","0410020C0")
subdf_nicot <- filter(db.gpextract, bnfchem%in%nicot) %>% collect()
subdf_nicot %>% group_by(chemname,bnfchem) %>% summarise(sumi = sum(Items)) %>% arrange(-sumi) %>% print(n=100)
fn_plotbnf(subdf_nicot, name)

## Subset 2
subdf_nicot2 <- subdf_nicot %>%
  mutate(chemname = ifelse(bnfchem == "0410000P0", "Nicotine various", chemname)) %>%
  filter(bnfchem!="0410020D0")
subdf_nicot2 %>% group_by(chemname,bnfchem) %>% summarise(sumi = sum(Items)) %>% arrange(-sumi) %>% print(n=100)
fn_plotbnf(subdf_nicot2, name)

## Outputs
ggsave(paste("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot4/trends/plot_", name, ".tiff",sep=""), dpi=200, width = 16, height = 6, units = "cm")
saveRDS(subdf_nicot2, paste("C:/Users/Dan/Documents/SERCYMRU/3_data/presc_wales_",name,".rds",sep=""))

##
## Paracetamol
##

## Treatment name
name <- "Paracetamol"

## Subset 1
parac_subpara <- c("0407010")
subdf_parac <- filter(db.gpextract, bnfsubpara%in%parac_subpara) %>% collect()
subdf_parac %>% group_by(chemname,bnfchem) %>% summarise(sumi = sum(Items)) %>% arrange(-sumi) %>% print(n=100)
fn_plotbnf(subdf_parac,name)# Focus on 0407010F0 0407010H0  

## Subset 2
#parac2 <- c("0407010F0","0407010H0")
#subdf_parac <- filter(db.gpextract, bnfchem%in%parac2) %>% collect()
#fn_plotbnf(subdf_parac,name)

## Outputs
ggsave(paste("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot4/trends/plot_", name, ".tiff",sep=""), dpi=200, width = 16, height = 6, units = "cm")
saveRDS(subdf_parac, paste("C:/Users/Dan/Documents/SERCYMRU/3_data/presc_wales_",name,".rds",sep=""))

##
## ACE Inhibitors
##

## Treatment name
name <- "ACE Inhibitors"

## Subset 1
ainhib <- c("0205053","0205052","0205051")
subdf_ainhib <- filter(db.gpextract, bnfsubpara%in%ainhib) %>% collect()
subdf_parac %>% group_by(chemname,bnfchem) %>% summarise(sumi = sum(Items)) %>% arrange(-sumi) %>% print(n=100)
fn_plotbnf(subdf_ainhib,name)

## Subset 2
ainhib <- c("0407010F0","0407010H0","0407010N0","0407010X0","0407010Y0","0407010B0")
subdf_ainhib <- filter(db.gpextract, bnfchem%in%ainhib) %>% collect()
fn_plotbnf(subdf_ainhib,name)

## Outputs
ggsave(paste("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot4/trends/plot_", name, ".tiff",sep=""), dpi=200, width = 16, height = 6, units = "cm")
saveRDS(subdf_ainhib,paste("C:/Users/Dan/Documents/SERCYMRU/3_data/presc_wales_",name,".rds",sep=""))

##
## Oral Contraceptives
##

##
## 7.3.1: Combined hormonal contraceptives and systems
name <- "Contraceptives combined"

## Subset 1
subdf_contra1 <- filter(db.gpextract, bnfsubpara%in%c("0703010","0703011")) %>% collect()
subdf_contra1 %>% group_by(chemname,bnfchem) %>% summarise(sumi = sum(Items)) %>% arrange(-sumi) %>% print(n=100)
fn_plotbnf(subdf_contra1,name) # Focus "0703010E0","0703010F0","0703010G0"

## Subset 2
contra1 <- c("0703010E0","0703010F0","0703010G0")
subdf_contra1 <- filter(db.gpextract, bnfchem%in%contra1) %>% collect()
fn_plotbnf(subdf_contra1,name) 

## Outputs
ggsave(paste("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot4/trends/plot_", name, ".tiff",sep=""), dpi=200, width = 16, height = 6, units = "cm")
saveRDS(subdf_contra1,paste("C:/Users/Dan/Documents/SERCYMRU/3_data/presc_wales_",name,".rds",sep=""))

##
## 7.3.2: Progestogen-only contraceptives
name <- "Progestogen-only"

## Subset 1
subdf_contra2 <- filter(db.gpextract, bnfsubpara%in%c("0703021","0703022","0703023")) %>% collect()
subdf_contra2 %>% group_by(chemname,bnfchem) %>% summarise(sumi = sum(Items)) %>% arrange(-sumi) %>% print(n=100)
fn_plotbnf(subdf_contra2,name) # Focus "0703010E0","0703010F0","0703010G0"

## Subset 2
subdf_contra2 <- filter(db.gpextract, bnfchem%in%c("0703021Q0","0703022M0","0703021N0")) %>% collect()
subdf_contra2 %>% group_by(chemname,bnfchem) %>% summarise(sumi = sum(Items)) %>% arrange(-sumi) %>% print(n=100)
fn_plotbnf(subdf_contra2,name) # Focus "0703010E0","0703010F0","0703010G0"

## Outputs
ggsave(paste("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot4/trends/plot_", name, ".tiff",sep=""), dpi=200, width = 16, height = 6, units = "cm")
saveRDS(subdf_contra2, paste("C:/Users/Dan/Documents/SERCYMRU/3_data/presc_wales_",name,".rds",sep=""))

##
## 7.3.3 & 7.3.4
name <- "Contraception Other"

## Subset 1
contra3 <- c("0703050A0","0703050B0","0703030G0")
subdf_contra3 <- filter(db.gpextract, bnfchem%in%contra3) %>% collect()
fn_plotbnf(subdf_contra3,name) # Focus none

## Outputs
ggsave(paste("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot4/trends/plot_", name, ".tiff",sep=""), dpi=200, width = 16, height = 6, units = "cm")
saveRDS(filter(gppresdata2, bnfchem%in%c("0703010E0","0703010F0","0703010G0","0703021Q0")),paste("C:/Users/Dan/Documents/SERCYMRU/3_data/presc_wales_",name,".rds",sep=""))

## Save contraceptives combined
saveRDS(rbind(subdf_contra1,subdf_contra2,subdf_contra3),"C:/Users/Dan/Documents/SERCYMRU/3_data/presc_wales_contraception_main.rds")

##
## Oral anticoagulants
##

##
## END
##