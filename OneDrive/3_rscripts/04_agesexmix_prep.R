
## Name: 01_gpsummary.R
## Date: 08/12/19

## Clear
rm(list=ls())

## Packages
library(readr)
library(dplyr)
library(VIM)

##
## Step 5: AGE SEX MIX (2017-2018)
##

## Function to combine data from two years
fn_combine <- function(dat_new, dat_old) {
  tmpdat <- full_join(dat_new, dat_old, by="PracticeID") # do a 'full' join
  newdat <- tmpdat %>% # coalesce to keep newer data where possible
    mutate(GPCluster = coalesce(GPCluster.x, GPCluster.y),
           npat  = coalesce(npat.x, npat.y),
           npat_male = coalesce(npat_male.x, npat_male.y),
           npat_fem  = coalesce(npat_fem.x, npat_fem.y),
           nm_0_4  = coalesce(nm_0_4.x, nm_0_4.y),
           nm_5_14 = coalesce(nm_5_14.x, nm_5_14.y),
           nm_15_44 = coalesce(nm_15_44.x, nm_15_44.y),
           nm_45_64 = coalesce(nm_45_64.x, nm_45_64.y),
           nm_65_74 = coalesce(nm_65_74.x, nm_65_74.y),
           nm_75_84 = coalesce(nm_75_84.x, nm_75_84.y),
           nm_85 = coalesce(nm_85.x, nm_85.y),
           nf_0_4  = coalesce(nf_0_4.x, nf_0_4.y),
           nf_5_14 = coalesce(nf_5_14.x, nf_5_14.y),
           nf_15_44 = coalesce(nf_15_44.x, nf_15_44.y),
           nf_45_64 = coalesce(nf_45_64.x, nf_45_64.y),
           nf_65_74 = coalesce(nf_65_74.x, nf_65_74.y),
           nf_75_84 = coalesce(nf_75_84.x, nf_75_84.y),
           nf_85 = coalesce(nf_85.x, nf_85.y)) %>%
    select(-c(npat.x:nf_85.y))
  return(
    newdat
  )
}

## Read data
gp_agesexdata_2018 <- read_csv(paste("2_interimdata/5_agesexdata","gp_agesexdata_2018.csv",sep="/"), col_names = TRUE) %>% 
  select(-year,-lhb,-la) %>%
  mutate_at(vars(starts_with("n")), ~as.integer(.))
gp_agesexdata_2017 <- read_csv(paste("2_interimdata/5_agesexdata","gp_agesexdata_2017.csv",sep="/"), col_names = TRUE) %>% 
  select(-year,-lhb) %>%
  mutate_at(vars(starts_with("n")), ~as.integer(.))

## Examine
head(gp_agesexdata_2018); head(gp_agesexdata_2017)

## Combine
gp_agesexdata <- fn_combine(gp_agesexdata_2018, gp_agesexdata_2017) %>%
  select(-GPCluster.x)

## Look at missingness
gp_agesexdata %>% aggr(numbers = TRUE, prop = c(FALSE, FALSE),bars=F,combined=T,oma = c(10,5,5,3))

## Save dataset
saveRDS(gp_agesexdata, "2_interimdata/5_agesexdata/gp_agesexdata_combined.rds")

##
## Step 6: QoF Disease Registers
##

## Function to combine data from two years
fn_combine_qof <- function(dat_new, dat_old) {
  tmpdat <- full_join(dat_new, dat_old, by="PracticeID")
  newdat <- tmpdat %>%
    mutate(GPCluster = coalesce(GPCluster.x, GPCluster.y),
           pop  = coalesce(pop.x, pop.y),
           CHD  = coalesce(CHD.x, CHD.y),
           COPD = coalesce(COPD.x, COPD.y),
           DEM  = coalesce(DEM.x, DEM.y),
           DIAB = coalesce(DIAB.x, DIAB.y),
           EPI  = coalesce(EPI.x, EPI.y),
           HF   = coalesce(HF.x, HF.y),
           HYP  = coalesce(HYP.x, HYP.y),
           LRN  = coalesce(LRN.x, LRN.y),
           MEN  = coalesce(MEN.x, MEN.y),
           OBI  = coalesce(OBI.x, OBI.y),
           OST  = coalesce(OST.x, OST.y),
           PAL  = coalesce(PAL.x, PAL.y),
           RAR  = coalesce(RAR.x, RAR.y),
           STR = coalesce(STR.x, STR.y),
           AF   = coalesce(AF.x, AF.y),
           AST  = coalesce(AST.x, AST.y),
           CAN  = coalesce(CAN.x, CAN.y),
           FLU  = coalesce(FLU.x, FLU.y)) %>%
    select(-c(GPCluster.x:FLU.y))
  
  return(
    newdat
  )
}

## Read register data
gp_qof_dregs_2019 <- read_csv(paste("2_interimdata/6_qof","gp_qof_dregs_2018-19.csv",sep="/"), col_names = TRUE) %>%
  select(-name,-lhbcode,-la,-lacode,-clustercode) %>% arrange(GPCluster)
gp_qof_dregs_2018 <- read_csv(paste("2_interimdata/6_qof","gp_qof_dregs_2017-18.csv",sep="/"), col_names = TRUE) 
gp_qof_dregs_2017 <- read_csv(paste("2_interimdata/6_qof","gp_qof_dregs_2016-17.csv",sep="/"), col_names = TRUE) %>%
  select(-lhbcode)

## Inspect
head(gp_qof_dregs_2017); head(gp_qof_dregs_2018); head(gp_qof_dregs_2019)

## Merge 18 and 19
gp_qof_dregs <- fn_combine_qof(gp_qof_dregs_2019, gp_qof_dregs_2018)

## Check missingness
gp_qof_dregs %>% aggr(numbers = TRUE, prop = c(FALSE, FALSE),bars=F,combined=T)

## Merge again
gp_qof_dregs2 <- fn_combine_qof(gp_qof_dregs, gp_qof_dregs_2017) %>% # merge with 17
  select(-year.x,-lhb.x) %>%
  mutate_at(vars(CHD:FLU), 
            ~./pop)

## Check missingness
gp_qof_dregs2 %>% aggr(numbers = TRUE, prop = c(FALSE, FALSE),bars=F,combined=T,oma = c(10,5,5,3))

## Save dataset
saveRDS(gp_qof_dregs2, "2_interimdata/6_qof/gp_qof_dregs_combined")

##
## END
##
