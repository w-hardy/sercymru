
## Name: 07_analysis_cs1_regression.R
## Date: 02/10/2020

## Clear
rm(list=ls())

## Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbiplot)
library(VIM)

##
source("3_rscripts/99_UtilityFunctions.R")

##
## PART 1: Read datasets
##

## Mapping data
mapdata_wales <- read.csv("2_interimdata/4_areadata/mapdata_wales.csv", stringsAsFactors = F) 

## Month number and date name lookups
monthlookup <- read.csv("2_interimdata/_misc/monthlookup.csv", stringsAsFactors = F)
head(monthlookup)

## Create a vector of month names for plots
months <- monthlookup %>%
  filter(month2%in%c(49:102)) %>%
  arrange(month2) %>%
  select(datename)

## GP practice data
df.gpsummary <- readRDS("2_interimdata/2_gpanalysis/gpanalysis1.rds") %>%
  filter(year==2019)
head(df.gpsummary)

##
df.gpattributes <- readRDS("2_interimdata/5_agesexdata/gp_agesexdata_combined.rds")
df.gpqof <- readRDS("2_interimdata/6_qof/gp_qof_dregs_combined.rds")
df.gpphwo <- read.csv("2_interimdata/7_phwodata/gp_phwo_summary_2015.csv", stringsAsFactors = F)

##
df2_kmean <- readRDS( "2_interimdata/1_gpextract/df_cs1_kmeanclust.rds")

##
## Linkd data
##

df2_clust_comb <- df2_kmean %>%
  left_join(df.gpsummary, by="PracticeID") %>%
  left_join(df.gpattributes, by="PracticeID") %>%
  left_join(df.gpqof, by="PracticeID") %>%
  left_join(df.gpphwo, by="PracticeID") 

## Look at missingness
df2_clust_comb %>% aggr(numbers = TRUE, prop = c(FALSE, FALSE),bars=F,combined=T,oma = c(10,5,5,3))

##
## IMD
##

## Plot
df2_comb_wimd <- df2_clust_comb %>%
  mutate(quant = cut(wimd2019, breaks = quantile(df2_clust_comb$wimd2019, probs = c(0,0.25,0.5,0.75,1),na.rm = T) )) %>%
  select(wimd2019, `3`, quant) %>%
  na.omit()

##
df2_comb_wimd %>%
  group_by(quant) %>%
  dplyr::summarise(mean3 = mean(`3`))

##
lm1 <- lm(`3` ~ quant, data = df2_comb_wimd)
summary(lm1)

## Plot age > 65 years
df2_comb_wimd %>%
  ggplot(aes(x=as.factor(quant), y=`3`)) +
  geom_boxplot(position=position_dodge(1),alpha=0.5, fill="#99cccc")#

##
## % Villages
##

## Plot
df2_comb_pcentvillage <- df2_clust_comb %>%
  mutate(quant = cut(pcentvillage, breaks = quantile(df2_clust_comb$pcentvillage, probs = c(0,0.25,0.5,0.75,1),na.rm = T) )) %>%
  select(pcentvillage, `3`, quant) %>%
  na.omit()

##
df2_comb_pcentvillage %>%
  group_by(quant) %>%
  dplyr::summarise(mean3 = mean(`3`))

##
lm1 <- lm(`3` ~ quant, data = df2_comb_pcentvillage)
summary(lm1)

## Plot age > 65 years
df2_comb_pcentvillage %>%
  ggplot(aes(x=as.factor(quant), y=`3`)) +
  geom_boxplot(position=position_dodge(1),alpha=0.5, fill="#99cccc")#

##
## Older ages
##

## Plot
df2_comb_pcent65plus <- df2_clust_comb %>%
  mutate(quant = cut(pcent65plus, breaks = quantile(df2_clust_comb$pcent65plus, probs = c(0,0.25,0.5,0.75,1),na.rm = T) )) %>%
  select(pcent65plus, `3`, quant) %>%
  na.omit()

##
df2_comb_pcent65plus %>%
  group_by(quant) %>%
  dplyr::summarise(mean3 = mean(`3`))

##
lm1 <- lm(`3` ~ quant, data = df2_comb_pcent65plus)
summary(lm1)

## Plot age > 65 years
df2_comb_pcent65plus %>%
  ggplot(aes(x=as.factor(quant), y=`3`)) +
  geom_boxplot(position=position_dodge(1),alpha=0.5, fill="#99cccc")#

##
## Deprivation
##

## Plot
df2_comb_pcentdep <- df2_clust_comb %>%
  mutate(quant = cut(pcentdep, breaks = quantile(df2_clust_comb$pcentdep, probs = c(0,0.25,0.5,0.75,1),na.rm = T) )) %>%
  select(pcentdep, `3`, quant) %>%
  na.omit()

##
df2_comb_pcentdep %>%
  group_by(quant) %>%
  dplyr::summarise(mean3 = mean(`3`))

##
lm1 <- lm(`3` ~ quant, data = df2_comb_pcentdep)
summary(lm1)

## Plot age > 65 years
df2_comb_pcentdep %>%
  ggplot(aes(x=as.factor(quant), y=`3`)) +
  geom_boxplot(position=position_dodge(1),alpha=0.5, fill="#99cccc")#

##
## COPD
##

## Plot
df2_comb_COPD <- df2_clust_comb %>%
  mutate(quant = cut(COPD, breaks = quantile(df2_clust_comb$COPD, probs = c(0,0.25,0.5,0.75,1),na.rm = T) )) %>%
  select(COPD, `3`, quant) %>%
  na.omit()

##
df2_comb_COPD %>%
  group_by(quant) %>%
  dplyr::summarise(mean3 = mean(`3`))

##
lm1 <- lm(`3` ~ quant, data = df2_comb_COPD)
summary(lm1)

## Plot age > 65 years
df2_comb_COPD %>%
  ggplot(aes(x=as.factor(quant), y=`3`)) +
  geom_boxplot(position=position_dodge(1),alpha=0.5, fill="#99cccc")#

##
## DIAB
##

## Plot
df2_comb_DIAB <- df2_clust_comb %>%
  mutate(quant = cut(DIAB, breaks = quantile(df2_clust_comb$DIAB, probs = c(0,0.25,0.5,0.75,1),na.rm = T) )) %>%
  select(DIAB, `3`, quant) %>%
  na.omit()

##
df2_comb_DIAB %>%
  group_by(quant) %>%
  dplyr::summarise(mean3 = mean(`3`))

##
lm1 <- lm(`3` ~ quant, data = df2_comb_DIAB)
summary(lm1)

## Plot age > 65 years
df2_comb_DIAB %>%
  ggplot(aes(x=as.factor(quant), y=`3`)) +
  geom_boxplot(position=position_dodge(1),alpha=0.5, fill="#99cccc")#

##
## AST
##

## Plot
df2_comb_AST <- df2_clust_comb %>%
  mutate(quant = cut(AST, breaks = quantile(df2_clust_comb$AST, probs = c(0,0.25,0.5,0.75,1),na.rm = T) )) %>%
  select(AST, `3`, quant) %>%
  na.omit()

##
df2_comb_AST %>%
  group_by(quant) %>%
  dplyr::summarise(mean3 = mean(`3`))

##
lm1 <- lm(`3` ~ quant, data = df2_comb_AST)
summary(lm1)

## Plot age > 65 years
df2_comb_AST %>%
  ggplot(aes(x=as.factor(quant), y=`3`)) +
  geom_boxplot(position=position_dodge(1),alpha=0.5, fill="#99cccc")#

##
## OBI
##

## Plot
df2_comb_OBI <- df2_clust_comb %>%
  mutate(quant = cut(OBI, breaks = quantile(df2_clust_comb$OBI, probs = c(0,0.25,0.5,0.75,1),na.rm = T) )) %>%
  select(OBI, `3`, quant) %>%
  na.omit()

##
df2_comb_OBI %>%
  group_by(quant) %>%
  dplyr::summarise(mean3 = mean(`3`))

##
lm1 <- lm(`3` ~ quant, data = df2_comb_OBI)
summary(lm1)

## Plot age > 65 years
df2_comb_OBI %>%
  ggplot(aes(x=as.factor(quant), y=`3`)) +
  geom_boxplot(position=position_dodge(1),alpha=0.5, fill="#99cccc")#

##
## FLU
##

## Plot
df2_comb_FLU <- df2_clust_comb %>%
  mutate(quant = cut(FLU, breaks = quantile(df2_clust_comb$FLU, probs = c(0,0.25,0.5,0.75,1),na.rm = T) )) %>%
  select(FLU, `3`, quant) %>%
  na.omit()

##
df2_comb_FLU %>%
  group_by(quant) %>%
  dplyr::summarise(mean3 = mean(`3`))

##
lm1 <- lm(`3` ~ quant, data = df2_comb_FLU)
summary(lm1)

## Plot age > 65 years
df2_comb_FLU %>%
  ggplot(aes(x=as.factor(quant), y=`3`)) +
  geom_boxplot(position=position_dodge(1),alpha=0.5, fill="#99cccc")#

##
## N patients
##

## Plot
df2_comb_npat <- df2_clust_comb %>%
  mutate(quant = cut(npat, breaks = quantile(df2_clust_comb$npat, probs = c(0,0.25,0.5,0.75,1),na.rm = T) )) %>%
  select(npat, `3`, quant) %>%
  na.omit()

##
df2_comb_npat %>%
  group_by(quant) %>%
  dplyr::summarise(mean3 = mean(`3`))

##
lm1 <- lm(`3` ~ quant, data = df2_comb_npat)
summary(lm1)

## Plot age > 65 years
df2_comb_npat %>%
  ggplot(aes(x=as.factor(quant), y=`3`)) +
  geom_boxplot(position=position_dodge(1),alpha=0.5, fill="#99cccc")#

##
## N GP
##

## Plot
df2_comb_ngp <- df2_clust_comb %>%
  mutate(quant = cut(ngp, breaks = quantile(df2_clust_comb$ngp, probs = c(0,0.25,0.5,0.75,1),na.rm = T) )) %>%
  select(ngp, `3`, quant) %>%
  na.omit()

##
df2_comb_ngp %>%
  group_by(quant) %>%
  dplyr::summarise(mean3 = mean(`3`))

##
lm1 <- lm(`3` ~ quant, data = df2_comb_ngp)
summary(lm1)

## Plot age > 65 years
df2_comb_ngp %>%
  ggplot(aes(x=as.factor(quant), y=`3`)) +
  geom_boxplot(position=position_dodge(1),alpha=0.5, fill="#99cccc")#

##
## Disp
##

## Plot
df2_comb_ngp <- df2_clust_comb %>%
  mutate(quant = cut(ngp, breaks = quantile(df2_clust_comb$ngp, probs = c(0,0.25,0.5,0.75,1),na.rm = T) )) %>%
  select(ngp, `3`, quant) %>%
  na.omit()

##
df2_clust_comb %>%
  group_by(dispensing) %>%
  dplyr::summarise(mean3 = mean(`3`))

##
lm1 <- lm(`3` ~ dispensing, data = df2_clust_comb)
summary(lm1)

## Plot age > 65 years
df2_clust_comb %>%
  ggplot(aes(x=as.factor(dispensing), y=`3`)) +
  geom_boxplot(position=position_dodge(1),alpha=0.5, fill="#99cccc")

##
##
##

##
## PART X: Multinomial regression
##

# head(df2_clust_comb)
# 
# df2_mnom <- df2_clust_comb %>%
#   select(PracticeID,grp,ngp,dispensing,pop,DIAB,OBI,AST,FLU,pcentdep,p_65,pcentvillage) %>%
#   na.omit() %>%
#   mutate_at(vars(ngp,pop,DIAB,OBI,AST,FLU,pcentdep,p_65,pcentvillage), ~(.-mean(.))/sd(.))
# 
# ## Look at missingness
# df2_mnom %>% aggr(numbers = TRUE, prop = c(FALSE, FALSE),bars=F,combined=T,oma = c(10,5,5,3))
# 
# 
# test <- multinom(grp ~ p_65, data = df2_mnom)
# summary(test)
# z <- summary(test)$coefficients/summary(test)$standard.errors
# (1 - pnorm(abs(z), 0, 1)) * 2

##
## END
##