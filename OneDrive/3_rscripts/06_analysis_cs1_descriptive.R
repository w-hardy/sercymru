
## Name: 06_analysis_expl_1.R
## Date: 08/12/19

## Clear
rm(list=ls())

## Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbiplot)

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

## GP practice data
df.gpsummary <- readRDS("2_interimdata/2_gpanalysis/gpanalysis1.rds")
head(df.gpsummary)

## Create a vector of month names for plots
months <- monthlookup %>%
  filter(month2%in%c(49:102)) %>%
  arrange(month2) %>%
  select(datename)

## Prescribing data
df.contraception <- readRDS("C:/Users/Dan/Documents/SERCYMRU/3_data/presc_wales_Contraceptives combined.rds")
head(df.contraception)

##
## Plot all GP time series
##

for (i in unique(df.contraception$PracticeID)) {
  fn_plot1gp(df.contraception, i)
  ggsave(paste("C:/Users/Dan/Documents/SERCYMRU/4_outputs/case1/",i,".tiff",sep=""),
         dpi=200, width = 25, height = 12, units = "cm")
}

##
## Create dataset of pre- and post-COVID prescribing 
##

## 
df1 <- df.contraception %>%
  filter(grepl("Mar|Apr|May|Jun",datename)) %>%
  group_by(PracticeID, HB, Locality, year, month2, month) %>%
  dplyr::summarise(sumq = sum(Quantity, na.rm = T)) %>%
  #mutate(sumq_pp = sumq/nprespats) %>%
  mutate(control = ifelse(year=="2020","covid","baseline")) %>%
  group_by(PracticeID, control, month) %>%
  dplyr::summarise(meanq_sumq = mean(sumq, na.rm = T)) %>%
  pivot_wider(names_from = control, values_from = meanq_sumq) %>%
  mutate(pcdiff = (covid - baseline)/baseline) %>%
  ungroup()
head(df1)

##
saveRDS(df1, "2_interimdata/1_gpextract/df1_pcmonthly.rds")

##
## Distribution of % changes (March)
##

df1 %>%
  ggplot() + 
  geom_histogram(aes(x=pcdiff), fill = "#99cccc") +
  xlab("Percentage change") + 
  ylab("Number of GP practices") +
  theme_classic()  

##
## Map changes
## 

##
## MARCH
##

##
df1_map.march <- df1 %>%
  left_join(filter(df.gpsummary,year==2020), by = "PracticeID") %>%
  filter(month==3) %>%
  mutate(pcdiff = ifelse(pcdiff > 1, NA, pcdiff))

##
df1_map.march %>%
  ggplot() + 
  geom_polygon(data = mapdata_wales, aes(x = long, y = lat, group=county), fill = "#ffffff", color="#cccccc", size = 1) + # Plot the map
  geom_point(aes(x = easting, y = northing, color = pcdiff), size=3, alpha=0.9) + # Plot the GPs  
  coord_fixed(1) + 
  ggtitle("March") +
  scale_color_gradient2(name="GP (%)",
                        midpoint=mean(df1_map.march$pcdiff,na.rm = T), 
                        low="blue", 
                        mid="white",
                        high="red", 
                        space ="Lab" ) +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y = element_blank())

ggsave(paste("C:/Users/Dan/Documents/SERCYMRU/4_outputs/3_Map_March.tiff",sep=""),
       dpi=200, width = 30, height = 17, units = "cm")

##
## APRIL
##

##
df1_map.april <- df1 %>%
  left_join(filter(df.gpsummary,year==2020), by = "PracticeID") %>%
  filter(month==4) %>%
  mutate(pcdiff = ifelse(pcdiff > 1, NA, pcdiff))

##
df1_map.april %>%
  ggplot() + 
  geom_polygon(data = mapdata_wales, aes(x = long, y = lat, group=county), fill = "#ffffff", color="#cccccc", size = 1) + # Plot the map
  geom_point(aes(x = easting, y = northing, color = pcdiff), size=3, alpha=0.9) + # Plot the GPs  
  coord_fixed(1) + 
  ggtitle("April") +
  scale_color_gradient2(name="GP (%)",
                        midpoint=mean(df1_map.april$pcdiff,na.rm = T), 
                        low="blue", 
                        mid="white",
                        high="red", 
                        space ="Lab" ) +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y = element_blank())

##
ggsave(paste("C:/Users/Dan/Documents/SERCYMRU/4_outputs/4_Map_April.tiff",sep=""),
       dpi=200, width = 30, height = 17, units = "cm")

##
## MAY
##

##
df1_map.may <- df1 %>%
  left_join(filter(df.gpsummary,year==2020), by = "PracticeID") %>%
  filter(month==5) %>%
  mutate(pcdiff = ifelse(pcdiff > 1, NA, pcdiff))

##
df1_map.may %>%
  ggplot() + 
  geom_polygon(data = mapdata_wales, aes(x = long, y = lat, group=county), fill = "#ffffff", color="#cccccc", size = 1) + # Plot the map
  geom_point(aes(x = easting, y = northing, color = pcdiff), size=3, alpha=0.9) + # Plot the GPs  
  coord_fixed(1) + 
  ggtitle("May") +
  scale_color_gradient2(name="GP (%)",
                        midpoint=mean(df1_map.may$pcdiff,na.rm = T), 
                        low="blue", 
                        mid="white",
                        high="red", 
                        space ="Lab" ) +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y = element_blank())

##
ggsave(paste("C:/Users/Dan/Documents/SERCYMRU/4_outputs/5_Map_May.tiff",sep=""),
       dpi=200, width = 30, height = 17, units = "cm")

##
## JUNE
##

##
df1_map.june <- df1 %>%
  left_join(filter(df.gpsummary,year==2020), by = "PracticeID") %>%
  filter(month==6) %>%
  mutate(pcdiff = ifelse(pcdiff > 1, NA, pcdiff))

##
df1_map.june %>%
  ggplot() + 
  geom_polygon(data = mapdata_wales, aes(x = long, y = lat, group=county), fill = "#ffffff", color="#cccccc", size = 1) + # Plot the map
  geom_point(aes(x = easting, y = northing, color = pcdiff), size=3, alpha=0.9) + # Plot the GPs  
  coord_fixed(1) + 
  ggtitle("June") +
  scale_color_gradient2(name="GP (%)",
                        midpoint=mean(df1_map.june$pcdiff,na.rm = T), 
                        low="blue", 
                        mid="white",
                        high="red", 
                        space ="Lab" ) +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y = element_blank())

##
ggsave(paste("C:/Users/Dan/Documents/SERCYMRU/4_outputs/6_Map_June.tiff",sep=""),
       dpi=200, width = 30, height = 17, units = "cm")


##
## PART 2: Descriptive statistics
##

## Table 1
# All contraceptives
# Total items
df.contraception %>%
  filter(grepl("Mar|Apr|May|Jun|Jul",datename)) %>%
  group_by(PracticeID, year, month2, month) %>%
  dplyr::summarise(sumq = sum(Items, na.rm = T)) %>%
  group_by(year, month) %>%
  dplyr::summarise(mean_sumq = mean(sumq, na.rm=T)) %>%
  pivot_wider(names_from = month, values_from = mean_sumq)

## Table 2
# All contraceptives
# Total quantity
df.contraception %>%
  left_join(df.gpsummary, by=c("PracticeID", "year"))%>%
  filter(grepl("Mar|Apr|May|Jun",datename)) %>%
  group_by(PracticeID, nprespats, ndisppats, year, month2, month) %>%
  dplyr::summarise(sumq = sum(Quantity, na.rm = T)) %>%
  mutate(sumq_pp = sumq/nprespats) %>%
  group_by(year, month) %>%
  dplyr::summarise(meansumq_pp = mean(sumq_pp, na.rm=T)) %>%
  pivot_wider(names_from = month, values_from = meansumq_pp)

## Table 3
# 0703010F0 only
# Total Quantity
df.contraception %>%
  filter(bnfchem == "0703010F0") %>%
  left_join(df.gpsummary, by=c("PracticeID", "year"))%>%
  filter(grepl("Mar|Apr|May|Jun",datename)) %>%
  group_by(PracticeID, nprespats, ndisppats, year, month2, month) %>%
  dplyr::summarise(sumq = sum(Quantity, na.rm = T)) %>%
  mutate(sumq_pp = sumq/nprespats) %>%
  group_by(year, month) %>%
  dplyr::summarise(meansumq_pp = mean(sumq_pp, na.rm=T)) %>%
  pivot_wider(names_from = month, values_from = meansumq_pp)

## Table 4
# 0703021Q0 only
# Total Quantity
df.contraception %>%
  filter(bnfchem == "0703021Q0") %>%
  left_join(df.gpsummary, by=c("PracticeID", "year"))%>%
  filter(grepl("Mar|Apr|May|Jun",datename)) %>%
  group_by(PracticeID, nprespats, ndisppats, year, month2, month) %>%
  dplyr::summarise(sumq = sum(Quantity, na.rm = T)) %>%
  mutate(sumq_pp = sumq/nprespats) %>%
  group_by(year, month) %>%
  dplyr::summarise(meansumq_pp = mean(sumq_pp, na.rm=T)) %>%
  pivot_wider(names_from = month, values_from = meansumq_pp)

##
## END
##