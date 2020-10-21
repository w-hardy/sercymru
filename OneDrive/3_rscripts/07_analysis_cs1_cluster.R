
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

## Create a vector of month names for plots
months <- monthlookup %>%
  filter(month2%in%c(49:102)) %>%
  arrange(month2) %>%
  select(datename)

## GP practice data
df.gpsummary <- readRDS("2_interimdata/2_gpanalysis/gpanalysis1.rds") %>%
  select(PracticeID, year, )
head(df.gpsummary)

## Prescribing data
df.contraception <- readRDS("C:/Users/Dan/Documents/SERCYMRU/3_data/presc_wales_Contraceptives combined.rds")
head(df.contraception)

##
## Create dataset of pre- and post-COVID prescribing 
##

## 
df1 <- df.contraception %>%
  left_join(filter(df.gpsummary,year==2020)%>%select(-year), by = "PracticeID") %>%
  filter(grepl("Mar|Apr|May|Jun",datename)) %>%
  group_by(PracticeID, HB, nprespats, Locality, year, month2, month) %>%
  dplyr::summarise(sumq = sum(Quantity, na.rm = T)) %>%
  mutate(sumq_pp = sumq/nprespats) %>%
  mutate(control = ifelse(year=="2020","covid","baseline")) %>%
  group_by(PracticeID, control, month) %>%
  dplyr::summarise(meanq_sumq = mean(sumq_pp, na.rm = T)) %>%
  pivot_wider(names_from = control, values_from = meanq_sumq) %>%
  mutate(pcdiff = (covid - baseline)/baseline) %>%
  ungroup()
head(df1)

##
## PART 2: Clustering
##

## Simplify dataset for clustering
df2_clust <- df1 %>%
  na.omit() %>%
  select(PracticeID, month, pcdiff) %>%
  pivot_wider(names_from = month, values_from = pcdiff) %>%
  mutate_all(~replace(., is.na(.)==T, 0))
head(df2_clust)

## Run clustering algorithms using 3 different methods
hc.ms1 <- hclust(dist(df2_clust[,c(2:5)]), method="complete")
hc.ms2 <- hclust(dist(df2_clust[,c(2:5)]), method="average")
hc.ms3 <- hclust(dist(df2_clust[,c(2:5)]), method="single")

## Plot hierarchical clustering results
plot(hc.ms1, cex = 0.6)
plot(hc.ms2, cex = 0.6)
plot(hc.ms3, cex = 0.6)

## Save dendrograms
#jpeg("rplot.jpg", width = 5000, height = 3000)
#plot(hc.ms1, hang = -1, cex = 0.6)
#dev.off()

## Outlier
fn_plot1gp(filter(df.contraception,bnfchem=="0703010F0"),"W93614")

##
df2_clust$grp <- cutree(hc.ms1, k = 5)

##
id_grp1 <- filter(df2_clust,grp==1)$PracticeID
id_grp2 <- filter(df2_clust,grp==2)$PracticeID
id_grp3 <- filter(df2_clust,grp==3)$PracticeID
id_grp4 <- filter(df2_clust,grp==4)$PracticeID
id_grp5 <- filter(df2_clust,grp==5)$PracticeID

##
## PART 1: Plot by cluster
##

p1 <- df2_clust %>%
  filter(grp==1) %>%
  select(-grp) %>%
  gather(key,value,-PracticeID) %>%
  ggplot() +
  geom_col(aes(x=key,y=value), fill="blue") +
  facet_wrap(~PracticeID)
ggsave("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot4/grp4/plot_grp1.tiff", 
       p1,
       dpi=100, width = 60, height = 30, units = "cm")

p2 <- df2_clust %>%
  filter(grp==2) %>%
  select(-grp) %>%
  gather(key,value,-PracticeID) %>%
  ggplot() +
  geom_col(aes(x=key,y=value), fill="blue") +
  facet_wrap(~PracticeID)
ggsave("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot4/grp4/plot_grp2.tiff", 
       p2,
       dpi=100, width = 80, height = 40, units = "cm")

p3 <- df2_clust %>%
  filter(grp==3) %>%
  select(-grp) %>%
  gather(key,value,-PracticeID) %>%
  ggplot() +
  geom_col(aes(x=key,y=value), fill="blue") +
  facet_wrap(~PracticeID)
ggsave("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot4/grp4/plot_grp3.tiff", 
       p3,
       dpi=100, width = 80, height = 40, units = "cm")

p4 <- df2_clust %>%
  filter(grp==4) %>%
  select(-grp) %>%
  gather(key,value,-PracticeID) %>%
  ggplot() +
  geom_col(aes(x=key,y=value), fill="blue") +
  facet_wrap(~PracticeID)
ggsave("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot4/grp4/plot_grp4.tiff", 
       p4,
       dpi=100, width = 10, height = 10, units = "cm")

p5 <- df2_clust %>%
  filter(grp==5) %>%
  select(-grp) %>%
  gather(key,value,-PracticeID) %>%
  ggplot() +
  geom_col(aes(x=key,y=value), fill="blue") +
  facet_wrap(~PracticeID)
ggsave("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot4/grp4/plot_grp5.tiff", 
       p5,
       dpi=100, width = 10, height = 10, units = "cm")

##
## PART 3: K means clustering
## 

df2_kmean <- select(df2_clust,-PracticeID)
wss <- (nrow(mydata)-1)*sum(apply(df2_kmean,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(df2_kmean,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#set.seed(123)
km.res <- kmeans(select(df2_clust,-PracticeID), 4, nstart = 25)
print(km.res)

km.res$cluster

##
df2_kmean <- df2_clust
df2_kmean$grp <- km.res$cluster

##
saveRDS(df2_kmean, "2_interimdata/1_gpextract/df_cs1_kmeanclust.rds")


##
id_grp1 <- filter(df2_clust,grp==1)$PracticeID
id_grp2 <- filter(df2_clust,grp==2)$PracticeID
id_grp3 <- filter(df2_clust,grp==3)$PracticeID
id_grp4 <- filter(df2_clust,grp==4)$PracticeID
#id_grp5 <- filter(df2_clust,grp==5)$PracticeID

##
## PART 4: Plot K means clusters
##

p1 <- df2_clust2 %>%
  filter(grp==1) %>%
  #head(n=50) %>%
  select(-grp) %>%
  gather(key,value,-PracticeID) %>%
  ggplot() +
  geom_col(aes(x=key,y=value), fill="blue") +
  facet_wrap(~PracticeID)
ggsave("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot5/plot_grp1.tiff", 
       p1,
       dpi=100, width = 60, height = 30, units = "cm")

p2 <- df2_clust2 %>%
  filter(grp==2) %>%
  #head(n=50) %>%
  select(-grp) %>%
  gather(key,value,-PracticeID) %>%
  ggplot() +
  geom_col(aes(x=key,y=value), fill="blue") +
  facet_wrap(~PracticeID)
ggsave("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot5/plot_grp2.tiff", 
       p2,
       dpi=100, width = 80, height = 40, units = "cm")

p3 <- df2_clust %>%
  filter(grp==3) %>%
  #head(n=50) %>%
  select(-grp) %>%
  gather(key,value,-PracticeID) %>%
  ggplot() +
  geom_col(aes(x=key,y=value), fill="blue") +
  facet_wrap(~PracticeID)
ggsave("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot5/plot_grp3.tiff", 
       p3,
       dpi=100, width = 80, height = 40, units = "cm")

p4 <- df2_clust %>%
  filter(grp==4) %>%
  #head(n=50) %>%
  select(-grp) %>%
  gather(key,value,-PracticeID) %>%
  ggplot() +
  geom_col(aes(x=key,y=value), fill="blue") +
  facet_wrap(~PracticeID)
ggsave("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot5/plot_grp4.tiff", 
       p4,
       dpi=100, width = 60, height = 60, units = "cm")

# p5 <- df2_clust %>%
#   filter(grp==5) %>%
#   #head(n=50) %>%
#   select(-grp) %>%
#   gather(key,value,-PracticeID) %>%
#   ggplot() +
#   geom_col(aes(x=key,y=value), fill="blue") +
#   facet_wrap(~PracticeID)
# ggsave("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot5/plot_grp5.tiff", 
#        p5,
#        dpi=100, width = 10, height = 10, units = "cm")

##
## Plot example GPs
##

## Facet labels 1
facet_names <- c(
  'W94018'="Group 1",
  'W93059'="Group 2",
  'W92051'="Group 3",
  'W97041'="Group 4"
)

df2_clust %>%
  filter(PracticeID%in%c("W94018","W93059","W92051","W97041")) %>%
  select(-grp) %>%
  gather(key,value,-PracticeID) %>%
  ggplot() +
  geom_col(aes(x=key,y=value*100), fill="#99cccc") +
  ylab("% Change compared with previous years") + xlab("Month") +
  scale_y_continuous(limits = c(-60,60)) +
  facet_wrap(~PracticeID,labeller=as_labeller(facet_names)) +
  theme_classic()  +
  scale_x_discrete(labels = c("March","April","May","June")) +
  theme(axis.text.x = element_text(size=11, angle=90, hjust=1, vjust=0.5),
        axis.text.y = element_text(size=11),
        legend.text = element_text(size=11),
        legend.title = element_text(size=11),
        axis.title = element_text(size=11))
ggsave("C:/Users/Dan/Documents/SERCYMRU/4_outputs/plot5/groupings.tiff", 
       dpi=200, width = 20, height = 15, units = "cm")

##
## END
##