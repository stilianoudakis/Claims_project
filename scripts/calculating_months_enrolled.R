library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(sas7bdat)
library(lubridate)
library(DescTools)

eligibility <- readRDS("X:/CommonPrograms/Oral Health Services Research Core/Spiro/Claims_Project/data/ffs_mco_2015_2016_2017_2018_full.rds")

dim(eligibility) 
#103985    151

length(unique(eligibility$srecip)) 
#103985

eligibility <- eligibility[order(eligibility$srecip),]

medata <- eligibility[,grep(paste0(c("mco_beg1_2015",
                                                     "mco_beg2_2015",
                                                     "mco_beg3_2015",
                                                     "mco_beg4_2015",
                                                     "mco_end1_2015",
                                                     "mco_end2_2015",
                                                     "mco_end3_2015",
                                                     "mco_end4_2015",
                                                     
                                                     "mco_beg1_2016",
                                                     "mco_beg2_2016",
                                                     "mco_beg3_2016",
                                                     "mco_beg4_2016",
                                                     "mco_end1_2016",
                                                     "mco_end2_2016",
                                                     "mco_end3_2016",
                                                     "mco_end4_2016",
                                                     
                                                     "mco_beg1_2017",
                                                     "mco_beg2_2017",
                                                     "mco_beg3_2017",
                                                     "mco_beg4_2017",
                                                     "mco_end1_2017",
                                                     "mco_end2_2017",
                                                     "mco_end3_2017",
                                                     "mco_end4_2017",
                                                     
                                                     "mco_beg1_2018",
                                                     "mco_beg2_2018",
                                                     "mco_beg3_2018",
                                                     "mco_beg4_2018",
                                                     "mco_end1_2018",
                                                     "mco_end2_2018",
                                                     "mco_end3_2018",
                                                     "mco_end4_2018",
                                                     
                                                     "ffs_beg1_2015",
                                                     "ffs_beg2_2015",
                                                     "ffs_beg3_2015",
                                                     "ffs_beg4_2015",
                                                     "ffs_end1_2015",
                                                     "ffs_end2_2015",
                                                     "ffs_end3_2015",
                                                     "ffs_end4_2015",
                                                     
                                                     "ffs_beg1_2016",
                                                     "ffs_beg2_2016",
                                                     "ffs_beg3_2016",
                                                     "ffs_beg4_2016",
                                                     "ffs_end1_2016",
                                                     "ffs_end2_2016",
                                                     "ffs_end3_2016",
                                                     "ffs_end4_2016",
                                                     
                                                     "ffs_beg1_2017",
                                                     "ffs_beg2_2017",
                                                     "ffs_beg3_2017",
                                                     "ffs_beg4_2017",
                                                     "ffs_end1_2017",
                                                     "ffs_end2_2017",
                                                     "ffs_end3_2017",
                                                     "ffs_end4_2017",
                                                     
                                                     "ffs_beg1_2018",
                                                     "ffs_beg2_2018",
                                                     "ffs_beg3_2018",
                                                     "ffs_beg4_2018",
                                                     "ffs_end1_2018",
                                                     "ffs_end2_2018",
                                                     "ffs_end3_2018",
                                                     "ffs_end4_2018"
), collapse = "|"), names(eligibility))]

myfunc <- function(x){
  df <- data.frame(matrix(t(x),nrow = 16,ncol=2, byrow = T))
  df2 <- df[order(df$X1, decreasing = FALSE),]
  df3 <- df2[order(df2$X2, decreasing = TRUE),]
  df4 <- df3[!duplicated(df3$X1),]
  df5 <- df4[!duplicated(df4$X2),]
  df6 <- na.omit(df5)
  df7 <- df6 %>%
    arrange(X1) %>% 
    mutate(indx = c(0, cumsum(as.numeric(lead(X1)) >
                                  cummax(as.numeric(X2)))[-n()])) %>%
    summarise(start = first(X1), end = last(X2))
  me <- floor(as.numeric(sum(as.Date(df7$end)-as.Date(df7$start)))/30)
  #me <- floor(as.numeric(sum(as.Date(df6$X2)-as.Date(df6$X1)))/30)
  return(me)
}




x <- medata[8,];
df <- data.frame(matrix(t(x),nrow = 32,ncol=2, byrow = T));
df$p <- paste0(substr(names(x)[seq(1,64,2)],1,3), "_", rep(1:4,8), "_", c(rep("2015",8),rep("2016",8),rep("2017",8),rep("2018",8)));
df

myfunc <- function(data){
  data <- data[order(data[,1], decreasing = FALSE),]
  data <- data[order(data[,2], decreasing = TRUE),]
  data <- data[!duplicated(data[,1]),]
  data <- data[!duplicated(data[,2]),]
  data <- na.omit(data)
  return(data)
}

myfunc(df[grep("mco",df$p),])
myfunc(df[grep("ffs",df$p),])

###########################################

reduceDates <- function(mydata){
  mydata <- mydata[order(mydata[,2], decreasing = TRUE),]
  mydata <- mydata[order(mydata[,1], decreasing = FALSE),]
  mydata <- mydata[!duplicated(mydata[,1]),]
  mydata <- mydata[!duplicated(mydata[,2]),]
  mydata <- na.omit(mydata)
  mydata$lagDate <- c(as.character(mydata[-1,1]),NA)
  mydata$diffDays <- as.vector(as.Date(mydata$lagDate)-as.Date(mydata[,2]))
  return(mydata)
}

#there are no instances where there are overlaps between dates within the same plan 

#monthsEnrolled <- function(x){
monthsEnrolled <- numeric()
for(i in 1:nrow(medata)){
  x<-medata[i,]
  df <- data.frame(matrix(t(x),nrow = 32,ncol=2, byrow = T))
  df$p <- paste0(substr(names(x)[seq(1,64,2)],1,3), "_", rep(1:4,8), "_", c(rep("2015",8),rep("2016",8),rep("2017",8),rep("2018",8)))
  
  mco_dat <- df[grep("mco",df$p),]
  ffs_dat <- df[grep("ffs",df$p),]
  
  #case 1: no mco dates; only ffs
  if(sum(is.na(mco_dat$X1))==16){
    plan <- "ffs"
    ffs_dat <- reduceDates(ffs_dat)
    me <- floor(as.numeric(sum(as.Date(as.character(ffs_dat$X2))-as.Date(as.character(ffs_dat$X1)))+1)/30)
  }
  
  #case 2: no ffs dates; only mco
  if(sum(is.na(ffs_dat$X1))==16){
    plan <- "mco"
    mco_dat <- reduceDates(mco_dat)
    me <- floor(as.numeric(sum(as.Date(as.character(mco_dat$X2))-as.Date(as.character(mco_dat$X1)))+1)/30)
  }
  
  #case 3: both mco and ffs dates 
  if( (sum(is.na(mco_dat$X1))!=16) & (sum(is.na(ffs_dat$X1))!=16) ){
    plan <- "both"
    mco_dat <- reduceDates(mco_dat)
    ffs_dat <- reduceDates(ffs_dat)
    
    #iterate through the matched plan stratum to find overlap between mco and ffs to get months enrolled
    uniqueIntervals <- unique(c(gsub("mco_","",mco_dat$p),gsub("ffs_","",ffs_dat$p)))
    me_uniqueInts <- numeric()
    for(j in 1:length(uniqueIntervals)){
      int_mco <- c(as.Date(as.character(mco_dat$X1[grep(uniqueIntervals[j], gsub("mco_","",mco_dat$p))])),
                   as.Date(as.character(mco_dat$X2[grep(uniqueIntervals[j], gsub("mco_","",mco_dat$p))])))
      int_ffs <- c(as.Date(as.character(ffs_dat$X1[grep(uniqueIntervals[j], gsub("ffs_","",ffs_dat$p))])),
                   as.Date(as.character(ffs_dat$X2[grep(uniqueIntervals[j], gsub("ffs_","",ffs_dat$p))])))
      
      #case 1:no mco dates among those plan dates
      if(length(int_mco)==0){
        me_uniqueInts <- c(me_uniqueInts, as.numeric(floor(diff(int_ffs)/30 +1 )))
      }
      #case 2:no ffs dates among those plan dates
      if(length(int_ffs)==0){
        me_uniqueInts <- c(me_uniqueInts, as.numeric(floor(diff(int_mco)/30 +1 )))
      }
      #case 3:both mco and ffs dates among the plan stratum
      if(length(int_mco)!=0 & length(int_ffs)!=0){
        me_uniqueInts <- c(me_uniqueInts, as.numeric(floor((diff(int_ffs) + diff(int_mco) - Overlap(int_mco,int_ffs))/30 +1 )))
      }
    }
    me <- sum(me_uniqueInts)
  }
  monthsEnrolled[i]<-me
} 
#}




