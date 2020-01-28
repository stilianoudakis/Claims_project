library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(sas7bdat)
library(lubridate)
library(DescTools)

ffs_mco_2015_2016_2017_2018 <- readRDS("X:/CommonPrograms/Oral Health Services Research Core/Spiro/Claims_Project/data/ffs_mco_2015_2016_2017_2018_full.rds")


# 2015

medata_2015 <- ffs_mco_2015_2016_2017_2018[,grep(paste0(c("srecip",
                                                          "mco_beg1_2015",
                                                          "mco_beg2_2015",
                                                          "mco_beg3_2015",
                                                          "mco_beg4_2015",
                                                          "mco_end1_2015",
                                                          "mco_end2_2015",
                                                          "mco_end3_2015",
                                                          "mco_end4_2015",
                                                          
                                                          "ffs_beg1_2015",
                                                          "ffs_beg2_2015",
                                                          "ffs_beg3_2015",
                                                          #"ffs_beg4_2015",
                                                          "ffs_end1_2015",
                                                          "ffs_end2_2015",
                                                          "ffs_end3_2015"#,
                                                          #"ffs_end4_2015"
), collapse = "|"), names(ffs_mco_2015_2016_2017_2018))]

medata_2015_list <- list(medata_2015[,1:9],medata_2015[,c(1,10:15)])

medata_2015_list[[2]]$ffs_beg4_2015 <- medata_2015_list[[2]]$ffs_end4_2015 <- as.Date(NA)

names(medata_2015_list[[1]])[-1] <- gsub("mco_","",names(medata_2015_list[[1]])[-1])
names(medata_2015_list[[2]])[-1] <- gsub("ffs_","",names(medata_2015_list[[2]])[-1])

medata_2015_2 <- rbind.data.frame(medata_2015_list[[1]],medata_2015_list[[2]])

medata_2015_3 <- medata_2015_2 %>%
  group_by(srecip) %>%
  dplyr::arrange(srecip,beg1_2015)
medata_2015_3 <- as.data.frame(medata_2015_3)

#apply(medata_2015_3,2,function(x)table(is.na(x)))

ids <- sort(medata_2015_3$srecip[seq(1,length(medata_2015_3$srecip),2)])
me <- numeric()
for(i in 1:length(ids)){
  
  srecipData <- medata_2015_3[which(medata_2015_3$srecip==ids[i]),]
  srecipData_dates <- as.matrix(srecipData[,-1])
  nummiss <- as.vector(rowSums(is.na(srecipData_dates)))
  
  if(nummiss[1]==8 & nummiss[2]==8){
    me[i] <- 0
  }else if(nummiss[1]!=8 & nummiss[2]==8){
    m = matrix(srecipData_dates[1,],nrow = 4, ncol = 2,byrow = T)
    me[i] <- as.numeric(sum(as.Date(m[,2])-as.Date(m[,1]), na.rm = T))/30.41667
  }else if(nummiss[1]==8 & nummiss[2]!=8){
    m = matrix(srecipData_dates[2,],nrow = 4, ncol = 2,byrow = T)
    me[i] <- as.numeric(sum(as.Date(m[,2])-as.Date(m[,1]), na.rm = T))/30.41667
  }else{
    n = max(as.vector(rowSums(!is.na(srecipData_dates))))
    p <- 0
    q <- 0
    #r <- 0
    common_int_list <- list()
    uncommon_int_list1 <- list()
    uncommon_int_list2 <- list()
    for(k in 1:(n/2)){
      p <- p+1
      q <- q+1
      #r <- r+1
      
      int1 <- c(srecipData_dates[1,2*k-1],srecipData_dates[1,2*k])
      int2 <- c(srecipData_dates[2,2*k-1],srecipData_dates[2,2*k])
      
      if(int1 %overlaps% int2 & !is.na(int1 %overlaps% int2)){
        common_int_list[[p]] <- c(min(int1[1],int2[1]), max(int1[2],int2[2]))
      }else{
        uncommon_int_list1[[q]] <- int1
        uncommon_int_list2[[q]] <- int2
      }
    }
    
    m <- rbind(do.call(rbind,common_int_list),
               do.call(rbind,uncommon_int_list1),
               do.call(rbind,uncommon_int_list2))
    if(sum(is.na(m))>0){m <- m[-which(is.na(m)),]}
    if(nrow(m)>1){m <- m[order(m[,1], decreasing=FALSE),]}
    
    if(TRUE %in% ((as.numeric(as.Date(m[,2])-as.Date(m[,1]))/30.41667) >= 11.9)){
      me[i] <- 12
    }else if(nrow(m)==1){
      me[i] <- (as.numeric(as.Date(m[,2])-as.Date(m[,1]))/30.41667)
    }else if(nrow(m)==2){
      if(c(m[1,1],m[1,2]) %overlaps% c(m[2,1],m[2,2])){
        me[i] <- (as.numeric(as.Date(max(m[1,2],m[2,2]))-as.Date(min(m[1,1],m[2,1])))/30.41667)
      }else{me[i] <- sum(as.numeric(as.Date(m[,2])-as.Date(m[,1]))/30.41667)}
    }else{
      h=1
      int_list_overlaps <- list()
      int_list_nooverlaps <- list()
      while(h < nrow(m)){
        if(h==(nrow(m)-1)){
          a <- m[-(1:h),]
          t <- sum(c(m[h,1],m[h,2]) %overlaps% a)
        }else{
          a <- lapply(1:nrow(m[-(1:h),]), function(i) m[-(1:h),][i,])
          #t <- which.max(TRUE %in% unlist(lapply(a,function(x){c(m[h,1],m[h,2]) %overlaps% x})))
          #t <- as.numeric(table(unlist(lapply(a,function(x){c(m[h,1],m[h,2]) %overlaps% x})))[2])
          t <- sum(unlist(lapply(a,function(x){c(m[h,1],m[h,2]) %overlaps% x})))
        }
        if(is.na(t)){
          int_list_nooverlaps[[h]] <- c(m[h,1],m[h,2])
          h <- h+1
        }else{
          int_list_overlaps[[h]] <- c(min(m[h,1],m[h+t,1]), max(m[h,2],m[h+t,2]))
          h <- h+t+1
        }
      }
      g1 <- do.call(rbind,Filter(Negate(is.null), int_list_overlaps))
      g2 <- do.call(rbind,Filter(Negate(is.null), int_list_nooverlaps))
      if(is.null(g2)){
        me[i] <- sum(as.numeric(as.Date(g1[,2])-as.Date(g1[,1]))/30.41667)
      }else if(is.null(g1)){
        me[i] <- sum(as.numeric(as.Date(g2[,2])-as.Date(g2[,1]))/30.41667)
      }else{
        me[i] <- sum(c(as.numeric(as.Date(g1[,2])-as.Date(g1[,1]))/30.41667,
                       as.numeric(as.Date(g2[,2])-as.Date(g2[,1]))/30.41667))
      }
    }
  }
  
  
}
me_2015 <- me
summary(me_2015)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   2.590   4.932  12.000 

# 2016
medata_2016 <- ffs_mco_2015_2016_2017_2018[,grep(paste0(c("srecip",
                                                          "mco_beg1_2016",
                                                          "mco_beg2_2016",
                                                          "mco_beg3_2016",
                                                          "mco_beg4_2016",
                                                          "mco_end1_2016",
                                                          "mco_end2_2016",
                                                          "mco_end3_2016",
                                                          "mco_end4_2016",
                                                          
                                                          "ffs_beg1_2016",
                                                          "ffs_beg2_2016",
                                                          "ffs_beg3_2016",
                                                          #"ffs_beg4_2016",
                                                          "ffs_end1_2016",
                                                          "ffs_end2_2016",
                                                          "ffs_end3_2016"#,
                                                          #"ffs_end4_2016"
), collapse = "|"), names(ffs_mco_2015_2016_2017_2018))]

medata_2016_list <- list(medata_2016[,1:9],medata_2016[,c(1,10:15)])

medata_2016_list[[2]]$ffs_beg4_2016 <- medata_2016_list[[2]]$ffs_end4_2016 <- as.Date(NA)

names(medata_2016_list[[1]])[-1] <- gsub("mco_","",names(medata_2016_list[[1]])[-1])
names(medata_2016_list[[2]])[-1] <- gsub("ffs_","",names(medata_2016_list[[2]])[-1])

medata_2016_2 <- rbind.data.frame(medata_2016_list[[1]],medata_2016_list[[2]])

medata_2016_3 <- medata_2016_2 %>%
  group_by(srecip) %>%
  dplyr::arrange(srecip,beg1_2016)
medata_2016_3 <- as.data.frame(medata_2016_3)

#apply(medata_2016_3,2,function(x)table(is.na(x)))

ids <- sort(medata_2016_3$srecip[seq(1,length(medata_2016_3$srecip),2)])
me <- numeric()
for(i in 1:length(ids)){
  
  srecipData <- medata_2016_3[which(medata_2016_3$srecip==ids[i]),]
  srecipData_dates <- as.matrix(srecipData[,-1])
  nummiss <- as.vector(rowSums(is.na(srecipData_dates)))
  
  if(nummiss[1]==8 & nummiss[2]==8){
    me[i] <- 0
  }else if(nummiss[1]!=8 & nummiss[2]==8){
    m = matrix(srecipData_dates[1,],nrow = 4, ncol = 2,byrow = T)
    me[i] <- as.numeric(sum(as.Date(m[,2])-as.Date(m[,1]), na.rm = T))/30.41667
  }else if(nummiss[1]==8 & nummiss[2]!=8){
    m = matrix(srecipData_dates[2,],nrow = 4, ncol = 2,byrow = T)
    me[i] <- as.numeric(sum(as.Date(m[,2])-as.Date(m[,1]), na.rm = T))/30.41667
  }else{
    n = max(as.vector(rowSums(!is.na(srecipData_dates))))
    p <- 0
    q <- 0
    #r <- 0
    common_int_list <- list()
    uncommon_int_list1 <- list()
    uncommon_int_list2 <- list()
    for(k in 1:(n/2)){
      p <- p+1
      q <- q+1
      #r <- r+1
      
      int1 <- c(srecipData_dates[1,2*k-1],srecipData_dates[1,2*k])
      int2 <- c(srecipData_dates[2,2*k-1],srecipData_dates[2,2*k])
      
      if(int1 %overlaps% int2 & !is.na(int1 %overlaps% int2)){
        common_int_list[[p]] <- c(min(int1[1],int2[1]), max(int1[2],int2[2]))
      }else{
        uncommon_int_list1[[q]] <- int1
        uncommon_int_list2[[q]] <- int2
      }
    }
    
    m <- rbind(do.call(rbind,common_int_list),
               do.call(rbind,uncommon_int_list1),
               do.call(rbind,uncommon_int_list2))
    if(sum(is.na(m))>0){m <- m[-which(is.na(m)),]}
    if(nrow(m)>1){m <- m[order(m[,1], decreasing=FALSE),]}
    
    if(TRUE %in% ((as.numeric(as.Date(m[,2])-as.Date(m[,1]))/30.41667) >= 11.9)){
      me[i] <- 12
    }else if(nrow(m)==1){
      me[i] <- (as.numeric(as.Date(m[,2])-as.Date(m[,1]))/30.41667)
    }else if(nrow(m)==2){
      if(c(m[1,1],m[1,2]) %overlaps% c(m[2,1],m[2,2])){
        me[i] <- (as.numeric(as.Date(max(m[1,2],m[2,2]))-as.Date(min(m[1,1],m[2,1])))/30.41667)
      }else{me[i] <- sum(as.numeric(as.Date(m[,2])-as.Date(m[,1]))/30.41667)}
    }else{
      h=1
      int_list_overlaps <- list()
      int_list_nooverlaps <- list()
      while(h < nrow(m)){
        if(h==(nrow(m)-1)){
          a <- m[-(1:h),]
          t <- sum(c(m[h,1],m[h,2]) %overlaps% a)
        }else{
          a <- lapply(1:nrow(m[-(1:h),]), function(i) m[-(1:h),][i,])
          #t <- which.max(TRUE %in% unlist(lapply(a,function(x){c(m[h,1],m[h,2]) %overlaps% x})))
          #t <- as.numeric(table(unlist(lapply(a,function(x){c(m[h,1],m[h,2]) %overlaps% x})))[2])
          t <- sum(unlist(lapply(a,function(x){c(m[h,1],m[h,2]) %overlaps% x})))
        }
        if(is.na(t)){
          int_list_nooverlaps[[h]] <- c(m[h,1],m[h,2])
          h <- h+1
        }else{
          int_list_overlaps[[h]] <- c(min(m[h,1],m[h+t,1]), max(m[h,2],m[h+t,2]))
          h <- h+t+1
        }
      }
      g1 <- do.call(rbind,Filter(Negate(is.null), int_list_overlaps))
      g2 <- do.call(rbind,Filter(Negate(is.null), int_list_nooverlaps))
      if(is.null(g2)){
        me[i] <- sum(as.numeric(as.Date(g1[,2])-as.Date(g1[,1]))/30.41667)
      }else if(is.null(g1)){
        me[i] <- sum(as.numeric(as.Date(g2[,2])-as.Date(g2[,1]))/30.41667)
      }else{
        me[i] <- sum(c(as.numeric(as.Date(g1[,2])-as.Date(g1[,1]))/30.41667,
                       as.numeric(as.Date(g2[,2])-as.Date(g2[,1]))/30.41667))
      }
    }
  }
  
  
}
me_2016 <- me
summary(me_2016)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   3.114   6.970  12.000 

# 2017
medata_2017 <- ffs_mco_2015_2016_2017_2018[,grep(paste0(c("srecip",
                                                          "mco_beg1_2017",
                                                          "mco_beg2_2017",
                                                          "mco_beg3_2017",
                                                          "mco_beg4_2017",
                                                          "mco_end1_2017",
                                                          "mco_end2_2017",
                                                          "mco_end3_2017",
                                                          "mco_end4_2017",
                                                          
                                                          "ffs_beg1_2017",
                                                          "ffs_beg2_2017",
                                                          "ffs_beg3_2017",
                                                          #"ffs_beg4_2017",
                                                          "ffs_end1_2017",
                                                          "ffs_end2_2017",
                                                          "ffs_end3_2017"#,
                                                          #"ffs_end4_2017"
), collapse = "|"), names(ffs_mco_2015_2016_2017_2018))]

medata_2017_list <- list(medata_2017[,1:9],medata_2017[,c(1,10:15)])

medata_2017_list[[2]]$ffs_beg4_2017 <- medata_2017_list[[2]]$ffs_end4_2017 <- as.Date(NA)

names(medata_2017_list[[1]])[-1] <- gsub("mco_","",names(medata_2017_list[[1]])[-1])
names(medata_2017_list[[2]])[-1] <- gsub("ffs_","",names(medata_2017_list[[2]])[-1])

medata_2017_2 <- rbind.data.frame(medata_2017_list[[1]],medata_2017_list[[2]])

medata_2017_3 <- medata_2017_2 %>%
  group_by(srecip) %>%
  dplyr::arrange(srecip,beg1_2017)
medata_2017_3 <- as.data.frame(medata_2017_3)

#apply(medata_2017_3,2,function(x)table(is.na(x)))

ids <- sort(medata_2017_3$srecip[seq(1,length(medata_2017_3$srecip),2)])
me <- numeric()
for(i in 1:length(ids)){
  
  srecipData <- medata_2017_3[which(medata_2017_3$srecip==ids[i]),]
  srecipData_dates <- as.matrix(srecipData[,-1])
  nummiss <- as.vector(rowSums(is.na(srecipData_dates)))
  
  if(nummiss[1]==8 & nummiss[2]==8){
    me[i] <- 0
  }else if(nummiss[1]!=8 & nummiss[2]==8){
    m = matrix(srecipData_dates[1,],nrow = 4, ncol = 2,byrow = T)
    me[i] <- as.numeric(sum(as.Date(m[,2])-as.Date(m[,1]), na.rm = T))/30.41667
  }else if(nummiss[1]==8 & nummiss[2]!=8){
    m = matrix(srecipData_dates[2,],nrow = 4, ncol = 2,byrow = T)
    me[i] <- as.numeric(sum(as.Date(m[,2])-as.Date(m[,1]), na.rm = T))/30.41667
  }else{
    n = max(as.vector(rowSums(!is.na(srecipData_dates))))
    p <- 0
    q <- 0
    #r <- 0
    common_int_list <- list()
    uncommon_int_list1 <- list()
    uncommon_int_list2 <- list()
    for(k in 1:(n/2)){
      p <- p+1
      q <- q+1
      #r <- r+1
      
      int1 <- c(srecipData_dates[1,2*k-1],srecipData_dates[1,2*k])
      int2 <- c(srecipData_dates[2,2*k-1],srecipData_dates[2,2*k])
      
      if(int1 %overlaps% int2 & !is.na(int1 %overlaps% int2)){
        common_int_list[[p]] <- c(min(int1[1],int2[1]), max(int1[2],int2[2]))
      }else{
        uncommon_int_list1[[q]] <- int1
        uncommon_int_list2[[q]] <- int2
      }
    }
    
    m <- rbind(do.call(rbind,common_int_list),
               do.call(rbind,uncommon_int_list1),
               do.call(rbind,uncommon_int_list2))
    if(sum(is.na(m))>0){m <- m[-which(is.na(m)),]}
    if(nrow(m)>1){m <- m[order(m[,1], decreasing=FALSE),]}
    
    if(TRUE %in% ((as.numeric(as.Date(m[,2])-as.Date(m[,1]))/30.41667) >= 11.9)){
      me[i] <- 12
    }else if(nrow(m)==1){
      me[i] <- (as.numeric(as.Date(m[,2])-as.Date(m[,1]))/30.41667)
    }else if(nrow(m)==2){
      if(c(m[1,1],m[1,2]) %overlaps% c(m[2,1],m[2,2])){
        me[i] <- (as.numeric(as.Date(max(m[1,2],m[2,2]))-as.Date(min(m[1,1],m[2,1])))/30.41667)
      }else{me[i] <- sum(as.numeric(as.Date(m[,2])-as.Date(m[,1]))/30.41667)}
    }else{
      h=1
      int_list_overlaps <- list()
      int_list_nooverlaps <- list()
      while(h < nrow(m)){
        if(h==(nrow(m)-1)){
          a <- m[-(1:h),]
          t <- sum(c(m[h,1],m[h,2]) %overlaps% a)
        }else{
          a <- lapply(1:nrow(m[-(1:h),]), function(i) m[-(1:h),][i,])
          #t <- which.max(TRUE %in% unlist(lapply(a,function(x){c(m[h,1],m[h,2]) %overlaps% x})))
          #t <- as.numeric(table(unlist(lapply(a,function(x){c(m[h,1],m[h,2]) %overlaps% x})))[2])
          t <- sum(unlist(lapply(a,function(x){c(m[h,1],m[h,2]) %overlaps% x})))
        }
        if(is.na(t)){
          int_list_nooverlaps[[h]] <- c(m[h,1],m[h,2])
          h <- h+1
        }else{
          int_list_overlaps[[h]] <- c(min(m[h,1],m[h+t,1]), max(m[h,2],m[h+t,2]))
          h <- h+t+1
        }
      }
      g1 <- do.call(rbind,Filter(Negate(is.null), int_list_overlaps))
      g2 <- do.call(rbind,Filter(Negate(is.null), int_list_nooverlaps))
      if(is.null(g2)){
        me[i] <- sum(as.numeric(as.Date(g1[,2])-as.Date(g1[,1]))/30.41667)
      }else if(is.null(g1)){
        me[i] <- sum(as.numeric(as.Date(g2[,2])-as.Date(g2[,1]))/30.41667)
      }else{
        me[i] <- sum(c(as.numeric(as.Date(g1[,2])-as.Date(g1[,1]))/30.41667,
                       as.numeric(as.Date(g2[,2])-as.Date(g2[,1]))/30.41667))
      }
    }
  }
  
  
}
me_2017 <- me
summary(me_2017)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   3.232   6.937  12.000 

# 2018
medata_2018 <- ffs_mco_2015_2016_2017_2018[,grep(paste0(c("srecip",
                                                          "mco_beg1_2018",
                                                          "mco_beg2_2018",
                                                          "mco_beg3_2018",
                                                          "mco_beg4_2018",
                                                          "mco_end1_2018",
                                                          "mco_end2_2018",
                                                          "mco_end3_2018",
                                                          "mco_end4_2018",
                                                          
                                                          "ffs_beg1_2018",
                                                          "ffs_beg2_2018",
                                                          "ffs_beg3_2018",
                                                          #"ffs_beg4_2018",
                                                          "ffs_end1_2018",
                                                          "ffs_end2_2018",
                                                          "ffs_end3_2018"#,
                                                          #"ffs_end4_2018"
), collapse = "|"), names(ffs_mco_2015_2016_2017_2018))]

medata_2018_list <- list(medata_2018[,1:9],medata_2018[,c(1,10:15)])

medata_2018_list[[2]]$ffs_beg4_2018 <- medata_2018_list[[2]]$ffs_end4_2018 <- as.Date(NA)

names(medata_2018_list[[1]])[-1] <- gsub("mco_","",names(medata_2018_list[[1]])[-1])
names(medata_2018_list[[2]])[-1] <- gsub("ffs_","",names(medata_2018_list[[2]])[-1])

medata_2018_2 <- rbind.data.frame(medata_2018_list[[1]],medata_2018_list[[2]])

medata_2018_3 <- medata_2018_2 %>%
  group_by(srecip) %>%
  dplyr::arrange(srecip,beg1_2018)
medata_2018_3 <- as.data.frame(medata_2018_3)

#apply(medata_2018_3,2,function(x)table(is.na(x)))

ids <- sort(medata_2018_3$srecip[seq(1,length(medata_2018_3$srecip),2)])
me <- numeric()
for(i in 1:length(ids)){
  
  srecipData <- medata_2018_3[which(medata_2018_3$srecip==ids[i]),]
  srecipData_dates <- as.matrix(srecipData[,-1])
  nummiss <- as.vector(rowSums(is.na(srecipData_dates)))
  
  if(nummiss[1]==8 & nummiss[2]==8){
    me[i] <- 0
  }else if(nummiss[1]!=8 & nummiss[2]==8){
    m = matrix(srecipData_dates[1,],nrow = 4, ncol = 2,byrow = T)
    me[i] <- as.numeric(sum(as.Date(m[,2])-as.Date(m[,1]), na.rm = T))/30.41667
  }else if(nummiss[1]==8 & nummiss[2]!=8){
    m = matrix(srecipData_dates[2,],nrow = 4, ncol = 2,byrow = T)
    me[i] <- as.numeric(sum(as.Date(m[,2])-as.Date(m[,1]), na.rm = T))/30.41667
  }else{
    n = max(as.vector(rowSums(!is.na(srecipData_dates))))
    p <- 0
    q <- 0
    #r <- 0
    common_int_list <- list()
    uncommon_int_list1 <- list()
    uncommon_int_list2 <- list()
    for(k in 1:(n/2)){
      p <- p+1
      q <- q+1
      #r <- r+1
      
      int1 <- c(srecipData_dates[1,2*k-1],srecipData_dates[1,2*k])
      int2 <- c(srecipData_dates[2,2*k-1],srecipData_dates[2,2*k])
      
      if(int1 %overlaps% int2 & !is.na(int1 %overlaps% int2)){
        common_int_list[[p]] <- c(min(int1[1],int2[1]), max(int1[2],int2[2]))
      }else{
        uncommon_int_list1[[q]] <- int1
        uncommon_int_list2[[q]] <- int2
      }
    }
    
    m <- rbind(do.call(rbind,common_int_list),
               do.call(rbind,uncommon_int_list1),
               do.call(rbind,uncommon_int_list2))
    if(sum(is.na(m))>0){m <- m[-which(is.na(m)),]}
    if(nrow(m)>1){m <- m[order(m[,1], decreasing=FALSE),]}
    
    if(TRUE %in% ((as.numeric(as.Date(m[,2])-as.Date(m[,1]))/30.41667) >= 11.9)){
      me[i] <- 12
    }else if(nrow(m)==1){
      me[i] <- (as.numeric(as.Date(m[,2])-as.Date(m[,1]))/30.41667)
    }else if(nrow(m)==2){
      if(c(m[1,1],m[1,2]) %overlaps% c(m[2,1],m[2,2])){
        me[i] <- (as.numeric(as.Date(max(m[1,2],m[2,2]))-as.Date(min(m[1,1],m[2,1])))/30.41667)
      }else{me[i] <- sum(as.numeric(as.Date(m[,2])-as.Date(m[,1]))/30.41667)}
    }else{
      h=1
      int_list_overlaps <- list()
      int_list_nooverlaps <- list()
      while(h < nrow(m)){
        if(h==(nrow(m)-1)){
          a <- m[-(1:h),]
          t <- sum(c(m[h,1],m[h,2]) %overlaps% a)
        }else{
          a <- lapply(1:nrow(m[-(1:h),]), function(i) m[-(1:h),][i,])
          #t <- which.max(TRUE %in% unlist(lapply(a,function(x){c(m[h,1],m[h,2]) %overlaps% x})))
          #t <- as.numeric(table(unlist(lapply(a,function(x){c(m[h,1],m[h,2]) %overlaps% x})))[2])
          t <- sum(unlist(lapply(a,function(x){c(m[h,1],m[h,2]) %overlaps% x})))
        }
        if(is.na(t)){
          int_list_nooverlaps[[h]] <- c(m[h,1],m[h,2])
          h <- h+1
        }else{
          int_list_overlaps[[h]] <- c(min(m[h,1],m[h+t,1]), max(m[h,2],m[h+t,2]))
          h <- h+t+1
        }
      }
      g1 <- do.call(rbind,Filter(Negate(is.null), int_list_overlaps))
      g2 <- do.call(rbind,Filter(Negate(is.null), int_list_nooverlaps))
      if(is.null(g2)){
        me[i] <- sum(as.numeric(as.Date(g1[,2])-as.Date(g1[,1]))/30.41667)
      }else if(is.null(g1)){
        me[i] <- sum(as.numeric(as.Date(g2[,2])-as.Date(g2[,1]))/30.41667)
      }else{
        me[i] <- sum(c(as.numeric(as.Date(g1[,2])-as.Date(g1[,1]))/30.41667,
                       as.numeric(as.Date(g2[,2])-as.Date(g2[,1]))/30.41667))
      }
    }
  }
  
  
}
me_2018 <- me
summary(me_2018)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   3.462   7.923  12.000 

#overall
me_overall <- rowSums(cbind(me_2015,me_2016,me_2017,me_2018))
summary(me_overall)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.893   7.956  11.967  12.397  15.912  48.000 

