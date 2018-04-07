  library(ggplot2)
  library(data.table)
  library(dplyr)
  library(reshape2)
  
  # clear variables
  rm(list = ls())  
  
  df_2012 <- read.csv("2012_mrt_hourly_daily_ridership 2.csv", header = TRUE, stringsAsFactors = FALSE)
  
  #start cleaning
  
  df_2012 <- df_2012[!(names(df_2012) %in% c("X", "X.1"))] #remove extra columns
  
  df_2012[df_2012 == "-"]<-NA
  df_2012[is.na(df_2012)]<- 0
  
  #end of cleaning
  
  #extract *_entry columns from the original data frame
  entries <- df_2012[!(colnames(df_2012) %like% "_exit")]
  exits <- df_2012[!(colnames(df_2012) %like% "_entry")]
  
  #get all station column names
  stations <- colnames(entries)[c(4:16)]
  
  #convert station columns to numeric
  for (station in stations) {
    entries[,station] <- as.numeric(entries[,station])
  }
  
  #get list of for our factor(months)
  level_months <- unique(entries$month)
  
  entries$month <- factor(entries$month, levels = level_months) # retain order upon aggregation
  
  consol <- aggregate(x = entries[stations], by=list(Month=entries$month), FUN = sum)
  
  consol_m <- melt(consol, id.vars = "Month", 
                measure.vars = grep("entry", names(consol), value = TRUE))
  
  ggplot(consol_m, aes(y = value, x = Month, colour = as.factor(variable))) +
    geom_point() +
    geom_line(aes(group = variable)) +
    geom_text(aes(label=value),hjust=0, vjust=0) +
    scale_y_continuous(labels = scales::comma, name = "Number of Passengers")