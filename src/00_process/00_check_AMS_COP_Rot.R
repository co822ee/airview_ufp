# Notes of the findings:
# https://www.notion.so/Check-the-UFP-data-AMS-COP-and-Rotterdam-8e9d62326acd46d68e13c14e3196f9d5
# slidee 6 "20240411_expanseUFP.pptx"
library(dplyr)
library(data.table)
# Read all the csv files from "data/raw/nl, dk"
target_dir <- "data/raw/nl, dk/"
cities <- c("amsterdam", "copenhagen")  #list.dirs(target_dir)
file_list <- list.files(paste0(target_dir, cities), pattern = ".csv", full.names = TRUE)
data_list <- lapply(file_list, fread)



raw_df <- data_list[[1]]
dim(raw_df)
head(raw_df)
summary(raw_df)
ufp <- raw_df[!is.na(raw_df$UFP),]
summary(ufp)


################ Rotterdam ####################
rotterdam <- list.files(paste0(target_dir, "rotterdam"), pattern = ".csv", full.names = TRUE) %>% 
                    fread()
summary(rotterdam)


stat_dfs <- rotterdam %>% dplyr::filter(Date>as.Date("2021-09-12"))    ##rotterdam %>% dplyr::filter(Date>as.Date("2021-09-12"))
stat_dfs2 <- stat_dfs[order(stat_dfs$DateTime),]
# determine the campaign
stat_dfs2$DateTime_diff <-c(0,  as.numeric(diff(stat_dfs2$DateTime, lag=1)))
# last day of the first campaign
stat_dfs2[1,]$DateTime
stat_dfs2[which.max(stat_dfs2$DateTime_diff)-1,]$DateTime
# first day of the second campaign
stat_dfs2[which.max(stat_dfs2$DateTime_diff),]$DateTime
stat_dfs2[nrow(stat_dfs2),]$DateTime


time_log <- data.frame( 
                        start1=stat_dfs2[1,]$DateTime, 
                        end1=stat_dfs2[which.max(stat_dfs2$DateTime_diff)-1,]$DateTime,
                        start2=stat_dfs2[which.max(stat_dfs2$DateTime_diff),]$DateTime,
                        end2=stat_dfs2[nrow(stat_dfs2),]$DateTime)
time_log
#### Check for Amster
# stat_dfs2 %>% dplyr::filter(Date>as.Date("2020-03-15"))  %>% summary
#### Check for Rotterdam
# stat_dfs2 %>% dplyr::filter(Date>as.Date("2021-09-12"))  %>% summary
stat_dfs2 %>% dplyr::filter(Date>as.Date("2022-08-22"), Date<as.Date("2022-12-22"))  %>% summary
unique((stat_dfs2 %>% dplyr::filter(Date>as.Date("2022-08-22"), Date<as.Date("2022-12-22")))$Date)

