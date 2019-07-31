
#
rm(list = ls())
#

library(tuber)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(gridExtra)
#
# this will load the data as if the following had all been run
#
yt_oauth(app_id = "redacted for privacy",
         app_secret = "redacted for privacy")
#
#
# Download and Prepare Data
# ID for Patti LaBelle's official channel on Youtube
# https://www.youtube.com/channel/UCW2B68KvVGi2At66bTc2XxQ

channel_id1 <- "UCW2B68KvVGi2At66bTc2XxQ"
chstats1 <-
  get_channel_stats(channel_id1)
views1 <- chstats1[["statistics"]][["viewCount"]]
#
# ID for Whitney Houston's official channel on Youtube
# https://www.youtube.com/channel/UC7fzrpTArAqDHuB3Hbmd_CQ
#
channel_id2 <- "UC7fzrpTArAqDHuB3Hbmd_CQ"
chstats2 <-
  get_channel_stats(channel_id2)
views2 <- chstats2[["statistics"]][["viewCount"]]
#
#
# ID for Mariah Carey's official channel on Youtube
# https://www.youtube.com/channel/UCurpiDXSkcUbgdMwHNZkrCg
#
#
channel_id3 <- "UCurpiDXSkcUbgdMwHNZkrCg"
chstats3 <-
  get_channel_stats(channel_id3)
views3 <- chstats3[["statistics"]][["viewCount"]]
#
# get the data for videos newer than 2009-01-01
#
videos1 <-
  yt_search(term = "", type = "video", channel_id = channel_id1)
videosPat <-
  videos1 %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2009-01-01") %>%
  arrange(date) %>%
  select(-publishedAt)
#  
videos2 <-
  yt_search(term = "", type = "video", channel_id = channel_id2)
videosWhit <-
  videos2 %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2009-01-01") %>%
  arrange(date) %>%
  select(-publishedAt)
#
videos3 <-
  yt_search(term = "", type = "video", channel_id = channel_id3)
videosMar <-
  videos3 %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2009-01-01") %>%
  arrange(date) %>%
  select(-publishedAt)

# get comments
#
# Comments for Patti Labelle Videos

comments1 <- 
  lapply(as.character(videos1$video_id), 
         function(x){ 
           get_comment_threads(c(video_id = x), max_results = 1000)
         }
  )

# Comments for Whitney Houston Videos

comments2 <- 
  lapply(as.character(videos2$video_id), 
         function(x){ 
           get_comment_threads(c(video_id = x), max_results = 1000)
         }
  )

# Comments for Mariah Carey Videos

comments3 <- 
  lapply(as.character(videos3$video_id), 
         function(x){ 
           get_comment_threads(c(video_id = x), max_results = 1000)
         }
  )

# some code to get all the text into one data.frame
#
# define a character vector to store all the comments
#
all_comments <- character()

comments <- c(comments1, comments2, comments3)
#
#loop through every element in the list comments
#
for (i in 1:length(comments)) {
  
  #
  #
  all_comments <-
    c(all_comments, as.character(comments[[i]][["textDisplay"]]))
}
#  

cat("There are ", length(all_comments), " total comments\n")
View(all_comments)
#
# video statistics tabulation
#
# the following "applies" the function to the result of
# as.character(videos$video_id) which just puts
# the contents of the video_id column into a character vector
#
# the function is a "generic" function as it is used in-line
# so it only exists inside the lapply call, not in the 
# environment
#
# all the function does is call get_stats for a video_id
# and lapply repeats that for every item in the character vector
# and the resulting list is stored in videostats for use later
#
videostats <-
  lapply(as.character(videos$video_id), 
         function(x){
           get_stats(video_id = x)
         }
  )
# Getting videostats on Patti LaBelle videos

videostats1 <-
  lapply(as.character(videos1$video_id), 
         function(x){
           get_stats(video_id = x)
         }
  )

# Getting videostats on Whitney Houston videos

videostats2 <-
  lapply(as.character(videos2$video_id), 
         function(x){
           get_stats(video_id = x)
         }
  )

# Getting videostats on Mariah Carey videos

videostats3 <-
  lapply(as.character(videos3$video_id), 
         function(x){
           get_stats(video_id = x)
         }
  )

# Getting all videostats in one vector

videostats <- c(videostats1, videostats2, videostats3)

videostats <- do.call(rbind.data.frame, videostats)
videostats$title <- c(videos1$title, videos2$title, videos3$title)
videostats$date <- c(videos1$date, videos2$date, videos3$date)
videostats <-
  videostats %>%
  select(date, title, viewCount, likeCount, dislikeCount, commentCount) %>%
  as.tibble() %>%
  mutate(viewCount = as.numeric(as.character(viewCount)),
         likeCount = as.numeric(as.character(likeCount)),
         dislikeCount = as.numeric(as.character(dislikeCount)),
         commentCount = as.numeric(as.character(commentCount)))
#
# Install Text Mining Package
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)

# To Prepare the Data
# Convert text to all lower case

all_comments = tm_map(all_comments, tolower)