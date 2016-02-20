library(readr)
library(ggplot2)
library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(DT)
source("helpers.R")

#### Getting individual show stats ####
incomparable <- get_podcast_stats("theincomparable", show_title = "The Incomparable")
robot        <- get_podcast_stats("robot",           show_title = "Robot or Not")
gameshow     <- get_podcast_stats("gameshow",        show_title = "Game Show")
teevee       <- get_podcast_stats("teevee",          show_title = "TeeVee")
tvtm         <- get_podcast_stats("tvtm",            show_title = "TV Talk Machine")
tpk          <- get_podcast_stats("tpk",             show_title = "Total Party Kill")

#### Saving files locally ####
saveRDS(incomparable, "data/incomparable.rds")
saveRDS(robot,        "data/robot.rds")
saveRDS(teevee,       "data/teevee.rds")
saveRDS(gameshow,     "data/gameshow.rds")
saveRDS(tvtm,         "data/tvtm.rds")
saveRDS(tpk,          "data/tpk.rds")

#### Binding the good datasets to a master dataset ####
incomparable_master <- bind_rows(incomparable, robot, teevee, gameshow, tvtm, tpk)
saveRDS(incomparable_master, "data/incomparable_master.rds")

#### Quick check ####
incomparable_master %>% group_by(podcast) %>%
  summarize(episodes = length(unique(number)),
            guests   = length(guest),
            guests_unique = length(unique(guest)),
            hours    = round(sum(duration)/60, 2))
