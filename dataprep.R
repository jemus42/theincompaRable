library(readr)
library(ggplot2)
library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(DT)
source("helpers.R")

incomparable <- get_initial_stats("theincomparable",
                                  show_title = "The Incomparable") %>%
                  get_podcast_stats()
robot        <- get_initial_stats("robot",
                                  show_title = "Robot or Not") %>%
                  get_podcast_stats()
teevee       <- get_initial_stats("teevee",
                                  show_title = "TeeVee") %>%
                  get_teevee_stats()

saveRDS(incomparable, "data/incomparable.rds")
saveRDS(robot,        "data/robot.rds")
saveRDS(teevee,       "data/teevee.rds")

incomparable_master <- bind_rows(incomparable, robot, teevee)

incomparable_master %>% group_by(podcast) %>%
  summarize(episodes = length(unique(number)))
