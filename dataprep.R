library(readr)
library(ggplot2)
library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(DT)
source("helpers.R")


#### Okay ####
# Output seems to be okay

# Problem with 86 due to too many damn quotes and commas
incomparable <- get_initial_stats("theincomparable", show_title = "The Incomparable") %>%
                  get_podcast_stats()
robot        <- get_initial_stats("robot", show_title = "Robot or Not") %>%
                  get_podcast_stats()
gameshow     <- get_initial_stats("gameshow", show_title = "Game Show") %>%
                 get_podcast_stats()

#### Needs work ####
# Something is not right, but it's more or less usable
# Missing episodes 65 and 75
teevee       <- get_initial_stats("teevee", show_title = "TeeVee") %>%
                  get_teevee_stats()

#### Experimental ####
# Testing
tvtm     <- get_initial_stats("tvtm", show_title = "TV Talk Machine") %>%
  get_podcast_stats()

#### Saving files locally ####
saveRDS(incomparable, "data/incomparable.rds")
saveRDS(robot,        "data/robot.rds")
#saveRDS(teevee,       "data/teevee.rds")
saveRDS(gameshow,     "data/gameshow.rds")

#### Binding the good datasets to a master dataset ####
incomparable_master <- bind_rows(incomparable, robot, gameshow)
saveRDS(incomparable_master, "data/incomparable_master.rds")

#### Quick check ####
incomparable_master %>% group_by(podcast) %>%
  summarize(episodes = length(unique(number)),
            guests   = length(guest),
            guests_unique = length(unique(guest)),
            hours    = round(sum(duration)/60, 2))
