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
incomparable      <- get_podcast_stats("theincomparable", show_title = "The Incomparable")
incomparable_wide <- incomparable %>% widen_guests()

robot         <- get_podcast_stats("robot",         show_title = "Robot or Not")
gameshow      <- get_podcast_stats("gameshow",      show_title = "Game Show")
teevee        <- get_podcast_stats("teevee",        show_title = "TeeVee")
tvtm          <- get_podcast_stats("tvtm",          show_title = "TV Talk Machine")
tpk           <- get_podcast_stats("tpk",           show_title = "Total Party Kill")
ump           <- get_podcast_stats("ump",           show_title = "Unjustly Maligned")
randomtrek    <- get_podcast_stats("randomtrek",    show_title = "Random Trek")
radio         <- get_podcast_stats("radio",         show_title = "Radio Theatre")
afoot         <- get_podcast_stats("afoot",         show_title = "Afoot!")
defocused     <- get_podcast_stats("defocused",     show_title = "Defocused")
lazydoctorwho <- get_podcast_stats("lazydoctorwho", show_title = "Lazy Doctor Who")
myke          <- get_podcast_stats("myke",          show_title = "Myke at the Movies")
ruin          <- get_podcast_stats("ruin",          show_title = "Phil and Lisa Ruin the Movies")
cartooncast   <- get_podcast_stats("cartooncast",   show_title = "Cartoon Cast")
pod4ham       <- get_podcast_stats("pod4ham",       show_title = "Pod4Ham")
notplaying    <- get_podcast_stats("notplaying",    show_title = "Not Playing")

#### Saving files locally ####
cache_podcast_data(incomparable)
cache_podcast_data(incomparable_wide)
cache_podcast_data(robot)
cache_podcast_data(teevee)
cache_podcast_data(gameshow)
cache_podcast_data(tvtm)
cache_podcast_data(tpk)
cache_podcast_data(ump)
cache_podcast_data(randomtrek)
cache_podcast_data(radio)
cache_podcast_data(afoot)
cache_podcast_data(defocused)
cache_podcast_data(lazydoctorwho)
cache_podcast_data(myke)
cache_podcast_data(ruin)
cache_podcast_data(cartooncast)
cache_podcast_data(pod4ham)
cache_podcast_data(notplaying)

#### Binding the good datasets to a master dataset ####
incomparable_master <- bind_rows(incomparable, robot, teevee, gameshow, tvtm, tpk, ump,
                                 randomtrek, radio, afoot, defocused, lazydoctorwho, myke,
                                 ruin, cartooncast, pod4ham, notplaying)

cache_podcast_data(incomparable_master)

#### Spreading guests
incomparable_master_wide <- incomparable_master %>%
  widen_guests()

cache_podcast_data(incomparable_master_wide, "data/incomparable_master_wide.rds")

#### Quick tets ####
incomparable_master_wide %>% group_by(podcast) %>%
  summarize(episodes = length(unique(number)),
            hosts_unique = length(unique(host)),
            hours    = round(sum(duration)/60, 2),
            days     = round(hours/60, 2)) %>%
  arrange(desc(episodes))
