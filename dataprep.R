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
incomparable  <- get_podcast_stats("theincomparable", show_title = "The Incomparable")
robot         <- get_podcast_stats("robot",           show_title = "Robot or Not")
gameshow      <- get_podcast_stats("gameshow",        show_title = "Game Show")
teevee        <- get_podcast_stats("teevee",          show_title = "TeeVee")
tvtm          <- get_podcast_stats("tvtm",            show_title = "TV Talk Machine")
tpk           <- get_podcast_stats("tpk",             show_title = "Total Party Kill")
ump           <- get_podcast_stats("ump",             show_title = "Unjustly Maligned")
randomtrek    <- get_podcast_stats("randomtrek",      show_title = "Random Trek")
radio         <- get_podcast_stats("radio",           show_title = "Radio Theatre")
afoot         <- get_podcast_stats("afoot",           show_title = "Afoot!")
defocused     <- get_podcast_stats("defocused",       show_title = "Defocused")
lazydoctorwho <- get_podcast_stats("lazydoctorwho",   show_title = "Lazy Doctor Who")
myke          <- get_podcast_stats("myke",            show_title = "Myke at the Movies")
ruin          <- get_podcast_stats("ruin",            show_title = "Phil and Lisa Ruin the Movies")
cartooncast   <- get_podcast_stats("cartooncast",     show_title = "Cartoon Cast")
pod4ham       <- get_podcast_stats("pod4ham",         show_title = "Pod4Ham")
notplaying    <- get_podcast_stats("notplaying",      show_title = "Not Playing")

#### Saving files locally ####
saveRDS(incomparable, "data/incomparable.rds")
saveRDS(robot,        "data/robot.rds")
saveRDS(teevee,       "data/teevee.rds")
saveRDS(gameshow,     "data/gameshow.rds")
saveRDS(tvtm,         "data/tvtm.rds")
saveRDS(tpk,          "data/tpk.rds")
saveRDS(ump,          "data/ump.rds")
saveRDS(randomtrek,   "data/randomtrek.rds")
saveRDS(radio,        "data/radio.rds")
saveRDS(afoot,        "data/afoot.rds")
saveRDS(defocused,    "data/defocused.rds")
saveRDS(lazydoctorwho,"data/lazydoctorwho.rds")
saveRDS(myke,         "data/myke.rds")
saveRDS(ruin,         "data/ruin.rds")
saveRDS(cartooncast,  "data/cartooncast.rds")
saveRDS(pod4ham,      "data/pod4ham.rds")
saveRDS(notplaying,   "data/notplaying.rds")

#### Binding the good datasets to a master dataset ####
incomparable_master <- bind_rows(incomparable, robot, teevee, gameshow, tvtm, tpk, ump,
                                 randomtrek, radio, afoot, defocused, lazydoctorwho, myke,
                                 ruin, cartooncast, pod4ham, notplaying)

saveRDS(incomparable_master, "data/incomparable_master.rds")

#### Quick check ####
incomparable_master %>% group_by(podcast) %>%
  summarize(episodes = length(unique(number)),
            hosts_unique = length(unique(host)),
            guests   = length(guest),
            guests_unique = length(unique(guest)),
            hours    = round(sum(duration)/60, 2),
            days     = round(hours/60, 2)) %>%
  arrange(desc(episodes))

#### Spreading guests ####
incomparable_master_wide <- incomparable_master %>%
  group_by(title) %>%
  mutate(guest_position = paste("guest_", 1:length(guest))) %>%
  spread(key = guest_position, value = guest)
