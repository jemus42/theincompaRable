source("prerequisites.R")
source("helpers.R")

#### Getting individual show stats ####
incomparable  <- get_podcast_stats("theincomparable", show_title = "The Incomparable")
robot         <- get_podcast_stats("robot",         show_title = "Robot or Not?")
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
ruin          <- get_podcast_stats("ruin",          show_title = "Ruin the Movies")
cartooncast   <- get_podcast_stats("cartooncast",   show_title = "Cartoon Cast")
pod4ham       <- get_podcast_stats("pod4ham",       show_title = "Pod4Ham")
notplaying    <- get_podcast_stats("notplaying",    show_title = "Not Playing")
bonustrack    <- get_podcast_stats("bonustrack",    show_title = "Bonus Track")
sophomorelit  <- get_podcast_stats("sophomorelit",  show_title = "Sophomore Lit")

#### Binding the good datasets to a master dataset ####
incomparable_master <- bind_rows(incomparable, robot, teevee, gameshow, tvtm, tpk, ump,
                                 randomtrek, radio, afoot, defocused, lazydoctorwho, myke,
                                 ruin, cartooncast, pod4ham, notplaying, bonustrack,
                                 sophomorelit) %>%
                        filter(!is.na(podcast))

# Spreading guests
incomparable_master_wide <- incomparable_master %>%
                              widen_people() %>%
                              full_join(get_podcast_segment_episodes(),
                                        by = c("number" = "number", "podcast" = "podcast"))

# Appending the segments to the long dataset
incomparable_master %<>%
  full_join(get_podcast_segment_episodes(),
            by = c("number" = "number", "podcast" = "podcast"))

#### Write to disk ####
cache_podcast_data(incomparable_master_wide)
cache_podcast_data(incomparable_master)

# Write master set as CSV with ; as separator because Numbers likes that more than ,
incomparable_master_wide %>%
  mutate(summary = str_replace_all(summary, '\\s"', ' “'),
         summary = str_replace_all(summary, '"(\\s)*', '” '),
         summary = str_trim(summary, "right")) %>%
  arrange(desc(date)) %>%
  write.table(., "data/incomparable_master_wide.csv", sep = ";", row.names = F)

incomparable_master %>%
  mutate(summary = str_replace_all(summary, '\\s"', ' “'),
         summary = str_replace_all(summary, '"(\\s)*', '” '),
         summary = str_trim(summary, "right")) %>%
  arrange(desc(date)) %>%
  write.table(., "data/incomparable_master.csv", sep = ";", row.names = F)

#### Also do Relay.fm things ####

relay <- get_relay_shows()
cache_podcast_data(relay)

relay %>%
  arrange(desc(date)) %>%
  write.table(., "data/relay.csv", sep = ";", row.names = F)

