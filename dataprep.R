library(readr)
library(ggplot2)
library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
source("helpers.R")

incomparable <- "https://www.theincomparable.com/theincomparable/stats.txt" %>%
                  read_csv(col_names = F, trim_ws = T, col_types = c()) %>%
                  set_colnames(c("number", "date", "duration", "title", "host", paste0("guest", 1:4)))

incomparable %<>%
  filter(!is.na(number)) %>%
  mutate(duration = parse_duration(duration),  # Duration in minutes
         date = dmy(date),
         host_first = str_extract(host, "^\\w*\\s\\w*"),
         host_second = str_replace(host, "^.*with\\s", "")) %>%
  select(number, date, duration, title, host_first, host_second, everything())


#### Experimental plots for testing ####

ggplot(data = incomparable, aes(x = reorder(host_first, duration), y = duration)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "The Incomparable Podcast\nShow Duration by Primary Host",
       x = "Host (first listed)", y = "Duration (min)") +
  theme_bw()

