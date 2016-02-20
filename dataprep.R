library(readr)
library(ggplot2)
library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(DT)
source("helpers.R")

incomparable <- "https://www.theincomparable.com/theincomparable/stats.txt" %>%
                  read_csv(col_names = F, trim_ws = T, col_types = c()) %>%
                  set_colnames(c("number", "date", "duration", "title", "host", paste0("guest", 1:(ncol(.)-5))))

incomparable %<>%
  filter(!is.na(number)) %>%
  mutate(duration = parse_duration(duration),  # Duration in minutes
         date = dmy(date),
         host_first = str_extract(host, "^\\w*\\s\\w*"),
         host_second = str_replace(host, "^.*with\\s", ""),
         host = host_first) %>%
  separate(host_second, c("guest_1", "guest_2"), sep = " and ") %>%
  separate(guest1, c("guest1_1", "guest1_2"), sep = " and ") %>%
  separate(guest2, c("guest2_1", "guest2_2"), sep = " and ") %>%
  separate(guest3, c("guest3_1", "guest3_2"), sep = " and ") %>%
  separate(guest4, c("guest4_1", "guest4_2"), sep = " and ") %>%
  gather(position, guest, contains("guest")) %>%
  filter(!is.na(guest)) %>%
  select(number, date, duration, title, host, guest) %>%
  arrange(desc(number))

saveRDS(incomparable, "data/incomparable.rds")
