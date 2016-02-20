#### Helper functions ####

#### Converting HH:MM:SS or MM:SS to a numeric vector of minutes ####
parse_duration <- function(x){
  mins <- sapply(x, function(x){
            if (str_count(x, ":") == 2) {
              xx      <- as.numeric(unlist(str_split(x, ":")))
              minutes <- xx[1] * 60 + xx[2] + xx[3] / 60
            } else if (str_count(x, ":") == 1) {
              xx      <- as.numeric(unlist(str_split(x, ":")))
              minutes <- xx[1] + xx[2] / 60
            } else {
              stop("Unexpected input format ", x)
            }
            return(unname(minutes))
          })
  mins <- unname(mins)
  return(mins)
}

#### Initial collection of the stats file ####
get_initial_stats <- function(urlpartial = "theincomparable", show_title = "The Incomparable") {
  require(readr)
  require(magrittr)
  require(httr)
  stats_url <- paste0("https://www.theincomparable.com/", urlpartial, "/stats.txt")
  showstats <- readLines(stats_url) %>%
    str_replace_all(',(?=[^"]*"(?:[^"]*"[^"]*")*[^"]*$)', "COMMA") %>%
    str_replace_all('"', "“") %>%
    paste0(collapse = "\n") %>%
    read_csv(col_names = F, trim_ws = T) %>%
    filter(!is.na(X1))

  if (ncol(showstats) == 5) {
    names(showstats) <- c("number", "date", "duration", "title", "host")
  } else {
    names(showstats) <- c("number", "date", "duration", "title", "host", paste0("guest", 1:(ncol(showstats) - 5)))
  }
  showstats$podcast <- show_title
  showstats$guest1[!(str_detect(showstats$host, pattern = ".*(and|with).*"))] <- "None"
  showstats$title  <- str_replace_all(showstats$title, "COMMA", ",")
  showstats$title  <- str_replace_all(showstats$title, "^“", "")
  showstats$title  <- str_replace_all(showstats$title, "“$", "")
  return(showstats)
}

#### Fully clean stats data ####
get_podcast_stats <- function(showstats) {
  showstats %<>%
    filter(!is.na(number)) %>%
    mutate(duration = parse_duration(duration),
           date = dmy(date),
           host_first = str_extract(host, "^\\w*\\s\\w*"),
           host_second = str_replace(host, "^.*(with|and)\\s", ""),
           host = host_first) %>%
    separate(host_second, c("guest_1", "guest_2"), sep = " and ")

  if ("guest1" %in% names(showstats)) {
    showstats %<>% separate(guest1, c("guest1_1", "guest1_2"), sep = " and ")
  }
  if ("guest2" %in% names(showstats)) {
    showstats %<>% separate(guest2, c("guest2_1", "guest2_2"), sep = " and ")
  }
  if ("guest3" %in% names(showstats)) {
    showstats %<>% separate(guest3, c("guest3_1", "guest3_2"), sep = " and ")
  }
  if ("guest4" %in% names(showstats)) {
    showstats %<>% separate(guest4, c("guest4_1", "guest4_2"), sep = " and ")
  }
  if ("guest5" %in% names(showstats)) {
    showstats %<>% separate(guest5, c("guest5_1", "guest5_2"), sep = " and ")
  }
  showstats %>%
    gather(position, guest, contains("guest")) %>%
    filter(!is.na(guest)) %>%
    select(podcast, number, date, duration, title, host, guest) %>%
    arrange(desc(number)) %>%
    return()
}


get_teevee_stats <- function(showstats) {
  showstats %<>%
    filter(!is.na(number)) %>%
    mutate(duration = parse_duration(duration),
           date = dmy(date),
           host_first = str_extract(host, "^\\w*\\s\\w*"),
           host_second = str_replace(host, "with", "and"),
           host_second = str_replace(host_second, "^\\w*\\s\\w*\\s(and)\\s", ""),
           host_first  = str_trim(host_first, "both"),
           host_second = str_trim(host_second, "both"),
           host = host_first) %>%
    separate(host_second, c("guest_1", "guest_2", "guest_3"), sep = " and ")

  if ("guest1" %in% names(showstats)) {
    showstats %<>% separate(guest1, c("guest1_1", "guest1_2"), sep = " and ")
  }
  if ("guest2" %in% names(showstats)) {
    showstats %<>% separate(guest2, c("guest2_1", "guest2_2"), sep = " and ")
  }
  if ("guest3" %in% names(showstats)) {
    showstats %<>% separate(guest3, c("guest3_1", "guest3_2"), sep = " and ")
  }
  if ("guest4" %in% names(showstats)) {
    showstats %<>% separate(guest4, c("guest4_1", "guest4_2"), sep = " and ")
  }
  if ("guest5" %in% names(showstats)) {
    showstats %<>% separate(guest5, c("guest5_1", "guest5_2"), sep = " and ")
  }
  showstats %>%
    gather(position, guest, contains("guest")) %>%
    filter(!is.na(guest)) %>%
    select(podcast, number, date, duration, title, host, guest) %>%
    arrange(desc(number)) %>%
    mutate(guest = str_trim(guest, side = "both")) %>%
    filter(host != guest) %>%
    return()
}
