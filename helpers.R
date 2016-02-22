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

#### Parse date and times
parse_podcasts_step1 <- function(showstats) {
  showstats %<>%
    filter(!is.na(number)) %>%
    mutate(duration = parse_duration(duration),
           date     = dmy(date),
           year     = as.factor(year(date)),
           month    = month(date, abbr = F, label = T),
           weekday  = wday(date, label = T, abbr = F)) %>%
    select(podcast, number, date, year, month, weekday, everything())
  return(showstats)
}

#### Initial collection of the stats file ####
# Thanks a lot, @l3viathan https://twitter.com/L3viathan2142/status/701009923841400833
get_initial_stats <- function(urlpartial = "theincomparable", show_title = "The Incomparable") {
  require(readr)
  require(magrittr)
  require(httr)
  stats_url <- paste0("https://www.theincomparable.com/", urlpartial, "/stats.txt")
  showstats <- readLines(stats_url) %>%
    str_replace_all(',(?=[^"]*"(?:[^"]*"[^"]*")*[^"]*$)', "COMMA") %>%
    str_replace_all('"(?=.*")', "QUOT") %>%
    str_replace("QUOT",'"') %>%
    paste0(collapse = "\n") %>%
    read_csv(col_names = F, trim_ws = T) %>%
    filter(!is.na(X1))

  if (ncol(showstats) == 5) {
    names(showstats) <- c("number", "date", "duration", "title", "host")
  } else {
    names(showstats) <- c("number", "date", "duration", "title", "host", paste0("guest", 1:(ncol(showstats) - 5)))
  }
  showstats$podcast <- show_title
  showstats %<>% select(podcast, everything())
  showstats$title  <- str_replace_all(showstats$title, "COMMA", ",")
  showstats$title  <- str_replace_all(showstats$title, "QUOT", "“")

  return(showstats)
}

extract_main_host <- function(showstats) {
  showstats %>% separate(host, into = c("host_1", "host_2", "host_3", "host_4"), sep = "(\\s(and)|with)") %>%
    mutate(host_2 = ifelse(is.na(host_2), "None", host_2),
           host_1 = str_trim(host_1, "both")) %>%
    rename(guest_dummy_1 = host_2,
           guest_dummy_2 = host_3,
           guest_dummy_3 = host_4) %>%
    return()
}

fix_guests <- function(showstats){
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
  showstats %<>%
    gather(position, guest, contains("guest")) %>%
    filter(!is.na(guest)) %>%
    mutate(guest = str_trim(guest, side = "both")) %>%
    select(-position) %>%
    rename(host = host_1) %>%
    arrange(desc(number))
  return(showstats)
}

get_podcast_stats <- function(urlpartial = "theincomparable", show_title = "The Incomparable"){
    get_initial_stats(urlpartial, show_title) %>%
    parse_podcasts_step1() %>%
    extract_main_host() %>%
    fix_guests()
}

#### Getting topics and categories ####

# Returns a data.frame suitable for colbinding
get_podcast_topics <- function(urlpartial = "theincomparable"){
  require(rvest)
  require(dplyr)
  require(stringr)

  url     <- paste0("https://www.theincomparable.com/", urlpartial, "/archive/")
  entries <- read_html(url) %>%
    html_nodes(css = "#entry") %>%
    html_text()

  epnums <- entries %>%
    str_extract("^\\n\\n\\s*\\d+") %>%
    str_extract("\\d+") %>%
    as.numeric()

  categories <- entries %>%
    str_replace_all("(\\n|\\s)*$", "") %>%
    str_extract("\\s\\w+$") %>%
    str_trim("both") %>%
    str_replace_all("^(minute|minutes|hour)$", "Not provided")

  topics <- entries %>%
    str_extract(".* •") %>%
    str_replace_all("•", "") %>%
    str_replace_all("^\\s$", "Not provided") %>%
    str_trim("both")

  result <- data_frame(number = epnums, category = categories, topic = topics)
  return(result)
}
