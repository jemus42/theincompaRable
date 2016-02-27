#### Helper functions ####

#### Utility functions ####
#### Converting HH:MM:SS or MM:SS to a numeric vector of minutes
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
enhance_datetimes <- function(showstats) {
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

#### Spread long to wide format
# Only applicable to master dataset
widen_people <- function(master, concatenate = FALSE) {
  master %<>%
    group_by(podcast, number, role) %>%
    mutate(person_number = paste0("person_", seq_along(person))) %>%
    spread(person_number, person) %>%
    unite(persons, starts_with("person_"), sep = ", ") %>%
    mutate(persons = str_replace_all(persons, ", NA", "")) %>%
    spread(role, persons) %>%
    ungroup %>%
    select(podcast, number, date, year, month, weekday,
           duration, title, host, guest, category, topic, summary)
  return(master)
}

#### Caching
cache_podcast_data <- function(x, dir = "data", filename = NULL) {
  if (is.null(filename)){
    filename <- deparse(substitute(x))
  }
  path <- paste0(file.path(dir, filename), ".rds")
  message("Saving ", filename, " to ", path)
  saveRDS(x, path)
}

#### Handling the stats.txt ####
#### Initial collection of the stats file
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
    paste0(collapse = "\n") %>% {
      suppressWarnings(read_csv(file = ., col_names = F, trim_ws = T, col_types = cols(X1 = col_character())))
    } %>%
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

#### Preparations after initital collection of stats.txt ####
#### Extract host
# Needs work, see issue #1
extract_show_hosts <- function(showstats) {
  showstats %<>%
    separate(host, into = c("host_1", "guest_1"), sep = "\\swith\\s") %>%
    mutate(host_1 = str_trim(host_1, "both"),
           guest_1 = str_trim(guest_1, "both"))

  if ("host_1" %in% names(showstats) & any(str_detect(showstats$host_1, "\\sand\\s"))) {
    showstats %<>%
      separate(host_1, into = c("host_1", "host_2"), sep = "\\sand\\s")
  }
  if ("host_2" %in% names(showstats) & any(str_detect(showstats$host_2, "\\sand\\s"), na.rm = T)) {
    showstats %<>%
      separate(host_2, into = c("host_2", "host_3"), sep = "\\sand\\s", na.rm = T)
  }
  if ("host_3" %in% names(showstats) & any(str_detect(showstats$host_3, "\\sand\\s"), na.rm = T)) {
    showstats %<>%
      separate(host_3, into = c("host_3", "host_4"), sep = "\\sand\\s")
  }
  return(showstats)
}

#### Further guest management
extract_show_guests <- function(showstats){
  if ("guest_1" %in% names(showstats)) {
    showstats %<>% separate(guest_1, c("guest1_a", "guest1_b"), sep = " and ")
  }
  if ("guest_1b" %in% names(showstats)) {
    showstats %<>% separate(guest_1b, c("guest1_c", "guest1_d"), sep = " and ")
  }
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
    mutate(guest = str_trim(guest, side = "both")) %>%
    select(-position) %>%
    arrange(desc(date))
  return(showstats)
}

#### Collapsing the people
collapse_show_people <- function(showstats) {
  showstats %<>%
    gather(host_position, host, contains("host")) %>%
    gather(role, person, host, guest) %>%
    select(-host_position) %>%
    distinct() %>%
    filter(!is.na(person))
  return(showstats)
}

#### Compilation of the above functions in one ####
handle_people <- function(showstats) {
  showstats %<>%
    extract_show_hosts() %>%
    extract_show_guests() %>%
    collapse_show_people()
  return(showstats)
}

get_podcast_stats <- function(urlpartial = "theincomparable", show_title = "The Incomparable"){
    get_initial_stats(urlpartial, show_title) %>%
    enhance_datetimes() %>%
    handle_people() %>%
    select(-title) %>%
    full_join(y = get_podcast_metadata(urlpartial),
              by = c("number" = "number")) %>%
    filter(!is.na(podcast)) %>%
    arrange(desc(date)) %>%
    select(podcast, number, date, year, month, weekday,
           duration, title, person, role, category, topic, summary)
}

#### Parsing the archive pages ####
#### Getting summaries, topics and categories
get_podcast_metadata <- function(urlpartial = "theincomparable"){
  require(rvest)
  require(dplyr)
  require(stringr)

  url     <- paste0("https://www.theincomparable.com/", urlpartial, "/archive/")

  archive_parsed <- read_html(url)

  entries <- archive_parsed %>%
    html_nodes(css = "#entry") %>%
    html_text()

  epnums <- archive_parsed %>%
    html_nodes(css = ".episode-number") %>%
    html_text() %>%
    as.character()

  # summaries <- entries %>%
  #   str_replace_all("^(\\W)*\\d+(\\W)*", "") %>%
  #   str_replace_all("^.*\\n(\\n+|\\s+)", "") %>%
  #   str_replace_all("\\n(\\n+|\\s+|.+|\\w|\\•|\\,|\\d+)+$", "")

  # Possibly smarter solution?
  summaries <- archive_parsed %>%
    html_nodes(css = ".episode-description") %>%
    html_text() %>%
    str_replace_all("^(\\W)*", "") %>%
    str_replace_all("\\W*$", "")

  titles <- archive_parsed %>%
    html_nodes(css = ".entry-title a") %>%
    html_text()

  categories <- entries %>%
    str_replace_all("(\\n|\\s)*$", "") %>%
    str_extract("\\s\\w+$") %>%
    str_trim("both") %>%
    str_replace_all("^(seconds|minute|minutes|hour)$", "Not provided")

  topics <- entries %>%
    str_extract(".* •") %>%
    str_replace_all("•", "") %>%
    str_replace_all("^\\s$", "Not provided") %>%
    str_trim("both")

  result <- data_frame(number = epnums, title = titles, summary = summaries,
                       category = categories, topic = topics)
  return(result)
}
