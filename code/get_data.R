library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(spotifyr)
id <- "your own id"
secret <- "your own secret"

Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
Sys.setenv(SPOTIFY_REDIRECT_URI = 'your own redirect uri')

access_token <- get_spotify_access_token()
categories <- list(
  "Arts%20%26%20Entertainment",
  "Business%20%26%20Technology",
  "Educational",
  "Games",
  "Lifestyle%20%26%20Health",
  "News%20%26%20Politics",
  "Sports%20%26%20Recreation"
)

search_shows <- function (q, offset = 0, type = "show", market = "US", limit = 50,
                          authorization = get_spotify_access_token()) 
{
  url <- "https://api.spotify.com/v1/search"
  params <- list(q = q, type = type, market = market, limit = limit,
                 offset = offset, access_token = authorization)
  res <- RETRY("GET", url, query = params, encode = "json")
  stop_for_status(res)
  res <- fromJSON(content(res, as = "text", encoding = "UTF-8"), 
                  flatten = TRUE)
  res
}

show_ids <- list()
for (category in categories) {
  show <- search_shows(q = category)
  ids <- show$shows$items$id
  names <- show$shows$items$name
  show_ids[[category]] <- data.frame(
    Showid = ids,
    Showname = names,
    stringsAsFactors = FALSE
  )
  show <- search_shows(q = category, offset = 50)
  ids <- show$shows$items$id
  names <- show$shows$items$name
  show_ids[[category]] <- data.frame(
    Showid = ids,
    Showname = names,
    stringsAsFactors = FALSE
  )
  Sys.sleep(1)
}

shows <- do.call(rbind, lapply(names(show_ids), function(category) {
  data.frame(
    Category = category,
    Showid = show_ids[[category]]$Showid,
    Showname = show_ids[[category]]$Showname,
    stringsAsFactors = FALSE
  )
}))

#write.csv(shows, "category_shows.csv", row.names = FALSE)

final_data <- data.frame(
  Category = character(),
  Showid = character(),
  Showname = character(),
  Episodeid = character(),
  Episodename = character(),
  Description = character(),
  Release_date = character(),
  Duration_ms = character(),
  stringsAsFactors = FALSE
)

for (category in names(show_ids)) {
  data <- show_ids[[category]]
  shows_ids <- data$Showid
  shows_names <- data$Showname
  
  for (i in seq_along(shows_ids)) {
    show_id <- shows_ids[i]
    show_name <- shows_names[i]
    url <- paste0("https://api.spotify.com/v1/shows/", show_id, "/episodes")
    headers <- add_headers(Authorization = paste("Bearer", access_token))
    response <- GET(url, headers)
    episodes <- content(response, as = "parsed", simplifyDataFrame = TRUE)
    episode_ids <- episodes$items$id
    episode_names <- episodes$items$name
    descriptions <- episodes$items$description
    release_date = episodes$items$release_date
    duration_ms = episodes$items$duration_ms
    
    temp_data <- data.frame(
      Category = category,
      Showid = show_id,
      Showname = show_name,
      Episodeid = episode_ids,
      Episodename = episode_names,
      Description = descriptions,
      Release_date = release_date,
      Duration_ms = duration_ms,
      stringsAsFactors = FALSE
    )
    final_data <- rbind(final_data, temp_data)
    Sys.sleep(1)
  }
}

final_data$Category <- str_replace_all(final_data$Category, "%20", " ")
final_data$Category <- str_replace_all(final_data$Category, "%26", "&")
write.csv(final_data, "all_episodes.csv", row.names = FALSE)