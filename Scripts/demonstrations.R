
## Data Collection

# Day 1 -------------------------------------------------------------------

bs_search_actors(
  query, # no wildcards, case insensitive
  typeahead = FALSE, # if TRUE, returns full profile (display name and description); if FALSE, returns limited profile (only name + description) 
  limit = NULL, # when 0 = default, is 25
  clean = TRUE # when TRUE, the JSON output is turned into a mostly clean dataframe
)

## Ex. 1: Search for your profile with typeahead true and false
my_profile_false <- bs_search_actors(
  "api-muncher",
  typeahead = FALSE,
  #[other arguments]
  limit = NULL,
  #[other arguments]
  clean = TRUE)

my_profile_true <- bs_search_actors(
  "api-muncher",
  typeahead = TRUE,
  #[other arguments]
  limit = NULL,
  #[other arguments]
  clean = TRUE)

setdiff(names(my_profile_false), names(my_profile_true))

names(my_profile_false)
names(my_profile_true)

## Ex. 2: Conduct three full-profile searches of for academics from three adjacent disciplines

disciplines <- c("department of sociology", "department of economy", "department of political science")

department_of_sociology <- bs_search_actors(
  "department of sociology",
  typeahead = TRUE,
  limit = NULL,
  clean = TRUE)

department_of_economy <- bs_search_actors(
  "department of economy",
  typeahead = TRUE,
  limit = NULL,
  clean = TRUE)

department_of_political_science <- bs_search_actors(
  "department of political science",
  typeahead = TRUE,
  limit = NULL,
  clean = TRUE)

# merge results into a single data frame
academic_profiles <- bind_rows(
  department_of_sociology,
  department_of_economy,
  department_of_political_science
)


## Search Posts

bs_search_posts(
  query, # supports wildcards (*), but buggy, not case sensitive
  sort = NULL, # order results by latest (default) or top (engagement-based)
  since = NULL, # filter posts since and or until
  until = NULL,
  mentions = NULL,
  author = NULL,
  lang = NULL,
  domain = NULL,
  url = NULL,
  tag = NULL, # filter by tag
  #[other arguments]
  limit = NULL,
  #[other arguments]
)

## Ex. 3: Search for posts mentioning groups from ex. 2
economy_posts <- bs_search_posts(
  query = "economy",
  lang = "en",
  limit = NULL,
  clean = TRUE
)

sociology_posts <- bs_search_posts(
  query = "sociology",
  lang = "en",
  limit = NULL,
  clean = TRUE
)

political_science_posts <- bs_search_posts(
  query = "political science",
  lang = "en",
  limit = NULL,
  clean = TRUE
)

# merge results into a single data frame
academic_posts <- bind_rows(
  economy_posts,
  sociology_posts,
  political_science_posts
)

names(academic_posts)
names(economy_posts)
names(sociology_posts)
names(political_science_posts)


## Get Profiles

bs_get_profile(
  actors, #accepts one ore more handle and did, e.g., ajpseditor.bsky.social, did:plc:xzzesxndb6f3xsp2phcfhb5n
  # note that argument is in plural — it can therefore accept a vector of multiple users
  # e.g., c("ajpseditor.bsky.social", "did:plc:xzzesxndb6f3xsp2phcfhb5n")
  # but be aware of rate limits when doing so
  
  #[other arguments]
)

verge_actors <- bs_get_profile(
  actors = "theverge.com"
)

## Looping function for handling a lot of data

# Basic Loop
the_list <- list() # create empty list to store results

for(i in seq_along(verge_actors$handle)) { # loop through each handle in the dataframe
  # get profile for each handle and store in list
  the_list[[i]] <- bs_get_profile(verge_actors$handle[i]) 
}

profiles <- bind_rows(the_list) # combine all profile data into a single dataframe


# more advanced loop
the_list <- list()

for(i in seq_along(verge_actors$handle)) {
  message("Processing:", i)
  tryCatch({
    the_list[[i]] <- bs_get_profile(verge_actors$handle[i])
  }, error = function(e) {
    the_list[[i]] <<- NA
    message("  Failed!")
  })
  Sys.sleep(0.2)
}

profiles <- bind_rows(the_list)


# BS Loop Function
# function to loop with bskyr functions
bs_loop <- function(data, var, func, add_var = FALSE, sleep = 0.2, ...) {
  
  # Create empty list to store results
  the_list <- list()
  
  # Capture the function and variable names
  func_name <- deparse(substitute(func))
  var_name <- deparse(substitute(var))
  
  # Get the actual function and variable values
  func <- get(func_name)
  variable_values <- data[[var_name]]
  
  # Loop through each value
  for(i in seq_along(variable_values)) {
    message("Processing: ", i, " out of ", length(variable_values))
    tryCatch({
      # Call function with the variable value and any additional arguments
      result <- func(variable_values[i], ...)
      # Add the variable value to the result if requested
      if(add_var && !is.null(result) && nrow(result) > 0) {
        result[[paste0("source_", var_name)]] <- variable_values[i]
      }
      the_list[[i]] <- result
    }, error = function(e) {
      the_list[[i]] <<- NA
      message("  Failed!")
    })
    Sys.sleep(sleep)
  }
  # Bind rows and return result
  output <- bind_rows(the_list)
  return(output)
}


## Ex. 4 Get personal profile

personal_profile <- bs_get_profile(
  actors = "squishboii.bsky.social"
)

## Ex. 5 Get profile data for your communities from Exercise 2

# Basic Loop
the_list <- list() # create empty list to store results

for(i in seq_along(department_of_economy$handle)) { # loop through each handle in the dataframe
  # get profile for each handle and store in list
  the_list[[i]] <- bs_get_profile(department_of_economy$handle[i]) 
}

profiles <- bind_rows(the_list) # combine all profile data into a single dataframe


# advanced loop
# more advanced loop
the_list <- list()

for(i in seq_along(department_of_economy$handle)) {
  message("Processing:", i)
  tryCatch({
    the_list[[i]] <- bs_get_profile(department_of_economy$handle[i])
  }, error = function(e) {
    the_list[[i]] <<- NA
    message("  Failed!")
  })
  Sys.sleep(0.2)
}

profiles2 <- bind_rows(the_list)


# BS Loop Function

# BS Loop Function
# function to loop with bskyr functions
bs_loop <- function(data, var, func, add_var = FALSE, sleep = 0.2, ...) {
  
  # Create empty list to store results
  the_list <- list()
  
  # Capture the function and variable names
  func_name <- deparse(substitute(func))
  var_name <- deparse(substitute(var))
  
  # Get the actual function and variable values
  func <- get(func_name)
  variable_values <- data[[var_name]]
  
  # Loop through each value
  for(i in seq_along(variable_values)) {
    message("Processing: ", i, " out of ", length(variable_values))
    tryCatch({
      # Call function with the variable value and any additional arguments
      result <- func(variable_values[i], ...)
      # Add the variable value to the result if requested
      if(add_var && !is.null(result) && nrow(result) > 0) {
        result[[paste0("source_", var_name)]] <- variable_values[i]
      }
      the_list[[i]] <- result
    }, error = function(e) {
      the_list[[i]] <<- NA
      message("  Failed!")
    })
    Sys.sleep(sleep)
  }
  # Bind rows and return result
  output <- bind_rows(the_list)
  return(output)
}

profiles3 <- bs_loop(
  data = department_of_economy,
  var = handle,
  func = bs_get_profile,
  add_var = TRUE,
  sleep = 0.2
)


## Get Profile Feed

bs_get_author_feed(
  actor, # note that it is in singular — does not accept multiple handle or did
  limit = NULL,
)

# get custom feeds
bs_get_feeds( # Retrieve a list of additional feeds a profile might have created
  actor,
  limit = NULL,
)

bs_get_feed( # fetch the posts within any of known feeds 
  feed, # accepts a single uri for a feed
  limit = NULL,
)

# Ex. 6 Collect personal feeds for members of one of your communities
# use a loop and add each profile’s personal identifier as a variable, called profile_did

bs_get_author_feed(
  actor = "squishboii.bsky.social", 
  limit = NULL,
)

personal_feeds <- bs_loop(
  data = department_of_economy,
  var = did,
  func = bs_get_author_feed,
  add_var = TRUE,
  sleep = 0.2,
  limit = NULL
)



# Get follows and followers
bs_get_follows( # Retrieve a list of accounts a profile follows
  actor,
  limit = NULL,
)

bs_get_follows(actor = "squishboii.bsky.social")

bs_get_followers( # Retrieve a list of followers of a profile
  actor,
  limit = NULL,
)

bs_get_followers(actor = "squishboii.bsky.social")

# get relationships
# Get whether a user follows or is followed by one or more others
bs_get_relationships(
  actor,
  others, # can be one or more users, specified by either their handle or did
)

bs_get_relationships(
  actor = "squishboii.bsky.social",
  others = c("frankdied.bsky.social",
             "squishboii.bsky.social")
)


## Ex. 8 Collect the list of profiles followed by the members of one of your communities

Follower_list <- bs_loop(
  data = department_of_economy,
  var = handle,
  func = bs_get_follows,
  add_var = TRUE
)

## Ex. 9 Collect the relationship information among the members of the same community
# compare the outputs with the one from the previous exercise, in terms of structure and variables

Relationship_list <- bs_loop(
  data = department_of_economy,
  var = handle,
  func = bs_get_relationships,
  add_var = TRUE,
  others = department_of_economy$handle
)

## Get Lists
# Retrieve a list of lists that a profile might have created
bs_get_actor_lists(
  actor,
  limit = NULL,
)

# then fetch the profiles on a given list
bs_get_list(
  list,
  limit = NULL,
)

# You can fetch the posts from the profiles on a given list
bs_get_list_feed(
  list,
  limit = NULL,
)

# example
lists <- bs_get_actor_lists(actor = "resulumit.bsky.social")
lists
bs_get_list_feed(list = lists$uri[1])

## Get Posts
# Get data for one or more known posts 
bs_get_posts(
  uris,
)
# example
bs_get_posts(uris = "at://did:plc:ic6zqvuw5ulmfpjiwnhsr2ns/app.bsky.feed.post/3k7qmjev5lr2s")

## Ex. 10 Collect new data for the posts from Exercise 3 using the bs_get_posts() function
# compare the output with that of Exercise 3, where we used bs_search_posts(), in terms of structure and variables

view(economy_posts)
get_posts_economy <- bs_loop(
  data = economy_posts,
  var = uri,
  func = bs_get_posts,
  add_var = TRUE
)

setdiff(names(economy_posts), names(get_posts_economy))


## Ex. 11 Using one or more posts you collected in Exercise 10:
# get the list of profiles that liked it using bs_get_post_likes()
# get the list of profiles that re-posted it using bs_get_reposts()
# get the list of profiles that quoted it using bs_get_quotes()
# compare the outputs, in terms of structure and variables

eco_post_likes <- bs_loop(
  data = get_posts_economy,
  var = uri,
  func = bs_get_post_likes,
  add_var = TRUE
)

eco_post_reposts <- bs_loop(
  data = get_posts_economy,
  var = uri,
  func = bs_get_reposts,
  add_var = TRUE
)

eco_post_quotes <- bs_loop(
  data = get_posts_economy,
  var = uri,
  func = bs_get_quotes,
  add_var = TRUE
)
setdiff(names(eco_post_likes), names(eco_post_reposts))#, names(eco_post_quotes))
setdiff(names(eco_post_likes), names(eco_post_quotes))
setdiff(names(eco_post_reposts), names(eco_post_quotes))



the_uri <- "at://did:plc:rcgrkfgh55jxcbsejzkzexv7/app.bsky.feed.post/3m3fbxto3jk22"

e11_1 <- bs_get_post_likes(uri = "at://did:plc:rcgrkfgh55jxcbsejzkzexv7/app.bsky.feed.post/3m3fbxto3jk22")
e11_2 <- bs_get_reposts(the_uri)
e11_3 <- bs_get_quotes(the_uri)


## Ex. 12 Collect new data for the posts from Exercise 3
#using the bs_get_record() function
#compare this output with previous methods:
#  bs_search_posts() from Exercise 3
#bs_get_posts() from Exercise 10
#note differences in data structure and available fields

get_record_economy <- bs_loop(
  data = economy_posts,
  var = uri,
  func = bs_get_record,
  add_var = TRUE
)

setdiff(names(economy_posts), names(get_record_economy))
setdiff(names(get_posts_economy), names(get_record_economy))

e12 <- bs_loop(
  data = e3_econ,
  var = uri,
  func = bs_get_record
)

## Get Record

bs_get_record(
  repo = NULL, # full uri of the record (gets parsed automatically)
  #or a did or handle, to be supplemented* with collection and rkey
  collection = NULL,
  rkey = NULL,
)

# record of post
bs_get_record(repo = "at://did:plc:gz3lflznbckpfsukml2mzrfi/app.bsky.feed.post/3la4crdz4hm23")

# record of feed
bs_get_record(repo = "at://did:plc:7lrkwgegtimlpzcydpimzmzr/app.bsky.feed.generator/aaaczhbec6n2o")

# record of follow
bs_get_record(repo = "at://did:plc:234e252yxxcfcr3rfesalzoo/app.bsky.graph.follow/3k7tgavaiy22g")


## Ex. 13 Collect data on follows from Exercise 9
#using the uri in the following variable

# Relationship_list

get_record_relationships <- bs_loop(
  data = Relationship_list,
  var = following,
  func = bs_get_record,
  limit = 10)

setdiff(names(Relationship_list), names(get_record_relationships))

e13 <- bs_loop(
  data = e9,
  var = following,
  func = bs_get_record,
  limit = 10
)

e13_alt <- bs_get_record(repo = "at://did:plc:e2alvfh67skz7iov5yevptwt/app.bsky.graph.follow/3k7tfdrep7j23")


## End of Data Collection Script

# Day 2 -------------------------------------------------------------------

## Data Cleaning

library(tidyr)
library(dplyr)
  
df <- bs_search_posts("lucerne") %>%  
  unnest_wider(col = record, 
                names_sep = "_")

df_hoist <- bs_search_posts(query = "lucerne") %>%
  hoist(record, post = "text")

df_embed <- bs_search_posts(query = "political science") %>%
  bs_clean_embed()


## Ex. 14

e14 <- readRDS("data/raw/posts_search.rds") %>%
   unnest_wider(col = author, names_sep = "_") %>%
   hoist(record, text = "text", createdAt = "createdAt")

e14_hoist <- readRDS("data/raw/posts_search.rds") %>%
  hoist(author, handle = "handle", displayName = "displayName")  

  
## Ex. 15

e15 <- readRDS("data/raw/posts_get.rds") %>%
  bs_clean_embed()

saveRDS(object = e15, "data/raw/posts_get.rds")


## Cleaning | Strings | Overview

## Bsky Team
# regex to match profile mentions
mention_bluesky <- "(^|\\s|\\()@([a-zA-Z0-9.-]+)\\b"

# regex to match tags
tag_bluesky <- "(^|\\s)[#＃]((?!\\ufe0f)[^\\s\\u00AD\\u2060\\u200A\\u200B\\u200C\\u200D\\u20e2]*[^\\d\\s\\p{P}\\u00AD\\u2060\\u200A\\u200B\\u200C\\u200D\\u20e2]+[^\\s\\u00AD\\u2060\\u200A\\u200B\\u200C\\u200D\\u20e2]*)?"

# regex to match URLs
url_bluesky <- "(^|\\s|\\()((https?://\\S+)|(([a-z][a-z0-9]*(\\.[a-z0-9]+)+)\\S*))"

## bskyr package
# regex to match profile mentions
mention_bskyr <- "[$|\\W](@([a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?\\.)+[a-zA-Z]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)"

# regex to match tags
tag_bskyr <- "(^|\\s)[#\\uFF03](?<tag>(?!\\ufe0f)[^\\s\\u00AD\\u2060\\u200A\\u200B\\u200C\\u200D\\u20e2]*[^\\d\\s\\p{P}\\u00AD\\u2060\\u200A\\u200B\\u200C\\u200D\\u20e2]+[^\\s\\u00AD\\u2060\\u200A\\u200B\\u200C\\u200D\\u20e2]*)?"

# regex to match URLs
url_bskyr <- "(^|[$|\\W])(https?://(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*[-a-zA-Z0-9@%_\\+~#//=])?)"


## other regex patterns
# regex to match emojis
emoji_regex <- ":[a-zA-Z0-9_]+:"

# regex to match line breaks
linebreak_regex <- "\\r\\n|\\n|\\r"

# regex to match punctuation
punctuation_regex <- "[[:punct:]]"

# regex to match numbers
number_regex <-"\\b[+-]?(?:\\d*\\.\\d+|\\d+)\\b"

## Detect
str_detect(
  string,
  pattern,
)

# example
post <- "These from @handle1.bsky.social are #socool. 👏 A #mustsee on @handle2.com!
          👉 https://handle2.com/aq7MJJ2..."
str_detect(string = post, pattern = url_bluesky)

## Count
str_count(
  string,
  pattern,
)

# example
post <- "These from @handle1.bsky.social are #socool. 👏 A #mustsee on @handle2.com!
          👉 https://handle2.com/aq7MJJ2..."
str_count(string = post, pattern = mention_bluesky)


## Extract

str_extract(
  string,
  pattern,
)

str_extract_all(
  string,
  pattern,
)

# example
post <- "These from @handle1.bsky.social are #socool. 👏 A #mustsee on @handle2.com!
          👉 https://handle2.com/aq7MJJ2..."
str_extract_all(string = post, pattern = url_bluesky)

## Remove
str_remove(
  string,
  pattern,
)

str_remove_all(
  string,
  pattern,
)

# example
post <- "These from @handle1.bsky.social are #socool. 👏 A #mustsee on @handle2.com!
          👉 https://handle2.com/aq7MJJ2..."
str_remove(string = post, pattern = mention_bluesky)


## Replace
str_replace(
  string,
  pattern,
  replacement,
)

str_replace_all(
  string,
  pattern,
  replacement,
)

# example
post <- "These from @handle1.bsky.social are #socool. 👏 A #mustsee on @handle2.com!
          👉 https://handle2.com/aq7MJJ2..."
str_replace(string = post, pattern = mention_bluesky, replacement = "[MENTION]")


## Squish
str_squish(
  string,
)
# example
post <- "These from @handle1.bsky.social are #socool. 👏 A #mustsee on @handle2.com!
          👉 https://handle2.com/aq7MJJ2..."
str_squish(string = post)


## Ex. 16

e16 <- e14 %>%
  mutate(
    text_clean = text %>%
      str_remove_all(pattern = mention_bluesky) %>%
      str_remove_all(pattern = url_bluesky) %>%
      str_remove_all(pattern = tag_bluesky) %>%
      str_squish() # remove extra white spaces
  )

e16$text_clean[1:10]


## Change lower case
str_to_lower(
  string,
  locale = "en"
)


## Ex. 17
e17 <- e16 %>%
  mutate(
    text_low = str_to_lower(text)) %>%
    filter(str_detect(text_low, "political science|economics|sociology")) %>%
    mutate(n_matches = str_count(text_low, "economics|political science|sociology")) %>%
    filter(n_matches >= 1) %>%
    mutate(topic = case_when(
      str_detect(text_low, "economics") ~ "econ",
      str_detect(text_low, "political science") ~ "pols",
      str_detect(text_low, "sociology") ~ "soc")
    ) %>%
  # drop temp variables
  select(-text_low, -n_matches)


e17$text_clean_lower[1:10]


## Ex. 18
e18 <- readRDS("data/raw/profiles_get.rds") %>%
  mutate(
    desc_low = str_to_lower(description)) %>%
  filter(str_detect(desc_low, "political science|economics|sociology")) %>%
  mutate(n_matches = str_count(desc_low, "economics|political science|sociology")) %>%
  filter(n_matches >= 1) %>%
  mutate(topic = case_when(
    str_detect(desc_low, "economics") ~ "econ",
    str_detect(desc_low, "political science") ~ "pols",
    str_detect(desc_low, "sociology") ~ "soc")
  ) %>%
  # drop temp variables
  select(-desc_low, -n_matches)
e18$description[1:10]  


## Ex. 19

e19 <- e17 %>%
  mutate(
    createdAt = as.Date(createdAt),
    indexed_at = as.Date(indexed_at),
    author_createdAt = as.Date(author_createdAt),
    reply_count = as.numeric(reply_count),
    repost_count = as.numeric(repost_count),
    like_count = as.numeric(like_count),
    quote_count = as.numeric(quote_count)
  )
glimpse(e19)  
saveRDS(object = e19, file = "data/raw/posts.rds.rds")

## Ex. 20

e20 <- e18 %>%
  mutate(
    created_at = as.Date(created_at),
    indexed_at = as.Date(indexed_at),
    followers_count = as.numeric(followers_count),
    follows_count = as.numeric(follows_count),
    posts_count = as.numeric(posts_count)
  )
glimpse(e20)
saveRDS(object = e20, file = "data/raw/profiles.rds")


## Ex. 21

feeds <- readRDS("data/raw/feeds.rds")

e21 <- feeds %>%
  select(from_profile = profile_did, to_profile = author_did) %>%
  filter(from_profile != to_profile) %>%
  filter(to_profile %in% feeds$profile_did) %>%
  group_by(from_profile, to_profile) %>% # aggregate
  summarise(repost_count = n(), .groups = "drop")
head(e21)
saveRDS(object = e21, file = "data/raw/reposts.rds")

## Ex. 22

follows <- readRDS("data/raw/follows.rds")
#rm(e22)
e22 <- follows %>%
  select(from_profile = did, to_profile = subject_did) %>%
  filter(from_profile != to_profile) %>%
  filter(to_profile %in% follows$subject_did)
head(e22)
saveRDS(object = e22, file = "data/raw/followers.rds")


## Ex. 23

e23 <- e19 %>%
  unnest_tokens(output = word, input = text_clean, token = "words", drop = FALSE) %>%
  anti_join(stop_words, by = "word")
head(e23)
saveRDS(object = e23, file = "data/raw/posts_words.rds")


## End of Cleaning Script


# Day 3 -------------------------------------------------------------------

## Data Analysis

profiles <- readRDS("data/raw/profiles.rds")

ggplot(data = profiles, aes(x = created_at)) +
  geom_density()

ggplot(data = profiles, aes(x = created_at, y = followers_count)) +
  geom_point() +
  geom_smooth(method = "lm")

m1 <- lm(followers_count ~ created_at, data = profiles)
summary(m1)


## Ex. 24
ggplot(data = profiles, aes(x = follows_count, y = followers_count)) +
  geom_point() +
  geom_smooth(method = "lm")

## Ex. 25
m2 <- lm(followers_count ~ follows_count, data = profiles)
summary(m2)

m3 <- lm(followers_count ~ created_at + follows_count, data = profiles)
summary(m3)

m4 <- lm(followers_count ~ created_at + follows_count + posts_count, data = profiles)
summary(m4)


## Ex. 26
posts_words <- readRDS("data/raw/posts_words.rds")

dictionary_afinn <- lexicon_afinn()

sentiment_afinn <- e23 %>%
  left_join(dictionary_afinn, by = "word") %>%
  group_by(uri, text) %>%
  summarise(sentiment_afinn = sum(value, na.rm = TRUE), .groups = "drop")
head(sentiment_afinn)  

dictionary_bing <- lexicon_bing()

sentiment_bing <- e23 %>%
  left_join(dictionary_bing, by = "word") %>%
  group_by(uri, text) %>%
  summarise(positive_words = sum(sentiment == "positive", na.rm = TRUE), 
            negative_words = sum(sentiment == "negative", na.rm = TRUE),
            sentiment_bing = positive_words - negative_words,
            .groups = "drop")

dictionary_nrc <- lexicon_nrc_eil() %>%
  pivot_wider(names_from = AffectDimension, values_from = score)

### from here - error message!!!
sentiment_nrc <- e23 %>%
  left_join(dictionary_nrc, by = c("word", "term")) %>%
  group_by(uri, text) %>%
  summarise(nrc_fear = sum(fear, na.rm = TRUE),
            nrc_anger = sum(anger, na.rm = TRUE),
            nrc_sadness = sum(sadness, na.rm = TRUE),
            nrc_joy = sum(joy, na.rm = TRUE),
            .groups = "drop")

## Ex. 27

m5 <- lm(like_count ~ sentiment_afinn, data = sentiment_afinn %>%
           left_join(e19 %>% select(uri, like_count), by = "uri"))