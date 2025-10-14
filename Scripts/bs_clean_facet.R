
# function to create a tidy dataset from the facets column
bs_clean_facet <- function(data) {
  
  # validation
  if (!is.data.frame(data) || 
      !all(c("uri", "record") %in% names(data)) ||
      !any(c("author_did", "author") %in% names(data))) {
    stop("Input must be a raw data frame returned by either bs_search_post() or bs_get_post().", call. = FALSE)
  }
  
  # helper function for null coalescing
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a
  
  # process for based on existing dataframe
  if ("author_did" %in% names(data)) {
    
    # bs_get_post format - author_did already exists
    data <- data |>
      select(uri, author_did, record) |>
      unnest_wider(record)
    
  } else if ("author" %in% names(data)) {
    
    # bs_search_post format - need to extract author_did from author
    data <- data |>
      hoist(record, author_did = "did") |>
      select(uri, author_did, record) |>
      unnest_wider(record)
    
  }
  
  # create a helper function to safely extract nested data
  safe_extract <- function(x, path) {
    tryCatch({
      if (is.null(x) || length(x) == 0) return(NA)
      result <- x
      for (p in path) {
        if (is.list(result) && p %in% names(result)) {
          result <- result[[p]]
        } else {
          return(NA)
        }
      }
      return(result)
    }, error = function(e) NA)
  }
  
  # function to process facets
  process_facets <- function(facets_list, post_id) {
    if (is.null(facets_list) || length(facets_list) == 0) {
      return(data.frame(
        post_id = post_id,
        facet_type = NA,
        facet_subtype = NA,
        byte_start = NA,
        byte_end = NA,
        stringsAsFactors = FALSE
      ))
    }
    
    results <- list()
    
    for (i in seq_along(facets_list)) {
      facet <- facets_list[[i]]
      if (is.null(facet) || length(facet) == 0) next
      
      # Extract byte positions
      byte_start <- safe_extract(facet, c("index", "byteStart")) %||% NA
      byte_end <- safe_extract(facet, c("index", "byteEnd")) %||% NA
      
      # Process features (can be multiple per facet)
      features <- facet$features
      if (!is.null(features) && length(features) > 0) {
        for (j in seq_along(features)) {
          feature <- features[[j]]
          feature_type <- safe_extract(feature, "$type") %||% "unknown"
          
          if (grepl("mention", feature_type, ignore.case = TRUE)) {
            # Handle mentions
            results[[length(results) + 1]] <- data.frame(
              post_id = post_id,
              facet_type = "mention",
              facet_subtype = "user_mention",
              byte_start = byte_start,
              byte_end = byte_end,
              mention_did = safe_extract(feature, "did") %||% NA,
              stringsAsFactors = FALSE
            )
            
          } else if (grepl("link", feature_type, ignore.case = TRUE)) {
            # Handle links
            results[[length(results) + 1]] <- data.frame(
              post_id = post_id,
              facet_type = "link",
              facet_subtype = "url",
              byte_start = byte_start,
              byte_end = byte_end,
              link_uri = safe_extract(feature, "uri") %||% NA,
              stringsAsFactors = FALSE
            )
            
          } else if (grepl("tag", feature_type, ignore.case = TRUE)) {
            # Handle hashtags
            results[[length(results) + 1]] <- data.frame(
              post_id = post_id,
              facet_type = "tag",
              facet_subtype = "hashtag",
              byte_start = byte_start,
              byte_end = byte_end,
              tag_value = safe_extract(feature, "tag") %||% NA,
              stringsAsFactors = FALSE
            )
            
          } else {
            # Handle unknown feature types
            results[[length(results) + 1]] <- data.frame(
              post_id = post_id,
              facet_type = "unknown",
              facet_subtype = feature_type,
              byte_start = byte_start,
              byte_end = byte_end,
              raw_feature = list(feature),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
    
    if (length(results) == 0) {
      return(data.frame(
        post_id = post_id,
        facet_type = NA,
        facet_subtype = NA,
        byte_start = NA,
        byte_end = NA,
        stringsAsFactors = FALSE
      ))
    }
    
    # combine all results and ensure consistent columns
    all_results <- bind_rows(results)
    return(all_results)
  }
  
  # process each post's facets data
  tidy_facets <- map2_dfr(data$facets, seq_along(data$facets), ~{
    if (is.null(.x) || length(.x) == 0) {
      return(data.frame(
        post_id = .y,
        facet_type = NA,
        facet_subtype = NA,
        byte_start = NA,
        byte_end = NA,
        stringsAsFactors = FALSE
      ))
    }
    process_facets(.x, .y)
  })
  
  # add original post metadata
  post_metadata <- data %>%
    select(-facets) %>%
    mutate(post_id = row_number())
  
  # join with facets data and clean up
  final_data <- tidy_facets %>%
    left_join(post_metadata, by = "post_id") %>%
    select(-post_id, -author_did) %>%
    select(uri, facet_type:byte_end, everything())
  
  return(final_data)
}