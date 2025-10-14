
# function to create a tidy dataset from the embed column
bs_clean_embed <- function(data) {

  # validation
  if (!is.data.frame(data) || 
      !all(c("uri", "record") %in% names(data)) ||
      !any(c("author_did", "author") %in% names(data))) {
    stop("Input must be a raw data frame returned by either bs_search_post() or bs_get_post().", call. = FALSE)
  }
  
  # helper function for null coalescing
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a
  
  # helper function to convert search embed format to record embed format
  convert_search_embed <- function(embed) {
    
    if (is.null(embed) || length(embed) == 0) {
      return(NULL)
    }
    
    # list of fields that should remain as arrays (don't unwrap these)
    preserve_arrays <- c("images", "videos", "facets")
    
    # recursively unwrap single-element arrays, but preserve certain arrays
    unwrap_arrays <- function(obj, field_name = "") {
      if (is.list(obj)) {
        # If this field should be preserved as an array, don't unwrap
        if (field_name %in% preserve_arrays) {
          # Still process the contents, but don't unwrap the array itself
          return(lapply(obj, function(x) unwrap_arrays(x, "")))
        }
        
        # If it's a single-element unnamed list, unwrap it
        if (length(obj) == 1 && (is.null(names(obj)) || names(obj)[1] == "")) {
          return(unwrap_arrays(obj[[1]], field_name))
        } else {
          # Otherwise, process each element
          result <- list()
          for (name in names(obj)) {
            result[[name]] <- unwrap_arrays(obj[[name]], name)
          }
          return(result)
        }
      } else {
        return(obj)
      }
    }
    
    # apply unwrapping to the entire embed structure
    unwrapped_embed <- unwrap_arrays(embed)
    
    return(unwrapped_embed)
  }
  
  
  fix_search_embeds <- function(search_df) {
    
    # Create a copy to modify
    result <- search_df
    
    # Process each row
    for (i in seq_len(nrow(search_df))) {
      # Check if record exists and has embed
      if (!is.null(search_df$record[[i]]) && !is.null(search_df$record[[i]]$embed)) {
        # Apply conversion to the embed
        cleaned_embed <- convert_search_embed(search_df$record[[i]]$embed)
        
        # Wrap in a list to match post_get format (list containing embed objects)
        result$record[[i]]$embed <- list(cleaned_embed)
      }
    }
    
    return(result)
  }
  
  # process for based on existing dataframe
  if ("author_did" %in% names(data)) {
    
    # bs_get_post format - author_did already exists
    data <- data |>
      select(uri, author_did, record) |>
      unnest_wider(record)
    
  } else if ("author" %in% names(data)) {
    
    # bs_search_post format - need to extract author_did from author
    data <- fix_search_embeds(data) |>
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
  
  # function to process different embed types
  process_embed <- function(embed_list, post_id) {
    if (is.null(embed_list) || length(embed_list) == 0) {
      return(data.frame(
        post_id = post_id,
        embed_type = NA,
        embed_subtype = NA,
        content = NA,
        stringsAsFactors = FALSE
      ))
    }
    
    results <- list()
    
    for (i in seq_along(embed_list)) {
      embed <- embed_list[[i]]
      if (is.null(embed) || length(embed) == 0) next
      
      # Determine embed type
      embed_type <- safe_extract(embed, c("$type"))
      
      if (is.na(embed_type)) {
        # If no $type, try to infer from structure
        if ("external" %in% names(embed)) {
          embed_type <- "app.bsky.embed.external"
        } else if ("images" %in% names(embed)) {
          embed_type <- "app.bsky.embed.images"
        } else if ("video" %in% names(embed)) {
          embed_type <- "app.bsky.embed.video"
        } else if ("record" %in% names(embed)) {
          embed_type <- "app.bsky.embed.record"
        } else {
          embed_type <- "unknown"
        }
      }
      
      # process based on embed type
      if (grepl("external", embed_type, ignore.case = TRUE)) {
        
        # external links
        external <- embed$external
        if (!is.null(external)) {
          results[[length(results) + 1]] <- data.frame(
            post_id = post_id,
            embed_type = "external",
            embed_subtype = "link",
            external_title = safe_extract(external, "title") %||% NA,
            external_description = safe_extract(external, "description") %||% NA,
            external__url = safe_extract(external, "uri") %||% NA,
            external_thumb_ref = safe_extract(external, c("thumb", "ref", "$link")) %||% NA,
            external_thumb_mime = safe_extract(external, c("thumb", "mimeType")) %||% NA,
            external_thumb_size = safe_extract(external, c("thumb", "size")) %||% NA,
            stringsAsFactors = FALSE
          )
        }
      } else if (grepl("images", embed_type, ignore.case = TRUE)) {
        
        # images
        images <- embed$images
        if (!is.null(images) && length(images) > 0) {
          for (j in seq_along(images)) {
            img <- images[[j]]
            results[[length(results) + 1]] <- data.frame(
              post_id = post_id,
              embed_type = "images",
              embed_subtype = paste0("image_", j),
              image_alt_text = safe_extract(img, "alt") %||% NA,
              image_ref = safe_extract(img, c("image", "ref", "$link")) %||% NA,
              image_mime = safe_extract(img, c("image", "mimeType")) %||% NA,
              image_size = safe_extract(img, c("image", "size")) %||% NA,
              image_aspect_width = safe_extract(img, c("aspectRatio", "width")) %||% NA,
              image_aspect_height = safe_extract(img, c("aspectRatio", "height")) %||% NA,
              stringsAsFactors = FALSE
            )
          }
        }
      } else if (grepl("video", embed_type, ignore.case = TRUE)) {
        
        # videos
        video <- embed$video
        if (!is.null(video)) {
          results[[length(results) + 1]] <- data.frame(
            post_id = post_id,
            embed_type = "video",
            embed_subtype = "video",
            video_ref = safe_extract(video, c("ref", "$link")) %||% NA,
            video_video_mime = safe_extract(video, "mimeType") %||% NA,
            video_video_size = safe_extract(video, "size") %||% NA,
            video_aspect_width = safe_extract(embed, c("aspectRatio", "width")) %||% NA,
            video_aspect_height = safe_extract(embed, c("aspectRatio", "height")) %||% NA,
            stringsAsFactors = FALSE
          )
        }
      } else if (grepl("record", embed_type, ignore.case = TRUE)) {
        
        # records (quotes/reposts)
        record <- embed$record
        if (!is.null(record)) {
          results[[length(results) + 1]] <- data.frame(
            post_id = post_id,
            embed_type = "record",
            embed_subtype = "quote",
            record_cid = safe_extract(record, "cid") %||% NA,
            record_uri = safe_extract(record, "uri") %||% NA,
            stringsAsFactors = FALSE
          )
        }
      } else if (grepl("recordWithMedia", embed_type, ignore.case = TRUE)) {
        
        # record with media (quote with additional media)
        record <- embed$record
        media <- embed$media
        
        if (!is.null(record)) {
          results[[length(results) + 1]] <- data.frame(
            post_id = post_id,
            embed_type = "recordWithMedia",
            embed_subtype = "quote_with_media_record",
            quote_record_cid = safe_extract(record, c("record", "cid")) %||% NA,
            quote_record_uri = safe_extract(record, c("record", "uri")) %||% NA,
            stringsAsFactors = FALSE
          )
        }
        
        if (!is.null(media)) {
          
          # process the media part recursively
          media_results <- process_embed(list(media), post_id)
          media_results$embed_subtype <- paste0("quote_with_media_", media_results$embed_subtype)
          results <- c(results, list(media_results))
        }
      }
    }
    
    if (length(results) == 0) {
      
      return(data.frame(
        post_id = post_id,
        embed_type = NA,
        embed_subtype = NA,
        content = NA,
        stringsAsFactors = FALSE
      ))
    }
    
    # combine all results and ensure consistent columns
    all_results <- bind_rows(results)
    return(all_results)
  }
  
  # process each post's embed data
  tidy_embeds <- map2_dfr(data$embed, seq_along(data$embed), ~{
    if (is.null(.x) || length(.x) == 0) {
      return(data.frame(
        post_id = .y,
        embed_type = NA,
        embed_subtype = NA,
        stringsAsFactors = FALSE
      ))
    }
    process_embed(.x, .y)
  })
  
  # add original post metadata
  post_metadata <- data %>%
    select(-embed) %>%
    mutate(post_id = row_number())
  
  # join with embed data, create links, re-arrange
  final_data <- tidy_embeds %>%
    left_join(post_metadata, by = "post_id") %>%
    mutate(image_url = if_else(!is.na(image_ref),
      paste0("https://cdn.bsky.app/img/feed_fullsize/plain/", author_did, "/", image_ref, "@jpeg"),
      NA_character_),
      external_thumb_url = if_else(!is.na(external_thumb_ref),
                          paste0("https://cdn.bsky.app/img/feed_thumbnail/plain/", author_did, "/", external_thumb_ref, "@jpeg"),
                          NA_character_)) |>
    select(-post_id, -author_did) |>
    select(uri:external_thumb_url, everything()) |>
    relocate(image_url, .after = image_mime) |>
    relocate(external_thumb_url, .after = external_thumb_mime)
  
  return(final_data)
  
}