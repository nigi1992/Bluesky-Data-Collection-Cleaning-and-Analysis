
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