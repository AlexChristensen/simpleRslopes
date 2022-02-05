# Plot for `simple_slopes` ----
# Updated 05.02.2022
plot.simpleRslopes <- function(x, one_plot = TRUE, ...)
{
  # Variables
  variablenames <- unique(x$results$Variable)
  
  # Moderates
  moderatornames <- unique(x$results$Moderator)
  
  # Initialize plot list
  plot_list <- list()
  
  # Create data frames for moderators
  for(i in seq_along(moderatornames)){
    
    # Moderator values
    moderator_values <- x$results$ModeratorValue[
        x$results$Moderator == moderatornames[i]
      ]
    
    # Variable values
    variable_values <- rep(
      range(
        x$results$ModeratorValue[
          x$results$Moderator == variablenames[i]
        ]
      ), each = length(moderator_values)
    )
    
    # Create data frame
    df <- data.frame(
      moderator_values,
      variable_values
    )
    
    # Rename
    colnames(df) <- c(moderatornames[i], variablenames[i])
    
    # Obtain y values
    df <- simple_predict(x$parameters, df)
    
    # Produce plots
    plot_list[[moderatornames[i]]] <- prepare_ggplot(
      df, variablenames[i], moderatornames[i],
      x$plot_args
    )
    
  }
  
  # One plot or multiple?
  if(isTRUE(one_plot)){ # One
    patchwork::wrap_plots(
      plot_list,
      ncol = length(plot_list)
    )
  }else{ # Multiple
    return(plot_list)
  }
  
}

# Print for `simple_slopes` ----
# Updated 05.02.2022
print.simpleRslopes <- function(x, ...)
{
  # Results
  results <- x$results
  
  print(results, row.names = FALSE)
  cat("---\n")
  cat("Signif. code: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 'n.s.' 1")
  
}
