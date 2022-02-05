#' S3 Plot Method for \code{\link{simpleRslopes}} Objects
#' 
#' @description Plot method for \code{\link[simpleRslopes]{simple_slopes}}
#' 
#' @param x \code{\link{simpleRslopes}} object
#' 
#' @param one_plot Boolean.
#' Should one plot be printed?
#' Defaults to \code{TRUE}
#' 
#' @param ... Additional arguments.
#' No additional arguments available at this time
#' 
#' @return Returns a list containing each plot for the simple slopes
#' 
#' @examples
#' # Generate data
#' df <- data.frame(
#'   y = rnorm(100),
#'   x1 = rnorm(100),
#'   x2 = rnorm(100)
#' )
#' 
#' # Estimate linear model
#' fit <- lm(y ~ x1 * x2, data = df)
#' 
#' # Estimate simple slopes
#' result <- simple_slopes(
#'   fit = fit,
#'   variable1name = "x1", # must be name as it appears in 'fit' object
#'   variable2name = "x2", # must be name as it appears in 'fit' object
#'   plot = FALSE # only FALSE for CRAN checks
#' )
#' 
#' # Plot simple slopes
#' plot(result)
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#' 
# Plot for `simple_slopes`
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