#' S3 Print Method for \code{\link{simpleRslopes}} Objects
#' 
#' @description Print method for \code{\link[simpleRslopes]{simple_slopes}}
#' 
#' @param x \code{\link{simpleRslopes}} object
#' 
#' @param ... Additional arguments.
#' No additional arguments available at this time
#' 
#' @return Returns a results for the simple slopes
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
#' simple_slopes(
#'   fit = fit,
#'   variable1name = "x1", # must be name as it appears in 'fit' object
#'   variable2name = "x2", # must be name as it appears in 'fit' object
#'   plot = FALSE # only FALSE for CRAN checks
#' )
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#' 
# Print for `simple_slopes`
# Updated 05.02.2022
print.simpleRslopes <- function(x, ...)
{
  # Results
  results <- x$results
  
  print(results, row.names = FALSE)
  cat("---\n")
  cat("Signif. code: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 'n.s.' 1")
  
}