#' Simple Slopes Analysis
#' 
#' @description Computes simple slopes for objects fit with
#' the class \code{"lm"}
#' 
#' @param fit \code{"lm"} object.
#' Object from a model fit using a regression model
#' 
#' @param variable1name Character.
#' Name of the first variable in interaction from \code{fit}
#' 
#' @param variable1type Character.
#' Category type of the first variable in interaction from \code{fit}
#' 
#' @param variable1values Numeric.
#' Values to be used when first variable is the moderator.
#' If \code{variable1type = "dichotomous"}, then
#' defaults to values 0 and 1.
#' If \code{variable1type = "continuous"}, then
#' defaults to values -1 SD, M, and +1 SD
#' 
#' @param variable2name Character.
#' Name of the second variable in interaction from \code{fit}
#' 
#' @param variable2type Character.
#' Category type of the second variable in interaction from \code{fit}
#' 
#' @param variable2values Numeric.
#' Values to be used when second variable is the moderator.
#' If \code{variable2type = "dichotomous"}, then
#' defaults to values 0 and 1.
#' If \code{variable2type = "continuous"}, then
#' defaults to values -1 SD, M, and +1 SD
#' 
#' @return Returns a list containing:
#' 
#' \item{results}{Data frame with variable, moderator, values of moderator
#' betas of simple slopes, \emph{t} values, \emph{p}-values, and significance level}
#' 
#' \item{parameters}{A list containing degrees of freedom (\code{df}),
#' betas (from \code{fit}), interaction name, variance-covariance matrix
#' of the variables involved, and data from \code{fit}}
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
#'   variable1type = "continuous",
#'   variable2name = "x2", # must be name as it appears in 'fit' object
#'   variable2type = "continuous"
#' )
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom stats pt rnorm
#' 
#' @export
#' 
# Simple Slopes function
# Updated 05.02.2022
simple_slopes <- function(
  fit,
  variable1name,
  variable1type = c("dichotomous", "continuous"),
  variable1values = NULL,
  variable2name,
  variable2type = c("dichotomous", "continuous"),
  variable2values = NULL
)
{
  # Check for names
  if(missing(variable1name) | missing(variable2name)){
    if(missing(variable1name)){
      stop("Variable name is missing for variable 1: 'variable1name'")
    }else if(missing(variable2name)){
      stop("Variable name is missing for variable 2: 'variable2name'")
    }
  }
  
  # Check for types
  if(missing(variable1type) | missing(variable2type)){
    if(missing(variable1type)){
      stop("Variable type is missing for variable 1: 'variable1type'")
    }else if(missing(variable2type)){
      stop("Variable type is missing for variable 2: 'variable2type'")
    }
  }else{
    # Match arguments
    variable1type <- match.arg(variable1type)
    variable2type <- match.arg(variable2type)
  }
  
  # Obtain regression parameters and variance-covariance matrix
  parameters <- regression_parameters(
    fit,
    variable1name,
    variable2name
  )
  
  # Check for values
  if(variable1type == "continuous"){
    
    # Check if values is null
    if(is.null(variable1values)){
      
      # Message user
      message("Values for variable 1 were not set using 'variable1values'. Using default of values -1 SD, M, and +1 SD")
      
      # Obtain values
      variable1values <- obtain_values(
        name = variable1name,
        parameters = parameters
      )
    }else{
      # Sort values
      variable1values <- sort(variable1values)
    }
    
  }else if(variable1type == "dichotomous"){
    
    # Check if values is null
    if(is.null(variable1values)){
      
      # Message user
      message("Values for variable 1 were not set using 'variable1values'. Using default of values 0 and 1")
      
      # Obtain values
      variable1values <- c(0, 1)
      
    }else{
      # Sort values
      variable1values <- sort(variable1values)
    }
    
  }
  
  # Check for values
  if(variable2type == "continuous"){
    
    # Check if values is null
    if(is.null(variable2values)){
      
      # Message user
      message("Values for variable 2 were not set using 'variable2values'. Using default of values -1 SD, M, and +1 SD")
      
      # Obtain values
      variable2values <- obtain_values(
        name = variable2name,
        parameters = parameters
      )
    }else{# Sort values
      variable2values <- sort(variable2values)
    }
    
  }else if(variable2type == "dichotomous"){
    
    # Check if values is null
    if(is.null(variable2values)){
      
      # Message user
      message("Values for variable 2 were not set using 'variable2values'. Using default of values 0 and 1")
      
      # Obtain values
      variable2values <- c(0, 1)
      
    }else{
      # Sort values
      variable2values <- sort(variable2values)
    }
    
  }
  
  # Compute simple slopes
  ## Variable 1
  variable1slopes <- parameters$betas[variable1name] + 
    parameters$betas[parameters$interactionName] * variable2values
  
  ## Variable 2
  variable2slopes <- parameters$betas[variable2name] + 
    parameters$betas[parameters$interactionName] * variable1values
  
  # Compute variances
  ## Variable 1
  variable1variances <- diag(parameters$varCov)[variable1name] +
    2 * variable2values * parameters$varCov[variable1name, parameters$interactionName] +
    variable2values^2 * diag(parameters$varCov)[parameters$interactionName]
  ## Variable 2
  variable2variances <- diag(parameters$varCov)[variable2name] +
    2 * variable1values * parameters$varCov[variable2name, parameters$interactionName] +
    variable1values^2 * diag(parameters$varCov)[parameters$interactionName]
  
  # Compute t-values
  ## Variable 1
  variable1ts <- variable1slopes / sqrt(variable1variances)
  ## Variable 2
  variable2ts <- variable2slopes / sqrt(variable2variances)
  
  # Compute p-values
  ## Variable 1
  variable1ps <- pt(
    variable1ts, df = parameters$df
  )
  ## Variable 2
  variable2ps <- pt(
    variable2ts, df = parameters$df
  )
  
  # Set up results data frame
  results_df <- data.frame(
    Variable = c(
      rep(variable1name, length(variable2values)),
      rep(variable2name, length(variable1values))
    ),
    Moderator = c(
      rep(variable2name, length(variable2values)),
      rep(variable1name, length(variable1values))
    ),
    ModeratorValue = round(
      c(
        variable2values,
        variable1values
      ), 5
    ),
    SimpleSlope = round(
      c(
        variable1slopes,
        variable2slopes
      ), 5
    ),
    t = round(
      c(
        variable1ts,
        variable2ts
      ),
      5
    ),
    p = round(
      c(
        variable1ps,
        variable2ps
      ),
      5
    )
  )
  
  # Add significance
  sig <- character(length = nrow(results_df))
  sig <- ifelse(results_df$p > .05, "n.s.", sig)
  sig <- ifelse(results_df$p <= .10 , ".", sig)
  sig <- ifelse(results_df$p <= .05 , "*", sig)
  sig <- ifelse(results_df$p <= .01 , "**", sig)
  sig <- ifelse(results_df$p <= .001 , "< .001", sig)
  results_df$sig <- sig
  
  # Set up results list
  results_list <- list()
  results_list$results <- results_df
  results_list$parameters <- parameters
  
  # Return results
  return(results_list)
  
}
