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
#' @param variable2values Numeric.
#' Values to be used when second variable is the moderator.
#' If \code{variable2type = "dichotomous"}, then
#' defaults to values 0 and 1.
#' If \code{variable2type = "continuous"}, then
#' defaults to values -1 SD, M, and +1 SD
#' 
#' @param plot_slopes Boolean.
#' Should simple slopes be plotted?
#' Defaults to \code{TRUE}
#' 
#' @return Returns a list containing:
#' 
#' \item{fit}{Returns \code{fit} object back}
#' 
#' \item{results}{Data frame with variable, moderator, values of moderator
#' betas of simple slopes, \emph{t} values, \emph{p}-values, and significance level}
#' 
#' \item{parameters}{A list containing degrees of freedom (\code{df}),
#' betas (from \code{fit}), interaction name, variance-covariance matrix
#' of the variables involved, and data from \code{fit}}
#' 
#' \item{plot_args}{Internal use only. Arguments for plots}
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
#'   plot = TRUE
#' )
#' 
#' @references
#' Preacher, K. J., Curran, P. J., & Bauer, D. J. (2006).
#' Computational tools for probing interactions in multiple linear regression, multilevel modeling, and latent curve analysis.
#' \emph{Journal of Educational and Behavioral Statistics}, \emph{31}(4), 437-448.
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
  variable1values = NULL,
  variable2name,
  variable2values = NULL,
  plot_slopes = TRUE
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
  
  # Obtain regression parameters and variance-covariance matrix
  parameters <- regression_parameters(
    fit,
    variable1name,
    variable2name
  )
  
  # Set up variable for plots
  plot_args <- vector(mode = "list", length = 2)
  names(plot_args) <- c(variable1name, variable2name)
  
  # Check for values
  if(parameters$variable1type == "continuous"){
    
    # Check if values is null
    if(is.null(variable1values)){
      
      # Message user
      message("Values for variable 1 were not set using 'variable1values'. Using default continuous values: -1 SD, M, and +1 SD")
      
      # Obtain values
      variable1values <- obtain_values(
        name = variable1name,
        parameters = parameters
      )
      
      # Variable 1 defaults
      plot_args[[variable1name]]$default <- TRUE
      
    }else{
      
      # Sort values
      variable1values <- sort(variable1values)
      
      # Variable 1 defaults
      plot_args[[variable1name]]$default <- FALSE

    }
    
  }else if(parameters$variable1type == "dichotomous"){
    
    # Check if values is null
    if(is.null(variable1values)){
      
      # Message user
      message("Values for variable 1 were not set using 'variable1values'. Using default dichotomous values: 0 and 1")
      
      # Obtain values
      variable1values <- c(0, 1)
      
      # Variable 1 defaults
      plot_args[[variable1name]]$default <- TRUE
      
    }else{
      
      # Sort values
      variable1values <- sort(variable1values)
      
      # Variable 1 defaults
      plot_args[[variable1name]]$default <- FALSE
      
    }
    
  }
  
  # Variable 1 type
  plot_args[[variable1name]]$type <- parameters$variable1type
  
  # Check for values
  if(parameters$variable2type == "continuous"){
    
    # Check if values is null
    if(is.null(variable2values)){
      
      # Message user
      message("Values for variable 2 were not set using 'variable2values'. Using default continuous values: -1 SD, M, and +1 SD")
      
      # Obtain values
      variable2values <- obtain_values(
        name = variable2name,
        parameters = parameters
      )
      
      # Variable 2 defaults
      plot_args[[variable2name]]$default <- TRUE
      
    }else{
      
      # Sort values
      variable2values <- sort(variable2values)
      
      # Variable 2 defaults
      plot_args[[variable2name]]$default <- FALSE
      
    }
    
  }else if(parameters$variable2type == "dichotomous"){
    
    # Check if values is null
    if(is.null(variable2values)){
      
      # Message user
      message("Values for variable 2 were not set using 'variable2values'. Using default dichotomous values: 0 and 1")
      
      # Obtain values
      variable2values <- c(0, 1)
      
      # Variable 2 defaults
      plot_args[[variable2name]]$default <- TRUE
      
    }else{
      
      # Sort values
      variable2values <- sort(variable2values)
      
      # Variable 2 defaults
      plot_args[[variable2name]]$default <- FALSE
      
    }
    
  }
  
  # Variable 2 type
  plot_args[[variable2name]]$type <- parameters$variable2type
  
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
  variable1ps <- (1 - pt(abs(variable1ts), df = parameters$df)) * 2
  ## Variable 2
  variable2ps <- (1 - pt(abs(variable2ts), df = parameters$df)) * 2
  
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
  sig <- ifelse(results_df$p <= .001 , "***", sig)
  results_df$sig <- sig
  
  # Set up results list
  results_list <- list()
  results_list$fit <- fit
  results_list$results <- results_df
  results_list$parameters <- parameters
  results_list$plot_args <- plot_args
  
  # Class
  class(results_list) <- "simpleRslopes"
  
  # Obtain plots
  results_list$plots <- plot(results_list, one_plot = FALSE)
  
  # Check printing plots
  if(isTRUE(plot_slopes)){
    plot(results_list)
  }
  
  # Return results
  return(results_list)
  
}
