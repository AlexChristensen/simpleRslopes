#%%%%%%%%%%%%%%%%%%%%%
# simple_slopes.R ----
#%%%%%%%%%%%%%%%%%%%%%

#' @noRd
#' @importFrom utils packageDescription
#' @importFrom stats vcov
#' 
# Regression parameters
# Updated 05.02.2022
regression_parameters <- function(
  fit,
  variable1name,
  variable2name
)
{
  
  # Obtain class
  fit_class <- class(fit)
  
  # Obtain summary
  fit_summ <- summary(fit)
  
  # Initialize output list
  output <- list()
  
  # Obtain parameters based on class
  ## lm
  if(any(fit_class == "lm")){
    
    # Data
    data <- fit$model
    
    # Betas
    betas <- fit_summ$coefficients[,"Estimate"]
    
    # Ensure names
    names(betas) <- row.names(fit_summ$coefficients)
    
    # Degrees of freedom
    degrees_of_freedom <- fit$df.residual
    
    # Obtain interaction name
    interactionName <- interaction_check(betas, variable1name, variable2name)
    
    # Betas of interest
    beta <- betas[c(
      "(Intercept)", # intercept
      variable1name, # variable 1
      variable2name, # variable 2
      interactionName # interaction
    )]
    
    # Variance-covariance
    varCov <- vcov(fit)
    
    # Ensure proper names
    row.names(varCov) <- names(betas)
    colnames(varCov) <- names(betas)
    
    # Variance-covariance of interest
    vCov <- varCov[
      c(
        "(Intercept)", # intercept
        variable1name, # variable 1
        variable2name, # variable 2
        interactionName # interaction
      ),
      c(
        "(Intercept)", # intercept
        variable1name, # variable 1
        variable2name, # variable 2
        interactionName # interaction
      )
    ]
    
  }
  
  # Set up output list
  output$df <- degrees_of_freedom
  output$betas <- beta
  output$interactionName <- interactionName
  output$varCov <- vCov
  output$data <- data
  
  # Return output
  return(output)
  
}

#' @noRd
#' 
# Interaction setup
# Updated 05.02.2022
interaction_check <- function(betas, variable1name, variable2name)
{
  # Check for interaction
  if(all(!grepl(":", names(betas)))){
    stop("No interactions detected in the fitted model.")
  }
  
  # Obtain interaction name
  if(paste(variable1name, ":", variable2name, sep = "") %in% names(betas)){
    interactionName <- paste(variable1name, ":", variable2name, sep = "")
  }else if(paste(variable2name, ":", variable1name, sep = "") %in% names(betas)){
    interactionName <- paste(variable2name, ":", variable1name, sep = "")
  }else{
    stop(
      paste(
        "Interaction between ", variable1name,
        " and ", variable2name,
        " not found in the fitted model. ", "\n",
        "Make sure the interaction term '",
        paste(variable1name, ":", variable2name, sep = ""),
        "' is included in your model: `summary(fit)`",
        sep = ""
      )
    )
  }
  
  # Return name of interaction
  return(interactionName)
  
}

#' @noRd
#' @importFrom stats sd
#' 
# Obtain variable values
# Updated 05.02.2022
obtain_values <- function(name, parameters)
{
  # Descriptives
  average <- mean(parameters$data[,name], na.rm = TRUE)
  stdev <- sd(parameters$data[,name], na.rm = TRUE)
  
  # Obtain values
  values <- c(
    average - stdev, # -1 SD
    average, # M
    average + stdev # +1 SD
  )
  
  # Return values
  return(values)
  
}
