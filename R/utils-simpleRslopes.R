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
    
    # Variable types
    ## Variable 1
    variable1type <- variable_type(data, variable1name)
    ## Variable 2
    variable2type <- variable_type(data, variable2name)
    
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
    
  }else{
    stop("Object 'fit' not of class 'lm'. Other regression classes are not yet supported.")
  }
  
  # Set up output list
  output$df <- degrees_of_freedom
  output$variable1type <- variable1type
  output$variable2type <- variable2type
  output$betas <- beta
  output$interactionName <- interactionName
  output$varCov <- vCov
  output$data <- data
  
  # Return output
  return(output)
  
}

#' @noRd
#' 
# Variable type
# Updated 05.02.2022
variable_type <- function(data, name)
{
  # Identify column in data
  variable_column <- unlist(
    lapply(colnames(data), function(x){
      grepl(x, name)
    })
  )
  
  # Obtain variable
  variable_values <- data[,variable_column]
  
  # Number of categories
  num_categories <- length(unique(variable_values))
  
  # Variable type
  type <- ifelse(num_categories > 2, "continuous", "dichotomous")
  
  # Return type
  return(type)
  
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

#%%%%%%%%%%%%%%%%%%%%%%%%%%
# plot.simpleRslopes.R ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @noRd
#' 
# Predict simple slopes values
# Updated 05.02.2022
simple_predict <- function(parameters, df)
{
  # Obtain coefficients
  coefs <- parameters$betas
  
  # Interaction name
  interactionName <- parameters$interactionName
  
  # Compute equation
  values <-lapply(1:nrow(df), function(i){
    
    # Get row
    row <- df[i,]
    
    # Intercept
    intercept <- coefs["(Intercept)"]
    
    # Main effects
    main <- sum(coefs[names(row)] * row)
    
    # Interaction
    interaction <- coefs[interactionName] * prod(row)
    
    # Prediction
    return(intercept + main + interaction)
    
  })
  
  # Make vector
  values <- unlist(values)
  
  # Attach to data frame
  df$y <- values
  
  # Return updated data frame
  return(df)
  
}

#' @noRd
#' 
# Prepare ggplot
# Updated 05.02.2022
prepare_ggplot <- function(
  df, variablename, moderatorname,
  plot_args
)
{
  
  # Make factors
  df[[moderatorname]] <- as.factor(df[[moderatorname]])
  
  # Check for dichotomouos
  if(plot_args[[variablename]]$type == "dichotomous"){
    df[[variablename]] <- as.factor(df[[variablename]])
  }
  
  # Make plot
  mod_plot <- ggplot2::ggplot(
    data = df, ggplot2::aes_string(
      x = variablename, y = "y",
      color = moderatorname, group = moderatorname
    )
  ) +
    ggplot2::geom_line(stat = "identity", size = 1.5) +
    ggplot2::labs(
      x = variablename, y = "Predicted Value",
      title = "Simple Slopes",
      subtitle = paste(
        moderatorname, "moderating", variablename
      )
    ) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 18, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 16, hjust = 0.5),
      axis.line = ggplot2::element_line(size = 1),
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold"),
      legend.text = ggplot2::element_text(size = 10, hjust = 0.5),
      legend.key = ggplot2::element_blank()
    )
  
  # Check for variable type
  if(plot_args[[variablename]]$type == "continuous"){
    
    # Check for defaults
    if(isTRUE(plot_args[[variablename]]$default)){
      
      # Change plot to include standard deviations
      mod_plot <- mod_plot + ggplot2::scale_x_continuous(
        breaks = c(
          range(df[[variablename]])[1],
          range(df[[variablename]])[2]
        ),
        labels = c("-1 SD", "+1 SD"),
        limits = c(
          (range(df[[variablename]])[1] - .5),
          (range(df[[variablename]])[2] + .5)
        )
      )
      
    }else{
      
      #  Change scale of x-axis
      mod_plot <- mod_plot + ggplot2::scale_x_continuous(
        n.breaks = 5
      )
      
    }
    
  }
  
  return(mod_plot)
  
}

