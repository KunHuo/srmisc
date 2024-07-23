#' Set Global Parameters for Analysis
#'
#' This function sets global parameters for an analysis by storing them in the
#' global options. These parameters include data, outcome, time, exposure, covariates,
#' and strata.
#'
#' @param data A data frame containing the dataset to be used in the analysis.
#' @param outcome A character vector specifying the outcome variable(s) in the dataset.
#' @param time A character vector specifying the time variable(s) in the dataset.
#' @param exposure A character vector specifying the exposure variable(s) in the dataset.
#' @param covariates A character vector specifying the covariate variable(s) in the dataset.
#' @param strata A character vector specifying the strata variable(s) in the dataset.
#'
#' @return This function does not return a value. It sets global options.
#'
#' @export
global_parameters <- function(data, outcome, time, exposure, covariates, strata){

  if(!missing(data)){
    options(global.data = data)
  }

  if(!missing(outcome)){
    options(global.outcome = outcome)
  }

  if(!missing(time)){
    options(global.time = time)
  }

  if(!missing(exposure)){
    options(global.exposure = exposure)
  }

  if(!missing(covariates)){
    options(global.covariates = covariates)
  }

  if(!missing(strata)){
    options(global.strata = strata)
  }
}
