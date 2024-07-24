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


#' Set or Get Global Font Size
#'
#' This function sets or gets the global font size option.
#'
#' @param size An optional numeric value specifying the font size to set. If
#' not provided, the current global font size is returned.
#'
#' @return If `size` is provided, the function sets the global font size and
#' returns `NULL`. If `size` is not provided, the current global font size is
#' returned. The default value is 9 if no global font size has been set.
#'
#' @examples
#' global_fontsize(12)  # Set global font size to 12
#' current_size <- global_fontsize()  # Get current global font size
#'
#' @export
global_fontsize <- function(size){
  if(!missing(size)){
    options(global.fontsize = size)
  }else{
    res <- options("global.fontsize")[[1]]
    ifelse(is.null(res), 9, res)
  }
}


#' Set or Get Global Font Family
#'
#' This function sets or gets the global font family option.
#'
#' @param family An optional character string specifying the font family to
#' set. If not provided, the current global font family is returned.
#' sans and arial for "Arial", "serif" and "times" for Times New Roman.
#'
#' @return If `family` is provided, the function sets the global font family and
#' returns `NULL`. If `family` is not provided, the current global font family
#' is returned. The default value is "Times New Roman" if no global font family
#' has been set.
#'
#' @examples
#' global_fontfamily("sans")  # Set global font family to Arial
#' current_family <- global_fontfamily()  # Get current global font family
#'
#' @export
global_fontfamily <- function(family){
  if(!missing(family)){

    if(tolower(family) == "arial"){
      family <- "sans"
    }else if(tolower(family) == "times"){
      family <- "serif"
    }
    options(global.fontfamily = family)
  }else{
    res <- options("global.fontfamily")[[1]]
    ifelse(is.null(res), "serif", res)
  }
}


#' Set or Get Global Color Palette
#'
#' This function sets or gets the global color palette option. Predefined
#' palettes can be used by specifying their names.
#'
#' @param palette An optional character string or vector specifying the color
#' palette to set. Predefined palettes include "jama", "nejm", "lancet", and
#' "jco". If not provided, the current global color palette is returned.
#'
#' @return If `palette` is provided, the function sets the global color palette
#' and returns `NULL`. If `palette` is not provided, the current global color
#' palette is returned.
#'
#' @examples
#' global_palette("jama")  # Set global palette to JAMA colors
#' current_palette <- global_palette()  # Get current global color palette
#'
#' @export
global_palette <- function(palette){
  if(!missing(palette)){
    if(tolower(palette[1]) == "jama"){
      palette <- pal_jama_7()
    }else if(tolower(palette[1]) == "nejm"){
      palette <- pal_nejm_8()
    }else if(tolower(palette[1]) == "lancet"){
      palette <- pal_lancet_9()
    }else if(tolower(palette[1]) == "jco"){
      palette <- pal_jco_10()
    }
    options(global.palette = palette)
  }else{
    options("global.palette")[[1]]
  }
}

#' Set or Get the Global Folder Option
#'
#' This function sets a global folder path option or retrieves the current
#' global folder path option.
#'
#' If a folder path is provided, it sets the global option `global.folder` to
#' the specified folder path.
#' If no folder path is provided, it retrieves and returns the current value of
#' the `global.folder` option.
#'
#' @param folder A character string specifying the folder path to be set as the
#' global folder. If missing, the current global folder option is returned.
#'
#' @return If the `folder` argument is provided, the function does not return
#' anything. If `folder` is missing, the function returns the current value of
#' the global folder option.
#'
#' @examples
#' # Set the global folder path
#' global_folder("/path/to/folder")
#'
#' # Get the current global folder path
#' current_folder <- global_folder()
#' print(current_folder)
#'
#' @export
global_folder <- function(folder){
  if(!missing(folder)){
    options(global.folder = folder)
  }else{
    options("global.folder")[[1]]
  }
}

