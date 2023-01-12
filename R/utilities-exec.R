#' bind a list by rows
#'
#' @param data a list contain data frame.
#' @param names.as.column names as a column, default TRUE..
#' @param collapse.names collapse names, default FALSE.
#' @param collapse.one.row collapse one row, default FALSE.
#' @param varname variable name.
#' @param labels labels.
#' @param dup.var a logical; delete duplicate variable names, default FALSE.
#'
#' @return a data frame.
#' @export
list_rbind <- function(data,
                       names.as.column = TRUE,
                       collapse.names = FALSE,
                       collapse.one.row = FALSE,
                       varname = "variable",
                       dup.var = FALSE,
                       labels = NULL){

  if(!inherits(data, "list")){
    stop("Data must be a list.", call. = FALSE)
  }

  # Index for NULL or nrow < 1L in a data frame.
  index <- sapply(data, \(d){
    if(is_empty(d)){
      TRUE
    }else{
     nrow(d) <1L
    }
  })


  # If all elements are empty, return NULL.
  if(all(index)){
    return(NULL)
  }

  # Get non-null data.
  data <- data[!index]

  NAMES <- names(data)

  if(is.null(NAMES)){
    NAMES <- sprintf("%d", 1:length(data))
  }

  collapse <- function(d, nm){
    d[[1]] <- paste0("    ", d[[1]])
    res <- rbind(c(rep(NA, ncol(d))), d)
    res[1, 1] <- nm
    res
  }

  collapse_column <- function(d, nm){
    if(collapse.one.row){
      collapse(d, nm)
    }else{
      if(nrow(d) == 1L){
        tmpname <- names(d)
        d <- cbind(data.frame(nm), d[, -1, drop = FALSE])
        names(d) <- tmpname
        d
      }else{
        collapse(d, nm)
      }
    }
  }

  out <- Map(function(d, nm){

    nm <- find_labels(labels, varname = nm, defalut = nm)

    if(names.as.column){
      if(collapse.names){
        collapse_column(d, nm)
      }else{
        cbind(data.frame(variable = nm), d)
      }
    }else{
      d
    }
  }, data, NAMES)

  out <- do.call(rbind, out)
  row.names(out) <- NULL
  if(names.as.column){
    names(out)[1] <- varname
    if(dup.var){
      out[[1]] <- delete_duplicate_values(out[[1]])
    }
  }
  out
}


#' Execute a function
#'
#' @param what either a function or a non-empty character string naming the
#' function to be called.
#' @param ... arguments for what.
#' @param envir an environment within which to evaluate the call. This will be
#' most useful if what is a character string and the arguments are symbols or quoted expressions.
#'
#' @return The result of the (evaluated) function call.
#' @export
do_call <- function(what, ..., envir = parent.frame()){
  args <- list(...)
  args <- flatten_list(args)
  do.call(what, args = args, quote = FALSE, envir = envir)
}


#' Apply a function to a data frame split by groups
#'
#' @param data  a data frame.
#' @param group one or more group variable name(s).
#' @param func a function to be applied to (usually data-frame) subsets of data.
#' the function must be return a data frame.
#' @param ... optional arguments to func.
#' @param labels labels.
#' @param out.list logical; if FALSE, return a data frame, otherwise return a list.
#' @param warning logical; whether to show error messages.
#'
#' @return a data frame if out.list is FALSE, otherwise return a list.
#' @export
#'
#' @examples
#' # Basic
#' group_exec(mtcars, func = \(d){
#'   data.frame(mean = mean(d$mpg), min = min(d$mpg))
#' })
#'
#' # By group
#' group_exec(mtcars, group = "vs", func = \(d){
#'   data.frame(mean = mean(d$mpg), min = min(d$mpg))
#' })
#'
#' # Return a list
#' group_exec(mtcars, group = "vs", func = \(d){
#'   data.frame(mean = mean(d$mpg), min = min(d$mpg))
#' }, out.list = TRUE)
#'
#' # With more groups
#' group_exec(mtcars, group = c("vs", "am"), func = \(d){
#'   data.frame(mean = mean(d$mpg), min = min(d$mpg))
#' })
group_exec <- function(data, group = NULL, func = NULL, ..., labels = NULL, out.list = FALSE, warning = TRUE){

  # # Group names from dplyr::group_by() functions
  group.dplyr <- names(attr(data, "groups"))
  group.dplyr <- group.dplyr[-length(group.dplyr)]

  if(is.null(group)){
    group <- group.dplyr
  }

  group <- select_variable(data, group)

  if(length(group) == 0L){
    sdat <- list(overall = data)
  }else{
    sdat <- split.data.frame(data, f = data[group], drop = FALSE, sep = "#")
  }

  out <- lapply(sdat, \(d){
    tryCatch(do_call(func, d, ...), error = function(e) {
      if(warning){
        print(e)
      }
      NULL
    })
  })

  if(all(sapply(out, is_empty))){
    return(NULL)
  }

  if(!out.list){
    if(length(group) == 0L){
      out <- list_rbind(out, names.as.column = FALSE)
    }else{
      group.labels <- sapply(group, \(x){
        find_labels(labels, varname = x, defalut = x)
      })

      if(length(group) == 1L){
        out <- list_rbind(out, names.as.column = TRUE, varname = group)
      }else{
        out <- list_rbind(out, names.as.column = TRUE, varname = ".groups")
        out <- separate2cols(out, varname = ".groups", sep = "#", into = group)
      }

      out[group] <- Map(function(x, g){
        l0 <- unique(x)
        l1 <- sapply(l0, \(i) find_labels(labels, varname = g, code = i, defalut = i))
        l1[match(x, l0)]
      }, out[group], group)

      for(i in seq_along(group.labels)){
        names(out)[names(out) == group[i]] <- group.labels[i]
      }
      out
    }
  }
  out
}
