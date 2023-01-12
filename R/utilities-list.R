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


#' Flatten a nested list to a one-level list
#'
#' Flatten a nested list to a one-level list.
#'
#' @param x a list.
#'
#' @return a one-level list.
#' @export
#'
#' @examples
#' dlist <- list("a", mtcars[1:2,], list(5, iris[2:3, ]))
#' dlist
#' list_flatten(dlist)
list_flatten <- function(x) {
  morelists <- sapply(x, function(xprime)
    class(xprime)[1] == "list")
  out <- c(x[!morelists], unlist(x[morelists], recursive = FALSE))
  if (sum(morelists)) {
    Recall(out)
  } else{
    return(out)
  }
}
