#' Print a table using the booktabs style.
#'
#' This function prints a table in the booktabs style, which is commonly used
#' in LaTeX documents.
#'
#' @param data A data frame or matrix to be printed.
#' @param sep Separator used to split column names into two parts for table headers.
#' @param adj A vector specifying the alignment of each column. Default is "left"
#' for character columns and "center" for numeric columns.
#' @param ... Additional arguments to be passed to the function.
#'
#' @details
#' This function prints a table in the booktabs style. It handles numeric values
#' by formatting them to a maximum number of digits and aligning them to the center.
#' Character values are aligned to the left. It also supports multi-level column
#' headers separated by the specified separator.
#'
#' @export
print_booktabs <- function(data, sep = "__", adj = NULL, ...){

  title <- attr(data, "title")
  note  <- attr(data, "note")

  if(!is.null(title)){
    cat("\n")
    cat(title, "\n")
  }

  pad_df <- function(data, adj = "left"){
    if(length(adj) != ncol(data)){
      adj <- c(adj,  rep(adj[length(adj)], ncol(data) - length(adj) ))
    }

    data[,] <-  Map(function(x, a){
      str_pad(x, width = max_nchar(x), adj = a)
    }, data, adj)
    data
  }

  print_lines <- function(data, n.title = 1, n.space = 3){
    for(i in 1:nrow(data)){
      row <- data[i, ,drop = TRUE]
      row <- paste(row, collapse = strrep(" ", n.space))
      lines <- strrep("-", nchar(row))
      if(n.title == 1){
        if(i == 1 | i == 2){
          cat(lines, "\n")
        }
      }else{
        if(i == 1 | i == 4){
          cat(lines, "\n")
        }
      }
      cat(row, "\n")
      if(i== nrow(data)){
        cat(lines, "\n")
      }
    }
  }

  if(is.null(adj)){
    adj <- sapply(data, function(x){
      ifelse(is.numeric(x), "center", "left")
    })
  }

  data[, ] <- lapply(data[, ], function(x){
    if(is.numeric(x)){
      sapply(x, \(i){
        if(is.na(i)){
          ""
        }else{
          fmt <- sprintf("%%.%df", max_digits(i))
          sprintf(fmt, i)
        }
      })
    }else{
      sapply(x, function(i){
        if(is.na(i)){
          ""
        }else{
          as.character(i)
        }
      })
    }
  })

  if(any(regex_detect(names(data), pattern = sep, fixed = TRUE))){
    titles <- names(data)
    titles <- strsplit(names(data), split = sep, fixed = TRUE)
    titles <- do.call(cbind, titles)
    title1 <- titles[1, ]
    title2 <- titles[2, ]

    data <- rbind(title2, data)
    data <- pad_df(data, adj = adj)
    data <- lapply(data, function(x){ c(strrep("-", max_nchar(x)), x) })
    data <- as.data.frame(data)

    cdata <- lapply(unique(title1), function(x){
      colindex <- which(x == title1)
      tmpdata <- data[, colindex, drop = FALSE]
      tmpdata <- apply(tmpdata, 1, paste, collapse = strrep(" ", 3))
      tmpdata[1] <- strrep("-", nchar(tmpdata[1]))
      tmpdata
    })

    names(cdata) <- unique(title1)
    cdata <- as.data.frame(cdata)
    cdata <- rbind(unique(title1), cdata)
    cdata <- pad_df(cdata, adj = c("left", "center"))

    for(i in seq_along(cdata)){
      if(str_trim(cdata[1, i]) == str_trim(cdata[3, i])){
        cdata[2, i] <- cdata[1, i]
        cdata[1, i] <- strrep(" ", nchar(cdata[1, i]))
        cdata[3, i] <- strrep(" ", nchar(cdata[3, i]))
      }
    }

    print_lines(cdata, n.title = 2, n.space = 3)
  }else{
    data <- rbind(names(data), data)
    data <- pad_df(data, adj = adj)
    print_lines(data, n.title = 1, n.space = 3)
  }

  if(!is.null(note)){
    cat(note)
    cat("\n\n")
  }
}


#' Determine the number of digits
#'
#' This function calculates the number of digits after the decimal point in
#' numeric values.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector indicating the number of digits after the decimal
#' point for each element in \code{x}.
#'
#' @details
#' This function takes a numeric vector as input and returns another numeric
#' vector indicating the number of digits after the decimal point for each
#' element in the input vector. If the input value is not numeric, the function
#' returns 0.
#'
#' @examples
#' n_digits(2.45)
#'
#' n_digits(3)
#'
#' @export
n_digits <- function(x){

  x <- as.character(x)

  sapply(x, function(i) {
    # Split the character at the decimal point
    i <- regex_split(i, pattern = ".", fixed = TRUE)
    i <- unlist(i)

    if (length(i) == 1L) {
      0 # If there is no decimal point, return 0
    } else{
      nchar(i[2]) # Otherwise, return the number of characters after the decimal point
    }
  })
}


max_digits <- function(x){
  max(n_digits(x), na.rm = TRUE)
}


max_nchar <- function(x){
  max(sapply(x, nchar), na.rm = TRUE)
}
