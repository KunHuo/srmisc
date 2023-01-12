#' String alignment
#'
#' Align a vector of strings to the left, to the right, to the center or to the
#' first occurance of a specified character, e.g. to the decimal separator.
#' Alignment is achieved by padding the strings with empty spaces (which evidently
#' only will have an alignment effect if the text is displayed with a monospaced font).
#'
#'
#' @param x
#' a character vector to be aligned.
#' @param sep
#' the character on whose position the strings will be aligned. Left alignment
#' can be requested by setting sep = "\\l", right alignment by "\\r" and center
#' alignment by "\\c". Mind the backslashes, as if they are omitted, strings
#' would be aligned to the character l, r or c respectively. Default value is
#' "\\r", thus right alignment.
#'
#' @return
#' a character vector containing the aligned strings
#' @export
#'
#' @examples
#' # align on (the first occuring) B
#' x <- c("ABCDMNB", "CDGHEBK", "BCI")
#' cbind(str_align(x, sep="B"))
#'
#' # align to decimal separator (here point)
#' z <- c("    6.0", "6.00  ", " 45.12    ", "784", NA)
#' cbind(str_align(z, sep="."))
#'
#' # right align, the width will be the max number of characters in x
#' cbind(str_align(x, sep="\\r"))
#' # left align
#' cbind(str_align(x, sep="\\l"))
#' # center
#' cbind(str_align(x, sep="\\c"))
str_align <- function (x, sep = "\\r"){
  id.na <- is.na(x)
  if (length(grep("\\", sep, fixed = TRUE)) == 0) {
    idx <- !grepl(x = x, pattern = sep, fixed = TRUE)
    x[idx] <- paste(x[idx], sep, sep = "")
  }
  if (sep == "\\c")
    return(str_pad(x, width = max(nchar(x), na.rm = TRUE),
                   pad = " ", adj = "center"))
  x <- str_pad(x, max(nchar(x), na.rm = TRUE))
  if (sep == "\\l")
    return(sub("(^ +)(.+)", "\\2\\1", x))
  if (sep == "\\r")
    return(sub("(.+?)( +$)", "\\2\\1", x))
  bef <- substr(x, 1, str_pos(x, sep, fix = TRUE))
  aft <- substr(x, str_pos(x, sep, fix = TRUE) + 1, nchar(x))
  aft <- substr(aft, 1, max(nchar(str_trim(aft, method = "right"))))
  res <- paste(replace(str_pad(bef, max(nchar(bef), na.rm = TRUE),
                               " ", adj = "right"), is.na(bef), ""),
               replace(str_pad(aft, max(nchar(aft), na.rm = TRUE), " ",
                               adj = "left"), is.na(aft), ""), sep = "")
  res[id.na] <- NA
  if (length(grep("\\", sep, fixed = TRUE)) == 0)
    res[idx] <- gsub(sep, " ", res[idx], fixed = TRUE)
  return(res)
}



#' Pad a string with justification
#'
#' str_pad will fill a string x with defined characters to fit a given length.
#'
#' @param x
#' a vector of strings to be padded.
#' @param width
#' resulting width of padded string. If x is a vector and width is left to NULL,
#' it will be set to the length of the largest string in x.
#' @param pad
#' string to pad with. Will be repeated as often as necessary. Default is " ".
#' @param adj
#' adjustement of the old string, one of "left", "right", "center". If set to
#' "left" the old string will be adjusted on the left and the new characters
#' will be filled in on the right side.
#'
#' @details
#' If a string x has more characters than width, it will be chopped on the
#' length of width.
#'
#' @return the string.
#' @export
#'
#' @examples
#' str_pad("My string", 25, "XoX", "center")
str_pad <- function (x, width = NULL, pad = " ", adj = "left") {
  .pad <- function(x, width, pad = " ", adj = "left") {
    if (is.na(x))
      return(NA)
    mto <- match.arg(adj, c("left", "right", "center"))
    free <- max(0, width - nchar(x))
    fill <- substring(paste(rep(pad, ceiling(free/nchar(pad))),
                            collapse = ""), 1, free)
    if (free <= 0)
      x
    else if (mto == "left")
      paste(x, fill, sep = "")
    else if (mto == "right")
      paste(fill, x, sep = "")
    else paste(substring(fill, 1, free%/%2), x, substring(fill,
                                                          1 + free%/%2, free), sep = "")
  }
  if (is.null(width))
    width <- max(nchar(x), na.rm = TRUE)
  lgp <- recycle(x = x, width = width, pad = pad,
                 adj = adj)
  sapply(1:attr(lgp, "maxdim"), function(i) .pad(lgp$x[i],
                                                 lgp$width[i], lgp$pad[i], lgp$adj[i]))
}


#' Recyle a list of elements
#'
#' This function recycles all supplied elments to the maximal dimension.
#'
#' @param ...
#' a number of vectors of elements.
#'
#' @return
#' a list of the supplied elements.
#' @seealso [rep], [replicate]
#' @export
#'
#' @examples
#' recycle(x=1:5, y=1, s=letters[1:2])
recycle <- function (...) {
  lst <- list(...)
  maxdim <- max(lengths(lst))
  res <- lapply(lst, rep, length.out = maxdim)
  attr(res, "maxdim") <- maxdim
  return(res)
}


#' Find position of first occurrence of a string
#'
#' Returns the numeric position of the first occurrence of a substring within a
#' string. If the search string is not found, the result will be NA.
#'
#'
#' @param x a character vector in which to search for the pattern, or an object
#' which can be coerced by as.character to a character vector.x
#' @param pattern
#' character string (search string) containing the pattern to be matched in the
#' given character vector. This can be a character string or a regular expression.
#' @param pos
#' integer, defining the start position for the search within x. The result will
#' then be relative to the begin of the truncated string. Will be recycled.
#' @param ...
#' the dots are passed to the function [regexpr].
#'
#' @details
#' This is just a wrapper for the function [regexpr].
#'
#' @return
#' a vector of the first position of pattern in x
#' @export
str_pos <- function (x, pattern, pos = 1, ...) {
  pos <- rep(pos, length.out = length(x))
  x <- substr(x, start = pos, stop = nchar(x))
  i <- as.vector(regexpr(pattern = pattern, text = x, ...))
  i[i < 0] <- NA
  return(i)
}


#' Remove leading/trailing whitespace from a string
#'
#' The function removes whitespace characters as spaces, tabs and newlines from
#' the beginning and end of the supplied string. Whitespace characters occurring
#' in the middle of the string are retained. Trimming with method "left" deletes
#' only leading whitespaces, "right" only trailing. Designed for users who were
#' socialized by SQL.
#'
#'
#' @param x
#' the string to be trimmed.
#' @param pattern
#' the pattern of the whitespaces to be deleted, defaults to space, tab.
#' @param method
#' one out of "both" (default), "left", "right". Determines on which side the
#' string should be trimmed.
#'
#' @return the string x without whitespaces.
#' @export
#'
#' @examples
#' str_trim("  Hello world! ")
#'
#' str_trim("  Hello world! ", method="left")
#' str_trim("  Hello world! ", method="right")
#'
#' # user defined pattern
#' str_trim(" ..Hello ... world! ", pattern=" \\.")
str_trim <- function (x, pattern = " \t\n", method = "both") {
  switch(
    match.arg(arg = method, choices = c("both", "left", "right")),
    both = {
      gsub(
        pattern = gettextf("^[%s]+|[%s]+$", pattern, pattern),
        replacement = "",
        x = x
      )
    },
    left = {
      gsub(
        pattern = gettextf("^[%s]+", pattern),
        replacement = "",
        x = x
      )
    },
    right = {
      gsub(
        pattern = gettextf("[%s]+$", pattern),
        replacement = "",
        x = x
      )
    }
  )
}



#' Truncate strings and add ellipses if a string is truncated
#'
#' Truncates one or more strings to a specified length, adding an ellipsis (...)
#' to those strings that have been truncated. The truncation can also be performed
#' using word boundaries. Use [str_align] to justify the strings if needed.
#'
#' @param x
#' a vector of strings.
#' @param maxlen
#' the maximum length of the returned strings (NOT counting the appended ellipsis).
#' maxlen is recycled.
#' @param ellipsis
#' he string to be appended, if the string is longer than the given maximal length.
#' The default is "...".
#' @param wbound
#' logical. Determines if the maximal length should be reduced to the next smaller
#' word boundary and so words are not chopped. Default is FALSE.
#'
#' @return
#' The string(s) passed as x now with a maximum length of maxlen + 3 (for the ellipsis).
#' @export
#'
#' @examples
#' x <- c("this is short", "and this is a longer text",
#' "whereas this is a much longer story, which could not be told shorter")
#'
#' # simple truncation on 10 characters
#' str_trunc(x, maxlen=10)
#'
#' # NAs remain NA
#' str_trunc(c(x, NA_character_), maxlen=15, wbound=TRUE)
#'
#' # using word boundaries
#' for(i in -5:20)
#'   print(str_trunc(x, maxlen=i, wbound=TRUE))
#'
#' # compare
#' for(i in -5:20)
#'   print(str_trunc(x, maxlen=i, wbound=FALSE))
str_trunc <- function (x, maxlen = 20, ellipsis = "...", wbound = FALSE) {
  x[!(valid <- !is.na(x))] <- ""
  maxlen <- rep(maxlen, length.out = length(x))
  if (wbound) {
    for (i in seq_along(x)) {
      if (nchar(x[i]) > maxlen[i]) {
        ll <- gregexpr("\\b\\W+\\b", x[i], perl = TRUE)[[1]]
        j <- ll <= maxlen[i]
        maxlen[i] <- if (all(!j)) {
          maxlen[i]
        }
        else {
          max(ll[ll <= maxlen[i]])
        }
      }
    }
  }
  res <- paste0(substr(x, 0L, maxlen), ifelse(nchar(x) > maxlen, ellipsis, ""))
  res[!valid] <- NA_character_
  return(res)
}


#' Capitalize the first letter of a string
#'
#' @param x string to be capitalized.
#' @param method one out of "first" (default), "word", "title". "first" will only
#' capitalize the first character of a string. "word" will capitalize all found
#' words and "title" will also capitalize wordwise, but leave out: a, an, the,
#' at, by, for, in, of, on, to, up, and, as, but, s, or and nor.)
#'
#' @return a vector of charaters with the first letter capitalized
#' @export
#'
#' @examples
#' # capitalize first character
#' str_capitalize(c("Hello", "bob", "daN"))
#' # but not all...
#' str_capitalize(c("Hello bob, how are you?", "And you, DANIEL?"))
#'
#' # wordwise
#' str_capitalize(c("Capitalize all words in titles of publications and documents",
#'                  "but Up and UP, not all and all", NA), method="word")
#'
#' # wordwise omitting the ones listed above
#' str_capitalize(c("Capitalize all words in titles of publications and documents",
#'                  "but Up and UP, not all and all", NA), method="title")
#'
#' # do not touch non alphabetic characters
#' z <- c("Lorem ipsum dolor", "-- sit amet", "consectetur --", " adipiscing elit ",
#'        "sed,.--(do) / +-*eiusmod")
#' str_capitalize(z, method="title")
str_capitalize <- function (x, method = c("first", "word", "title")) {
  .cap <- function(x) {
    capped <- grep("^[^A-Z]*", x, perl = TRUE)
    substr(x[capped], 1, 1) <- toupper(substr(x[capped],
                                              1, 1))
    return(x)
  }
  na <- is.na(x)
  switch(match.arg(method), first = {
    res <- .cap(x)
  }, word = {
    res <- unlist(lapply(lapply(strsplit(x, split = "\\b\\W+\\b"),
                                .cap), paste, collapse = " "))
  }, title = {
    z <- strsplit(tolower(x), split = "\\b\\W+\\b")
    low <- c("a", "an", "the", "at",
             "by", "for", "in", "of",
             "on", "to", "up", "and",
             "as", "but", "or", "nor",
             "s")
    z <- lapply(z, function(y) {
      y[y %nin% low] <- str_capitalize(y[y %nin% low])
      y[y %in% low] <- tolower(y[y %in% low])
      y
    })
    nn <- strsplit(x, split = "\\w+")
    res <- unlist(lapply(1:length(z), function(i) {
      if (length(nn[[i]]) != length(z[[i]])) {
        if (z[[i]][1] == "") {
          z[[i]] <- z[[i]][-1]
        } else {
          z[[i]] <- c(z[[i]], "")
        }
      } else {
        if (z[[i]][1] == "" & length(z[[i]]) >
            1) z[[i]] <- vec_rot(z[[i]], -1)
      }
      do.call(paste, list(nn[[i]], z[[i]], sep = "",
                          collapse = ""))
    }))
  })
  res[na] <- NA
  return(res)
}

vec_rot <- function (x, k = 1L) {
  if (k != round(k)) {
    k <- round(k)
    warning("'k' is not an integer")
  }
  k <- k%%length(x)
  rep(x, times = 2L)[(length(x) - k + 1L):(2L * length(x) - k)]
}


`%nin%` <- function (x, table) {
  is.na(match(x, table))
}
