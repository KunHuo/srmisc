#' Extract matching patterns from a string
#'
#' @param string a character vector where matches are sought, or an object which
#' can be coerced by as.character to a character vector. Long vectors are supported.
#' @param pattern character string containing a regular expression (or character
#' string for fixed = TRUE) to be matched in the given character vector. Coerced
#' by as.character to a character string if possible. If a character vector of
#' length 2 or more is supplied, the first element is used with a warning. Missing
#' values are allowed except for regexpr, gregexpr and regexec.
#' @param ignore.case if FALSE, the pattern matching is case sensitive and if TRUE,
#' case is ignored during matching.
#' @param perl logical. Should Perl-compatible regexps be used?
#' @param fixed logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @param useBytes logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#'
#' @section Functions:
#' \itemize{
#'   \item regex_extract: Extract first match, return a vector.
#'   \item regex_extract_all: Extract all matches, return a list.
#' }
#'
#' @examples
#' shopping_list <- c("apples x4", "bag of flour", "bag of sugar", "milk x2")
#' regex_extract(shopping_list, "\\d")
#' regex_extract(shopping_list, "[a-z]+")
#' regex_extract(shopping_list, "[a-z]{1,4}")
#' regex_extract(shopping_list, "\\b[a-z]{1,4}\\b")
#'
#' # Extract all matches
#' regex_extract_all(shopping_list, "[a-z]+")
#' regex_extract_all(shopping_list, "\\b[a-z]+\\b")
#' regex_extract_all(shopping_list, "\\d")
#'
#' @export
regex_extract <- function(string,
                          pattern,
                          ignore.case = FALSE,
                          perl = FALSE,
                          fixed = FALSE,
                          useBytes = FALSE){
  regmatches(string,
             regexpr(pattern,
                     string,
                     ignore.case = ignore.case,
                     perl = perl,
                     fixed = fixed,
                     useBytes = useBytes))
}


#' @rdname regex_extract
#' @export
regex_extract_all <- function(string,
                              pattern,
                              ignore.case = FALSE,
                              perl = FALSE,
                              fixed = FALSE,
                              useBytes = FALSE){
  regmatches(string,
             gregexpr(pattern,
                      string,
                      ignore.case = ignore.case,
                      perl = perl,
                      fixed = fixed,
                      useBytes = useBytes))
}



#' Replace matched patterns in a string
#'
#' @inheritParams regex_extract
#' @param replacement a replacement for matched pattern. Coerced
#' to character if possible. For fixed = FALSE this can include backreferences
#' "\\1" to "\\9" to parenthesized subexpressions of pattern. For perl = TRUE only,
#' it can also contain "\\U" or "\\L" to convert the rest of the replacement to upper
#' or lower case and "\\E" to end case conversion. If a character vector of length 2
#' or more is supplied, the first element is used with a warning. If NA, all
#' elements in the result corresponding to matches will be set to NA.
#'
#' @section Functions:
#' \itemize{
#'   \item regex_replace: Replace first match.
#'   \item regex_replace_all: Replace all matches.
#' }
#'
#' @return a vector of string.
#'
#' @examples
#' fruits <- c("one apple", "two pears", "three bananas")
#'
#' regex_replace(fruits, "[aeiou]", "-")
#' regex_replace_all(fruits, "[aeiou]", "-")
#'
#' regex_replace(fruits, "([aeiou])", "")
#' regex_replace(fruits, "([aeiou])", "\\1\\1")
#' regex_replace(fruits, "[aeiou]", c("1", "2", "3"))
#' regex_replace(fruits, c("a", "e", "i"), "-")
#'
#' @export
regex_replace <- function(string,
                          pattern,
                          replacement,
                          ignore.case = FALSE,
                          perl = FALSE,
                          fixed = FALSE,
                          useBytes = FALSE){

  sub(pattern = pattern,
      replacement = replacement,
      x = string,
      ignore.case = ignore.case,
      perl = perl,
      fixed = fixed,
      useBytes = useBytes)
}


#' @rdname regex_replace
#' @export
regex_replace_all <- function(string,
                              pattern,
                              replacement,
                              ignore.case = FALSE,
                              perl = FALSE,
                              fixed = FALSE,
                              useBytes = FALSE){
  gsub(pattern = pattern,
       replacement = replacement,
       x = string,
       ignore.case = ignore.case,
       perl = perl,
       fixed = fixed,
       useBytes = useBytes)
}



#' Locate the position of patterns in a string
#'
#' Vectorised over string and pattern. If the match is of length 0, (e.g. from a
#' special match like $) end will be one character less than start.
#'
#' @inheritParams regex_extract
#'
#' @section Functions:
#' \itemize{
#'   \item regex_locate: Locate first match, return an integer matrix. First column gives start postion of match, and second column gives end position.
#'   \item regex_locate_all: Locate all matches, return a list of integer matrices.
#' }
#'
#' @examples
#' fruit <- c("apple", "banana", "pear", "pineapple")
#'
#' regex_locate(fruit, "$")
#' regex_locate(fruit, "a")
#' regex_locate(fruit, "e")
#' regex_locate(fruit, c("a", "b", "p", "p"))
#'
#' regex_locate_all(fruit, "a")
#' regex_locate_all(fruit, "e")
#' regex_locate_all(fruit, c("a", "b", "p", "p"))
#'
#'
#' @export
regex_locate <- function(string,
                         pattern,
                         ignore.case = FALSE,
                         perl = FALSE,
                         fixed = FALSE,
                         useBytes = FALSE){
  res <- regexpr(pattern,
                 string,
                 ignore.case = ignore.case,
                 perl = perl,
                 fixed = fixed,
                 useBytes = useBytes)
  locate_start_end(res)
}


#' @rdname regex_locate
#' @export
regex_locate_all <- function(string,
                             pattern,
                             ignore.case = FALSE,
                             perl = FALSE,
                             fixed = FALSE,
                             useBytes = FALSE){
  res <- gregexpr(pattern,
                  string,
                  ignore.case = ignore.case,
                  perl = perl,
                  fixed = fixed,
                  useBytes = useBytes)
  lapply(res, function(x) locate_start_end(x))
}


locate_start_end <- function(data){
  start <- data
  start[start == -1] <- NA
  end <- start + attr(data, "match.length") - 1
  res <- cbind(start, end)

  if(any(is.na(res))){
    res <- res[-1, ]
  }

  res
}


#' Detect the presence or absence of a pattern in a string
#'
#' @inheritParams regex_extract
#'
#' @return a logical vector.
#'
#' @examples
#' fruit <- c("apple", "banana", "pear", "pinapple")
#' regex_detect(fruit, "a")
#' regex_detect(fruit, "^a")
#' regex_detect(fruit, "a$")
#' regex_detect(fruit, "b")
#' regex_detect(fruit, "[aeiou]")
#'
#' @export
regex_detect <- function(string,
                         pattern,
                         ignore.case = FALSE,
                         perl = FALSE,
                         fixed = FALSE,
                         useBytes = FALSE){
  grepl(
    pattern,
    string,
    ignore.case = ignore.case,
    perl = perl,
    fixed = fixed,
    useBytes = useBytes)

}


#' Split up a string into pieces
#'
#' @param string a character vector where matches are sought, or an object which
#' can be coerced by as.character to a character vector. Long vectors are supported.
#' @param pattern character string containing a regular expression (or character
#' string for fixed = TRUE) to be matched in the given character vector. Coerced
#' by as.character to a character string if possible. If a character vector of
#' length 2 or more is supplied, the first element is used with a warning. Missing
#' values are allowed except for regexpr, gregexpr and regexec.
#' @param perl logical. Should Perl-compatible regexps be used?
#' @param fixed logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @param useBytes logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#'
#' @examples
#'
#' fruits <- c(
#'   "apples and oranges and pears and bananas",
#'   "pineapples and mangos and guavas"
#' )
#'
#' regex_split(fruits, " and ")
#'
#' @export
regex_split <- function(string,
                        pattern,
                        perl = FALSE,
                        fixed = FALSE,
                        useBytes = FALSE){
  strsplit(
    string,
    pattern,
    perl = perl,
    fixed = fixed,
    useBytes = useBytes)
}
