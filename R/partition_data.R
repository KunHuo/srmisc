#' Data Splitting functions
#'
#' A series of test/training partitions are created using createDataPartition
#' while createResample creates one or more bootstrap samples. createFolds splits
#' the data into k groups while createTimeSlices creates cross-validation split
#' for series data. groupKFold splits the data based on a grouping factor.
#'
#' @param y a vector of outcomes.
#' @param times the number of partitions to create.
#' @param p the percentage of data that goes to training.
#' @param list logical - should the results be in a list (TRUE) or a matrix with
#' the number of rows equal to floor(p * length(y)) and times columns.
#' @param groups for numeric y, the number of breaks in the quantiles.
#' @param seed Seed for random number generation. Default is 123.
#'
#' @return A list or matrix of row position integers corresponding to the
#' training data.
#'
#' @details
#' This function partitions the data by stratified random sampling. If the
#' response variable is of class "Surv" (survival), it extracts the "time"
#' column for sampling. It then ensures that each class has at least one record
#' and warns if some classes have no records or only one record. It uses
#' stratified random sampling to create partitions of the data based on the
#' specified proportion.
#'
#' @export
partition_data <- function (y, times = 1, p = 0.5, list = TRUE, groups = min(5, length(y)), seed = 123) {

  # If response variable is of class "Surv", extract "time" column
  if (inherits(y, "Surv")){
    y <- y[, "time"]
  }

  # Initialize output as a list of length 'times'
  out <- vector(mode = "list", times)

  # Check if 'y' has at least 2 data points
  if (length(y) < 2){
    stop("y must have at least 2 data points")
  }

  # Ensure 'groups' is at least 2
  if (groups < 2){
    groups <- 2
  }

  # If 'y' is numeric, create 'groups' number of equally spaced quantiles
  if (is.numeric(y)) {
    y <- cut(y,
             unique(stats::quantile(y, probs = seq(0, 1, length = groups))),
             include.lowest = TRUE)
  }
  else {
    # If 'y' is categorical, check for classes with no or single records
    xtab <- table(y)
    if (any(xtab == 0)) {
      warning(paste("Some classes have no records (",
                    paste(names(xtab)[xtab ==0], sep = "", collapse = ", "),
                    ") and these will be ignored"))
      y <- factor(as.character(y))
    }

    if (any(xtab == 1)) {
      warning(paste("Some classes have a single record (",
                    paste(names(xtab)[xtab == 1], sep = "", collapse = ", "),
                    ") and these will be selected for the sample"))
    }
  }

  set.seed(seed)

  # Define function to subsample data
  subsample <- function(dat, p) {
    if (nrow(dat) == 1) {
      out <- dat$index
    }
    else {
      num <- ceiling(nrow(dat) * p)
      out <- sample(dat$index, size = num)
    }
    out
  }

  # Iterate 'times' to create partitions
  for (j in 1:times) {
    # Stratified random sampling using 'group_exec' function from 'srmisc' package
    tmp <- group_exec(data.frame(y = y, index = seq(along = y)), group = "y", \(dat){
      subsample(dat, p = p)
    }, out.list = TRUE)
    # Sort and store partitioned data
    tmp <- sort(as.vector(unlist(tmp)))
    out[[j]] <- tmp
  }

  # Convert output to matrix if 'list' is FALSE
  if (!list) {
    out <- matrix(unlist(out), ncol = times)
    colnames(out) <- prettySeq(1:ncol(out))
  }
  else {
    # Otherwise, name the list elements
    names(out) <- prettySeq(out)
  }
  out
}


prettySeq <- function (x) {
  paste("Resample", gsub(" ", "0", format(seq(along = x))), sep = "")
}


#' Randomly Partition Data into Groups
#'
#' This function partitions the input data into two groups randomly,
#' typically for the purpose of creating training and testing datasets.
#'
#' @param data The input data frame or matrix to be partitioned.
#' @param outcome The variable indicating the outcome of interest. If NULL, the
#' function will randomly partition the data without considering any outcome variable.
#' @param p The proportion of data to be allocated to the train group. Default is 0.7.
#' @param group.name The name of the variable/column indicating the groups.
#' Default is ".group".
#' @param group.levels A character vector specifying the names of the groups.
#' Default is c("Train", "Test").
#' @param seed An optional seed for reproducibility. Default is 123.
#'
#' @return The input data frame or matrix with an additional column indicating
#' the group assignment.
#'
#' @seealso [get_train_data()], [get_test_data()].
#'
#' @examples
#' rdata <- random_group(sleep, outcome = "group")
#' rdata
#'
#' get_train_data(rdata)
#'
#' get_test_data(rdata)
#'
#' @export
random_group <- function(data,
                         outcome = NULL,
                         p = 0.7,
                         group.name = ".group",
                         group.levels = c("Train", "Test"),
                         seed = 123){
  outcome <- select_variable(data, outcome)
  index <- partition_data(data[[outcome]], p = p, list = FALSE, seed = seed)
  data$.group <- group.levels[2]
  data$.group[index] <- group.levels[1]
  data$.group <- factor(data$.group, levels = group.levels)
  names(data)[names(data) == ".group"] <- group.name
  attr(data, "group.name") <- group.name
  attr(data, "group.levels") <- group.levels
  attr(data, "index") <- index
  data
}

#' Get Training Data
#'
#' This function extracts the training data from a dataset that has been
#' partitioned using the [random_group()] function.
#'
#' @param data The partitioned dataset containing training and testing data.
#'
#' @return The training data subset from the input dataset.
#'
#' @seealso [get_test_data()], [random_group()].
#'
#' @export
get_train_data <- function(data){
  index <- attr(data, "index")
  group.name <- attr(data, "group.name")
  data <- data[index, ]
  data <- data[-which(names(data) == group.name)]
  row.names(data) <- NULL
  data
}

#' Get Testing Data
#'
#' This function extracts the testing data from a dataset that has been
#' partitioned using the [random_group()] function.
#'
#' @param data The partitioned dataset containing training and testing data.
#'
#' @return The testing data subset from the input dataset.
#'
#' @seealso [get_train_data()], [random_group()].
#'
#' @export
get_test_data <- function(data){
  index <- attr(data, "index")
  group.name <- attr(data, "group.name")
  data <- data[-index, ]
  data <- data[-which(names(data) == group.name)]
  row.names(data) <- NULL
  data
}

