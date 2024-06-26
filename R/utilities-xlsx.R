#' Read an excel file
#'
#' @param path an xlsx file, Workbook object or URL to xlsx file.
#' @param sheet the name or index of the sheet to read data from.
#' @param code.sheet the name or index of the sheet to read code data from.
#' @param str2num convert text to numbers.
#' @param ... arguments passed to [openxlsx::read.xlsx()]
#'
#' @return a data frame.
#' @export
read_xlsx <- function(path, sheet, code.sheet, str2num = TRUE, ...){

  data <- openxlsx::read.xlsx(xlsxFile = path, sheet = sheet, ...)

  if(str2num){
    data <- str2num(data)
  }

  if(missing(code.sheet)){
    code.sheet <- "code"
  }

  tryCatch(
    {
      code <- openxlsx::read.xlsx(xlsxFile = path, sheet = code.sheet)
      data <- codes2labels(data, codes = code)
    },
    error = function(e){
      data
    }
  )
  data
}


#' Convert text to numbers
#'
#' @param data a data frame.
#'
#' @return a data frame.
#' @export
str2num <- function(data){
  data[, ] <- lapply(data[, ], \(x){
    if(is.character(x) & all(srmisc::regex_detect(x, pattern = "^(\\-|\\+)?\\d+(\\.\\d+)?$"))){
      as.numeric(x)
    }else{
      x
    }
  })
  data
}


#' write data to an xlsx file
#'
#' @param data a data.frame or a (named) list of objects.
#' @param path A file path to save the xlsx file
#' @param code code for data in a sheet.
#' @param asTable 	If TRUE will use writeDataTable() rather than writeData()
#' to write x to the file (default: FALSE)
#' @param ... additional arguments passed to [openxlsx::buildWorkbook()].
#'
#' @export
write_xlsx <- function(data, path = "", code = TRUE, asTable = FALSE, ...){

  if(is.list(data)){
     openxlsx::write.xlsx(data, file = path, asTable = asTable, ...)
  }else{
    if(code){
      codes <- codes(data)
      data[,] <- lapply(data[,], \(d){
        if(is.factor(d)){
          as.numeric(d)
        }else{
          d
        }
      })
      openxlsx::write.xlsx(data, file = path, asTable = asTable, ...)

    }else{
      openxlsx::write.xlsx(data, file = path, asTable = asTable, ...)
    }
  }
}
