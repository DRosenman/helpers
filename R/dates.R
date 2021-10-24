#' Convert month integers to two digit strings
#'
#' Converts month from integer format to two digit string format. 1 to '01'. 11 to '11'.
#' @param month integer vector; allowed values 1 to 12.
#'
#' @return character vector with 0 appended to the start of values less than 10.
#' @export
#'
#' @examples
#' month_char(1:12)
month_char <- function(month) {
  stopifnot(is.integer(month) | is.double(month))
  if(!is.integer(month)) {
    month <- as.integer(month)
  }
  if(any(is.na(month) | month < 1 | month > 12)) {
    stop("month must be contain only non NA integers between 1 and 12")
  }

  ifelse(month < 10, paste0("0", month), as.character(month))
}


#' Convert year and month to month label
#'
#' Converts year and month to 'year-two digit month number-month abbreviation or name
#' @param year integer vector
#' @param month integer vector contain values from 1 to 12
#' @param abb if TRUE month abbreviation is used instead of full name (default FALSE)
#' @param upper_case if TRUE month abbreviation or name is upper case
#'
#' @return
#' @export
#'
#' @examples
#' month_label(2021, 6)
#' month_label(2021, 6, abb = TRUE)
#' month_label(2021, 6, upper_case = TRUE)
month_label <- function(year, month, abb = FALSE, upper_case = FALSE) {
  stopifnot(is.integer(month) | is.double(month))
  if(!is.integer(month)) {
    month <- as.integer(month)
  }
  if(!is.integer(year)) {
    year <- as.integer(year)
  }

  if(any(is.na(year))) {
    stop("year must contain only non NA integer")
  }

  if(any(is.na(month) | month < 1 | month > 12)) {
    stop("month must be contain only non NA integers between 1 and 12")

  }
  if(abb) {
    month_name <- month.abb[month]
  } else {
    month_name <- month.name[month]
  }

  month_num <- month_char(month)
  x <- paste(year, month_num, month_name, sep = "_")
  if(upper_case) {
    x <- toupper(x)
  }
  return(x)
}




