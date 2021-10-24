#' Read table from database as a tibble
#'
#' Wrapper around DBI::dbReadTable that imports database tables as tibbles instead of data.frames.
#' Also has the option to convert all field names to upper case.
#' @param conn DBI database connection
#' @param name name of table to import
#' @param all_upper (default FALSE) if TRUE converts all field names to upper case.
#'
#' @return tibble
#' @export
#'
tbl_dbReadTable <- function(conn, name, all_upper = F) {
  .data <- DBI::dbReadTable(conn, name) %>% tibble::as_tibble()
  if(all_upper) {
    names(.data) <- stringr::str_to_upper(names(.data))
  }

  return(.data)

}

#' Query data from database as a tibble
#'
#' Wrapper around DBI::dbGetQuery that imports query results as tibbles instead of data.frames.
#' Also has the option to convert all field names to upper case.
#'
#' @param conn DBI database connection
#' @param statement database query (select statement)
#' @param all_upper (default FALSE) if TRUE converts all field names to upper case.
#'
#' @return tibble
#' @export
#'
tbl_dbGetQuery <- function(conn, statement, all_upper = F) {
  .data <- DBI::dbGetQuery(conn, statement) %>% dplyr::as_tibble()
  if(all_upper) {
    names(.data) <- stringr::str_to_upper(names(.data))
  }
  return(.data)
}


#' Download flat text file to data.frame
#'
#' @param ... see ?data.table::fread or args(data.table::fread)
#'
#' @return
#' @export
tbl_fread <- function(...) {
 data.table::fread(...) %>% tibble::as_tibble()
}



