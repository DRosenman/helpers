#' Create monthly pivot summary
#'
#' @param data data.frame
#' @param date date column name
#' @param group_by column(s) to group the data by
#' @param values_from values column
#' @param calc calculation function (default sum)
#' @param na.rm remove NAs in calculation (default is TRUE)
#' @param total add total column (default FALSE)
#' @param total_name name of total column
#'
#' @return
#' @export
#'
#' @examples
#' monthly_pivot(dplyr::filter(hflights, Date >= '2011-10-01'), date = 'Date', group_by = c("Origin", "Dest"), values_from = "DepDelay", total = TRUE)
monthly_pivot <- function(data, date, group_by, values_from, calc = sum, na.rm = TRUE, total = FALSE, total_name = NULL) {
  # if(!is.null(formals(args(calc))$na.rm) & (formals(args(calc))$na.rm %in% c(TRUE,FALSE))) {
  #   data <- data %>%
  #     dplyr::select(Date = date, !!!rlang::syms(group_by), !!rlang::sym(values_from)) %>%
  #     dplyr::mutate(Date = as.Date(Date), Year = lubridate::year(Date),
  #                   Month = glue::glue("{Year}-{month_char(lubridate::month(Date))}")) %>%
  #     dplyr::select(Month, !!!rlang::syms(group_by), !!rlang::sym(values_from)) %>%
  #     dplyr::group_by(Month, !!!rlang::syms(group_by)) %>%
  #     dplyr::summarise(Values = do.call(calc, .data[[values_from]], na.rm = na.rm)) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::arrange(Month) %>%
  #     tidyr::pivot_wider(names_from = 'Month', values_from = 'Values')
  #
  #
  #
  # } else {
    data_pivot <- data %>%
      dplyr::select(Date = date, !!!rlang::syms(group_by), !!rlang::sym(values_from)) %>%
      dplyr::mutate(Date = as.Date(Date), Year = lubridate::year(Date),
                    Month = glue::glue("{Year}-{month_char(lubridate::month(Date))}")) %>%
      dplyr::select(Month, !!!rlang::syms(group_by), !!rlang::sym(values_from)) %>%
      dplyr::group_by(Month, !!!rlang::syms(group_by)) %>%
      dplyr::summarise(Values = calc(.data[[values_from]], na.rm = na.rm)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(Month) %>%
      tidyr::pivot_wider(names_from = 'Month', values_from = 'Values')

  #}


  if(total) {
    if(is.null(total_name)) {
      total_name <- "Total"
    }

    data_summary <- data %>% dplyr::group_by(!!!rlang::syms(group_by)) %>%
      dplyr::summarise(!!rlang::sym(total_name) := sum(!!rlang::sym(values_from), na.rm = TRUE)) %>%
      dplyr::ungroup()

    data_pivot <- data_pivot %>% dplyr::left_join(data_summary, by = group_by)
  }

  return(data_pivot)

  # dplyr::summarise(Value = calc(.data[[values_from]], na.rm = TRUE))))



    # dplyr::group_by_(c(group_by) %>%
    # dplyr::summarise(Value = calc({{values_from}}, na.rm = na.rm))
}


