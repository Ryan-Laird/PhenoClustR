#' @title dplyr::case_when() that returns factor
#' @description Thanks to https://stackoverflow.com/questions/49572416/r-convert-to-factor-with-order-of-levels-same-with-case-when
#' @export
fct_case_when <- function(...) {
    args <- as.list(match.call())
    levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
    levels <- levels[!is.na(levels)]
    factor(dplyr::case_when(...), levels=levels)
}

#---------------------------------------------------------------------------------------------------------

#' @title Calculate number of years between two dates
#' @description [Stack Overflow](https://stackoverflow.com/questions/15569333/get-date-difference-in-years-floating-point)
#' @param end_date Date
#' @param start_date Date
#' @return dbl
#' @export
#' @examples
#' elapsed_years(lubridate::mdy("01-01-2020"), lubridate::mdy("07-16-2000"))
elapsed_years <- function(start_date, end_date) {
    lubridate::time_length(lubridate::interval(start_date, end_date), "years")
}
