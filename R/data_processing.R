#' Merge sample and continuous data
#'
#' @param continuous_data
#' @param continuous_date_heading
#' @param continuous_date_format
#' @param sample_data
#' @param sample_date_heading
#' @param sample_date_format
#' @param method
#' @param max_gap
#'
#' @return a data frame
#' @export
#'

merge_data <- function(continuous_data, continuous_date_heading, continuous_date_format,
                       sample_data, sample_date_heading, sample_date_format,
                       method, max_gap) {

}

#' Add seasonality variables
#'
#' @param merged_data
#' @param date_heading
#'
#' @return a data frame
#' @export
#'

add_seasonality <- function(merged_data, date_heading = "datetime") {

}

#' Add transforms
#'
#' @param merged_data
#' @param column_headings
#' @param transforms
#'
#' @return a data frame
#'
#' @export
#'

add_transforms <- function(merged_data, columns = "All",
                           transforms = c("cube", "square", "sqrt", "cubrt", "log", "ln")) {

}

