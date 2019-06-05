#' Plot a time series for exploring model data
#'
#' @param continuous_data
#' @param date_heading
#' @param plot_heading
#' @param transform
#'
#' @return a ggplot object
#'
#' @export
#'

plot_time_series <- function(continuous_data, date_heading, plot_heading, transform) {

}

#' Plot a duration curve with optional sample points
#'
#' @param continuous_data
#' @param continuous_heading
#' @param sample_data
#' @param sample_heading
#' @param transform
#'
#' @return a ggplot object
#'
#' @export
#'

plot_duration <- function(continuous_data, continuous_heading,
                          sample_data = NULL, sample_heading = NULL,
                          transform = "none") {

}

#' Output a correlation plot matrix
#'
#' @param merged_data
#' @param response_heading
#' @param explanatory_headings
#'
#' @return a plot object
#'
#' @export
#'

correlation_plot_subset <- function(merged_data, response_heading, explanatory_headings) {

}



