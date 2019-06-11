#' Merge sample and continuous data
#'
#' @param continuous_data
#' @param continuous_date_heading
#' @param continuous_date_format
#' @param sample_data
#' @param sample_date_heading
#' @param sample_date_format
#' @param max_gap
#'
#' @return a data frame
#' @export
#'

merge_data <- function(continuous_data, continuous_date_heading,
                       continuous_date_format, continuous_tz = "",
                       sample_data, sample_date_heading,
                       sample_date_format, sample_tz = "",
                       max_gap) {

  #Convert date columns
  continuous_data[,continuous_date_heading] <-
    as.POSIXct(continuous_data[,continuous_date_heading],
               format = continuous_date_format, tz = continuous_tz)
  sample_data[,sample_date_heading] <-
    as.POSIXct(sample_data[,sample_date_heading],
               format = sample_date_format, tz = sample_tz)

  #Interpolate the numeric columns in the continuous data that are not in the sample data
  x <- continuous_data$datetime
  xout <- sample_data$datetime

  for(i in names(continuous_data)) {
    if(!(i %in% names(sample_data)) & is.numeric(continuous_data[,i])) {
      y <- continuous_data[,i]
      p <- na.omit(data.frame(x, y)) %>%
        dplyr::mutate(gap = as.numeric(difftime(x, lag(x), units = "mins")))
      sample_data[,i] <- approx(x, y, xout)$y
      sample_data$gap <- approx(p$x, p$gap, xout, method = "constant", f = 1)$y
      sample_data[sample_data$gap > max_gap,i] <- NA
      sample_data <- dplyr::select(sample_data, -gap)
    }
  }

  return(sample_data)

}

#' Add seasonality variables
#'
#' @param merged_data
#' @param date_heading
#'
#' @return a data frame
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

add_seasonality <- function(merged_data, date_heading = "datetime") {

  merged_data$dd <- lubridate::decimal_date(merged_data[,date_heading])
  merged_data <- merged_data %>%
    dplyr::mutate(sin2piD = sin(2 * pi * dd),
           cos2piD = cos(2 * pi * dd),
           sin4piD = sin(4 * pi * dd),
           cos4piD = cos(4 * pi * dd)) %>%
    dplyr::select(-dd)

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

add_transforms <- function(merged_data, columns = NULL,
                           transforms = c("cube", "square", "sqrt", "cubrt", "log", "ln")) {
  #Get a vector of numeric columns
  num_columns <-
    names(merged_data)[names(merged_data) %in% names(merged_data)[lapply(merged_data, class) == "numeric"]]

  if(is.null(columns)) {
    columns <- num_columns
  } else {
    columns <- columns[columns %in% num_columns]
  }

  for(i in columns) {
    if("cube" %in% transforms)
      if(!(paste0("cube", i) %in% names(merged_data)))
        merged_data[,paste0("cube", i)] <- merged_data[,i]^3
    if("square" %in% transforms)
      if(!(paste0("square", i) %in% names(merged_data)))
        merged_data[,paste0("square", i)] <- merged_data[,i]^2
    if("sqrt" %in% transforms)
      if(!(paste0("squrt", i) %in% names(merged_data)))
        merged_data[,paste0("sqrt", i)] <- merged_data[,i]^(1/2)
    if("cubrt" %in% transforms)
      if(!(paste0("cubrt", i) %in% names(merged_data)))
        merged_data[,paste0("cubrt", i)] <- merged_data[,i]^(1/3)
    if("log" %in% transforms)
      if(!(paste0("log", i) %in% names(merged_data)))
        merged_data[,paste0("log", i)] <- log10(merged_data[,i])
    if("ln" %in% transforms)
      if(!(paste0("ln", i) %in% names(merged_data)))
        merged_data[,paste0("ln", i)] <- log(merged_data[,i])
  }

  return(merged_data)

}

#' Replace left-censored values with half the detection limit
#'
#' @param data
#' @param data_heading
#' @param remark_heading if remark column is null, and the class of
#' the data column is "character", will look for "<" signs within the cells
#'
#' @return a data frame
#'
#' @export
#'

replace_censored <- function(data, heading, remark_heading = NULL) {

  if(!(heading %in% names(data)))
    stop("heading is not one of the columns in data")
  if(!is.null(remark_heading)) {
    if(!(remark_heading %in% names(data)))
      stop("remark_heading is not one of the columns in data")
  }

  if(is.null(remark_heading)) {
    remark_heading <- paste0("R", heading)
    censored <- grepl("<", data[,heading])
    data[,heading] <- as.numeric(gsub("<", "", data[,heading]))
    data[censored, heading] <- data[censored, heading] / 2
    data[,remark_heading] <- ""
    data[censored, remark_heading] <- "<"
  }

  data[grepl("<", data[,remark_heading]), heading] <-
    data[grepl("<", data[,remark_heading]), heading] / 2

  return(data)
}
