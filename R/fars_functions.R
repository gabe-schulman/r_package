#' Read data into R
#' 
#' This is a simple function that will read data from the
#' US National Highway Traffic Safety Administration's Fatality Analysis Reporting System
#' into R as a dataframe object if the file containing the data exists in the working directory.
#' This function requires the "readr" package to be installed.d
#' 
#' @param filename A character string specifying the name of the file containing the data.
#' 
#' @return This function returns the data as a dataframe object.
#' 
#' @examples 
#' fars_read("fars_data")
#' 
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Makes file name with specified year
#' 
#' Simple function that creates the name of the file containing FARS data for the year specified.
#' 
#' @param year Specifies the year of data that is of interest to the user.
#' 
#' @return This function returns a character string that can be inputted into the 'fars_read' function
#' to read in FARS data for available years.
#' 
#' @examples
#' make_filename(2012)
#' 
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Create dataframes of FARS data for desired years
#' 
#' Function that returns dataframes of FARS data for years specified by the user. Returns an error if
#' there is no FARS data for a specified year.
#' 
#' @param years A vector containing the years of FARS data the user wishes to analyze.
#' 
#' @return Returns a dataframe.
#' 
#' @examples
#' fars_read_years(c(2012,2013,2014))
#' 
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>% 
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarizes number of accidents by month and year
#' 
#' A function that takes a vector of years as input and returns a table that displays
#' the number of accidents by year and month. This function requires the "dplyr" package
#' to be installed.
#' 
#' @param years The years of data the user wishes to include in the summary.
#' 
#' @return Returns a key-value table displaying the number of accidents each month
#' of the years in the vector that was passed as an argument to the function.
#' 
#' @examples 
#' fars_summarize_years(c(2012,2013,2014))
#' 
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Maps yearly accident data for a state.
#' 
#' This function takes the state and year as arguments and produces a map of accidents in the state
#' for the year specified. This function will throw an error if the state number is not valid, or if there are
#' no accidents in state for the year specified. This function requires the "maps" package to be installed.
#' 
#' @param state.num The unique ID that identifies the state the user is interested in analyzing.
#' @param year The year of accident data the user wants to display.
#' 
#' @return Returns a map that will be displayed.
#' 
#' @examples
#' fars_map_state(5, 1998)
#' 
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}