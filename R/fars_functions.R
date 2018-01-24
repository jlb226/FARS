

#' Read in NHTSA FARS data
#'
#' The fars_read function takes as an input the filename of a *.csv file.  It reads in the file
#' data as a data frame.
#'
#' @param filename A string indicating the name of the *.csv file to be read in
#'
#' @return The fars_read function returns a tbl_df object
#'
#' @details The fars_read function first checks whether the file exists and prints a
#' warning if the file cannot be found.  The function generates an error if the data
#' present within the file is invalid.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#'   fars_read("accident_2015.csv.bz2")
#' }
#'
#' @export
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}



#' Print the filename corresponding to the specified year
#'
#' The make_filename function accepts as input an integer or a string that represents
#' a specified year.  It then prints the filename corresponding to the year.
#'
#' @param year An integer or a string corresponding to a specific year for which
#' the filename should be labeled
#'
#' @return The make_filename function returns the filename as a string.
#'
#' @details The make_filename function results in an error if values other than
#' years are passed to it or if the data cannot be found.
#'
#' @examples
#' \dontrun{
#'   make_filename(2015)
#' }
#'
#' @export
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}



#' Return the month and year of each FARS observation
#'
#' The fars_read_years function takes as input an integer or a string that represents
#' a specified year.  It then reads in the data from the *.csv file corresponding to
#' the given year and returns each observation's month and year.
#'
#' @param years An integer or a string corresponding to a specific year
#'
#' @return The fars_read_years function returns a list of months and years.
#'
#' @details The fars_read_years function returns an error message for invalid years.
#'
#' @importFrom dplyr %>% mutate select
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#'   fars_read_years(2015)
#' }
#'
#' @export
#'
fars_read_years <- function(years) {
  year <- NULL
  MONTH <- NULL
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



#' Return a summary of accidents by month in the specified year
#'
#' The fars_summarize_years function takes as input a year in the form of an integer or string.
#' It then summarizes accident data from the specified year.
#'
#' @param years An integer or a string corresponding to a specific year
#'
#' @return The fars_summarize_years function returns the summary of accidents as a tbl_df object
#'
#' @details The fars_summarize_years function returns an error message if years is invalid.
#'
#' @importFrom dplyr %>% group_by bind_rows summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#'   fars_summarize_years(2015)
#' }
#'
#' @export
#'
fars_summarize_years <- function(years) {
  MONTH <- NULL
  n <- NULL
  year <- NULL
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}



#' Plot a map of accidents
#'
#' The fars_map_state function accepts as input a state number and a year as either integers or strings.
#' It then uses the values to plot a map of the observations for the specified state and year.
#'
#' @param state.num An integer or a string corresponding to a specific state within the data set
#'
#' @param year An integer or a string corresponding to a specific year
#'
#' @return The fars_map_state function returns a map of the observations for the specified state and year
#'
#' @details The fars_map_state function returns an error message for invalid years, invalid state numbers,
#' and if the data is not present
#'
#' @importFrom maps map
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#'   fars_map_state(3, 2015)
#' }
#'
#' @export
#'
fars_map_state <- function(state.num, year) {
  STATE <- NULL
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
