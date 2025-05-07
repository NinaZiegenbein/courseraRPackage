#' Read in the fars data
#'
#' This function checks wether the provided filename exists, if it does not it
#' prints that the file does not exist and stops the function. If the file does
#' exist, the data is read without returning messages or showing the progress and
#' the data is then made into a table-dataframe using dplyr.
#'
#' @param filename The filename of the fars data file.
#'
#' @return A table-like dataframe containing the contents of the FARS data file
#'
#' @examples
#' fars_read("fars_data.csv")
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

#' Create a FARS filename for a given year
#'
#' This function generates a standard FARS filename string for a given year.
#'
#' @param year An integer or numeric value specifying the year.
#'
#' @return A character string with the FARS filename in the format
#' "accident_<year>.csv.bz2".
#'
#' @examples
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS data for multiple years
#'
#' This function reads in FARS data files for each year specified, returning a
#' list of data frames with the `MONTH` and `year` columns.
#' If a file for a year is missing or unreadable, a warning is issued and `NULL`
#' is returned for that year.
#'
#' @param years A numeric or integer vector of years.
#'
#' @return A list of data frames, one per year, or `NULL` for years with missing
#' or invalid data.
#'
#' @examples
#' fars_read_years(c(2013, 2014, 2015))
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

#' Summarize FARS data by month and year
#'
#' This function reads FARS data for multiple years, groups the data by
#' `MONTH` and `year`, counts the number of records in each group, and returns
#' a summary table with months as rows and years as columns.
#'
#' @param years A numeric or integer vector of years to summarize.
#'
#' @return A table-like dataframes where each row corresponds to a month and
#' each column corresponds to a year, with the entries showing the number of
#' records for that month and year.
#'
#' @examples
#' fars_summarize_years(c(2013, 2014, 2015))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot accidents on a state map for a given year
#'
#' This function reads FARS data for a specified year and plots accidents for a
#' given state on a U.S. map. If no accidents are found for the state, a
#' message is shown.
#'
#' @param state.num An integer giving the state number (matching the `STATE`
#' column in the FARS dataset).
#' @param year A numeric year indicating which year's data to use.
#'
#' @return A map is drawn as a side effect. If no accidents are present for
#' the specified state,the function returns `NULL` invisibly.
#'
#' @details
#' Longitude values greater than 900 and latitude values greater than 90 are
#' set to `NA`prior to plotting, since they are invalid.
#'
#' @examples
#' fars_map_state(1, 2013)
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
