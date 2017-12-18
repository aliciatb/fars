#' Print "Fars Read" 
#'
#' This function reads a CSV into a data.frame by passing in the full path 
#' and file name like (\code{filename = "accident_2015.csv.bz2}).
#'
#' @param filename A character string of the full path and filename to the accident data.
#' 
#' @return This function returns a data.frame from a delimited file using readr library. Future versions will return a tibble. 
#'
#' @note Incorrect path to filename will result in the message [File Name] does not exist.
#'
#' @examples
#' fars_read("/data/accident_2015.csv.bz2"))
#' fars_read(filename = "/data/accident_2015.csv.bz2"))
#'
#' @importFrom dplyr
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

#' Print "Fars Make FileName"
#'
#' This function creates the file name for a given year by inserting the
#' year into the file name pattern like accident_\code{2015}.csv.bz2.
#' 
#' @param year A character string representing 4 character length year value.
#' 
#' @return This function returns the filename for the year provided to it. 
#'
#' @note An invalid year will result in invalid file name.
#'
#' @examples
#' make_filename('2015')
#' make_filename(year = '2015')
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Print "Fars Read Years" 
#'
#' This function loops through one or more years and calls \code{make_filename} to 
#' assemble the  file name for a year and calls \code{fars_read} to read the data.
#'
#' @param years A vector of one or more character string years for the desired Fars data.
#' 
#' @return This function returns a list with year and month from valid Fars files or will return
#' a warning and a \code{NULL} value if the file is invalid.
#'
#' @note An invalid year will result in a warning message with nothing returned. If working directory
#' has not been set, a warning message will result for valid file names.
#'
#' @examples
#' fars_read_years(c('2015'))
#' fars_read_years(c('2014','2015'))
#' fars_read_years(years = c('2013','2014','2015'))
#'
#' @importFrom dplyr
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

#' Print "Fars Summarize Years" 
#'
#' This function returns a summary view of the number of records per year and month
#' of the Fars data for all years passed to it.
#'
#' @inheritParams fars_read_years
#' 
#' @return This function returns a single data frame with the number of the records per month 
#' and year that has been tidied so that each year's records is shown in a column. 
#'
#' @note Invalid years will result in a warning message with nothing returned. If working directory
#' has not been set, a warning message will result for valid file names.
#' 
#' @examples
#' fars_summarize_years(c('2014','2015'))
#' fars_summarize_years(years = c('2014','2015'))
#'
#' @importFrom dplyr tidyr
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Print "Fars Map State" 
#'
#' This function plots any accidents for a single state and year.
#'
#' @param state.num A character string signifying the code for a state. For example, 
#' Washington state equals '53'
#' @param year A character string representing 4 character length year value like '2015'.
#' 
#' @return This function returns a map with accidents plotted for a given state and year.
#' Any records with invalid latitude and longitude coordinates will be excluded from the plot.
#'
#' @note An invalid year and/or state code will result in a warning message the failed condition.
#'
#' @examples
#' fars_map_state('53', '2015')
#' fars_map_state(state.num = '53', year = '2015')
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
