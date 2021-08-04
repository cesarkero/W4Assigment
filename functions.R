# The assignment will be graded based on how closely the documentation reflects the actual functioning of the code presented in the script file. In particular, you will be expected to document
#
# what each function does, in general terms;
#  - the function arguments (inputs);
#  - each function's return value;
#  - conditions that may result in an error;
#  - functions that need to be imported from external packages;
#  - examples of each function's usage

#-------------------------------------------------------------------------------
#' @title  fars_read
#' @description
#' Function to read data from data from the US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System, which is a nationwide
#' census providing the American public yearly data regarding fatal injuries
#' suffered in motor vehicle traffic crashes
#'
#' @param filename File name or locations with the data
#'
#' @return tibble with the readed data
#'
#' @examples
#' fars_read('./data/accident_2013.csv.bz2')
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#-------------------------------------------------------------------------------
#' @title  make_filename
#'
#' @description
#' Funtion to stablish a normalized filename given a year number
#' @param year Year of the data
#'
#' @return character with the name of the file (with extenion .csv.bz2)
#'
#' @details
#'
#' @examples
#' make_filename(2000)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#-------------------------------------------------------------------------------
#' @title  fars_read
#'
#' @description
#' This function returns a list of the tibbles from years founded in data.
#'
#' @param vector years to find data
#'
#' @return List of tibbles with two fields: MONTH and year. If year not in data
#' a warning message will be printed
#'
#' @details
#' Data will be readed from the execution place of the file
#' The data in csv.bz2 format need to be in the working directory
#'
#' @examples
#' x <- fars_read_years(c(2000,2013,2015, 2021))
#' x[[2]]$MONTH
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

#-------------------------------------------------------------------------------
#' @title  fars_read
#'
#' @description
#' Function summarize data from different source files into one tibble containing
#' months in rows and the number of register by month and year. If a year from the vector
#' is not listed It will kept out of the tibble
#'
#' @param vector years to find data
#'
#' @return Single tibble with the sum of registers by month and year
#'
#' @details
#' Data will be readed from the execution place of the file
#' The data in csv.bz2 format need to be in the working directory
#'
#' @examples
#' x <- fars_summarize_years(c(2000,2013,2015, 2021))
#' x
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#-------------------------------------------------------------------------------
#' @title  fars_read
#'
#' @description This function return a plot with the state limits and the coordinates of the cases within it
#'
#' @param state.num numeric value of the state (USA)
#' @param year year to get data retrieved
#'
#' @return plot with the state and coordinates of the data
#'
#' @details
#'
#' @examples
#' x <- fars_map_state(1, 2000)
#' > error file 'accident_2000.csv.bz2' does not exist
#'
#' x <- fars_map_state(1, 2013)
#'
#' x <- fars_map_state(250, 2013)
#' > Error in fars_map_state(250, 2013) : invalid STATE number: 250
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

#--------------------------------------------------------------------------------
# TRYOUT

