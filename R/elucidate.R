# Copyright 2024 Province of British Columbia
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


# mcv (internal) ----------------------------------------------------------
#' @title
#' Obtain the most common value (mode) of a vector.
#'
#' @description
#' \code{mcv} returns the mode AKA most common value rounded to 3 decimal
#' places if y is a numeric vector. If y is non-numeric the most common value is
#' returned as a character string. For additional unique values of y sorted by
#' decreasing frequency, use counts() instead.
#'
#' @importFrom stats na.omit
#'
#' @param y a numeric vector/variable.
#'
#' @param digits This determines the number of digits used for rounding of
#'   numeric outputs. Default = 3.
#'
#' @param inv This allows you to get the inverse or opposite of the mode, i.e.
#'   the least common value or "anti-mode". Default is FALSE.
#'
#' @param na.rm This determines whether missing values (NAs) should be removed
#'   before attempting to count values and extract the mode (or anti-mode).
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#' y <- c(1:100, 2, 2, 4, 5, 6, 25, 50)
#'
#' mcv(y) #returns the mode
#'
#' mcv(y, inv = TRUE) #returns the anti-mode
#
#' @references
#' Code was obtained from \href{https://github.com/bcgov/elucidate/blob/main/R/miscellaneous.R}{https://github.com/bcgov/elucidate/blob/main/R/miscellaneous.R}
#'
#' @noRd
mcv <- function(y, digits = 3, inv = FALSE, na.rm = TRUE){
  if(na.rm == TRUE) {
    y <- na.omit(y)
  }
  if(inv == FALSE) {
    if(na.rm == TRUE) {
      out <- names(sort(table(y, useNA = "no"), decreasing = TRUE))[1]
    } else {
      out <- names(sort(table(y, useNA = "ifany"), decreasing = TRUE))[1]
    }
  } else if (inv == TRUE) {
    if(na.rm == TRUE) {
      out <- names(sort(table(y, useNA = "no"), decreasing = FALSE))[1]
    } else {
      out <- names(sort(table(y, useNA = "ifany"), decreasing = FALSE))[1]
    }
  }
  if(is.numeric(y)) {
    out <- round(as.numeric(out), digits)
  }
  return(out)
}


# translate (internal) ----------------------------------------------------
#' @title Recode a variable using a matched pair of vectors.
#'
#' @description Recode a variable using a pair of vectors matched by row (i.e. a
#'   dictionary of the sort commonly made using a spreadsheet). This allows you
#'   to translate a variable from one coding scheme to another using arbitrary
#'   conditional matching in situations where it would be overly tedious to use
#'   recode, if_else, or case_when to recode a variable because there are many
#'   conditional replacements to be specified.
#'
#' @param y A vector that you wish to translate/recode.
#'
#' @param old A vector containing values of the old coding scheme with
#'   corresponding values/rows in "new". Should be the same length as the "new"
#'   vector.
#'
#' @param new A vector containing values of the new coding scheme with
#'   corresponding values/rows in "old". Should be the same length as the "old"
#'   vector.
#'
#' @return An updated version of the input vector that has been translated from
#'   the old coding scheme to the new one.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#'
#' data(mtcars)
#'
#' old_values <- c(1:10)
#'
#' new_values <- c("one", "two", "three", "four", "five",
#'                 "six", "seven", "eight", "nine", "ten")
#'
#' #use it on its own
#' translate(y = mtcars$cyl, old = old_values, new = new_values)
#'
#' #or within a dplyr::mutate call
#' dplyr::mutate(mtcars,
#'               translated_cyl = translate(cyl,
#'                                          old = old_values,
#'                                          new = new_values))
#'
#' @seealso \code{\link[base]{match}}
#'
#' @references
#' Code was obtained from \href{https://github.com/bcgov/elucidate/blob/main/R/miscellaneous.R}{https://github.com/bcgov/elucidate/blob/main/R/miscellaneous.R}
#'
#' @noRd
translate <- function(y, old, new) {
  out <- new[match(y, old)]
  return(out)
}


# wash_df (internal) ------------------------------------------------------
#' @title clean up a data frame by parsing/updating column classes.
#'
#' @description Clean up a data frame by parsing/updating column classes,
#'   converting column names to a common case for easier use, and remove empty
#'   rows & columns. A convenience wrapper for some helpful routine cleaning
#'   functions from the janitor, readr, & tibble packages.
#'
#' @importFrom janitor remove_empty
#' @importFrom janitor clean_names
#' @importFrom tibble rownames_to_column
#' @importFrom tibble column_to_rownames
#' @importFrom purrr map_dfc
#' @importFrom readr parse_guess
#'
#' @param data A messy data frame that contains inappropriate column
#'   classifications, inconsistently structured column names, empty rows/columns
#'
#' @param clean_names If TRUE (default), applies
#'   \code{\link[janitor]{clean_names}} to reformat column names according to
#'   the specified case.
#'
#' @param case The case/format you want column names to be converted to if
#'   clean_names = TRUE. Default is snake_case.
#'
#' @param remove_empty If TRUE (the default), applies
#'   \code{\link[janitor]{remove_empty}} to remove empty rows &/or columns as
#'   per remove_which
#'
#' @param remove_which Either "rows" to remove empty rows, "cols" to remove
#'   empty columns, or c("rows", "cols") to remove both (the default).
#'
#' @param parse If TRUE (the default), applies \code{\link[readr]{parse_guess}}
#'   to each column in data to guess the appropriate column classes and update
#'   them accordingly.
#'
#' @param guess_integer If TRUE, will classify variables containing whole
#'   numbers as integer, otherwise they are classified as the more general
#'   double/numeric class.
#'
#' @param na A character vector of values that should be read as missing/NA when
#'   parse = TRUE. Default is c("", "NA").
#'
#' @param rownames_to_column If TRUE, applies
#'   \code{\link[tibble]{rownames_to_column}} to add the row names of data as a
#'   column. This is often helpful when cleaning up a data frame or tibble that
#'   used to be a matrix with row names.
#'
#' @param col_name If rownames_to_column = TRUE, this specifies the name of the
#'   new column to store the row names in.
#'
#' @param column_to_rownames If TRUE, applies
#'   \code{\link[tibble]{column_to_rownames}} to use the values of a column as
#'   the row names for the data object.
#'
#' @param names_col If column_to_rownames = TRUE, this specifies the column
#'   containing the names you want to assign to the rows of the data object.
#'
#' @return An updated version of the input data, modified according to the
#'   chosen options.
#'
#' @author Craig P. Hutton, \email{Craig.Hutton@@gov.bc.ca}
#'
#' @examples
#' data(mtcars)
#'
#' mtcars$`Extra Column` <- rep(NA, length.out = nrow(mtcars)) #add an empty column
#'
#' mtcars[33:50,] <- NA #add some missing rows
#'
#' mtcars #now mtcars is messy & more like a real raw data set
#'
#' #clean it up and convert the row names to a column
#' mtcars <- wash_df(mtcars, rownames_to_column = TRUE, col_name = "car")
#'
#' mtcars #the empty rows and column are gone, huzzah! So is that awkard column name!
#'
#' #or turn a column with rownames into row names
#' mtcars <- wash_df(mtcars, column_to_rownames = TRUE, names_col = "car")
#' mtcars
#'
#' @seealso \code{\link[janitor]{remove_empty}}
#'
#' @references
#' Code was obtained from \href{https://github.com/bcgov/elucidate/blob/main/R/miscellaneous.R}{https://github.com/bcgov/elucidate/blob/main/R/miscellaneous.R}
#'
#' @noRd
wash_df <- function(data, clean_names = TRUE, case = "snake",
                    remove_empty = TRUE, remove_which = c("rows", "cols"),
                    parse = TRUE, guess_integer = FALSE, na = c("", "NA"),
                    rownames_to_column = FALSE, col_name = "rowname",
                    column_to_rownames = FALSE, names_col = "rowname"){

  if (remove_empty == TRUE) {
    data <- janitor::remove_empty(data, which = remove_which)
  }

  if (clean_names == TRUE) {
    data <- janitor::clean_names(data, case = case)
  }

  if (rownames_to_column == TRUE) {
    data <- tibble::rownames_to_column(data, var = col_name)
  }

  if (parse == TRUE) {
    data <- purrr::map_dfc(data, ~as.character(.x))
    data <- purrr::map_dfc(data, ~readr::parse_guess(.x, na = na,
                                                     guess_integer = guess_integer))
  }

  if (column_to_rownames == TRUE) {
    data <- tibble::column_to_rownames(data, var = names_col)
  }
  return(data)
}
