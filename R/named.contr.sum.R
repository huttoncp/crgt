# Copyright 2016 Matthew Flickinger
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

#' Named Sum-to-Zero Contrasts
#'
#' Generates and labels sum-to-zero contrasts for factors.
#'
#' @importFrom stats contr.sum
#'
#' @param x The input vector or factor.
#' @param ... Additional arguments to be passed to the \code{contr.sum} function.
#'
#' @return A sum-to-zero contrast matrix.
#'
#' @details If \code{x} is a factor, the function uses the levels of the factor
#'   to compute sum-to-zero contrasts to support use of Type III Sums of Squares
#'   for Analysis of Variance.
#'
#' If \code{x} is a numeric vector with length 1, an error is thrown, indicating that it is not possible to
#' create names with an integer value and factor levels should be passed instead.
#'
#' @examples
#' x <- factor(c("A", "B", "A", "C"))
#' named.contr.sum(x)
#'
#' @author Matthew Flickinger, \href{http://www.matthewflickinger.com/}{http://www.matthewflickinger.com/}
#'
#' @references
#' Code was obtained from \href{https://stackoverflow.com/a/24516650}{https://stackoverflow.com/a/24516650}
#'
#' @export
named.contr.sum <-function(x, ...) {
  if (is.factor(x)) {
    x <- levels(x)
  } else if (is.numeric(x) & length(x) == 1L) {
    stop("cannot create names with integer value. Pass factor levels")
  }
  x <- contr.sum(x, ...)
  colnames(x) <- apply(x, 2, function(x)
    paste(names(x[x > 0]), names(x[x < 0]), sep = "\u2212")
  )
  x
}


