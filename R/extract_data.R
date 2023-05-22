#' Title
#'
#' @param external
#' @param ...
#'
#' @importFrom methods is
#' @importFrom stats rbinom
#'
#' @return
#' @export
#'
#' @examples
extract_data <- function(external = FALSE, ...) {
  # 1. Recognise type of input
  # 2. Extract relevant data from input
  # 3. Combine with other inputted data (e.g., data matrix)
  # 4. Return standardized data object for use

  # Use this to extract number of levels for ordinal variables:
  # levels <- length(unique(data[, var]))
  # (Inspired by cor_auto in qgraph)

  # Externality check
  ext_check()

  # Create return object with appropriate defaults
  extracted <- list()
  class(extracted) <- "augr_data"

  funinput <- list(...)

  # Graph objects from other packages
  if (is(input, "qgraph")) {

  }
  if (is(input, "bootnetResult")) {

  }
  if (is(input, "bootnet")) {

  }
  if (is(input, "psychonetrics")) {

  }

  # R system classes
  if (is(input, "matrix")) {

  }
  if (is(input, "data.frame")) {
    # This also catches tibbles!
  }
}
