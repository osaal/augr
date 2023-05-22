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
extract_data <- function(..., unlock = FALSE) {
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

  # --- ARGUMENT EXTRACTION ----------------------------------------------------
  # Save type of user-supplied network object
  if("network" %in% names(funinput)) {
    network <- funinput$network
  }

  # Retrieve user-supplied ready material
  if("data"  %in% names(funinput)) { data  <- funinput$data  }
  if("wmat"  %in% names(funinput)) { wmat  <- funinput$wmat  }
  #if("scale" %in% names(funinput)) { scale <- funinput$scale }
  # TODO: Implement custom scale addition

  # --- GRAPH OBJECT EXTRACTION ------------------------------------------------
  # SHOULD THESE BE MOVED TO THEIR OWN FUNCTION?
  # Graph objects from other packages
  if (is(network, "qgraph")) {
    # TODO: Data must be explicitly given, note this
    # TODO: Unit test for wmat
    # TODO: Unit test for varnames
    if (verbose) { message("Input is qgraph, extracting wmat and varnames from graph") }

    data <- data
    wmat <- input$Arguments$input
    varnames <- names(input$graphAttributes$Nodes$labels)
    colnames(wmat) <- rownames(wmat) <- varnames
    # TODO: Extract qgraph information
  }
  if (is(network, "bootnetResult")) {
    # TODO: Extract bootnetResult information
    if (verbose) { message("Input is bootnetResult, extracting wmat and varnames from graph") }
  }
  if (is(network, "bootnet")) {

  }
  if (is(network, "psychonetrics")) {

  }

  # --- R SYSTEM CLASS EXTRACTION ----------------------------------------------
  # TODO: Check which object will be extracted here! It is not "network", but what else could it be?
  # R system classes
  if (is(data, "matrix")) {

  }
  if (is(data, "data.frame")) {
    # This also catches tibbles!
  }
}
