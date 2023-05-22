#' Title
#'
#' @param input
#' @param data
#' @param wmat
#' @param scale
#' @param type
#' @param stdc
#' @param verbose
#'
#' @importFrom methods is
#' @importFrom stats rbinom
#'
#' @return
#' @export
#'
#' @examples
netupdate <- function(
    input,
    data,
    wmat,
    scale,
    type = c("standardized", "raw"),
    stdc = 1,
    verbose = TRUE
    ) {

  # Store arguments for later
  arguments <- as.list(match.call())

  # netupdate only takes one graph at a time
  # TODO: Avoid nesting?
  if (is.vector(input)) { stop("Input must be a graph object, not a vector") }
  if (is.list(input)) {
    if (!is(input, "qgraph")) {
      stop("Input must be a graph object, not a list")
    }
  }

  # Extract relevant information
  if (is(input, "qgraph")) {
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
  if (is(input, "bootnetResult")) {
    # TODO: Extract bootnetResult information
    if (verbose) { message("Input is bootnetResult, extracting wmat and varnames from graph") }
  }

  # EVERYTHING UNDER HERE IS UNTESTED!!!

  #varnames <- colnames(wmat)
  withr::local_options(list(scipen = 100)) # Local scope for options
  probabilities <- vector("list", length = length(varnames))

  # Initialise the result object for ease of access
  resultnames <- c("result", "data", "wmat", "probabilities", "arguments")
  result <- sapply(resultnames, function (x) NULL)
  class(result) <- "netupdate"
  result$arguments <- arguments
  result$data <- data
  result$wmat <- wmat

  # Determine updating probabilities for each connection
  for (i in seq_along(varnames)) {
    cumprob = 1
    for (j in seq_along(varnames)) {
      if (as.numeric(wmat[i,j])) {
        if (type == "standardized") {
          cumprob <- cumprob + as.numeric(wmat[i,j])
        } else {
          cumprob <- cumprob * as.numeric(wmat[i,j])
        }
      }
    }
    if (type == "standardized") {
      old <- cumprob
      cumprob <- tanh(cumprob/(1/length(varnames)))
      # TODO: Implement scaling with stdc in [0,1]
      print(paste("Old:", round(old, 2), " -> New:", round(cumprob, 2)))
    }
    probabilities[i] <- cumprob
  }

  result$probabilities <- probabilities
  simres <- matrix(nrow = nrow(data), ncol = length(varnames))

  # Update data based on probabilities
  for (obs in 1:nrow(data)) {
    for (var in seq_along(varnames)) {
      prob <- as.numeric(probabilities[var])
      draw <- rbinom(1, 1, abs(prob))
      if (!draw) { next }
      if (is.na(data[obs, varnames[var]])) { next }

      if(prob > 0) { value <- data[obs,varnames[var]] + scale[3] }
      if(prob < 0) { value <- data[obs,varnames[var]] - scale[3] }

      if(value > scale[2]) { value <- scale[2] }
      if(value < scale[1]) { value <- scale[1] }

      simres[obs,var] <- value
    }
  }
  result$result <- simres
  return(result)
}
