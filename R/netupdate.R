#' Title
#'
#' @param network
#' @param data
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
    network,
    data,
    scale,
    type = c("standardized", "raw"),
    stdc = 1,
    verbose = TRUE,
    ...
    ) {

  # Store arguments for later
  arguments <- as.list(match.call())

  # Extract and reformat data for use inside function
  processed <- extract_data(network, unlock=TRUE)
  varnames <- processed$variables
  wmat <- processed$weights
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
