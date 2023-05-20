#' Update data based on network configuration
#'
#' @param ... Stuff that goes in.
#'
#' @return Nothing.
#' @export
#'
#' @examples
netupdate <- function(
    input,
    data,
    wmat,
    scale,
    type = c("standardized", "raw"),
    stdc = 1
    ) {

  # netupdate only takes one graph at a time!
  if (is.vector(input)) { stop(paste0("Input must be a graph object, not a vector")) }
  if (is.list(input)) { stop(paste0("Input must be a graph object, not a list")) }

  # Extract relevant information
  if (is(input, "qgraph")) {
    # TODO: Extract qgraph information
  }
  if (is(input, "bootnetResult")) {
    # TODO: Extract bootnetResult information
  }


  varnames <- colnames(wmat)
  withr::local_options(list(scipen = 100)) # Local scope for options
  probabilities <- vector("list", length = length(varnames))
  result <- data
  class(result) <- "netupdate"

  for (i in seq_along(varnames)) {
    cumprob = 1
    for (j in seq_along(varnames)) {
      if (as.numeric(wmat[i,j]) != 0) {
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

      result[obs,var] <- value
    }
  }

  return(result)
}
