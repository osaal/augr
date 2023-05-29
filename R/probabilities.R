probabilities <- function(input) {

  if (!is(input, "augr_data")) {
    stop(paste0("Function uses augr_data as input, not ", class(input)))
  }

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
}
