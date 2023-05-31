### --- AugrNetwork S4 CLASS DEFINITION ----------------------------------------

setClass(
  "AugrNetwork",
  slots = c(
    type = "character",
    weights = "matrix",
    variables = "character"
  ),
  prototype = list(
    type = character(0),
    weights = matrix(NA),
    variables = character(0)
  )
)

AugrNetwork <- function(type, weights, variables) {
  type <- if (length(type) >= 2) { type[1] } else { type }
  weights <- as.matrix(weights)
  variables <- as.character(variables)

  methods::new(
    "AugrNetwork",
    type = type,
    weights = weights,
    variables = variables)
}

### --- AugrData S4 CLASS DEFINITION -------------------------------------------

setClass(
  "AugrData",
  slots = c(
    data = "matrix",
    names = "character",
    level = "character",
    scale = "numeric"
  ),
  prototype = list(
    data = matrix(NA),
    names = character(0),
    level = character(0),
    scale = numeric(0)
  )
)

AugrData <- function(data, names, level, scale) {

  if (is.na(names)[1]) {
    names <- colnames({{ data }}) # Embrasure for safety
  } else {
    names <- names
  }

  if (inherits(data, "data.frame")) { data <- as.matrix(data) }

  methods::new(
    "AugrData",
    data = data,
    names = names,
    level = level,
    scale = scale
  )
}

### --- MAIN FUNCTIONS ---------------------------------------------------------
Augr <- function(data, network, nRounds = 100, ..., verbose = FALSE) {

  if(!is(network, "AugrNetwork")) {
    if (verbose) {
      cli::cli_alert_info("Network is not an AugrNetwork, running extract_network()")
    }
    network <- extract_network(network, unlock = T)
  }
  if(!is(data, "AugrData")) {
    if (verbose) {
      cli::cli_alert_info("Data is not AugrData, running extract_data()")
    }
    data <- extract_data(data, unlock = T)
  }

  if (verbose) {
    cli::cli_alert_info("Retrieving probabilities...")
  }
  probs <- probabilities(network, type = "raw", unlock = T)

  if (verbose) {
    cli::cli_alert_info("Starting updating...")
  }
  for (round in seq_len(nRounds)) {
    if (verbose) {
      cli::cli_alert_info("Round {round} of {nRounds} started...")
    }
    data <- update_data(data, probs = probs, unlock = T)
    if (verbose) {
      cli::cli_alert_info("...done!")
    }
  }
  if (verbose) {
    cli::cli_alert_info("Updating done!")
  }
  return(list(data = data, network = network, nRounds = nRounds))
}

extract_data <- function(data, ..., unlock = FALSE, verbose = FALSE) {
  # Constructs an AugrData object.

  dots <- list(...)

  if ("names" %in% names(dots)) {
    names <- dots$names
  } else {
    names <- colnames(data)
  }

  if ("level" %in% names(dots)) {
    if (!is.character(dots$level)) {
      cli::cli_abort(c("Level must be a character vector"))
    }
    level <- dots$level <- stringr::str_to_lower(dots$level)
  } else {
    if (max(data) - min(data) <= 6) {
      level <- "ordinal"
      scale <- c(min(data), max(data), 1)
    } else {
      level <- "quantitative"
      scale <- NA
    }
  }

  if (!inherits(data, "data.frame")) {
    cli::cli_warn(c(
      "Supplied data might not be suitable for Augr",
      "i" = "Make sure the data is a matrix, tibble or data frame"
    ))
  }

  dataobject <- AugrData(
    data = data,
    names = names,
    level = level,
    scale = scale
  )

  if (verbose) {
    cli::cli_alert_info(c(
      "Extracted data of type {class(dataobject@data)} with ",
      "{nrow(dataobject@data)} rows and {ncol(dataobject@data)} variables. \n",
      "{cli::symbol$bullet} Variable level defined as {dataobject@level}."
    ))
  }

  return (dataobject)

}

extract_network <- function(network, ..., unlock = FALSE, verbose = FALSE) {
  # 1. Recognise type of input
  # 2. Extract relevant data from input
  # 3. Return AugrNetwork object for use

  # Use this to extract number of levels for ordinal variables:
  # levels <- length(unique(data[, var]))
  # (Inspired by cor_auto in qgraph)

  # Externality check
  ext_check()

  # Initialize return object and save dots
  extracted <- AugrNetwork(
    type = NA_character_,
    weights = matrix(NA),
    variables = NA_character_
  )
  dots <- list(...)

  # You had one job.
  if (missing(network)) {
    cli::cli_abort(c(
      "Function requires a 'network' object",
      "i" = "See ?extract_network for a list of supported networks."
    ))
  }

  extracted@type <- class(network)[1]
  if (verbose) { message ("Input contained network, processing network") }

  # --- GRAPH OBJECT EXTRACTION ------------------------------------------------
  if (is(network, "qgraph")) {
    if (verbose) {
      message("Input is qgraph, extracting wmat and varnames from graph")
    }
    extracted@weights <- network$Arguments$input
    extracted@variables <- names(network$graphAttributes$Nodes$labels)
  }

  if (is(network, "bootnetResult")) {
    if (verbose) {
      message("Input is bootnetResult, extracting wmat and varnames from graph")
    }

    extracted@weights <- network$graph
    extracted@variables <- network$labels
  }

  if (is(network, "bootnet")) {
    if (verbose) {
      message("Input is bootnet, extracting wmat and varnames from graph")
    }

    extracted@weights <- network$sample$graph
    extracted@variables <- network$sample$labels
  }

  if (is(network, "psychonetrics")) {
    if (verbose) {
      message("Input is psychonetrics, extracting wmat and varnames from graph")
    }

    extracted@weights <- network@modelmatrices[["fullsample"]][["omega"]]
    extracted@variables <- network@sample@variables[["label"]]
  }

  if (is(network, "matrix")) {
    if (verbose) {
      message("Input is matrix, returning as-is")
    }

    extracted@weights <- network
    extracted@variables <- colnames(network)

    if (is.null(colnames(network))) {
      message("No names found in input matrix!")
    }
  }

  # DEPRECATED, REMOVE WHEN SAFE
  if (is(network, "netupdate")) {
    if (verbose) {
      message("Input is netupdate, extracting wmat and varnames from graph")
    }

    extracted@weights <- network$weights
    extracted@variables <- network$variables
  }

  if (verbose) {
    cli::cli_alert_info(c(
      "Extracted {extracted@type} network with {length(extracted@variables)} ",
      "variable{?s}"
    ))
  }

  return(extracted)

}

probabilities <- function(
    network,
    type = c("standardized", "raw"),
    ...,
    unlock = FALSE,
    verbose = FALSE
  ) {

  # Externality check
  ext_check()

  # Preinitialization
  Nvars <- length(network@variables)
  probabilities <- numeric(0)

  if (!is(network, "AugrNetwork")) {
    stop(paste0("Function uses AugrNetwork as input, not ", class(network)))
  }

  # Determine updating probabilities for each connection
  for (i in seq_len(Nvars)) {
    cumprob = 1
    for (j in seq_len(Nvars)) {
      if (network@weights[i,j] != 0) {
        if (type == "standardized") {
          cumprob <- cumprob + network@weights[i,j]
        } else {
          cumprob <- cumprob * network@weights[i,j]
        }
      }
    }
    if (type == "standardized") {
      old <- cumprob
      cumprob <- tanh(cumprob/(1/Nvars))
      if (verbose) {
        cli::cli_alert_info("Node {i} adjusted from {old} to {cumprob}")
      }
  }

    probabilities[i] <- cumprob

    if (verbose) {
      cli::cli_alert_info("Node {i} probability: {probabilities[i]}")
    }

  }

  return(probabilities)

}

update_data <- function(input, probs, ..., unlock = FALSE, verbose = FALSE) {

  # Externality check
  ext_check()

  # Data extraction
  data <- input@data
  varnames <- input@names
  Nvars <- length(varnames)
  Nobs <- nrow(data)
  level <- input@level
  scale <- input@scale

  if (ncol(data) != Nvars) {
    cli::cli_abort(c(
      "Name count and column count does not match!",
      "i" = "Check if your AugrData is defined correctly"
    ))
  }

  # Pre-initialization
  simres <- matrix(nrow = nrow(data), ncol = ncol(data))
  colnames(simres) <- varnames
  value <- numeric(0)

  for (obs in seq_len(Nobs)) {
    for (var in seq_len(Nvars)) {
      if (is.na(data[obs, var])) { next }

      prob <- probs[var]
      draw <- rbinom(1, 1, abs(prob))

      if (!draw) {
        simres[obs,var] = data[obs,var]
        next
      }

      if(prob > 0) { value <- data[obs,var] + scale[3] }
      if(prob < 0) { value <- data[obs,var] - scale[3] }

      if(value > scale[2]) { value <- scale[2] }
      if(value < scale[1]) { value <- scale[1] }

      simres[obs,var] <- value
    }
  }
  result <- AugrData(
    data = simres,
    names = varnames,
    level = level,
    scale = scale
  )
  return(result)
}
