### --- AugrNetwork S4 CLASS DEFINITION ----------------------------------------

setClass(
  "AugrNetwork",
  slots = c(
    type = "character",
    weights = "matrix",
    variables = "character"
  ),
  prototype = list(
    type = NA_character_,
    weights = matrix(NA),
    variables = NA_character_
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

### --- END OF AugrNetwork -----------------------------------------------------

### --- AugrData S4 CLASS DEFINITION -------------------------------------------

setClass(
  "AugrData",
  slots = c(
    data = "matrix",
    level = "character",
    names = "character"
  ),
  prototype = list(
    data = matrix(NA),
    level = character(0),
    names = NA_character_
  )
)

AugrData <- function(data, level, names) {

  if (is.na(names)[1]) {
    names <- colnames({{ data }}) # Embrasure for safety
  } else {
    names <- names
  }

  if (inherits(data, "data.frame")) { data <- as.matrix(data) }

  methods::new(
    "AugrData",
    data = data,
    level = level,
    names = names
  )
}

### --- END OF AugrData --------------------------------------------------------

augr <- function(network, data, scale, type, nRounds = 100) {

  if(!is(network, "AugrNetwork")) { extract_network(network) }

  for (round in seq_along(nRounds)) {
    # Do something
  }
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
    } else {
      level <- "quantitative"
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
    level = level,
    names = names
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



