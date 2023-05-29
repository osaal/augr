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
# extract_data <- function(network, ..., unlock = FALSE, verbose = FALSE) {
#   # MOVED INTO augr.R
#   # 1. Recognise type of input
#   # 2. Extract relevant data from input
#   # 3. Combine with other inputted data (e.g., data matrix)
#   # 4. Return standardized data object for use
#
#   # Use this to extract number of levels for ordinal variables:
#   # levels <- length(unique(data[, var]))
#   # (Inspired by cor_auto in qgraph)
#
#   # Externality check
#   ext_check()
#
#   # Create return object with appropriate defaults
#   extracted <- list()
#   class(extracted) <- "augr_data"
#
#   funinput <- list(...)
#
#   # --- ARGUMENT EXTRACTION ----------------------------------------------------
#   # Save type of user-supplied network object
#   if("network" %in% names(funinput)) {
#     network <- funinput$network
#     if (verbose) { message ("Input contained network, processing network") }
#   }
#
#   # --- GRAPH OBJECT EXTRACTION ------------------------------------------------
#   if (is(network, "qgraph")) {
#     if (verbose) {
#       message("Input is qgraph, extracting wmat and varnames from graph")
#     }
#
#     wmat <- network$Arguments$input
#     varnames <- names(network$graphAttributes$Nodes$labels)
#     colnames(wmat) <- rownames(wmat) <- varnames
#   }
#   if (is(network, "bootnetResult")) {
#     if (verbose) {
#       message("Input is bootnetResult, extracting wmat and varnames from graph")
#     }
#
#     wmat <- network$graph
#     varnames <- network$labels
#     colnames(wmat) <- rownames(wmat) <- varnames
#   }
#   if (is(network, "bootnet")) {
#     if (verbose) {
#       message("Input is bootnet, extracting wmat and varnames from graph")
#     }
#
#     wmat <- network$sample$graph
#     varnames <- network$sample$labels
#     colnames(wmat) <- rownames(wmat) <- varnames
#   }
#   if (is(network, "psychonetrics")) {
#     if (verbose) {
#       message("Input is psychonetrics, extracting wmat and varnames from graph")
#     }
#
#     wmat <- network@modelmatrices[["fullsample"]][["omega"]]
#     varnames <- network@sample@variables[["label"]]
#   }
#   if (is(network, "matrix")) {
#     if (verbose) {
#       message("Input is matrix, returning as-is")
#     }
#
#     wmat <- network
#     varnames <- rownames(wmat) <- colnames(wmat)
#
#     if (is.null(varnames)) { message("No names found in input matrix!") }
#   }
#
#   if (is(network, "netupdate")) {
#     if (verbose) {
#       message("Input is netupdate, extracting wmat and varnames from graph")
#     }
#
#     wmat <- network$weights
#     varnames <- network$variables
#   }
#
#   # --- RETURN CONSTRUCTION ----------------------------------------------------
#
#   extracted$input$network <- network
#   extracted$input$type <- class(network)[1]
#   extracted$weights <- wmat
#   extracted$variables <- varnames
#
#   return(extracted)
# }
