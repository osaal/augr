
# Set global environment flag for using internal functions externally.
# These give quick access to devs, so that the flag does not need to be set
# every time a function is run.

enable_internals <- function() {
  rlang::env_poke(.GlobalEnv, "NETUPDATE_EXTERNAL_USE_FLAG", TRUE)
  message("Usage of internal functions enabled with global environment flag")
}

disable_internals <- function() {
  rlang::env_poke(.GlobalEnv, "NETUPDATE_EXTERNAL_USE_FLAG", FALSE)
  message("Usage of internal functions disabled with global environment flag")
}

enable_verbose <- function() {
  rlang::env_poke(.GlobalEnv, "NETUPDATE_VERBOSE_FLAG", TRUE)
  message("Verbose output enabled with global environment flag")
}

disable_verbose <- function() {
  rlang::env_poke(.GlobalEnv, "NETUPDATE_VERBOSE_FLAG", FALSE)
  message("Verbose output disabled with global environment flag")
}

# Externality checker - used in all internal-only functions

ext_check <- function() {
  caller <- as.list(sys.call(-1))[[1]]
  external <- rlang::caller_env()$unlock
  FLAG <- get0(
    "NETUPDATE_EXTERNAL_USE_FLAG",
    envir = .GlobalEnv,
    inherits = FALSE,
    ifnotfound = FALSE)
  if (!external & !FLAG) {
    return(
      cli::cli_abort(c(
        "{.fn {caller}} is currently locked!",
        "i" = "Did you unlock the internal function for external use?"
      ))
    )
  }
}
