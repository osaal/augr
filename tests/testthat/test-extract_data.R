# EVERY INTERNAL FUNCTION MUST PASS THIS!
test_that("externality check", {
  rlang::env_poke(.GlobalEnv, "NETUPDATE_EXTERNAL_USE_FLAG", FALSE)
  expect_error(extract_data(external = FALSE))
})
