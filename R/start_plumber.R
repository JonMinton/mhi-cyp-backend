start_plumber <- function(port = 8000) {
  plumber::pr(
    here::here("routes", "plumber.R")
  ) |>
    plumber::pr_run(port = port)
}
