

#' Make rowwise id and metadata list
#' For making the contents of the metadata df more consistent with
#' expected json structure
#'
#' @param df a dataframe containing at least the id column.
#'
#'
#' @return all_id_metadata - a ragged list as long as the number of rows in df,
#' and where each entry contains id and metadata.
#' @export
#'
#' @examples
make_rowwise_id_metadata <- function(df){
  all_id_metadata <- vector("list", nrow(df))

  N <- nrow(df)

  for (i in 1:N){
    this_list <- list()
    this_list$id <- df$id[i]
    this_list$metadata <-
      df[i , which(colnames(df) != 'id')] |>
      as.list()
    all_id_metadata[[i]] <- this_list
  }

  rm(N, this_list)

  all_id_metadata
}


#' Make fake proportions data
#' Produce some fake proportions data to populate the expected structure
#'
#' @param time_periods How many periods to fake forwards/backwards
#' @param se_rate The amount of variation for producing CIs
#' @param trend_se The amount of variation in the trend component of a time series object
#' @param noise_se The amount of noise in the trend component of a time series object
#'
#' @return
#' @export
#'
#' @examples
make_fake_data <- function(
    time_periods = 10,
    se_rate = 5,
    trend_se = 0.02,
    noise_se = 0.05
) {
  # set.seed(12)
  #
  #
  # # z scores of point estimates are from standard normal distribution
  main_val_z <- rnorm(1)
  #
  # These are now converted to probability scale
  main_val <- main_val_z |> inv_logit()
  #
  # # The amount of variation is modelled from an exponential distribution, as it cannot be negative
  assoc_se_z <- rexp(1, rate = 5)
  #
  # # For intervals want to +/- 2 * the z value of assoc_se_z to main_vals_z
  # # then do inv_logit to get to probability scale
  #
  assoc_lower_z <- main_val_z - 2 * assoc_se_z
  assoc_upper_z <- main_val_z + 2 * assoc_se_z
  #
  assoc_lower <- inv_logit(assoc_lower_z)
  assoc_upper <- inv_logit(assoc_upper_z)
  #
  assoc_point <- main_val
  #
  # # For representing changes over time, we'll take the last value, and add a smaller random amount multiple times to get a random walk backwards
  #
  # We need the z_value, call this z_0

  z_0 <- main_val_z
  t_vals <- 1:10
  z_vals <- vector("numeric", 10)
  z_vals[1] <- z_0
  # Create a trend component
  trend_z <- rnorm(1, 0, 0.2)

  for (i in 2:10){
    noise_z <- rnorm(1, 0, 0.05)
    next_z <- z_vals[i-1] + trend_z * (i-1) + noise_z
    z_vals[i] <- next_z
  }

  y_vals <- inv_logit(z_vals)
  df_ts <- data.frame(
    t = t_vals,
    y = y_vals
  )

  data = list(
    main = main_val,
    ci = list(
      lower = assoc_lower,
      point = assoc_point,
      upper = assoc_upper
    ),
    time_series =
      df_ts
  )

  data
}


#' Add a data list element to an existing ragged list with id and metadata
#'
#' @param x
#' @param useFakeData
#'
#' @return
#' @export
#'
#' @examples
addData <- function(x, useFakeData = TRUE){
  if (isDataless(x)){
    data = NULL
  } else {
    # We now want to extend this a bit more ...
    if (useFakeData){
      data = make_fake_data()
    } else {
      stop("Real data not implemented yet!")
    }
  }
  list(id = x$id, metadata = x$metadata, data = data)
}

