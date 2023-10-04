inv_logit <- function(x) 1 / (1 + exp(-x))
set.seed(12)


# z scores of point estimates are from standard normal distribution
main_vals_z <-
  nrow(dta) |>
  rnorm()

# These are now converted to probability scale
main_vals <-
  main_vals_z |>
  inv_logit()

# The amount of variation is modelled from an exponential distribution, as it cannot be negative
assoc_se_z <-
  nrow(dta) |>
  rexp(rate = 5)

# For intervals want to +/- 2 * the z value of assoc_se_z to main_vals_z
# then do inv_logit to get to probability scale

assoc_lower_z <- main_vals - 2 * assoc_se_z
assoc_upper_z <- main_vals + 2 * assoc_se_z

assoc_lower <- inv_logit(assoc_lower_z)
assoc_upper <- inv_logit(assoc_upper_z)

assoc_point <- main_vals

# For representing changes over time, we'll take the last value, and add a smaller random amount multiple times to get a random walk backwards

ts_list <- vector("list", nrow(dta))

for (i in 1:nrow(dta)) {
  time_vec <- vector("numeric", 10)
  init_val <- main_vals[i]
  time_vec[10] <- init_val
  trend <- rnorm(1, 0, 0.02)

  for (j in 9:1){
    change <- rnorm(1, trend, 0.1)
    time_vec[j] <- time_vec[j+1] + change
  }
  ts_list[[i]] <- time_vec
}

# plot(ts_list[[12]], type = "l", ylim = c(-1.2, 1.2))
# for (i in 2:length(ts_list)) lines(ts_list[[i]])


# These now look fairly reasonable

# How should they be saved?

data_list <- vector("list", length = nrow(dta))

for (i in 1:length(data_list)) {
  data_list[[i]] <-
    list(
      main = main_vals[i],
      cis = list(
        lower = assoc_lower[i],
        point = assoc_point[i],
        upper = assoc_upper[i]
      ),
      trend = list(
        period = 1:10,
        value = ts_list[[i]]
      )
    )
}


overall_list <- vector("list", length = nrow(dta))

for (i in 1:nrow(dta)) {
  overall_list[[i]] <-
    list(
      id = i,
      metadata = dta[i,-1] |> as.list(),
      data = data_list[[i]]
    )
}

test_indicator_data <- jsonlite::toJSON(overall_list)

# We can check this has the expected structure using the listviewer::jsonedit function

# listviewer::jsonedit(overall_list_json, height = "800px", mode = "view")

usethis::use_data(test_indicator_data, overwrite = TRUE)
