regionalCV <- function(data, cv_dist = 6, init = 36, step = 1){

  # CV accuracy for fableModels() and prophetModels()

  regions <- unique(data$regional_unit) # Get unique regional_units for map()

  fableCV <- function(data){ # Return CV model accuracy for fableModels()
    data_test <-
      data %>%
      mutate(datename = yearmonth(datename))

    data_tr <-
      data %>%
      select(regional_unit, datename, n) %>%
      mutate(datename = yearmonth(datename)) %>%
      fill_gaps() %>%
      slice(1:(n()-cv_dist), .preserve = TRUE) %>%
      stretch_tsibble(.init = init, .step = step)

    data_tr %>%
      fableModels() %>%
      forecast(h = cv_dist) %>%
      accuracy(data_test)

  }

  prophetCV <- function(data){# Return CV model accuracy for prophetModels()
    data_test <-
      data %>%
      mutate(datename = yearmonth(datename))

    data_tr <-
      data %>%
      select(regional_unit, datename, n) %>%
      mutate(datename = yearmonth(datename)) %>%
      fill_gaps() %>%
      slice(1:(n()-cv_dist), .preserve = TRUE) %>%
      stretch_tsibble(.init = init, .step = step)

    data_tr %>%
      prophetModels() %>%
      forecast(h = cv_dist) %>%
      accuracy(data_test)
  }

  # Prophet models won't run with plan(multisession) so need to be run
  # sequentially. Is it worth trying to write this in Python and pass it back
  # to R?

    # Probably worth trying the prophet::cross_validation() method. If it
    # returns RMSE and is faster it will be useful.
  prophet_fits <-
    map_dfr(.x =  regions,
            .f = ~prophetCV(data = filter(data, regional_unit == .x)))
  message("prophetModels() complete")

  # Fable models will run using plan(multisession), but need to be run as below
  # with `furrr_options(seed = TRUE)` otherwise improper random numbers are
  # generated
  plan(multisession) # Set up parallel processing to decrease computation time
  fable_fits <-
    future_map_dfr(.x =  regions,
                   .f = ~fableCV(data = filter(data, regional_unit == .x)),
                   furrr_options(seed = TRUE))
  plan(sequential) # Close the multiprocess function to prevent memory leakage
  message("fableModels() complete")

  bind_rows(fable_fits, prophet_fits) %>%
    group_by(regional_unit) %>%
    arrange(RMSE, MAE)
}
