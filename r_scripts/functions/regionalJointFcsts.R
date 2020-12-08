regionalJointFcsts <- function(data, h ="5 months"){
  regions <- unique(data$regional_unit)
  plan(multisession)
  fable_fit <-
    future_map_dfr(.x = regions,
                   .f = ~fableModels(filter(data, regional_unit ==.x)),
                   .options = furrr_options(seed = TRUE))
  message("fable fits")

  fable_fcst <- fable_fit %>%
    forecast(h = h) %>%
    as_tibble()
  message("fable forecasts")


  prophet_fit <-
    future_map_dfr(.x = regions,
                   .f = ~prophetModels(filter(data, regional_unit ==.x)),
                   .options = furrr_options(seed = TRUE))
  message("prophet fits")

  prophet_fcst <-
    forecast(prophet_fit, h = h) %>%
    as_tibble()
  # Some prophet forecasts are giving daily forecasts. We want monthly forecasts.
  # This may also be influencing accuracy

  message("prophet forecasts")
  plan(sequential)

<<<<<<< HEAD
  # prophetModels now being run in parallel. Needed to reinstall prophet from
=======
  # prophetModels now being run in parallel. Needed to reinstall proophet from
>>>>>>> 3803eb99c31ac84a5d2cc9211e7c43700fcf4b8e
  # source

  joint_fit <-
    left_join(fable_fit, prophet_fit, by = "regional_unit")
  joint_fcst <-
    bind_rows(fable_fcst, prophet_fcst) %>%
    mutate(lb_95 = quantile(n, .025),
           lb_80 = quantile(n, .1),
           med = median(n),
           ub_80 = quantile(n, .9),
           ub_95 = quantile(n, .975),
           sd = distributional::variance(n)^2,
           dist = n,
           n = NULL)

  return(list(fits = joint_fit, forecasts = joint_fcst))
}

