regionalJointFits <- function(data){
  regions <- unique(data$regional_unit)

  plan(multisession)
  fable_fit <-
    future_map_dfr(.x = regions,
                   .f = ~fableModels(filter(data, regional_unit == .x)),
                   .options = furrr_options(seed = TRUE))
  message("fable fits")

  plan(sequential)

  prophet_fit <-
    map_dfr(.x = regions,
            .f = ~prophetModels(filter(data, regional_unit == .x)))
  message("prophet fits")

  message("prophet forecasts")

  joint_fit <-
    left_join(fable_fit, prophet_fit, by = "regional_unit")

  return(joint_fit)
}