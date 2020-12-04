regionalPlot <- function(region, data){

  ## Prophet forecast - can this be taken outside of the function?
  ## no, when it is taken outside of the model, the forecast is becomes
  ## much worse 2020-11-14
  df <- data %>%
    filter(regional_unit == region & datename < ymd("2020-03-23")) %>%
    transmute(ds = datename, y = n)

  m <- prophet::prophet(df, holidays = holidays,
                        seasonality.mode = "additive",# mcmc.samples = 1000,
                        weekly.seasonality = FALSE, daily.seasonality = FALSE)
  future <- make_future_dataframe(m, periods = 5, freq = 'month')
  fcst <- predict(m, future)

  ## Plotting
  plot <- fcsts %>%
    filter(regional_unit == region) %>%
    fabletools::autoplot(data, level = 80) + # fable forecasts over observed data
    geom_line(aes(y = c(rep(NA,50), fcst[51:55,]$yhat)), # prophet prediction
              col = "blue", lty = 3) +
    geom_ribbon(aes(ymin = c(rep(NA,50), fcst[51:55,]$yhat_lower), # prophet confidence
                    ymax = c(rep(NA,50), fcst[51:55,]$yhat_upper)),
                alpha = .3, fill = "blue") +
    geom_vline(xintercept = dmy("23-March-2020"), colour = "red") + # add line to mark the start of lockdown
    labs(title = region)

  ## Results
  result <- list("plots" = plot)#, "prophet" = fcst)
  return(result)
}
