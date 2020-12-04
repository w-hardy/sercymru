regionalProphetFits <- function(data, region){
  # Return various model objects for each region
  region <- enquo(region)

  oc_prophet_model1 <-
    fable.prophet::prophet(n ~ growth("linear", n_changepoints = 0) +
                             season("year", type = "additive") +
                             holiday(holidays))
  oc_prophet_model2 <-
    fable.prophet::prophet(n ~ growth("linear", n_changepoints = 0) +
                             season("year", type = "multiplicative") +
                             holiday(holidays))
  oc_prophet_model3 <-
    fable.prophet::prophet(n ~ growth("linear",) +
                             season("year", type = "additive") +
                             holiday(holidays))
  oc_prophet_model4 <-
    fable.prophet::prophet(n ~ growth("linear",) +
                             season("year", type = "multiplicative") +
                             holiday(holidays))
  oc_prophet_model5 <-
    fable.prophet::prophet(n ~ season("year", type = "additive") +
                             holiday(holidays))
  oc_prophet_model6 <-
    fable.prophet::prophet(n ~ season("year", type = "multiplicative") +
                             holiday(holidays))
  oc_prophet_model7 <-
    fable.prophet::prophet(n ~ holiday(holidays))
  oc_prophet_model8 <-
    fable.prophet::prophet(n)

  data %>%
    filter(regional_unit == !!region) %>%
    model(oc_prophet_model1, oc_prophet_model2, oc_prophet_model3,
          oc_prophet_model4, oc_prophet_model5, oc_prophet_model6,
          oc_prophet_model7, oc_prophet_model8, .safely = TRUE)
}