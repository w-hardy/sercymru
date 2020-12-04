regionalFableFits <- function(data, region){
  # Return various model objects for each region
  print(region)
  region <- enquo(region)

  data %>%
    filter(regional_unit == !!region) %>%
    fableModels()
}
