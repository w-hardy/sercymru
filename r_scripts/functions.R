if(getwd() == "/Users/willhardy/OneDrive - Outreach Rescue/covid_prescribing/sercymru"){
map(.x = list.files("r_scripts/functions"),
    .f = ~source(paste0("r_scripts/functions/", .x)))
} else if (getwd() == "/Users/willhardy/OneDrive - Outreach Rescue/covid_prescribing/sercymru/r_notebooks") {
  map(.x = list.files("../r_scripts/functions"),
      .f = ~source(paste0("../r_scripts/functions/", .x)))
} else{
  print("Check wd is set correctly in functions.r")
  }


