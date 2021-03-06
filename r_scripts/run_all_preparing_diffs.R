library(tidyverse)
library(rmarkdown)

rmd_files <- list.files("r_notebooks/preparing_differences")[grep(".Rmd", list.files("r_notebooks/preparing_differences"))]

map(.x = rmd_files,
    .f = ~render(paste0("r_notebooks/preparing_differences/", .x)))

