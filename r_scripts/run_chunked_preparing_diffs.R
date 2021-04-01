library(tidyverse)
library(rmarkdown)

### Data

# Prescribing
data_files <-
  paste0("data/prescribing_data/", list.files("data/prescribing_data"))


# GP practice
gp <-
  read_rds("OneDrive/2_interimdata/8_linkeddata/gp_agesex_qof_combined.rds") %>%
  mutate(practice_id = as_factor(practiceid)) %>%
  ungroup() %>%
  janitor::clean_names() %>%
  select(practice_id, year)


# GP practice chunks of <= 20
unique_gp_ids <- as.character(unique(gp$practice_id))

split_gp_ids <-
  split(unique_gp_ids, ceiling(seq_along(unique_gp_ids) / 20)) %>% # Reduce this number if RAM requirements are too high
  map(unlist)

### Functions

render_files <- function(data){
  drug <- data %>%
    str_remove_all("data/prescribing_data/") %>%
    str_remove_all(".rds")

    map(.x = 1:length(split_gp_ids),
        .f = ~rmarkdown::render(input = "r_notebooks/preparing_differences/preparing_differences_template.Rmd",
                                output_file = paste0(drug, "_", .x, ".html"),
                                params =  list(data = data,
                                               drug = drug,
                                               practices = split_gp_ids[.x],
                                               iteration = .x)))}


### Map across combination of drugs and practices

# Dan
map(.x = data_files[3],
    .f = ~render_files(data = .x))

