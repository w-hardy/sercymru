#### Packages ####
library(tidyverse)
library(rebus)
library(fuzzyjoin)

#### Data ####
data_files <-
  paste0("data/prescribing_data/", list.files("data/prescribing_data"))
names(data_files) <-
  str_remove_all(list.files("data/prescribing_data"), ".rds")

pres_data <-
  map_dfr(.x = data_files,
          .f = ~read_rds(.x),
          .id = "drug") %>%
  select(-c(ddd, adq, period, year, year2, month, month2, quarter, quarter2, county)) %>%
  filter(!drug %in% c("all_drugs", "warfarin_ddd")) %>%
  rename(bnf_chem = bnfchem)

gp <-
  read_rds("data/gp_agesex_qof_combined.rds")

ddd_data <- #https://www.whocc.no/atc_ddd_index/
  readxl::read_excel("data/described_daily_doses_whocc.xlsx", "Sheet1")


#### Preparing prescription data ####
ddd_pres_data <-
  pres_data %>%
  mutate(drug = paste0("ddd_", drug),
         bnf_name = tolower(bnf_name)) %>%
  left_join(ddd_data, by = "bnf_chem") %>%
  mutate(strength1 = str_remove_all(bnf_name, SPACE),
         strength = case_when(
           drug == "oral_contra" ~ "1d",
           strength1 == "arthrotec50_tab" ~ "50mg", # https://www.medicines.org.uk/emc/product/6678/smpc
           strength1 == "arthrotec75_tab" ~ "75mg", # https://www.medicines.org.uk/emc/product/1144/smpc
           strength1 == "nurofenplus_tab" ~ "200mg", # https://www.medicines.org.uk/emc/product/5627/smpc
           strength1 == "nurofencold&flu_tab" ~ "200mg", # https://www.medicines.org.uk/emc/product/5626
           strength1 == "nurofenforchild_suspsachs/f(sbery)" ~ "200mg", # https://www.medicines.org.uk/emc/product/10459/smpc
           strength1 == "nurofenchild_coldpainfeversusp(sbery)" ~ "100mg", # https://www.medicines.org.uk/emc/product/3249/smpc
           strength1 == "nurofenchild_coldpainfeversusp(orng)" ~ "100mg", # https://www.medicines.org.uk/emc/product/3248/smpc
           strength1 == "care_ibuprofenforchildrenoralsusp" ~ "100mg", # https://www.medicines.org.uk/emc/product/4903
           strength1 == "sudafeddualreliefmax_tab" ~ "200mg", # https://www.medicines.org.uk/emc/product/9174/smpc
           strength1 == "fultium-d3_cap20000" ~ "20000u",
           strength1 == "dlux400_oralspy" ~ "400u",
           strength1 == "prod3folic_400cap" ~ "400u",
           strength1 == "accreted3_tab" ~ "10mcg", # https://www.medicines.org.uk/emc/product/2766/smpc
           str_detect(strength1, "rivaroxaban_init") ~ "25.7mg", # This is a combination pack, the 1 instance in the data has quantity = 49 and items = 1. I believe that this is a 28 week pack, 21*15*2 + 7*20
           str_detect(strength1, "titrationpack") ~ "5mg", # There are several different titration packs available, the median strength in the packs is 5mg. tritace_titrationpack(tab2.5/5/10mg)" & Ramipril_Titration Pack (Tab 2.5/5/10mg) https://www.medicines.org.uk/emc/product/6252/smpc#PACKAGE
           str_detect(strength1, "d3") &
             str_detect(strength1, one_or_more(char_range(0,9)) %R% "u") ~
             str_extract(str_remove_all(strength1, "d3"),
                         one_or_more(char_range(0,9)) %R% "u"),
           str_detect(strength1, "sunvit") ~
             paste0(str_extract(str_remove_all(strength1, "d3"),
                                one_or_more(char_range(0,9))), "u"),
           str_detect(strength1, one_or_more(char_range(0,9)) %R% "d") ~
             str_extract(strength1, one_or_more(char_range(0,9)) %R% "d"),
           str_detect(strength1, one_or_more(char_range(0,9)) %R% "u") ~
             str_extract(strength1, one_or_more(char_range(0,9)) %R% "u"),
           str_detect(strength1, "units/") ~
             str_extract(strength1, one_or_more(char_range(0,9)) %R% "u"),
           str_detect(strength1, "mg") ~
             str_extract(strength1, one_or_more(char_range(0,9)) %R%
                           optional(DOT) %R%
                           optional(one_or_more(char_range(0,9))) %R% "mg"),
           str_detect(strength1, "mcg") ~
             str_extract(strength1, one_or_more(char_range(0,9)) %R%
                           optional(DOT) %R%
                           optional(one_or_more(char_range(0,9))) %R% "mcg"),
           str_detect(strength1, "g") ~
             str_extract(strength1, one_or_more(char_range(0,9)) %R%
                           optional(DOT) %R%
                           optional(one_or_more(char_range(0,9))) %R% "g")),
         strength_value = as.numeric(str_remove_all(strength, ALPHA)),
         strength_unit = str_remove_all(strength, DOT),
         strength_unit = str_remove_all(strength_unit, char_range(0,9)),
         quantity = if_else(str_detect(strength1, "titration_pack") &
                              quantity == items,  items*50, quantity), # When looking at the records for the titration packs to establish their strength, it appears that the quantity has been incorrectly reported for many cases as the number of items.
         ddd_n = case_when(
           strength_unit == "d" ~ quantity * strength_value,
           strength_unit == "u" & drug == "vitd" ~
             quantity * (strength_value/40),
           ddd_unit == "unit" ~ quantity / ddd_value,
           strength_unit == "mcg" & ddd_unit == "mg" ~
             quantity * (strength_value/1000) / ddd_value,
           strength_unit == "mg" & ddd_unit == "g" ~
             quantity * (strength_value/1000) / ddd_value,
           ddd_unit == strength_unit ~ quantity * strength_value / ddd_value),
         quantity = ddd_n)

map(.x = unique(ddd_pres_data$drug),
    .f = ~saveRDS(filter(ddd_pres_data, drug == .x),
                  paste0("data/prescribing_data/", .x, ".rds")))

