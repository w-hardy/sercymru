#### Packages ####
library(tidyverse)
library(rebus)

#### Data ####
data_files <-
  paste0("data/prescribing_data/", list.files("data/prescribing_data"))

names(data_files) <-
  str_remove_all(list.files("data/prescribing_data"), ".rds")


pres_data <-
  map_dfr(.x = data_files,
          .f = ~read_rds(.x),
          .id = "drug") %>%
  filter(!drug %in% c("quant_all_drugs", "warfarin_ddd", "warfarin_ddd_nat") &
           bnf_chem != "0407010F0") %>%
  select(-c(name, ddd_value:ddd_n))

ddd_data <- #https://www.whocc.no/atc_ddd_index/
  readxl::read_excel("data/described_daily_doses_whocc.xlsx", "Sheet1")


#### Preparing prescription data ####
ddd_pres_data <-
  pres_data %>%
  mutate(bnf_name = tolower(bnf_name)) %>%
  left_join(ddd_data, by = "bnf_chem") %>%

  # Create variables for the strength (value and unit) from disp data
  mutate(strength1 = str_remove_all(bnf_name, SPACE),
         strength = case_when(
           # Manual searches

           ## ddd_ace
           str_detect(strength1, "titrationpack") ~ "5mg", # There are several different titration packs available, the median strength in the packs is 5mg. tritace_titrationpack(tab2.5/5/10mg)" & Ramipril_Titration Pack (Tab 2.5/5/10mg) https://www.medicines.org.uk/emc/product/6252/smpc#PACKAGE

           ## ddd_cortico
           strength1 == "clenilmodulite_inha100mcg(200d)" ~ paste0(100*200, "mcg"),

           ## ddd_hcq

           ## ddd_nsaid

           ## ddd_oral_contra
           drug == "ddd_oral_contra" ~ "1d",

           ## ddd_paracet
           strength1 == "sudafeddualreliefmax_tab" ~ "200mg", # https://www.medicines.org.uk/emc/product/9174/smpc

           ## ddd_sal
           strength1 == "ventolin_evohaler100mcg(200d)" ~ paste0(100*200, "mcg"),

           ## ddd_ssri
           strength1 == "arthrotec50_tab" ~ "50mg", # https://www.medicines.org.uk/emc/product/6678/smpc
           strength1 == "arthrotec75_tab" ~ "75mg", # https://www.medicines.org.uk/emc/product/1144/smpc
           strength1 == "nurofenplus_tab" ~ "200mg", # https://www.medicines.org.uk/emc/product/5627/smpc
           strength1 == "nurofencold&flu_tab" ~ "200mg", # https://www.medicines.org.uk/emc/product/5626
           strength1 == "nurofenforchild_suspsachs/f(sbery)" ~ "200mg", # https://www.medicines.org.uk/emc/product/10459/smpc
           strength1 == "nurofenchild_coldpainfeversusp(sbery)" ~ "100mg", # https://www.medicines.org.uk/emc/product/3249/smpc
           strength1 == "nurofenchild_coldpainfeversusp(orng)" ~ "100mg", # https://www.medicines.org.uk/emc/product/3248/smpc
           strength1 == "care_ibuprofenforchildrenoralsusp" ~ "100mg", # https://www.medicines.org.uk/emc/product/4903

           ## ddd_vitd
           strength1 == "cacitd3_effgransach4gs/f" ~ "4000000u",
           strength1 == "calc&colecal_effgransach4gs/f" ~ "4000000u",
           strength1 == "ergocalciferol_tab12.5mcg" ~ paste0(12.5*40, "u"),
           strength1 == "ergocalciferol_tab250mcg" ~ paste0(250*40, "u"),
           strength1 == "ergocalciferol_tab125mcg" ~ paste0(125*40, "u"),
           strength1 == "calciferolstrong_tab1.25mg" ~ paste0(1.25*40, "u"),
           strength1 == "ergo-d2_tab12.5mcg" ~ paste0(12.5*40, "u"),
           strength1 == "ergo-d2_cap1.25mg" ~ paste0(1.25*40, "u"),
           strength1 == "calciferolhighstrgh_tab250mcg" ~ paste0(250*40, "u"),
           strength1 == "calciferol_tab500mcg" ~ paste0(500*40, "u"),


           strength1 == "fultium-d3_cap20000" ~ "20000u",
           strength1 == "dlux400_oralspy" ~ "400u",
           strength1 == "prod3folic_400cap" ~ "400u",
           strength1 == "accreted3_tab" ~ "10mcg", # https://www.medicines.org.uk/emc/product/2766/smpc
           strength1 == "calcichewd3oncedaily_tabchble1g/800u" ~ "800u", # https://www.medicines.org.uk/emc/product/12843/smpc
           strength1 == "calcichewd3fte_tabchble" ~ "400u", # https://www.medicines.org.uk/emc/product/12844/smpc
           strength1 == "calcichewd3_tabchble" ~ "200u", # https://www.medicines.org.uk/emc/product/12845/smpc
           strength1 == "calcichewd3_capl" ~ "400u", # https://bnf.nice.org.uk/drug/colecalciferol-with-calcium-carbonate.htm
           strength1 == "adcal-d3_tabchble" ~ "400u", # https://www.medicines.org.uk/emc/product/1356/smpc
           strength1 == "adcal-d3_tabchble(lem)" ~ "400u", # https://www.medicines.org.uk/emc/product/401/smpc
           strength1 == "calceos_tabchble" ~ "400u", # https://www.medicines.org.uk/emc/product/3747/smpc
           str_detect(strength1, "d3") &
             str_detect(strength1, one_or_more(char_range(0,9)) %R% "u") ~
             str_extract(str_remove_all(strength1, "d3"), one_or_more(char_range(0,9)) %R% "u"),
           str_detect(strength1, "sunvit") ~
             paste0(str_extract(str_remove_all(strength1, "d3"), one_or_more(char_range(0,9))), "u"),


           str_detect(strength1, "rivaroxaban_init") ~ "25.7mg", # This is a combination pack, the 1 instance in the data has quantity = 49 and items = 1. I believe that this is a 28 week pack, 21*15*2 + 7*20

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
         # Strength value
         strength_value = as.numeric(str_remove_all(strength, ALPHA)),
         # Strength unit
         strength_unit = str_remove_all(strength, DOT),
         strength_unit = str_remove_all(strength_unit, char_range(0,9)),
         quantity = if_else(str_detect(strength1, "titration_pack") &
                              quantity == items,  items * 50, quantity)) %>%  # When looking at the records for the titration packs to establish their strength, it appears that the quantity has been incorrectly reported for many cases as the number of items.

  # Make sure dispensing data strength is in same unit as DDD (mg or unit)
  mutate(strength_value_std =
           case_when(drug == "ddd_vitd" & strength_unit == "g" ~ strength_value * 1e6 * 40,
                     drug == "ddd_vitd" & strength_unit == "mg" ~ strength_value * 1e3 * 40,
                     drug == "ddd_vitd" & strength_unit == "mcg" ~ strength_value * 40,
                     drug == "ddd_vitd" & strength_unit == "u" ~ strength_value,
                     strength_unit == "g" ~ strength_value * 1000,
                     strength_unit == "mg" ~ strength_value,
                     strength_unit == "mcg" ~ strength_value / 1000,
                     strength_unit == "d" ~ strength_value),
         ddd_value_std =
           case_when(ddd_unit == "g" ~ ddd_value * 1000,
                     ddd_unit == "mg" ~ ddd_value,
                     ddd_unit == "mcg" ~ ddd_value / 1000,
                     ddd_unit == "unit" ~ ddd_value),
         ddd_n = case_when(
           drug == "ddd_oral_contra" ~ quantity * strength_value_std,
           drug != "ddd_oral_contra" ~ quantity * strength_value_std / ddd_value_std,
         ),
         quantity = ddd_n # This is what the serCymruTools functions expect
  )


map(.x = unique(ddd_pres_data$drug),
    .f = ~saveRDS(filter(ddd_pres_data, drug == .x),
                  paste0("data/prescribing_data/", .x, ".rds")))
