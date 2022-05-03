#### Packages ####
library(tidyverse)
library(rebus)
library(fuzzyjoin)

#### Data ####
who_ddd <- readxl::read_excel("data/described_daily_doses_whocc.xlsx", "bnf_code_only") #https://www.whocc.no/atc_ddd_index/
oac <-
  read_rds("data/presdata_updates_040121/presc_wales_OAC.rds") %>%
  select(-c(HB, Locality, NIC, ActCost, DDD, ADQ, Period, year, year2, month,
            month2, quarter, quarter2)) %>%
  janitor::clean_names() %>%
  mutate(bnf_name = tolower(bnf_name),
         # Create variables for the strength (value and unit) from disp data
         strength = case_when(
           str_detect(bnf_name, "rivaroxaban_init ") ~ "25.7mg", # This is a combination pack, the 1 instance in the data has quantity = 49 and items = 1. I believe that this is a 28 week pack, 21*15*2 + 7*20
           str_detect(bnf_name, "tab") ~  word(bnf_name, -1),
           str_detect(bnf_name, "cap") ~  word(bnf_name, -1),
           str_detect(bnf_name, "susp") ~
             str_extract(bnf_name, SPACE %R%
                           one_or_more(char_range(0,9)) %R%
                           one_or_more(ALNUM)),
           str_detect(bnf_name, "spec") ~
             str_extract(bnf_name, SPACE %R%
                           one_or_more(char_range(0,9)) %R%
                           one_or_more(ALNUM))),
         strength_value = str_remove_all(strength, one_or_more(ALPHA)),
         strength_unit = str_remove_all(strength, DOT),
         strength_unit = str_remove_all(strength_unit, one_or_more(char_range(0,9))),
         across(starts_with("strength_"), ~str_remove_all(., SPACE)),
         strength_value = as.numeric(strength_value),
         across(.cols = c(practice_id:bnf_name, chemname), ~as.factor(.x))) %>%
  # Join DDD data
  left_join(select(who_ddd, c(bnf_chem, ddd_value, ddd_unit)),
            by = c("bnfchem" = "bnf_chem")) %>%
  # Make sure dispensing data strength is in same unit as DDD
  mutate(strength_value_std =
           case_when(strength_unit == "g" ~ strength_value * 1000,
                     strength_unit == "mg" ~ strength_value,
                     strength_unit == "mcg" ~ strength_value / 1000),
         ddd_value_std =
           case_when(ddd_unit == "g" ~ ddd_value * 1000,
                     ddd_unit == "mg" ~ ddd_value,
                     ddd_unit == "mcg" ~ ddd_value / 1000)) %>%
  mutate(n_ddd = quantity * strength_value_std / ddd_value_std,
         warfarin = if_else(str_detect(bnfchem, "0208020V0"), 1, 0))


#### By Practice ####
oac %>%
  group_by(practice_id, datename) %>%
  summarise(quantity = 100*sum((warfarin * n_ddd)) / sum(n_ddd)) %>%
  saveRDS("data/prescribing_data/warfarin_ddd.rds")


#### National ####
oac %>%
  group_by(datename) %>%
  summarise(quantity = 100*sum(warfarin * n_ddd) / sum(n_ddd)) %>%
  saveRDS("data/prescribing_data/warfarin_ddd_nat.rds")
