# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#' NOTE THAT COLUMN NAMES CHANGE IN OCCUPATIONAL CHARACTERISTICS FILE, AND THE lmo_edition NEEDS TO BE CHANGED YEARLY

#constants-----------------
lmo_edition <- 2024
plus_five <- lmo_edition+5
plus_ten <- lmo_edition+10

# libraries--------------
library(tidyverse)
library(readxl)
library(openxlsx)
library(here)
library(janitor)
library(assertthat)
library(conflicted)
conflicts_prefer(dplyr::filter)
# functions------------
source(here("R", "functions.R"))
# read in the data----------------

occ_char <- read_excel(here("current_data",
                            "lmo",
                            list.files(here("current_data", "lmo"),
                                       pattern = "Occupational Characteristics")), skip=3)|>
  clean_names()|>
  mutate(noc_code=str_sub(noc, start=2), .after=noc)|>
  rename(
    construction_trades = occ_group_construction_trades, #WATCH OUT FOR POSSIBLE NAME CHANGE
    trades = occ_group_trades, #WATCH OUT FOR POSSIBLE NAME CHANGE
    stc =  occ_group_skilled_trades_certification #WATCH OUT FOR POSSIBLE NAME CHANGE
  )|>
  mutate(non_construction_trades=if_else(
    construction_trades=="Non-Construction Trades" &
      trades=="Trades",
    "Non-Construction Trades",
    NA_character_
      ))

noc_list <- occ_char %>%
  select(
    noc_code,
    description,
    ten_year_jo=paste0("lmo_job_openings_", lmo_edition, "_", plus_ten), #NAMES CHANGE YEARLY
    emp_now = paste0("lmo_employment_",lmo_edition), #NAMES CHANGE YEARLY
    emp_five = paste0("lmo_employment_", plus_five), #NAMES CHANGE YEARLY
    emp_ten = paste0("lmo_employment_", plus_ten), #NAMES CHANGE YEARLY
    non_construction_trades,
    construction_trades,
    trades,
    stc
  )%>%
  mutate(ffy_cagr=scales::percent((emp_five/emp_now)^(.2)-1, accuracy=.1),
         sfy_cagr=scales::percent((emp_ten/emp_five)^(.2)-1, accuracy=.1),
         ty_cagr=scales::percent((emp_ten/emp_now)^(.1)-1, accuracy=.1))%>%
  nest(data = everything()) %>%
  mutate(
    construction_trades = map(data, filter_and_select, construction_trades, "Construction Trades"),
    non_construction_trades = map(data, filter_and_select, non_construction_trades, "Non-Construction Trades"),
    stc = map(data, filter_and_select, stc, "STC Trades"),
    all_trades = map(data, filter_and_select, trades, "Trades")
  ) %>%
  select(-data) %>%
  pivot_longer(cols = everything())%>%
  mutate(value=map(value, fix_column_names))

mapping <- occ_char %>%
  select(
    noc_code,
    description,
    construction_trades,
    non_construction_trades,
    trades,
    stc
  )
write_csv(mapping, here("current_data","mapping","mapping.csv"))


active <- read_clean_join("Active")
cofq <- read_clean_join("CofQ")
new_reg <- read_clean_join("New")

new_reg%>%
  na.omit()%>%
  mutate(date=lubridate::ym(paste(year,str_sub(month,1,2),sep = "-")))%>%
  summarize(max(date, na.rm=TRUE))%>%
  pull()%>%
  write_rds(here::here("processed_data","max_date.rds"))


# active--------------
active_nested <- active %>%
  nest(data = everything())%>%
  mutate(
    construction_trades = map(data, filter_and_aggregate, construction_trades, "equal", "Construction Trades"),
    construction_trades__plot = map(construction_trades, make_plt, label=construction_trades, smooth=FALSE, title="Active Apprenticeships"),
    construction_trades__table = map(construction_trades, wider_with_totals),
    non_construction_trades = map(data, filter_and_aggregate, construction_trades, "equal", "Non-Construction Trades"),
    non_construction_trades__plot = map(non_construction_trades, make_plt, label=construction_trades, smooth=FALSE, title="Active Apprenticeships"),
    non_construction_trades__table = map(non_construction_trades, wider_with_totals),
    stc = map(data, filter_and_aggregate, stc, "equal", "STC Trades"),
    stc__plot = map(stc, make_plt, label=stc, smooth=FALSE, title="Active Apprenticeships"),
    stc__table = map(stc, wider_with_totals),
    all_trades = map(data, filter_and_aggregate, trades, "equal", "Trades"),
    all_trades__plot = map(all_trades, make_plt, label=trades, smooth=FALSE, title="Active Apprenticeships"),
    all_trades__table = map(all_trades, wider_with_totals),
    total = map(data, just_aggregate),
    total__plot = map(total, make_plt, label=group, smooth=FALSE, title="Active Apprenticeships"),
    total__table = map(total, wider_with_totals)
  ) %>%
  select(contains("table") | contains("plot")) %>%
  pivot_longer(cols = everything(), names_to = "name", values_to = "value") %>%
  separate(name, into = c("category", "thing"), sep = "__") %>%
  pivot_wider(names_from = thing, values_from = value)
# save active----------------
write_rds(active_nested, here::here("processed_data", "active_nested.rds"))

active_wb <- createWorkbook()
active_nested %>%
  mutate(
    walk(category, addsheet, wb = active_wb),
    walk(category, adjust_width, wb = active_wb),
    walk2(category, table, writeDataTable, wb = active_wb),
    walk2(plot, category, add_plot, wb = active_wb)
  )
noc_list %>%
  mutate(walk2(name, value, writeDataTable, wb = active_wb, startRow = 25))
saveWorkbook(active_wb, here("out", "current_output", str_replace_all(paste0("active_",lubridate::today(),".xlsx")," ","_")), overwrite = TRUE)
# cofq--------------------
cofq_nested <- cofq %>%
  nest(data = everything()) %>%
  mutate(
    construction_trades = map(data, filter_and_aggregate, construction_trades, "equal", "Construction Trades", grp = program_type),
    construction_trades__plot = map(construction_trades, make_plt, label = program_type, title="Certificates of Qualification"),
    construction_trades__table = map(construction_trades, wider_with_totals),
    non_construction_trades = map(data, filter_and_aggregate, construction_trades, "equal", "Non-Construction Trades", grp = program_type),
    non_construction_trades__plot = map(non_construction_trades, make_plt, label = program_type, title="Certificates of Qualification"),
    non_construction_trades__table = map(non_construction_trades, wider_with_totals),
    stc = map(data, filter_and_aggregate, stc, "equal", "STC Trades", grp = program_type),
    stc__plot = map(stc, make_plt, label = program_type, title = "Certificates of Qualification"),
    stc__table = map(stc, wider_with_totals),
    all_trades = map(data, filter_and_aggregate, trades, "equal", "Trades", grp = program_type),
    all_trades__plot = map(all_trades, make_plt, label = program_type, title="Certificates of Qualification"),
    all_trades__table = map(all_trades, wider_with_totals),
    total = map(data, just_aggregate, grp = program_type),
    total__plot = map(total, make_plt, label = program_type, title="Certificates of Qualification"),
    total__table = map(total, wider_with_totals)
  ) %>%
  select(contains("table") | contains("plot")) %>%
  pivot_longer(cols = everything(), names_to = "name", values_to = "value") %>%
  separate(name, into = c("category", "thing"), sep = "__") %>%
  pivot_wider(names_from = thing, values_from = value)
# save cofq------------------------
write_rds(cofq_nested, here::here("processed_data", "cofq_nested.rds"))

cofq_wb <- createWorkbook()
cofq_nested %>%
  mutate(
    walk(category, addsheet, wb = cofq_wb),
    walk(category, adjust_width, wb = cofq_wb),
    walk2(category, table, writeDataTable, wb = cofq_wb),
    walk2(plot, category, add_plot, wb = cofq_wb)
  )
noc_list %>%
  mutate(walk2(name, value, writeDataTable, wb = cofq_wb, startRow = 25))

saveWorkbook(cofq_wb, here("out", "current_output", str_replace_all(paste0("cofq_",lubridate::today(),".xlsx")," ","_")), overwrite = TRUE)


# new reg--------------
new_reg_nested <- new_reg %>%
  nest(data = everything()) %>%
  mutate(
    construction_trades = map(data, filter_and_aggregate, construction_trades, "not equal", "Non-Trades"),
    construction_trades__table = map(construction_trades, wider_with_totals),
    construction_trades__plot = map(construction_trades, make_plt, label = construction_trades, title = "New Registrations"),
    stc = map(data, filter_and_aggregate, stc, "equal", "STC Trades"),
    stc__plot = map(stc, make_plt, label = stc, title = "New Registrations"),
    stc__table = map(stc, wider_with_totals),
    all_trades = map(data, filter_and_aggregate, trades, "equal", "Trades"),
    all_trades__plot = map(all_trades, make_plt, label = trades, title="New Registrations"),
    all_trades__table = map(all_trades, wider_with_totals),
    total = map(data, just_aggregate),
    total__plot = map(total, make_plt, label = group, title = "New Registrations"),
    total__table = map(total, wider_with_totals)
  ) %>%
  select(contains("table") | contains("plot")) %>%
  pivot_longer(cols = everything(), names_to = "name", values_to = "value") %>%
  separate(name, into = c("category", "thing"), sep = "__") %>%
  pivot_wider(names_from = thing, values_from = value)
# save new_reg------------------
write_rds(new_reg_nested, here::here("processed_data", "new_reg_nested.rds"))
new_reg_wb <- createWorkbook()
new_reg_nested %>%
  mutate(
    walk(category, addsheet, wb = new_reg_wb),
    walk(category, adjust_width, wb = new_reg_wb),
    walk2(category, table, writeDataTable, wb = new_reg_wb),
    walk2(plot, category, add_plot, wb = new_reg_wb)
  )
noc_list %>%
  filter(name != "non_construction_trades") %>%
  mutate(walk2(name, value, writeDataTable, wb = new_reg_wb, startRow = 25))
non_construction_df <- noc_list %>%
  filter(name == "non_construction_trades") %>%
  pull(value)
writeDataTable(wb = new_reg_wb, sheet = "construction_trades", x = non_construction_df[[1]], startRow = 65)
saveWorkbook(new_reg_wb, here("out", "current_output", str_replace_all(paste0("new_reg_",lubridate::today(),".xlsx")," ","_")), overwrite = TRUE)

