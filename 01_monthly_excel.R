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

# libraries--------------
library(tidyverse)
library(readxl)
library(openxlsx)
library(here)
library(janitor)
library(assertthat)
# functions------------
source(here("R", "functions.R"))

# read in the data----------------
occ_char <- read_csv(here("data", "current_lmo", list.files(here("data", "current_lmo"), pattern = "LMO_occ_char"))) %>%
  clean_names()%>%
  rename(
    noc_code = noc,
    construction_trades = occ_group_construction_trades,
    trades = occ_group_trades,
    stc = occ_group_trades_mandatory_certification
  )

noc_list <- occ_char %>%
  select(
    noc = noc_code,
    description,
    lmo_job_openings_2021_2031,
    `2021 Employment` = x2021_employment,
    `2031 Employment` = x2031_employment,
    `2021-2026 CAGR` = x1st_5_year_cagr,
    `2026-2031 CAGR` = x2nd_5_year_cagr,
    `2021-2031 CAGR` = x10_year_cagr,
    construction_trades,
    trades,
    stc
  ) %>%
  nest(data = everything()) %>%
  mutate(
    construction_trades = map(data, filter_and_select, construction_trades, "Construction Trades"),
    non_construction_trades = map(data, filter_and_select, construction_trades, "Non-Construction Trades"),
    stc = map(data, filter_and_select, stc, "First Ten MC Trades"),
    all_trades = map(data, filter_and_select, trades, "Trades")
  ) %>%
  select(-data) %>%
  pivot_longer(cols = everything())

mapping <- occ_char %>%
  select(
    noc_code,
    construction_trades,
    trades,
    stc
  )

active <- read_clean_join("Active")
cofq <- read_clean_join("CofQ")
new_reg <- read_clean_join("New")

# active--------------

active_nested <- active %>%
  nest(data = everything())%>%
  mutate(
    construction_trades = map(data, filter_and_aggregate, construction_trades, "equal", "Construction Trades"),
    construction_trades__plot = map(construction_trades, make_plt, construction_trades, FALSE, "Active Apprenticeships"),
    construction_trades__table = map(construction_trades, wider_with_totals),
    non_construction_trades = map(data, filter_and_aggregate, construction_trades, "equal", "Non-Construction Trades"),
    non_construction_trades__plot = map(non_construction_trades, make_plt, construction_trades, FALSE, "Active Apprenticeships"),
    non_construction_trades__table = map(non_construction_trades, wider_with_totals),
    stc = map(data, filter_and_aggregate, stc, "equal", "First Ten MC Trades"),
    stc__plot = map(stc, make_plt, stc, FALSE, "Active Apprenticeships"),
    stc__table = map(stc, wider_with_totals),
    all_trades = map(data, filter_and_aggregate, trades, "equal", "Trades"),
    all_trades__plot = map(all_trades, make_plt, trades, FALSE, "Active Apprenticeships"),
    all_trades__table = map(all_trades, wider_with_totals),
    total = map(data, just_aggregate),
    total__plot = map(total, make_plt, group, FALSE, "Active Apprenticeships"),
    total__table = map(total, wider_with_totals)
  ) %>%
  select(contains("table") | contains("plot")) %>%
  pivot_longer(cols = everything(), names_to = "name", values_to = "value") %>%
  separate(name, into = c("category", "thing"), sep = "__") %>%
  pivot_wider(names_from = thing, values_from = value)

# save active to excel-------------
active_wb <- createWorkbook()
active_nested %>%
  mutate(
    walk(category, addsheet, wb = active_wb),
    walk(category, adjust_width, wb = active_wb),
    walk2(category, table, writeDataTable, wb = active_wb),
    walk2(plot, category, add_plot, wb = active_wb)
  )

noc_list %>%
  mutate(walk2(name, value, writeDataTable, wb = active_wb, startRow = 19))
saveWorkbook(active_wb, here("out", "current_output", str_replace_all(paste0("active_",date(),".xlsx")," ","_")), overwrite = TRUE)

# cofq--------------------

cofq_nested <- cofq %>%
  nest(data = everything()) %>%
  mutate(
    construction_trades = map(data, filter_and_aggregate, construction_trades, "equal", "Construction Trades", program_type),
    construction_trades__plot = map(construction_trades, make_plt, program_type, ttl="Certificates of Qualification"),
    construction_trades__table = map(construction_trades, wider_with_totals),
    non_construction_trades = map(data, filter_and_aggregate, construction_trades, "equal", "Non-Construction Trades", program_type),
    non_construction_trades__plot = map(non_construction_trades, make_plt, program_type, ttl="Certificates of Qualification"),
    non_construction_trades__table = map(non_construction_trades, wider_with_totals),
    stc = map(data, filter_and_aggregate, stc, "equal", "First Ten MC Trades", program_type),
    stc__plot = map(stc, make_plt, program_type, ttl="Certificates of Qualification"),
    stc__table = map(stc, wider_with_totals),
    all_trades = map(data, filter_and_aggregate, trades, "equal", "Trades", program_type),
    all_trades__plot = map(all_trades, make_plt, program_type, ttl="Certificates of Qualification"),
    all_trades__table = map(all_trades, wider_with_totals),
    total = map(data, just_aggregate, program_type),
    total__plot = map(total, make_plt, program_type, ttl="Certificates of Qualification"),
    total__table = map(total, wider_with_totals)
  ) %>%
  select(contains("table") | contains("plot")) %>%
  pivot_longer(cols = everything(), names_to = "name", values_to = "value") %>%
  separate(name, into = c("category", "thing"), sep = "__") %>%
  pivot_wider(names_from = thing, values_from = value)

# save cofq to excel-----------------------
cofq_wb <- createWorkbook()
cofq_nested %>%
  mutate(
    walk(category, addsheet, wb = cofq_wb),
    walk(category, adjust_width, wb = cofq_wb),
    walk2(category, table, writeDataTable, wb = cofq_wb),
    walk2(plot, category, add_plot, wb = cofq_wb)
  )

noc_list %>%
  mutate(walk2(name, value, writeDataTable, wb = cofq_wb, startRow = 19))
saveWorkbook(cofq_wb, here("out", "current_output", str_replace_all(paste0("cofq_",date(),".xlsx")," ","_")), overwrite = TRUE)
# new reg--------------

new_reg_nested <- new_reg %>%
  nest(data = everything()) %>%
  mutate(
    construction_trades = map(data, filter_and_aggregate, construction_trades, "not equal", "Non-Trades"),
    construction_trades__table = map(construction_trades, wider_with_totals),
    construction_trades__plot = map(construction_trades, make_plt, construction_trades, ttl="New Registrations"),
    stc = map(data, filter_and_aggregate, stc, "equal", "First Ten MC Trades"),
    stc__plot = map(stc, make_plt, stc, ttl="New Registrations"),
    stc__table = map(stc, wider_with_totals),
    all_trades = map(data, filter_and_aggregate, trades, "equal", "Trades"),
    all_trades__plot = map(all_trades, make_plt, trades, ttl="New Registrations"),
    all_trades__table = map(all_trades, wider_with_totals),
    total = map(data, just_aggregate),
    total__plot = map(total, make_plt, group, ttl="New Registrations"),
    total__table = map(total, wider_with_totals)
  ) %>%
  select(contains("table") | contains("plot")) %>%
  pivot_longer(cols = everything(), names_to = "name", values_to = "value") %>%
  separate(name, into = c("category", "thing"), sep = "__") %>%
  pivot_wider(names_from = thing, values_from = value)

# save new_reg to excel-----------------------
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
  mutate(walk2(name, value, writeDataTable, wb = new_reg_wb, startRow = 19))
non_construction_df <- noc_list %>%
  filter(name == "non_construction_trades") %>%
  pull(value)
writeDataTable(wb = new_reg_wb, sheet = "construction_trades", x = non_construction_df[[1]], startRow = 60)
saveWorkbook(new_reg_wb, here("out", "current_output", str_replace_all(paste0("new_reg_",date(),".xlsx")," ","_")), overwrite = TRUE)

