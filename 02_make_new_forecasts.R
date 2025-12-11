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
library(conflicted)
conflicts_prefer(dplyr::filter)

read_clean_join <- function(pat) {
  temp <- read_xlsx(here("current_data", "ita", list.files(here("current_data", "ita"), pattern = pat))) %>%
    clean_names() %>%
    mutate(noc_code = as.character(noc_code_2021)) %>%
    left_join(mapping, by = "noc_code") %>%
    rename(
      year = contains("year"),
      month = contains("month")
    )
}

fill_holes <- function(tbbl){
  tbbl %>%
    complete(date = seq(min_date, max_date, by = "month"), fill = list(new_reg=0))
}

backward_annualize <- function(data, date_col, value_col, n = 12) {
  data %>%
    arrange(desc({{date_col}})) %>%
    mutate(exponent = ((row_number() - 1) %/% n)) %>%
    group_by(exponent) %>%
    summarise(
      mid_point=mean({{date_col}}),
      start = min({{date_col}}),
      end   = max({{date_col}}),
      new_reg = sum({{value_col}})
    )|>
    filter(start>min(start))# throw out oldest year, typically incomplete.
}

get_agf <- function(tbbl){
  start_year <- min(tbbl$year)
  end_year <- max(tbbl$year)
  span <- end_year-start_year
  start_value <- tbbl$employment[tbbl$year==start_year]
  end_value <- tbbl$employment[tbbl$year==end_year]
  agf <- (end_value/start_value)^(1/span)
  return(agf)
}

# read in the registration data
mapping <- read_csv(here("current_data","mapping","mapping.csv"))|>
  mutate(stc_=if_else(stc=="STC Trades", "STC Trades", NA_character_))

new_reg <- read_clean_join("New")|>
  mutate(short_month=as.numeric(str_sub(month,1, 2)),
         date=lubridate::ym(paste(year, short_month, sep="-")), .after=month,
         drname=if_else(drname=="Lower Mainland--Southwest", "Mainland South West",drname),
         drname=if_else(drname=="Vancouver Island and Coast", "Vancouver Island Coast",drname),
         drname=if_else(drname %in% c("North Coast", "Nechako", "Northeast", "Cariboo"), "North", drname),
         drname=if_else(drname %in% c("Kootenay", "Thompson-Okanagan"), "Southeast", drname)
)

max_date <- max(new_reg$date)
min_date <- min(new_reg$date)

mapping <- new_reg|> #overwrites the mapping object from above (done with it)
  select(noc=noc_code, functional_trades_group, stc)|>
  mutate(noc=paste0("#",noc))|>
  distinct()|>
  mutate(stc=if_else(stc=="STC Trades", "STC Trades", NA_character_))|>
  pivot_longer(cols = -noc, values_to = "functional_group")|>
  select(-name)|>
  na.omit()

keep_nocs <- mapping|> #for single counting of employment
  select(noc)|>
  distinct()

#aggregate new registrations by date, region, and group-----------

ftg <- new_reg|>
  group_by(date, drname, group=functional_trades_group)|>
  summarize(new_reg=sum(count))

ftg_bc <- new_reg|>
  group_by(date, group=functional_trades_group)|>
  summarize(new_reg=sum(count))|>
  mutate(drname="British Columbia", .after="date")

ftg_group <-  new_reg|>
  group_by(date, drname)|>
  summarize(new_reg=sum(count))|>
  mutate(group="All groups")

ftg_everything <- new_reg|>
  group_by(date)|>
  summarize(new_reg=sum(count))|>
  mutate(group="All groups",
         drname="British Columbia"
         )

stc <- new_reg|>
  filter(stc=="STC Trades")|>
  mutate(group="STC Trades")|>
  group_by(date, drname, group)|>
  summarize(new_reg=sum(count))

stc_bc <- new_reg|>
  filter(stc=="STC Trades")|>
  mutate(group="STC Trades")|>
  group_by(date, group)|>
  summarize(new_reg=sum(count))|>
  mutate(drname="British Columbia", .after="date")

ftg_and_stc <- bind_rows(ftg, ftg_group, ftg_bc, ftg_everything, stc, stc_bc)|>
  group_by(drname, group)|>
  nest()|>
  rename(registrations=data)|>
  mutate(registrations=map(registrations, fill_holes),
         backward_annualized=map(registrations, backward_annualize, date, new_reg)
         )

# LMO employment forecasts

lmo <- vroom::vroom(here("current_data", "lmo", list.files(here("current_data", "lmo"), pattern = "employment")), skip = 3)|>
  pivot_longer(cols=starts_with("2"), names_to="year", values_to = "count")|>
  clean_names()|>
  filter(noc!="#T",
         industry=="All industries")|>
  select(noc, geographic_area, year, count)|>
  mutate(geographic_area=if_else(geographic_area %in% c("Kootenay", "Thompson Okanagan"), "Southeast", geographic_area),
         geographic_area=if_else(geographic_area %in% c("Cariboo","North Coast & Nechako","North East"), "North", geographic_area),
         year=as.numeric(year)
  )|>
  group_by(noc, geographic_area, year)|>
  summarize(count=sum(count, na.rm = TRUE))

# aggregate employment by year, region, and group-----------

emp_disagg <- lmo|>
  full_join(mapping, by="noc", multiple = "all")|>
  filter(!is.na(functional_group))|>
  group_by(year, drname=geographic_area, group=functional_group)|>
  summarize(employment=sum(count))|>
  group_by(drname, group)|>
  nest()

all_groups <- lmo|>
  semi_join(keep_nocs)|> #single counting of NOCs (Nicole spotted)
  group_by(year, drname=geographic_area)|>
  summarize(employment=sum(count))|>
  mutate(group="All groups")|>
  group_by(drname, group)|>
  nest()

employment <- bind_rows(emp_disagg, all_groups)|>
  rename(employment=data)|>
  mutate(agf=map_dbl(employment, get_agf))

reg_and_employment <- inner_join(ftg_and_stc, employment)|>
  ungroup()|>
  arrange(group, drname)

write_rds(reg_and_employment, here("processed_data","reg_and_employment.rds"))


################################

#for slide deck-------------------

pop <- cansim::get_cansim("17-10-0005-01")|>
  janitor::clean_names()|>
  filter(geo=="British Columbia",
         date>lubridate::ym("2016-Jan"),
         age_group %in% c("15 to 19 years",
                          "20 to 24 years",
                          "25 to 29 years",
                          "30 to 34 years",
                          "35 to 39 years",
                          "40 to 44 years",
                          "45 to 49 years",
                          "50 to 54 years",
                          "55 to 59 years"),
         gender=="Total - gender"
  )|>
  mutate(year=lubridate::year(date),
         age_group=str_sub(age_group, end=-7)
  )|>
  select(value, year, age_group)
write_rds(pop, here::here("current_data","lmo","pop.rds"))

