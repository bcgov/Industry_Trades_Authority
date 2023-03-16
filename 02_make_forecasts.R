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

# read in the data----------------
mapping <- read_csv(here("current_data","mapping", "functional_groups.csv"))

new_reg <- read_xlsx(here("current_data",
                          "ita",
                          list.files(here("current_data",
                                          "ita"),
                                     pattern = "New_Apprenticeship_Registrations"))) %>%
  clean_names() %>%
  mutate(noc_code = as.character(noc_code)) %>%
  rename(
    year = contains("year"),
    month = contains("month")
  )%>%
  mutate(short_month=as.numeric(str_sub(month,1, 2)),
         date=lubridate::ym(paste(year, short_month, sep="-")), .after=month)%>%
  mutate(drname=fct_collapse(drname,
                             `Vancouver Island and Coast`="Vancouver Island and Coast",
                             `Lower Mainland Southwest`= "Lower Mainland--Southwest",
                             Southeast=c("Thompson-Okanagan", "Kootenay"),
                             North=c("North Coast","Nechako","Northeast", "Cariboo")))

ltm <- lubridate::month(max(new_reg$date)-months(0:2)) #the last three months of data
ytd <- 1:lubridate::month(max(new_reg$date))  #ytd months
f_weight <- length(ytd)/12 #this is the weight put on the extrapolation of ytd registrations.
max_year <- max(new_reg$year)

lmo <- vroom::vroom(here("current_data", "lmo", list.files(here("current_data", "lmo"), pattern = "employment")), skip = 2)%>%
  pivot_longer(cols=-c(NOC,Description, Industry, Variable,`Geographic Area`), names_to="year", values_to = "count")%>%
  clean_names()%>%
  full_join(mapping, by="noc", multiple = "all")%>%
  mutate(geographic_area=fct_recode(geographic_area,
                             `Vancouver Island and Coast`="Vancouver Island Coast",
                             `Lower Mainland Southwest`= "Mainland South West",
                             `Southeast`="South East"))%>%
  filter(geographic_area %in% c("British Columbia", "Vancouver Island and Coast","Lower Mainland Southwest","Southeast","North"))

#aggregate new registrations by year, region, and group-----------

ftg <- new_reg%>%
  group_by(year, drname, group=functional_trades_group)%>%
  summarize(new_reg=sum(count))

ftg_bc <- new_reg%>%
  group_by(year, group=functional_trades_group)%>%
  summarize(new_reg=sum(count))%>%
  mutate(drname="British Columbia", .after="year")

ftg_group <-  new_reg%>%
  group_by(year, drname)%>%
  summarize(new_reg=sum(count))%>%
  mutate(group="All groups")

ftg_everything <- new_reg%>%
  group_by(year)%>%
  summarize(new_reg=sum(count))%>%
  mutate(group="All groups",
         drname="British Columbia"
         )

stc <- new_reg%>%
  filter(stc_trades=="Y")%>%
  mutate(group="Skilled Trades Certification (STC)")%>%
  group_by(year, drname, group)%>%
  summarize(new_reg=sum(count))

stc_bc <- new_reg%>%
  filter(stc_trades=="Y")%>%
  mutate(group="Skilled Trades Certification (STC)")%>%
  group_by(year, group)%>%
  summarize(new_reg=sum(count))%>%
  mutate(drname="British Columbia", .after="year")

ftg_and_stc <- bind_rows(ftg, ftg_group, ftg_bc, ftg_everything, stc, stc_bc)

full_years <- ftg_and_stc%>%
  filter(year<max_year)%>%
  mutate(year=as.character(year)) #necessary for join below

partial_scaled_up <- ftg_and_stc%>%
  filter(year==max_year)%>%
  mutate(year=as.character(year))%>% #necessary for join below
  mutate(scaled_up=(new_reg/length(ytd))*12)%>%
  select(-new_reg)

# aggregate LMO by year, region, and group-----------

lmo_emp_disagg <- lmo%>%
  filter(noc!="T",
         !is.na(functional_group))%>%
  group_by(year, drname=geographic_area, group=functional_group)%>%
  summarize(employment=sum(count))

lmo_all_groups <- lmo%>%
  filter(noc!="T",
         !is.na(functional_group))%>%
  group_by(year, drname=geographic_area)%>%
  summarize(employment=sum(count))%>%
  mutate(group="All groups")

lmo_employment <- bind_rows(lmo_emp_disagg, lmo_all_groups)

reg_and_employment <- full_join(full_years, lmo_employment)

prop_reg_utilized <- reg_and_employment%>%
  filter(year %in% c(2016:2019,2021,2022))%>%
  group_by(drname, group)%>%
  summarize(prop_reg_utilized=sum(new_reg)/sum(employment))

fcast_tbbl <- full_join(reg_and_employment, prop_reg_utilized)%>%
  mutate(demand_driven_fcast=round(employment*prop_reg_utilized, 0),
         obs_and_fcast=if_else(is.na(new_reg), demand_driven_fcast, new_reg))%>%
  filter(!is.na(obs_and_fcast))%>%
  full_join(partial_scaled_up)%>%
  mutate(forecast=if_else(is.na(scaled_up), obs_and_fcast, f_weight*scaled_up +(1-f_weight)*obs_and_fcast))

  write_rds(fcast_tbbl, here("temp","fcast_tbbl.rds"))

for_plots <- fcast_tbbl%>%
  select(year, drname, group, forecast)%>%
  mutate(year=as.numeric(year))

for_plots%>%
  write_rds(here("temp","for_plots.rds"))

#for slide deck-------------------

pop <- cansim::get_cansim("17-10-0005-01")%>%
  janitor::clean_names()%>%
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
         sex=="Both sexes"
  )%>%
  mutate(year=lubridate::year(date),
         age_group=str_sub(age_group, end=-7)
  )%>%
  select(value, year, age_group)
write_rds(pop, here::here("current_data","lmo","pop.rds"))

