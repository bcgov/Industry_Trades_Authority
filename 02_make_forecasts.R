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
# read in the data----------------

new_reg <- read_xlsx(here("current_data",
                          "ita",
                          list.files(here("current_data",
                                          "ita"),
                                     pattern = "New_Apprenticeship_Registrations"))) %>%
  clean_names()%>%
  mutate(noc_code = as.character(noc_code_2021)) %>%
  rename(
    year = contains("year"),
    month = contains("month")
  )%>%
  mutate(short_month=as.numeric(str_sub(month,1, 2)),
         date=lubridate::ym(paste(year, short_month, sep="-")), .after=month,
         drname=if_else(drname=="Lower Mainland--Southwest", "Mainland South West",drname),
         drname=if_else(drname=="Vancouver Island and Coast", "Vancouver Island Coast",drname),
         drname=if_else(drname %in% c("North Coast", "Nechako", "Northeast", "Cariboo"), "North", drname),
         drname=if_else(drname %in% c("Kootenay", "Thompson-Okanagan"), "Southeast", drname)
  )

ltm <- lubridate::month(max(new_reg$date)-months(0:2)) #the last three months of data
ytd <- 1:lubridate::month(max(new_reg$date))  #ytd months
f_weight <- length(ytd)/12 #this is the weight put on the extrapolation of ytd registrations.
#f_weight <- 0 #if last year is complete the backcast and forecasts intersect observed new registrations
max_year <- max(new_reg$year)

mapping <- new_reg|>
  select(noc=noc_code, functional_trades_group, stc_trades)|>
  mutate(noc=paste0("#",noc))|>
  distinct()|>
  mutate(stc_trades=if_else(stc_trades=="Y", "STC Trades", NA_character_))|>
  pivot_longer(cols = -noc, values_to = "functional_group")|>
  select(-name)|>
  na.omit()

#LFS data

lfs <- vroom::vroom(here("current_data","lfs",list.files(here("current_data","lfs"))))|>
  clean_names()|>
  mutate(ertab=if_else(is.na(ertab),59, ertab))|>
  na.omit()|>
  filter(lf_stat=="Employed",
         noc_5!="missi",
         !is.na(noc_5),
         syear < max_year)|>
  mutate(year=as.character(syear),
         geographic_area=case_when(ertab==59~"British Columbia",
                                   ertab==5910~"Vancouver Island Coast",
                                   ertab==5920~"Mainland South West",
                                   ertab %in% c(5930, 5940)~"Southeast",
                                   ertab %in% c(5950, 5960, 5970, 5980)~"North"),
         count=count/12)|>
  select(noc=noc_5, geographic_area, year, count)|>
  group_by(noc, geographic_area, year)|>
  summarize(count=sum(count, na.rm=TRUE))

#LMO data
lmo <- vroom::vroom(here("current_data", "lmo", list.files(here("current_data", "lmo"), pattern = "employment")), skip = 3)%>%
  pivot_longer(cols=starts_with("2"), names_to="year", values_to = "count")%>%
  clean_names()%>%
  filter(noc!="#T",
         industry=="All industries")|>
  mutate(noc=str_sub(noc, start=2))|>
  select(noc, geographic_area, year, count)|>
  mutate(geographic_area=if_else(geographic_area %in% c("Kootenay", "Thompson Okanagan"), "Southeast", geographic_area),
         geographic_area=if_else(geographic_area %in% c("Cariboo","North Coast & Nechako","North East"), "North", geographic_area)
         )|>
  group_by(noc, geographic_area, year)|>
  summarize(count=sum(count, na.rm = TRUE))

employment <- bind_rows(lmo, lfs)|>
  mutate(noc=paste0("#",noc))|>
  full_join(mapping, by="noc", multiple = "all")


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
  mutate(group="STC Trades")%>%
  group_by(year, drname, group)%>%
  summarize(new_reg=sum(count))

stc_bc <- new_reg%>%
  filter(stc_trades=="Y")%>%
  mutate(group="STC Trades")%>%
  group_by(year, group)%>%
  summarize(new_reg=sum(count))%>%
  mutate(drname="British Columbia", .after="year")

ftg_and_stc <- bind_rows(ftg, ftg_group, ftg_bc, ftg_everything, stc, stc_bc)

if(max(ytd)<12){
  full_years <- ftg_and_stc%>%
    filter(year<max_year)%>%
    mutate(year=as.character(year)) #necessary for join below
}else{
  full_years <-ftg_and_stc%>%
    mutate(year=as.character(year)) #necessary for join below
}

#use the partial year of data to adjust forecast
partial_scaled_up <- ftg_and_stc%>%
  filter(year==max_year)%>%
  mutate(year=as.character(year))%>% #necessary for join below
  mutate(scaled_up=(new_reg/length(ytd))*12)|>  #scale up potentially incomplete last year.
  select(-new_reg)

no_nas <- full_years|>
  mutate(year=as.numeric(year))|>
  tsibble::as_tsibble(key=c(drname, group), index = year)|>
  tsibble::fill_gaps(.full = TRUE, new_reg=0)|>
  as_tibble()|>
  mutate(year=as.character(year))|>
  arrange(year, drname, group)

# aggregate employment by year, region, and group-----------

lmo_emp_disagg <- employment%>%
  filter(!is.na(functional_group))%>%
  group_by(year, drname=geographic_area, group=functional_group)%>%
  summarize(employment=sum(count))

lmo_all_groups <- employment%>%
  filter(!is.na(functional_group))%>%
  group_by(year, drname=geographic_area)%>%
  summarize(employment=sum(count))%>%
  mutate(group="All groups")

lmo_employment <- bind_rows(lmo_emp_disagg, lmo_all_groups)

reg_and_employment <- full_join(no_nas, lmo_employment)|>
  filter(drname!="NULL")|>
  ungroup()|>
  arrange(year, group, drname)

################################

for_props <- reg_and_employment%>%
  filter(year %in% c(2016:2019, 2021:2024))|>
  mutate(based_on=if_else(year<2020, "2016:2019 ratio", "2021:2024 ratio"))

by_period <- for_props%>%
  group_by(drname, group, based_on)%>%
  summarize(prop_reg_utilized=sum(new_reg)/sum(employment))

not_by_period <- for_props%>%
  group_by(drname, group)%>%
  summarize(prop_reg_utilized=sum(new_reg)/sum(employment))|>
  mutate(based_on="both period ratio")

prop_reg_utilized <- bind_rows(by_period, not_by_period)|>
  pivot_wider(names_from = based_on, values_from = prop_reg_utilized)

fcast_tbbl <- full_join(reg_and_employment, prop_reg_utilized)|>
  full_join(partial_scaled_up)|>
  mutate(`based on 2016:2019 ratio`=employment*`2016:2019 ratio`,
         `based on 2021:2024 ratio`=employment*`2021:2024 ratio`,
         `based on both periods`=employment*`both period ratio`,
         `based on 2016:2019 ratio`=if_else(is.na(scaled_up),
                                            round(`based on 2016:2019 ratio`),
                                            round(f_weight*scaled_up+(1-f_weight)*`based on 2016:2019 ratio`)),
         `based on 2021:2024 ratio`=if_else(is.na(scaled_up),
                                            round(`based on 2021:2024 ratio`),
                                            round(f_weight*scaled_up+(1-f_weight)*`based on 2021:2024 ratio`)),
         `based on both periods`=if_else(is.na(scaled_up),
                                            round(`based on both periods`),
                                            round(f_weight*scaled_up+(1-f_weight)*`based on both periods`))
         )|>
  rename(`New Registrations`=new_reg)|>
  arrange(drname, group)

fcast_long <- fcast_tbbl|>
  select(year, drname, group, `New Registrations`, contains("based"))|>
  pivot_longer(cols=c(`New Registrations`, contains("based")))|>
  mutate(year=as.numeric(year))

write_rds(fcast_tbbl, here("processed_data", "fcast_tbbl.RDS"))
write_rds(fcast_long, here("processed_data", "fcast_long.RDS"))

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
         gender=="Total - gender"
  )%>%
  mutate(year=lubridate::year(date),
         age_group=str_sub(age_group, end=-7)
  )%>%
  select(value, year, age_group)
write_rds(pop, here::here("current_data","lmo","pop.rds"))

