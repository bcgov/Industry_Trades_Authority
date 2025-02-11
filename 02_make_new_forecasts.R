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
                                     pattern = "New_Apprenticeship_Registrations"))) |>
  clean_names()|>
  mutate(noc_code = as.character(noc_code_2021)) |>
  rename(
    year = contains("year"),
    month = contains("month")
  )|>
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

keep_nocs <- mapping|> #for single counting of employment
  select(noc)|>
  distinct()

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
lmo <- vroom::vroom(here("current_data", "lmo", list.files(here("current_data", "lmo"), pattern = "employment")), skip = 3)|>
  pivot_longer(cols=starts_with("2"), names_to="year", values_to = "count")|>
  clean_names()|>
  filter(noc!="#T",
         industry=="All industries",
         year>=max_year)|>
  mutate(noc=str_sub(noc, start=2))|>
  select(noc, geographic_area, year, count)|>
  mutate(geographic_area=if_else(geographic_area %in% c("Kootenay", "Thompson Okanagan"), "Southeast", geographic_area),
         geographic_area=if_else(geographic_area %in% c("Cariboo","North Coast & Nechako","North East"), "North", geographic_area)
         )|>
  group_by(noc, geographic_area, year)|>
  summarize(count=sum(count, na.rm = TRUE))

employment_raw <- bind_rows(lmo, lfs)|>
  mutate(noc=paste0("#",noc))


#aggregate new registrations by year, region, and group-----------

ftg <- new_reg|>
  group_by(year, drname, group=functional_trades_group)|>
  summarize(new_reg=sum(count))

ftg_bc <- new_reg|>
  group_by(year, group=functional_trades_group)|>
  summarize(new_reg=sum(count))|>
  mutate(drname="British Columbia", .after="year")

ftg_group <-  new_reg|>
  group_by(year, drname)|>
  summarize(new_reg=sum(count))|>
  mutate(group="All groups")

ftg_everything <- new_reg|>
  group_by(year)|>
  summarize(new_reg=sum(count))|>
  mutate(group="All groups",
         drname="British Columbia"
         )

stc <- new_reg|>
  filter(stc_trades=="Y")|>
  mutate(group="STC Trades")|>
  group_by(year, drname, group)|>
  summarize(new_reg=sum(count))

stc_bc <- new_reg|>
  filter(stc_trades=="Y")|>
  mutate(group="STC Trades")|>
  group_by(year, group)|>
  summarize(new_reg=sum(count))|>
  mutate(drname="British Columbia", .after="year")

ftg_and_stc <- bind_rows(ftg, ftg_group, ftg_bc, ftg_everything, stc, stc_bc)

if(max(ytd)<12){
  full_years <- ftg_and_stc|>
    filter(year<max_year)|>
    mutate(year=as.character(year)) #necessary for join below
}else{
  full_years <-ftg_and_stc|>
    mutate(year=as.character(year)) #necessary for join below
}

#use the partial year of data to adjust forecast
partial_scaled_up <- ftg_and_stc|>
  filter(year==max_year)|>
  mutate(year=as.character(year))|> #necessary for join below
  mutate(scaled_up=(new_reg/length(ytd))*12)|>  #scale up potentially incomplete last year.
  select(-new_reg)

no_nas <- full_years|>
  mutate(year=as.numeric(year))|>
  tsibble::as_tsibble(key=c(drname, group), index = year)|>
  tsibble::fill_gaps(.full = TRUE, new_reg=0)|>
  as_tibble()|>
  mutate(year=as.character(year))|>
  arrange(year, drname, group)|>
  bind_rows(partial_scaled_up|>rename(new_reg=scaled_up))#add in scaled up partial year... could be wrong

# aggregate employment by year, region, and group-----------

emp_disagg <- employment_raw|>
  full_join(mapping, by="noc", multiple = "all")|>
  filter(!is.na(functional_group))|>
  group_by(year, drname=geographic_area, group=functional_group)|>
  summarize(employment=sum(count))

all_groups <- employment_raw|>
  semi_join(keep_nocs)|> #single counting of NOCs (Nicole spotted)
  group_by(year, drname=geographic_area)|>
  summarize(employment=sum(count))|>
  mutate(group="All groups")

employment <- bind_rows(emp_disagg, all_groups)

reg_and_employment <- full_join(no_nas, employment)|>
  filter(drname!="NULL")|>
  ungroup()|>
  arrange(year, group, drname)

################################

for_props <- reg_and_employment|>
  filter(year %in% c(2016:2024))

prop_reg_annual <- for_props|>
  group_by(drname, group, year)|>
  summarize(annual_prop=sum(new_reg)/sum(employment))

prop_reg_mean <- for_props|>
  group_by(drname, group)|>
  summarize(prop_mean=sum(new_reg)/sum(employment))

prop_reg <- full_join(prop_reg_annual, prop_reg_mean)

#take a look at the proportions
ggplot(prop_reg)+
  geom_line(aes(as.numeric(year), prop_mean), colour="white", lwd=2)+
  geom_line(aes(as.numeric(year), annual_prop))+
  facet_grid(drname~group)+
  labs(x=NULL, y="New Registrations/Employment")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

prop_reg_wide <- prop_reg|>
  pivot_wider(names_from = year, values_from=annual_prop, names_prefix = "prop_")

fcast_tbbl <- full_join(reg_and_employment, prop_reg_wide)|>
  full_join(partial_scaled_up)|>
  mutate(
    across(
      .cols = contains("prop_"),
      .fns = ~.x*employment
    )
  )|>
  mutate(
    across(
      .cols = contains("prop_"),
      .fns = ~ if_else(is.na(scaled_up),
                       round(.x),
                       round(f_weight*scaled_up+(1-f_weight)*.x))
    )
  )|>
  rename(`New Registrations`=new_reg)|>
  arrange(drname, group)


fcast_long <- fcast_tbbl|>
  select(year, drname, group, `New Registrations`, contains("prop"))|>
  pivot_longer(cols=c(`New Registrations`, contains("prop")))|>
  mutate(year=as.numeric(year))

write_rds(fcast_tbbl, here("processed_data", "fcast_tbbl.RDS"))
write_rds(fcast_long, here("processed_data", "fcast_long.RDS"))

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

