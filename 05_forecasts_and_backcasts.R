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


make_plot <- function(tbbl, group, noc){
  if(is.na(noc)){
    title=group
  }else{
    title=paste(group, noc, sep=": ")
  }
  ggplot(tbbl, aes(year, value, colour=name))+
    geom_line(lwd=2)+
    scale_y_continuous(labels=scales::comma)+
    labs(title=title,
         x="",
         y="",
         colour="")+
    theme_minimal(base_size = 15)+
    expand_limits(y = 0)+
    facet_wrap(~thing)
}

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
  mutate(short_month=as.numeric(str_sub(month, 1, 2)),
         date=lubridate::ym(paste(year, short_month, sep="-")), .after=month)%>%
  select(data_type, date, year, noc_code, noc, functional_trades_group, stc_trades, count)

noc_info <- new_reg%>%
  select(group=noc_code, noc)%>%
  distinct()

seats <- read_xlsx(here("current_data",
                        "seats",
                        list.files(here("current_data",
                                        "seats"),
                                   pattern = "Utilization"))) %>%
  clean_names()%>%
  filter(level_clean=="1")%>%
  mutate(year=paste0("20",str_sub(fiscal_year, 1, 2)))%>%
  filter(year>2015)%>%
  mutate(data_type="Level 1 Seats")%>%
  mutate(trade_noc=as.character(trade_noc))%>%
  select(data_type,
         year,
         noc_code=trade_noc,
         functional_trades_group=functional_trade_group,
         stc_trades=stc_trade_noc,
         count=actuals)%>%
  full_join(noc_info, by=c("noc_code"="group"))

#"contstants"-----------------
ltm <- lubridate::month(max(new_reg$date)-months(0:2)) #the last three months of data
ytd <- 1:lubridate::month(max(new_reg$date))  #ytd months
f_weight <- length(ytd)/12 #this is the weight put on the extrapolation of ytd registrations.
max_year <- max(new_reg$year)

lmo_bc <- vroom::vroom(here("current_data", "lmo", list.files(here("current_data", "lmo"), pattern = "employment")), skip = 2)%>%
  pivot_longer(cols=-c(NOC,Description, Industry, Variable,`Geographic Area`), names_to="year", values_to = "count")%>%
  clean_names()%>%
  full_join(mapping, by="noc", multiple = "all")%>%
  filter(geographic_area=="British Columbia")

#aggregate seats by year and group

seats_by_noc <- seats%>%
  group_by(year, group=noc_code)%>%
  summarize(observed=sum(count))

seats_by_ftg <- seats%>%
  group_by(year, group=functional_trades_group)%>%
  summarize(observed=sum(count))

seats_agg <- seats%>%
  group_by(year)%>%
  summarize(observed=sum(count))%>%
  mutate(group="All groups"
         )
seats_stc <- seats%>%
  filter(stc_trades=="Y")%>%
  mutate(group="Skilled Trades Certification (STC)")%>%
  group_by(year, group)%>%
  summarize(observed=sum(count))

agg_seats <- bind_rows(seats_agg, seats_stc, seats_by_ftg, seats_by_noc)%>%
  na.omit()

#aggregate new registrations by year and group-----------

reg_by_noc <- new_reg%>%
  group_by(year, group=noc_code)%>%
  summarize(observed=sum(count))

reg_by_ftg <- new_reg%>%
  group_by(year, group=functional_trades_group)%>%
  summarize(observed=sum(count))

reg_agg <- new_reg%>%
  group_by(year)%>%
  summarize(observed=sum(count))%>%
  mutate(group="All groups")

reg_stc <- new_reg%>%
  filter(stc_trades=="Y")%>%
  mutate(group="Skilled Trades Certification (STC)")%>%
  group_by(year, group)%>%
  summarize(observed=sum(count))

agg_registrations <- bind_rows(reg_agg, reg_stc, reg_by_ftg, reg_by_noc)

reg_full_years <- agg_registrations%>%
  filter(year<max_year)%>%
  mutate(year=as.character(year)) #necessary for join below

reg_scaled <- agg_registrations%>%
  filter(year==max_year)%>%
  mutate(year=as.character(year))%>% #necessary for join below
  mutate(scaled_up=(observed/length(ytd))*12)%>%
  select(-observed)

# aggregate LMO by year and group-----------

lmo_by_noc <- lmo_bc%>%
  filter(noc!="#T",
         !is.na(functional_group))%>%
  group_by(year, group=noc)%>%
  summarize(employment=sum(count))%>%
  mutate(group=str_sub(group, start=2))

lmo_by_functional_group <- lmo_bc%>%
  filter(noc!="#T",
         !is.na(functional_group))%>%
  group_by(year, group=functional_group)%>%
  summarize(employment=sum(count))

lmo_agg <- lmo_bc%>%
  filter(noc!="#T",
         !is.na(functional_group))%>%
  group_by(year)%>%
  summarize(employment=sum(count))%>%
  mutate(group="All groups")

agg_lmo <- bind_rows(lmo_by_noc, lmo_by_functional_group, lmo_agg)

reg_and_employment <- full_join(reg_full_years, agg_lmo)
seats_and_employment <- full_join(agg_seats, agg_lmo)

prop_seats_utilized <- seats_and_employment%>%
  filter(year %in% c(2016:2019, 2021, 2022))%>%
  group_by(group)%>%
  summarize(prop_seats_utilized=mean(observed, na.rm=TRUE)/mean(employment, na.rm=TRUE))%>%
  na.omit()

prop_reg_utilized <- reg_and_employment%>%
  filter(year %in% c(2016:2019,2021,2022))%>%
  group_by(group)%>%
  summarize(prop_reg_utilized=mean(observed, na.rm=TRUE)/mean(employment, na.rm=TRUE))%>%
  na.omit()

#visualize relationships between props
inner_join(prop_reg_utilized, prop_seats_utilized)%>%
  ggplot(aes(prop_reg_utilized, prop_seats_utilized))+
  geom_point()+
  geom_abline(slope=1, intercept = 0)


reg_tbbl <- full_join(reg_and_employment, prop_reg_utilized)%>%
  mutate(prop_cast=round(employment*prop_reg_utilized, 0),
         forecast=if_else(year<max_year, NA, prop_cast),
         backcast=if_else(year<=max_year, prop_cast, NA))%>%
  full_join(reg_scaled)%>%
  mutate(with_scaled=if_else(is.na(scaled_up), forecast, f_weight*scaled_up +(1-f_weight)*forecast))%>%
  full_join(noc_info)%>%
  select(year,
         group,
         noc,
         observed,
         forecast,
         backcast)%>%
  mutate(year=as.numeric(year))%>%
  pivot_longer(cols=-c(year,group, noc), names_to = "name", values_to = "value")%>%
  filter(!is.na(value))%>%
  mutate(thing="new registrations")%>%
  group_by(group, noc)%>%
  nest()%>%
  mutate(num_rows=map(data, nrow))%>%
  filter(num_rows>20)%>%
  select(-num_rows)%>%
  unnest(data)


seats_tbbl <- full_join(seats_and_employment, prop_seats_utilized)%>%
  mutate(prop_cast=round(employment*prop_seats_utilized, 0),
         forecast=if_else(year<max_year, NA, prop_cast),
         backcast=if_else(year<=max_year, prop_cast, NA))%>%
  full_join(noc_info)%>%
  select(year,
         group,
         noc,
         observed,
         forecast,
         backcast)%>%
  mutate(year=as.numeric(year))%>%
  pivot_longer(cols=-c(year,group, noc), names_to = "name", values_to = "value")%>%
  filter(!is.na(value))%>%
  mutate(thing="seats")%>%
  group_by(group, noc)%>%
  nest()%>%
  mutate(num_rows=map(data, nrow))%>%
  filter(num_rows>20)%>%
  select(-num_rows)%>%
  unnest(data)

nested <- bind_rows(reg_tbbl, seats_tbbl)%>%
  group_by(group, noc)%>%
  nest()%>%
  mutate(plot=pmap(list(data, group, noc), make_plot))

pdf(here("out","current_output", "reg_and_seats.pdf"), onefile = TRUE, height=8.5, width=11)
nested%>%
  select(plot)%>%
  walk(print)
dev.off()




