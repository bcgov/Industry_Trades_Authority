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

#' TO DO:
#' MAKE SURE THE FILES LMO_occ_char.csv AND lmo_employment ARE UP TO DATE (CHANGE ONCE PER YEAR AFTER LMO RELEASE)
#' PLACE THE 3 NEW ITA FILES IN current_ita.
#' SOURCE THIS FILE.
#' OUTPUT CAN BE FOUND IN out/current_output
tictoc::tic()
library(assertthat)
library(here)

# check to make sure directory structure exists------------------------------
if (!dir.exists("data")) {dir.create("data")}
if (!dir.exists(file.path("data","current_ita"))) {dir.create(file.path("data","current_ita"))}
if (!dir.exists(file.path("data","old_ita"))) {dir.create(file.path("data","old_ita"))}
if (!dir.exists(file.path("data","current_lmo"))) {dir.create(file.path("data","current_lmo"))}
if (!dir.exists(file.path("data","old_lmo"))) {dir.create(file.path("data","old_lmo"))}
if (!dir.exists("out")) {dir.create("out")}
if (!dir.exists(file.path("out","current_output"))) {dir.create(file.path("out","current_output"))}
if (!dir.exists(file.path("out","old_output"))) {dir.create(file.path("out","old_output"))}
if (!dir.exists("temp")) {dir.create("temp")}

#are the required files where they are supposed to be?----------------
assert_that(length(list.files(here("data","current_ita"), pattern="Active"))==1,
            msg="The file Active_Apprenticeship_Registrations*.xlsx must be in folder data/current_ita")
assert_that(length(list.files(here("data","current_ita"), pattern="CofQ"))==1,
            msg="The file CofQ_Apprenticeship_Challenge*.xlsx must be in folder data/current_ita")
assert_that(length(list.files(here("data","current_ita"), pattern="New"))==1,
            msg="The file New_Apprenticeship_Registrations*.xlsx must be in folder data/current_ita")
assert_that(length(list.files(here("data","current_lmo"), pattern="LMO_occ_char"))==1,
            msg="The file *LMO_occ_char.csv must be in folder data/current_lmo")

#archive old output
filesstrings::file.move(here("out","current_output", list.files(here("out", "current_output"))),
                        here("out", "old_output"),
                        overwrite = TRUE)
#create new output
source("01_monthly_excel.R")
source("02_make_forecasts.R")
rmarkdown::render("03_forecast_dashboard.Rmd",
                  output_file =  str_replace_all(paste0("ita_forecasts_",lubridate::today(),".html")," ","_"),
                  output_dir = here::here("out","current_output"))
rmarkdown::render("04_slide_deck.Rmd",
                  output_file =  str_replace_all(paste0("ita_slides_",lubridate::today(),".pdf")," ","_"),
                  output_dir = here::here("out","current_output"))

tictoc::toc()
#archive ita input files--------
#filesstrings::file.move(here("data","current_ita", list.files(here("data", "current_ita"))), here("data", "old_ita"))





