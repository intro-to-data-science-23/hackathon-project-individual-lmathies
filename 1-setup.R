# packages ----------------------------------------------------------------

library(tidyverse)
library(janitor)
library(readr)
library(dplyr)
library(haven)
library(ggplot2)
library(plotly)
library(patchwork)
library(tidyr)
library(fuzzyjoin)
library(lubridate)


# --------------------------------------------------------------------------
# data --------------------------------------------------------------------
# --------------------------------------------------------------------------

survey_usa_df <- readRDS("transformed_data/survey_usa_df.rds")
# survey_usa_small <- readRDS("transformed_data/survey_usa_small.rds")

web_usa_df <- readRDS("transformed_data/web_usa_df.rds")
# web_usa_small <- readRDS("transformed_data/web_usa_small.rds")

website_visits_n <- readRDS("transformed_data/website_visits_n.rds")
filtered_website_visits_n <- readRDS("transformed_data/filtered_website_visits_n.rds")

time_zones <- readRDS("tracking/time_zones.RDS")

# --------------------------------------------------------------------------
## USEFUL SUBSETS ----------------------------------------------------------
# --------------------------------------------------------------------------

### by size

survey_usa_small <- sample_n(survey_usa_df, 1000)

web_usa_small <- sample_n(web_usa_df, 1500)

### by party

# republican subset
repub_subset <- survey_usa_df |>
  filter(party_cat == "republican") |>
  group_by(personid)

n_repub <- nrow(repub_subset)

# democratic subset
demo_subset <- survey_usa_df |>
  filter(party_cat == "democrat") |>
  group_by(personid)

n_demo <- nrow(demo_subset)

# other subset
other_subset <- survey_usa_df |>
  filter(party_cat == c("independent", "other", "not sure")) |>
  group_by(personid)

n_other <- nrow(other_subset)
