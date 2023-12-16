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

survey_usa_df <- readRDS("survey/meof_usa_survey_df.RDS")

web_usa_df <- readRDS("tracking/meof_usa_web_df.RDS")

time_zones <- readRDS("tracking/time_zones.RDS")



# --------------------------------------------------------------------------
## PREPARE SURVEY COVARIATES: INTUTIVE TRANSFORMATIONS ----------------------------
# --------------------------------------------------------------------------

### `party_cat` <- party identification (pid3)
#- demo = 1
#- repub = 2

survey_usa_df <- survey_usa_df %>%
  mutate(party_cat = factor(pid3,
                            levels = c(1, 2, 3, 4, 5),
                            labels = c("democrat", "republican", "independent", "other", "not sure"))
  )

### `ideo_cat` <- ideo5 
#- V liberal = 1
#- V conservative = 5

survey_usa_df <- survey_usa_df %>%
  mutate(ideo_cat = factor(ideo5,
                           levels = c(1, 2, 3, 4, 5, 6),
                           labels = c("very liberal", "liberal", "moderate", "conservative", "very conservative", "not sure"))
  )

### `gender_cat` <- gender
#- male = 1
#- female = 2

survey_usa_df <- survey_usa_df |> 
  mutate(gender_cat = factor(gender,
                             levels = c(1,2),
                             labels = c("male", "female"))
  ) 

### `educ_cat` <- educ

survey_usa_df <- survey_usa_df %>%
  mutate(educ_cat = case_when(
    educ == 1 ~ "no high school",
    educ == 2 ~ "high school",
    educ %in% c(3, 4, 5) ~ "grad school",
    educ == 6 ~ "post-grad"
  ) %>% factor(levels = c("no high school", "high school", "grad school", "post-grad")))


### `faminc_log` <- `faminc_new`

survey_usa_df <- survey_usa_df %>%
  mutate(
    faminc_log = log(case_when(
      faminc_new == 1 ~ 5000,    # < $10,000
      faminc_new == 2 ~ 15000,   # $10,000 - $19,999
      faminc_new == 3 ~ 25000,   # $20,000 - $29,999
      faminc_new == 4 ~ 35000,   # etc
      faminc_new == 5 ~ 45000,   
      faminc_new == 6 ~ 55000,   
      faminc_new == 7 ~ 65000,   
      faminc_new == 8 ~ 75000,  
      faminc_new == 9 ~ 90000,   
      faminc_new == 10 ~ 110000, 
      faminc_new == 11 ~ 135000, 
      faminc_new == 12 ~ 175000, 
      faminc_new == 13 ~ 225000, 
      faminc_new == 14 ~ 300000, 
      faminc_new == 15 ~ 425000, 
      faminc_new == 16 ~ 500000, 
      TRUE ~ NA_real_            
    ))
  )

### `faminc_cat`

# Based on Pew Research Centre classifications: https://www.pewresearch.org/politics/2021/09/23/biden-economy-appendix/

#  For the American Trends Panel, annually for an avg family of three:

#- lower-income families have incomes less than roughly $42,000
#- middle-income families have incomes greater than 42,000, and less than $125,900
#- upper-income families have incomes greater than roughly $125,900 

survey_usa_df <- survey_usa_df |>
  mutate(faminc_cat = case_when(
    faminc_new %in% c(1:4) ~ "lower income",
    faminc_new %in% c(5:10) ~ "middle income",
    faminc_new %in% c(10:16) ~ "higher income"
  ) %>% factor(levels = c("lower income", "middle income", "higher income")))

faminc_cat_table <- tabyl(survey_usa_df$faminc_cat)


# --------------------------------------------------------------------------
## `as factor` Transformations ---------------------------------------------
# --------------------------------------------------------------------------
# (Names remain the same)

# wave 
survey_usa_df <- survey_usa_df |> 
  mutate(wave = haven::as_factor(wave))

# presidential vote
survey_usa_df <- survey_usa_df |> 
  mutate(presvote16post = haven::as_factor(presvote16post))

# political interest
survey_usa_df <- survey_usa_df |> 
  mutate(polinterest = haven::as_factor(polinterest))


# --------------------------------------------------------------------------
# WEB_USA_DF TRANSFORMATIONS------------------------------------------------
# --------------------------------------------------------------------------

# create 'wave period's based on first and last observations in survey data, then applied to each observation in the web data
wave_periods <- survey_usa_df |> 
  group_by(wave) |>
  summarise(beginning_period = first(starttime), 
            ending_period = last(endtime)
  )

# non-equi join to assign waves based on used_at timestamp
# NB: this does work when i tried it on a subset of web_usa_df, it just takes a long time 
# NEED TO LET IT RUN ONCE AND THEN SAVE OUTPUT:
#web_usa_df <- fuzzyjoin::fuzzy_left_join(web_usa_df, wave_periods,
#                                         by = c("used_at" = "beginning_period", "used_at" = "ending_period"),
#                                         match_fun = list(`>=`, `<=`)) 
#web_usa_df <- web_usa_df |>
#select(-c(beginning_period, ending_period))

# --------------------------------------------------------------------------
# add time of day component to web data
web_usa_df <- web_usa_df |>
  mutate(
    day_of_week = wday(used_at, label = TRUE),  #extracts DoW
    time_of_day = hour(used_at) * 60 + minute(used_at),  # convert to minutes since midnight
    time_category = case_when(
      day_of_week %in% c("Mon", "Tue", "Wed", "Thu", "Fri") & time_of_day >= 420 & time_of_day <= 1080 ~ "working day",  # 7am to 6pm
      day_of_week %in% c("Mon", "Tue", "Wed", "Thu", "Fri") & time_of_day > 1080 & time_of_day <= 1439 ~ "after work",  # 6:01pm to 11:59pm
      time_of_day >= 0 & time_of_day < 420 ~ "late night / early morning",  # Midnight to 6:59am
      TRUE ~ "weekend"  # Remaining time
    )
  )

# --------------------------------------------------------------------------
# CREATE COUNT OF VISITS DF ------------------------------------------------
# --------------------------------------------------------------------------

# count visits for each website
website_visits_n <- web_usa_df |>
  group_by(domain) |>
  summarise(visit_count = n()) |>
  ungroup() |>
  arrange(desc(visit_count))

# List of survey-related, ad-related, other non-insightful domains to exclude
survey_related_domains <- c("adservice.google.com", "nav.smartscreen.microsoft.com", "vid-io.springserve.net", "syn.entertainow.com", "surveyjunkie.com", "samplicio.us", "acds.prod.vidible.tv", "time.rmprod.zone", "pf.entertainow.com", "track1.aniview.com", "mypoints.com", "collect.sbkcenter.com", "s.cpx.to", "t.nav.smartscreen.microsoft.com", "t.myvisualiq.net", "otf.msn.com", "sync.graph.bluecava.com", "yougovus.wakoopa.com", "yougovus2.wakoopa.com", "thrtle.com", "fc.yahoo.com", "hlsrv.vidible.tv", "tag.cogocast.net", "inboxdollars.com")  # check these 

# Filter out non-relevant-related domains
filtered_website_visits_n <- website_visits_n %>%
  filter(!domain %in% survey_related_domains)


# --------------------------------------------------------------------------
# SELECTED VARIABLES -------------------------------------------------------
# --------------------------------------------------------------------------

# use this for ad hoc adding https://hertieschool.sharepoint.com/:x:/s/HackathonProjectIDS2023/EQNkj9pXUwdJnD8SnTMNXX4B-EHRxVk5LMLKco5XoLchuw?e=vuhTrA
# will come back at the end to clean this up and have finalised list of vars





# Trust variables
# selected_vars <- survey_usa_df |> select(starts_with("trust_")) |> names()

#factor_variable_fn <- function(x) {
#factor(x, levels = c(1, 2), labels = c("selected", "not selected"))
#}

# Apply the factorization function to selected variables
#survey_usa_df <- survey_usa_df %>%
#mutate_at(vars(one_of(selected_vars)), ~ factor_variable_fn(.))'



# --------------------------------------------------------------------------
## EXPORT TRANSFORMED DATA -------------------------------------------------
# --------------------------------------------------------------------------

# create directory
if(!dir.exists("transformed_data")){
  dir.create("transformed_data", recursive = TRUE)
}

write_rds(survey_usa_df, "transformed_data/survey_usa_df.rds")
# write_rds(survey_usa_small, "transformed_data/survey_usa_small.rds")

write_rds(web_usa_df, "transformed_data/web_usa_df.rds")
# write_rds(web_usa_small, "transformed_data/web_usa_small.rds")

write_rds(website_visits_n, "transformed_data/website_visits_n.rds") # this I used once in subsequent scripts, but it contains lots of ad and survey related domains; it's not as good as filtered_website_visits_n
write_rds(filtered_website_visits_n, "transformed_data/filtered_website_visits_n.rds") # this is better, use this 

