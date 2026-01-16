library(readr)
library(tidyverse)
library(dplyr)
library(purrr)
library(openxlsx)

## Let's laod all the data first ####
years <- 2009:2023
base_url <- "https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_1/"

#### Load 990 Files - Part 0 ####
f990_p0_data <- years |>
  map(~ read_csv(
    paste0(base_url, "F9-P00-T00-HEADER-", .x, ".CSV"),
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) |>
    select(any_of(c("ORG_EIN", "TAX_YEAR","RETURN_TYPE" ))) %>%
    mutate(year = .x)) |>
  list_rbind()

#### Load 990 Files - Part 1 ####
f990_p1_data <- years |>
  map(~ read_csv(
    paste0(base_url, "F9-P01-T00-SUMMARY-", .x, ".CSV"),
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) |>
    mutate(year = .x)) |>
  list_rbind()

#### Load 990ez Files - Part 1 ####
f990EZ_p1_data <- years |>
  map(
    ~ read_csv(
      paste0(base_url, "F9-P01-T00-SUMMARY-EZ-", .x, ".CSV"),
      show_col_types = FALSE
    ) |>
      mutate(year = .x)
  ) |>
  list_rbind()

#### Load 990EZ Files - Part 2 ####
f990_p2_data <- years |>
  map(~ read_csv(
    paste0(base_url, "F9-P02-T00-SIGNATURE-", .x, ".CSV"),
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) |>
    mutate(year = .x)) |>
  list_rbind()

#### Load 990 Files - Part 8 ####
f990_p8_data <- years |>
  map(~ read_csv(
    paste0(base_url, "F9-P08-T00-REVENUE-", .x, ".CSV"),
    show_col_types = FALSE
  ) |>
    mutate(year = .x)) |>
  list_rbind()

#### Load 990 Files - Part 10 ####
f990_p10_data <- years |>
  map(~ read_csv(
    paste0(base_url, "F9-P10-T00-BALANCE-SHEET-", .x, ".CSV"),
    show_col_types = FALSE
  ) |>
    mutate(year = .x)) |>
  list_rbind()

food_aid <- readr::read_csv("data/food_assistance_nonprofits_list.csv")

#### cleaning up the data ####
Data_990_p0_clean <- f990_p0_data |>
  distinct(ORG_EIN, TAX_YEAR,RETURN_TYPE)

## merge food_aid with clean data ###
food_990_matches <- Data_990_p0_clean |>
  dplyr::inner_join(food_aid %>% distinct(ORG_EIN),
    by = "ORG_EIN")
### Number of nonprofits filed 990 P0 By Year###
f990_p0_n <- food_990_matches |>
  dplyr::group_by(TAX_YEAR,RETURN_TYPE ) |>
  dplyr::filter(RETURN_TYPE == "990") |>
  dplyr::summarise(
    `Percent of Food-Aid Organizations` = (((n_distinct(ORG_EIN)) / 16225)),
    .groups = "drop"
  )

f990_p0EZ_n <- food_990_matches |>
  dplyr::group_by(TAX_YEAR,RETURN_TYPE ) |>
  dplyr::filter(RETURN_TYPE != "990") |>
  dplyr::summarise(
    `Percent of Food-Aid Organizations` = (((n_distinct(ORG_EIN)) / 16225)),
    .groups = "drop"
  )


### cleaning up the P1 data ####
Data_990_p1_clean <- f990_p1_data |>
  distinct(ORG_EIN, TAX_YEAR, RETURN_TYPE)

## merge food_aid with clean data ###
food_990_p1_matches <- Data_990_p1_clean |>
  dplyr::inner_join(food_aid %>% distinct(ORG_EIN),
    by = "ORG_EIN"
  )

### Number of nonprofits filed 990 & 990EZ P1 By Year###
f990_p1_n <- food_990_p1_matches |>
  dplyr::group_by(TAX_YEAR,RETURN_TYPE) |>
  dplyr::filter(RETURN_TYPE == "990") |>
  dplyr::summarise(
    `Percent of Food-Aid Organizations` = (((n_distinct(ORG_EIN)) / 16225)),
    .groups = "drop"
  )

f990_p1EZ_n <- food_990_p1_matches |>
  dplyr::group_by(TAX_YEAR,RETURN_TYPE) |>
  dplyr::filter(RETURN_TYPE != "990") |>
  dplyr::summarise(
    `Percent of Food-Aid Organizations` = (((n_distinct(ORG_EIN)) / 16225)),
    .groups = "drop"
  )
### cleaning up the P2 data ####
Data_990_p2_clean <- f990_p2_data |>
  distinct(ORG_EIN, TAX_YEAR, RETURN_TYPE)

## merge food_aid with clean data ###
food_990_p2_matches <- Data_990_p2_clean |>
  dplyr::inner_join(food_aid %>% distinct(ORG_EIN),
                    by = "ORG_EIN"
  )

### Number of nonprofits filed 990 & 990EZ P2 By Year###
f990_p2_n <- food_990_p2_matches |>
  dplyr::group_by(TAX_YEAR,RETURN_TYPE) |>
  dplyr::filter(RETURN_TYPE == "990") |>
  dplyr::summarise(
    `Percent of Food-Aid Organizations` = (((n_distinct(ORG_EIN)) / 16225)),
    .groups = "drop"
  )

f990_p2EZ_n <- food_990_p2_matches |>
  dplyr::group_by(TAX_YEAR,RETURN_TYPE) |>
  dplyr::filter(RETURN_TYPE != "990") |>
  dplyr::summarise(
    `Percent of Food-Aid Organizations` = (((n_distinct(ORG_EIN)) / 16225)),
    .groups = "drop"
  )
### Number of nonprofits filed 990 P10 By Year###
# clean the data so it's one count per EIN and rid of duplicates #

Data_990_p10_clean <- f990_p10_data |>
  distinct(ORG_EIN, TAX_YEAR, RETURN_TYPE)

## merge food_aid with clean data ###
food_990_p10_matches <- Data_990_p10_clean |>
  dplyr::inner_join(food_aid %>% distinct(ORG_EIN),
    by = "ORG_EIN"
  )

### Number of nonprofits filed 990 P10 By Year###
f990_p10_n <- food_990_p10_matches |>
  dplyr::group_by(TAX_YEAR, RETURN_TYPE ) |>
  dplyr::filter(RETURN_TYPE == "990") |>
  dplyr::summarise(
    `Percent of Food-Aid Organizations` = (((n_distinct(ORG_EIN)) / 16225)),
    .groups = "drop"
  )

f990_p10EZ_n <- food_990_p10_matches |>
  dplyr::group_by(TAX_YEAR, RETURN_TYPE ) |>
  dplyr::filter(RETURN_TYPE != "990") |>
  dplyr::summarise(
    `Percent of Food-Aid Organizations` = (((n_distinct(ORG_EIN)) / 16225)),
    .groups = "drop"
  )

### Number of nonprofits filed 990 P8 By Year###
# clean the data so it's one count per EIN and rid of duplicates #

Data_990_p8_clean <- f990_p8_data |>
  distinct(ORG_EIN, TAX_YEAR, RETURN_TYPE)

## merge food_aid with clean data ###
food_990_p8_matches <- Data_990_p8_clean |>
  dplyr::inner_join(food_aid %>% distinct(ORG_EIN),
    by = "ORG_EIN"
  )

### Number of nonprofits filed 990 P10 By Year###
f990_p8_n <- food_990_p8_matches |>
  dplyr::group_by(TAX_YEAR,RETURN_TYPE) |>
  dplyr::filter(RETURN_TYPE == "990") |>
  dplyr::summarise(
    `Percent of Food-Aid Organizations` = (((n_distinct(ORG_EIN)) / 16225)),
    .groups = "drop"
  )

f990_p8EZ_n <- food_990_p8_matches |>
  dplyr::group_by(TAX_YEAR,RETURN_TYPE) |>
  dplyr::filter(RETURN_TYPE != "990") |>
  dplyr::summarise(
    `Percent of Food-Aid Organizations` = (((n_distinct(ORG_EIN)) / 16225)),
    .groups = "drop"
  )
### Place all the outputs in one excel sheet ###
f990_p0_n <- f990_p1_n %>%
  mutate(metric = "f990_p0_n")

f990_p1_n <- f990_p1_n %>%
  mutate(metric = "f990_p1_n")

f990_p2_n <- f990_p2_n %>%
  mutate(metric = "f990_p2_n")

f990_p8_n <- f990_p8_n %>%
  mutate(metric = "f990_p8_n")

f990_p10_n <- f990_p10_n %>%
  mutate(metric = "f990_p10_n")

long_table <- bind_rows(
  f990_p1_n,
  f990_p2_n,
  f990_p8_n,
  f990_p10_n
)

final_table <- long_table %>%
  pivot_wider(
    names_from  = metric,
    values_from = "Percent of Food-Aid Organizations"
  ) %>%
  arrange(year)

#### place all sheets in one excel together ####
# create workbook
wb <- createWorkbook()
addWorksheet(wb, "Summary_by_Year")
writeData(wb, "Summary_by_Year", final_table)

saveWorkbook(
  wb,
  "food_aid_990_summary.xlsx",
  overwrite = TRUE
)