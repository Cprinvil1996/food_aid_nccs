library(readr)
library(tidyverse)
library(dplyr)
library(purrr)
library(openxlsx)

## Let's laod all the data first ####
years <- 2009:2023
base_url <- "https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_1/"

#### Load 990 Files - Part 1 ####
f990_p1_data <- years |>
  map(~ read_csv(
    paste0(base_url, "F9-P01-T00-SUMMARY-", .x, ".CSV"),
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

# number of unique EINs #

EIN_COUNT <- length(unique(food_aid$ORG_EIN))

### Create a function that cleans, filters, and summarises food data ###

food_aid_percent <- function(food_part_data, food_aid, EIN_COUNT) {
  food_part_data |>
    dplyr::distinct(ORG_EIN, TAX_YEAR, RETURN_TYPE) |> #cleans up the summary data
    dplyr::filter(ORG_EIN %in% food_aid$ORG_EIN) |> #only keeps EIN found in 990
    dplyr::group_by(TAX_YEAR,RETURN_TYPE) |>
    dplyr::summarise(
      `Percent of Food-Aid Organizations` = n_distinct(ORG_EIN) 
      / EIN_COUNT,
      .groups = "drop"
    ) 
}

### Number of nonprofits filed 990 & 990EZ By Year###
f990_p1_n <- food_aid_percent(f990_p1_data, food_aid, EIN_COUNT)
f990_p10_n <- food_aid_percent(f990_p10_data, food_aid, EIN_COUNT)
f990_p8_n <- food_aid_percent(f990_p8_data, food_aid, EIN_COUNT)

### Place all the outputs in one excel sheet ###
f990_p1_n <- f990_p1_n %>%
  mutate(metric = "f990_p1_n")

f990_p8_n <- f990_p8_n %>%
  mutate(metric = "f990_p8_n")

f990_p10_n <- f990_p10_n %>%
  mutate(metric = "f990_p10_n")

fix_tax_year <- function(df) {
  df %>% mutate(TAX_YEAR = as.numeric(TAX_YEAR))
}

long_table <- bind_rows(
  fix_tax_year(f990_p1_n),
  fix_tax_year(f990_p8_n),
  fix_tax_year(f990_p10_n)
)

final_table <- long_table %>%
  pivot_wider(
    names_from  = metric,
    values_from = "Percent of Food-Aid Organizations"
  ) %>%
  arrange(TAX_YEAR)

#### place all sheets in one excel together ####
# create workbook
wb <- createWorkbook()
addWorksheet(wb, "Summary_by_Year")
writeData(wb, "Summary_by_Year", final_table)

saveWorkbook(
  wb,
  "data/food_aid_990_summary.xlsx",
  overwrite = TRUE
)