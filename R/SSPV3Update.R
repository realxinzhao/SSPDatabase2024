library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
require(readr)
require(readxl)

library(gcamdata)


# gcamdata is requred to use gcamdata::left_join_error_no_match()


# need to download the source data from SSP data base explorer
SSPv3 <- readxl::read_excel("data/SSPv3.0.1/1710759470883-ssp_basic_drivers_release_3.0.1_full.xlsx",
                            sheet = 2)

# Only keep needed model and variables
SSPv3 %>%
  filter(Model == "IIASA-WiC POP 2023") %>%
  filter(!grepl("\\(|World", Region)) %>%
  bind_rows(
    SSPv3 %>%
      filter(Model == "OECD ENV-Growth 2023", Variable == "GDP|PPP") %>%
      filter(!grepl("\\(|World", Region))
  ) -> SSP_database_2024

# readr::write_csv("SSP_database_2024.csv")


# export .gz
gcam_dataset <- "SSP_database_2024"
col_type_nonyear <- "ccccc"
title <- "SSP database version 3.0.1 updated in 2024"
description <- "SSP database downloaded May 2024"
out_dir = "."
GZIP = T


col_type = paste0(col_type_nonyear, paste0(rep("n", ncol(get(gcam_dataset, envir = parent.frame())) - nchar(col_type_nonyear)), collapse = "") )

  cmnts <- c(
    paste0("File: ", gcam_dataset, ifelse(GZIP, ".csv.gz", ".csv")),
    paste0("Title: ", title),
    paste0("Units: ", "various"),
    paste0("Description:  ", description),
    paste0("Data source: IIASA SSP database"),
    paste0("Date of CSV last update: ", Sys.Date()),
    paste0("Column types: ",col_type) ,
    "----------"
  )
  fqfn <- file.path(".", paste0(gcam_dataset, ".csv"))
  suppressWarnings(file.remove(fqfn))

  if (GZIP == F) {
    cat(paste("#", cmnts), file = fqfn, sep = "\n", append = TRUE)
    readr::write_csv(get(gcam_dataset, envir = parent.frame()), fqfn, append = TRUE, col_names = TRUE, na = "")
  } else {
    cat(paste("#", cmnts), file = gzfile(paste0(fqfn, ".gz")), sep = "\n", append = TRUE)
    readr::write_csv(get(gcam_dataset, envir = parent.frame()), gzfile(paste0(fqfn, ".gz")), append = TRUE, col_names = TRUE, na = "")
  }



  # mappings----
  iso_GCAM_regID <- readr::read_csv("data/SSP/iso_GCAM_regID.csv", comment = "#")
  Regmapping <- readr::read_csv("data/SSP/Regmapping.csv", comment = "#")
  # SSP regions added
  iso_SSP_regID <- readr::read_csv("data/SSPv3.0.1/iso_GCAM_regID.csv", comment = "#")
  pop_laborforce_variable <- readr::read_csv("data/SSPv3.0.1/pop_laborforce_variable.csv", comment = "#")



  SSPv3 %>%
    # make variable names lower case
    rename_all(tolower) %>%
    # remove aggregated regions
    filter(!grepl("\\(|World", region)) %>%
    left_join_error_no_match(
      iso_GCAM_regID %>% distinct(iso, region = ssp_country_name),
      by = "region") %>%
    gather_years() %>%
    year <= max(MODEL_FUTURE_YEARS) ->
    SSPv3_New

  SSP_database_v9 %>%
    rename_all(tolower) %>%
    mutate(iso = tolower(region),
           scenario = substr(scenario, 1, 4)) %>%
    gather_years() %>%
    filter(year <= max(MODEL_FUTURE_YEARS)) ->
    SSPv1_Old


  # Population scenarios (IIASA-WiC POP) ----

  SSPv3_New %>%
    filter(model == "IIASA-WiC POP 2023", variable == "Population") %>%
    mutate(model = gsub(" 2023", "", model))->
    SSPv3_pop0


  SSPv3_New %>%
    filter(model == "IIASA-WiC POP 2023") %>%
    mutate(model = gsub(" 2023", "", model))->
    SSPv3_pop0

  # Using the Historical Reference scenario to fill history of SSPs
  SSPv3_pop0 %>%
    filter(scenario != "Historical Reference") %>%
    left_join(
      SSPv3_pop0 %>% filter(scenario == "Historical Reference") %>% select(-scenario) %>%
        rename(hist = value),
      by = c("model", "region", "variable", "unit", "iso", "year")
    ) %>%
    mutate(value = if_else(year <= 2015, hist, value)) %>%
    select(-hist) ->
    SSPv3_pop


  SSPv1_Old %>%
    filter(model == "IIASA-WiC POP") %>% ->
    SSPv1_pop

  SSPv3_pop %>% mutate(version = "sspv3") %>%
    bind_rows(SSPv1_pop %>% mutate(version = "sspv1")) ->
    SSP_update_pop
