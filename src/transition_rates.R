# Build rate functions for one origin/generation/cohort combination.

library(dplyr)
library(tidyr)

mort_rates <- read.table(
  file.path(project_root, "transition_rates", "mortality_rates_EW_2018_2020.csv"),
  header = TRUE,
  sep = ",",
  fileEncoding = "UTF-8-BOM"
) %>%
  pivot_longer(2:3)

f_mort_rates <- mort_rates %>% filter(name == "female")
m_mort_rates <- mort_rates %>% filter(name == "male")

return_mort_rate <- function(df, age) {
  sapply(age, function(a) df$value[df$age == floor(a)])
}

f_mort <- function(age, calTime, duration) return_mort_rate(f_mort_rates, age)
m_mort <- function(age, calTime, duration) return_mort_rate(m_mort_rates, age)

read_rates <- function(project_root) {
  read.table(
    file.path(project_root, "transition_rates", "uk_coefficients_parity_1970plus_cohort.csv"),
    header = TRUE,
    sep = ",",
    fileEncoding = "UTF-8-BOM"
  ) %>%
    mutate(sex = ifelse(sex == "female", "f", "m"))
}

process_si_rates <- function(rates, generation, origin, cohort) {
  rates %>%
    filter(state == "single") %>%
    group_by(sex) %>%
    filter(grepl(paste(origin, "age", "constant", "cohort", "Parity", sep = "|"), name)) %>%
    mutate(
      constant = estimate[name == "constant"],
      cohab = estimate[name == paste("C#", generation, ", ", origin, sep = "")],
      marr = estimate[name == paste("M#", generation, ", ", origin, sep = "")],
      birth = estimate[name == paste("B#", generation, ", ", origin, sep = "")],
      cohort = estimate[name == paste("cohort_", cohort, sep = "")],
      b1 = estimate[name == "Parity_1"],
      b2 = estimate[name == "Parity_2"],
      b3 = estimate[name == "Parity_3+"]
    ) %>%
    filter(grepl("age", name)) %>%
    mutate(
      c_b0 = cohab + estimate + constant + cohort,
      mar_b0 = marr + estimate + constant + cohort,
      birth_b0 = birth + estimate + constant + cohort,
      c_b1 = c_b0 + b1,
      mar_b1 = mar_b0 + b1,
      birth_b1 = birth_b0 + b1,
      c_b2 = c_b0 + b2,
      mar_b2 = mar_b0 + b2,
      birth_b2 = birth_b0 + b2,
      c_b3 = c_b0 + b3,
      mar_b3 = mar_b0 + b3,
      birth_b3 = birth_b0 + b3
    ) %>%
    mutate(
      age = paste(substr(name, 5, 6), "-", substr(name, 8, 9), sep = ""),
      start = as.numeric(substr(age, 1, 2)),
      end = as.numeric(substr(age, 4, 5)) + 1
    ) %>%
    select(matches("sex|start|end|_")) %>%
    pivot_longer(2:13) %>%
    mutate(value = exp(value) * 12)
}

process_c_rates <- function(rates, generation, origin, cohort) {
  rates %>%
    filter(state == "cohab") %>%
    group_by(sex) %>%
    filter(grepl(paste(origin, "age", "constant", "cohort", "Parity", sep = "|"), name)) %>%
    mutate(
      constant = estimate[name == "constant"],
      sep = estimate[name == paste("Sep#", generation, ", ", origin, sep = "")],
      marr = estimate[name == paste("M#", generation, ", ", origin, sep = "")],
      birth = estimate[name == paste("B#", generation, ", ", origin, sep = "")],
      cohort = estimate[name == paste("cohort_", cohort, sep = "")],
      b1 = estimate[name == "Parity_1"],
      b2 = estimate[name == "Parity_2"],
      b3 = estimate[name == "Parity_3+"]
    ) %>%
    filter(grepl("age", name)) %>%
    mutate(
      se_b0 = sep + estimate + constant + cohort,
      mar_b0 = marr + estimate + constant + cohort,
      birth_b0 = birth + estimate + constant + cohort,
      se_b1 = se_b0 + b1,
      mar_b1 = mar_b0 + b1,
      birth_b1 = birth_b0 + b1,
      se_b2 = se_b0 + b2,
      mar_b2 = mar_b0 + b2,
      birth_b2 = birth_b0 + b2,
      se_b3 = se_b0 + b3,
      mar_b3 = mar_b0 + b3,
      birth_b3 = birth_b0 + b3
    ) %>%
    mutate(
      age = paste(substr(name, 5, 6), "-", substr(name, 8, 9), sep = ""),
      start = as.numeric(substr(age, 1, 2)),
      end = as.numeric(substr(age, 4, 5)) + 1
    ) %>%
    select(matches("sex|start|end|_")) %>%
    pivot_longer(2:13) %>%
    mutate(value = exp(value) * 12)
}

process_se_rates <- function(rates, generation, origin, cohort) {
  rates %>%
    filter(state == "single") %>%
    group_by(sex) %>%
    filter(grepl(paste(origin, "age", "constant", "sep", "cohort", "Parity", sep = "|"), name)) %>%
    mutate(
      constant = estimate[name == "constant"],
      cohab = estimate[name == paste("C#", generation, ", ", origin, sep = "")],
      marr = estimate[name == paste("M#", generation, ", ", origin, sep = "")],
      birth = estimate[name == paste("B#", generation, ", ", origin, sep = "")],
      sep = estimate[name == "sep"],
      cohort = estimate[name == paste("cohort_", cohort, sep = "")],
      b1 = estimate[name == "Parity_1"],
      b2 = estimate[name == "Parity_2"],
      b3 = estimate[name == "Parity_3+"]
    ) %>%
    filter(grepl("age", name)) %>%
    mutate(
      c_b0 = cohab + estimate + constant + sep + cohort,
      mar_b0 = marr + estimate + constant + sep + cohort,
      birth_b0 = birth + estimate + constant + sep + cohort,
      c_b1 = c_b0 + b1,
      mar_b1 = mar_b0 + b1,
      birth_b1 = birth_b0 + b1,
      c_b2 = c_b0 + b2,
      mar_b2 = mar_b0 + b2,
      birth_b2 = birth_b0 + b2,
      c_b3 = c_b0 + b3,
      mar_b3 = mar_b0 + b3,
      birth_b3 = birth_b0 + b3
    ) %>%
    mutate(
      age = paste(substr(name, 5, 6), "-", substr(name, 8, 9), sep = ""),
      start = as.numeric(substr(age, 1, 2)),
      end = as.numeric(substr(age, 4, 5)) + 1
    ) %>%
    select(matches("sex|start|end|_")) %>%
    pivot_longer(2:13) %>%
    mutate(value = exp(value) * 12)
}

process_mar_rates <- function(rates, generation, origin, cohort) {
  rates %>%
    filter(state == "married") %>%
    group_by(sex) %>%
    filter(grepl(paste(origin, "age", "constant", "cohort", "Parity", sep = "|"), name)) %>%
    mutate(
      constant = estimate[name == "constant"],
      sep = estimate[name == paste("Sep#", generation, ", ", origin, sep = "")],
      birth = estimate[name == paste("B#", generation, ", ", origin, sep = "")],
      cohort = estimate[name == paste("cohort_", cohort, sep = "")],
      b1 = estimate[name == "Parity_1"],
      b2 = estimate[name == "Parity_2"],
      b3 = estimate[name == "Parity_3+"]
    ) %>%
    filter(grepl("age", name)) %>%
    mutate(
      se_b0 = sep + estimate + constant + cohort,
      birth_b0 = birth + estimate + constant + cohort,
      se_b1 = se_b0 + b1,
      birth_b1 = birth_b0 + b1,
      se_b2 = se_b0 + b2,
      birth_b2 = birth_b0 + b2,
      se_b3 = se_b0 + b3,
      birth_b3 = birth_b0 + b3
    ) %>%
    mutate(
      age = paste(substr(name, 5, 6), "-", substr(name, 8, 9), sep = ""),
      start = as.numeric(substr(age, 1, 2)),
      end = as.numeric(substr(age, 4, 5)) + 1
    ) %>%
    select(matches("sex|start|end|_")) %>%
    pivot_longer(2:9) %>%
    mutate(value = exp(value) * 12)
}

rates <- read_rates(project_root)
rates_si <- process_si_rates(rates, generation, origin, cohort)
rates_c <- process_c_rates(rates, generation, origin, cohort)
rates_mar <- process_mar_rates(rates, generation, origin, cohort)
rates_se <- process_se_rates(rates, generation, origin, cohort)

return_rate <- function(df, age) {
  dplyr::case_when(
    (age >= df$start[1]) & (age < df$end[1]) ~ df$value[1],
    (age >= df$start[2]) & (age < df$end[2]) ~ df$value[2],
    (age >= df$start[3]) & (age < df$end[3]) ~ df$value[3],
    (age >= df$start[4]) & (age < df$end[4]) ~ df$value[4],
    (age >= df$start[5]) & (age < df$end[5]) ~ df$value[5],
    (age >= df$start[6]) & (age < df$end[6]) ~ df$value[6],
    (age >= df$start[7]) & (age < df$end[7]) ~ df$value[7],
    .default = 0
  )
}

part_source <- list(si = rates_si, c = rates_c, mar = rates_mar, se = rates_se)
make_rate_fn <- function(rate_df) {
  force(rate_df)
  function(age, calTime, duration) return_rate(rate_df, age)
}

transition_fn_names <- unique(c(fertTrMatrix[, 2], partTrMatrix[, 2]))
for (fn_name in transition_fn_names) {
  bits <- strsplit(fn_name, "_", fixed = TRUE)[[1]]
  rate_df <- part_source[[bits[2]]]
  rate_df <- rate_df[
    rate_df$name == paste0(bits[4], "_b", bits[3]) & rate_df$sex == bits[1],
  ]
  assign(fn_name, make_rate_fn(rate_df), envir = .GlobalEnv)
}
