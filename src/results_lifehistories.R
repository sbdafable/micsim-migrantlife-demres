# Generating plots of life histories using the synthetic population
# Run after main.R from the repository root.

library(dplyr)
library(ggplot2)
library(readxl)
library(patchwork)

project_root <- normalizePath(
  Sys.getenv("MIGRANTLIFE_PROJECT", unset = getwd()),
  mustWork = FALSE
)
maxAge <- 50
population_size <- 10000
cohort <- "80_03"

parse_state <- function(x) {
  x <- gsub("^\\(|\\)$", "", x)
  x <- gsub(". ", "/", x, fixed = TRUE)
  trimws(x)
}

state_by_age <- function(pop, maxAge) {
  spells <- pop %>%
    filter(state != "restCategory") %>%
    group_by(ID) %>%
    arrange(ID, transitionAge) %>%
    mutate(
      spell_state = state,
      age_start = transitionAge,
      age_end = lead(transitionAge, default = maxAge)
    ) %>%
    ungroup() %>%
    filter(age_start < maxAge)

  ages <- seq(0L, maxAge - 1L)
  do.call(rbind, lapply(ages, function(age) {
    spells %>%
      filter(age_start <= age & age_end > age) %>%
      group_by(spell_state) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(age = age + 1L)
  })) %>%
    rename(From = spell_state)
}

graph_combined <- function(df) {
  df_prop <- df %>%
    ungroup() %>%
    group_by(group) %>%
    filter(age >= 16, From != "dead") %>%
    mutate(
      sex = substr(From, 1, 1),
      state = substr(From, 3, nchar(From) - 2),
      parity = substr(From, nchar(From), nchar(From)),
      state = case_when(
        state == "c" ~ "Cohabiting",
        state == "mar" ~ "Married",
        state == "si" ~ "Single",
        state == "se" ~ "Separated"
      ),
      parity = ifelse(parity == "3", "3+", parity),
      state = paste(state, parity, sep = "/")
    ) %>%
    group_by(sex, age, state, group) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    group_by(age, group) %>%
    mutate(prop = n / sum(n)) %>%
    rename(State = state)

  colors <- c(
    "#F8766DFF", "#F8766DBF", "#F8766D7F", "#F8766D40",
    "#7CAE00FF", "#7CAE00BF", "#7CAE007F", "#7CAE0040",
    "#00BFC4FF", "#00BFC4BF", "#00BFC47F", "#00BFC440",
    "#C77CFFFF", "#C77CFFBF", "#C77CFF7F", "#C77CFF40"
  )

  ggplot(df_prop, aes(x = as.numeric(age), y = prop, fill = State)) +
    geom_area(position = "fill", linewidth = 0.2, colour = "black") +
    theme_bw() +
    labs(x = "Age", y = "Proportion") +
    scale_fill_manual(values = colors) +
    facet_wrap(~factor(group, levels = c(
      "Indian 1G", "Indian 2G+", "Pakistani 1G", "Pakistani 2G+",
      "Caribbean 1G", "Caribbean 2G+", "African 1G", "African 2G+",
      "European & Western 1G", "European & Western 2G+", "Native"
    )), ncol = 2)
}

origin_gens <- c(
  "Native", "Pakistan1G", "Pakistan2G", "Bangladesh1G", "Bangladesh2G",
  "India1G", "India2G", "African1G", "African2G", "Caribbean1G",
  "Caribbean2G", "EW1G", "EW2G", "Other1G", "Other2G"
)
origin_gens_adj <- c(
  "Native", "Pakistani 1G", "Pakistani 2G+", "Bangladesh 1G", "Bangladesh 2G+",
  "Indian 1G", "Indian 2G+", "African 1G", "African 2G+", "Caribbean 1G",
  "Caribbean 2G+", "European & Western 1G", "European & Western 2G+",
  "Other 1G", "Other 2G+"
)
groups_for_graph <- c(
  "Indian 1G", "Indian 2G+", "Pakistani 1G", "Pakistani 2G+",
  "Caribbean 1G", "Caribbean 2G+", "African 1G", "African 2G+",
  "European & Western 1G", "European & Western 2G+", "Native"
)

read_population <- function(origin_generation) {
  path <- file.path(
    project_root,
    "results",
    paste0(
      "birth_cohort_pop_", population_size, "_origin_",
      origin_generation, "cohort_", cohort, ".xlsx"
    )
  )

  raw <- read_excel(path) %>%
    mutate(
      state = parse_state(NewState),
      transitionAge = AgeAtTr
    ) %>%
    group_by(ID) %>%
    arrange(ID, transitionAge) %>%
    ungroup()

  sex_by_id <- raw %>%
    group_by(ID) %>%
    slice_min(transitionAge, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(ID, sex = substr(state, 1, 1))

  left_join(raw, sex_by_id, by = "ID")
}

dfs_cohort <- lapply(origin_gens, read_population)

make_sex_plot <- function(sex_code, plot_title) {
  dfs_state_by_age <- Map(
    function(pop, group_name) {
      pop %>%
        filter(sex == sex_code) %>%
        state_by_age(maxAge) %>%
        mutate(group = group_name)
    },
    dfs_cohort,
    origin_gens_adj
  )

  cohort_combined <- do.call(rbind, dfs_state_by_age) %>%
    subset(group %in% groups_for_graph)

  graph_combined(cohort_combined) + ggtitle(plot_title)
}

life_histories_male <- make_sex_plot("m", "Male")
life_histories_female <- make_sex_plot("f", "Female")
life_histories_by_sex <-
  (life_histories_male | life_histories_female) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

life_histories_by_sex


