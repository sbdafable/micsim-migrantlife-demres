# Specifying the simulation parameters
# including the state space, simulation horizon, and transition functions

project_root <- normalizePath(
  Sys.getenv("MIGRANTLIFE_PROJECT", unset = getwd()),
  mustWork = FALSE
)
pathMicCore <- Sys.getenv(
  "MICCORE_PATH",
  unset = file.path(project_root, "fastMicSim-master")
)
pathInputData <- file.path(project_root, "input")
pathOutputData <- file.path(project_root, "output")

# State space
sex <- c("f", "m")
part <- c("si", "c", "mar", "se")
fert <- c("0", "1", "2", "3")
stateSpace <- expand.grid(sex = sex, part = part, fert = fert)
part_transitions <- c(
  "si->c", "si->mar", "c->mar", "c->se",
  "mar->se", "se->c", "se->mar"
)
zero_rate <- function(age, calTime, duration) {
  rep(0, length(age))
}

# Birth-cohort simulation horizon and population size.
startDate <- 20120101
endDate <- 20470101
simHorizon <- set_sim_horizon_from_yyyymmdd(startDate, endDate)
maxAge <- 50
n <- 10000
birth_cohort_year <- 1996

# Generate full compound-state transition names used by this model.
gen_fert_matrix <- function(sex, part, fert) {
  fert_transitions <- character()
  fert_functions <- character()

  for (i in sex) {
    for (j in part) {
      for (k in fert[-length(fert)]) {
        state <- paste(i, j, k, sep = "/")
        destination <- paste(i, j, as.numeric(k) + 1, sep = "/")
        fert_transitions <- c(fert_transitions, paste(state, destination, sep = "->"))
        fert_functions <- c(fert_functions, paste(i, j, k, "birth", sep = "_"))
      }
    }
  }

  cbind(fert_transitions, fert_functions)
}

gen_part_matrix <- function(sex, fert, part_transitions) {
  transition_names <- character()
  function_names <- character()

  for (i in sex) {
    for (j in fert) {
      for (k in part_transitions) {
        parts <- strsplit(k, "->", fixed = TRUE)[[1]]
        origin <- paste(i, parts[1], j, sep = "/")
        destination <- paste(i, parts[2], j, sep = "/")
        transition_names <- c(transition_names, paste(origin, destination, sep = "->"))
        function_names <- c(function_names, paste(i, parts[1], j, parts[2], sep = "_"))
      }
    }
  }

  cbind(transition_names, function_names)
}

fertTrMatrix <- gen_fert_matrix(sex, part, fert)
partTrMatrix <- gen_part_matrix(sex, fert, part_transitions)
allTransitions <- rbind(fertTrMatrix, partTrMatrix)

# MicCore requires one deterministic newborn state for each sex.
initStatesFem <- c("f", "si", "0")
initStatesMale <- c("m", "si", "0")
initStatesProbFem <- 1
initStatesProbMale <- 1
sexRatio <- 0.5

# MicCore requires shared absorbing-state labels. Mortality is dispatched by sex.
absTransitions <- rbind(c("dead", "f_mort"), c("rest", "zero_rate"))
transitionMatrix <- build_transition_matrix_compound(
  allTransitions = allTransitions,
  absTransitions = absTransitions,
  stateSpace = stateSpace
)
transitionMatrix <- patch_abs_transition_by_rule(
  transitionMatrix,
  "dead",
  function(origin_state) {
    if (startsWith(origin_state, "f/")) "f_mort" else "m_mort"
  }
)
