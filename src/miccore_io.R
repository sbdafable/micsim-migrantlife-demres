# Compatibility and I/O helpers between the model and fastMicSim/MicCore.

library(chron)

# MicSim-style yyyymmdd values are converted to chron dates because MicCore
# and fastMicSim use chron objects internally.
yyyymmdd_to_chron <- function(yyyymmdd) {
  yyyymmdd <- as.character(yyyymmdd)
  dmy <- paste(
    substr(yyyymmdd, 7, 8),
    substr(yyyymmdd, 5, 6),
    substr(yyyymmdd, 1, 4),
    sep = "/"
  )
  chron(
    dates = dmy,
    format = c(dates = "d/m/Y"),
    out.format = c(dates = "d/m/year")
  )
}

yyyymmdd_to_dmy_string <- function(yyyymmdd) {
  yyyymmdd <- as.character(yyyymmdd)
  paste(
    substr(yyyymmdd, 7, 8),
    substr(yyyymmdd, 5, 6),
    substr(yyyymmdd, 1, 4),
    sep = "/"
  )
}

set_sim_horizon_from_yyyymmdd <- function(startDate, endDate) {
  setSimHorizon(
    startDate = yyyymmdd_to_dmy_string(startDate),
    endDate = yyyymmdd_to_dmy_string(endDate)
  )
}

# The model uses compound states (for example, f/si/0), whereas the
# fastMicSim helper matches one state dimension at a time.
build_transition_matrix_compound <- function(allTransitions, absTransitions, stateSpace) {
  if (is.vector(absTransitions)) {
    absTransitions <- matrix(absTransitions, ncol = 2, nrow = 1)
  }

  absStates <- absTransitions[, 1]
  stateNames <- apply(stateSpace, 1, paste, collapse = "/")
  transitionMatrix <- matrix(
    "0",
    nrow = length(stateNames),
    ncol = length(stateNames) + length(absStates),
    dimnames = list(stateNames, c(stateNames, absStates))
  )

  for (i in seq_len(nrow(absTransitions))) {
    transitionMatrix[, absTransitions[i, 1]] <- absTransitions[i, 2]
  }

  if (!is.null(allTransitions)) {
    for (i in seq_len(nrow(allTransitions))) {
      parts <- strsplit(allTransitions[i, 1], "->", fixed = TRUE)[[1]]
      transitionMatrix[parts[1], parts[2]] <- allTransitions[i, 2]
    }
  }

  transitionMatrix
}

patch_abs_transition_by_rule <- function(transitionMatrix, absState, rule_fn) {
  column <- which(colnames(transitionMatrix) == absState)
  for (origin_state in rownames(transitionMatrix)) {
    transitionMatrix[origin_state, column] <- rule_fn(origin_state)
  }
  transitionMatrix
}

# These definitions replace incompatible helpers in older fastMicSim releases.
isLeapYear <- function(year) {
  ((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)
}

giveCorrectAge <- function(bd, cd) {
  year_of <- function(x) chron::month.day.year(x)$year
  birth_year <- year_of(bd)
  current_year <- year_of(cd)
  complete_years <- current_year - birth_year - 1
  days_in_birth_year <- ifelse(isLeapYear(birth_year), 366, 365)
  days_in_current_year <- ifelse(isLeapYear(current_year), 366, 365)
  fraction_birth_year <- as.numeric(
    chron(paste(31, "/", 12, "/", birth_year), format = c(dates = "d/m/y")) - bd
  ) / days_in_birth_year
  fraction_current_year <- as.numeric(
    cd - chron(paste(1, "/", 1, "/", current_year), format = c(dates = "d/m/y")) + 1
  ) / days_in_current_year
  complete_years + fraction_birth_year + fraction_current_year
}

# The fastMicSim writer reads initPop and model settings from the global scope.
writeInputMicCore <- function(pathOut, mig = FALSE) {
  year_of <- function(x) chron::month.day.year(x)$year
  month_of <- function(x) chron::month.day.year(x)$month
  day_of <- function(x) chron::month.day.year(x)$day

  initPopPath <- file.path(pathOut, "initPop.txt")
  initPopR <- initPop[, -1]
  initPopR[, 2] <- paste0("(", gsub("/", ". ", initPopR[, 2], fixed = TRUE), ")")
  initPopR[, 1] <- paste(
    year_of(initPopR[, 1]), month_of(initPopR[, 1]), day_of(initPopR[, 1]),
    sep = "-"
  )
  write.table(initPopR, file = initPopPath, quote = FALSE, row.names = FALSE)

  if (mig) {
    immigrPopPath <- file.path(pathOut, "immigrPop.txt")
    immigrPopR <- immigrPop[, -1]
    immigrPopR[, 3] <- paste0("(", gsub("/", ". ", immigrPopR[, 3], fixed = TRUE), ")")
    immigrPopR[, 1] <- paste(
      year_of(immigrPopR[, 1]), month_of(immigrPopR[, 1]), day_of(immigrPopR[, 1]),
      sep = "-"
    )
    immigrPopR[, 2] <- paste(
      year_of(immigrPopR[, 2]), month_of(immigrPopR[, 2]), day_of(immigrPopR[, 2]),
      sep = "-"
    )
    write.table(immigrPopR, file = immigrPopPath, quote = FALSE, row.names = FALSE)
  }

  origin_states <- character()
  destination_states <- character()
  start_year <- year_of(simHorizon[1])
  end_year <- year_of(simHorizon[2])
  rates_array <- array(
    NA,
    dim = c(maxAge + 1, sum(transitionMatrix != "0"), end_year - start_year + 1)
  )
  ages <- 0:maxAge
  index <- 1

  for (i in seq_len(nrow(transitionMatrix))) {
    for (j in seq_len(ncol(transitionMatrix))) {
      if (transitionMatrix[i, j] != "0") {
        origin_states <- c(origin_states, rownames(transitionMatrix)[i])
        destination_states <- c(destination_states, colnames(transitionMatrix)[j])
        for (year in start_year:end_year) {
          values <- eval(do.call(
            transitionMatrix[i, j],
            args = list(age = ages, calTime = year, duration = 0)
          ))
          values[values == Inf] <- 1000
          rates_array[, index, year - start_year + 1] <- values
        }
        index <- index + 1
      }
    }
  }

  trRatesPath <- file.path(pathOut, "trRates.txt")
  if (file.exists(trRatesPath)) file.remove(trRatesPath)
  connection <- file(trRatesPath, "a")
  cat(c(start_year, end_year), file = connection, sep = " ")
  cat("\n", file = connection)
  cat(paste0("(", paste(initStatesFem, collapse = ". "), ")"), file = connection)
  cat("\n", file = connection)
  cat(initStatesProbFem, file = connection)
  cat("\n", file = connection)
  cat(paste0("(", paste(initStatesMale, collapse = ". "), ")"), file = connection)
  cat("\n", file = connection)
  cat(initStatesProbMale, file = connection)
  cat("\n", file = connection)
  cat(sexRatio, file = connection)
  cat("\n", file = connection)
  cat("(dead)", file = connection)
  cat("\n", file = connection)
  cat("(rest)", file = connection)
  cat("\n", file = connection)
  cat(maxAge, file = connection)
  cat("\n", file = connection)
  cat(paste0("(", gsub("/", ". ", origin_states, fixed = TRUE), ")"), file = connection, sep = " ")
  cat("\n", file = connection)
  cat(paste0("(", gsub("/", ". ", destination_states, fixed = TRUE), ")"), file = connection, sep = " ")
  cat("\n", file = connection)

  for (year in start_year:end_year) {
    write.table(
      rates_array[, , year - start_year + 1],
      file = connection,
      append = TRUE,
      quote = FALSE,
      col.names = FALSE,
      row.names = FALSE
    )
    cat("\n", file = connection)
  }
  close(connection)
}

chron_to_date <- function(x) {
  ymd <- chron::month.day.year(x)
  as.Date(sprintf("%04d-%02d-%02d", ymd$year, ymd$month, ymd$day))
}

build_command_quoted <- function(pathInputData, pathOutputData, pathMicCore,
                                 startDate, stopDate, seed = 2365, mig = FALSE) {
  quote_path <- function(x) paste0("\"", x, "\"")
  command <- c(
    "java -Xmx1100m -Xms1100m -jar",
    quote_path("MicCore1.0.jar"),
    paste(seed, startDate, stopDate),
    quote_path(file.path(pathInputData, "trRates.txt")),
    quote_path(file.path(pathInputData, "initPop.txt")),
    if (mig) quote_path(file.path(pathInputData, "immigrPop.txt")) else "\"\"",
    quote_path(pathOutputData)
  )
  paste(command, collapse = " ")
}

run_fastmicsim <- function(pathInputData, pathOutputData, pathMicCore,
                           startDate, stopDate, seed = 2365) {
  writeInputMicCore(pathInputData, mig = FALSE)

  trRatesPath <- file.path(pathInputData, "trRates.txt")
  trLines <- readLines(trRatesPath)
  naCount <- sum(grepl("\\bNA\\b", trLines))
  if (naCount > 0) {
    warning(sprintf("%d NA values in trRates.txt - replacing with 0", naCount))
    writeLines(gsub("\\bNA\\b", "0", trLines), trRatesPath)
  }

  staleFiles <- list.files(pathOutputData, full.names = TRUE)
  staleFiles <- staleFiles[!file.info(staleFiles)$isdir]
  if (length(staleFiles) > 0) file.remove(staleFiles)

  command <- build_command_quoted(
    pathInputData = pathInputData,
    pathOutputData = pathOutputData,
    pathMicCore = pathMicCore,
    startDate = startDate,
    stopDate = stopDate,
    seed = seed,
    mig = FALSE
  )

  old_directory <- getwd()
  setwd(pathMicCore)
  on.exit(setwd(old_directory), add = TRUE)
  mic_output <- suppressWarnings(system(
    command = paste(command, "2>&1"),
    intern = TRUE
  ))
  exit_status <- attr(mic_output, "status")
  if (!is.null(exit_status) && exit_status != 0) {
    stop(sprintf(
      "MicCore.jar exited with status %s. Its own output:\n%s",
      exit_status,
      paste(mic_output, collapse = "\n")
    ))
  }

  output_files <- list.files(pathOutputData)
  if (!any(grepl("birth", output_files)) || !any(grepl("phase", output_files))) {
    stop(sprintf(
      paste(
        "MicCore.jar did not produce output files in %s. Check that",
        "MicCore1.0.jar is present in %s and that Java is on PATH.",
        "MicCore output was:\n%s"
      ),
      pathOutputData,
      pathMicCore,
      paste(mic_output, collapse = "\n")
    ))
  }

  files <- getLatestFiles(pathOutputData)
  data <- readData(files$birthfile, files$phasefile)
  constructEventHistories(data)
}
