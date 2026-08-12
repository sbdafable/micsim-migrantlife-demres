# Main code for running the demographic microsimulation
# Run from the repository root, or set MIGRANTLIFE_PROJECT to its path.

library(writexl)

project_root <- normalizePath(
  Sys.getenv("MIGRANTLIFE_PROJECT", unset = getwd()),
  mustWork = FALSE
)
src_path <- file.path(project_root, "src")
miccore_path <- Sys.getenv(
  "MICCORE_PATH",
  unset = file.path(project_root, "fastMicSim-master")
)

source(file.path(miccore_path, "auxFctInMicSim.r"))
source(file.path(miccore_path, "auxFctOutMicSim.r"))
source(file.path(src_path, "miccore_io.R"))
source(file.path(src_path, "simulation_parameters.R"))
source(file.path(src_path, "init_pop.R"))

set.seed(123)
dir.create(pathInputData, recursive = TRUE, showWarnings = FALSE)
dir.create(pathOutputData, recursive = TRUE, showWarnings = FALSE)
results_dir <- file.path(project_root, "results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

params <- list(
  list("80_03", "Native", ""),
  list("80_03", "Pakistan", "1G"), 
  list("80_03", "Pakistan", "2G"),
  list("80_03", "India", "1G"), 
  list("80_03", "India", "2G"),
  list("80_03", "Caribbean", "1G"), 
  list("80_03", "Caribbean", "2G"),
  list("80_03", "Bangladesh", "1G"), 
  list("80_03", "Bangladesh", "2G"),
  list("80_03", "African", "1G"), 
  list("80_03", "African", "2G"),
  list("80_03", "EW", "1G"), 
  list("80_03", "EW", "2G"),
  list("80_03", "Other", "1G"), 
  list("80_03", "Other", "2G")
)

run_status <- list()
for (parameter in params) {
  generation <- parameter[[3]]
  origin <- parameter[[2]]
  cohort <- parameter[[1]]
  run_label <- paste0(origin, generation)
  
  cat("=== Origin:", origin, "| Generation:", generation, "| Cohort:", cohort, "===\n")
  
  status <- tryCatch(
    withCallingHandlers({
      # Rates depend on the current origin, generation, and cohort.
      source(file.path(src_path, "transition_rates.R"))
      initPop <- init_birth_cohort(n, birth_cohort_year)
      
      pop <- run_fastmicsim(
        pathInputData = pathInputData,
        pathOutputData = pathOutputData,
        pathMicCore = pathMicCore,
        startDate = yyyymmdd_to_dmy_string(startDate),
        stopDate = yyyymmdd_to_dmy_string(endDate)
      )
      
      # chron stores dates as day counts; convert them for Excel output.
      pop$BirthDate <- chron_to_date(pop$BirthDate)
      pop$TrDate <- chron_to_date(pop$TrDate)
      
      out_file <- file.path(
        results_dir,
        paste0(
          "birth_cohort_pop_", n, "_origin_", origin, generation,
          "cohort_", cohort, ".xlsx"
        )
      )
      write_xlsx(pop, out_file)
      cat("  -> wrote", basename(out_file), "(", nrow(pop), "rows)\n")
      "OK"
    }, warning = function(w) {
      cat("  -> warning:", conditionMessage(w), "\n")
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      cat("  -> FAILED:", conditionMessage(e), "\n")
      paste("ERROR:", conditionMessage(e))
    }
  )
  run_status[[run_label]] <- status
}

cat("\n=== Summary ===\n")
for (name in names(run_status)) {
  cat(sprintf("%-20s %s\n", name, run_status[[name]]))
}
