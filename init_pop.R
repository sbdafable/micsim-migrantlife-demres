# Initial population for a synthetic birth-cohort projection.

init_birth_cohort <- function(n, birth_year) {
  birth_yyyymmdd <- birth_year * 10000 + 101
  birth_dates <- rep(yyyymmdd_to_chron(birth_yyyymmdd), n)
  init_states <- rep(c("m/si/0", "f/si/0"), n / 2)

  data.frame(
    ID = seq_len(n),
    birthDate = birth_dates,
    initState = init_states,
    stringsAsFactors = FALSE
  )
}
