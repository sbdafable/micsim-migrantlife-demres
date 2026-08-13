# MigrantLife

**Continuous-time microsimulation of the partnership and childbearing trajectories
of migrants and their descendants in the United Kingdom**
  
This repository contains code for projecting life courses using continuous-time
demographic microsimulation. In particular, it reproduces synthetic cohort projections
of the partnership and parity trajectories of first- and second-generation migrants
in the UK, as shown in Figure 2 of Ibbetson et al. (forthcoming in Demographic Research).

It uses fastMicSim, developed by Sabine Zinn, which delegates simulation to a Java
engine (MicCore.jar) to substantially reduce runtimes compared to base-R MicSim.

Read more about demographic microsimulation and the MicSim package:  
  https://microsimulation.pub/articles/00105

## Repository layout

```text
src/                R scripts
transition_rates/   model input CSV files
figures/            contains plot of life histories
```

The two inputs expected under `transition_rates/` are mortality rates
`mortality_rates_EW_2018_2020.csv` and the multi-state transition rates
`uk_coefficients_parity_1970plus_cohort.csv`. We assume a closed population,
but codes can be modified to include migration rates where available.

## Requirements

- Install fastMicSim: https://github.com/bieneSchwarze/fastMicSim
- R with the `chron`, `dplyr`, `tidyr`, `ggplot2`, `writexl`, `readxl`,
and `patchwork` packages
- Java on `PATH` for MicCore
- MicCore1.0.jar and the input CSV files described above

## Running

Run commands from the repository root:
  
  ```sh
Rscript src/main.R
Rscript src/results_lifehistories.R
```

`main.R` runs all configured origin/generation combinations and writes Excel
life-history files under `results/`. The plotting script reads those files and
returns a combined male/female plot in an interactive R session.

By default the current working directory is treated as the repository root.
For another working directory, set the project path explicitly:
  
To use a different repository location or a different fastMicSim folder,
set these before sourcing `main.R`

```r
Sys.setenv(MIGRANTLIFE_PROJECT = "C:/path/to/repository")
Sys.setenv(MICCORE_PATH = "C:/path/to/fastMicSim-master")
source("src/main.R")
```
