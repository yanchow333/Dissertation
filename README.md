# Dissertation
Ethnic Group, Country of Birth and Neighbourhood Settlement, 2011–2021: 

A reproducible 16 script workflow analysing ethnic group and country of birth across the 2011 and 2021 Censuses of England and Wales

The pipeline covers census harmonisation, separation indices, size-conditional inference,
Shapley decomposition, local spatial clustering, and neighbourhood classification,
distinguishing recent from longer established minority settlement.

The classification is independently validated against year of arrival, which is excluded from its construction.

## Running the analysis

1. Open `dissertation.Rproj` so that the project is set as the working directory.
2. Run `renv::restore()` to install the package versions specified in `renv.lock` (R 4.5.1).
3. Run `Rscript run_all.r`, or execute scripts `00`–`16` in order. Each script sources `00_environemnt_sourve` and reads only outputs saved by earlier scripts.
4. (So scripts 1-16 are sequential)
5. A full run typically takes 30 minutes. Most of the runtime is concentrated in scripts `05`, `06`, and `09`.
