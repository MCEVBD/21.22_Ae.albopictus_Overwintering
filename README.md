# 2021–22 *Aedes albopictus* Overwintering

## General Information

This repository contains the data and analysis scripts for a field study of overwintering survival of *Aedes albopictus* (Asian tiger mosquito) eggs during the 2021–22 winter season. Eggs were deployed on paper sheets inside scrap tires at six field sites along a north–south latitudinal gradient in Illinois and Wisconsin, USA. The study investigates how microclimate temperature (inside tires, soil, and ambient air), snow depth, and snow cover relate to egg hatch and survival, and tests the predictive ability of a mixed-effects model previously fit on 2018–19 data (Susong, Tucker et al. 2022).

**Author:** Katie M. Susong

---

## File Inventory

### Data Files (`00_Data/`)

| File | Description |
|------|-------------|
| `section_survival_data.csv` | Egg-section-level hatch and survival outcomes for *Ae. albopictus* egg sheets deployed at six field sites and a laboratory control (CTR) during the 2021–22 overwintering season. Each row represents one egg-paper section placed inside a tire. |

> **Note:** Raw temperature logger files (HOBO CSVs) and raw SNODAS snow-depth rasters are excluded from the repository via `.gitignore` and are not included in this deposit.

### R Scripts (`00_Scripts/`)

| File | Description |
|------|-------------|
| `01_FUNCTIONS.R` | Defines utility functions used across the pipeline: `temp.variable.add` (annotates temperature records with site, tire number, and orientation), `edit.date` (corrects HOBO 24:00:00 timestamp formatting and creates a DateTime column), and `FtoC` (Fahrenheit-to-Celsius conversion). |
| `01_format_temperature_file.R` | Reads raw HOBO temperature logger CSVs (in-tire, tire-base probe, external soil) and hourly weather station data for all sites, merges them into a single wide-format hourly temperature file, and computes tire–air, soil–air, and tire–soil temperature difference columns. |
| `01.b_arl3_temperature.R` | Constructs piecewise temperature trajectories for the Arlington (Arl) tire 3 staggered-removal experiment, splicing field tire temperatures with lab control (CTR) temperatures at each egg-sheet pull date. |
| `02_create_allsnow_file.R` | Merges field-measured daily snow depth, SNODAS-modeled snow depth, and site coordinate data into a single combined snow-depth file. |
| `02b_add_temperature_snow.R` | Reshapes snow data from wide to long format (by tire orientation), aggregates hourly temperatures to daily mean/min/max, and merges daily temperature summaries with the snow data. |
| `03_inital_survvival_data_proc.R` | Processes section-level survival data: derives average egg count, total live and dead hatch, proportion live, percent survival, and relative survival versus the lab control; produces exploratory boxplots of survival by site. |
| `04_inital_snow_plots.R` | Generates exploratory plots of snow depth: ground versus SNODAS scatter plots, ground versus tire-level depth, east versus west tire depth, and depth distributions over time. |
| `05_summary_temperature.R` | Computes per-tire thermal summary statistics (mean January and DJF temperatures, minimum temperatures, first frost date, days below −12 °C, longest cold run, freeze–thaw counts, growing degree days via `chillR::GDD`) and merges them with the survival data. |
| `05.b_sumary_temperature_Arl3.R` | Same thermal summary calculations as `05_summary_temperature.R` but applied specifically to the Arlington tire 3 staggered-removal sheets. |
| `06_Temperature_survival_initial.R` | Produces exploratory scatter and box plots of egg survival versus thermal summary variables (mean tire temperature, GDD, minimum temperature, days below −12 °C, etc.). |
| `07_temperature_initial.R` | Investigates temporal and distributional patterns of tire versus air temperature: time-series plots, scatter plots, boxplots of the tire–air difference by site, and GLMs testing the effect of site on tire–air temperature decoupling. |
| `08_snow_SNODAS_analysis.R` | Validates SNODAS-modeled snow depth against field measurements using Poisson GLMs with latitude, longitude, and their interactions as predictors; reports AIC-based model selection. |
| `09_Snow_temperaure_inital.R` | Explores the relationship between daily snow depth/cover and tire temperature, with time-series and cross-plots colored by snow cover category to illustrate snow insulation effects. |
| `10_18.19glm_21.22predict.R` | Fits the 2018–19 mixed-effects model (`lmer`: tire temperature ~ ambient temperature + SNODAS snow category + (1\|site)) and applies it to predict 2021–22 tire temperatures; compares predictions to observations. |
| `11_snow_temperature_analysis.R` | Fits and compares mixed-effects models (GLMMs) for mean daily tire temperature on 2021–22 data, replicating and extending the 2018–19 model structure with ambient temperature, SNODAS snow bins, site random effects, and interactions; reports AIC-based model comparisons. |

### SNODAS Processing Scripts (`00_Scripts/SNODAS_scripts/`)

| File | Description |
|------|-------------|
| `01_getSNODAS.sh` | Downloads masked SNODAS `.tar` archives (October 2021 – April 2022) from the NSIDC FTP server. |
| `02_untarSNODAS.sh` | Untars and gunzips downloaded SNODAS archives, then sorts `.dat` files into variable-specific subdirectories (SNWZ = snow depth, SWEM = snow water equivalent, etc.). |
| `03_procSNODAS.sh` | Converts SNODAS binary `.dat` files to NetCDF format using `gdal_translate`, then applies `ncrename` and `ncatted` to assign proper variable names and units. |
| `04_depthsitesSNODAS.R` | Extracts daily SNODAS snow depth values at each study site location (from a shapefile) and compiles them into a site-level time series. |
| `generic.hdr` | ENVI header template file required by `gdal_translate` when converting SNODAS masked binary grids to NetCDF. |

### Other Files

| File | Description |
|------|-------------|
| `21.22_Ae.albopictus_Overwintering.Rproj` | RStudio project configuration file. |
| `.gitignore` | Specifies files excluded from version control (raw snow-depth and temperature data directories). |

---

## Column-Level Metadata for `section_survival_data.csv`

| Column | Data Type | Description | Units / Coding | Missing Values |
|--------|-----------|-------------|----------------|----------------|
| `site` | Categorical | Abbreviated name of the study site where the tire was located. | `CTR` = laboratory control (no field deployment); `Bon` = Bonfield, IL; `Car` = Carbondale, IL; `Dek` = DeKalb, IL; `Arl` = Arlington, WI; `Han` = Hancock, WI; `Spo` = Spooner, WI | No missing values. |
| `sheet` | Categorical | Identifier for the egg paper sheet placed in the tire. Each tire received three sheets (A, B, C) at different positions. | `A`, `B`, or `C` | No missing values. |
| `id` | Categorical | Unique section identifier combining the sheet letter and a numeric code (e.g., `A.11`). Identifies the individual egg-paper section. | Format: `{sheet}.{number}` | No missing values. |
| `loc.id` | Categorical | Location–tire identifier encoding the state, site, and tire number. | Format: `{state}_{site}_{tire}` (e.g., `IL_Bon_T1`, `WI_Arl_T3`). `CTR` for the laboratory control. `T1` = east-facing tire; `T2` = west-facing tire; `T3` = south-facing tire (Arlington only, staggered-removal experiment). | No missing values. |
| `egg1` | Numeric (integer) | First count of eggs visible on the section under a dissecting microscope prior to hatching. | Count (dimensionless) | No missing values. |
| `eeg1b` | Numeric (integer) | Second independent count of eggs on the same section (replicate count for quality assurance). | Count (dimensionless) |`NA` = count could not be obtained (e.g., damaged section). |
| `live.hatch1` | Numeric (integer) | Number of eggs that hatched and produced live larvae in the first hatch attempt. | Count (dimensionless) | No missing values. |
| `live.hatch2` | Numeric (integer) | Number of eggs that hatched and produced live larvae in the second hatch attempt. | Count (dimensionless) | |
| `live.hatch3` | Numeric (integer) | Number of eggs that hatched and produced live larvae in the third hatch attempt. | Count (dimensionless) |  |
| `dead.hatch1` | Numeric (integer) | Number of eggs that hatched but produced dead (non-viable) larvae in the first hatch attempt. | Count (dimensionless) | No missing values. |
| `dead.hatch2` | Numeric (integer) | Number of eggs that hatched but produced dead (non-viable) larvae in the second hatch attempt. | Count (dimensionless) |  |
| `dead.hatch3` | Numeric (integer) | Number of eggs that hatched but produced dead (non-viable) larvae in the third hatch attempt. | Count (dimensionless) |   |
| `x` | Numeric (double) | Longitude of the study site. | Decimal degrees, WGS 84 datum (EPSG:4326). Negative values indicate west of the Prime Meridian. | Blank = no field coordinates (applies to CTR laboratory control rows, which have no field location).|
| `y` | Numeric (double) | Latitude of the study site. | Decimal degrees, WGS 84 datum (EPSG:4326). Positive values indicate north of the Equator. | Blank = no field coordinates (same rows as `x`: CTR laboratory control). |

### Notes on Missing Values

- **Blank cells** (empty strings) indicate that a measurement or observation was not taken or not applicable for that row, as described per-column above.
- **`NA`** (explicit string) appears once in the `eeg1b` column (Hancock, sheet C, id C.15) and indicates that the replicate egg count could not be obtained.
- CTR (laboratory control) rows lack geographic coordinates (`x`, `y`) because eggs were maintained under controlled laboratory conditions and were not deployed at a field site.

*This README was generated with the assistance of AI. All information has been reviewed and the author confirms its accuracy* 
