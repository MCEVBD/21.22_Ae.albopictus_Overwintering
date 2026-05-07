# 2021–22 *Aedes albopictus* Overwintering

## General Information

This repository contains the data and analysis scripts for a field study of overwintering survival of *Aedes albopictus* (Asian tiger mosquito) eggs during the 2021–22 winter season. Eggs were deployed on paper sheets inside scrap tires at six field sites along a north–south latitudinal gradient in Illinois and Wisconsin, USA. The study investigates how microclimate temperature (inside tires, soil, and ambient air), snow depth, and snow cover relate to egg hatch and survival, and tests the predictive ability of a mixed-effects model previously fit on 2018–19 data (Susong, Tucker et al. 2022).

**Author:** Katie M. Susong

---

## File Inventory

### Data Files (`00_Data/`)

| File | Description |
|------|-------------|
| `21.22_section_survival_data.csv` | Egg-section-level hatch and survival outcomes for *Ae. albopictus* egg sheets deployed at six field sites and a laboratory control (CTR) during the 2021–22 overwintering season. Each row represents one egg-paper section placed inside a tire. Includes raw hatch counts across four attempts, derived survival metrics, and site coordinates. **Primary data file — see column-level metadata below.** |
| `21.22_temperature.csv` | Hourly sensor readings for all sites: air temperature, in-tire temperature, tire-base probe, soil, relative humidity, solar radiation, precipitation, dew point, wind speed/direction, and computed difference fields. |
| `21.22_temperature_NO.Arl3.csv` | Hourly temperature data for all sites excluding the Arlington tire 3 (staggered-removal) experiment; includes month, date, and accumulated growing degree day (GDD) columns. |
| `21.22_temperature_Arl3.csv` | Hourly temperature data subset for the Arlington tire 3 staggered-removal sections; includes pull-date, sheet identity, and condition columns to track the sequential egg-sheet retrieval experiment. |
| `21.22_snowdepth.csv` | Daily field-measured snow depth by location; includes ground depth, tire-level depth (east, west, south), and categorical snow-on-tire flags (top, side) for each tire orientation. |
| `21.22_SNODAS.csv` | Daily SNODAS-modeled snow depth extracted at each study-site location (October 2021 – April 2022). |
| `21.22_SNODAS 12_jan_NOEDIT.csv` | Archival copy of the SNODAS snow-depth extraction; preserved as a reference snapshot and should not be edited. |
| `21.22_all_snow.csv` | Combined daily snow-depth file merging field-measured depth, SNODAS-modeled depth, and site location metadata (network, city, state, coordinates). |
| `21.22_snow_temperature.csv` | Daily mean, minimum, and maximum air, tire, and soil temperatures merged with SNODAS snow depth, on-site snow depth, tire-level cover categories, and site coordinates. |
| `21.22_studysite_location.csv` | Lookup table of the six study sites with network affiliation, city, state, and geographic coordinates. |
| `21.22_1990-2010_JANavg_normal.csv` | PRISM 1990–2010 January mean temperature normals for each study site. |
| `21.22_hatch_temperature_summary.csv` | Per-section hatch outcomes merged with seasonal temperature summaries (January and DJF means, minima, frost dates, days below −12 °C, freeze–thaw counts, GDD) for all sites including Arlington T3. |
| `21.22_hatch_temperature_summary_NO.Arl3.csv` | Same as above but excluding Arlington tire 3 sections; includes additional median, standard-deviation, and full-season GDD columns. |
| `21.22_bytire.hatch_temperature_summary_NO.Arl3.csv` | Tire-level aggregated survival and thermal summary statistics (excluding Arlington T3); one row per tire location. |
| `21.22_bysite.hatch_temperature_summary_NO.Arl3.csv` | Site-level aggregated survival and thermal summary (excluding Arlington T3); one row per site with a binary survival flag. |
| `21.22_bysite.temperature.survival.csv` | Site-level temperature and survival summary, similar to above but with a slightly reduced column set. |
| `21.22_bysurvial.hatch_temperature_summary_NO.Arl3.csv` | Two-row summary comparing thermal metrics between sites with and without observed live survival (excluding Arlington T3). |
| `21.22_dailyFTevents.csv` | Daily freeze–thaw event counts per site number for tire and air temperatures, with their difference. |
| `allsite_TempSnow_data.csv` | Combined 2018–19 and 2021–22 daily site-level ambient and tire temperatures with SNODAS snow-depth categories, used for cross-season model comparison. |
| `allsite_latlong.csv` | Coordinates (longitude, latitude) for all sites across both the 2018–19 and 2021–22 studies, including numeric site identifiers. |

### Shapefiles (`00_Data/Shapefiles/Study_locations/`)

| File | Description |
|------|-------------|
| `study_location.shp` | ESRI Shapefile containing point geometries for the six study-site locations. |
| `study_location.shx` | Shapefile spatial index. |
| `study_location.dbf` | Attribute table for the shapefile (site names and metadata). |
| `study_location.prj` | Coordinate reference system definition (WGS 84, EPSG:4326). |
| `study_location.qpj` | QGIS projection sidecar (WGS 84, EPSG:4326). |
| `study_location.cpg` | Character encoding declaration (UTF-8). |

> **Note:** Raw temperature logger files (HOBO CSVs) and raw SNODAS snow-depth rasters are excluded from the repository via `.gitignore` and are not included in this deposit.

### R Scripts (`00_Scripts/`)

#### Data Processing and Formatting

| File | Description |
|------|-------------|
| `01_FUNCTIONS.R` | Defines utility functions used across the pipeline: `temp.variable.add` (annotates temperature records with site, tire number, and orientation), `edit.date` (corrects HOBO 24:00:00 timestamp formatting and creates a DateTime column), and `FtoC` (Fahrenheit-to-Celsius conversion). |
| `01_format_temperature_file.R` | Reads raw HOBO temperature logger CSVs (in-tire, tire-base probe, external soil) and hourly weather-station data for all sites, merges them into a single wide-format hourly temperature file, and computes tire–air, soil–air, and tire–soil temperature difference columns. Sources `01_FUNCTIONS.R`. |
| `01.b_arl3_temperature.R` | Constructs piecewise temperature trajectories for the Arlington tire 3 staggered-removal experiment, splicing field tire temperatures with lab control temperatures at each egg-sheet pull date. |
| `02_create_allsnow_file.R` | Merges field-measured daily snow depth, SNODAS-modeled snow depth, and site coordinate data into a single combined snow-depth file (`21.22_all_snow.csv`). |
| `02b_add_temperature_snow.R` | Reshapes snow data from wide to long format (by tire orientation), aggregates hourly temperatures to daily mean/min/max, and merges daily temperature summaries with the snow data. |
| `03_inital_survvival_data_proc.R` | Processes section-level survival data: derives average egg count, total live and dead hatch, proportion live, percent survival, and relative survival versus the lab control; produces exploratory boxplots of survival by site. |

#### Exploratory Analysis

| File | Description |
|------|-------------|
| `04_inital_snow_plots.R` | Generates exploratory plots of snow depth: ground versus SNODAS scatter plots, ground versus tire-level depth, east versus west tire depth, and depth distributions over time. |
| `05_summary_temperature.R` | Computes per-tire thermal summary statistics (mean January and DJF temperatures, minimum temperatures, first frost date, days below −12 °C, longest cold run, freeze–thaw counts, growing degree days via `chillR::GDD`) and merges them with the survival data. |
| `05.b_sumary_temperature_Arl3.R` | Same thermal summary calculations as `05_summary_temperature.R` but applied specifically to the Arlington tire 3 staggered-removal sheets. |
| `06_Temperature_survival_initial.R` | Produces exploratory scatter and box plots of egg survival versus thermal summary variables (mean tire temperature, GDD, minimum temperature, days below −12 °C, etc.). |
| `07_temperature_initial.R` | Investigates temporal and distributional patterns of tire versus air temperature: time-series plots, scatter plots, boxplots of the tire–air difference by site, and GLMs testing the effect of site on tire–air temperature decoupling. |
| `09_Snow_temperaure_inital.R` | Explores the relationship between daily snow depth/cover and tire temperature, with time-series and cross-plots colored by snow cover category to illustrate snow insulation effects. |
| `17_envi_survival_anlysis.R` | Exploratory visualization of wind speed and solar radiation by site and time; produces boxplots by month and time-series of environmental covariates. |

#### Statistical Modeling

| File | Description |
|------|-------------|
| `08_snow_SNODAS_analysis.R` | Validates SNODAS-modeled snow depth against field measurements using Poisson GLMs with latitude, longitude, and their interactions as predictors; reports AIC-based model selection. |
| `10_18.19glm_21.22predict.R` | Fits the 2018–19 mixed-effects model (`lmer`: tire temperature ~ ambient temperature + SNODAS snow category + (1\|site)) and applies it to predict 2021–22 tire temperatures; compares predictions to observations. |
| `11_snow_temperature_analysis.R` | Fits and compares GLMs and mixed-effects models for mean daily tire temperature on 2021–22 data, replicating and extending the 2018–19 model structure with ambient temperature, SNODAS snow bins, site random effects, and interactions; reports AIC-based model comparisons via `AICcmodavg::aictab`. |
| `12_21.22glm_18.19predict.R` | Fits the snow-insulation mixed-effects model on 2021–22 data and uses it to predict 2018–19 tire temperatures; evaluates cross-season predictive performance with residual diagnostics. |
| `13_combine_glm_temperature_snow.R` | Row-binds 2018–19 and 2021–22 datasets, randomly splits into 70% development / 30% holdout, fits the mixed-effects model on the training set, predicts the holdout set, and evaluates fit with residual plots. |
| `14_compare_snow.temperature_glm.R` | Fits three parallel GLMs (2018–19 only, 2021–22 only, combined training set) predicting mean tire temperature from ambient temperature and SNODAS snow bins with interactions; predicts on the shared holdout for cross-model comparison. Sources data from the prior study. |
| `15_survival_analysis.R` | Fits GLMs of percent survival versus growing degree day accumulations and other thermal metrics at the tire and site level (excluding Arlington T3 and lab control); includes overdispersion diagnostics. |
| `15_survival_NB.Rmd` | R Markdown notebook expanding on `15_survival_analysis.R` with Pearson correlations, regressions of survival against January means, cold-exposure hours, minima, and freeze–thaw counts. |
| `16_Arl3_analysis.R` | Analyzes the Arlington tire 3 staggered-removal experiment: computes relative survival by pull date, summarizes cold metrics (days/hours below −12 °C, freeze–thaw) for each retrieval interval, and plots temperature trajectories and survival outcomes. |
| `18_18.19_21.22_survival.R` | Combines 2018–19 and 2021–22 site-level survival summaries, computes relative survival scaled by control rates per study year, and fits mixed-effects models for relative survival versus cold exposure with random effects for year and location. |
| `19_cover_temperature_analysis.R` | Derives SNODAS and on-site snow-depth bins and camera-derived cover categories from the daily snow-temperature data; fits GLMs and mixed-effects models linking tire temperature and tire–air temperature difference to cover type, snow depth, and ambient temperature. |
| `20_soil_tire_analysis.R` | Merges hourly and daily temperature/snow data to explore soil–tire temperature relationships; fits a series of linear models predicting daily mean tire temperature from soil temperature, SNODAS depth, cover, and their interactions. |
| `22_Daily_FreeseThaw_analysis.R` | Counts freeze–thaw run-length events per site for daily mean tire and air temperatures (sequences crossing 0 °C via `rle`); computes air–tire freeze–thaw count differences. |
| `23GLM_maxdailytemperatureanalysi.R` | Fits GLMs for mean, maximum, and minimum daily tire temperature using combined 2018–19 and 2021–22 data; compares on-site versus SNODAS snow classifications with 70/30 train–holdout validation and forest-plot coefficient comparisons. |

#### Publication Figures

| File | Description |
|------|-------------|
| `FIGURE_cover_snodas.R` | Generates publication figures: violin/box plots of tire–air temperature difference by SNODAS snow bin and cover type, bar plots of cover frequency by snow bin, coefficient forest plot from the cover model, and proportional cover-by-depth bars. Sources `19_cover_temperature_analysis.R`. |
| `FIGURE_snodas_model.R` | Produces the main scatter plot of ambient versus internal tire temperature colored by snow-depth bin with model smooths, coefficient forest plots, and effects visualizations from the combined 2018–19 / 2021–22 GLM. Sources `14_compare_snow.temperature_glm.R`. |
| `FIGURE_snodasVonsite.R` | Plots SNODAS-modeled versus on-site field-measured snow-depth time series (faceted by site) and a 1:1 scatter with linear trend for model validation. |
| `FIGURE_survival_corr.R` | Creates four-panel figure of egg survival versus DJF growing degree days, January mean tire temperature, days below −12 °C, and freeze–thaw count at the tire and site level. |
| `FIGURE_timeVsnow.R` | Plots SNODAS snow depth over time (December–February) faceted by site with a highlighted January window; includes ridgeline density variants. |
| `FIGURE_tire_ext_temp.R` | Plots mean air versus tire temperature time series by site and a scatter of tire versus air temperature colored by SNODAS snow depth. |
| `FIGURE_tire_snow_soil.R` | Generates supplementary figure of smoothed daily mean soil, tire, and air temperature trajectories over the winter season, faceted by site. |

### SNODAS Processing Scripts (`00_Scripts/SNODAS_scripts/`)

| File | Description |
|------|-------------|
| `01_getSNODAS.sh` | Downloads masked SNODAS `.tar` archives (October 2021 – April 2022) from the NSIDC FTP server. |
| `02_untarSNODAS.sh` | Untars and gunzips downloaded SNODAS archives, then sorts `.dat` files into variable-specific subdirectories (SNWZ = snow depth, SWEM = snow water equivalent, etc.). |
| `03_procSNODAS.sh` | Converts SNODAS binary `.dat` files to NetCDF format using `gdal_translate`, then applies `ncrename` and `ncatted` to assign proper variable names and units. |
| `04_depthsitesSNODAS.R` | Extracts daily SNODAS snow depth values at each study-site location (from the shapefile) and compiles them into a site-level time series. |
| `generic.hdr` | ENVI header template file required by `gdal_translate` when converting SNODAS masked binary grids to NetCDF. |

### Rendered Outputs (`00_Scripts/`)

| File | Description |
|------|-------------|
| `06_Temperature_survival_initial.html` | Rendered HTML output of `06_Temperature_survival_initial.R`. |
| `15_survival_analysis.html` | Rendered HTML output of `15_survival_analysis.R`. |
| `15_survival_NB.html` | Rendered HTML output of `15_survival_NB.Rmd`. |
| `15_survival_NB.nb.html` | R Notebook HTML output of `15_survival_NB.Rmd` (interactive version). |
| `16_Arl3_analysis.html` | Rendered HTML output of `16_Arl3_analysis.R`. |
| `19_cover_temperature_analysis.html` | Rendered HTML output of `19_cover_temperature_analysis.R`. |

### Other Files

| File | Description |
|------|-------------|
| `21.22_Ae.albopictus_Overwintering.Rproj` | RStudio project configuration file. |
| `cover_model.pdf` | PDF output of the cover-type model coefficient plot generated by `FIGURE_cover_snodas.R`. |
| `.gitignore` | Specifies files excluded from version control (R user data, macOS `.DS_Store` files, raw snow-depth and temperature data directories). |

---

## Column-Level Metadata for `21.22_section_survival_data.csv`

| Column | Data Type | Description | Units / Coding | Missing Values |
|--------|-----------|-------------|----------------|----------------|
| *(first column, unnamed)* | Integer | R-exported row index. | Sequential integer (1–45). | No missing values. |
| `site` | Categorical | Abbreviated name of the study site where the tire was located. | `Bon` = Bonfield, IL; `Car` = Carbondale, IL; `Dek` = DeKalb, IL; `Arl` = Arlington, WI; `Han` = Hancock, WI; `Spo` = Spooner, WI. | `NA` for laboratory control rows (CTR). |
| `sheet_id` | Categorical | Replicate sheet identifier within a tire. Each tire received three sheets (A, B, C) at different positions. | `A`, `B`, or `C`. | No missing values. |
| `sheet` | Categorical | Unique section identifier combining the sheet letter and a numeric code. | Format: `{letter}.{number}` (e.g., `A.13`, `B.02`). | No missing values. |
| `loc.id` | Categorical | Location–tire identifier encoding the state, site, and tire number. | Format: `{state}_{site}_{tire}` (e.g., `IL_Bon_T1`, `WI_Arl_T3`). `CTR` for the laboratory control. | No missing values. |
| `number` | Integer | Numeric site identifier used across the analysis pipeline. | `0` = laboratory control; `1`–`13` = field site–tire combinations. | No missing values. |
| `egg1` | Integer | First count of eggs visible on the section under a dissecting microscope prior to hatching. | Count (dimensionless). | No missing values. |
| `egg1b` | Integer | Second independent count of eggs on the same section (replicate count for quality assurance). | Count (dimensionless). | No missing values. |
| `live.hatch1` | Integer | Number of eggs that hatched and produced live larvae in the first hatch attempt. | Count (dimensionless). | No missing values. |
| `live.hatch2` | Integer | Number of eggs that hatched and produced live larvae in the second hatch attempt. | Count (dimensionless). | `0` = no live larvae observed. |
| `live.hatch3` | Integer | Number of eggs that hatched and produced live larvae in the third hatch attempt. | Count (dimensionless). | `0` = no live larvae observed. |
| `live.hatch4` | Integer | Number of eggs that hatched and produced live larvae in the fourth hatch attempt. | Count (dimensionless). | `0` = no live larvae observed. |
| `dead.hatch1` | Integer | Number of eggs that hatched but produced dead (non-viable) larvae in the first hatch attempt. | Count (dimensionless). | No missing values. |
| `dead.hatch2` | Integer | Number of eggs that hatched but produced dead larvae in the second hatch attempt. | Count (dimensionless). | `0` = no dead larvae observed. |
| `dead.hatch3` | Integer | Number of eggs that hatched but produced dead larvae in the third hatch attempt. | Count (dimensionless). | `0` = no dead larvae observed. |
| `dead.hatch4` | Integer | Number of eggs that hatched but produced dead larvae in the fourth hatch attempt. | Count (dimensionless). | `0` = no dead larvae observed. |
| `x` | Numeric (double) | Longitude of the study site. | Decimal degrees, WGS 84 datum (EPSG:4326). Negative values indicate west of the Prime Meridian. | `NA` for CTR laboratory control rows (no field location). |
| `y` | Numeric (double) | Latitude of the study site. | Decimal degrees, WGS 84 datum (EPSG:4326). Positive values indicate north of the Equator. | `NA` for CTR laboratory control rows (no field location). |
| `tire_num` | Categorical | Tire position identifier at the site. | `T1` = east-facing tire; `T2` = west-facing tire; `T3` = south-facing tire (Arlington only, staggered-removal experiment). | Empty string for CTR laboratory control rows. |
| `ABC` | Categorical | Tire orientation/side label. | `E` = east; `W` = west; `S` = south. | No missing values. |
| `avg.egg` | Numeric (double) | Average of the two egg counts (`egg1` and `egg1b`). | Eggs (dimensionless). | No missing values. |
| `total.live` | Integer | Total live larvae across all four hatch attempts. | Sum of `live.hatch1` through `live.hatch4`. Count (dimensionless). | No missing values. |
| `total.dead` | Integer | Total dead larvae across all four hatch attempts. | Sum of `dead.hatch1` through `dead.hatch4`. Count (dimensionless). | No missing values. |
| `total.hatch` | Integer | Total individuals with any hatch outcome (live or dead). | `total.live` + `total.dead`. Count (dimensionless). | No missing values. |
| `pro.live` | Numeric (double) | Proportion of hatched individuals that were live. | `total.live` / `total.hatch`. Dimensionless, range 0–1. | `0` when `total.live` = 0. |
| `per.sur.live` | Numeric (double) | Percent survival: proportion of eggs that produced live larvae. | `total.live` / `avg.egg`. Dimensionless, range 0–1. | `0` when no live larvae were observed. |
| `rel.per.sur.live` | Numeric (double) | Relative percent survival, scaled by the mean CTR (laboratory control) survival rate. | `per.sur.live` / mean CTR `per.sur.live`. Dimensionless ratio. | `0` when field survival is zero. |
| `per.red.live` | Numeric (double) | Percent reduction in survival relative to the laboratory control. | Derived from the difference between control and field survival rates. Dimensionless. | `Inf` when field survival is zero and the formula produces a division by zero. |

### Notes on Missing Values

- **`NA`** (R-style missing value) appears in the `site`, `sheet_id`, `x`, and `y` columns for CTR (laboratory control) rows. The control eggs were maintained under laboratory conditions and were not deployed at a field site, so site name and geographic coordinates are not applicable.
- **Empty string** (`""`) appears in the `tire_num` column for CTR rows, as no tire was used in the laboratory control.
- **`Inf`** (positive infinity) appears in the `rel.per.sur.live` and `per.red.live` columns when the survival formula involves division by zero (i.e., when field survival is zero).
- **`0`** in hatch count columns (`live.hatch1`–`live.hatch4`, `dead.hatch1`–`dead.hatch4`) indicates that no live or dead larvae were observed in that hatch attempt. All hatch attempts were conducted; zeros represent the observed outcome, not missing data.

---

*This README was generated with the assistance of AI. All information has been reviewed and the author confirms its accuracy.*
