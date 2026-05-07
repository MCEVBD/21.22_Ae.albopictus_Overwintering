# 2021–22 *Aedes albopictus* Overwintering

## General Information

This repository contains the data and analysis scripts for a field study of overwintering survival of *Aedes albopictus* (Asian tiger mosquito) eggs during the 2021–22 winter season. Eggs were deployed on paper sheets inside scrap tires at six field sites along a north–south latitudinal gradient in Illinois and Wisconsin, USA. The study investigates how microclimate temperature (inside tires, soil, and ambient air), snow depth, and snow cover relate to egg hatch and survival, and tests the predictive ability of a mixed-effects model previously fit on 2018–19 data (Susong, Tucker et al. 2022).

This work builds on the methods and findings of the prior 2018–19 overwintering study:

> Susong KM, Tucker BJ, Conley AK, Bartholomay LC. Snow-Covered Tires Generate Microhabitats That Enhance Overwintering Survival of *Aedes albopictus* (Diptera: Culicidae) in the Midwest, USA. *Environmental Entomology*. 2022;51(3):586–594. doi:[10.1093/ee/nvac023](https://doi.org/10.1093/ee/nvac023). PMID: [35552675](https://pubmed.ncbi.nlm.nih.gov/35552675/).

**Author:** Katie M. Susong
**Last modified:** May 2026

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

> **Note:** Raw HOBO temperature logger CSVs (`00_Data/01_RAW_temperature/`) and raw field snow-depth records (`00_Data/01_RAW_Snowdepth/`) are excluded from the repository via `.gitignore`. The processed and formatted versions of these data are included as the CSV files listed above. Raw SNODAS NetCDF rasters were processed on an external drive and are not included; the extracted site-level values are deposited as `21.22_SNODAS.csv`.

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

---

## Column-Level Metadata for `21.22_temperature.csv`

Hourly sensor readings across all sites. 64,414 rows, 23 columns. `NA` values occur where a sensor type was not deployed for a given tire orientation (see notes below).

| Column | Data Type | Description | Units / Coding | Missing Values |
|--------|-----------|-------------|----------------|----------------|
| *(first column, unnamed)* | Integer | R-exported row index. | Sequential integer. | No missing values. |
| `location` | Categorical | Study site abbreviation. | `Arl`, `Bon`, `Car`, `Dek`, `Han`, `Spo`. | No missing values. |
| `number` | Integer | Numeric site–tire identifier. | `1`–`13`. | No missing values. |
| `ABC` | Categorical | Tire orientation. | `E` = east-facing; `W` = west-facing; `S` = south-facing (Arlington T3 only). | No missing values. |
| `DateTime` | DateTime | Date and hour of the observation. | `YYYY-MM-DD HH:MM:SS`, hourly intervals. | No missing values. |
| `Air_Temp` | Numeric | Ambient air temperature from the nearest weather station. | Degrees Celsius (°C). | `NA` when station data was unavailable for that hour. |
| `Tire` | Numeric | Internal tire temperature from the primary in-tire probe. | °C. | `NA` for west-facing (W) tires, which use `Tire_b` instead. |
| `Tire_b` | Numeric | Internal tire temperature from the tire-base probe. | °C. | `NA` for east-facing (E) and south-facing (S) tires, which use `Tire` instead. |
| `Soil` | Numeric | Soil temperature from a probe buried near the tire base. | °C. | `NA` at sites or orientations where no soil probe was deployed. |
| `RH` | Numeric | Ambient relative humidity from the weather station. | Percent (%). | `NA` when station data was unavailable. |
| `Tire_RH` | Numeric | Relative humidity inside the tire. | Percent (%). | `NA` for west-facing tires and where in-tire humidity was not recorded. |
| `Solar` | Numeric | Solar radiation from the weather station. | W/m². | `NA` when station data was unavailable. |
| `pcpn` | Numeric | Hourly precipitation from the weather station. | mm. | `NA` when station data was unavailable. |
| `dwpt` | Numeric | Dew point temperature from the weather station. | °C. | `NA` when station data was unavailable. |
| `wdir` | Numeric | Wind direction from the weather station. | Degrees (0–360). | `NA` when station data was unavailable. |
| `wspd` | Numeric | Mean wind speed from the weather station. | m/s. | `NA` when station data was unavailable. |
| `wspd_max` | Numeric | Maximum wind gust speed. | m/s. | `NA` when station data was unavailable. |
| `wstdv` | Numeric | Standard deviation of wind speed. | m/s. | `NA` when station data was unavailable. |
| `Diff` | Numeric | Tire–air temperature difference (`Tire` − `Air_Temp`). | °C. | `NA` when `Tire` is `NA`. |
| `Diff.S` | Numeric | Soil–air temperature difference (`Soil` − `Air_Temp`). | °C. | `NA` when `Soil` is `NA`. |
| `Diff.ST` | Numeric | Soil–tire temperature difference (`Soil` − `Tire`). | °C. | `NA` when either `Soil` or `Tire` is `NA`. |
| `Diff.RH` | Numeric | Tire–ambient humidity difference (`Tire_RH` − `RH`). | Percentage points. | `NA` when either humidity value is `NA`. |
| `Diff.b` | Numeric | Tire-base–air temperature difference (`Tire_b` − `Air_Temp`). | °C. | `NA` when `Tire_b` is `NA`. |

**Sensor placement note:** East-facing (E) and south-facing (S) tires recorded `Tire` (primary in-tire probe); west-facing (W) tires recorded `Tire_b` (tire-base probe). Each column is `NA` for the orientation it does not serve. The `Soil` probe was deployed at a subset of sites.

---

## Column-Level Metadata for `21.22_snowdepth.csv`

Daily field-measured snow depth at each study site. 1,162 rows, 13 columns.

| Column | Data Type | Description | Units / Coding | Missing Values |
|--------|-----------|-------------|----------------|----------------|
| `date` | Date | Date of the snow measurement. | `M/D/YY` format (e.g., `10/6/21`). | No missing values. |
| `location` | Categorical | Study site abbreviation. | `Arl`, `Bon`, `Car`, `Dek`, `Han`, `Spo`. | No missing values. |
| `CAMERA` | Categorical | Whether a trail camera was operational at the site on that date. | `T` = camera active; blank = camera not active or not deployed. | Blank when camera was not active. |
| `depth_G` | Numeric | Ground-level snow depth measured at the site. | mm. | Blank on days when no measurement was taken. |
| `depth_Etire` | Numeric | Snow depth at the east-facing tire. | mm. | Blank on days when no measurement was taken. |
| `depth_Wtire` | Numeric | Snow depth at the west-facing tire. | mm. | Blank on days when no measurement was taken. |
| `top_Etire` | Categorical | Whether snow was present on the top of the east-facing tire. | `y` = yes; `n` = no. | Blank or `NA` on days when no observation was made. |
| `side_Etire` | Categorical | Whether snow was present on the side of the east-facing tire. | `y` = yes; `n` = no. | Blank or `NA` on days when no observation was made. |
| `top_Wtire` | Categorical | Whether snow was present on the top of the west-facing tire. | `y` = yes; `n` = no. | Blank on days when no observation was made. |
| `side_Wtire` | Categorical | Whether snow was present on the side of the west-facing tire. | `y` = yes; `n` = no. | Blank on days when no observation was made. |
| `depth_Stire` | Numeric | Snow depth at the south-facing tire (Arlington only). | mm. | Blank for all non-Arlington sites (no south-facing tire). |
| `top_Stire` | Categorical | Whether snow was present on the top of the south-facing tire. | `y` = yes; `n` = no. | Blank for all non-Arlington sites. |
| `side_Stire` | Categorical | Whether snow was present on the side of the south-facing tire. | `y` = yes; `n` = no. | Blank for all non-Arlington sites. |

**Note on blank cells:** Blank cells in depth columns indicate that a measurement was not taken on that date (e.g., no site visit). Blank cells in the south-facing tire columns (`depth_Stire`, `top_Stire`, `side_Stire`) for non-Arlington sites indicate that no south-facing tire was present at those locations.

---

## Column-Level Metadata for `21.22_snow_temperature.csv`

Daily aggregated temperature and snow data for each site–tire combination. 2,506 rows, 25 columns.

| Column | Data Type | Description | Units / Coding | Missing Values |
|--------|-----------|-------------|----------------|----------------|
| *(first column, unnamed)* | Integer | R-exported row index. | Sequential integer. | No missing values. |
| `number` | Integer | Numeric site–tire identifier. | `1`–`13`. | No missing values. |
| `Date` | Date | Date of the observation. | `YYYY-MM-DD`. | No missing values. |
| `MeanT_Air` | Numeric | Daily mean ambient air temperature. | °C. | `NA` when station data was unavailable. |
| `MeanT_Tire` | Numeric | Daily mean in-tire temperature. | °C. | `NA` when tire sensor data was unavailable. |
| `MeanT_Soil` | Numeric | Daily mean soil temperature. | °C. | `NA` at sites or orientations without a soil probe. |
| `MinT_Air` | Numeric | Daily minimum ambient air temperature. | °C. | `NA` when station data was unavailable. |
| `MinT_Tire` | Numeric | Daily minimum in-tire temperature. | °C. | `NA` when tire sensor data was unavailable. |
| `MinT_Soil` | Numeric | Daily minimum soil temperature. | °C. | `NA` at sites or orientations without a soil probe. |
| `MaxT_Air` | Numeric | Daily maximum ambient air temperature. | °C. | `NA` when station data was unavailable. |
| `MaxT_Tire` | Numeric | Daily maximum in-tire temperature. | °C. | `NA` when tire sensor data was unavailable. |
| `MaxT_Soil` | Numeric | Daily maximum soil temperature. | °C. | `NA` at sites or orientations without a soil probe. |
| `MeanT_Diff` | Numeric | Daily mean tire–air temperature difference. | °C. | `NA` when either temperature is `NA`. |
| `MinT_Diff` | Numeric | Daily minimum tire–air temperature difference. | °C. | `NA` when either temperature is `NA`. |
| `MaxT_Diff` | Numeric | Daily maximum tire–air temperature difference. | °C. | `NA` when either temperature is `NA`. |
| `location` | Categorical | Study site abbreviation. | `Arl`, `Bon`, `Car`, `Dek`, `Han`, `Spo`. | No missing values. |
| `depth_G` | Numeric | Ground-level field-measured snow depth. | mm. | Blank on days when no measurement was taken. |
| `snodas` | Numeric | SNODAS-modeled snow depth at the site. | mm. | No missing values. |
| `depth_T` | Numeric | Field-measured snow depth at the tire. | mm. | Blank on days when no measurement was taken. |
| `cover_T` | Categorical | Whether snow cover was observed on the tire. | `y` = yes; `n` = no. | Blank on days when no observation was made. |
| `cover_S` | Categorical | Whether snow cover was observed on the side of the tire. | `y` = yes; `n` = no. | Blank on days when no observation was made. |
| `x` | Numeric (double) | Longitude of the study site. | Decimal degrees, WGS 84 (EPSG:4326). Negative = west. | No missing values. |
| `y` | Numeric (double) | Latitude of the study site. | Decimal degrees, WGS 84 (EPSG:4326). Positive = north. | No missing values. |
| `ABC` | Categorical | Tire orientation. | `E` = east; `W` = west; `S` = south. | No missing values. |
| `cover` | Categorical | Combined snow-cover category for the tire. | `none` = no snow on tire; `top` = snow on top only; `side` = snow on side only; `both` = snow on top and side. | Blank on days when no observation was made. |

---

## Column-Level Metadata for `21.22_1990-2010_JANavg_normal.csv`

PRISM 1990–2010 January mean temperature normals. 6 rows, 1 column. One value per study site, ordered from southernmost to northernmost.

| Column | Data Type | Description | Units / Coding | Missing Values |
|--------|-----------|-------------|----------------|----------------|
| `PRISM_tmea` | Numeric | Long-term (1990–2010) PRISM January mean air temperature normal for each study site. | °C. | No missing values. |

---

### Notes on Missing Values

**In `21.22_section_survival_data.csv`:**
- **`NA`** appears in the `site`, `sheet_id`, `x`, and `y` columns for CTR (laboratory control) rows. The control eggs were maintained under laboratory conditions and were not deployed at a field site, so site name and geographic coordinates are not applicable.
- **Empty string** (`""`) appears in the `tire_num` column for CTR rows, as no tire was used in the laboratory control.
- **`Inf`** (positive infinity) appears in the `rel.per.sur.live` and `per.red.live` columns when the survival formula involves division by zero (i.e., when field survival is zero).
- **`0`** in hatch count columns (`live.hatch1`–`live.hatch4`, `dead.hatch1`–`dead.hatch4`) indicates that no live or dead larvae were observed in that hatch attempt. All hatch attempts were conducted; zeros represent the observed outcome, not missing data.

**In `21.22_temperature.csv`:**
- **`NA`** indicates a sensor was not deployed for that tire orientation or that weather-station data was unavailable for that hour. East-facing and south-facing tires use the `Tire` column (with `Tire_b` as `NA`); west-facing tires use the `Tire_b` column (with `Tire` as `NA`).

**In `21.22_snowdepth.csv`:**
- **Blank cells** in depth and snow-flag columns indicate that no field measurement or observation was taken on that date. South-facing tire columns (`depth_Stire`, `top_Stire`, `side_Stire`) are blank for all non-Arlington sites because only Arlington had a south-facing tire.

**In `21.22_snow_temperature.csv`:**
- **`NA`** in temperature columns indicates that sensor data was unavailable (e.g., no soil probe at that site). **Blank cells** in snow-depth and cover columns indicate that no field observation was taken on that date.

---

*This README was generated with the assistance of AI. All information has been reviewed and the author confirms its accuracy.*
