# Data and code from: Multi-species interactions with artificial causeways in a fragmented lake

Dataset DOI: [10.5061/dryad.gb5mkkx2q](10.5061/dryad.gb5mkkx2q)

## Description of the data and file structure

This dataset contains the data and code required to replicate analyses in Futia et al. (in review), evaluating the influence of artificial causeways in Lake Champlain (New York and Vermont) on intra-lake movements of lake sturgeon (*Acipenser fulvescens*), lake trout (*Salvelinus namaycush*), and walleye (*Sander vitreus*). Data include biometric data for fish studied, acoustic telemetry detections for lake trout and walleye, acoustic receiver deployments, a key for tracked fish identified as having died or expelled its transmitter, and corrected data for walleye movements. Lake sturgeon data are not included due to protections as a state endangered species, but these data are available upon reasonable request (contact Lee Simard, Vermont Fish and Wildlife Department, [lee.simard@vermont.gov](mailto:lee.simard@vermont.gov)). Acoustic telemetry data include (1) time-corrected detections at individual receiver stations for fish tagged between 2013 and 2023 and (2) acoustic receiver coordinates and deployment periods. A shapefile including lake basins and major tributaries on the Vermont side of the lake is also included.

Lake Champlain has multiple basins that are separated by islands and human-built causeways that have been hypothesized to limit connectivity between basins and restrict fish movement. This study evaluated the occurrence and frequency of movements through openings in causeways throughout the lake by identifying when fish were detected in different basins. Comparisons were made among species seasonally and based on fish maturity (immature vs mature) when possible. Total crossing frequency was similar among species, but seasonal variation reflected species-specific differences in spawning seasons and temperature preferences. Few fish crossed causeways overall, but those that did often repeated crossing in subsequent years. Thus, causeways may limit fish movement between basins to few individuals but those fish repeatedly cross to access resources, indicating inter-individual diversity in movement and exploration behaviors.

### Files and variables

#### File: receiver_deployments_Futiaetal2025.qs

**Description:** Deployment periods and coordinates for individual acoustic receivers used in the study.

*Variables:*

`glatos_project_receiver` five-letter abbreviation for the GLATOS project that receivers were associated with.

`station` abbreviated identification for individual receiver locations

`station_name` the name associated with a receiver deployment location

`first_dep` a date-time string indicating the date and time when a receiver was first deployed at a specific location

`last_reco` a date-time string indicating the date and time when a receiver was last recovered at a specific location

`lat_x` average latitude for the receiver deployed at a given location throughout all deployments

`lon_x` average longitude for the receiver deployed at a given location throughout all deployments

#### File: trout_detections_Futiaetal2025.qs

**Description:** Transmitter and detection data for acoustically tracked lake trout.

*Variables:*

`animal_id` a unique identifier associated with a tagged fish

`detection_timestamp_utc` a date-time string indicating the date and time that a transmission from a tagged fish was detected at a receiver in UTC time

`transmitter_codespace` the codespace associated with a given transmitter provided by the transmitter manufacturer

`transmitter_id` the unique ID associated with a given transmitter

`sensor_value` environmental data obtained by the transmitter. NA when no data were collected

`sensor_unit` the unit of measurement associated with the sensor value (meters or deg_c = degrees Celsius)

`receiver_sn` the serial number associated with a given acoustic receiver

`deploy_lat` the latitude where an acoustic receiver was deployed at the time of a detection

`deploy_long` the latitude where an acoustic receiver was deployed at the time of a detection

`glatos_project_receiver` five-letter abbreviation for the GLATOS project that receivers were associated with.

`station` abbreviated identification for individual receiver locations

`species` the common name associated with the tagged fish

`utc_release_date_time` the date and time a tagged fish was released

#### File: walleye_detections_Futiaetal2025.qs

**Description:** Transmitter and detection data for acoustically tracked walleye.

*Variables:*

`animal_id` a unique identifier associated with a tagged fish

`detection_timestamp_utc` a date-time string indicating the date and time that a transmission from a tagged fish was detected at a receiver in UTC time

`transmitter_codespace` the codespace associated with a given transmitter provided by the transmitter manufacturer

`transmitter_id` the unique ID associated with a given transmitter

`sensor_value` environmental data obtained by the transmitter. NA when no data were collected

`sensor_unit` the unit of measurement associated with the sensor value (meters or deg_c = degrees Celsius)

`receiver_sn` the serial number associated with a given acoustic receiver

`deploy_lat` the latitude where an acoustic receiver was deployed at the time of a detection

`deploy_long` the latitude where an acoustic receiver was deployed at the time of a detection

`glatos_project_receiver` five-letter abbreviation for the GLATOS project that receivers were associated with.

`station` abbreviated identification for individual receiver locations

`species` the common name associated with the tagged fish

`utc_release_date_time` the date and time a tagged fish was released

#### File: fish_reference_data_Futiaetal2025.qs

**Description:** Transmitter and detection data for all acoustically tagged fish.

*Variables:*

`species` the common name associated with the tagged fish

`animal_id` a unique identifier associated with a tagged fish

`length` fish total length in meters

`start_region` the lake basin where fish were captured and released after tagging

`tag_model` the model of a transmitter; transmitter details can be found at the Innovasea website ([https://www.innovasea.com/fish-tracking/products/acoustic-transmitters/](https://www.innovasea.com/fish-tracking/products/acoustic-transmitters/))

`release_location` the location name where fish were captured and released after tagging

`sex` biological sex of the tagged individual; M = male, F = female, NA = sex was not determined

#### File: fish_removal_Futiaetal2025.csv

**Description:** Reference file to identify fish that were considered to have died or expelled their transmitter and the last date and location such individuals were considered active.

*Variables:*

`animal_id` a unique identifier associated with a tagged fish

`transmitter_id` the unique ID associated with a given transmitter

`species` the common name associated with the tagged fish

`remove` binary for whether an individual fish was considered to have died/expelled its transmitter and detection data should be reduced

`date_rm` the last date an individual was considered active if it was considered to have died or expelled its transmitter

`station_rm` the last location an individual was considered active if it was considered to have died or expelled its transmitter

#### File: corrected_walleye_crossing_Futiaetal2025.csv

**Description:** Reference file with corrected causeway crossing data for walleye with animal ID 55285. This individual had two movements between the Main Lake and Northeast Arm that were not detected at any causeway receivers and likely passed through the Gut considering no receiver was deployed in that location at the time of both movements.

*Variables:*

`event` unique identifier for continuous detection periods within an individual region

`animal_id` a unique identifier associated with a tagged fish

`regions` the lake basin where a fish was detected during the event

`mean_latitude` mean latitude of detections comprising each event

`mean_longitude` mean longitude of detections comprising each event

`first_detection` the time of the first detection in a given event

`last_detection` the time of the last detection in a given event

`num_detections` the total number of detection that comprised a given event

`first_station` the first receiver location where an individual was detected during a given event

`last_station` the last receiver location where an individual was detected during a given event

`res_time_sec` the elapsed time in seconds between the first and last detection in a given event

`species` the common name associated with the tagged fish

`species_mat` identifier for the species and maturity status including a three-letter species abbreviation (LKS = lake sturgeon, LKT = lake trout, WAL = walleye) and maturity (adult, juv = juvenile)

`length` fish total length in meters

`start_region` the lake basin where fish were captured and released after tagging

`tag_model` the model of a transmitter; transmitter details can be found at the Innovasea website ([https://www.innovasea.com/fish-tracking/products/acoustic-transmitters/](https://www.innovasea.com/fish-tracking/products/acoustic-transmitters/))

`release_location` the location name where fish were captured and released after tagging

`sex` biological sex of the tagged individual; M = male, F = female, NA = sex was not determined

`arrive_basin` the lake basin that a fish arrived in at the start of a given event

`previous_basin` the lake basin associated with the previous event for a given fish

`previous_station` the last location a given fish was detected during the previous event

`previous_depart` the time of the last detection associated with the previous event

`date_detect` the date associated with the first detection during a given event

`n_region` the number of unique basins that a fish was detected within for a given day

`no_cross` identifier for crossings that start and end in the same basin for a given day (1 = same start and end basin, 0 = different start and end basin)

`trim_cross` identifier for fish that were detected within exactly two basins for a given day to reduce multiple crossings to a single (last) event for that date and individual (1 = detected within exactly two basins, 0 = detected in more than two basins)

#### File: Lake&Tribs.*

**Description:** files associated with a shapefile Lake Champlain basins and major tributaries on the Vermont side of the lake. File types include .dbf, .prj, .shp, and .shx

*Variables*

`regions` the name of a given lake basin or tributary

`geometry` spatial attributes of a given lake basin or tributary comprising the surround polygon made from coordinates

## Code/software

#### File: Futia.etal.CJFAS_wrangling.R

**Description:** R code for data wrangling to combine, filter, and standardize detection data. Filter includes removing false detections, fish assumed to have died or expelled their transmitter, duplicated detections, and detections during the first two weeks after release. Location information is added to each detection along with the season and lake basin associated with each detection. This file requires the R packages `here`, `qs`, `glatos`, `sf`, `magrittr`, and `tidyverse`.

#### File: Futia.etal.CJFAS_analyses.R

**Description:** R code for data visualization and analyses to evaluate movement patterns. The script uses the `filtered_region_dets` and `tracked_fish` objects produced in the *Futia.etal.CJFAS_wrangling.R* file. The script includes code for the analyses and visuals used to assess and compare causeway crossing frequency in Lake Champlain. R packages required for the script include `here`, `qs`, `glatos`, `sf`, `magrittr`, `mgcv`, `gratia`, and `tidyverse`.

## Access information

Acoustic telemetry data for lake trout and some lake sturgeon detections were associated with the following previous published research:

* Pinheiro et al. 2017: [https://doi.org/10.1016/j.jglr.2016.12.005](https://doi.org/10.1016/j.jglr.2016.12.005)
* Izzo et al. 2022: [https://doi.org/10.1002/tafs.10378](https://doi.org/10.1002/tafs.10378)
* Futia & Marsden 2025: [https://doi.org/10.1111/jfb.70071](https://doi.org/10.1111/jfb.70071)

The lake sturgeon data used in this article cannot be shared publicly due to endangered species protections for this species imposed by the state of Vermont (10 V.S.A. § 5402 and 10 V.S.A. § 4132). These data will be shared upon reasonable request to the Vermont Fish and Wildlife Department ([fwinformation@vermont.gov](mailto:fwinformation@vermont.gov)) for researchers who meet the criteria for access.
