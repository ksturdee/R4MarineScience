#### ---
# Project: Multi-species interactions with artificial causeways in a fragmented lake
# Date: 2025-06-18
# Description: Data wrangling for evaluating species distributions and interactions with causeways in Lake Champlain
# Produced by M. H. Futia
#### ---

### Load packages----
library(here)
library(qs)
library(glatos)
library(sf)
library(magrittr)
library(tidyverse)


### Generate functions----
# negate based on values in list
`%!in%` <- Negate(`%in%`)


### Load data----
# fish reference data
tagged_fish <- qread(
  here("data","fish_reference_data_Futiaetal2025.qs"))
  
# detection data for each species; *.qs files must be in the same folder as the current R project
lks_detections <- qread(
  here("data","sturgeon_detections_Futiaetal2025.qs")) # lake sturgeon data are only available upon reasonable request
lkt_detections <- qread(
  here("data","trout_detections_Futiaetal2025.qs"))
wal_detections <- qread(
  here("data","walleye_detections_Futiaetal2025.qs"))

# receiver deployments
recs_sum <- qread(
  here("data","receiver_deployments_Futiaetal2025.qs"))

# lake regions and major Vermont tributaries shapefile
lc_reg_trib <- st_read(dsn = here("shapefiles"),
                       layer = "Lake&Tribs")


### Merge detection data----
all_dets <- bind_rows(lks_detections,
                      lkt_detections,
                      wal_detections)

# separate juvenile and adult fish
all_dets <- all_dets |> 
  mutate(species_mat = case_when(species %in% "LKT" & length < .5 ~ "LKT_juv",
                                 species %in% "LKT" & length > .5 ~ "LKT_adult",
                                 species %in% "LKS" & length < 1 ~ "LKS_juv",
                                 species %in% "LKS" & length > 1 ~ "LKS_adult",
                                 species %in% "WAL" ~ "WAL"))


### Remove likely false detections----
true_dets <- false_detections(det = all_dets, 
                              tf = 3600) |> 
  filter(passed_filter %in% 1) # removes 272,186 detections (1.19%)


### Remove detections associated with potentially dead fish or dropped tags----
# load fish removal file for individuals identified as having died or lost tag
fish_rm <- read.csv(here("data","fish_removal_Futiaetal2025.csv")) |> 
  mutate(animal_id = factor(animal_id),
         transmitter_id = as.character(transmitter_id))

# join with detection data
true_dets <- true_dets |> 
  left_join(fish_rm) |> 
  mutate(date_rm = as.POSIXct(date_rm, format = "%m/%d/%Y", tz = "UTC"))

# remove fish based on last date and location observed when assumed dead
live_dets <- true_dets |> 
  mutate(date_rm = if_else(remove == "no", 
                           detection_timestamp_utc+3600, 
                           date_rm)) |>
  filter(detection_timestamp_utc < date_rm)


### Remove duplicated detections identified by time gap less than 50 sec----
live_dets_cut <- live_dets |> 
  group_by(animal_id, species) |> 
  arrange(detection_timestamp_utc)|> 
  mutate(time_diff = detection_timestamp_utc-lag(detection_timestamp_utc)) |> 
  ungroup() |> 
  filter(is.na(time_diff) | time_diff > 50)


### Remove detections during the first two weeks after release----
# identify initial cutoff in seconds
start_cutoff <- 2*7*24*60*60

# filter first detections based on cutoff  
live_dets_filtered <- live_dets_cut |> 
  filter(detection_timestamp_utc > utc_release_date_time + start_cutoff)

# reduce fish reference file to remaining fish after data filtering
tracked_fish <- live_dets_filtered |> 
  select(animal_id, species_mat) |> 
  unique() |> 
  left_join(tagged_fish)


### Assess number of crossings during first two weeks removed
# assign detections to regions
dets_start_rm <- live_dets_cut |> 
  filter(detection_timestamp_utc <= utc_release_date_time + start_cutoff) |>
  left_join(recs_sum) |>
  # standardize station names
  mutate(station_name = case_when(station_name %in% c("Carry Bay", "Carry") ~ "Carry Bay",
                                  station_name %in% c("Whallon Bay", "Whallon") ~ "Whallon Bay",
                                  station_name %in% c("Shelburne Point", "ShelburnePoint") ~ "Shelburne Point",
                                  station_name %in% c("Alburg", "AlburgPass") ~ "Alburg Pass",
                                  station_name %in% c("Shelburne", "ShelburneBay") ~ "Shelburne Bay",
                                  station_name %in% c("Willsboro") ~ "Willsboro Bay",
                                  station_name %in% c("Appletree Shoal", "Appletree Bay") ~ "Appletree Bay",
                                  TRUE ~ station_name)) |>
  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = st_crs(lc_reg_trib)) |>
  # assign detections to regions based on receiver coordinates
  st_join(lc_reg_trib, join = st_nearest_feature) %>%
  mutate(deploy_long = st_coordinates(.)[,1],
         deploy_lat = st_coordinates(.)[,2],
         region_short = case_when(regions %in% c("Winooski River","Main_North","Main_Central","Main_South") ~ "Main",
                                  regions %in% c("Lamoille River", "Malletts") ~ "Malletts",
                                  TRUE ~ regions))

# condense to detection events by region
start_events <- dets_start_rm |>
  detection_events(location_col = "region_short") |>
  left_join(tagged_fish[,c("animal_id", "species_mat")]) |>
  group_by(animal_id) |>
  mutate(regions = n_distinct(location)) |>
  ungroup() |>
  filter(regions > 1)

# identify transitions between regions separated by causeways
start_cross <- start_events |> 
  group_by(animal_id, species_mat) |> 
  arrange(first_detection) |> 
  mutate(arrive_basin = location,
         previous_basin = lag(location)) |> 
  ungroup() |> 
  filter(arrive_basin != previous_basin)


### Count number and proportion of days fish were in the system----
# create season duration table
ssn_dur <- data.frame(det_ssn = c("winter", "spring", "summer", "fall"),
                      ssn_first_day = c(335, 91, 182, 274),
                      ssn_last_day = c(90, 181, 273, 334)) |>
  mutate(ssn_dur = if_else(det_ssn == "winter", 365-ssn_first_day+ssn_last_day+1,
                           ssn_last_day-ssn_first_day+1))

# identify first and last detection for each fish
first_last <- live_dets_filtered |>
  group_by(animal_id, species) |>
  reframe(first_det = min(utc_release_date_time + start_cutoff),
          last_det = max(detection_timestamp_utc),
          first_year = year(first_det),
          last_year = year(last_det))

# add season to first and last detection
first_last <- first_last |>
  mutate(first_day = yday(first_det),
         last_day = yday(last_det),
         first_ssn = case_when(first_day < 91 | first_day >= 335 ~ "winter", # Dec 1 - Mar 31
                               first_day >= 91 & first_day < 182 ~ "spring", # Apr 1 - June 30
                               first_day >= 182 & first_day < 274 ~ "summer", # July 1 - Sept 30
                               first_day >= 274 & first_day < 335 ~ "fall"), # Oct 1 - Nov 30
         last_ssn = case_when(last_day < 91 | last_day >= 335 ~ "winter",
                              last_day >= 91 & last_day < 182 ~ "spring",
                              last_day >= 182 & last_day < 274 ~ "summer",
                              last_day >= 274 & last_day < 335 ~ "fall"))

# add season to each detection
live_dets_filtered <- live_dets_filtered |>
  mutate(det_day = yday(detection_timestamp_utc),
         det_ssn = case_when(det_day < 91 | det_day >= 335 ~ "winter",
                             det_day >= 91 & det_day < 182 ~ "spring",
                             det_day >= 182 & det_day < 274 ~ "summer",
                             det_day >= 274 & det_day < 335 ~ "fall"),
         det_year = if_else(det_day >= 335,
                            year(detection_timestamp_utc)+1,
                            year(detection_timestamp_utc)))

# identify detection duration for first and last seasons
first_last_dur <- first_last |>
  pivot_longer(cols = c(first_det, last_det), names_to = "det_period", values_to = "det_date") |>
  mutate(det_ssn = if_else(det_period == "first_det", first_ssn, last_ssn),
         det_day = yday(det_date),
         det_year = if_else(det_day >= 335,
                            year(det_date)+1,
                            year(det_date))) |>
  left_join(ssn_dur) |>
  group_by(animal_id, species, det_ssn, det_year, ssn_dur, det_date, det_period) |>
  reframe(det_dur = case_when(det_period == "first_det" & det_day >= 335 ~ 365 - det_day + 91 + 1,
                              det_period == "first_det" & det_day < 335 ~ ssn_last_day - det_day + 1,
                              det_period == "last_det" & det_day < 91 ~ det_day + (365-335) + 1,
                              det_period == "last_det" & det_day >= 91 ~ det_day - ssn_first_day + 1),
          det_per = det_dur/ssn_dur)

# merge durations with same season and year for first and last detection
first_last_dur_short <- first_last_dur |>
  group_by(animal_id, species, det_ssn, det_year) |>
  reframe(det_dur = max(det_dur),
          det_per = max(det_per))

# add duration to trimmed data
live_dets_final <- live_dets_filtered |>
  left_join(first_last_dur_short) |>
  left_join(ssn_dur[,c("det_ssn", "ssn_dur")]) |>
  mutate(det_dur = if_else(is.na(det_dur), ssn_dur, det_dur),
         det_per = if_else(is.na(det_per), 1, det_per),
         det_ssn = factor(det_ssn,
                          levels = c("winter", "spring", "summer", "fall"),
                          ordered = T))

# percent of data removed
(nrow(all_dets) - nrow(live_dets_final))/nrow(all_dets)


### Add station names and assign detection to regions----
# add stations and convert to sf object
dets_sf <- live_dets_final |> 
  left_join(recs_sum[,c("glatos_project_receiver","station","station_name")]) |> 
  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = st_crs(lc_reg_trib))

# standardize station names
dets_sf <- dets_sf |> 
  mutate(station_name = case_when(station_name %in% c("Carry Bay", "Carry") ~ "Carry Bay",
                                  station_name %in% c("Whallon Bay", "Whallon") ~ "Whallon Bay",
                                  station_name %in% c("Shelburne Point", "ShelburnePoint") ~ "Shelburne Point",
                                  station_name %in% c("Alburg", "AlburgPass") ~ "Alburg Pass",
                                  station_name %in% c("Shelburne", "ShelburneBay") ~ "Shelburne Bay",
                                  station_name %in% c("Willsboro") ~ "Willsboro Bay",
                                  station_name %in% c("Appletree Shoal", "Appletree Bay") ~ "Appletree Bay",
                                  TRUE ~ station_name))


# join detections with regions
filtered_region_dets <- dets_sf |> 
  st_join(lc_reg_trib, join = st_nearest_feature) |> 
  mutate(deploy_long = st_coordinates(.)[,1],
         deploy_lat = st_coordinates(.)[,2]) |> 
  data.frame() |> 
  mutate(geometry = NULL)


### End of code----