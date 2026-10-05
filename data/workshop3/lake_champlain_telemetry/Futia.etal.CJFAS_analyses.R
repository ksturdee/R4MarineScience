#### ---
# Project: Multi-species interactions with artificial causeways in a fragmented lake
# Date: 2025-06-18
# Description: Data wrangling for evaluating species distributions and interactions with causeways in Lake Champlain. 
#              Uses `filtered_region_dets` and `tracked_fish` objects from "Futia.etal.CJFAS_wrangling.R"
# Produced by M. H. Futia
#### ---


### Load packages----
library(here)
library(qs)
library(glatos)
library(sf)
library(magrittr)
library(mgcv)
library(gratia)
library(tidyverse)


### Generate functions----
# negate based on values in list
`%!in%` <- Negate(`%in%`)

# updated detection_events function to include
detection_events <- function (det, location_col, time_sep = Inf,
                              condense = TRUE)
{
  detections <- data.table::as.data.table(det)
  if (is.character(time_sep)) {
    time_sep <- as.numeric(time_sep)
    if (all(is.na(time_sep)))
      stop("`time_sep` argument should be numeric.")
  }
  if (!is.logical(condense)) {
    stop("input argument 'condense' must be either TRUE or FALSE (unquoted).")
  }
  missingCols <- setdiff(c("animal_id", "detection_timestamp_utc",
                           "deploy_lat", "deploy_long"), names(detections))
  if (length(missingCols) > 0) {
    stop(paste0("Detections dataframe is missing the following ",
                "column(s):\n", paste0("       '", missingCols,
                                       "'", collapse = "\n")), call. = FALSE)
  }
  if (!("POSIXct" %in% class(detections$detection_timestamp_utc))) {
    stop(paste0("Column 'detection_timestamp_utc' in the detections dataframe",
                "must be of class 'POSIXct'."), call. = FALSE)
  }
  animal_id <- detection_timestamp_utc <- time_diff <- arrive <- depart <- event <- deploy_lat <- deploy_long <- NULL
  data.table::setnames(detections, location_col, "location_col")
  data.table::setkey(detections, animal_id, detection_timestamp_utc)
  detections[, `:=`(time_diff, c(NA, diff(as.numeric(detection_timestamp_utc)))),
             by = c("animal_id", "location_col")]
  detections[, `:=`(arrive, as.numeric((location_col != data.table::shift(location_col,
                                                                          fill = TRUE)) | (time_diff > time_sep))), by = animal_id]
  detections[, `:=`(depart, as.numeric((location_col != data.table::shift(location_col,
                                                                          fill = TRUE, type = "lead")) |
                                         (data.table::shift(time_diff,
                                                            fill = TRUE, type = "lead") > time_sep))), by = animal_id]
  detections[, `:=`(event, cumsum(arrive))]
  Results <- detections[, .(animal_id = animal_id[1],
                            location = location_col[1],
                            mean_latitude = mean(deploy_lat, na.rm = T),
                            mean_longitude = mean(deploy_long, na.rm = T),
                            first_detection = detection_timestamp_utc[1],
                            last_detection = detection_timestamp_utc[.N],
                            num_detections = .N,
                            # added output to identify the first receiver station within a region where a fish was detected
                            first_station = station_name[1], 
                            # added output to identify the last receiver station within a region where a fish was detected
                            last_station = station_name[.N],
                            res_time_sec = diff(range(as.numeric(detection_timestamp_utc)))),
                        by = event]
  out_class <- function(xout, xin) {
    if (inherits(xin, "data.table")) {
      return(xout)
    }
    if (inherits(xin, "tbl")) {
      return(dplyr::as_tibble(xout))
    }
    return(as.data.frame(xout))
  }
  if (condense) {
    message(paste0("The event filter distilled ", nrow(detections),
                   " detections down to ", nrow(Results), " distinct detection events."))
    return(out_class(Results, det))
  }
  else {
    message(paste0("The event filter identified ", max(detections$event,
                                                       na.rm = TRUE), " distinct events in ", nrow(detections),
                   " detections."))
    data.table::setnames(detections, "location_col", location_col)
    return(out_class(detections, det))
  }
}


### Load data----
# receiver deployments
recs_sum <- qread(
  here("data","receiver_deployments_Futiaetal2025.qs"))

# lake regions and major Vermont tributaries shapefile
lc_reg_trib <- st_read(dsn = here("shapefiles"),
                       layer = "Lake&Tribs")


### Evaluate latitudinal range by season and year for each species/maturity----
# calculate seasonal latitudinal range
lat_range <- filtered_region_dets |> 
  group_by(animal_id, species_mat, det_year, det_ssn, det_per) |> 
  reframe(n_pos = n_distinct(detection_timestamp_utc),
          min_lat = min(deploy_lat, na.rm = T),
          max_lat = max(deploy_lat, na.rm = T),
          dist_straight = geosphere::distHaversine(c(max_lat, mean(deploy_long, na.rm = T)),
                                                   c(min_lat, mean(deploy_long, na.rm = T)))) |> 
  left_join(tracked_fish) |> 
  mutate(species_mat = factor(species_mat,
                              levels = c("LKS_juv", "LKS_adult", "LKT_juv", "LKT_adult", "WAL")),
         season_lvl = as.numeric(det_ssn),
         year_re = factor(det_year),
         animal_id_re = factor(animal_id))


# plot histogram of latitudinal range
hist(lat_range$dist_straight)

# compare distribution families to data
fitdistrplus::descdist(lat_range$dist_straight)

# GAMM by species_mat
lat_gamm <- bam(dist_straight ~ species_mat + s(season_lvl, k = 4, bs = "cc", by = species_mat) +
                  s(animal_id_re, bs  = "re") +
                  s(year_re, bs = "re") +
                  s(det_per, bs = "re"),
                data = lat_range,
                family = tw,
                method = "fREML")

# check model performance 
appraise(lat_gamm)

# evaluate main effects and effect of smoothers (linear or non-linear)
anova.gam(lat_gamm)

draw(lat_gamm)

summary(lat_gamm)

# GAMM overall pairwise spp evaluations
gamm_emm_lat <- emmeans::emmeans(lat_gamm, specs = c("species_mat"))
gamm_p_lat <- emmeans::pwpm(gamm_emm_lat)

gamm_p_lat_df <- matrix(gamm_p_lat,
                        ncol = 5,
                        dimnames = list(rownames(gamm_p_lat),rownames(gamm_p_lat))) |>
  data.frame()

# GAMM pairwise evaluations of spp*season interactions
lat_dif <- difference_smooths(lat_gamm, select = "s(season_lvl)", group_means = T)

lat_dif_df <- lat_dif |> 
  filter(season_lvl %in% c(1,2,3,4)) |> 
  mutate(sig = if_else(.lower_ci < 0 & .upper_ci < 0 | .lower_ci > 0 & .upper_ci > 0, "*", ""))

# visualize pairwise comparisons
lat_pw <- draw(lat_dif) &
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") &
  theme_classic()

lat_pw

# visualize latitudinal range by species and maturity
lat_plot <- lat_range %>% 
  group_by(animal_id, species_mat, det_ssn) %>% 
  mutate(dist_m = dist_straight/1000) %>% 
  group_by(species_mat, det_ssn) %>% 
  mutate(n_fish = n_distinct(animal_id)) %>% 
  ungroup() %>% 
  ggplot(aes(x = species_mat, y = dist_m)) + # dist_m or ave_dist
  geom_point(shape = 21, color = "gray30", fill = "gray60", alpha = 0.5, size = 0.9, position = position_dodge2(width = 0.65)) +
  geom_boxplot(fill = "transparent", outliers = F) +
  geom_text(aes(label = n_fish), y = 31) +
  scale_y_continuous(limits = c(0,31), breaks = seq(0,30,5)) +
  lemon::facet_rep_wrap(~ det_ssn) +
  theme_classic()

lat_plot


### Lake-wide distributions by species and year----
# summarize detections by station
station_dets <- filtered_region_dets |>
  group_by(species_mat) |> 
  mutate(n_fish = n_distinct(animal_id),
         species_mat = factor(species_mat, levels = c("LKS_juv", "LKS_adult", "LKT_juv", "LKT_adult", "WAL"))) |> 
  group_by(species_mat, station_name) |> 
  reframe(n_dets = n_distinct(detection_timestamp_utc),
          n_dets_cor = n_dets/n_fish,
          site_n_fish = n_distinct(animal_id),
          site_prop_fish = site_n_fish/n_fish,
          deploy_lat = mean(deploy_lat),
          deploy_lon = mean(deploy_long)) |> 
  unique() |> 
  group_by(species_mat) |> 
  mutate(dets_100 = scales::rescale(n_dets_cor, to = c(0.1, 100))) |> 
  ungroup()

# plot species distributions to evaluate overall lake-wide distributions by species & maturity
spp_bubbleplot <- station_dets |> 
  ggplot() +
  geom_sf(data = lc_reg_trib, fill = "gray90") +
  geom_point(aes(x = deploy_lon, y = deploy_lat, 
                 fill = site_prop_fish,
                 size = dets_100),
             shape = 21) +
  lemon::facet_rep_wrap(~species_mat, nrow = 1) +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(-73.5, -73.1, 0.2)) +
  coord_sf(xlim = c(-73.5, -73.085), ylim = c(44, 45.1)) +
  theme_classic()

spp_bubbleplot

# number & percent of sturgeon detected in each tributary by capture location
LKS_trib <- filtered_region_dets |> 
  filter(species_mat == "LKS_adult" & 
           station_name %in% c("WinooskiRiverUpper", "WinooskiUpper", "WinooskiRiverLower2","WinooskiRiverLower1","WinooskiLower",
                               "OtterCreekUpper", "OtterCreekLower", "LamoilleUpper","LamoilleLower")) |> 
  mutate(trib = case_when(station_name %in% c("WinooskiRiverUpper","WinooskiUpper","WinooskiRiverLower2","WinooskiRiverLower1","WinooskiLower") ~ "Win",
                          station_name %in% c("OtterCreekUpper", "OtterCreekLower") ~ "Otter",
                          station_name %in% c("LamoilleUpper","LamoilleLower") ~ "Lam")) |> 
  select(animal_id, capture_location, trib) |>
  unique() |> 
  mutate(n_fish = n_distinct(animal_id)) |> 
  group_by(trib, capture_location) |> 
  reframe(n_sturg = n_distinct(animal_id),
          per_sturg = n_sturg*100/n_fish) |> 
  unique() 


### Evaluate frequency of causeway crossings----
# create new column for general lake regions
filtered_region_dets <- filtered_region_dets |> 
  mutate(region_short = case_when(regions %in% c("Main_North","Main_Central","Main_South","Winooski River","Otter Creek","South_Lake") ~ "Main_Lake",
                                  regions %in% c("Malletts","Lamoille River") ~ "Malletts_Bay",
                                  TRUE ~ regions))

# create detection events by species
det_events_wal <- filtered_region_dets |> 
  filter(species %in% "WAL") |>  
  detection_events(det = _, location_col = "region_short") |> 
  mutate(species = "WAL")

det_events_lks <- filtered_region_dets |> 
  filter(species %in% "LKS") |> 
  detection_events(det = _, location_col = "region_short") |> 
  mutate(species = "LKS")

det_events_lkt <- filtered_region_dets |> 
  filter(species %in% "LKT") |> 
  detection_events(det = _, location_col = "region_short") |> 
  mutate(species = "LKT")

# merge detection events and add reference data and region assignment
det_events <- bind_rows(det_events_wal,det_events_lks,det_events_lkt) |> 
  rename(regions = location) |> 
  left_join(tracked_fish)

# identify crossings between regions
cross_events <- det_events |> 
  group_by(animal_id, species) |> 
  arrange(first_detection) |> 
  mutate(arrive_basin = regions,
         previous_basin = lag(regions),
         previous_station = lag(last_station),
         previous_depart = lag(last_detection),
         species_mat = factor(species_mat,
                              levels = c("LKS_juv", "LKS_adult", "LKT_juv", "LKT_adult", "WAL"))) |> 
  ungroup() |> 
  filter(arrive_basin != previous_basin)

# count number of distinct region crossings
cross_events_summary <- cross_events |> 
  group_by(previous_basin, arrive_basin) |> 
  reframe(n_cross = n())

# reduce crossings within a single day that bounce between regions (e.g., fish positioned between fill E and fill W)
cross_events_cut <- cross_events |> 
  mutate(date_detect = date(first_detection)) |> 
  group_by(animal_id, species_mat, species, date_detect) |> 
  arrange(first_detection) |> 
  # count number of unique regions occupied within a day
  mutate(n_region = n_distinct(unlist(select(pick(everything()), arrive_basin, previous_basin))), 
         # identify crossings that start and end in the same region
         no_cross = if_else(n_region > 1 & first(previous_basin) == last(arrive_basin), 1, 0)) |> 
  filter(no_cross %in% 0) |> 
  mutate(trim_cross = if_else(n_region == 2, 1, 0)) |>
  # reduce multiple crossings across a causeway that end in a new location to a single (last) event
  filter(trim_cross %in% 0 | first_detection %in% max(first_detection)) |> 
  ungroup()

# correct walleye (animal_id 55285) movement through the Gut due to missing receiver at time of crossing
wal_update <- read.csv(file = here("data","corrected_walleye_crossing_Futiaetal2025.csv")) |> 
  mutate(animal_id = factor(animal_id),
         sex = "F",
         first_detection = as.POSIXct(first_detection, format = "%m/%d/%Y %H:%M"),
         last_detection = as.POSIXct(last_detection, format = "%m/%d/%Y %H:%M"),
         previous_depart = as.POSIXct(previous_depart, format = "%m/%d/%Y %H:%M"),
         date_detect = as.Date(date_detect, format = "%m/%d/%Y"))

# replace animal_id 55285 crossings
cross_events_cor <- cross_events_cut |> 
  filter(animal_id != "55285") |> 
  bind_rows(wal_update)

# add column for causeway names
cross_events_cor <- cross_events_cor |> 
  mutate(cause_name = case_when(animal_id == "4311575638" & event == "243" ~ "Unknown", # identify unknown crossing event
                                animal_id == "4313087350" & event == "353" ~ "Unknown", # identify unknown crossing event
                                previous_basin %in% c("Gut","Main_Lake") & arrive_basin %in% c("Main_Lake","Gut") ~ "W Gut",
                                previous_basin %in% c("Gut","Northeast_Arm") & arrive_basin %in% c("Gut","Northeast_Arm") ~ "E Gut",
                                previous_basin %in% c("Main_Lake","Malletts_Bay") & arrive_basin %in% c("Main_Lake","Malletts_Bay") ~ "Island Line",
                                previous_basin %in% c("Malletts_Bay","Northeast_Arm") & arrive_basin %in% c("Malletts_Bay","Northeast_Arm") ~ "Sandbar",
                                previous_basin %in% c("Main_Lake","Northeast_Arm") & arrive_basin %in% c("Main_Lake","Northeast_Arm") ~ "Carry Bay"))

# calculate number of days between previous depart and first detection
cross_events_cor <- cross_events_cor |> 
  mutate(cross_gap = difftime(first_detection, previous_depart, units = c("days")))

# set cross date to date last observed at causeway receiver, or average date if no causeway receiver
cross_events_cor <- cross_events_cor |> 
  rowwise() |> 
  mutate(cross_date = case_when(previous_station %in% c("Carry Bay","Island Line East","Gut",
                                                        "Sand Bar Malletts","Sand Bar Inland Sea", "Fill East") ~ previous_depart,
                                first_station %in% c("Carry Bay","Island Line East","Gut",
                                                     "Sand Bar Malletts","Sand Bar Inland Sea", "Fill East") ~ first_detection,
                                TRUE ~ mean.Date(as.Date(c(previous_depart, first_detection), format = "%Y-%m-%d %H:%M:%S")))) |> 
  ungroup()

# add cross season
cross_events_cor <- cross_events_cor |> 
  mutate(dayn = yday(cross_date),
         cross_year = year(cross_date),
         # correct year for winter season (the start of winter is associted with the following year due to winter overlapping two years)
         cross_ssnYr = if_else(dayn >= 335, year(cross_date) + 1, year(cross_date)),
         cross_ssn = case_when(dayn < 91 | dayn >= 335 ~ "winter", # Dec 1 - Apr 30
                               dayn >= 91 & dayn < 182 ~ "spring", # May 1 - June 30
                               dayn >= 182 & dayn < 274 ~ "summer", # July 1 - Sept 30
                               dayn >= 274 & dayn < 335 ~ "fall"), # Oct 1 - Nov 30
         cross_ssn = factor(cross_ssn,
                            levels = c("winter", "spring", "summer", "fall")))

# total number of crossings
nrow(cross_events_cor)

# total number of fish that crossed by species*mat
cross_events_cor |> 
  group_by(species_mat) |> 
  reframe(n_cross = n_distinct(animal_id))

# count number of crossings by causeway and species mat
cross_events_cor |> 
  mutate(total_cross = n()) |> 
  group_by(cause_name, species_mat) |> 
  reframe(n_cross = n_distinct(event),
          n_fish = n_distinct(animal_id)) |> 
  unique()

# number of crossings by spp group and season
cross_events_cor |> 
  group_by(species) |> 
  mutate(n_cross = n()) |> 
  ungroup() |> 
  group_by(species, cross_ssn) |> 
  reframe(n_ssn_cross = n(),
          per_ssn_cross = n_ssn_cross*100/n_cross) |> 
  unique() |> 
  arrange(species, -per_ssn_cross)

# count number of crossings by spp group and sex
tracked_fish |>
  group_by(species_mat, sex) |> 
  reframe(total_fish = n_distinct(animal_id)) |> 
  right_join(cross_events_cor |> 
              group_by(species_mat, sex) |> 
              reframe(n_fish_cross = n_distinct(animal_id),
                      n_cross = n_distinct(event))) |> 
  mutate(per_fish_cross = n_fish_cross*100/total_fish,
         ave_cross = n_cross/n_fish_cross)

# plot number of causeway crossings by day of year
cycle_plot <- cross_events_cor %>% 
  filter(!is.na(species)) %>% 
  group_by(species) %>%
  ggplot() +
  geom_histogram(aes(x = dayn, fill = cross_ssn),
                 color = "black") +
  coord_polar() +
  facet_wrap(~ species, scales = "free") +
  scale_x_continuous(limits = c(0,365), breaks = seq(0,350, 50)) +
  scale_fill_manual(values = c("#053061", "#92c5de", "#b2182b", "#f4a582")) +
  labs(x = "Day of year", y = "Crossing frequency") +
  theme_bw() +
  theme(legend.position = "none")

cycle_plot


### Fill in seasons with no crossings for each fish----
# arrange data as number of crossings per individual by season and causeway
cross_sum <- cross_events_cor |> 
  mutate(ssnyr = paste(cross_ssn, cross_ssnYr, sep = "_")) |> 
  group_by(animal_id, species_mat, species, ssnyr, cause_name) |> 
  reframe(n_cross = n_distinct(event))

# add fish with no causeway crossings
cross_sum_full <- tracked_fish[,c("animal_id","species_mat", "species")] |> 
  full_join(cross_sum[,c("animal_id", "species", "species_mat", "ssnyr", "cause_name", "n_cross")]) 

# identify all seasons fish tags were active
fish_seasons <- filtered_region_dets |> 
  group_by(animal_id, species, species_mat) |> 
  # identify detection window as start date (first day after 2-week removal period) and end date (last detection)
  reframe(start_date = unique(date(utc_release_date_time) + 14),
          end_date = max(detection_timestamp_utc)) |> 
  mutate(start_day = yday(start_date),
         end_day = yday(end_date),
         first_year = year(start_date),
         last_year = year(end_date),
         first_season = case_when(start_day < 91 | start_day >= 335 ~ "winter", # Dec 1 - March 31
                                  start_day >= 91 & start_day < 182 ~ "spring", # April 1 - June 30
                                  start_day >= 182 & start_day < 274 ~ "summer", # July 1 - Sept 30
                                  start_day >= 274 & start_day < 335 ~ "fall"), # Oct 1- Nov 30
         last_season = case_when(end_day < 91 | end_day >= 335 ~ "winter", # Dec 1 - Apr 30
                                 end_day >= 91 & end_day < 182 ~ "spring", # May 1 - June 30
                                 end_day >= 182 & end_day < 274 ~ "summer", # July 1 - Sept 30
                                 end_day >= 274 & end_day < 335 ~ "fall"), # Oct 1- Nov 30
         first_ssnyr = if_else(start_day >= 335, 
                               paste(first_season, first_year+1, sep = "_"),
                               paste(first_season, first_year, sep = "_")),
         last_ssnyr = if_else(end_day >= 335,
                              paste(last_season, first_year+1, sep = "_"),
                              paste(last_season, last_year, sep = "_"))) |> 
  group_by(animal_id) |> 
  # reduce recaptured fish
  filter(start_day == min(start_day)) |>  
  ungroup()

# create vector for all season year combinations
ssnyr <- factor(c("fall_2013", "winter_2014", "spring_2014", "summer_2014",
                  "fall_2014", "winter_2015", "spring_2015", "summer_2015",
                  "fall_2015", "winter_2016", "spring_2016", "summer_2016",
                  "fall_2016", "winter_2017", "spring_2017", "summer_2017",
                  "fall_2017", "winter_2018", "spring_2018", "summer_2018",
                  "fall_2018", "winter_2019", "spring_2019", "summer_2019",
                  "fall_2019", "winter_2020", "spring_2020", "summer_2020",
                  "fall_2020", "winter_2021", "spring_2021", "summer_2021",
                  "fall_2021", "winter_2022", "spring_2022", "summer_2022",
                  "fall_2022", "winter_2023", "spring_2023", "summer_2023", "fall_2023"),
                levels = c("fall_2013", "winter_2014", "spring_2014", "summer_2014",
                           "fall_2014", "winter_2015", "spring_2015", "summer_2015",
                           "fall_2015", "winter_2016", "spring_2016", "summer_2016",
                           "fall_2016", "winter_2017", "spring_2017", "summer_2017",
                           "fall_2017", "winter_2018", "spring_2018", "summer_2018",
                           "fall_2018", "winter_2019", "spring_2019", "summer_2019",
                           "fall_2019", "winter_2020", "spring_2020", "summer_2020",
                           "fall_2020", "winter_2021", "spring_2021", "summer_2021",
                           "fall_2021", "winter_2022", "spring_2022", "summer_2022",
                           "fall_2022", "winter_2023", "spring_2023", "summer_2023", "fall_2023"),
                ordered = T)

# add seasons with no crossings for each fish 
all_det_ssn <- fish_seasons |>
  group_by(animal_id, species) |> 
  mutate(all_ssns = list(ssnyr[ssnyr >= first_ssnyr & ssnyr <= last_ssnyr])) |> 
  ungroup() |> 
  unnest(all_ssns) |> 
  select(animal_id, species, species_mat, all_ssns) |> 
  unique() |> 
  rename(ssnyr = all_ssns)

# overall seasonal crossings
cross_sum_final <- all_det_ssn |> 
  left_join(cross_sum_full) |> 
  group_by(animal_id, species_mat, ssnyr) |> 
  reframe(total_ssn_cross = sum(n_cross, na.rm = T)) |> 
  separate(ssnyr, into = c("cross_ssn", "cross_ssnYr"), sep = "_") |> 
  mutate(species_mat = factor(species_mat, levels = c("LKS_juv","LKS_adult","LKT_juv","LKT_adult","WAL")),
         year_re = factor(cross_ssnYr),
         cross_ssn = factor(cross_ssn, levels = levels(lat_range$det_ssn), ordered = T),
         season_lvl = as.numeric(cross_ssn),
         det_year = as.numeric(cross_ssnYr)) |> 
  rename(det_ssn = cross_ssn) |> 
  left_join(lat_range[,c("animal_id", "species_mat", "det_ssn", "det_year", "det_per")]) |> 
  mutate(animal_id = factor(animal_id),
         det_per = if_else(is.na(det_per), 1, det_per))

# plot crossing frequency by species
spp_cross_plot <- cross_sum_final %>% 
  group_by(animal_id, species_mat) %>% 
  # average crosses across years
  reframe(ave_cross = mean(total_ssn_cross)) %>% 
  group_by(species_mat) %>%
  # count number of fish by species and maturity during each season
  mutate(n_fish = n_distinct(animal_id)) %>%  
  ungroup() %>% 
  ggplot(aes(x = species_mat, y = ave_cross)) +
  geom_point(shape = 21, fill = "gray70", alpha = 0.6, position = position_dodge2(width = 0.65)) +
  geom_violin(fill = "transparent") +
  geom_text(aes(label = n_fish), y = 4) +
  scale_y_continuous(limits = c(-0.1,5), breaks = c(seq(0, 4, 1))) +
  theme_classic()

spp_cross_plot


### Relationship between latitudinal range and crossing frequency----
# combine seasonal latitudinal range with crossings frequency
cross_lat_sum <- cross_sum_final |> 
  select("animal_id", "species_mat", "det_ssn", "det_year", "total_ssn_cross") |> 
  left_join(lat_range[,c("animal_id", "species_mat", "det_ssn", "det_year", "dist_straight")])

# remove seasons when fish were not detected (latitudinal range == NA)
cross_lat_sum <- cross_lat_sum |> 
  filter(!is.na(dist_straight))

# check data normality
shapiro.test(cross_lat_sum$total_ssn_cross)

# run Spearman's correlation
cor.test(x = cross_lat_sum$dist_straight, y = cross_lat_sum$total_ssn_cross,
         method = "spearman", 
         exact = F)

lm(total_ssn_cross~dist_straight, 
   data = cross_lat_sum) |>  
  summary()

# visualize correlation between latitudinal range and crossing frequency
cross_lat_plot <- cross_lat_sum %>% 
  ggplot(aes(x = dist_straight, y = total_ssn_cross)) +
  geom_point(alpha = 0.5, aes(fill = species_mat, shape = species_mat)) +
  geom_smooth(method = "lm", formula = y~x) +
  scale_fill_manual(values = c("#dfc27d","#8c510a","#80cdc1","#01665e","black")) +
  scale_shape_manual(values = 21:25) +
  theme_classic()

cross_lat_plot


### Analyze crossing frequency using mixed models----
# histogram of seasonal crosses
hist(cross_sum_final$total_ssn_cross)

# GAMM
cross_gamm <- bam(total_ssn_cross ~ species_mat + s(season_lvl, k = 4, bs = "cc", by = species_mat) + 
                    s(animal_id, bs  = "re") +
                    s(year_re, bs = "re") +
                    s(det_per, bs = "re"),
                  data = cross_sum_final, 
                  family = tw,
                  method = "fREML")

# check model performance 
appraise(cross_gamm)

# evaluate main effects and effect of smoothers (linear or non-linear)
anova.gam(cross_gamm)

draw(cross_gamm)

summary(cross_gamm)

# GAMM overall pairwise spp evaluations
gamm_emm_cross <- emmeans::emmeans(cross_gamm, specs = c("species_mat"))
gamm_p_cross <- emmeans::pwpm(gamm_emm_cross)

gamm_p_cross_df <- matrix(gamm_p_cross,
                          ncol = 5,
                          dimnames = list(rownames(gamm_p_cross),rownames(gamm_p_cross))) |>
  data.frame()

# average number of crossings by species*maturity and season
cross_sum_final |>
  group_by(species_mat, det_ssn) |> 
  reframe(ave_cross = round(mean(total_ssn_cross),2),
          sd_cross = round(sd(total_ssn_cross),2))

# GAMM pairwise spp evaluations by season
cross_dif <- difference_smooths(cross_gamm, select = "s(season_lvl)", group_means = T)

cross_dif_df <- cross_dif |>
  filter(season_lvl %in% c(1,2,3,4)) |>
  mutate(sig = if_else(.lower_ci < 0 & .upper_ci < 0 | .lower_ci > 0 & .upper_ci > 0, "*", ""))

cross_pw <- draw(cross_dif) &
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") &
  theme_classic()

cross_pw


### Evaluate repeat crossings across years by causeway and fish----
# summarize data by unique year, causeway, and individual
cross_years <- cross_events_cor |> 
  select(animal_id, species_mat, cross_year, cause_name) |>  
  unique()

# remove unknown causeway crossings
cross_years <- filter(cross_years, cause_name %!in% "Unknown")

# add years fish were tagged and detected
detect_years <- filtered_region_dets |>  
  select(animal_id, species_mat, det_year, utc_release_date_time) |>  
  unique() |>
  rename(cross_year = det_year) |>
  mutate(release_year = year(utc_release_date_time)) |>
  filter(animal_id %in% cross_years$animal_id & species_mat %in% cross_years$species_mat)

# fill missing years between detections
detect_years_full <- detect_years |>
  group_by(animal_id, species_mat) |>
  complete(cross_year = min(release_year):max(cross_year)) |>
  ungroup()

# join full years with cross years
cross_all_years <- full_join(cross_years, detect_years_full)

# fill all possible causeway crossing year combos for each fish
cross_years_full <- cross_all_years |>
  mutate(cause_cross = if_else(is.na(cause_name), 0 , 1)) |>
  group_by(animal_id, species_mat) |>
  complete(cross_year, cause_name) |>
  filter(!is.na(cause_name)) |>
  mutate(cause_cross = if_else(is.na(cause_cross), 0, cause_cross),
         first_release = min(release_year, na.rm = T)) |>
  filter(release_year == first_release | is.na(release_year))

# calculate probability of crossing in subsequent years after initial cross
cross_years_prob <- cross_years_full |>
  group_by(animal_id,species_mat,cause_name) |>
  reframe(first_year = case_when(cause_cross == 1 ~ cross_year),
          rem_years = max(cross_year) - first_year,
          total_cross = sum(cause_cross)-1,
          cross_prob = total_cross/rem_years) |>
  group_by(animal_id,species_mat,cause_name) |>
  filter(first_year == min(first_year, na.rm = T)) |>
  ungroup()

# number of fish that crossed prior to their final year
cross_years_prob |>
  filter(rem_years >= 1) |>
  group_by(species_mat) |>
  reframe(n_fish = n_distinct(animal_id))

# probability of re-crossing averaged across all fish
cross_years_prob |>
  filter(rem_years >= 1) |>
  reframe(ave_prop = mean(cross_prob))

# percent of fish that did not cross again
cross_years_prob |>
  filter(rem_years > 0) |>
  mutate(nf = n_distinct(animal_id)) |>
  filter(cross_prob == 0) |>
  reframe(n_fish = n_distinct(animal_id),
          per_fish = n_fish*100/nf) |>
  unique()

# average probability of recrossing based on species
cross_years_prob |>
  filter(rem_years >= 1) |>
  group_by(species_mat) |>
  reframe(n_fish = n_distinct(animal_id),
          ave_prob = mean(cross_prob))

# average probability of recrossing based on species and causeway
cross_years_prob |>
  filter(rem_years >= 1) |>
  group_by(species_mat, cause_name) |>
  reframe(n_fish = n_distinct(animal_id),
          ave_prob = mean(cross_prob))


### Generate crossing abacus plot----
# determine period fish are active (2 weeks post release to last detection)
detect_windows <- filtered_region_dets %>% 
  filter(animal_id %in% cross_events_cor$animal_id) %>% 
  group_by(animal_id, species_mat) %>% 
  reframe(start_date = min(utc_release_date_time + 2*7*24*60*60),
          end_date = max(detection_timestamp_utc)) %>% 
  mutate(start_day = as_date(yday(start_date), origin = lubridate::origin),
         end_day = as_date(yday(end_date)+(year(end_date)-year(start_date))*365)) %>% 
  unique()

# generate unique ids for y axis (creates warnings, can ignore)
fish_ids <- cross_events_cor %>% 
  select(species_mat, animal_id) %>% 
  unique() %>% 
  group_by(species_mat) %>% 
  arrange(animal_id) %>% 
  mutate(n_fish = n_distinct(animal_id),
         fish_id = 1:n_fish,
         spp_mat_id = paste(species_mat, fish_id, sep = "_")) %>% 
  ungroup() %>% 
  # arrange fish based on visual patterns of crossing timing
  mutate(spp_mat_id = factor(spp_mat_id, 
                             levels = c("WAL_1","WAL_2","WAL_3","WAL_5", "WAL_4",
                                        "LKT_adult_3","LKT_adult_2","LKT_adult_5","LKT_adult_9","LKT_adult_20","LKT_adult_16","LKT_adult_14",
                                        "LKT_adult_22","LKT_adult_13","LKT_adult_21","LKT_adult_18","LKT_adult_12","LKT_adult_26","LKT_adult_23",
                                        "LKT_adult_25","LKT_adult_24","LKT_adult_8","LKT_adult_11","LKT_adult_10","LKT_adult_7","LKT_adult_4",
                                        "LKT_adult_1","LKT_adult_6","LKT_adult_19","LKT_adult_17","LKT_adult_15",
                                        "LKT_juv_1","LKT_juv_2","LKT_juv_3",
                                        "LKS_adult_2","LKS_adult_6","LKS_adult_10","LKS_adult_7","LKS_adult_3","LKS_adult_12","LKS_adult_4",
                                        "LKS_adult_14","LKS_adult_5","LKS_adult_8","LKS_adult_13","LKS_adult_9","LKS_adult_11","LKS_adult_1"))) 

# generate standardized dates
cross_dates <- cross_events_cor %>% 
  left_join(detect_windows) %>% 
  mutate(cross_day_n = yday(cross_date),
         post_years = cross_year-year(start_date),
         cross_day_total = cross_day_n + post_years*365,
         cross_date_total = as_date(cross_day_total, origin = lubridate::origin))

# determine directionality relative to smaller region (in = crossing into smaller region, out = crossing out of smaller region)
cross_dates <- cross_dates %>% 
  mutate(direction = case_when(cause_name == "Carry Bay" & arrive_basin == "Northeast_Arm" ~ "in",
                               cause_name == "Carry Bay" & arrive_basin == "Main_Lake" ~ "out",
                               cause_name == "E Gut" & arrive_basin == "Gut" ~ "in",
                               cause_name == "E Gut" & arrive_basin == "Northeast_Arm" ~ "out",
                               cause_name == "W Gut" & arrive_basin == "Gut" ~ "in",
                               cause_name == "W Gut" & arrive_basin == "Main_Lake" ~ "out",
                               cause_name == "Island Line" & arrive_basin == "Malletts_Bay" ~ "in",
                               cause_name == "Island Line" & arrive_basin == "Main_Lake" ~ "out",
                               cause_name == "Sandbar" & arrive_basin == "Malletts_Bay" ~ "in",
                               cause_name == "Sandbar" & arrive_basin == "Northeast_Arm" ~ "out",
                               cause_name == "Unknown" ~ "out"))

# visualize crossing occurrence by individual
cross_abacus <- cross_dates %>% 
  mutate(cause_name = factor(cause_name, levels = c("Island Line", "Carry Bay", "Sandbar", "E Gut", "W Gut", "Unknown"))) %>% 
  left_join(fish_ids) %>% 
  ggplot(aes(x = cross_date_total, y = spp_mat_id)) +
  geom_vline(xintercept = as_date(c(365, 365*2, 365*3, 365*4, 365*5, 365*6, 365*7, 365*8, 365*9)), linetype = "dotted", color = "gray50") +
  geom_segment(aes(y = spp_mat_id, yend = spp_mat_id, x = start_day, xend = end_day), color = "gray50") +
  geom_point(aes(shape = cause_name, fill = direction), alpha = 0.6) + 
  scale_shape_manual(values = c(21:25, 4)) +
  scale_fill_manual(values = c("transparent", "grey20")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b") +
  theme_classic()

cross_abacus


### End of code----