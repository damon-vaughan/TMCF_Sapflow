# Damon's sapflow data processing script. Last update mid September 2023

library(needs)
needs(tidyverse, readxl, lubridate, here, filters, zoo)

# Constants, functions, and directories -----------------------------------

# All functions are saved in a separate script:
source("SF_Functions.R")

# Directory paths
data_import_dir <- here("Sapflow_data_import")
data_raw_dir <- here("Sapflow_data_raw")
data_support_dir <- here("Sapflow_data_supporting")

TreeVec <- c("ET1", "ET2", "ET3", "ET4", "ET5", "ET6", "ET7", "ET8",
             "FB1", "FB2", "FB3", "FB4", "FB5", "FB6", "FB7", "FB8",
             "TV1", "TV2", "TV3", "TV4")

# Import to raw ------------------------------------------------------------
# Format and append the .dat file onto the existing raw csv
# Make sure to check the Log vector for problems
# Make sure to update the import log sheet

import_log <- read_csv(file.path(data_support_dir,
                                 "Sapflow_data_import_log.csv"))
Log <- NULL

# i <- "FB2"
for (i in TreeVec){
  TreeID <- i

  # Bring in the data to be imported
  filename.in.import <- file.path(data_import_dir,
                                  str_c(i, "_Data_Table.dat"))
  import.data <- read_sapflow_dat(filename.in.import)

  # If there is no raw file, start one
  filename.in.raw <- file.path(data_raw_dir, str_c(i, "_Sapflow.csv"))
  if (file.exists(filename.in.raw) == F) {

    import.data2 <- import.data %>%
      format_sapflow_dat()

    write_csv(import.data2, str_c("Sapflow_data_raw/", TreeID, "_Sapflow.csv"))

    cat("Started new raw file for", i, "\n")
    Log <- c(Log, str_c("Started new raw file for", i))

    next
  }

  # How current is the raw data? Find timestamp where last import ended
  raw.data.ends <- import_log %>%
    filter(Tree == i) %>%
    pull(Last_import)

  # How current is the import data?
  import.data.ends <- max(import.data$Timestamp)

  if(import.data.ends > raw.data.ends){

    import.data2 <- import.data %>%
      filter(Timestamp > raw.data.ends) %>%
      format_sapflow_dat()

    # Check for duplicated timestamps or missing data
    duplicated.timestamp <- length(which(duplicated(import.data2$Timestamp) == T))
    missing.data <- round(difftime(min(import.data2$Timestamp), raw.data.ends,
                             units = "days"), 1)

    write_csv(import.data2, filename.in.raw, append = T)

    import_log <- import_log %>%
      mutate(Last_import = case_when(
        Tree == i ~ import.data.ends,
        Tree != i ~ Last_import))

    cat("Importing from:", i, "\n",
        "Duplicates?", duplicated.timestamp, "\n",
        "MissingData?", missing.data, "days", "\n")
    Log <- c(Log, str_c("Importing from:", i,
                      "Duplicates?", duplicated.timestamp,
                      "MissingData?", missing.data, "days"))
  } else {
    cat("No import needed from ", i, "\n")
    Log <- c(Log, str_c("No import needed from ", i))
  }
}

# Update the import logs
write_csv(import_log, file.path(data_support_dir,
                                "Sapflow_data_import_log.csv"))

# Raw to L1 ------------------------------------------------------------
# Convert temperature per thermocouple to velocity per Position

# Parameters that control which raw data values are considered erratic.
NA.temp.min <- 5
NA.temp.max <- 30
NA.delta.min <- 0.05
NA.delta.max <- 3

# Sapflow box wiring guide and other info
Station.info <- read_excel(file.path(data_support_dir,
                                     "Sapflow_station_info.xlsx"))

# i <- "FB2"
for (i in TreeVec){
  Station.info2 <- Station.info %>%
    filter(Tree == i)

  data.starts <- as.POSIXct(Station.info2$Good.data.starts[1], tz = "UTC")

  # Read in raw file
  d <- read_csv(file.path(data_raw_dir, str_c(i, "_Sapflow.csv")),
                show_col_types = F) %>%
    filter(Time >= data.starts) %>%
    rename(Tree = TreeID, Timestamp = Time)
  data.ends <- max(d$Timestamp)

  # Parse base temp and after pulse values, convert to NA where necesary
  Base_Temp <- parse_sfd_columns(d, "Wood_Temperature", "Base_Temp") %>%
    mutate(across(
      where(is.numeric), ~NAify_numeros_locos(., NA.temp.min, NA.temp.max)))

  After_Pulse <- parse_sfd_columns(d, "After_Heat_Pulse_Temperature",
                                   "After_Pulse") %>%
    mutate(across(
      where(is.numeric), ~NAify_numeros_locos(., NA.temp.min, NA.temp.max)))

  # Subtract the before values from the after values
  HRM_Deltas_Pre <- After_Pulse[,-(65:66)] - Base_Temp[,-(65:66)]

  # Bind the time column back on, and remove erratic values
  HRM_Deltas <- HRM_Deltas_Pre %>%
    bind_cols("Timestamp" = Base_Temp$Timestamp) %>%
    mutate(type = "HRM_Delta") %>%
    mutate(across(
      .cols = where(is.numeric),
      .fns = ~NAify_small_deltas(.x, NA.delta.min, NA.delta.max)
    ))

  # Create vector with a number for each cable
  cable_vector <- make_port_vector_by_cable()

  # For each cable, make a dataframe of the ratios
  ratios <- lapply(cable_vector, make_ratio_df)

  # Convert ratios to sap flux velocity using formula from David
  velocities <- suppressWarnings(lapply(ratios, apply_velocity_function))

  # Bind all cables into one dataframe and clean it up a bit
  All.Cables <- bind_rows(velocities) %>%
    filter(is.na(Timestamp) != T) %>%
    mutate(Tree = i) %>%
    select(Tree, everything())

  # Make implicitly missing values explicit
  Timestamp_vector = seq(from = data.starts, to = data.ends, by = "15 min")
  full.Timestamps <- expand_grid(cable_vector, Timestamp_vector) %>%
    rename(Cable = cable_vector, Timestamp = Timestamp_vector) %>%
    mutate(Tree = i) %>%
    select(Tree, everything())

  All.Cables2 <- full.Timestamps %>%
    left_join(All.Cables, by = c("Tree", "Cable", "Timestamp"))

  out <- str_c("Sapflow_data_L1/", i, "_Sapflow_L1.csv")
  write_csv(All.Cables2, out)
  cat("L1 data created for:", i, "\n")
}

# L1 to L2  --------------------------
# Remove bad data windows, add sensor move indicator, and remove outliers

# SF_actions needed to identify when sensors were moved
SF_actions <- read_csv(file.path("Sapflow_data_supporting",
                                 "Sapflow_maintenance_actions.csv")) %>%
  mutate(Action = ifelse(Action == "M" | Action == "R" | Action == "MR",
                         "AddOne", Action)) %>%
  filter(is.na(Action) == F)

SF_actions2 <- SF_actions %>%
  group_by(Tree, Cable) %>%
  mutate(letter = seq(1, n())) %>%
  mutate(letter = case_when(
    letter == "1" ~ "b",
    letter == "2" ~ "c",
    letter == "3" ~ "d"
  )) %>%
  ungroup()

# Bad data sheet identifies windows of bad data; needs extensive processing here
bad.data <- read_excel(file.path("Sapflow_data_supporting",
                                 "Sapflow_bad_data_windows.xlsx")) %>%
  mutate(Start = as.POSIXct(Start, format = c("%Y-%m-%d %H:%M"), tz = "UTC"),
         End = as.POSIXct(End, format = c("%Y-%m-%d %H:%M"), tz = "UTC")) %>%
  select(-Note, -Notes)

sfd.bad <-  bad.data %>%
  pivot_longer(4:27, names_to = "Cable_Location", values_to = "Affects") %>%
  filter(is.na(Affects) == F) %>%
  select(-Affects) %>%
  separate(Cable_Location, c("Cable", "Location"), sep = "_")

# Change factors to facilitate merge with velocity data, and nest
sfd.bad2 <- sfd.bad %>%
  mutate(Location = case_when(
    Location == "Outer" ~ "outer_vel",
    Location == "Middle" ~ "middle_vel",
    Location == "Inner" ~ "inner_vel")) %>%
  mutate(Cable = str_remove(Cable, "Cable"),
         Cable = as.numeric(Cable)) %>%
  group_by(Tree, Cable, Location) %>%
  nest()

# Expand and unnest the nested bad.data sheet so that each bad timestamp is identified in one long dataframe
sfd.bad.expanded <- sfd.bad2 %>%
  mutate(bad.data = map(data, expand_bad_data)) %>%
  select(Tree, Cable, Location, bad.data) %>%
  unnest(bad.data) %>%
  distinct()

# i <- "ET8"
hampel.flags <- NULL
for(i in TreeVec){
  d <-  read_csv(str_c("Sapflow_data_L1/", i, "_Sapflow_L1.csv"),
                 show_col_types = F)

  # Pivot the data and join the bad data flags
  d2 <- d %>%
    pivot_longer(4:6, names_to = "Location", values_to = "Velocity") %>%
    left_join(sfd.bad.expanded,
              by = c("Tree", "Cable", "Location", "Timestamp")) %>%
    replace_na(list(bad.data = 0))

  # Convert bad data to NA, then pivot back to original form
  d3 <- d2 %>%
    mutate(Velocity = ifelse(bad.data == 1, NA, Velocity)) %>%
    select(-bad.data) %>%
    pivot_wider(names_from = Location, values_from = Velocity)

  # Join on the maintenance actions
  d4 <- d3 %>%
    left_join(SF_actions2, by = c("Tree", "Cable", "Timestamp"))

  # Split by cable, apply letters, and re-combine
  d4.split <- d4 %>%
    split(~Cable)

  d4.split2 <- lapply(d4.split, apply_letter)

  d5 <- d4.split2 %>%
    bind_rows() %>%
    arrange(Tree, Cable, Timestamp) %>%
    unite("Cable", Cable, letter, sep = "") %>%
    select(-Action)

  # Split again by cable. This time cable has a letter included so more groups
  d5.split <- d5 %>%
    split(~Cable)

  # Use the hampel filter from the filters package...much faster than pracna
  d5.split2 <- lapply(d5.split, apply_hampel_to_SF2)

  d6 <- d5.split2 %>%
    bind_rows()

  # Split off the flag columns to a separate df
  to.bind <- d6 %>%
    select(-outer_vel, -middle_vel, -inner_vel) %>%
    filter(is.na(outer_flag) == F | is.na(middle_flag) == F |
             is.na(inner_flag) == F)
  if(nrow(to.bind) != 0){
    hampel.flags <- bind_rows(hampel.flags, to.bind)
  }

  d7 <- d6 %>%
    select(-outer_flag, -middle_flag, -inner_flag)

  write_csv(d7, str_c("Sapflow_data_L2/", i, "_Sapflow_L2.csv"))
  cat("Created L2 data for:", i, "\n")
}
write_csv(hampel.flags, file.path("Sapflow_data_supporting", "Hampel_flags.csv"))

# L2 to L3 ---------------------------------------------------------------------
# Baseline

# SF_actions needed to identify when sensors were moved
SF_actions <- read_csv(file.path("Sapflow_data_supporting",
                                 "Sapflow_maintenance_actions.csv")) %>%
  mutate(Action = ifelse(Action == "M" | Action == "R" | Action == "MR",
                         "AddOne", Action)) %>%
  filter(is.na(Action) == F)

# Drop known baseline period issues here
Zeroes <- read_csv(file.path("Sapflow_data_supporting", "Zeroes_full.csv"),
                   show_col_types = F) %>%
  filter(Tree != "FB2" |
           (periodBegin != ymd_hms("2022-10-31 18:00:00", tz = "UTC") &
           periodBegin != ymd_hms("2022-10-30 18:00:00", tz = "UTC")))

TreeVec <- FullTreeVec
TreeVec <- "FB2"
baseline.flags <- NULL
for(i in TreeVec){

  d <- read_csv(str_c("Sapflow_data_L2/", i, "_Sapflow_L2.csv"),
                show_col_types = FALSE)

  # Bring in the zeroes df created in the Microclimate project
  Zeroes.sub <- Zeroes %>%
    filter(Tree == i) %>%
    select(-Tree) %>%
    filter(periodBegin >= min(d$Timestamp)) %>%
    rowid_to_column("Group")

  # Make expanded zeroes df
  expanded_zeroes <- expand_zeroes(Zeroes.sub)

  # Bring in a nested df of the values to use
  valsToUse <- find_baselining_values(d, expanded_zeroes, Zeroes.sub)

  # Prep the actual data to be joined onto the valsToUse data. Add Grouping column and carry numbers forward
  d2 <- d %>%
    left_join(Zeroes.sub, by = c("Timestamp" = "periodBegin")) %>%
    select(-periodEnd) %>%
    mutate(Group = ifelse(Timestamp == min(Timestamp), 0, Group),
           NewGroup = ifelse(is.na(Group) == F, "yes", NA)) %>%
    mutate(Group = na.locf(Group))

  # Adjust group upward after any sensor changes. This is so zeroing values from the previous installation aren't used on the new one. This only applies to the group immediately following the change- later groups are unaffected
  d3 <- d2 %>%
    separate(Cable, into = c("CableNum", "Letter"), sep = 1, remove = F) %>%
    mutate(CableNum = as.numeric(CableNum)) %>%
    left_join(SF_actions,
              by = c("Tree", "CableNum" = "Cable", "Timestamp")) %>%
    # Assigning "End" here stops the fill-forward at the end of the group
    mutate(Action = ifelse(is.na(NewGroup) == F, "End", Action)) %>%
    # Fill-forward the NAs
    mutate(Action = na.locf(Action)) %>%
    # Add 1 to the group numbers where appropriate
    mutate(Group = ifelse(Action == "AddOne" & Group < max(d2$Group),
                          Group + 1, Group)) %>%
    select(-NewGroup, -Action, -CableNum, -Letter)

  # d3.nst <- d3 %>%
  #   group_by(Cable, Group) %>%
  #   nest() %>%
  #   left_join(valsToUse.nst, by = c("Cable", "Group"))

  d4 <- d3 %>%
    left_join(valsToUse, by = c("Tree", "Cable", "Group"))

  # Create and save a separate df to track baselining issues
  to.bind <- d4 %>%
    select(Tree, Cable, Timestamp, baseline_inner, baseline_middle,
           baseline_outer) %>%
    mutate(baseline_inner = ifelse(is.na(baseline_inner) == T, "no_bl_val", NA),
           baseline_middle = ifelse(is.na(baseline_middle) == T, "no_bl_val", NA),
           baseline_outer = ifelse(is.na(baseline_outer) == T,
                                   "no_bl_val", NA)) %>%
    filter(is.na(baseline_inner) == F | is.na(baseline_middle) == F |
             is.na(baseline_outer) == F)
  if(nrow(to.bind) != 0){
    baseline.flags <- bind_rows(baseline.flags, to.bind)
  }

  d5 <- d4 %>%
    replace_na(list(baseline_outer = 0, baseline_middle = 0,
                    baseline_inner = 0)) %>%
    mutate(outer_vel = outer_vel - baseline_outer,
           middle_vel = middle_vel - baseline_middle,
           inner_vel = inner_vel - baseline_inner) %>%
    select(-Group, -periodBegin, -baseline_inner,
           -baseline_middle, -baseline_outer)

  # Apply the function to the nested dataframe
  # baselined <- d3.nst %>%
  #   mutate(baselined = map2(data, valsToUse, baseline_sfd)) %>%
  #   select(Cable, Group, baselined) %>%
  #   unnest(baselined) %>%
  #   ungroup() %>%
  #   select(-Group)

  write_csv(d5, here("Sapflow_data_L3",
                            str_c(i, "_Sapflow_L3.csv")))
  cat("L3 data saved for", i, "\n")
}

write_csv(baseline.flags, file.path("Sapflow_data_supporting",
                                    str_c("Baseline_flags.csv")))
