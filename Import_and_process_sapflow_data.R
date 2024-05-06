# Damon's sapflow data processing script. Last update December 2023

# devtools::install_github("stulacy/filters")
library(needs)
needs(tidyverse, readxl, lubridate, filters, zoo)

options(readr.show_col_types = FALSE)

# Constants, functions, and directories -----------------------------------

# All functions are saved in a separate script:
source("SF_Functions.R")

# Directory paths
data.import.dir <- file.path("Sapflow_data_import")
data.raw.dir <- file.path("Sapflow_data_raw")
data.support.dir <- file.path("Sapflow_data_supporting")

# Import to raw ------------------------------------------------------------
# Appends onto existing raw csv in the write_csv() step, to avoid loading the raw data (time consuming bc of 250+ columns)

# tree.vec <- full.tree.vec
# tree.vec <- "FB6"

filenames.import <- list.files(data.import.dir, full.names = T)
trees.import <- str_sub(filenames.import, start = 21, end = 23)

# i <- filenames.import[1]
for (i in filenames.import){
  # For use in one of the functions
  tree.ID <- str_sub(i, start = 21, end = 23)

  import.log <- read_csv(file.path(data.support.dir,
                               "Sapflow_data_import_log.csv"),
                         show_col_types = F)

  import.data <- read_sapflow_dat(i)

  # If there is no raw file, start one
  filename.in.raw <- file.path(data.raw.dir, str_c(tree.ID, "_Sapflow.csv"))
  if (!file.exists(filename.in.raw)) {

    import.data2 <- import.data %>%
      format_sapflow_dat()

    write_csv(import.data2, str_c("Sapflow_data_raw/", tree.ID, "_Sapflow.csv"))

    cat("Started new raw file for", tree.ID, "\n")

    next
  }

  # How current is the raw data? Find timestamp where last import ended
  raw.data.ends <- import.log %>%
    filter(Tree == tree.ID) %>%
    pull(Last.import)

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

    # use append = T to avoid loading the huge raw files
    write_csv(import.data2, filename.in.raw, append = T)

    import.log <- import.log %>%
      mutate(Last.import = case_when(
        Tree == tree.ID ~ import.data.ends,
        Tree != tree.ID ~ Last.import))

    write_csv(import.log, file.path(data.support.dir,
                                    "Sapflow_data_import_log.csv"))

    cat("Imported from:", tree.ID, "\n",
        "Duplicates?", duplicated.timestamp, "\n",
        "MissingData?", missing.data, "days", "\n")
  } else {
    cat("No import needed from ", tree.ID, "\n")
  }
}

# Raw to L1 ------------------------------------------------------------
# Appends to L1 not in the write_csv() step but by binding to L1 data a few steps before. This way, processing is only done on the new data.
# Convert temperature per thermocouple to velocity per Position

# Parameters that control which raw data values are considered erratic.
NA.temp.min <- 5
NA.temp.max <- 30
NA.delta.min <- 0.05
NA.delta.max <- 3

# Sapflow box wiring guide and other info
station.info <- read_excel(file.path(data.support.dir,
                                     "Sapflow_station_info.xlsx"))

# choose full.tree.vec for all or trees.import for just the most recent changes
# tree.vec <- full.tree.import
tree.vec <- trees.import
# tree.vec <- "FB6"

# i <- "TV4"
for (i in tree.vec){
  station.info2 <- station.info %>%
    filter(Tree == i)

  # Read in L1 file, to be appended to
  L1 <- read_csv(file.path("Sapflow_data_L1", str_c(i, "_Sapflow_L1.csv")))
  old.data.ends <- max(L1$Timestamp, na.rm = T)

  # Read in raw file
  raw <- read_csv(file.path(data.raw.dir, str_c(i, "_Sapflow.csv")),
                show_col_types = F)
  new.data.ends <- max(raw$Timestamp, na.rm = T)

  if(new.data.ends > old.data.ends){
    d <- raw %>%
      filter(Timestamp > old.data.ends)
  } else {
    cat("No import needed from: ", i, "\n")
    next
  }

  # Parse base temp and after pulse values, convert to NA where necesary
  base.temp <- parse_sfd_columns(d, "Wood_Temperature", "Base.temp") %>%
    mutate(across(
      where(is.numeric), ~NAify_numeros_locos(., NA.temp.min, NA.temp.max)))

  after.pulse <- parse_sfd_columns(d, "After_Heat_Pulse_Temperature",
                                   "After.pulse") %>%
    mutate(across(
      where(is.numeric), ~NAify_numeros_locos(., NA.temp.min, NA.temp.max)))

  # Subtract the before values from the after values
  HRM.deltas.pre <- after.pulse[,-(65:66)] - base.temp[,-(65:66)]

  # Bind the time column back on, and remove erratic values
  HRM.deltas <- HRM.deltas.pre %>%
    bind_cols("Timestamp" = base.temp$Timestamp) %>%
    mutate(type = "HRM.delta") %>%
    mutate(across(
      .cols = where(is.numeric),
      .fns = ~NAify_small_deltas(.x, NA.delta.min, NA.delta.max)
    ))

  # Create vector with a number for each cable
  cable.vector <- make_port_vector_by_cable()

  # For each cable, make a dataframe of the ratios
  ratios <- lapply(cable.vector, make_ratio_df)

  # Convert ratios to sap flux velocity using formula from David
  velocities <- suppressWarnings(lapply(ratios, apply_velocity_function))

  # Bind all cables into one dataframe and clean it up a bit
  all.cables <- bind_rows(velocities) %>%
    filter(is.na(Timestamp) != T) %>%
    mutate(Tree = i) %>%
    select(Tree, everything())

  # Make implicitly missing values explicit
  timestamp.vector = seq(from = old.data.ends + minutes(15),
                         to = new.data.ends,
                         by = "15 min")
  full.timestamps <- expand_grid(cable.vector, timestamp.vector) %>%
    rename(Cable = cable.vector, Timestamp = timestamp.vector) %>%
    mutate(Tree = i) %>%
    select(Tree, everything())

  all.cables2 <- full.timestamps %>%
    left_join(all.cables, by = c("Tree", "Cable", "Timestamp")) %>%
    bind_rows(L1) %>%
    distinct() %>%
    arrange(Tree, Cable, Timestamp)

  out <- str_c("Sapflow_data_L1/", i, "_Sapflow_L1.csv")
  write_csv(all.cables2, out)
  cat("L1 data appended for:", i, "\n")
}

# L1 to L2  --------------------------
# Re-creates everytime, does not append
# Remove bad data windows, add sensor move indicator, and remove outliers

# SF.actions needed to identify when sensors were moved
SF.actions <- read_csv(file.path("Sapflow_data_supporting",
                                 "Sapflow_maintenance_actions.csv")) %>%
  mutate(Action = ifelse(Action == "M" | Action == "R" | Action == "MR",
                         "AddOne", Action)) %>%
  filter(is.na(Action) == F)

SF.actions2 <- SF.actions %>%
  group_by(Tree, Cable) %>%
  mutate(letter = seq(1, n())) %>%
  mutate(letter = case_when(
    letter == "1" ~ "b",
    letter == "2" ~ "c",
    letter == "3" ~ "d"
  )) %>%
  ungroup()

# Bad data sheet identifies windows of bad data; needs extensive processing here
# My original sheet
# bad.data <- read_excel(file.path(data.support.dir,
#                                  "Sapflow_bad_data_windows.xlsx")) %>%
#   mutate(Start = as.POSIXct(Start, format = c("%Y-%m-%d %H:%M"), tz = "UTC"),
#          End = as.POSIXct(End, format = c("%Y-%m-%d %H:%M"), tz = "UTC")) %>%
#   select(-Note, -Notes)

# Jose's sheet
bad.data <- read_excel(file.path(data.support.dir, "Sapflow_bad_data_windows_Jose.xlsx"),
                       sheet = "ET") %>%
  bind_rows(read_excel(file.path(data.support.dir, "Sapflow_bad_data_windows_Jose.xlsx"),
                       sheet = "FB")) %>%
  bind_rows(read_excel(file.path(data.support.dir, "Sapflow_bad_data_windows_Jose.xlsx"),
                       sheet = "TV")) %>%
  mutate(Start = as.POSIXct(Start, format = c("%Y-%m-%d %H:%M"), tz = "UTC"),
         End = as.POSIXct(End, format = c("%Y-%m-%d %H:%M"), tz = "UTC")) %>%
  select(-Notes)

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

# choose full.tree.vec for all or trees.import for just the most recent changes
# tree.vec <- full.tree.import
tree.vec <- trees.import
# tree.vec <- "TV1"
# i <- "TV1"

hampel.flags <- NULL

for(i in tree.vec){
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
    left_join(SF.actions2, by = c("Tree", "Cable", "Timestamp"))

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
write_csv(hampel.flags,
          file.path("Sapflow_data_supporting", "Hampel_flags.csv"))

# L2 to L3 ---------------------------------------------------------------------
# Baseline
# Convert 500 LWS to Wetness
# 1.54 * exp(0.0058 * 500)

## Calculate zeroes --------------------------------------------------------

library(needs)
needs(tidyverse, readxl, lubridate)

# Thresholds where water is not moving in the tree:
# LWS.threshold <- 25 #leaf should be dry
# Solar.threshold <- 0 #it should be dark out
# VPD.threshold <- 0.025 #low atmospheric demand for water
# VPD.thresh.ET1 <- 0.05

MC.dir <- "C:/Users/vaug8/OneDrive - University of Kentucky/TMCF/Continuous_data/TMCF_Microclimate"

# full.tree.vec <- "TV1"
# i <- "TV4"
for(i in full.tree.vec){

  filename.in.L3 <- file.path(MC.dir, "Microclimate_data_L3",
                              str_c(i, "MC", "L3.csv", sep = "_"))

  if(!file.exists(filename.in.L3)){
    next
  }

  full.df <- read_csv(filename.in.L3,
                     show_col_types = F) %>%
    select(Tree, Station, Timestamp, Solar, VPD, Wetness, RH) %>%
    filter(Station != "S5")

  max.date <- date(max(full.df$Timestamp))

  # Average all stations together, except S5
  full.df2 <- full.df %>%
    group_by(Tree, Timestamp) %>%
    summarise(across(c("Solar", "VPD", "Wetness"), ~mean(.x, na.rm = T)))

  # Give ET1 a looser tolerance because VPD is never low enough
  if(!(i %in% c("ET1", "ET2", "ET5", "TV3"))){
    zeroes <- getThreeHourZeroes(full.df2, 25, 0, 0.05)
  } else {
    zeroes <- getThreeHourZeroes(full.df2, 25, 0, 0.075)
  }

  zeroes2 <- zeroes %>%
    adjust6amTo6pm() %>%
    mutate(Tree = i)

  # write_csv(zeroes2, file.path("Sapflow_data_supporting", "Zeroes",
  #                              str_c(i, "_Zeroes_", max.date, ".csv")))
  write_csv(zeroes2, file.path("Sapflow_data_supporting", "Zeroes",
                               str_c(i, "_Zeroes.csv")))

  cat("finished with", i, "\n")
}

## Baseline ----------------------------------------------------------------

zero.filenames <- list.files(file.path("Sapflow_data_supporting", "Zeroes"),
                        full.names = T)

zeroes <- lapply(zero.filenames, read_csv, show_col_types = F) %>%
  bind_rows() %>%
  distinct()
# write_csv(zeroes, file.path("Sapflow_data_supporting", "Zeroes_full.csv"))

# SF.actions needed to identify when sensors were moved
SF.actions <- read_csv(file.path("Sapflow_data_supporting",
                                 "Sapflow_maintenance_actions.csv")) %>%
  mutate(Action = ifelse(Action == "M" | Action == "R" | Action == "MR",
                         "AddOne", Action)) %>%
  filter(is.na(Action) == F)

# Drop known baseline period issues here
zeroes2 <- zeroes %>%
  filter(Tree != "FB2" |
           (periodBegin != ymd_hms("2022-10-31 18:00:00", tz = "UTC") &
           periodBegin != ymd_hms("2022-10-30 18:00:00", tz = "UTC")))

# full.tree.vec <- "FB2"
baseline.flags <- NULL
# i <- "TV4"

# choose full.tree.vec for all or trees.import for just the most recent changes
# tree.vec <- "TV1"
tree.vec <- trees.import
# tree.vec <- full.tree.vec

for(i in tree.vec){

  zero.filename <- zero.filenames[which(str_detect(zero.filenames, i) == T)]

  # If there is no zeroes file, skip to next
  if(length(zero.filename) == 0){
    next
  }

  d <- read_csv(str_c("Sapflow_data_L2/", i, "_Sapflow_L2.csv"),
                show_col_types = FALSE)

  # Bring in the zeroes df created in the Microclimate project
  zeroes.sub <- zeroes2 %>%
    filter(Tree == i) %>%
    select(-Tree) %>%
    filter(periodBegin >= min(d$Timestamp)) %>%
    rowid_to_column("Group")

  # Make expanded zeroes df
  expanded.zeroes <- expand_zeroes(zeroes.sub)

  # Bring in a nested df of the values to use
  vals.to.use <- find_baselining_values(d, expanded.zeroes, zeroes.sub)

  # Prep the actual data to be joined onto the vals.to.use data. Add Grouping column and carry numbers forward
  d2 <- d %>%
    left_join(zeroes.sub, by = c("Timestamp" = "periodBegin")) %>%
    select(-periodEnd) %>%
    mutate(Group = ifelse(Timestamp == min(Timestamp), 0, Group),
           NewGroup = ifelse(is.na(Group) == F, "yes", NA)) %>%
    mutate(Group = na.locf(Group))

  # Adjust group upward after any sensor changes. This is so zeroing values from the previous installation aren't used on the new one. This only applies to the group immediately following the change- later groups are unaffected
  d3 <- d2 %>%
    separate(Cable, into = c("CableNum", "Letter"), sep = 1, remove = F) %>%
    mutate(CableNum = as.numeric(CableNum)) %>%
    left_join(SF.actions,
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
  #   left_join(vals.to.use.nst, by = c("Cable", "Group"))

  d4 <- d3 %>%
    left_join(vals.to.use, by = c("Tree", "Cable", "Group"))

  # Create and save a separate df to track baselining issues
  to.bind <- d4 %>%
    select(Tree, Cable, Timestamp, baseline_inner, baseline_middle,
           baseline_outer) %>%
    mutate(baseline_inner = ifelse(is.na(baseline_inner) == T, "no_bl_val", NA),
           baseline_middle = ifelse(is.na(baseline_middle) == T,
                                    "no_bl_val", NA),
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
  #   mutate(baselined = map2(data, vals.to.use, baseline_sfd)) %>%
  #   select(Cable, Group, baselined) %>%
  #   unnest(baselined) %>%
  #   ungroup() %>%
  #   select(-Group)

  write_csv(d5, file.path("Sapflow_data_L3",
                            str_c(i, "_Sapflow_L3.csv")))
  cat("L3 data saved for", i, "\n")
}

write_csv(baseline.flags, file.path("Sapflow_data_supporting",
                                    str_c("Baseline_flags.csv")))


# Corrections and other -------------------------------------------------------------

## Sensor changes --------------------------------------------

tree.vec <- full.tree.vec

laptop.filepath <- "C:/Users/vaug8/OneDrive - University of Kentucky/TMCF/Continuous_data/Maintenance notes"
desktop.filepath <- "C:/Users/User/OneDrive - University of Kentucky/TMCF/Continuous_data/Maintenance notes"

# change here
filepath <- desktop.filepath

read_sheet <- function(x){
  sheet = read_excel(file.path(filepath, "SensorNotes_All.xlsx"), sheet = x) %>%
    filter(Part == "Cable 1" | Part == "Cable 2" | Part == "Cable 3" | Part == "Cable 4" |
             Part == "Cable 5" | Part == "Cable 6" | Part == "Cable 7" | Part == "Cable 8") %>%
    mutate(Tree = x) %>%
    select(Tree, Cable = Part, everything(), -Location)
  sheet2 = sheet %>%
    pivot_longer(3:ncol(sheet), names_to = "Date", values_to = "Action")
}

d <- lapply(full.tree.vec, read_sheet) %>%
  bind_rows() %>%
  na.omit()

# Warning ok- just bc most entries don't have notes
d2 <- d %>%
  mutate(Cable = str_sub(Cable, start = 7, end = 8)) %>%
  mutate(Date = str_sub(Date, start = 1, end = 10),
         ToD = "10:00:00",
         Timestamp = str_c(Date, ToD, sep = " ")) %>%
  separate(Action, into = c("Action", "Junk"), sep = ";") %>%
  select(Tree, Cable, Timestamp, Action) %>%
  mutate(Timestamp = ymd_hms(Timestamp, tz = "UTC"))

write_csv(d2, "Sapflow_data_supporting/Sapflow_maintenance_actions.csv")

## Rollback import  -------------------------------------------------------
# Roll back Raw data if there was ever an import problem. Particularly needed if a config change ever happens
# Only applies to Raw and L1 data

tree.ID <- "FB2"
rollback.to <- "2024-02-08 09:15:00"

# Only do one at a time, but the for loop is just a trick to get it to run all at once
# i <- "FB6"
for(i in tree.ID){
  Raw <- read_csv(file.path("Sapflow_data_raw", str_c(tree.ID, "_Sapflow.csv"))) %>%
    filter(Timestamp <= as_datetime(rollback.to))
  write_csv(Raw, file.path("Sapflow_data_raw", str_c(tree.ID, "_Sapflow.csv")))
  cat("Raw: rolled back ", i, "to", rollback.to, "\n")

  L1 <-  read_csv(str_c("Sapflow_data_L1/", tree.ID, "_Sapflow_L1.csv")) %>%
    filter(Timestamp <= as_datetime(rollback.to))
  write_csv(L1, str_c("Sapflow_data_L1/", tree.ID, "_Sapflow_L1.csv"))
  cat("L1: rolled back ", i, "to", rollback.to, "\n")

  import.log <- read_csv(file.path(data.support.dir, "Sapflow_data_import_log.csv")) %>%
    mutate(Last.import = ifelse(Tree == tree.ID, as_datetime(rollback.to), Last.import)) %>%
    mutate(Last.import = as_datetime(Last.import))
  write_csv(import.log, file.path(data.support.dir, "Sapflow_data_import_log.csv"))
  cat("Adjusted import log for ", i, "\n")
}

## Add missed data back in ----------------------------------------

tree.ID <- "TV4"
filename <- file.path(data.import.dir, "TV4-Sap_Flow_Station_Data_Table_2023-12-05_2023-12-23.dat")

# Read it in
import.data <- read_sapflow_dat(filename) %>%
  format_sapflow_dat()



# Append to raw
filename.in.raw <- file.path(data.raw.dir, str_c(tree.ID, "_Sapflow.csv"))
raw.data <- read_csv(filename.in.raw)

full.data <- raw.data %>%
  bind_rows(import.data) %>%
  distinct() %>%
  arrange(Tree, Timestamp)

write_csv(full.data, filename.in.raw)

# Then, go to "Raw to L1" and go line by line, processing import.data and appending onto L1. In the for loop, skip to where base.temp is created. Use the following 4 instead of the ones that would be created in the loop
d <- import.data
new.data.starts <- min(import.data$Timestamp, na.rm = T)
new.data.ends <- max(import.data$Timestamp, na.rm = T)
old.data.ends <- min(import.data$Timestamp, na.rm = T) - minutes(15)
i <- tree.ID

# Remove the Timestamps coinciding with the new data, because they were made explicit and filled out with NAs in the L1 processing step. Thus, distinct() does not take care of them.
L1 <- read_csv(file.path("Sapflow_data_L1", str_c(tree.ID, "_Sapflow_L1.csv"))) %>%
  filter(Timestamp < new.data.starts | Timestamp > new.data.ends)



# Other: Work directly with the files ------------------------------------

## .dat --------------------------------------------------------------------

tree.ID <- "FB6"

filename.in.import <- file.path(data.import.dir,
                                str_c(tree.ID, "_Data_Table.dat"))

# Copied directly from the Functions files
read_sapflow_dat <- function(x){
  x2 = read.table(x, skip = 1, stringsAsFactors = F, sep = ",",
                  na.strings = "NAN", header = T)
  x3 = x2 %>%
    slice(-(1:2)) %>%
    distinct() %>%
    rename(Timestamp = TIMESTAMP) %>%
    mutate(Timestamp = as.POSIXct(Timestamp,
                                  format = c("%Y-%m-%d %H:%M"), tz = "UTC")) %>%
    arrange(Timestamp)
  return(x3)
}

dat.test <- read_sapflow_dat(filename.in.import)

dat.dupes <- dat.test %>%
  dplyr::group_by(Timestamp) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

## Raw ---------------------------------------------------------------------

tree.ID <- "TV4"

raw.test <-  read_csv(str_c("Sapflow_data_raw/", tree.ID, "_Sapflow.csv"))

raw.dupes <- raw.test %>%
  dplyr::group_by(Tree, Timestamp) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

## L1 ----------------------------------------------------------------------

tree.ID <- "FB6"

L1.test <-  read_csv(str_c("Sapflow_data_L1/", tree.ID, "_Sapflow_L1.csv"))

L1.dupes <- L1.test %>%
  dplyr::group_by(Tree, Cable, Timestamp) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

L1.dupes <- All.Cables2 %>%
  dplyr::group_by(Tree, Cable, Timestamp) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)
