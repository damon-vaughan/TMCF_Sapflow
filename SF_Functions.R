# Functions and constants for Sapflow diagnostic and processing. Last update mid Sept 2023

# Constants ---------------------------------------------------------------
port.numbers.full <- str_c(rep("Port", 64), seq(1:64))
probe_wiring_key <- c("Outer", "Middle", "Inner")
Thermal_Diffusivity_of_Green_Wood <- 0.0019
Distance_Between_Heater_and_Probes <- 0.5 # Units: cm
full.tree.vec <- c("ET1", "ET2", "ET3", "ET4", "ET5", "ET6", "ET7", "ET8",
             "FB1", "FB2", "FB3", "FB4", "FB5", "FB6", "FB7", "FB8",
             "TV1", "TV2", "TV3", "TV4")

# Importing -------------------------------------------

# Function to read the data and prepare it so it can be filtered by date. Read the .dat file, remove the nonsensical first two rows, remove duplicate rows, convert TIMESTAMP to POSIXct, and arrange by TIMESTAMP
# x is filename
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

# Function format data from .dat file: convert all non-time columns to numeric, rename the first five columns, and add a TreeID column at the beginning.
# x is output from read_sapflow__dat, which must be in R environment
format_sapflow_dat <- function(x){
  x %>%
    mutate(across(2:ncol(x), as.numeric)) %>%
    mutate(TreeID = TreeID) %>%
    rename(Record_Number = RECORD,
           Panel_Temperature = Panel_Temperature_Avg,
           Big_Battery_Voltage = Big_Battery_Voltage_Min,
           Small_Battery_Voltage = Small_Battery_Voltage_Min) %>%
    select(TreeID, everything())
}

# Raw to L1 -------------------------------------------------------------

# x is the dataframe to do this on ("d2"), y goes with "starts_with" to identify the columns you want to extract, and z is the short names you want to give those columns
parse_sfd_columns <- function(x, y, z){
  x2 = x %>%
    select(starts_with(y), Timestamp) %>%
    mutate(type = z)
  colnames(x2) = c(port.numbers.full, "Timestamp", "type")
  return(x2)
}

# This function is applied with mutate(across()) to eliminate numeros locos at an early step. y is minimum temp threshold, z is maximum temp threshold.
NAify_numeros_locos <- function(x, y, z){
  ifelse(x < y, NA, ifelse(x > z, NA, x))
}

# y is a limit for how low an acceptable delta can get, z a high limit. Low deltas result from heater or relay problems, high ones from ???
NAify_small_deltas <- function(x, y, z){
  ifelse(x < y, NA, ifelse(x > z, NA, x))
}

# Makes a character vector with an element for each cable. No inputs needed
make_port_vector_by_cable <- function(){
  cable.locations = Station.info2 %>%
    select(Cable1:Cable8) %>%
    select(!where(~is.na(unique(.))))
  vec = as.numeric(str_remove(names(cable.locations), "Cable"))
  return(vec)
}

# x is an element of the cable_vector produced in the script. Simply a number between 1 and 8. For each cable, the deltas then ratios are calculated, then organized into a df
make_ratio_df <- function(x){
  cable.start = as.numeric(str_split(Station.info2[1,x+1], ":")[[1]][1])
  port.numbers = seq(cable.start, cable.start + 5)
  HRM_Delta = HRM_Deltas %>%
    select(type, Timestamp, all_of(port.numbers))
  Ratios = HRM_Delta[3:5]/HRM_Delta[6:8]
  Ratios2 = Ratios %>%
    bind_cols("Timestamp" = Base_Temp$Timestamp) %>%
    mutate(Cable = x) %>%
    select(Cable, Timestamp, everything())
  colnames(Ratios2) = c("Cable", "Timestamp", "outer", "middle", "inner")
  return(Ratios2)
}

# Function from David to calculate heat pulse velocity
Heat_Pulse_Velocity_Function <- function (x) {
  ((Thermal_Diffusivity_of_Green_Wood / Distance_Between_Heater_and_Probes) * log(x)) * 3600
}

# Apply the heat pulse velocity to all Positions
# x is a ratio df produced above
apply_velocity_function <- function(x){
  x %>%
    mutate(across(c("outer", "middle", "inner"),
                  .fns = Heat_Pulse_Velocity_Function,
                  .names = "{.col}_vel")) %>%
    select(Cable, Timestamp, outer_vel, middle_vel, inner_vel)
}


# L1 to L2 ---------------------------------------

# Function that takes a small df of bad data and expands it into a long one with a row for each timestamp of bad data. In the initial df, each row has tree, cable number, and a nested df showing start and end of bad data. This is written purr style, to be used with map()
# x is the list column in the initial small df.
expand_bad_data <- function(x){
  results <- NULL
  for(j in 1:nrow(x)){
    toBind <- data.frame(
      Timestamp = seq(from = x$Start[j],
                 to = x$End[j],
                 by = "15 min")) %>%
      mutate(bad.data = 1)
    results <- bind_rows(results, toBind)
  }
  return(results)
}

# Applies a new letter to the cable number every time the cable was moved or replaced. This often dramatically affects the data so it's a necessary step
apply_letter <- function(x){
  x$letter[1] = "a"
  x2 <- x %>%
    mutate(letter = na.locf(letter))
  return(x2)
}

# Old Hampel function based on Pracma. Cant can't handle NAs and is really slow.
# apply_hampel_to_SF <- function(x){
#   O = x %>%
#     select(Tree, Timestamp, outer_vel) %>%
#     na.omit()
#   if(nrow(O) >= 50){
#     O <- O %>%
#       mutate(outer_vel = hampel(outer_vel, k = 24, t0 = 3)$y)}
#
#   M = x %>%
#     select(Tree, Timestamp, middle_vel) %>%
#     na.omit()
#   if(nrow(M) >= 50){
#     M <- M %>%
#       mutate(middle_vel = hampel(middle_vel, k = 24, t0 = 3)$y)}
#
#   I = x %>%
#     select(Tree, Timestamp, inner_vel) %>%
#     na.omit()
#   if(nrow(I) >= 50){
#     I <- I %>%
#       mutate(inner_vel = hampel(inner_vel, k = 24, t0 = 3)$y)}
#
#   output = x %>%
#     select(Tree, Timestamp) %>%
#     left_join(O, by = c("Tree", "Timestamp")) %>%
#     left_join(M, by = c("Tree", "Timestamp")) %>%
#     left_join(I, by = c("Tree", "Timestamp"))
#
#   return(output)
# }

apply_hampel_to_SF2 <- function(x){
  x2 <- x %>%
    mutate(outer_vel_hampel = filters::hampel(outer_vel, window_size = 48,
                                               a = 3, miss = "single"),
           middle_vel_hampel = filters::hampel(middle_vel, window_size = 48,
                                               a = 3, miss = "single"),
           inner_vel_hampel = filters::hampel(inner_vel, window_size = 48,
                                              a = 3, miss = "single"))
  x3 <- x2 %>%
    mutate(outer_vel_hampel = ifelse(is.na(outer_vel_hampel) == T &
                                       is.na(outer_vel) == F, outer_vel, outer_vel_hampel),
           middle_vel_hampel = ifelse(is.na(middle_vel_hampel) == T &
                                        is.na(middle_vel) == F, middle_vel, middle_vel_hampel),
           inner_vel_hampel = ifelse(is.na(inner_vel_hampel) == T &
                                       is.na(inner_vel) == F, inner_vel, inner_vel_hampel))

  x4 <- x3 %>%
    mutate(
      outer_flag = ifelse(outer_vel_hampel != outer_vel, "outlier.Hampel", NA),
      middle_flag = ifelse(middle_vel_hampel != middle_vel, "outlier.Hampel", NA),
      inner_flag = ifelse(inner_vel_hampel != inner_vel, "outlier.Hampel", NA))

  x5 <- x4 %>%
    select(-outer_vel, -middle_vel, -inner_vel) %>%
    rename(outer_vel = outer_vel_hampel, middle_vel = middle_vel_hampel,
           inner_vel = inner_vel_hampel)

  return(x5)
}


# L2 to L3 ------------------------------------------------------------

# Function from Danel to find zeroing periods from MC data
getThreeHourZeroes <- function(x, thresh.LWS, thresh.Sol, thresh.VPD){
  zeroesDF <- data.frame(
    "periodBegin" = ymd_hm("2022-08-01 00:00", tz = "UTC"),
    "periodEnd" = ymd_hm("2022-08-01 00:0", tz = "UTC"))
  zeroesDF <- zeroesDF[-1,]
  zeroesDF

  conditionFulfilledDF <- x %>%
    filter(Wetness < thresh.LWS & Solar == thresh.Sol &
             VPD < thresh.VPD)
  counter <- 0

  for(i in seq(2, nrow(conditionFulfilledDF), by = 1)){
    diff <- difftime(conditionFulfilledDF$Timestamp[i],
                     conditionFulfilledDF$Timestamp[i - 1],
                     units = "mins")
    diff <- as.numeric(diff)
    if(diff == 15){
      counter <- counter + 1
      #print(counter)
    } else {
      if(counter >= 12){
        zeroesDF <- zeroesDF %>%
          add_row(periodBegin = conditionFulfilledDF$Timestamp[i - counter - 1],
                  periodEnd = conditionFulfilledDF$Timestamp[i - 1])
      }
      counter <- 0
    }
  }

  if(counter >= 12){
    zeroesDF <- zeroesDF %>%
      add_row(periodBegin =
                conditionFulfilledDF$Timestamp[
                  nrow(conditionFulfilledDF)-counter],
              periodEnd = conditionFulfilledDF$Timestamp[
                nrow(conditionFulfilledDF)])
  }
  return(zeroesDF)
}

adjust6amTo6pm <- function(x){
  #x <- zeroesGuindon
  x$duration <- x$periodEnd - x$periodBegin
  x$startNew <- x$periodBegin
  x$endNew <- x$periodEnd
  for(i in 1:nrow(x)){ #  i <- 1
    if(hour(x$periodBegin[i]) < 18){
      if(hour(x$periodBegin[i]) >= 6){
        x$startNew[i] <- x$startNew[i] +
          (17 - hour(x$startNew[i]))*3600 +
          (60 - minute(x$startNew[i]))*60
      }
    }

    if(hour(x$periodEnd[i]) > 6){
      if(hour(x$periodEnd[i]) < 18){
        x$endNew[i] <- x$endNew[i] -
          (hour(x$endNew[i])-6)*3600 -
          minute(x$endNew[i])*60
      }
    }

    if(hour(x$periodEnd[i]) == 6 & minute(x$periodEnd[i]) > 0){
      x$endNew[i] <- x$endNew[i] -
        (hour(x$endNew[i])-6)*3600 -
        minute(x$endNew[i])*60
    }
  }

  x$newDuration <- x$endNew - x$startNew
  x <- subset(x, newDuration >= 3)
  x <- x[, c("startNew", "endNew")]
  names(x) <- c("periodBegin", "periodEnd")
  return(x)
}

# Make long dataframe of all times meeting the zero criteria.
expand_zeroes <- function(zeroes.og){
  # results <- data.frame(Timestamp = min(df$Timestamp, na.rm = T), Group = 0)
  results <- NULL
  for(i in 1:nrow(zeroes.og)){
    toBind <- data.frame(
      Timestamp = seq(from = zeroes.og$periodBegin[i],
                 to = zeroes.og$periodEnd[i],
                 by = "15 min")) %>%
      mutate(Group = i)
    results <- bind_rows(results, toBind)
  }
  return(results)
}

# Use period start and end times to look up the values to be used in baselining. Values need to be averaged, cleaned, and prepped
find_baselining_values <- function(df, zeroes.exp, zeroes.og){
  valsToUse <- df %>%
    left_join(zeroes.exp, by = "Timestamp") %>%
    filter(is.na(Group) == F) %>%
    group_by(Tree, Cable, Group) %>%
    # Average all the sapflow values during the windows
    summarise(across(c("inner_vel", "middle_vel", "outer_vel"),
                     ~mean(.x, na.rm = T))) %>%
    ungroup() %>%
    # Join Zeroes.og again so we know when the zeroing period starts
    left_join(zeroes.og, by = "Group") %>%
    select(-periodEnd)

  # Need to create a zero row for all groups. Use the same values as Row 1
  firstRows <- valsToUse %>%
    # filter(is.na(outer_vel) == F | is.na(middle_vel) == F |
    #          is.na(inner_vel) == F) %>%
    group_by(Cable) %>%
    filter(Group == min(Group)) %>%
    mutate(Group = 0) %>%
    replace_na(list(outer_vel = 0, middle_vel = 0, inner_vel = 0))

  # Add zero row to valstouse, and fill forward the NAs
  valsToUse2 <- bind_rows(firstRows, valsToUse) %>%
    arrange(Cable, Group) %>%
    group_by(Cable) %>%
    mutate(baseline_inner = na.locf(inner_vel),
           baseline_middle = na.locf(middle_vel),
           baseline_outer = na.locf(outer_vel)) %>%
    ungroup() %>%
    select(-inner_vel, -middle_vel, -outer_vel)

  # valsToUse.nst <- valsToUse2 %>%
  #   group_by(Cable, Group) %>%
  #   nest() %>%
  #   rename(valsToUse = data)

  return(valsToUse2)
}

# df <- d3.nst$data[[1]]
# baseline.vals <- valsToUse.nst$valsToUse[[1]]

# This function written for purr. x is the actual data (LVL1b.nst), and y is the data used to baseline it.
baseline_sfd <- function(df, baseline.vals){
  baselined <- df %>%
    mutate(outer_vel = outer_vel - unique(baseline.vals$outer_vel),
           middle_vel = middle_vel - unique(baseline.vals$middle_vel),
           inner_vel = inner_vel - unique(baseline.vals$inner_vel))
  return(baselined)
}
# x <- 16
# baseline_sfd(d3.nst$data[[x]], valsToUse.nst$valsToUse[[x]])

# Plotting functions -----------------------------------------

# Meant to be used with map()
# x is the tree dataframe after bing split by Cable
# y is whether or not to use free scales on the y-axis
view_SF_cable <- function(x, y){
  x2 = x %>%
    pivot_longer(4:6, names_to = "Position", values_to = "Velocity")
  dateBreaks = unname(quantile(x2$Timestamp, probs = c(0, .33, .66, 1)))
  ggplot(x2) +
    geom_line(aes(x = Timestamp, y = Velocity)) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = dateBreaks) +
    labs(y = "cm / hour") +
    theme_bw() +
    theme(axis.title.x = element_blank()) +
    ggtitle(str_c(TreeID, "_Cable", unique(x2$Cable), "_HRM Velocity")) +
    if(y == "free"){
      facet_wrap(~Position, ncol = 1, scales = "free")
    } else if(y == "fixed"){
      facet_wrap(~Position, ncol = 1, scales = "fixed")
    }
}

view_SF_cable_daterange <- function(x, y, startdate, enddate){
  x2 = x %>%
    filter(Timestamp >= as.POSIXct(startdate, tz = "UTC") &
             Timestamp <= as.POSIXct(enddate, tz = "UTC")) %>%
    pivot_longer(4:6, names_to = "Position", values_to = "Velocity")
  dateBreaks = unname(quantile(x2$Timestamp, probs = c(0, .33, .66, 1)))
  ggplot(x2) +
    geom_line(aes(x = Timestamp, y = Velocity)) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = dateBreaks) +
    labs(y = "cm / hour") +
    theme_bw() +
    theme(axis.title.x = element_blank()) +
    ggtitle(str_c(TreeID, "_Cable", unique(x2$Cable), "_HRM Velocity")) +
    if(y == "free"){
      facet_wrap(~Position, ncol = 1, scales = "free")
    } else if(y == "fixed"){
      facet_wrap(~Position, ncol = 1, scales = "fixed")
    }
}

# Starts with full tree dataframe of all cables. Plot showing inner middle and outer as different colors on one plot, one cable at a time
view_SF_cable_after_filtering <- function(x, y, cable){
  x2 = x %>%
    filter(Cable == cable) %>%
    pivot_longer(4:6, names_to = "Position", values_to = "Velocity")
  ggplot(x2) +
    geom_line(aes(x = Timestamp, y = Velocity)) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = dateBreaks) +
    labs(y = "cm / hour") +
    theme_bw() +
    theme(axis.title.x = element_blank()) +
    ggtitle(str_c(TreeID, "_Cable", unique(x2$Cable), "_HRM Velocity")) +
    if(y == "free"){
      facet_wrap(~Position, ncol = 1, scales = "free")
    } else if(y == "fixed"){
      facet_wrap(~Position, ncol = 1, scales = "fixed")
    }
}

# Same plot but zooming in on a daterange
view_SF_cable_after_filtering_daterange <- function(x, y, cable, startdate, enddate){
  x2 = x %>%
    filter(Cable == cable) %>%
    filter(Timestamp >= as.POSIXct(startdate, tz = "UTC") &
             Timestamp <= as.POSIXct(enddate, tz = "UTC")) %>%
    pivot_longer(4:6, names_to = "Position", values_to = "Velocity")
  ggplot(x2) +
    geom_line(aes(x = Timestamp, y = Velocity)) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = dateBreaks) +
    labs(y = "cm / hour") +
    theme_bw() +
    theme(axis.title.x = element_blank()) +
    ggtitle(str_c(TreeID, "_Cable", unique(x2$Cable), "_HRM Velocity")) +
    if(y == "free"){
      facet_wrap(~Position, ncol = 1, scales = "free")
    } else if(y == "fixed"){
      facet_wrap(~Position, ncol = 1, scales = "fixed")
    }
}

# Old functions that are likely not used any more -------------------------

# Quick and dirty function for dropping the extreme outliers. This should eventually be replaced with a hampel filter. This simply cuts off lower and upper values based on a percentile input, typically (0.005, 0.995), which always removes the same # of observations, regardless of how extreme they are.
# drop_spikes <- function(x, lower.limit, upper.limit){
#   inner.bounds <- c(quantile(x$inner_vel, lower.limit, na.rm = T),
#                     quantile(x$inner_vel, upper.limit, na.rm = T))
#   middle.bounds <- c(quantile(x$middle_vel, lower.limit, na.rm = T),
#                      quantile(x$middle_vel, upper.limit, na.rm = T))
#   outer.bounds <- c(quantile(x$outer_vel, lower.limit, na.rm = T),
#                     quantile(x$outer_vel, upper.limit, na.rm = T))
#   x2 <- x %>%
#     mutate(inner_vel = ifelse(inner_vel < inner.bounds[1] |
#                                 inner_vel > inner.bounds[2], NA, inner_vel),
#            middle_vel = ifelse(middle_vel < middle.bounds[1] |
#                                  middle_vel > middle.bounds[2], NA, middle_vel),
#            outer_vel = ifelse(outer_vel < outer.bounds[1] |
#                                 outer_vel > outer.bounds[2], NA, outer_vel))
#   return(x2)
# }

# Position is inner, middle, or outer. Location is a column that is used to segregate the full sfd- could be by cable, or could be branch vs trunk
# sapflow_plot_by_tree <- function(sfd, position, location){
#   ggplot(sfd) +
#     geom_line(aes(x = Time, y = .data[[position]],
#                   color = factor(.data[[location]]))) +
#     geom_hline(yintercept = 0) +
#     labs(y = "cm / hour", color = location) +
#     theme_bw() +
#     theme(axis.title.x = element_blank()) +
#     ggtitle(str_c(unique(sfd$Tree), "_", position, "_HRM"))
# }

# sapflow_plot_by_tree_daterange <- function(sfd, position, location, startdate, enddate){
#   ggplot(sfd %>%
#            filter(Time >= as.POSIXct(startdate, tz = "UTC") &
#                     Time <= as.POSIXct(enddate, tz = "UTC"))) +
#     geom_line(aes(x = Time, y = .data[[position]],
#                   color = factor(.data[[location]]))) +
#     geom_hline(yintercept = 0) +
#     labs(y = "cm / hour", color = location) +
#     theme_bw() +
#     theme(axis.title.x = element_blank()) +
#     ggtitle(str_c(unique(sfd$Tree), "_", position, "_HRM"))
# }

# Produce 3-facet velocity graphs showing outer, middle, and inner of a cable. X is the "All.cables" dataframe that has sapflow velocity for all cables in a tree. y is cable number


# With scales not free
# sapflow_diagnostic_plot2 <- function(x, y){
#   x2 = x %>%
#     filter(Cable == y) %>%
#     pivot_longer(4:6, names_to = "Position", values_to = "Velocity")
#   y.limits = quantile(x2$Velocity, probs = c(.05, .95), na.rm = T)
#   dateBreaks = unname(quantile(x2$Time, probs = c(0, .33, .66, 1)))
#   ggplot(x2) +
#     geom_line(aes(x = Time, y = Velocity)) +
#     geom_hline(yintercept = 0) +
#     # ylim(y.limits) +
#     scale_x_continuous(breaks = dateBreaks) +
#     labs(y = "cm / hour") +
#     theme_bw() +
#     ggtitle(str_c(unique(x$Tree), "_Cable", y, "_HRM Velocity")) +
#     facet_wrap(~Position, ncol = 1)
# }

# Diagnostics -------------------------------------------------------------
# These functions are only used in the diagnostic script and not part of the data processing pipeline

# Function to plot voltage levels over time
# x is the full sapflow dataset after formatting column types, but before parsing out the various components
plot_voltages <- function(x){
  x2 = x %>%
    filter(Big_Battery_Voltage > 8)
  ggplot(x2) +
    geom_line(aes(x = Timestamp, y = Big_Battery_Voltage)) +
    geom_hline(yintercept = 12, color = "blue") +
    geom_hline(yintercept = 11.5, color = "red") +
    theme_bw()
}

# Makes a character vector with an element for each port. Needed to make the before/after dataframe
make_port_vector_by_probe <- function(){
  cable.locations = Station.info2 %>%
    select(Cable1:Cable8) %>%
    select(!where(~is.na(unique(.))))
  vec = as.numeric(str_remove(names(cable.locations), "Cable"))
  vec2 = sort(rep(vec, 2))
  vec3 = rep(c("U", "L"), length(vec2)/2)
  vec4 = str_c(vec2, vec3)
  return(vec4)
}

# Function to help make before/after heat pulse plots useful to see if heaters failed
# x is cable#???
make_before_after_by_probe <- function(x){
  cable = as.numeric(str_sub(x, start = 1, end = 1))
  placement = str_sub(x, start = 2, end = 2)
  cable.start = as.numeric(str_split(Station.info2[1,cable+1], ":")[[1]][1])
  if(placement == "U"){
    port.numbers = seq(cable.start, cable.start + 2)
  } else {
    port.numbers = seq(cable.start + 3, cable.start + 5)
  }
  Before = Base_Temp %>%
    select(type, Timestamp, all_of(port.numbers))
  After = After_Pulse %>%
    select(type, Timestamp, all_of(port.numbers))
  Before_After = Before %>%
    bind_rows(After) %>%
    mutate(Cable = cable,
           Placement = placement) %>%
    select(Cable, Placement, Timestamp, everything())
  new.names = str_c(names(Before_After)[-(1:4)], "_", probe_wiring_key)
  colnames(Before_After) = c("Cable", "Placement", "Timestamp", "type", new.names)
  return(Before_After)
}

# Create the before/after plots to diagnose heater problems
# x is list dataframe (list element)
# y is cable number
plot_before_after <- function(x){
  x2 = x %>%
    pivot_longer(5:7, names_to = "Position", values_to = "Temperature")
  dateBreaks = unname(quantile(x$Timestamp, probs = c(0, .33, .66, 1)))
  ggplot(x2) +
    geom_line(aes(x = Timestamp, y = Temperature, color = type)) +
    # ylim(15, 25) +
    scale_x_continuous(breaks = dateBreaks) +
    ggtitle(str_c(TreeID, "_Cable_", unique(x2$Cable), "_", unique(x2$Placement))) +
    theme_bw() +
    facet_wrap(~Position, scales = "free", ncol = 1) +
    theme(legend.position = "none")
}

# Same plot as previous, but for looking at a small number of points. Useful if you have just started the datalogger and don't have that many points yet
plot_before_after_point <- function(x){
  x2 = x %>%
    pivot_longer(5:7, names_to = "Position", values_to = "Temperature")
  dateBreaks = unname(quantile(x$Timestamp, probs = c(0, .33, .66, 1)))
  ggplot(x2) +
    geom_point(aes(x = Timestamp, y = Temperature, color = type)) +
    # ylim(15, 25) +
    scale_x_continuous(breaks = dateBreaks) +
    ggtitle(str_c(TreeID, "_Cable_", unique(x2$Cable), "_", unique(x2$Placement))) +
    theme_bw() +
    facet_wrap(~Position, scales = "free", ncol = 1) +
    theme(legend.position = "none")
}

