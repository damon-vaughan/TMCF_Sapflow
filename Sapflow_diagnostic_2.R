# Diagnostic script for checking sapflow stations in the field.

library(needs)
needs(tidyverse, lubridate, readxl)

source("SF_Functions.R")

tree.ID <- "FB2"
days_ago_to_end <- 1
days_to_view <- 10

station.info <- read_excel("Sapflow_data_supporting/Sapflow_station_info.xlsx")

station.info2 <- station.info %>%
  filter(Tree == tree.ID)

# filename <- str_c("SF_DL2/", TreeID, "_Data_Table.dat")
# filename <- str_c("Sapflow_data_import/", TreeID, "_Data_Table.dat")
filename <- str_c("H:/My Drive/TMCF/TMCF_CostaRica_equipo/Data_share/Sapflow_data_share/", tree.ID, "_Data_Table.dat")
# filename <- str_c("Sapflow_data_import_fullDL/", TreeID, "_Data_Table.dat")
d <- read_sapflow_dat(filename)
d2 <- format_sapflow_dat(d)

DateMax <- max(d2$Time, na.rm = T)
EndDate <- DateMax - (days_ago_to_end * 86400)
StartDate <- EndDate - (days_to_view * 86400)

# Manually entering
# StartDate <- as.POSIXct(("2022-09-01 00:00:00"))
# EndDate <- as.POSIXct(("2022-10-31 23:59:59"))

d3 <- d2 %>%
  filter(Big_Battery_Voltage != 0,
         Timestamp >= StartDate & Timestamp < EndDate)

base.temp <- parse_sfd_columns(d3, "Wood_Temperature", "Base.temp")
after.pulse <- parse_sfd_columns(d3, "After_Heat_Pulse_Temperature", "After.pulse")

cable_vector <- make_port_vector_by_probe()

Before_After_List <- lapply(cable_vector, make_before_after_by_probe)
# ggplotly(plot_before_after(Before_After_List[[16]]))

lapply(rev(Before_After_List), plot_before_after)

# If you want to see the voltages over time...
plot_voltages(d2)

# Calculate sap flow velocity ---------------------------------------------

HRM_Deltas_Pre <- after.pulse[,-(65:66)] - base.temp[,-(65:66)]
HRM.deltas <- HRM_Deltas_Pre %>%
  bind_cols("Timestamp" = base.temp$Timestamp) %>%
  mutate(type = "HRM.delta")

cable_vector <- make_port_vector_by_cable()

ratios <- lapply(cable_vector, make_ratio_df)
velocities <- lapply(ratios, apply_velocity_function)

All.Cables <- bind_rows(velocities) %>%
  filter(is.na(Timestamp) != T) %>%
  mutate(Tree = TreeID) %>%
  select(Tree, everything())

# All.Cables2 <- All.Cables %>%
#   pivot_longer(4:6, names_to = "Position", values_to = "Velocity")

All.Cables.split <- All.Cables %>%
  split(~Cable)

# graficos
lapply(All.Cables.split, view_SF_cable, "fixed")

# !!!!Detenerse aqui ------------------------------------------------------------

# !!!! Stop here ----------------------------------------------------------

# To just look at the few most recent points ------------------------------

DateMax <- max(d2$Time, na.rm = T)

d3_short <- d3 %>%
  filter(Time <= DateMax) %>%
  slice_tail(n = 5)

Base_Temp <- parse_sfd_columns(d3_short, "Wood_Temperature", "Base_Temp")
After_Pulse <- parse_sfd_columns(d3_short, "After_Heat_Pulse_Temperature", "After_Pulse")

cable_vector <- make_port_vector_by_probe()

Before_After_List <- lapply(cable_vector, make_before_after_by_probe)

lapply(rev(Before_After_List), plot_before_after_point)




# Find how long a battery lasts -------------------------------------------
# Filter the data to find lines where battery voltage is zero. These are times it was changed.

last.block <- d2 %>%
  filter(Time >= "2022-10-05 10:15:00")
max(last.block$Time) - min(last.block$Time)
weeks <- 15.8/7
(max(last.block$Big_Battery_Voltage) - min(last.block$Big_Battery_Voltage))/weeks


# Messin around -----------------------------------------------------------

plot_velocity_by_cable_alt <- function(x, y){
  x2 = x %>%
    filter(Cable == y)
  y.limits = quantile(x2$Velocity, probs = c(.05, .95), na.rm = T)
  dateBreaks = unname(quantile(x2$Time, probs = c(0, .33, .66, 1)))
  shade.margin = (max(x2$Velocity) - min(x2$Velocity)) * 0.05
  ggplot(x2) +
    geom_line(aes(x = Time, y = Velocity)) +
    geom_hline(yintercept = 0) +
    # ylim(y.limits) +
    scale_x_continuous(breaks = dateBreaks) +
    labs(y = "cm / hour") +
    theme_bw() +
    ggtitle(str_c(TreeID, "_Cable", y, "_HRM Velocity")) +
    facet_wrap(~Position, ncol = 1, scales = "free") +
    annotate("rect", fill = "blue", alpha = 0.25,
             xmin = as.POSIXct("2022-09-12 0:00:00",
                               tz = "UTC"),
             xmax = as.POSIXct("2022-09-18 22:00:00",
                               tz = "UTC"),
             ymin = min(x2$Velocity) - shade.margin,
             ymax = max(x2$Velocity) + shade.margin) +
    annotate("rect", fill = "red", alpha = 0.25,
             xmin = as.POSIXct("2022-10-25 0:00:00",
                               tz = "UTC"),
             xmax = as.POSIXct("2022-10-30 22:00:00",
                               tz = "UTC"),
             ymin = min(x2$Velocity) - shade.margin,
             ymax = max(x2$Velocity) + shade.margin)
}

plot_velocity_by_cable_alt(All.Cables2, 1)
