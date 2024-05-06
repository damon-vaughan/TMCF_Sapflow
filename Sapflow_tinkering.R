
library(needs)
needs(tidyverse, readxl, lubridate, corrr)

source("SF_Functions.R")
options(readr.show_col_types = FALSE)


# Directories -------------------------------------------------------------

# Directory paths
data.support.dir <- file.path("Sapflow_data_supporting")
MC.dir <- "C:/Users/vaug8/OneDrive - University of Kentucky/TMCF/Continuous_data/TMCF_Microclimate"
analysis.dir <- "C:/Users/vaug8/OneDrive - University of Kentucky/TMCF/TMCF_Analysis/TMCF_data_raw"


# Check small batt voltages -----------------------------------------------
TreeID <- "TV4"
d <- read_csv(file.path("Sapflow_data_raw", str_c(TreeID, "_Sapflow.csv")))

ggplot(d %>% filter(Big_Battery_Voltage != 0)) +
  geom_line(aes(x = Time, y = Big_Battery_Voltage)) +
  geom_hline(yintercept = 11.75) +
  theme_bw()

# Make sapflow maintenance actions sheet -----------------------

TreeVec <- FullTreeVec

read_sheet <- function(x){
  sheet = read_excel("C:/Users/vaug8/OneDrive - University of Kentucky/TMCF/Site info/Maintenance notes/SensorNotes_All.xlsx", sheet = x) %>%
    filter(Part == "Cable 1" | Part == "Cable 2" | Part == "Cable 3" | Part == "Cable 4" |
             Part == "Cable 5" | Part == "Cable 6" | Part == "Cable 7" | Part == "Cable 8") %>%
    mutate(Tree = x) %>%
    select(Tree, Cable = Part, everything(), -Location)
  sheet2 = sheet %>%
    pivot_longer(3:ncol(sheet), names_to = "Date", values_to = "Action")
}

d <- lapply(FullTreeVec, read_sheet) %>%
  bind_rows() %>%
  na.omit()

d2 <- d %>%
  mutate(Cable = str_sub(Cable, start = 7, end = 8)) %>%
  mutate(Date = str_sub(Date, start = 1, end = 10),
         ToD = "10:00:00",
         Time = str_c(Date, ToD, sep = " ")) %>%
  select(Tree, Cable, Time, Action) %>%
  mutate(Time = ymd_hms(Time, tz = "UTC"))

d3 <- d2 %>%
  mutate(Action = case_when(
    str_detect(Action, "ove") == T & str_detect(Action, "eplace") == T ~ "MR",
    str_detect(Action, "ove") == T ~ "M",
    str_detect(Action, "eplace") == T ~ "R")
  )

write_csv(d3, "Sapflow_data_supporting/Sapflow_maintenance_actions.csv")
write_csv(d3, "C:/Users/vaug8/OneDrive - University of Kentucky/TMCF/Continuous_data/Apps/Sapflow_app/Sapflow_maintenance_actions.csv")


# Gap filling ---------------------------------------------------

library(needs)
needs(tidyverse, here, readxl, lubridate, corrr)

# Functions:
source("SF_Functions.R")

## Gap filling length ones -------------------------------------------------

find_length1_NA <- function(x){
  x2 = rle(is.na(x$Velocity))
  x3 = data.frame(lgth = x2$lengths, vals = as.numeric(x2$values)) %>%
    mutate(onlyOne = ifelse(lgth == vals, 1, 0))
  x4 = inverse.rle(list(lengths = x3$lgth, values = x3$onlyOne))
  x5 = data.frame(y = x$Velocity, impute = x4)
  return(x5)
}

impute_length1_NA <- function(x, y){
  x2 = x %>%
    mutate(down1 = lag(y),
           up1 = lead(y)) %>%
    mutate(imputedY = ifelse(impute == 1, (down1 + up1)/2, y))
  y2 = y %>%
    mutate(imputedY = x2$imputedY,
           imputed = x2$impute)
  return(y2)
}

TreeVec <- FullTreeVec
# i <- "FB4"
for(i in TreeVec){
  TreeID <- i
  d <- read_csv(str_c("Sapflow_data_LVL3/", TreeID, "_LVL3.csv"), show_col_types = FALSE) %>%
    pivot_longer(4:6, names_to = "Position", values_to = "Velocity") %>%
    arrange(Tree, Cable, Position, Time)
  d2 <- d   %>%
    group_by(Cable, Position) %>%
    nest()
  d3 <- d2 %>%
    mutate(length1.NA = map(data, find_length1_NA)) %>%
    mutate(imputed = map2(length1.NA, data, impute_length1_NA))
  d4 <- d3 %>%
    select(Cable, Position, imputed) %>%
    unnest(cols = imputed) %>%
    ungroup()
  # %>%
  #   mutate(imputed = case_when(
  #     imputed == 1 ~ "Impute1"))
  write_csv(d4, str_c("Sapflow_data_LVL4/", TreeID, "_LVL4.csv"))
}

## Gap filling within sensor ----------------------------------------------

# FB4 Cable 1, early on
TreeID <- "FB4"
d <- read_csv(here("Sapflow_data_LVL4", str_c(TreeID, "_LVL4.csv"))) %>%
  filter(Cable == 1)

x <- "outer_vel"
find_longer_NAs <- function(x){
  d2 <- d %>%
    filter(Position == x) %>%
    rownames_to_column("index") %>%
    mutate(index = as.numeric(index))
  ind <- d2 %>%
    select(index, Time)
  missloc = rle(is.na(d2$imputedY))
  missloc2 = data.frame(lgth = missloc$lengths, vals = as.numeric(missloc$values)) %>%
    mutate(index = cumsum(lgth)) %>%
    filter(vals == 1) %>%
    left_join(ind, by = "index")
  return(missloc2)
}
outer <- find_longer_NAs("outer_vel") %>%
  mutate(End = Time + ((lgth - 1) * minutes(15)))
middle <- find_longer_NAs("middle_vel") %>%
  mutate(End = Time + ((lgth - 1) * minutes(15)))
inner <- find_longer_NAs("inner_vel") %>%
  mutate(End = Time + ((lgth - 1) * minutes(15)))

seq(ymd_hms(outer[1,4]), ymd_hms(outer[1,5]), by = "15 mins")

find_data_within_sensor <- function(x){
  outer
}

outer <- find_longer_NAs("outer_vel") %>%
  mutate(End = Time + ((lgth - 1) * minutes(15)))
middle <- find_longer_NAs("middle_vel") %>%
  mutate(End = Time + ((lgth - 1) * minutes(15)))
inner <- find_longer_NAs("inner_vel") %>%
  mutate(End = Time + ((lgth - 1) * minutes(15)))

## Gap filling within tree -------------------------------------------------

# Start with the FB4 Cable 6 gap December 6 - 28
TreeID <- "FB4"
d <- read_csv(here("Sapflow_data_LVL4", str_c(TreeID, "_LVL4.csv")))

d.6  <- d %>%
  filter(Cable == 6) %>%
  select(Tree, Cable, Position, Time, imputedY) %>%
  pivot_wider(names_from = Position, values_from = imputedY) %>%
  mutate(allMissing = ifelse(is.na(inner_vel) == T & is.na(middle_vel) == T & is.na(outer_vel) == T,
                             1, 0))

d.5 <- d %>%
  filter(Cable == 5) %>%
  select(Tree, Cable, Position, Time, imputedY) %>%
  mutate(Position = str_c(Position, "_C5")) %>%
  pivot_wider(names_from = Position, values_from = imputedY)


x <- d.6
find_allMissing_NAs <- function(x){
  d2 <- x %>%
    rownames_to_column("index") %>%
    mutate(index = as.numeric(index))
  ind <- d2 %>%
    select(index, Time)
  missloc = rle(d2$allMissing)
  missloc2 = data.frame(lgth = missloc$lengths, vals = missloc$values) %>%
    mutate(index = cumsum(lgth),
           index = lag(index)) %>%
    filter(vals == 1) %>%
    left_join(ind, by = "index")
  return(missloc2)
}

missloc <- find_allMissing_NAs(d.6) %>%
  mutate(End = Time + ((lgth - 1) * minutes(15)))

# Start with the 14th
i <- 14
# Make a comparison df that has the response and all possible predictors. Included dates are the gap period and the week leading up to it.
comp.df <- data.frame(
  Time = seq(ymd_hms(missloc[i,4]) - weeks(1), ymd_hms(missloc[i,5]), by = "15 mins")) %>%
  left_join(d.6, by = "Time") %>%
  select(-allMissing) %>%
  left_join(d.5, by = c("Tree", "Time"))

# Make correlation matrix to pick the best
corrmat <- comp.df %>%
  select(6:10, -Cable.y) %>%
  correlate()

corrmat2 <- corrmat %>%
  slice_head(n = 1) %>%
  pivot_longer(3:5, names_to = "Sensor", values_to = "corr") %>%
  arrange(desc(corr))

comp.df2 <- comp.df %>%
  select(Tree, Time, outer_vel, winner = as.character(corrmat2[1, 3]))

mod <- lm(outer_vel ~ winner, comp.df2)
new <- comp.df2 %>%
  filter(is.na(outer_vel) == T) %>%
  select(-outer_vel)
new$ypreds <- predict(mod, new)

d.6a <- d.6 %>%
  left_join(new, by = c("Tree", "Time"))

## Gap fill from neighbor tree ---------------------------------------------

# This is the cleanest data but has 2 gaps
FB2 <- read_csv(file.path("Sapflow_data_L3", "FB2_Sapflow_L3.csv")) %>%
  filter(str_detect(Cable, "1") == T) %>%
  select(Tree, Cable, Timestamp, outer_vel)

# Maybe useful for gap filling
FB1 <- read_csv(file.path("Sapflow_data_L3", "FB1_Sapflow_L3.csv")) %>%
  filter(str_detect(Cable, "1") == T) %>%
  select(Tree, Cable, Timestamp, outer_vel)

# No gaps but not quite as nice looking as FB2
FB5 <- read_csv(file.path("Sapflow_data_L3", "FB5_Sapflow_L3.csv")) %>%
  filter(str_detect(Cable, "1") == T) %>%
  select(Tree, Cable, Timestamp, outer_vel)

d <- FB2 %>%
  bind_rows(FB1) %>%
  pivot_wider(names_from = Tree, values_from = outer_vel)

d2 <- d %>%
  separate(Timestamp, into = c("Day", "ToD"), sep = " ", remove = F) %>%
  mutate(Hour = hour(Timestamp))
str(d2)

ggplot(d) +
  geom_line(aes(x = Timestamp, y = FB2)) +
  theme_bw()

mod1 <- lm(FB2 ~ FB1 + factor(Hour), data = d2)
summary(mod1)
anova(mod1)

# Copied and adapted from Danel's code ------------------------------------

d2 <- d %>%
  mutate(isMissing = ifelse(is.na(imputedY) == T, 1, 0))

missinLoc <- which(d2$isMissing == 1)
sameSlot <- numeric(length(missinLoc))
sameSlot[1] <- val <- 1

for(j in 1:(length(missinLoc) - 1)){
  if(missinLoc[j+1] != missinLoc[j] + 1)  val <- val + 1
  sameSlot[j+1] <- val
}

missings <- data.frame(missingLoc = missinLoc, sameSlot = sameSlot)

# Was 6 in Danel's script
numPtsToTake <- 12
# maxMiss <- 5

# Grab the six values before the NA string starts and the 6 values after it ends
rowFirst <- (min(missings$missingLoc[missings$sameSlot == 1]) - numPtsToTake)
rowLast <- (max(missings$missingLoc[missings$sameSlot == 1]) + numPtsToTake)

missDF <- d2 %>%
  slice(rowFirst:rowLast) %>%
  select(Tree, Position, Cable, Time, imputedY)
# Make sure this is less than 6
sum(is.na(missDF$imputedY))

corMax <- 0; winnerFound <- 0

dx <- d %>%
  filter(Position == "middle_vel")

# make sure these are diff- different plants
unique(d2$Position) != unique(dx$Position)

# And these should be equal- same trees
unique(d2$Tree) == unique(dx$Tree)

compData <- subset(dx, Time %in% missDF$Time, select = c("Tree", "Position", "Cable", "Time", "imputedY"))

# same number of rows
nrow(compData) == nrow(missDF)

# There is data to impute
sum(is.na(compData$imputedY[which(is.na(missDF$imputedY))])) == 0

# There is at most one missing value in the data to impute
sum(is.na(compData$imputedY)) <= 1

# Make sure all values in +- 1 hr are observed
sum(is.na(missDF$imputedY)) == length(missings$missingLoc[missings$sameSlot == 1])

corVal <- abs(cor(missDF$imputedY, compData$imputedY, use = "complete.obs"))

if(corVal > corMax){
  corMax <- corVal
  winner <- compData
  winnerFound <- 1
}

junk <- data.frame(good = winner$imputedY, bad = missDF$imputedY)
mod <- lm(bad ~ good, junk)
new <- data.frame(good = junk$good[is.na(junk$bad)])
new$ypreds <- predict(mod, new)



# End here ----------------------------------------------------------------



d2 <- d %>%

  mutate(is.gap = ifelse(is.na(Velocity) == T, "y", "n"))
gap.table <- data.frame(lengths = rle(d2$is.gap)$lengths, values = rle(d2$is.gap)$values) %>%
  mutate(index = cumsum(lengths))

# Step1: Grab the right Position from the LVL3 data ---------------------

filenames <- list.files("Sapflow_data_LVL3", full.names = T)

guide <- read_excel(here("Sapflow_data_supporting", "Sapflow_cable_notes.xlsx")) %>%
  mutate(Use = case_when(
    Use == "Outer" ~ "outer_vel",
    Use == "Middle" ~ "middle_vel",
    Use == "Inner" ~ "inner_vel",
    Use == "NA" ~ "None"
  ))

# x <- filenames[1]
create_final_dataset <- function(x){
  d <- read_csv(x, show_col_types = F) %>%
    pivot_longer(4:6, names_to = "Position", values_to = "Velocity")

  guide2 <- guide %>%
    filter(TreeID == unique(d$Tree)) %>%
    select(-Note)

  d2 <- d %>%
    left_join(guide2, by = c("Tree" = "TreeID", "Cable"))

  d3 <- d2 %>%
    filter(Position == Use) %>%
    filter(Time >= Starting | is.na(Starting) == T) %>%
    select(Tree, Cable, Time, Position, Velocity) %>%
    mutate(Position = as.factor(Position))

  return(d3)
}

fullList <- lapply(filenames, create_final_dataset)

fullDF <- fullList %>%
  bind_rows() %>%
  mutate(Position = factor(Position)) %>%
  filter(Time >= as.POSIXct("2022-09-01 00:00:00", tz = "UTC"))

write_csv(fullDF, "Sapflow_data_final/Sapflow_ToMarch2023/fullDF.csv")


# ** Step2: Cable gap filling ------------------------------------------
x <- d.sub.wide
y <- "Cable1"
z <- "Cable3"
gap_fill_simple <- function(x, y, z){
  x2 <- x %>%
    mutate(Cable = y) %>%
    select(Tree, Time, Cable, all_of(y), all_of(z))

  mod1 <- lm(as.formula(str_c(colnames(x2)[4], "~", colnames(x2)[5])),
             data = x2)

  x3 <- x2 %>%
    mutate(yhat = predict(mod1, newdata = x2)) %>%
    select(Tree, Time, Cable, yhat)

  return(x3)
}
d <- read_csv("Sapflow_data_final/Sapflow_ToMarch2023/fullDF.csv") %>%
  select(-Position)

d.sub <- d %>%
  filter(Tree == "FB2") %>%
  mutate(Cable = str_c("Cable", Cable))

plot_fullDF2 <- function(x){
  ggplot(x) +
    geom_line(aes(x = Time, y = Velocity)) +
    theme_bw() +
    facet_wrap(~Cable, ncol = 1) +
    # xlim(c(as.POSIXct("2022-10-01"), as.POSIXct("2023-03-15"))) +
    scale_x_datetime(date_breaks="1 month", date_labels="%m-%Y",
                     limits = c(as.POSIXct("2022-09-01"), as.POSIXct("2023-03-15"))) +
    theme(strip.text = element_blank()) +
    theme(strip.text = element_text(size = 7)) +
    guides(color = "none") +
    ggtitle(unique(x$Tree))
}
