library(needs)
needs(tidyverse, readxl, lubridate, corrr)
needs(tidymodels, ranger)

source("SF_Functions.R")
options(readr.show_col_types = FALSE)

# inspiration:
# https://www.tidymodels.org/start/models/

# Directories -------------------------------------------------------------

# Directory paths
data.support.dir <- file.path("Sapflow_data_supporting")
MC.dir <- "C:/Users/vaug8/OneDrive - University of Kentucky/TMCF/Continuous_data/TMCF_Microclimate"
analysis.dir <- "C:/Users/vaug8/OneDrive - University of Kentucky/TMCF/TMCF_Analysis/TMCF_data_raw"

# Setting up data ---------------------------------------------------------

FB1.1 <- read_csv(file.path("Sapflow_data_L3", "FB1_Sapflow_L3.csv")) %>%
  filter(Cable == "1a") %>%
  select(Tree, Cable, Timestamp, outer_vel)

MC <- read_csv(file.path(MC.dir, "Microclimate_data_L3", "FBP1_MC_L3.csv"))

dormancy <- read_excel(file.path(analysis.dir, "Tree_dormancy_SF.xlsx"),
                       col_types = c("guess", "guess", "guess", "date",
                                     "date", "skip", "skip", "skip")) %>%
  filter(Tree == "FB1", Cable == 1)

sfd <- FB1.1 %>%
  mutate(outer_vel = ifelse(
    Timestamp >= pull(dormancy, Dormant) & Timestamp <= pull(dormancy, Returned),
    NA, outer_vel)) %>%
  distinct()

mc <- MC %>%
  select(Timestamp, Solar, VPD, Wind_speed, Precipitation, Wetness) %>%
  distinct() %>%
  group_by(Timestamp) %>%
  summarise(across(where(is.numeric), ~mean(., na.rm = T))) %>%
  ungroup()
which(duplicated(mc$Timestamp) == T)

d <- sfd %>%
  left_join(mc, by = "Timestamp")
str(d)

ggplot(d) +
  geom_line(aes(x = Timestamp, y = outer_vel)) +
  theme_bw()

set.seed(222)
# Put 3/4 of the data into the training set
data.split <- initial_split(d, prop = 3/4)

# Create data frames for the two sets:
train.data <- training(data.split)
test.data  <- testing(data.split)

# Finding lengths 1-5 ------------------------------------------------------------

# Pad both ends so there are good values before and after the gap
x <- d
find_small_gaps <- function(x){
  x2 <- x %>%
    rownames_to_column("index") %>%
    mutate(Index = as.numeric(index))
  ind <- x2 %>%
    select(Index, Timestamp)
  Miss.loc <- rle(is.na(x2$outer_vel))
  Miss.loc2 <- data.frame(Length = Miss.loc$lengths, Values = as.numeric(Miss.loc$values)) %>%
    mutate(Index = cumsum(Length)) %>%
    filter(Values == 1) %>%
    filter(Length <= 5)
  Miss.loc3 <- Miss.loc2 %>%
    left_join(ind, by = "Index") %>%
    mutate(Gap.num = seq(1, nrow(Miss.loc2))) %>%
    mutate(Start = Timestamp - ((Length) * minutes(15))) %>%
    mutate(End = Timestamp + minutes(15)) %>%
    select(Gap.num, Length, Start, End)
  return(Miss.loc3)
}

d.na <- find_small_gaps(d)

x <- d.na
i <- 13
expand_timestamps <- function(x){
  results <- NULL
  for(i in 1:nrow(x)){
    toBind <- data.frame(
      Timestamp = seq(from = x$Start[i],
                      to = x$End[i],
                      by = "15 min")) %>%
      mutate(Gap.num = i)
    results <- bind_rows(results, toBind)
  }
  return(results)
}

d.na2 <- d.na %>%
  expand_timestamps()

d2 <- d %>%
  left_join(d.na2, by = "Timestamp")




# Lengths 1 thru 5 --------------------------------------------------------
# Linear interpolation of values on either side




# Modeling ------------------------------------------------

rf.mod <- rand_forest(
  # mtry = ,
  trees = 200,
  min_n = 5
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf.fit <- rf.mod %>%
  fit(outer_vel ~ Solar + VPD + Wetness, data = train.data)

test.data2 <- test.data %>%
  bind_cols(predict(rf.fit, test.data))

cor(test.data2$outer_vel, test.data2$.pred) ^ 2

lm.mod <- linear_reg() %>%
  set_engine("lm")

lm.fit <- lm.mod %>%
  fit(outer_vel ~ Solar * VPD, data = train.data)

test.data3 <- test.data %>%
  bind_cols(predict(lm.fit, test.data))

cor(test.data3$outer_vel, test.data3$.pred) ^ 2
