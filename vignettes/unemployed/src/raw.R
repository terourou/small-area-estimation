
library(dplyr)
library(tidyr)
library(readr)

raw <- read_csv("data/ARXSTK1_20210313-211050.csv",
                skip = 2) %>%
    select(age,
           sex = gender,
           district,
           time,
           labour_force = "Labour force in average per month",
           unemployed = "Unemployment in average per month") %>%
    mutate(age = sub("Over 60 years", "60+", age),
           age = sub(" years", "", age)) %>%
    mutate(sex = factor(sex, levels = c("Women", "Men"), labels = c("Female", "Male")))

saveRDS(raw,
        file = "out/raw.rds")
