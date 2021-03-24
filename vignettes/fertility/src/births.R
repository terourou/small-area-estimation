
library(dembase)
library(dplyr)
library(tidyr)
library(readr)


births <- read_csv("data/VSB477104_20210324_111551_95.csv",
                   skip = 1,
                   n_max = 210) %>%
    rename(time = X1, area = X2) %>%
    fill(time) %>%
    pivot_longer(cols = -c(time, area), names_to = "age", values_to = "count") %>%
    filter(area != "Great Barrier Local Board Area") %>%
    mutate(area = sub(" Local Board Area", "", area),
           area = factor(area, levels = unique(area))) %>%
    mutate(age = sub("Under 20 years", "15-19", age),
           age = sub("40 years and over", "40-44", age),
           age = sub(" - ", "-", age),
           age = sub(" years", "", age)) %>%
    mutate(count = as.integer(count)) %>%
    dtabs(count ~ age + area + time) %>%
    Counts(dimscales = c(time = "Intervals"))

saveRDS(births,
        file = "out/births.rds")


