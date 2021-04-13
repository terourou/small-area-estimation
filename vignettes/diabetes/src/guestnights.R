
library(readr)
library(dplyr)
library(tidyr)
library(dembase)

guestnights <- read_csv("data/ACS348801_20210324_065628_96.csv",
                         skip = 1,
                         n_max = 145) %>%
    slice(-1) %>%
    rename(yearmonth = X1) %>%
    pivot_longer(cols = -yearmonth, names_to = "region", values_to = "count") %>%
    separate(col = yearmonth, into = c("year", "month"), sep = "M") %>%
    mutate(time = as.integer(month) + 12L * as.integer(year)) %>%
    mutate(region = factor(region, levels = unique(region))) %>%
    mutate(count = as.integer(count)) %>%
    dtabs(count ~ region + time) %>%
    Counts(dimscales = c(time = "Intervals"))

saveRDS(guestnights,
        file = "out/guestnights.rds")
