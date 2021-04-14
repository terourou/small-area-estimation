
library(demest)
library(dplyr)
library(tidyr)

diabetes <- readRDS("out/diabetes.rds")

population <- readRDS("out/population.rds")

prev_historical <- (diabetes / population) %>%
    as.data.frame() %>%
    mutate(age = factor(age, levels = unique(age))) %>%
    mutate(time = as.integer(time))

saveRDS(prev_historical,
        file = "out/prev_historical.rds")
