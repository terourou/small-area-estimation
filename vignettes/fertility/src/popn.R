
library(dembase)
library(dplyr)
library(readr)

popn <- read_csv("data/Subnational population estimates (TA/TABLECODE7980_Data_d1066377-5e47-404a-a1a5-64b21615c99e.csv") %>%
    select(area = Area, age = Age, time = "Year at 30 June", count = Value) %>%
    filter(time >= 2011) %>%
    mutate(age = sub(" Years", "", age)) %>%
    filter(area != "Aotea/Great Barrier local board area") %>%
    mutate(area = sub(" local board area", "", area),
           area = factor(area, levels = unique(area))) %>%
    mutate(count = as.integer(count)) %>%
    dtabs(count ~ age + area + time) %>%
    Counts(dimscales = c(time = "Intervals"))


saveRDS(popn,
        file = "out/popn.rds")
