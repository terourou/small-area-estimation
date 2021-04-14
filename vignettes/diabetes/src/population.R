
library(readr)
library(tidyr)
library(dplyr)
library(forcats)
library(dembase)


maori <- read_csv("data/DPE479901_20210414_110618_25.csv",
                  skip = 2,
                  n_max = 15) %>%
    rename(time = X1) %>%
    pivot_longer(-time, names_to = "age", values_to = "maori")


total <- read_csv("data/DPE403903_20210414_110734_1.csv",
                  skip = 3,
                  n_max = 15) %>%
    rename(time = X1) %>%
    pivot_longer(-time, names_to = "age", values_to = "total")


population <- left_join(maori, total, by = c("time", "age")) %>%
    mutate(age = cleanAgeGroup(age)) %>%
    mutate(time = as.integer(time) - 1L) %>% ## using mean year to June as proxy for 31 December count
    mutate(nonmaori = total - maori) %>%
    select(-total) %>%
    pivot_longer(cols = c(maori, nonmaori), names_to = "ethnicity", values_to = "count") %>%
        mutate(ethnicity = fct_recode(ethnicity, "Maori" = "maori", "Non-Maori" = "nonmaori")) %>%
    dtabs(count ~ age + ethnicity + time) %>%
    Counts(dimscales = c(time = "Points"))


saveRDS(population,
        file = "out/population.rds")


