
library(dembase)
library(dplyr)
library(tidyr)
library(readr)

breaks_age <- c(0, 1, seq(5, 100, 5))

popn <- read_csv("data/MAN02001.csv",
                 skip = 1) %>%
    select(-Municipality) %>%
    rename(age = Age) %>%
    pivot_longer(cols = -age,
                 names_to = c("year", "sex"),
                 names_sep = " ",
                 values_to = "count") %>%
    mutate(age = sub("In 1st year", 0, age),
           age = sub(" years?", "", age),
           age = sub("109", "109+", age)) %>%
    mutate(year = as.integer(year) - 1L) %>% # because 'exposure' uses year at end to construct label
    mutate(sex = sub("s", "", sex)) %>%
    dtabs(count ~ age + sex + year) %>%
    Counts(dimscales = c(year = "Points")) %>%
    exposure() %>%
    collapseIntervals(dimension = "age", breaks = breaks_age)

saveRDS(popn,
        file = "out/popn.rds")
