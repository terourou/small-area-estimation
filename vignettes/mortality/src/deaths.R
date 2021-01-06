
library(dembase)
library(dplyr)
library(tidyr)
library(readr)

breaks_age <- c(0, 1, seq(5, 100, 5))

deaths <- read_csv("data/MAN05221.csv",
                   skip = 1) %>%
    pivot_longer(cols = c(Males, Females), names_to = "sex", values_to = "count") %>%
    select(age = Age, sex, year = Year, count) %>%
    mutate(age = sub("On 1st year", 0, age),
           age = sub(" years?", "", age),
           age = sub("109", "109+", age)) %>%
    mutate(sex = sub("s", "", sex)) %>%
    dtabs(count ~ age + sex + year) %>%
    Counts(dimscales = c(year = "Intervals")) %>%
    collapseIntervals(dimension = "age", breaks = breaks_age)

saveRDS(deaths,
        file = "out/deaths.rds")


