
library(demest)
library(demlife)
library(dplyr)

filename <- "out/model.est"

life_expectancy <- fetch(filename, where = c("model", "likelihood", "rate")) %>%
    LifeTable() %>%
    lifeExpectancy() %>%
    collapseIterations(prob = c(0.025, 0.5, 0.975)) %>%
    as.data.frame()

saveRDS(life_expectancy,
        file = "out/life_expectancy.rds")
