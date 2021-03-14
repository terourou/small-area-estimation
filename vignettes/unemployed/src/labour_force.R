
library(dembase)
library(dplyr)

## for simplicity, taking only a single year

labour_force <- readRDS("out/raw.rds") %>%
    filter(time == 2019) %>%
    dtabs(labour_force ~ age + sex + district) %>%
    Counts()

saveRDS(labour_force,
        file = "out/labour_force.rds")
