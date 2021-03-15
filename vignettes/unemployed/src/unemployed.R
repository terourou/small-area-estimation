
library(dembase)
library(dplyr)

## for simplicity, taking only a single year

unemployed <- readRDS("out/raw.rds") %>%
    filter(time == 2019) %>%
    dtabs(unemployed ~ age + sex + district) %>%
    Counts()

saveRDS(unemployed,
        file = "out/unemployed.rds")
