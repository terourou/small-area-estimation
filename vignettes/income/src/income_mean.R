
## Format point estimates for log income
## as a Values array

library(dembase)
library(dplyr)

income_mean <- readRDS("out/income_df.rds") %>%
    dtabs(income ~ age + sex + qual) %>%
    Values()

saveRDS(income_mean,
        file = "out/income_mean.rds")

