
## Format standard errors for log income
## as a Values array

library(dembase)
library(dplyr)

income_sd <- readRDS("out/income_df.rds") %>%
    dtabs(se ~ age + sex + qual) %>%
    Values()

saveRDS(income_sd,
        file = "out/income_sd.rds")

