
library(demest)
library(dplyr)

births <- fetch(filename = "out/model.est",
                where = "y")

popn <- fetch(filename = "out/model.est",
              where = "exposure")

tfr_direct <- (births / popn) %>%
    tfr() %>%
    as.data.frame()

saveRDS(tfr_direct,
        file = "out/tfr_direct.rds")
