
library(demest)
library(dplyr)

unemployed <- fetch(filename = "out/model.est",
                    where = "y")

labour_force <- fetch(filename = "out/model.est",
                      where = "exposure")

rate_direct <- (100 * unemployed / labour_force) %>%
    as.data.frame(midpoints = "age")

saveRDS(rate_direct,
        file = "out/rate_direct.rds")
