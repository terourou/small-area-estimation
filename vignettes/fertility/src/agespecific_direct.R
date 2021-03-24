
library(demest)
library(dplyr)

births <- fetch(filename = "out/model.est",
                where = "y")

popn <- fetch(filename = "out/model.est",
              where = "exposure")

agespecific_direct <- (1000 * births / popn) %>%
    as.data.frame(midpoints = "age")

saveRDS(agespecific_direct,
        file = "out/agespecific_direct.rds")
