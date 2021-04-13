
library(demest)
library(dplyr)
library(tidyr)

guestnights_forecasted <- fetch(filename = "out/forecast.pred",
                         where = c("model", "likelihood", "count")) %>%
    collapseIterations(prob = c(0.025, 0.5, 0.975)) %>%
    as.data.frame()

saveRDS(guestnights_forecasted,
        file = "out/guestnights_forecasted.rds")
