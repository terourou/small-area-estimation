
library(demest)
library(magrittr)

rate_modelled <- fetch(filename = "out/model.est",
                       where = c("model", "likelihood", "prob")) %>%
    multiply_by(100) %>%
    collapseIterations(prob = c(0.025, 0.5, 0.975)) %>%
    as.data.frame(midpoints = "age")

saveRDS(rate_modelled,
        file = "out/rate_modelled.rds")
