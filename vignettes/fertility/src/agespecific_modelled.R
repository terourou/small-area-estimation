
library(demest)
library(magrittr)

agespecific_modelled <- fetchBoth(filenameEst = "out/model.est",
                                  filenamePred = "out/forecast.pred",
                                  where = c("model", "likelihood", "rate")) %>%
    multiply_by(1000) %>%
    collapseIterations(prob = c(0.025, 0.25, 0.5, 0.75, 0.975)) %>%
    as.data.frame(midpoints = "age")

saveRDS(agespecific_modelled,
        file = "out/agespecific_modelled.rds")
