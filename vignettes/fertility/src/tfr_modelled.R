
library(demest)
library(dplyr)

tfr_modelled <- fetchBoth(filenameEst = "out/model.est",
                          filenamePred = "out/forecast.pred",
                          where = c("model", "likelihood", "rate")) %>%
    tfr() %>%
    collapseIterations(prob = c(0.025, 0.25, 0.5, 0.75, 0.975)) %>%
    as.data.frame()

saveRDS(tfr_modelled,
        file = "out/tfr_modelled.rds")
