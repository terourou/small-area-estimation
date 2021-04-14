
library(demest)
library(dplyr)
library(magrittr)
library(tidyr)

## use fetchBoth to get last historical year

last_historical <- readRDS("out/diabetes.rds") %>%
    dimnames() %>%
    extract2("time") %>%
    max()
    
prev_forecast <- fetchBoth(filenameEst = "out/model.est",
                           filenamePred = "out/forecast.pred",
                           where = c("model", "likelihood", "prob")) %>%
    subarray(time >= last_historical) %>%
    collapseIterations(prob = c(0.05, 0.25, 0.5, 0.75, 0.95)) %>%
    as.data.frame() %>%
    mutate(age = factor(age, levels = unique(age))) %>%
    mutate(time = as.integer(time))

saveRDS(prev_forecast,
        file = "out/prev_forecast.rds")
