
library(demest)

guestnights <- readRDS("out/guestnights.rds")

model <- Model(y ~ Poisson(mean ~ region * time,
                           useExpose = FALSE),
               time ~ DLM(damp = NULL,
                          season = Season(n = 12)),
               region:time ~ DLM(trend = NULL,
                                 damp = NULL,
                                 season = Season(n = 12)))

filename <- "out/model.est"

set.seed(0)
estimateModel(model = model,
              y = guestnights,
              filename = filename,
              nBurnin = 1000,
              nSim = 1000,
              nChain = 4,
              nThin = 5)
fetchSummary(filename)
