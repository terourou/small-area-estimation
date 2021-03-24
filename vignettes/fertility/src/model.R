
library(demest)

births <- readRDS("out/births.rds")

popn <- readRDS("out/popn.rds")

model <- Model(y ~ Poisson(mean ~ age * area + area * time),
               age ~ DLM(trend = NULL,
                         damp = NULL),
               time ~ DLM(),
               area:time ~ DLM())

filename <- "out/model.est"

estimateModel(model = model,
              y = births,
              exposure = popn,
              filename = filename,
              nBurnin = 4000,
              nSim = 4000,
              nChain = 4,
              nThin = 20)
fetchSummary(filename, nSample = 100)
