
library(demest)

unemployed <- readRDS("out/unemployed.rds")

labour_force <- readRDS("out/labour_force.rds")

model <- Model(y ~ Binomial(mean ~ age + sex + district),
               age ~ DLM(damp = NULL),
               jump = 0.15)

filename <- "out/model.est"

set.seed(0)
estimateModel(model = model,
              y = unemployed,
              exposure = labour_force,
              filename = filename,
              nBurnin = 20000,
              nSim = 20000,
              nChain = 4,
              nThin = 80)
fetchSummary(filename)



                                  
                    



