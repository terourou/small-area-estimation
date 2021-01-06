
library(demest)

deaths <- readRDS("out/deaths.rds")

popn <- readRDS("out/popn.rds")

model <- Model(y ~ Poisson(mean ~ age * sex + year),
               age ~ DLM(damp = NULL,
                         covariates = Covariates(infant = TRUE)),
               age:sex ~ DLM(level = Level(scale = HalfT(df = Inf, scale = 0.1)),
                             trend = NULL,
                             damp = NULL),
               year ~ DLM(damp = NULL),
               jump = 0.065)

filename <- "out/model.est"

estimateModel(model = model,
              y = deaths,
              exposure = popn,
              filename = filename,
              nBurnin = 10000,
              nSim = 10000,
              nChain = 4,
              nThin = 40)
fetchSummary(filename, nSample = 100)



                                  
                    



