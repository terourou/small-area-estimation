
## Fit model predicting mean log income
## from age and highest qualification

## We are treating the data-level standard errors
## as known. The only way to do this with the
## current interface is via a hack involving
## weights. Note that the likelihood is
## y[i] ~ N(mean[i], sd^2 / weight[i])

library(demest)

income_mean <- readRDS("out/income_mean.rds")

income_sd <- readRDS("out/income_sd.rds")

weights <- Counts(1 / income_sd^2)

model <- Model(y ~ Normal(mean ~ age + sex + qual, sd = 1),
               age ~ DLM(trend = NULL, damp = NULL))


filename <- "out/model.est"

estimateModel(model = model,
              y = income_mean,
              weights = weights,
              filename = filename,
              nBurnin = 1000,
              nSim = 1000,
              nChain = 4,
              nThin = 4)
fetchSummary(filename, nSample = 100)



                                  
                    



