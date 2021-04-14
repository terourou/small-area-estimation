
library(demest)

diabetes <- readRDS("out/diabetes.rds")

population <- readRDS("out/population.rds")

model <- Model(y ~ Binomial(mean ~ age * ethnicity + age * time + ethnicity * time),
               age ~ DLM(trend = NULL,
                         damp = NULL),
               age:ethnicity ~ DLM(trend = NULL,
                                   damp = NULL),
               time ~ DLM(),
               age:time ~ DLM(),
               ethnicity:time ~ DLM(trend = NULL),
               jump = 0.045)

filename <- "out/model.est"

set.seed(0)
estimateModel(model = model,
              y = diabetes,
              exposure = population,
              filename = filename,
              nBurnin = 5000,
              nSim = 5000,
              nChain = 4,
              nThin = 20)
fetchSummary(filename)



                                  
                    



