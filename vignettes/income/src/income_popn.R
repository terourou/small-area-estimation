
## Extract estimates of log income for whole population
## (as opposed to respondents to the survey).
## Convert back to original scale


library(demest)
library(dplyr)

filename <- "out/model.est"

income_popn <- fetch(filename, where = c("model", "likelihood", "mean")) %>%
    ## converting back to original units - can be omitted
    ## if difficult to do with current interface
    exp() %>%
    collapseIterations(prob = c(0.025, 0.5, 0.975)) %>%
    as.data.frame(midpoints = "age", stringsAsFactors = TRUE)

saveRDS(income_popn,
        file = "out/income_popn.rds")
