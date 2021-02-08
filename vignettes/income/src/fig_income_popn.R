
## Point estimates and 95% credible intervals for
## mean income, by age and sex, in the whole population

library(ggplot2)
library(tidyr)

data <- readRDS("out/income_popn.rds") %>%
    pivot_wider(id_cols = c(age, sex, qual), names_from = quantile)

p <- ggplot(data, aes(x = age)) +
    facet_grid(vars(sex), vars(qual)) +
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), fill = "lightblue") +
    geom_line(aes(y = `50%`), col = "darkblue") +
    xlab("Age") +
    ylab("Income")


pdf(file = "out/fig_income_popn.pdf",
    width = 6,
    height = 4)
plot(p)
dev.off()
