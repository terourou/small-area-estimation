
library(ggplot2)
library(tidyr)

data <- readRDS("out/life_expectancy.rds") %>%
    pivot_wider(id_cols = c(sex, year), names_from = quantile)

p <- ggplot(data, aes(x = year)) +
    facet_wrap(vars(sex)) +
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), fill = "lightblue") +
    geom_line(aes(y = `50%`), col = "darkblue") +
    xlab("Year") +
    ylab("Life expectancy at birth (years)")


pdf(file = "out/fig_life_expectancy.pdf",
    width = 6,
    height = 4)
plot(p)
dev.off()
