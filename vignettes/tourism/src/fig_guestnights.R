
library(ggplot2)
library(tidyr)
library(dplyr)

guestnights <- readRDS("out/guestnights.rds") %>%
    as.data.frame()

guestnights_forecasted <- readRDS("out/guestnights_forecasted.rds") %>%
    pivot_wider(names_from = quantile, values_from = count)

p <- ggplot(guestnights_forecasted, aes(x = time)) +
    facet_wrap(vars(region)) +
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), fill = "lightblue") +
    geom_line(aes(y = `50%`), col = "darkblue", size = 0.2) +
    geom_line(aes(x = time, y = count), data = guestnights, size = 0.2) +
    xlab("Time") +
    ylab("Nights")


pdf(file = "out/fig_guestnights.pdf",
    width = 7,
    height = 8)
plot(p)
dev.off()
