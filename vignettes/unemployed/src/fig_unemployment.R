
library(ggplot2)
library(tidyr)
library(dplyr)

rate_modelled <- readRDS("out/rate_modelled.rds") %>%
    pivot_wider(names_from = quantile, values_from = value) %>%
    filter(sex == "Male")

rate_direct <- readRDS("out/rate_direct.rds") %>%
    filter(sex == "Male")


p <- ggplot(rate_modelled, aes(x = age)) +
    facet_wrap(vars(district)) +
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), fill = "lightblue") +
    geom_line(aes(y = `50%`), col = "darkblue") +
    geom_line(aes(x = age, y = value), data = rate_direct, size = 0.5) +
    xlab("Age") +
    ylab("Rate")


pdf(file = "out/fig_unemployment.pdf",
    width = 6,
    height = 6)
plot(p)
dev.off()
