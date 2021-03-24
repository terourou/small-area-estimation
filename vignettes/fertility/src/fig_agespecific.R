
library(ggplot2)
library(tidyr)
library(dplyr)

## there are two many times, so we keep 4
times_keep <- c(2011, 2015, 2020, 2025)

## there are two many local boards to show in one graph, so we randomly select 5
local_boards_keep <- readRDS("out/agespecific_direct.rds") %>%
    pull(area) %>%
    unique() %>%
    sample(size = 5)

agespecific_direct <- readRDS("out/agespecific_direct.rds") %>%
    filter(time %in% times_keep,
           area %in% local_boards_keep)

agespecific_modelled <- readRDS("out/agespecific_modelled.rds") %>%
    pivot_wider(names_from = quantile, values_from = value) %>%
    filter(time %in% times_keep,
           area %in% local_boards_keep)


p <- ggplot(agespecific_modelled, aes(x = age)) +
    facet_grid(vars(area), vars(time)) +
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), fill = "lightskyblue1") +
    geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = "lightskyblue3") +
    geom_line(aes(y = `50%`), col = "white", size = 0.2) +
    geom_line(aes(x = age, y = value), data = agespecific_direct, size = 0.2, col = "red") +
    xlab("Age") +
    ylab("Rate")


pdf(file = "out/fig_agespecific.pdf",
    width = 6,
    height = 6)
plot(p)
dev.off()
