
library(ggplot2)
library(tidyr)
library(dplyr)

tfr_direct <- readRDS("out/tfr_direct.rds")

tfr_modelled <- readRDS("out/tfr_modelled.rds") %>%
    pivot_wider(names_from = quantile, values_from = count)


p <- ggplot(tfr_modelled, aes(x = time)) +
    facet_wrap(vars(area)) +
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), fill = "lightskyblue1") +
    geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = "lightskyblue3") +
    geom_line(aes(y = `50%`), col = "white", size = 0.2) +
    geom_line(aes(x = time, y = count), data = tfr_direct, size = 0.2, col = "red") +
    ylim(0, NA) +
    xlab("Time") +
    ylab("Births per woman")

pdf(file = "out/fig_tfr.pdf",
    width = 6,
    height = 6)
plot(p)
dev.off()
