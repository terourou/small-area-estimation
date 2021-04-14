
library(ggplot2)
library(tidyr)
library(dplyr)


selected_ages <- c("30-34", "40-44", "50-54", "60-64", "70-74", "80-84")

prev_historical <- readRDS("out/prev_historical.rds") %>%
    filter(age %in% selected_ages)

prev_forecast <- readRDS("out/prev_forecast.rds") %>%
    pivot_wider(names_from = quantile, values_from = value) %>%
    filter(age %in% selected_ages)

p <- ggplot(prev_historical, aes(x = time)) +
    facet_grid(vars(ethnicity), vars(age)) +
    geom_line(aes(y = value),
              col = "black",
              size = 0.3) +
    geom_ribbon(aes(ymin = `5%`, ymax = `95%`),
                data = prev_forecast,
                fill = "lightblue") +
    geom_ribbon(aes(ymin = `25%`, ymax = `75%`),
                data = prev_forecast,
                fill = "blue") +
    geom_line(aes(y = `50%`),
              data = prev_forecast,
              col = "white",
              size = 0.3) +
    xlab("Time") +
    ylab("Prevalence")


pdf(file = "out/fig_prevalence.pdf",
    width = 8,
    height = 5)
plot(p)
dev.off()
