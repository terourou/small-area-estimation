### these only need installing once
# remotes::install_github('StatisticsNZ/demdata')
# remotes::install_github('StatisticsNZ/dembase')
# remotes::install_github('StatisticsNZ/demest')
# remotes::install_github('StatisticsNZ/demlife')

### but these might need updating once in a while
# remotes::install_github("iNZightVIT/iNZight@dev")
# remotes::install_github("iNZightVIT/iNZightModules@dev")

# devtools::load_all("~/iNZight/iNZight")
library(iNZight)

# try(ui$close(), TRUE)
ui <- iNZGUI$new()
wd <- getwd()
ui$initializeGui(addonDir = wd)

nzincome <- demdata::nz.income
ui$initializeGui(nzincome, addonDir = wd)



# 1. load data
library(iNZightTools)

deaths <- iNZightTools::smart_read('../vignettes/mortality/data/MAN05221.csv') %>%
    dplyr::mutate(Age = ifelse(grepl("1st", as.character(Age)), 0, as.character(Age))) %>%
    dplyr::mutate(Age = gsub("[^0-9]", "", Age)) %>%
    dplyr::mutate(Age = cut(
        as.integer(Age),
        breaks = c(0, 1, seq(5, 100, 5), Inf),
        right = FALSE,
        include.lowest = TRUE,
        labels = c(0, paste0(c(1, seq(5, 95, 5)), "-", c(seq(4, 99, 5))), "100+")
    )) %>%
    dplyr::mutate(Age = gsub("\\[|\\(|\\]|\\)", "", Age)) %>%
    dplyr::mutate(Age = as.factor(gsub(",", "-", Age, fixed = TRUE)))

pop <- iNZightTools::smart_read("../vignettes/mortality/data/MAN02001.csv") %>%
    dplyr::mutate(Age = ifelse(grepl("1st", as.character(Age)), 0, as.character(Age))) %>%
    dplyr::mutate(Age = gsub("[^0-9]", "", Age)) %>%
    dplyr::mutate(Age = cut(
        as.integer(Age),
        breaks = c(0, 1, seq(5, 100, 5), Inf),
        right = FALSE,
        include.lowest = TRUE,
        labels = c(0, paste0(c(1, seq(5, 95, 5)), "-", c(seq(4, 99, 5))), "100+")
    )) %>%
    dplyr::mutate(Age = gsub("\\[|\\(|\\]|\\)", "", Age)) %>%
    dplyr::mutate(Age = as.factor(gsub(",", "-", Age, fixed = TRUE)))

write.csv(deaths, "deaths.csv", quote = FALSE, row.names = FALSE)
write.csv(pop, "total.csv", quote = FALSE, row.names = FALSE)

### Mortality example
ui <- iNZGUI$new()
ui$initializeGui(addonDir = wd)
