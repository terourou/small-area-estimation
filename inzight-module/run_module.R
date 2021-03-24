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

library(demlife)
library(magrittr)

## Income example

# try(ui$close(), TRUE)
ui <- iNZGUI$new()
wd <- getwd()
if (dir.exists(file.path(getwd(), "inzight-module")))
    wd <- file.path(wd, "inzight-module")
ui$initializeGui(addonDir = wd)


## Not working:

# Error in checkAndTidyWeights(weights = weights, y = y) :
#   'weights' has missing values in places where 'y' does not

# 1. load data
# 2. filter to (income > 0)
# 3. log (e) income
# 4. rename variables
# 5. convert age_category to categorical ("age")
# 6. aggregate income_log: count, mean, sd
# 7. calculate std. err as income_se
# 8. launch module




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

# 1. load deaths (.rda, from 'out')
# 2. convert year to cat
# 3. aggregate -> sum of counts
# 4. rename count_sum -> deaths
# 5. rename dataset to deaths

# 6. load total
# 7. convert year to cat
# 8. aggregate -> sum of counts
# 9. rename count_sum -> population
# 10. rename dataset to population

# 11. choose deaths, then merge population
# 12. open module
