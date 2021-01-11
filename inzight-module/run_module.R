### these only need installing once
# remotes::install_github('StatisticsNZ/demdata')
# remotes::install_github('StatisticsNZ/dembase')
# remotes::install_github('StatisticsNZ/demest')

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
