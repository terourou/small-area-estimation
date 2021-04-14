
library(readxl)
library(tidyr)
library(dplyr)
library(purrr)
library(forcats)
library(dembase)


data_dir <- "data/VDR Dec2005-2019 final v686"


diabetes <- dir("data/VDR Dec2005-2019 final v686") %>%
    file.path(data_dir, .) %>%
    map(read_xlsx, sheet = "AgeGrp_Eth", range = "A3:F21") %>%
    set_names(2005:2019) %>%
    bind_rows(.id = "time") %>%
    mutate(age = coalesce(`Age-group`, agegroup)) %>% ## different names used in different years
    mutate(maori = coalesce(`Mäori`, `Māori`)) %>% ## different names used in different years
    mutate(nonmaori = Total - maori) %>%
    select(time, age, maori, nonmaori) %>%
    pivot_longer(c(maori, nonmaori), names_to = "ethnicity", values_to = "count") %>%
    mutate(age = cleanAgeGroup(age)) %>%
    mutate(ethnicity = fct_recode(ethnicity, "Maori" = "maori", "Non-Maori" = "nonmaori")) %>%
    dtabs(count ~ age + ethnicity + time) %>%
    Counts(dimscales = c(time = "Points"))


saveRDS(diabetes,
        file = "out/diabetes.rds")
