
## Get point estimates, and associated standard errors,
## for log income by age, sex, and highest qualification
## Restrict to people with income > 0

library(readxl)
library(dplyr)
library(tidyr)
library(survey)


income_df <- read_xlsx("data/NZIS-Sub-SURF1.xlsx") %>%
    filter((weekly_income > 0)) %>%
    mutate(age = paste(age_category, age_category + 4, sep = "-"),
           age = factor(age)) %>%
    mutate(sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male"))) %>%
    rename(qual = highest_qualification) %>%
    mutate(income = log(weekly_income)) %>%
    svydesign(id = ~ 1, data = .) %>%
    svyby(formula = ~ income,
          by = ~ interaction(age, sex, qual),
          design = .,
          FUN = svymean) %>%
    as.data.frame() %>%
    separate(col = "interaction(age, sex, qual)",
             into = c("age", "sex", "qual"),
             sep = "\\.") %>%
    mutate(qual = factor(qual,
                         levels = c("no qual",
                                    "school",
                                    "post-school",
                                    "vocational/trade",
                                    "bachelor or higher")))

saveRDS(income_df,
        file = "out/income_df.rds")


