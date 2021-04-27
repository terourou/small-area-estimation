create(counts) %>%
    poisson_likelihood(~ age * sex + year, exposure = totals) %>%
    prior_dlm_trend(~age) %>%
    # set_priors(age ~ DLM(), sex ~ ...) %>%
    mcmc_parameters(
        jump = 0.065
    )

#


check(big_model)
