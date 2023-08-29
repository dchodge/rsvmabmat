# These are from fitted erlang-3 distributions

mat_bounded_wane_par <- readRDS(file = here::here("data", "efficacies", "mat_bounded_wane.rds"))
mat_nobound_wane_par <- readRDS(file = here::here("data", "efficacies", "mat_nobound_wane.rds"))
nmab_bounded_wane <- readRDS(file = here::here("data", "efficacies", "nmab_bounded_wane.rds"))
nmab_nobound_wane <- readRDS(file = here::here("data", "efficacies", "nmab_nobound_wane.rds"))
oa_papi_bounded_wane <- readRDS(file = here::here("data", "efficacies", "oa_papi_bounded_wane.rds"))
oa_papi_nobound_wane <- readRDS(file = here::here("data", "efficacies", "oa_papi_nobound_wane.rds"))

df_eff_disease_mat <- data.frame(
    product = "mat",
    infection = 0.513,
    symptomatic = 0.513,
    gp = 0.513,
    hosp = 0.694,
    a_e = 0.694,
    icu = 0.694,
    death = 0.694
)

# need to check waneing model correct
df_eff_disease_nmab <- data.frame(
    product = "mab",
    infection = 0.795,
    symptomatic = 0.795,
    gp = 0.795,
    hosp = 0.795,
    a_e = 0.795,
    icu = 0.860,
    death = 0.860
)

# need to check waneing model correct
df_eff_disease_papi <- data.frame(
    product = "lav",
    infection = 0.717,
    symptomatic = 0.717,
    gp = 0.717,
    hosp = 0.94,
    a_e = 0.717,
    icu = 0.94,
    death = 0.94
)

disease_eff_values <- bind_rows(df_eff_disease_mat, df_eff_disease_nmab, df_eff_disease_papi)

immune_profile_none <- list(
    mass = list(product = "none"),
    
    vhr = list(
        product = "none"),
    disease_eff = disease_eff_values,
    direct = FALSE
)

## Maternal protection, exponential waning for infants and erlang-2 for adults 
# `base` is bounded protection, slight underestimation in year 1, no protection in year 2
immune_profile_mat_base <- list(
    mass = list(
        product = "mat",
        wane_function = "a * exp(b * t))",  a = oa_papi_bounded_wane$wane_a_er3, b = 1/oa_papi_bounded_wane$wane_b_er3,
        sero_delay = "flu_like",
        gest_age_devilery_wks = "26 34",
        wane_function_neonate = "a * exp(b * t))", a_mat = mat_bounded_wane_par$wane_a_er3, b_mat =  1/mat_bounded_wane_par$wane_b_er3
    ),
    vhr = list(
        product = "pal",
        wane_function = "a * exp(b * t))", a = 0.7, b = 1 / 60,
        sero_delay = "none"
    ),
    disease_eff = disease_eff_values,
    direct = FALSE
)


# m-year is unbounded protection, better estimates for year 1, beut protection exists in second season
immune_profile_mat_unbound <- list(
    mass = list(
        product = "mat",
        wane_function = "a * exp(b * t))",  a = oa_papi_nobound_wane$wane_a_er3, b =  1/oa_papi_nobound_wane$wane_b_er3,
        sero_delay = "flu_like",
        gest_age_devilery_wks = "26 34",
        wane_function_neonate = "a * exp(b * t))", a_mat = mat_nobound_wane_par$wane_a_er3, b_mat =  1/mat_nobound_wane_par$wane_b_er3
    ),
    vhr = list(
        product = "pal",
        wane_function = "a * exp(b * t))", a = 0.7, b = 1 / 60,
        sero_delay = "none"
    ),
    disease_eff = disease_eff_values,
    direct = FALSE
)

## Nmab protection, erlang-3 is best
# `base` is bounded protection, slight underestimation in year 1, no protection in year 2
immune_profile_mab_base <- list(
    mass = list(
        product = "mab",
        wane_function = "a * exp(b * t))",  a = nmab_bounded_wane$wane_a_er3, b =  1/nmab_bounded_wane$wane_b_er3,
        sero_delay = "none"
    ),
    vhr = list(
        product = "pal",
        wane_function = "a * exp(b * t))", a = 0.7, b = 1 / 60,
        sero_delay = "none"
    ),
    disease_eff = disease_eff_values,
    direct = FALSE
)

# m-year is unbounded protection, better estimates for year 1, beut protection exists in second season
immune_profile_mab_unbound <- list(
    mass = list(
        product = "mab",
        wane_function = "a * exp(b * t))",  a = nmab_nobound_wane$wane_a_er3, b =  1/nmab_nobound_wane$wane_b_er3,
        sero_delay = "none"
    ),
    vhr = list(
        product = "pal",
        wane_function = "a * exp(b * t))", a = 0.7, b = 1 / 60,
        sero_delay = "none"
    ),
    disease_eff = disease_eff_values,
    direct = FALSE
)

## Nmab protection, erlang-3 is best
# `base` is bounded protection, slight underestimation in year 1, no protection in year 2
immune_profile_lav_base <- list(
    mass = list(
        product = "lav",
        wane_function = "a * exp(b * t))",  a = oa_papi_bounded_wane$wane_a_er3, b =  1/oa_papi_bounded_wane$wane_b_er3,
        sero_delay = "none"
    ),
    vhr = list(
        product = "pal",
        wane_function = "a * exp(b * t))", a = 0.7, b = 1 / 60,
        sero_delay = "none"
    ),
    disease_eff = disease_eff_values,
    direct = FALSE
)

# m-year is unbounded protection, better estimates for year 1, beut protection exists in second season
immune_profile_lav_unbound <- list(
    mass = list(
        product = "lav",
        wane_function = "a * exp(b * t))",  a = oa_papi_nobound_wane$wane_a_er3, b =  1/oa_papi_nobound_wane$wane_b_er3,
        sero_delay = "none"
    ),
    vhr = list(
        product = "pal",
        wane_function = "a * exp(b * t))", a = 0.7, b = 1 / 60,
        sero_delay = "none"
    ),
    disease_eff = disease_eff_values,
    direct = FALSE
)

# Get very high risk with mabs programmes mat
immune_profile_mat_base_vhr <- immune_profile_mat_base
immune_profile_mat_unbound_vhr <- immune_profile_mat_unbound

immune_profile_mat_base_vhr$vhr <- immune_profile_mab_base$mass
immune_profile_mat_unbound_vhr$vhr <- immune_profile_mab_unbound$mass

# Get very high risk with mabs programmes ,mab
immune_profile_mab_base_vhr <- immune_profile_mab_base
immune_profile_mab_unbound_vhr <- immune_profile_mab_unbound

immune_profile_mab_base_vhr$vhr <- immune_profile_mab_base$mass
immune_profile_mab_unbound_vhr$vhr <- immune_profile_mab_unbound$mass


immune_profiles_bounded <- list(
    none = immune_profile_none,
    mabs_vhr = immune_profile_mab_base_vhr,
    mat_vhr = immune_profile_mat_base_vhr,
    mabs = immune_profile_mab_base,
    mat = immune_profile_mat_base,
    lav = immune_profile_lav_base
)

immune_profiles_unbound <- list(
    none = immune_profile_none,
    mabs_vhr = immune_profile_mab_unbound_vhr,
    mat_vhr = immune_profile_mat_unbound_vhr,
    mabs = immune_profile_mab_unbound,
    mat = immune_profile_mat_unbound,
    lav = immune_profile_lav_unbound
)

saveRDS(immune_profiles_bounded, here::here("data", "efficacies", "immune_profiles_bounded.RDS"))
saveRDS(immune_profiles_unbound, here::here("data", "efficacies", "immune_profiles_unbound.RDS"))