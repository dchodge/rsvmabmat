# These are from fitted erlang-3 distributions

mat_bounded_wane_par <- load(file = here::here("data", "efficacies", "mat_bounded_post_wane.RData"))
mat_bounded_wane_par <- get(mat_bounded_wane_par)
mat_nobound_wane_par <- load(file = here::here("data", "efficacies", "mat_nobound_post_wane.RData"))
mat_nobound_wane_par <- get(mat_nobound_wane_par)
nmab_bounded_wane <- load(file = here::here("data", "efficacies", "nmab_bounded_post_wane.RData"))
nmab_bounded_wane <- get(nmab_bounded_wane)
nmab_nobound_wane <- load(file = here::here("data", "efficacies", "nmab_nobound_post_wane.RData"))
nmab_nobound_wane <- get(nmab_nobound_wane)
oa_papi_bounded_wane <- load(file = here::here("data", "efficacies", "oa_papi_bounded_post_wane.RData"))
oa_papi_bounded_wane <- get(oa_papi_bounded_wane)
oa_papi_nobound_wane <- load(file = here::here("data", "efficacies", "oa_papi_nobound_post_wane.RData"))
oa_papi_nobound_wane <- get(oa_papi_nobound_wane)

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
    gp = 0.826,
    hosp = 0.941,
    a_e = 0.826,
    icu = 0.941,
    death = 0.941
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
df_eff_disease_pfizer <- data.frame(
    product = "pfi_oa",
    infection = 0.717,
    symptomatic = 0.621,
    gp = 0.667,
    hosp = 0.857,
    a_e = 0.667,
    icu = 0.857,
    death = 0.857
)

# need to check waneing model correct
df_eff_disease_gsk <- data.frame(
    product = "gsk_oa",
    infection = 0.717,
    symptomatic = 0.717,
    gp = 0.826,
    hosp = 0.941,
    a_e = 0.826,
    icu = 0.941,
    death = 0.941
)

full_info_o <- bind_rows(
    bind_rows(
        df_eff_disease_mat,
        df_eff_disease_nmab,
        df_eff_disease_pfizer,
        df_eff_disease_gsk
    ) %>% mutate(type = "efficacy"),

    bind_rows(
        c(df_eff_disease_mat[1], df_eff_disease_mat[2:8]/as.numeric(df_eff_disease_mat[2])),
        c(df_eff_disease_nmab[1], df_eff_disease_nmab[2:8]/as.numeric(df_eff_disease_nmab[2])),
        c(df_eff_disease_pfizer[1], df_eff_disease_pfizer[2:8]/as.numeric(df_eff_disease_pfizer[2])),
        c(df_eff_disease_gsk[1], df_eff_disease_gsk[2:8]/as.numeric(df_eff_disease_gsk[2]))
    ) %>% mutate(type = "multiplier")
)

### Average over time period and comparison to data

# Get proporiton protected 1 year after vaccination
point_prot <- function(day_no, efficacy_data) {
    c(
        1:4000 %>% map_dbl(~efficacy_data$wane_a_er3[.x] * (1 - pgamma(day_no, 3, 1/efficacy_data$wane_b_er3[.x])) ) %>% mean,
        1:4000 %>% map_dbl(~efficacy_data$wane_a_er3[.x] * (1 - pgamma(day_no, 3, 1/efficacy_data$wane_b_er3[.x])) ) %>% quantile(c(0.025, 0.975))
    )
}

average_prot <- function(day_no, efficacy_data) {
    c(
        1:4000 %>% map_dbl(~efficacy_data$wane_a_er3[.x] * (1 - pgamma(day_no, 3, 1/efficacy_data$wane_b_er3[.x])) %>% mean ) %>% mean,
        1:4000 %>% map_dbl(~efficacy_data$wane_a_er3[.x] * (1 - pgamma(day_no, 3, 1/efficacy_data$wane_b_er3[.x])) %>% mean) %>% quantile(c(0.025, 0.975))
    )
}

# Get data on efficacy of vaccination type for manuscript

#point_prot(365, efficacy_mat)
#point_prot(365, efficacy_mab)
#point_prot(365, efficacy_lav)


#average_prot(1:180, efficacy_mat)
#average_prot(1:150, efficacy_mab)
#average_prot(1:180, efficacy_lav)


