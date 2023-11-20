get_averted_df <- function(base, interventions) {
    recode_age <- c("1" = "<1 month", "2" = "1 month", "3" = "2 month", "4" = "3 month", "5" = "4 month",
    "6" = "5 month", "7" = "6 month", "8" = "7 month", "9" = "8 month", "10" = "9 month", "11" = "10 month",
    "12" = "11 month", "13" = "1 year", "14" = "2 year", "15" = "3 years", "16" = "4 years", 
    "17" = "5+ years", "18" = "5+ years", "19" = "5+ years", "20" = "5+ years", "21" = "5+ years",
    "22" = "5+ years", "23" = "5+ years", "24" = "5+ years", "25" = "5+ years")
    relabel_outcomes <- c("symptomatic" = "Symptomatic cases", "gp" = "GP consultations", 
        "hosp" = "Hospital cases", "icu" = "ICU admissions", "a_e" = "A+E visits", "death" = "Deaths")
    RSV_impact <- interventions %>%
        left_join(base %>% rename(case_total_base = cases_total), by = c("s", "outcome", "age_group")) %>%
        mutate(total_case_averted = case_total_base - cases_total) %>% mutate(prop_cases_averted = (case_total_base - cases_total) / case_total_base)  %>% mutate(age_group = recode(age_group, !!!recode_age)) %>%
        mutate(age_group = factor(age_group, levels = unique(recode_age))) %>%
        mutate(outcome = recode(outcome, !!!relabel_outcomes)) %>%
        mutate(outcome = factor(outcome, levels = unique(relabel_outcomes)))
    RSV_impact
}


load_data_fig2 <- function() {

    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_pal.RData")) # RSV_mat_vhr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_vhr.RData")) # RSV_mat_vhr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_s.RData")) # RSV_mat_s
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_yr.RData")) # RSV_mat_yr

    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_pal.RData")) # RSV_mab_vhr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_vhr.RData")) # RSV_mab_vhr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_s.RData"))  # RSV_mab_s
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_s_cu.RData"))  # RSV_mab_s
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_yr.RData"))  # RSV_mab_s

    RSV_mat_pal_sum <- RSV_mat_pal %>% summarise_outcomes
    RSV_mat_vhr_sum <- RSV_mat_vhr %>% summarise_outcomes
    RSV_mat_s_sum <- RSV_mat_s %>% summarise_outcomes
    RSV_mat_yr_sum <- RSV_mat_yr %>% summarise_outcomes

    RSV_mab_pal_sum <- RSV_mab_pal %>% summarise_outcomes
    RSV_mab_vhr_sum <- RSV_mab_vhr %>% summarise_outcomes
    RSV_mab_s_sum <- RSV_mab_s %>% summarise_outcomes
    RSV_mab_s_cu_sum <- RSV_mab_s_cu %>% summarise_outcomes
    RSV_mab_yr_sum <- RSV_mab_yr %>% summarise_outcomes

    RSV_mat_compare <- bind_rows(
        get_averted_df(RSV_mat_pal_sum, RSV_mat_s_sum) %>% mutate(intervention = "Seasonal maternal"),
        get_averted_df(RSV_mat_pal_sum, RSV_mat_yr_sum) %>% mutate(intervention = "Year-round maternal")
    )

    RSV_mab_compare <- bind_rows(
        get_averted_df(RSV_mat_pal_sum, RSV_mab_s_sum) %>% mutate(intervention = "Seasonal la-mAB"),
        get_averted_df(RSV_mat_pal_sum, RSV_mab_s_cu_sum) %>% mutate(intervention = "Seasonal la-mAB with\n annual catch-up"),
        get_averted_df(RSV_mat_pal_sum, RSV_mab_yr_sum) %>% mutate(intervention = "Year-round la-mAB")
    )
    list(mat = RSV_mat_compare, mab = RSV_mab_compare)
}

plot_fig2 <- function(df_fig2) {

    df_fig2$mat %>% bind_rows(df_fig2$mab) %>%
        mutate(intervention = factor(intervention, levels = c("Seasonal maternal", "Year-round maternal", 
            "Seasonal la-mAB", "Seasonal la-mAB with\n annual catch-up",  "Year-round la-mAB" ))) %>%
        ggplot() + 
            stat_lineribbon(aes(x = age_group, y = prop_cases_averted, fill = intervention), alpha = 0.5, .width = 0.95) + 
            facet_grid(rows = vars(outcome), cols = vars(intervention)) + theme_bw() + 
            scale_fill_manual(values = c("#0d4fb2", "#91BAd6", "#c07002", "#fcae44", "#f0de16"))  + 
            labs(x = "Age group", y = "Proportional reduction in cases per age group") + 
            geom_hline(yintercept = 0, color = "gray30") +
            guides(fill = "none") + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 13), text = element_text(size = 16)) 
}
