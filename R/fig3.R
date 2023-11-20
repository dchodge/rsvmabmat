plot_fig3a <- function(df_fig2) {
    source(here::here("R", "utils.R"))

    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_pal.RData")) # RSV_mat_vhr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_vhr.RData")) # RSV_mat_vhr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_s.RData")) # RSV_mat_s
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_yr.RData")) # RSV_mat_yr

    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_pal.RData")) # RSV_mab_vhr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_vhr.RData")) # RSV_mab_vhr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_s.RData"))  # RSV_mab_s
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_s_cu.RData"))  # RSV_mab_s
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_yr.RData"))  # RSV_mab_s

    joiner_doses_mat <- bind_rows(
            get_doses(RSV_mat_s, "Seasonal maternal", "mat", 1, FALSE),
            get_doses(RSV_mat_yr, "Year-round maternal", "mat", 1, FALSE)
    ) %>% rename(intervention = scenario)
    RSV_mat_compare_eff <- df_fig2$mat %>% left_join(joiner_doses_mat) %>% mutate(total_cases_averted_dose = total_case_averted / doses_mat) 
    
    joiner_doses_mab <- bind_rows(
            get_doses(RSV_mab_s, "Seasonal la-mAB", "mAB", 1, FALSE),
            get_doses(RSV_mab_s_cu, "Seasonal la-mAB with\n annual catch-up", "mAB", 1, FALSE),
            get_doses(RSV_mab_yr, "Year-round la-mAB", "mAB", 1, FALSE)
    ) %>% rename(intervention = scenario)
    RSV_mab_compare_eff <- df_fig2$mab %>% left_join(joiner_doses_mab) %>% mutate(total_cases_averted_dose = total_case_averted / doses_mab) 

    RSV_mat_compare_eff_sum <- RSV_mat_compare_eff %>% ungroup %>% summarise(total_case_averted = sum(total_case_averted), doses = mean(doses_mat), .by = c("s", "outcome", "intervention"))
    RSV_mab_compare_eff_sum <- RSV_mab_compare_eff %>% ungroup %>% summarise(total_case_averted = sum(total_case_averted), doses = mean(doses_mab), .by = c("s", "outcome", "intervention"))

    p_eff_df <- bind_rows(
        RSV_mat_compare_eff_sum,
        RSV_mab_compare_eff_sum
    ) %>% 
        mutate(intervention = factor(intervention, levels = c("Seasonal maternal", "Year-round maternal", 
            "Seasonal la-mAB", "Seasonal la-mAB with\n annual catch-up",  "Year-round la-mAB" ))) %>%
            mutate(vaccine_avert = 1 / (total_case_averted / doses)) 

    # Need for manuscript
    p_eff_df %>% group_by(outcome, intervention) %>% mean_qi(vaccine_avert) %>% as.data.frame

    p_eff_df %>%
        ggplot() + 
            stat_pointinterval(aes(x = intervention, y = vaccine_avert, fill = intervention), shape = 21, point_size = 5, position = position_dodge(0.5)) + 
            labs(y = "Number needed to vaccinate", x = "Intervention programme", fill = "Intervention") +
            scale_fill_manual(values = c("#0d4fb2", "#91BAd6", "#c07002", "#fcae44", "#f0de16"))  + 
            facet_grid(rows = vars(outcome), scales = "free_y") + guides(fill = "none") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 18)) + theme_bw()
}



plot_fig3bc <- function() {
    source(here::here("R", "utils.R"))

    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_pal.RData")) # RSV_mat_vhr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_vhr.RData")) # RSV_mat_vhr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_s.RData")) # RSV_mat_s
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_yr.RData")) # RSV_mat_yr

    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_pal.RData")) # RSV_mab_vhr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_vhr.RData")) # RSV_mab_vhr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_s.RData"))  # RSV_mab_s
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_s_cu.RData"))  # RSV_mab_s
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_yr.RData"))  # RSV_mab_s


    summarise_qalys <- function(x) {
        x@outcomes$qaly %>% ungroup %>% select(s, total) %>% unique
    }

    get_qaly_averted <- function(base, intervention, name_string) {
        intervention %>%
            left_join(base %>% rename(total_base = total), by = join_by(s)) %>%
            mutate(qaly_averted = total_base - total) %>%  mutate(intervention = name_string)

    }

    RSV_mat_pal_sum <- RSV_mat_pal %>% summarise_qalys
    RSV_mat_s_sum <- RSV_mat_s %>% summarise_qalys
    RSV_mat_yr_sum <- RSV_mat_yr %>% summarise_qalys

    RSV_mab_pal_sum <- RSV_mab_pal %>% summarise_qalys
    RSV_mab_s_sum <- RSV_mab_s %>% summarise_qalys
    RSV_mab_s_cu_sum <- RSV_mab_s_cu %>% summarise_qalys
    RSV_mab_yr_sum <- RSV_mab_yr %>% summarise_qalys

    require(scales)

    plot_qalys <- bind_rows(
        get_qaly_averted(RSV_mat_pal_sum, RSV_mat_s_sum, "Seasonal maternal"),
        get_qaly_averted(RSV_mat_pal_sum, RSV_mat_yr_sum, "Year-round maternal"),
        get_qaly_averted(RSV_mat_pal_sum, RSV_mab_s_sum, "Seasonal la-mAB"),
        get_qaly_averted(RSV_mat_pal_sum, RSV_mab_s_cu_sum, "Seasonal la-mAB with\n annual catch-up"),
        get_qaly_averted(RSV_mat_pal_sum, RSV_mab_yr_sum, "Year-round la-mAB")
    ) %>% 
        mutate(intervention = factor(intervention, levels = c("Seasonal maternal", "Year-round maternal", 
            "Seasonal la-mAB", "Seasonal la-mAB with\n annual catch-up",  "Year-round la-mAB" )))

    # Manuscript
    plot_qalys %>% summarise(get_mean_95ci(qaly_averted) , .by = c("intervention")) %>%
        mutate(table = paste0(round(mean, 0), " (", round(lb, 0), "-", round(ub, 0), ")"))

    p1 <- plot_qalys %>% 
        mutate(intervention = factor(intervention, levels = c("Seasonal maternal", "Year-round maternal", 
            "Seasonal la-mAB", "Seasonal la-mAB with\n annual catch-up",  "Year-round la-mAB" ))) %>%
        ggplot() + 
            geom_boxplot(aes(x = intervention, qaly_averted, fill = intervention)) + 
            geom_jitter(aes(x = intervention, qaly_averted, fill = intervention), height = 0, width = 0.3, shape = 21, alpha = 0.7) + 
            scale_fill_manual(values = c("#0d4fb2", "#91BAd6", "#c07002", "#fcae44", "#f0de16")) +
            scale_y_continuous(labels = comma) + guides(fill = "none") + 
            labs(y = "QALY gained over 10 years\n (discount rate 3.5%)", x = "Intervention",
                fill = "Intervention") + 
            theme_bw() + theme(text = element_text(size = 11))


    summarise_cost <- function(x) {
        x@outcomes$costs %>% ungroup %>% select(s, total) %>% unique
    }

    get_cost_averted <- function(base, intervention, name_string) {
        intervention %>%
            left_join(base %>% rename(total_base = total), by = join_by(s)) %>%
            mutate(cost_averted = total_base - total) %>%  mutate(intervention = name_string)

    }


    RSV_mat_pal_sum <- RSV_mat_pal %>% summarise_cost
    RSV_mat_s_sum <- RSV_mat_s %>% summarise_cost
    RSV_mat_yr_sum <- RSV_mat_yr %>% summarise_cost

    RSV_mab_pal_sum <- RSV_mab_pal %>% summarise_cost
    RSV_mab_s_sum <- RSV_mab_s %>% summarise_cost
    RSV_mab_s_cu_sum <- RSV_mab_s_cu %>% summarise_cost
    RSV_mab_yr_sum <- RSV_mab_yr %>% summarise_cost


    plot_costs <- bind_rows(
        get_cost_averted(RSV_mat_pal_sum, RSV_mat_s_sum, "Seasonal maternal"),
        get_cost_averted(RSV_mat_pal_sum, RSV_mat_yr_sum, "Year-round maternal"),
        get_cost_averted(RSV_mat_pal_sum, RSV_mab_s_sum, "Seasonal la-mAB"),
        get_cost_averted(RSV_mat_pal_sum, RSV_mab_s_cu_sum, "Seasonal la-mAB with\n annual catch-up"),
        get_cost_averted(RSV_mat_pal_sum, RSV_mab_yr_sum, "Year-round la-mAB")
    ) %>% 
        mutate(intervention = factor(intervention, levels = c("Seasonal maternal", "Year-round maternal", 
            "Seasonal la-mAB", "Seasonal la-mAB with\n annual catch-up",  "Year-round la-mAB" ))) 

    # Manuscript
    plot_costs %>% summarise(get_mean_95ci(cost_averted), .by = c("intervention")) %>%
        mutate(table = paste0(round(mean / 1000, 0), " (", round(lb / 1000, 0), "-", round(ub / 1000, 0), ")"))


    p2 <-  plot_costs %>% ggplot() + 
            geom_boxplot(aes(x = intervention, cost_averted, fill = intervention)) + 
            geom_jitter(aes(x = intervention, cost_averted, fill = intervention), height = 0, width = 0.3, shape = 21, alpha = 0.7) + 
            scale_fill_manual(values = c("#0d4fb2", "#91BAd6", "#c07002", "#fcae44", "#f0de16")) +
            scale_y_continuous(labels = comma) +  guides(fill = "none") + 
            labs(y = "Healthcare cost saved over 10 years\n (discount rate 3.5%)", x = "Intervention programme",
                fill = "Intervention") + theme_bw() + theme(text = element_text(size = 11))

    p1 / p2 + plot_layout(guides = "collect")
}