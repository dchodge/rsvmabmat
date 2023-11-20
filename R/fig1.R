load(file = here::here("outputs", "scenarios", "opt_unbound_ls", "RSV_mab_pal.RData")) # RSV_mab_pal

risks_df <- RSV_mab_pal@risks_df %>% select(age_group, outcome, ref, mean, lb_95, ub_95)
age_groups <- RSV_mab_pal@econ_df$age_group %>% unique

relabel_outcomes <- c("symptomatic" = "Symptomatic cases", "gp" = "GP consultations", 
    "a_e" = "A+E visits", "hosp" = "Hospital cases", "icu" = "ICU admissions", "death" = "Deaths")

efficacy_mat <- load(file = here::here("data", "efficacies", "mat_nobound_post_wane.RData")) 
efficacy_mat <- get(efficacy_mat)
efficacy_mab <- load(file = here::here("data", "efficacies", "nmab_nobound_post_wane.RData")) 
efficacy_mab <- get(efficacy_mab)
efficacy_lav <- load(file = here::here("data", "efficacies", "oa_papi_nobound_post_wane.RData")) 
efficacy_lav <- get(efficacy_lav)

df_wane_mat <- 1:100 %>% map_df(~
    data.frame(
        days = 0:365,
        prot = (efficacy_mat$wane_a_er3[.x] * (1 - pgamma(0:365, 3, 1/efficacy_mat$wane_b_er3[.x])) )
    )
)

df_wane_mab <- 1:100 %>% map_df(~
    data.frame(
        days = 0:365,
        prot = (efficacy_mab$wane_a_er3[.x] * (1 - pgamma(0:365, 3, 1/efficacy_mab$wane_b_er3[.x])) )
    )
)

df_wane_lav <- 1:100 %>% map_df(~
    data.frame(
        days = 0:365,
        prot = (efficacy_lav$wane_a_er3[.x] * (1 - pgamma(0:365, 3, 1/efficacy_lav$wane_b_er3[.x])) )
    )
)

p1 <- risks_df %>% mutate(age_group = factor(age_group, levels = age_groups)) %>% 
        mutate(outcome = recode(outcome, !!!relabel_outcomes)) %>%  
        mutate(outcome = factor(outcome, levels = relabel_outcomes)) %>%  
        ggplot() + 
            geom_ribbon(aes(x = age_group, ymin = lb_95, ymax = ub_95, group = outcome), alpha = 0.5) + 
            geom_line(aes(x = age_group, y = mean, group = outcome), size = 2) + 
            labs(x = "Age group", y = "Per-infection risk of health outcome") + 
            theme_bw() + facet_wrap(vars(outcome), scales = "free_y", nrow = 2) + 
            theme(axis.text.x = element_text(angle = 90))

# MONOCLONAL ANTIBODIES 
## 0.795: infection, symptomatic, gp, hosp, a_e
## 0.86 (1.08): icu, death

p2 <- df_wane_mab %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days, prot ), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 150, y = 0.795, yend = 0.795, linewidth = 2, color = "red") +
        theme_bw() + coord_cartesian(ylim = c(0, 1) )+ 
        labs(x = "Days post vaccination", y = "Proportion of individuals protected", 
            title = "Effiacy of la-mAB: \nInfection, symp, GP hosp, A&E")

p2_sev <- df_wane_mab %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days,  pmin(prot  * 1.08, 1) ), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 150, y = 0.795 * 1.08, yend = 0.795 * 1.08, linewidth = 2, color = "red") +
        theme_bw() + coord_cartesian(ylim = c(0, 1) )+ 
        labs(x = "Days post vaccination", y = "Proportion of individuals protected", 
            title = "Effiacy of la-mAB: \nICU, death")

p2 + p2_sev
ggsave(here::here("figs", "other", "mab_eff_compare.pdf"))

# MATERNAL VACCINATION IN NETONATES
## 0.795: infection, symptomatic, gp, hosp, a_e
## 0.86 (1.08): icu, death

p3 <- df_wane_mat %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days, prot), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 180, y =  0.5133, yend =  0.5133, linewidth = 2, color = "red") +
        theme_bw() + coord_cartesian(ylim = c(0, 1) ) + 
        labs(x = "Days after birth", y = "Proportion of individuals protected", 
            title = "Efficacy of MV (neonates)\nInfection, symp, GP")

p3_sev <- df_wane_mat %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days, pmin(prot  * 1.35, 1) ), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 180, y =  0.5133  * 1.35, yend =  0.5133  * 1.35, linewidth = 2, color = "red") +
        theme_bw() + coord_cartesian(ylim = c(0, 1) )+ 
        labs(x = "Days after birth", y = "Proportion of individuals protected", 
            title = "Efficacy of MV (neonates)\nA&E, hosp, ICU, deaths")

p3 + p3_sev
ggsave(here::here("figs", "other", "mat_neo_eff_compare.pdf"))


# MATERNAL VACCINATION IN PREGNANT WOMEN
## 0.717: infection, symptomatic
## 0.86 (1.15): GP, A&E,
## 0.941 (1.32): ICU, death



p4 <- df_wane_lav %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days, prot), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 365 / 12 * 6.7, y =  0.711, yend =  0.711, linewidth = 2, color = "red") +
        theme_bw() +  ylim(0, 1) + 
        labs(x = "Days post vaccination", y = "Proportion of individuals protected", 
            title = "Efficacy of MV (pregnant people)\nInfection, symp")

p4_sev <- df_wane_lav %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days, pmin(prot * 1.15, 1)), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 365 / 12 * 6.7, y =  0.711 * 1.15, yend =  0.711 * 1.15, linewidth = 2, color = "red") +
        theme_bw() +  coord_cartesian(ylim = c(0, 1) )+ 
        labs(x = "Days post vaccination", y = "Proportion of individuals protected", 
            title = "Efficacy of MV (pregnant people)\nGP, A&E")

p4_vsev <- df_wane_lav %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days, pmin(prot * 1.32, 1)), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 365 / 12 * 6.7, y =  0.711 * 1.32, yend =  0.711 * 1.32, linewidth = 2, color = "red") +
        theme_bw() +  coord_cartesian(ylim = c(0, 1) )+ 
        labs(x = "Days post vaccination", y = "Proportion of individuals protected", 
            title = "Efficacy of MV (pregnant people)\nhosp, ICU, death")

p4 + p4_sev + p4_vsev
ggsave(here::here("figs", "other", "mat_preg_eff_compare.pdf"))



# GSK VAC OLDER ADULTS
## 0.717: infection, symptomatic
## 0.86 (1.15): GP, A&E,
## 0.941 (1.32): ICU, death

p4 <- df_wane_lav %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days, prot), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 365 / 12 * 6.7, y =  0.711, yend =  0.711, linewidth = 2, color = "red") +
        theme_bw() + coord_cartesian(ylim = c(0, 1) )+ 
        labs(x = "Days post vaccination", y = "Proportion of individuals protected", 
            title = "Efficacy of OA (GSK)\nInfection, symp")

p4_sev <- df_wane_lav %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days, pmin(prot * 1.15, 1)), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 365 / 12 * 6.7, y =  0.711 * 1.15, yend =  0.711 * 1.15, linewidth = 2, color = "red") +
        theme_bw() +  coord_cartesian(ylim = c(0, 1) )+ 
        labs(x = "Days post vaccination", y = "Proportion of individuals protected", 
            title = "Efficacy of OA (GSK) \nGP, A&E")

p4_vsev <- df_wane_lav %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days, pmin(prot * 1.32, 1)), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 365 / 12 * 6.7, y =  0.711 * 1.32, yend =  0.711 * 1.32, linewidth = 2, color = "red") +
        theme_bw() + coord_cartesian(ylim = c(0, 1) )+ 
        labs(x = "Days post vaccination", y = "Proportion of individuals protected", 
            title = "Efficacy of OA (GSK)\nhosp, ICU, death")

p4 + p4_sev + p4_vsev
ggsave(here::here("figs", "other", "GSK_eff_compare.pdf"))


# PFIZER VAC OLDER ADULTS
## 0.621 (0.866): infection, symptomatic
## 0.667 (0.930): GP, A&E
## 0.85 (1.19): hosp, ICU, death

p4 <- df_wane_lav %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days, prot * 0.866), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 365 / 12 * 6.7, y =  0.71 * 0.866, yend =  0.711 * 0.866, linewidth = 2, color = "red") +
        theme_bw() +  coord_cartesian(ylim = c(0, 1) )+ 
        labs(x = "Days post vaccination", y = "Proportion of individuals protected", 
            title = "Efficacy of OA (Pfizer) )\nInfection, symp")

p4_sev <- df_wane_lav %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days, pmin(prot * 0.930, 1)), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 365 / 12 * 6.7, y =  0.711 * 0.930, yend =  0.711 * 0.930, linewidth = 2, color = "red") +
        theme_bw() +  coord_cartesian(ylim = c(0, 1) )+ 
        labs(x = "Days post vaccination", y = "Proportion of individuals protected", 
            title = "Efficacy of OA (GSK)\nGP, A&E")

p4_vsev <- df_wane_lav %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days, pmin(prot * 1.195)), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 365 / 12 * 6.7, y =  0.711 * 1.195, yend =  0.711 * 1.195, linewidth = 2, color = "red") +
        theme_bw() +  coord_cartesian(ylim = c(0, 1) )+ 
        labs(x = "Days post vaccination", y = "Proportion of individuals protected", 
            title = "Efficacy of OA (GSK)\nhosp, ICU, death")

p4 + p4_sev + p4_vsev
ggsave(here::here("figs", "other", "pfizer_eff_compare.pdf"))








p1 / (p2 + p3 + p4) + plot_annotation(tag_levels = "A") 
ggsave(here::here("figs", "other", "fig1.pdf"), height = 15, width = 18)



p2 <- df_wane_mab %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days, prot ), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 150, y = 0.795, yend = 0.795, linewidth = 2, color = "red") +
        theme_bw() + coord_cartesian(ylim = c(0, 1) )+ 
        labs(x = "Days post vaccination", y = "Proportion of individuals protected", 
            title = "Effiacy of la-mAB: \nInfection, symp, GP hosp, A&E")

p2_sev <- df_wane_mab %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days,  pmin(prot  * 1.08, 1) ), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 150, y = 0.795 * 1.08, yend = 0.795 * 1.08, linewidth = 2, color = "red") +
        theme_bw() + coord_cartesian(ylim = c(0, 1) )+ 
        labs(x = "Days post vaccination", y = "Proportion of individuals protected", 
            title = "Effiacy of la-mAB: \nICU, death")

p2 + p2_sev
ggsave(here::here("figs", "other", "mab_eff_compare.pdf"))

# MATERNAL VACCINATION IN NETONATES
## 0.795: infection, symptomatic, gp, hosp, a_e
## 0.86 (1.08): icu, death

p3 <- df_wane_mat %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days, prot), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 180, y =  0.5133, yend =  0.5133, linewidth = 2, color = "red") +
        theme_bw() + coord_cartesian(ylim = c(0, 1) ) + 
        labs(x = "Days after birth", y = "Proportion of individuals protected", 
            title = "Efficacy of MV (neonates)\nInfection, symp, GP")

p3_sev <- df_wane_mat %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days, pmin(prot  * 1.35, 1) ), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 180, y =  0.5133  * 1.35, yend =  0.5133  * 1.35, linewidth = 2, color = "red") +
        theme_bw() + coord_cartesian(ylim = c(0, 1) )+ 
        labs(x = "Days after birth", y = "Proportion of individuals protected", 
            title = "Efficacy of MV (neonates)\nA&E, hosp, ICU, deaths")

p3 + p3_sev
ggsave(here::here("figs", "other", "mat_neo_eff_compare.pdf"))


p4 <- df_wane_lav %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days, prot), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 365 / 12 * 6.7, y =  0.711, yend =  0.711, linewidth = 2, color = "red") +
        theme_bw() +  ylim(0, 1) + 
        labs(x = "Days post vaccination", y = "Proportion of individuals protected", 
            title = "Efficacy of MV (pregnant people)\nInfection, symp, GP, A&E")

p4_vsev <- df_wane_lav %>% 
    ggplot() + 
        stat_lineribbon(aes(x = days, pmin(prot * 1.32, 1)), .width = 0.95, fill = "gray", alpha = 0.6) + 
        geom_segment(x = 0, xend = 365 / 12 * 6.7, y =  0.711 * 1.32, yend =  0.711 * 1.32, linewidth = 2, color = "red") +
        theme_bw() +  coord_cartesian(ylim = c(0, 1) )+ 
        labs(x = "Days post vaccination", y = "Proportion of individuals protected", 
            title = "Efficacy of MV (pregnant people)\nhosp, ICU, death")

p4 + p4_sev + p4_vsev

(p2 + p3 + p4) / (p2_sev + p3_sev + p4_vsev)  & theme(text = element_text(size = 18))
ggsave(here::here("figs", "other", "fig1_alt.pdf"), height = 12, width = 18)

