
load_cea_table <- function(model_type) {
    load(here::here("outputs", "scenarios", model_type, "RSV_mat_pal.RData"))
    load(here::here("outputs", "scenarios", model_type, "RSV_mat_s.RData"))
    load(here::here("outputs", "scenarios", model_type, "RSV_mat_yr.RData"))

    load(here::here("outputs", "scenarios", model_type, "RSV_mab_pal.RData"))
    load(here::here("outputs", "scenarios", model_type, "RSV_mab_s.RData"))
    load(here::here("outputs", "scenarios", model_type, "RSV_mab_s_cu.RData"))
    load(here::here("outputs", "scenarios", model_type, "RSV_mab_yr.RData"))

    # Maternal stuff

    costs_mat <- left_join(
        RSV_mat_pal@outcomes$costs %>% ungroup %>% select(s, base = total) %>% unique,
        RSV_mat_s@outcomes$costs %>% ungroup %>% select(s, mat_sea = total) %>% unique, by = "s"
    ) %>% left_join(
        RSV_mat_yr@outcomes$costs %>% ungroup %>% select(s, mat_yr = total) %>% unique, by = "s"
    ) %>% mutate(across(base:mat_yr, ~(.x - base) ) )

    qaly_mat <- left_join(
        RSV_mat_pal@outcomes$qaly %>% ungroup %>% select(s, base = total) %>% unique,
        RSV_mat_s@outcomes$qaly %>% ungroup %>% select(s, mat_sea = total) %>% unique, by = "s"
    ) %>% left_join(
        RSV_mat_yr@outcomes$qaly %>% ungroup %>% select(s, mat_yr = total)  %>% unique, by = "s"
    ) %>% mutate(across(base:mat_yr, ~(base - .x) ) )

    # Monoclonal stuff

    costs_mab <- left_join(
        RSV_mab_pal@outcomes$costs %>% ungroup %>% select(s, base = total) %>% unique,
        RSV_mab_s@outcomes$costs %>% ungroup %>% select(s, mab_sea = total) %>% unique, by = "s"
    ) %>% left_join(
        RSV_mab_s_cu@outcomes$costs %>% ungroup %>% select(s, mab_sea_cu = total) %>% unique, by = "s"
    ) %>% left_join(
        RSV_mab_yr@outcomes$costs %>% ungroup %>% select(s, mab_yr = total) %>% unique, by = "s"
    ) %>% mutate(across(base:mab_yr, ~(.x - base) ) )

    qaly_mab <- left_join(
        RSV_mab_pal@outcomes$qaly %>% ungroup %>% select(s, base = total) %>% unique,
        RSV_mab_s@outcomes$qaly %>% ungroup %>% select(s, mab_sea = total) %>% unique, by = "s"
    ) %>% left_join(
        RSV_mab_s_cu@outcomes$qaly %>% ungroup %>% select(s, mab_sea_cu = total) %>% unique, by = "s"
    ) %>% left_join(
        RSV_mab_yr@outcomes$qaly %>% ungroup %>% select(s, mab_yr = total) %>% unique, by = "s"
    ) %>% mutate(across(base:mab_yr, ~(base - .x) ) )


    all_sce_combined <- left_join(
        bind_rows(
            qaly_mab %>% pivot_longer(base:mab_yr, names_to = "scenario", values_to = "qaly"),
            qaly_mat %>% pivot_longer(base:mat_yr, names_to = "scenario", values_to = "qaly"),
        ),
        bind_rows(
            costs_mab %>% pivot_longer(base:mab_yr, names_to = "scenario", values_to = "cost"),
            costs_mat %>% pivot_longer(base:mat_yr, names_to = "scenario", values_to = "cost")
        ), by = join_by(s, scenario), relationship = "many-to-many"
    ) %>% unique

    doses <- bind_rows(
        get_doses(RSV_mat_pal, "base", "none"),
        get_doses(RSV_mat_s, "mat_sea", "mat"),
        get_doses(RSV_mat_yr, "mat_yr", "mat"),
        get_doses(RSV_mab_s, "mab_sea", "mAB"),
        get_doses(RSV_mab_s_cu, "mab_sea_cu", "mAB"),
        get_doses(RSV_mab_yr, "mab_yr", "mAB")
    )

    sce_full <- all_sce_combined %>% left_join(doses, by = join_by(scenario))  
    sce_full
}


load_cea_table_no_cu <- function(model_type) {
    load(here::here("outputs", "scenarios", model_type, "RSV_mat_pal.RData"))
    load(here::here("outputs", "scenarios", model_type, "RSV_mat_s.RData"))
    load(here::here("outputs", "scenarios", model_type, "RSV_mat_yr.RData"))

    load(here::here("outputs", "scenarios", model_type, "RSV_mab_pal.RData"))
    load(here::here("outputs", "scenarios", model_type, "RSV_mab_s.RData"))
    load(here::here("outputs", "scenarios", model_type, "RSV_mab_yr.RData"))

    # Maternal stuff

    costs_mat <- left_join(
        RSV_mat_pal@outcomes$costs %>% ungroup %>% select(s, base = total) %>% unique,
        RSV_mat_s@outcomes$costs %>% ungroup %>% select(s, mat_sea = total) %>% unique, by = "s"
    ) %>% left_join(
        RSV_mat_yr@outcomes$costs %>% ungroup %>% select(s, mat_yr = total) %>% unique, by = "s"
    ) %>% mutate(across(base:mat_yr, ~(.x - base) ) )

    qaly_mat <- left_join(
        RSV_mat_pal@outcomes$qaly %>% ungroup %>% select(s, base = total) %>% unique,
        RSV_mat_s@outcomes$qaly %>% ungroup %>% select(s, mat_sea = total) %>% unique, by = "s"
    ) %>% left_join(
        RSV_mat_yr@outcomes$qaly %>% ungroup %>% select(s, mat_yr = total)  %>% unique, by = "s"
    ) %>% mutate(across(base:mat_yr, ~(base - .x) ) )

    # Monoclonal stuff

    costs_mab <- left_join(
        RSV_mab_pal@outcomes$costs %>% ungroup %>% select(s, base = total) %>% unique,
        RSV_mab_s@outcomes$costs %>% ungroup %>% select(s, mab_sea = total) %>% unique, by = "s"
    ) %>%  left_join(
        RSV_mab_yr@outcomes$costs %>% ungroup %>% select(s, mab_yr = total) %>% unique, by = "s"
    ) %>% mutate(across(base:mab_yr, ~(.x - base) ) )

    qaly_mab <- left_join(
        RSV_mab_pal@outcomes$qaly %>% ungroup %>% select(s, base = total) %>% unique,
        RSV_mab_s@outcomes$qaly %>% ungroup %>% select(s, mab_sea = total) %>% unique, by = "s"
    ) %>% left_join(
        RSV_mab_yr@outcomes$qaly %>% ungroup %>% select(s, mab_yr = total) %>% unique, by = "s"
    ) %>% mutate(across(base:mab_yr, ~(base - .x) ) )


    all_sce_combined <- left_join(
        bind_rows(
            qaly_mab %>% pivot_longer(base:mab_yr, names_to = "scenario", values_to = "qaly"),
            qaly_mat %>% pivot_longer(base:mat_yr, names_to = "scenario", values_to = "qaly"),
        ),
        bind_rows(
            costs_mab %>% pivot_longer(base:mab_yr, names_to = "scenario", values_to = "cost"),
            costs_mat %>% pivot_longer(base:mat_yr, names_to = "scenario", values_to = "cost")
        ), by = join_by(s, scenario), relationship = "many-to-many"
    ) %>% unique

    doses <- bind_rows(
        get_doses(RSV_mat_pal, "base", "none"),
        get_doses(RSV_mat_s, "mat_sea", "mat"),
        get_doses(RSV_mat_yr, "mat_yr", "mat"),
        get_doses(RSV_mab_s, "mab_sea", "mAB"),
        get_doses(RSV_mab_yr, "mab_yr", "mAB")
    )

    sce_full <- all_sce_combined %>% left_join(doses, by = join_by(scenario))  
    sce_full
}


generate_ppd_samples <- function(cea_table, ppds) {

    full_psa <-
        future_map2_dfr(ppds$x, ppds$y,
            function(x, y) {
                sce_full_ppd <- cea_table %>% mutate(cost = cost + doses_mab * x + doses_mat * y + doses_vhr * 306.35) %>% as.data.table %>%
                    select(s, scenario, qaly, cost) %>% mutate(ppd_mab = x, ppd_mat = y)
                sce_full_ppd
            }
        )
}

generate_cea_metrics <- function(full_psa, ppds, thresholds) {

    mean_psa <- full_psa %>% 
        summarise(across(qaly:cost, ~mean(.x)), .by = c(scenario, ppd_mat, ppd_mab))

    full_psa_temp <- full_psa %>% filter(ppd_mab == 0, ppd_mat == 0)
    impact_prog <- cea(full_psa_temp, comparator = "base", k = 20000, sample = "s", strategy = "scenario",  e = "qaly", c = "cost")$summary %>% 
        select(scenario, e_mean)
    impact_order <- impact_prog %>% arrange(e_mean) %>% pull(scenario) %>% rev

    inmb_full_list <- list()
    inmb_opt_list <- list()
    inmb_best_opt_list <- list()
    evpi_opt_list <- list()
    i <- 1

    for(k in thresholds) {
        for(x in unique(ppds$x)) {
            for(y in unique(ppds$y)) {
                full_psa_temp <- full_psa %>% filter(ppd_mab == x, ppd_mat == y)
                cea_analysis <- cea(full_psa_temp, comparator = "base", k = k, sample = "s", strategy = "scenario",  e = "qaly", c = "cost")
                
                inmb_full_list[[i]] <- cea_analysis$mce %>% as.data.frame %>% mutate(ppd_mab = x, ppd_mat = y, threshold = k) 
                inmb_opt_list[[i]] <- cea_analysis$mce %>% as.data.frame %>% filter(best == 1) %>% mutate(ppd_mab = x, ppd_mat = y, threshold = k) 
                inmb_tab <- cea_analysis$nmb 
                names <- inmb_tab %>% mutate(inmb = enmb - as.numeric(inmb_tab[1, 4]) ) %>% filter(inmb >= 0) %>% pull(scenario)
                best_cea <- names[order(factor(names, levels = impact_order))][1]

                inmb_best_opt_list[[i]] <- best_cea %>% as.data.frame %>% mutate(ppd_mab = x, ppd_mat = y, threshold = k) %>% rename(scenario = ".") 
                evpi_opt_list[[i]] <- cea_analysis$evpi %>% mutate(ppd_mab = x, ppd_mat = y, threshold = k)  
                i <- i + 1
            }
        }
    }
    inmb_full <- inmb_full_list %>% bind_rows
    inmb_opt <- inmb_opt_list %>% bind_rows
    inmb_best_opt <- inmb_best_opt_list %>% bind_rows
    evpi_opt <- evpi_opt_list %>% bind_rows

    list(inmb_full = inmb_full, inmb = inmb_opt, inmb_impact = inmb_best_opt, impact_qaly = impact_prog, evpi = evpi_opt)

}

load_data_fig4 <- function(string_name, ppds, thresholds, scaling = c(1, 1, 1, 1), type_string = "A") {

    if (type_string == "A") {
        cea_table <- load_cea_table(paste0("opt_", string_name))
    } else if (type_string == "C") {
        cea_table <- load_cea_table_no_cu(paste0("opt_", string_name))
    }

    cea_table <- cea_table %>% mutate(qaly = 
        case_when(doses_mab > 0 ~ qaly * scaling[1], TRUE ~ qaly)) 
    cea_table <- cea_table %>% mutate(qaly = 
        case_when(doses_mat > 0 ~ qaly * scaling[2], TRUE ~ qaly)) 
    cea_table <- cea_table %>% mutate(cost = 
        case_when(doses_mab > 0 ~ cost * scaling[3], TRUE ~ cost)) 
    cea_table <- cea_table %>% mutate(cost = 
        case_when(doses_mat > 0 ~ cost * scaling[4], TRUE ~ cost)) 

    cat("Generating ppd samples", string_name, "\n")
    full_psa <- generate_ppd_samples(cea_table, ppds)
    cea_metrics <- generate_cea_metrics(full_psa, ppds, thresholds)
    cat("Plotting ", string_name, "\n")
    write_rds(full_psa, file = here::here("outputs", "scenarios", paste0("opt_", string_name), paste0("full_psa_", type_string, ".rds")))
    write_rds(cea_metrics, file = here::here("outputs", "scenarios", paste0("opt_", string_name), paste0("cea_metrics_", type_string, ".rds")))
    cea_metrics
}


plot_figs4 <- function(cea_metrics, threshold_icer, mab_cov = 90, mat_cov = 60) {

    relabel_sce <- c("base" = "Palivizumab progamme",
        "mab_sea" = "Seasonal LA-mAbs",
        "mab_sea_cu" = "Seasonal LA-mAbs with catch-up", 
        "mat_sea" = "Seasonal maternal programme",
        "mat_yr" = "Year-round maternal programme",
        "mab_yr" = "Year-round LA-mAbs programme"
        )

    threshold_icer_string <- format(threshold_icer, big.mark = ",")

    require(scales)
    inmb_opt_20000 <- cea_metrics$inmb %>% mutate(scenario = recode(scenario, !!!relabel_sce)) %>% 
        filter(threshold == threshold_icer) %>% mutate(uncert = case_when(
        prob >= 0.5~">50%",
        prob < 0.5~"<50%")   )


    p1 <- inmb_opt_20000 %>% 
        ggplot() +  
            geom_tile(aes(x = ppd_mat, y = ppd_mab, fill = scenario), alpha = 0.85, size = 1.1, width=4.3, height=4.5) +
            geom_point(aes(x = ppd_mat, y = ppd_mab, color = uncert, alpha = uncert), size = 1.1) +
            scale_fill_manual(values = c("gray50", "#c07002", "#fcae44", "#0d4fb2", "#91BAd6")) + 
            scale_color_manual(values = c("black", "gray50")) +
            scale_alpha_manual(values = c(1, 0)) + 
            guides(color = "none") + 
            guides(alpha = guide_legend(override.aes = list(fill = "gray", size = 3))) +
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            labs(x = "CCPA maternal vaccination (£)",
            y = "CCPA monolconal antibodies (£)", fill = "Optimal scenario", alpha = paste0("Confidence, (", threshold_icer_string, " £/QALY)"), 
                subtitle = 
                    paste0(threshold_icer_string, " £/QALY, la-mAB coverage: ", mab_cov, "%, MV covarage: ", mat_cov, "%"),
            title = "Optimal programme with using INMB") +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5)) 


    p2 <- cea_metrics$evpi %>% filter(threshold == threshold_icer) %>% 
        ggplot() +  
            geom_tile(aes(x = ppd_mat, y = ppd_mab, fill = evpi), alpha = 0.85, size = 1.1, width=4.3, height=4.5) + 
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            labs(x = "CCPA maternal vaccination (£)",
            y = "CCPA monolconal antibodies (£)", fill = "EVPI", color = paste0("Confidence, (", threshold_icer_string, " £/QALY)"), 
                subtitle = 
                    paste0(threshold_icer_string, " £/QALY, la-mAB coverage: ", mab_cov, "%, MV covarage: ", mat_cov, "%"),
            title = "Expected value of perfect information") +
            scale_fill_gradient2(low = "white", high = "red", labels = comma) +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))


    p0A <- p1 / p2 + plot_layout(guides = "collect")  + plot_annotation(tag_levels = "A")
    p0A
}



plot_figs4_30 <- function(cea_metrics, threshold_icer, mab_cov = 90, mat_cov = 60) {

    relabel_sce <- c("base" = "Palivizumab progamme",
        "mab_sea" = "Seasonal LA-mAbs",
        "mab_sea_cu" = "Seasonal LA-mAbs with catch-up", 
        "mat_sea" = "Seasonal maternal programme",
        "mat_yr" = "Year-round maternal programme",
        "mab_yr" = "Year-round LA-mAbs programme"
        )

    threshold_icer_string <- format(threshold_icer, big.mark = ",")

    require(scales)
    inmb_opt_30000 <- cea_metrics$inmb %>% mutate(scenario = recode(scenario, !!!relabel_sce)) %>% 
        filter(threshold == threshold_icer) %>% mutate(uncert = case_when(
        prob >= 0.9~">90%",
        prob < 0.9~"<90%")   )


    p1 <- inmb_opt_30000 %>% 
        ggplot() +  
            geom_tile(aes(x = ppd_mat, y = ppd_mab, fill = scenario), alpha = 0.85, size = 1.1, width=4.3, height=4.5) +
            geom_point(aes(x = ppd_mat, y = ppd_mab, color = uncert, alpha = uncert), size = 1.1) +
            scale_fill_manual(values = c("gray50", "#c07002", "#fcae44", "#0d4fb2", "#91BAd6")) + 
            scale_color_manual(values = c("black", "gray50")) +
            scale_alpha_manual(values = c(1, 0)) + 
            guides(color = "none") + 
            guides(alpha = guide_legend(override.aes = list(fill = "gray", size = 3))) +
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            labs(x = "CCPA maternal vaccination (£)",
            y = "CCPA monolconal antibodies (£)", fill = "Optimal scenario", alpha = paste0("Confidence, (", threshold_icer_string, " £/QALY)"), 
                subtitle = 
                    paste0(threshold_icer_string, " £/QALY, la-mAB coverage: ", mab_cov, "%, MV covarage: ", mat_cov, "%"),
            title = "Optimal programme with using INMB") +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5)) 


    p2 <- cea_metrics$evpi %>% filter(threshold == threshold_icer) %>% 
        ggplot() +  
            geom_tile(aes(x = ppd_mat, y = ppd_mab, fill = evpi), alpha = 0.85, size = 1.1, width=4.3, height=4.5) + 
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            labs(x = "CCPA maternal vaccination (£)",
            y = "CCPA monolconal antibodies (£)", fill = "EVPI", color = paste0("Confidence, (", threshold_icer_string, " £/QALY)"), 
                subtitle = 
                    paste0(threshold_icer_string, " £/QALY, la-mAB coverage: ", mab_cov, "%, MV covarage: ", mat_cov, "%"),
            title = "Expected value of perfect information") +
            scale_fill_gradient2(low = "white", high = "red", labels = comma) +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))


    p0A <- p1 / p2 + plot_layout(guides = "collect")  + plot_annotation(tag_levels = "A")
    p0A
}


plot_figs4_trim <- function(cea_metrics) {

    relabel_sce <- c("base" = "Palivizumab progamme",
        "mab_sea" = "Seasonal LA-mAbs",
        "mab_sea_cu" = "Seasonal LA-mAbs with catch-up", 
        "mat_sea" = "Seasonal maternal programme",
        "mat_yr" = "Year-round maternal programme",
        "mab_yr" = "Year-round LA-mAbs programme"
        )

    require(scales)
    inmb_opt_20000 <- cea_metrics$inmb %>% mutate(scenario = recode(scenario, !!!relabel_sce)) %>% 
        filter(threshold == 20000) %>% mutate(uncert = case_when(
        prob >= 0.5~">50%",
        prob < 0.5~"<50%")   )

    inmb_opt_30000 <- cea_metrics$inmb %>% mutate(scenario = recode(scenario, !!!relabel_sce)) %>%
        filter(threshold == 30000) %>%
        mutate(prob >= 0.9) %>% mutate(uncert = case_when(
        prob >= 0.9~">90%",
        prob < 0.9~"<90%")   )

    p1 <- inmb_opt_20000 %>% 
        ggplot() +  
            geom_tile(aes(x = ppd_mat, y = ppd_mab, fill = scenario), alpha = 0.85, size = 1.1, width=4.3, height=4.5) +
            geom_point(aes(x = ppd_mat, y = ppd_mab, color = uncert, alpha = uncert), size = 1.1) +
        scale_fill_manual(values = c("gray50", "#c07002", "#fcae44", "#0d4fb2", "#91BAd6")) + 
            scale_color_manual(values = c("black", "gray50")) +
            scale_alpha_manual(values = c(1, 0)) + 
            guides(color = "none") + 
            guides(alpha = guide_legend(override.aes = list(fill = "gray", size = 3))) +
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            labs(x = "CCPA maternal vaccination (£)",
            y = "CCPA monolconal antibodies (£)", fill = "Optimal scenario", alpha = "Confidence (20,000 £/QALY)", 
            subtitle = "20,000£/QALY",
            title = "Optimal programme with using INMB") +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5)) 

    p0A <- p1 + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")
    p0A
}


run_cea_sens <- function(ppds, thresholds) {
    load_data_fig4("unbound_ss_50_70", ppds, thresholds)
    load_data_fig4("unbound_ss_60_70", ppds, thresholds)
    load_data_fig4("unbound_ss_70_70", ppds, thresholds)
    load_data_fig4("unbound_ss_80_70", ppds, thresholds)
    load_data_fig4("unbound_ss_90_70", ppds, thresholds)

    load_data_fig4("unbound_ss_50_80", ppds, thresholds)
    load_data_fig4("unbound_ss_60_80", ppds, thresholds)
    load_data_fig4("unbound_ss_70_80", ppds, thresholds)
    load_data_fig4("unbound_ss_80_80", ppds, thresholds)
    load_data_fig4("unbound_ss_90_80", ppds, thresholds)

    load_data_fig4("unbound_ss_50_90", ppds, thresholds)
    load_data_fig4("unbound_ss_60_90", ppds, thresholds)
    load_data_fig4("unbound_ss_70_90", ppds, thresholds)
    load_data_fig4("unbound_ss_80_90", ppds, thresholds)
    load_data_fig4("unbound_ss_90_90", ppds, thresholds)
}

load_data_fig5 <- function() {
    relabel_sce <- c("base" = "Palivizumab progamme",
        "mab_sea" = "Seasonal la-mAbs",
        "mab_sea_cu" = "Seasonal la-mAbs with catch-up", 
        "mat_sea" = "Seasonal maternal programme",
        "mat_yr" = "Year-round maternal programme",
        "mab_yr" = "Year-round la-mAbs programme"
        )


    covs <- crossing(x = seq(50, 90, 10), y = seq(70, 90, 10))
    full_data_rshinyB <- map2_df(covs$x, covs$y, 
        function(x, y) {
            names_string <- paste0("unbound_ss_", x, "_", y)
            cea_metrics <- readRDS(file = here::here("outputs", "scenarios", paste0("opt_", names_string), paste0("cea_metrics_A.rds")))
            cea_metric_trim <- cea_metrics$inmb_full %>% filter(ppd_mab == ppd_mat, threshold == 20000) %>% 
                mutate(prob = prob * 100) 
            map_df(seq(0, 200, 5),
                function(y) {
                    temp_ppd <- cea_metric_trim %>% filter(ppd_mab == y) 
                    map_df(1:6,
                        ~data.frame(
                            scenario = rep(temp_ppd[.x, 2] , temp_ppd[.x, 5])
                        )
                    ) %>% mutate(sample = 1:nrow(.), ppd = y)
                }
            ) %>% mutate(cov_mat = x, cov_mab = y)
        }
    ) %>% mutate(scenario = recode(scenario, !!!relabel_sce))

}



plot_fig5 <- function(df_fig5) {
    horizontal_lines_data <- data.frame(
    xintercept = seq(2.5, 150, 5)   # y-coordinate for the horizontal line(s)
    )


    df_fig5 %>% arrange(ppd, scenario) %>% filter(ppd <= 150) %>% 
        ggplot() + geom_tile(aes(ppd, sample, fill = scenario)) + 
        facet_grid(rows = vars(cov_mab), cols = vars(cov_mat)) + theme_bw() + 
    # geom_vline(data = horizontal_lines_data, aes(xintercept = xintercept), 
    #          color = "white", size = 0.4) +
        scale_fill_manual(values = c("gray50", "#c07002", "#fcae44", "#0d4fb2", "#91BAd6")) + 
        scale_y_continuous(expand = c(0, 0),
            sec.axis = sec_axis(~ . , name = "Coverage of la-mAB programme (%)", breaks = NULL, labels = NULL))  +
        scale_x_continuous(expand = c(0, 0), 
            sec.axis = sec_axis(~ . , name = "Coverage of MV programme (%)", breaks = NULL, labels = NULL), breaks = seq(0, 140, 30)) + 
        theme(text = element_text(size = 15),
            axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
            axis.title.x.top = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0))
        ) + 
        labs(x = "CCPA for MV and la-mAB ", y = "Proportion of MC samples (%)", fill = "Optimal scenario")
}