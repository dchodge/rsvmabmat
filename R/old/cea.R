
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


load_cea_table_vhr <- function(model_type) {
    load(here::here("outputs", "scenarios", model_type, "RSV_mat_pal.RData"))
    load(here::here("outputs", "scenarios", model_type, "RSV_mat_vhr.RData"))
    load(here::here("outputs", "scenarios", model_type, "RSV_mat_s.RData"))
    load(here::here("outputs", "scenarios", model_type, "RSV_mat_yr.RData"))

    load(here::here("outputs", "scenarios", model_type, "RSV_mab_pal.RData"))
    load(here::here("outputs", "scenarios", model_type, "RSV_mab_vhr.RData"))
    load(here::here("outputs", "scenarios", model_type, "RSV_mab_s.RData"))
    load(here::here("outputs", "scenarios", model_type, "RSV_mab_s_cu.RData"))
    load(here::here("outputs", "scenarios", model_type, "RSV_mab_yr.RData"))

    # Maternal stuff

    costs_mat <- left_join(
        RSV_mat_pal@outcomes$costs %>% ungroup %>% select(s, base = total) %>% unique,
        RSV_mat_vhr@outcomes$costs %>% ungroup %>% select(s, mat_vhr = total) %>% unique,
     ) %>% left_join(
        RSV_mat_s@outcomes$costs %>% ungroup %>% select(s, mat_sea = total) %>% unique, by = "s"
    ) %>% left_join(
        RSV_mat_yr@outcomes$costs %>% ungroup %>% select(s, mat_yr = total) %>% unique, by = "s"
    ) %>% mutate(across(base:mat_yr, ~(.x - base) ) )

    qaly_mat <- left_join(
        RSV_mat_pal@outcomes$qaly %>% ungroup %>% select(s, base = total) %>% unique,
        RSV_mat_vhr@outcomes$costs %>% ungroup %>% select(s, mat_vhr = total) %>% unique,
     ) %>% left_join(
        RSV_mat_s@outcomes$qaly %>% ungroup %>% select(s, mat_sea = total) %>% unique, by = "s"
    ) %>% left_join(
        RSV_mat_yr@outcomes$qaly %>% ungroup %>% select(s, mat_yr = total)  %>% unique, by = "s"
    ) %>% mutate(across(base:mat_yr, ~(base - .x) ) )

    # Monoclonal stuff

    costs_mab <- left_join(
        RSV_mab_pal@outcomes$costs %>% ungroup %>% select(s, base = total) %>% unique,
        RSV_mab_vhr@outcomes$costs %>% ungroup %>% select(s, mab_vhr = total) %>% unique, by = "s"
    ) %>% left_join(
        RSV_mab_s@outcomes$costs %>% ungroup %>% select(s, mab_sea = total) %>% unique, by = "s"
    ) %>% left_join(
        RSV_mab_s_cu@outcomes$costs %>% ungroup %>% select(s, mab_sea_cu = total) %>% unique, by = "s"
    ) %>% left_join(
        RSV_mab_yr@outcomes$costs %>% ungroup %>% select(s, mab_yr = total) %>% unique, by = "s"
    ) %>% mutate(across(base:mab_yr, ~(.x - base) ) )

    qaly_mab <- left_join(
        RSV_mab_pal@outcomes$qaly %>% ungroup %>% select(s, base = total) %>% unique,
        RSV_mab_vhr@outcomes$qaly %>% ungroup %>% select(s, mab_vhr = total) %>% unique, by = "s"
    ) %>% left_join(
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
        get_doses(RSV_mat_s, "mat_vhr", "mat"),
        get_doses(RSV_mat_s, "mat_sea", "mat"),
        get_doses(RSV_mat_yr, "mat_yr", "mat"),
        get_doses(RSV_mab_vhr, "mab_vhr", "mAB"),
        get_doses(RSV_mab_s, "mab_sea", "mAB"),
        get_doses(RSV_mab_s_cu, "mab_sea_cu", "mAB"),
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


generate_cea_metrics_oa <- function(full_psa, ppds, thresholds) {
    mean_psa <- full_psa %>% 
        summarise(across(qaly:cost, ~mean(.x)), .by = c(scenario, ppd_lav))

    icer_opt <- map_df(thresholds,
        function(k) {
            future_map_dfr(ppds$x,
            function(x) {
                mean_psa_temp <- mean_psa %>% filter(ppd_lav == x)
                as.data.table(calculate_icers(
                    mean_psa_temp$cost, 
                    mean_psa_temp$qaly,
                    mean_psa_temp$scenario
                ))[Status == "ND" & (ICER < k | is.na(ICER))] %>% .[nrow(.),] %>%
                    mutate(ppd_lav = x, threshold = k)
                }
            )
        } 
    ) %>% rename(scenario = Strategy)

    inmb_opt <- map_df(thresholds,
        function(k) {
            future_map_dfr(ppds$x,
                function(x) {
                    full_psa_temp <- full_psa %>% filter(ppd_lav == x)
                    cea(full_psa_temp, comparator = "base", k = k, sample = "s", strategy = "scenario",  e = "qaly", c = "cost")$mce %>% 
                        as.data.frame %>% filter(best == 1) %>% mutate(ppd_lav = x, threshold = k) 
                }
            ) 
        }
    )

    evpi_opt <- map_df(c(20000, 30000),
        function(k) {
            future_map_dfr(ppds$x, 
                function(x) {
                    full_psa_temp <- full_psa %>% filter(ppd_lav == x)
                    cea(full_psa_temp, comparator = "base", k = k, sample = "s", strategy = "scenario",  e = "qaly", c = "cost")$evpi %>% 
                        mutate(ppd_lav = x, threshold = k) 
                }
            ) 
        }
    )

    list(icer = icer_opt, inmb = inmb_opt, evpi = evpi_opt)

}

generate_cea_metrics_full <- function(full_psa, ppds, thresholds) {
    mean_psa <- full_psa %>% 
        summarise(across(qaly:cost, ~mean(.x)), .by = c(scenario, ppd_mat, ppd_mab))

    #icer_opt <- map_df(thresholds,
    #    function(k) {
   #         future_map2_dfr(ppds$x, ppds$y,
  #          function(x, y) {
 #               mean_psa_temp <- mean_psa %>% filter(ppd_mat == x, ppd_mab == y)
#
      #          as.data.table(calculate_icers(
     #               mean_psa_temp$cost, 
    #                mean_psa_temp$qaly,
   #                 mean_psa_temp$scenario
  #              ))[Status == "ND" & (ICER < k | is.na(ICER))] %>% .[nrow(.),] %>%
  #                  mutate(ppd_mat = x, ppd_mab = y, threshold = k)
  #              }
  #          )
  #      } 
 #   ) %>% rename(scenario = Strategy)


    inmb_full <- map_df(thresholds,
        function(k) {
            future_map2_dfr(ppds$x, ppds$y,
                function(x, y) {
                    full_psa_temp <- full_psa %>% filter(ppd_mab == x, ppd_mat == y)
                    cea(full_psa_temp, comparator = "base", k = k, sample = "s", strategy = "scenario",  e = "qaly", c = "cost")$mce %>% 
                        as.data.frame %>% mutate(ppd_mab = x, ppd_mat = y, threshold = k) 
                }
            )
        }
    )


    list(inmb = inmb_full)

}


generate_cea_metrics <- function(full_psa, ppds, thresholds) {
    mean_psa <- full_psa %>% 
        summarise(across(qaly:cost, ~mean(.x)), .by = c(scenario, ppd_mat, ppd_mab))

    #icer_opt <- map_df(thresholds,
    #    function(k) {
   #         future_map2_dfr(ppds$x, ppds$y,
  #          function(x, y) {
 #               mean_psa_temp <- mean_psa %>% filter(ppd_mat == x, ppd_mab == y)
#
      #          as.data.table(calculate_icers(
     #               mean_psa_temp$cost, 
    #                mean_psa_temp$qaly,
   #                 mean_psa_temp$scenario
  #              ))[Status == "ND" & (ICER < k | is.na(ICER))] %>% .[nrow(.),] %>%
  #                  mutate(ppd_mat = x, ppd_mab = y, threshold = k)
  #              }
  #          )
  #      } 
 #   ) %>% rename(scenario = Strategy)


    inmb_opt <- map_df(thresholds,
        function(k) {
            future_map2_dfr(ppds$x, ppds$y,
                function(x, y) {
                    full_psa_temp <- full_psa %>% filter(ppd_mab == x, ppd_mat == y)
                    cea(full_psa_temp, comparator = "base", k = k, sample = "s", strategy = "scenario",  e = "qaly", c = "cost")$mce %>% 
                        as.data.frame %>% filter(best == 1) %>% mutate(ppd_mab = x, ppd_mat = y, threshold = k) 
                }
            )
        }
    )
    full_psa_temp <- full_psa %>% filter(ppd_mab == 0, ppd_mat == 0)
    impact_prog <- cea(full_psa_temp, comparator = "base", k = 20000, sample = "s", strategy = "scenario",  e = "qaly", c = "cost")$summary %>% 
        select(scenario, e_mean)
    impact_order <- impact_prog %>% arrange(e_mean) %>% pull(scenario) %>% rev

    inmb_best_opt <- map_df(thresholds,
        function(k) {
            future_map2_dfr(ppds$x, ppds$y,
                function(x, y) {
                    full_psa_temp <- full_psa %>% filter(ppd_mab == x, ppd_mat == y)
                    inmb_tab <- cea(full_psa_temp, comparator = "base", k = k, sample = "s", strategy = "scenario",  e = "qaly", c = "cost")$nmb 
                    names <- inmb_tab %>% mutate(inmb = enmb - as.numeric(inmb_tab[1, 4]) ) %>% filter(inmb >= 0) %>% pull(scenario)
                    best_cea <- names[order(factor(names, levels = impact_order))][1]
                    best_cea %>% as.data.frame %>% mutate(ppd_mab = x, ppd_mat = y, threshold = k) %>% rename(scenario = ".") 
                }
            ) 
        }
    )

    evpi_opt <- map_df(c(20000, 30000),
        function(k) {
            future_map2_dfr(ppds$x, ppds$y,
                function(x, y) {
                    full_psa_temp <- full_psa %>% filter(ppd_mab == x, ppd_mat == y)
                    cea(full_psa_temp, comparator = "base", k = k, sample = "s", strategy = "scenario",  e = "qaly", c = "cost")$evpi %>% 
                        mutate(ppd_mab = x, ppd_mat = y, threshold = k) 
                }
            ) 
        }
    )

    list(inmb = inmb_opt, inmb_impact = inmb_best_opt, impact_qaly = impact_prog, evpi = evpi_opt)

}


plot_grids_oa <- function(cea_metrics, plot_name) {
    require(scales)

    relabel_sce <- c("base" = "Palivizumab progamme",
        "lav_65" = "Seasonal to 65 yrs+",
        "lav_75" = "Seasonal to 75 yrs+"
        )


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
            geom_tile(aes(x = ppd_lav, y = 1, fill = scenario, color = uncert), alpha = 0.85, size = 1.1, width=4.3, height=4.5) + 
            scale_fill_manual(values = c("gray", "darkred", "red")) + 
            scale_color_manual(values = c("white", "gray50")) +
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90), axis.text.y = element_blank()) +
            labs(x = "CCPA OA vaccine (£)",
            y = "", fill = "Optimal scenario", color = "Confidence (20,000 £/QALY)", 
            subtitle = "20,000£/QALY",
            title = "Optimal programme with using INMB") +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5)) 

    p3 <- inmb_opt_30000 %>% 
        ggplot() +  
            geom_tile(aes(x = ppd_lav, y = 1, fill = scenario, color = uncert), alpha = 0.85, size = 1.1, width=4.3, height=4.5) + 
            scale_fill_manual(values = c("gray", "darkred", "red")) + 
            scale_color_manual(values = c("white", "black")) +
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90), axis.text.y = element_blank()) +
            labs(x = "CCPA OA vaccine  (£)",
            y = "", fill = "Optimal scenario", color = "Confidence (30,000 £/QALY)", 
            subtitle = "30,000£/QALY",
            title = "Optimal programme with using INMB") +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))

    p2 <- cea_metrics$evpi %>% filter(threshold == 20000) %>% 
        ggplot() +  
            geom_tile(aes(x = ppd_lav, y = 1, fill = evpi), alpha = 0.85, size = 1.1, width=4.3, height=4.5) + 
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            labs(x = "CCPA OA vaccine (£)",
            y = "", fill = "EVPI", color = "Confidence (30,000 £/QALY)", 
                    subtitle = "20,000£/QALY",

            title = "Expected value of perfect information") +
            scale_fill_gradient2(low = "white", high = "red", labels = comma) +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))

    p4 <- cea_metrics$evpi %>% filter(threshold == 30000) %>% 
        ggplot() +  
            geom_tile(aes(x = ppd_lav, y = 1, fill = evpi), alpha = 0.85, size = 1.1, width=4.3, height=4.5) + 
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            labs(x = "CCPA OA vaccine (£)",
            y = "", fill = "EVPI", color = "Confidence (30,000 £/QALY)", 
                            subtitle = "30,000£/QALY",
            title = "Expected value of perfect information") +
                scale_fill_gradient2(low = "white", high = "red", labels = comma) +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))

    p0a <- (p1 / p2) 
    p0b <- (p3 / p4)

    p1 / p2 / p3 / p4 + plot_layout(heights = c(1,1,1,1))
    ggsave(here::here("figs", "cea", paste0(plot_name, ".pdf")), width = 15, height = 15)

}



plot_grids <- function(cea_metrics, plot_name) {


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
            geom_tile(aes(x = ppd_mat, y = ppd_mab, fill = scenario, color = uncert), alpha = 0.85, size = 1.1, width=4.3, height=4.5) + 
            scale_fill_manual(values = c("gray50", "#c07002", "#fcae44", "#0d4fb2", "#91BAd6")) + 
            scale_color_manual(values = c("white", "gray50")) +
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            labs(x = "CCPA maternal vaccination (£)",
            y = "CCPA monolconal antibodies (£)", fill = "Optimal scenario", color = "Confidence (20,000 £/QALY)", 
            subtitle = "20,000£/QALY",
            title = "Optimal programme with using INMB") +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5)) 

    p3 <- inmb_opt_30000 %>% 
        ggplot() +  
            geom_tile(aes(x = ppd_mat, y = ppd_mab, fill = scenario, color = uncert), alpha = 0.85, size = 1.1, width=4.3, height=4.5) + 
            scale_fill_manual(values = c("gray50", "#c07002", "#fcae44", "#0d4fb2", "#91BAd6")) + 
            scale_color_manual(values = c("white", "black")) +
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            labs(x = "CCPA maternal vaccination (£)",
            y = "CCPA monolconal antibodies (£)", fill = "Optimal scenario", color = "Confidence (30,000 £/QALY)", 
            subtitle = "30,000£/QALY",
            title = "Optimal programme with using INMB") +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))

    p2 <- cea_metrics$evpi %>% filter(threshold == 20000) %>% 
        ggplot() +  
            geom_tile(aes(x = ppd_mat, y = ppd_mab, fill = evpi), alpha = 0.85, size = 1.1, width=4.3, height=4.5) + 
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            labs(x = "CCPA maternal vaccination (£)",
            y = "CCPA monolconal antibodies (£)", fill = "EVPI", color = "Confidence (30,000 £/QALY)", 
                    subtitle = "20,000£/QALY",
            title = "Expected value of perfect information") +
            scale_fill_gradient2(low = "white", high = "red", labels = comma) +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))

    p4 <- cea_metrics$evpi %>% filter(threshold == 30000) %>% 
        ggplot() +  
            geom_tile(aes(x = ppd_mat, y = ppd_mab, fill = evpi), alpha = 0.85, size = 1.1, width=4.3, height=4.5) + 
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            labs(x = "CCPA maternal vaccination (£)",
            y = "CCPA monolconal antibodies (£)", fill = "EVPI", color = "Confidence (30,000 £/QALY)", 
                            subtitle = "30,000£/QALY",
            title = "Expected value of perfect information") +
                scale_fill_gradient2(low = "white", high = "red", labels = comma) +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))

    p0A <- p1 / p2 + plot_layout(guides = "collect")
    p0B <- p3 / p4 + plot_layout(guides = "collect")
    p0A | p0B 
    ggsave(here::here("figs", "cea", paste0(plot_name, ".pdf")), width = 30, height = 20)

}


plot_grids_2 <- function(cea_metrics, plot_name) {

    relabel_sce <- c("base" = "Palivizumab progamme",
        "mab_sea" = "Seasonal LA-mAbs",
        "mab_sea_cu" = "Seasonal LA-mAbs with catch-up", 
        "mat_sea" = "Seasonal maternal programme",
        "mat_yr" = "Year-round maternal programme",
        "mab_yr" = "Year-round LA-mAbs programme"
        )

    require(scales)
    inmb_opt_20000 <- cea_metrics$inmb %>% left_join(cea_metrics$impact_qaly) %>%
        mutate(scenario = recode(scenario, !!!relabel_sce)) %>% 
        filter(threshold == 20000) %>% mutate(uncert = case_when(
        prob >= 0.5~">50%",
        prob < 0.5~"<50%")   )

    inmb_opt_20000_best <- inmb_opt_20000 %>% select(!best) %>% left_join(cea_metrics$inmb_impact %>% rename(best = scenario), 
        by = c("ppd_mab", "ppd_mat", "threshold") ) %>% left_join(cea_metrics$impact_qaly %>% rename(best = scenario, e_mean_2 = e_mean)) %>%
         mutate(best = recode(best, !!!relabel_sce)) %>% 
         mutate(qaly_gain = e_mean_2 - e_mean)

    p1 <- inmb_opt_20000_best %>% 
        ggplot() +  
            geom_tile(aes(x = ppd_mat, y = ppd_mab, fill = scenario, color = best), alpha = 0.85, size = 1.1, width=4.3, height=4.5) + 
            scale_fill_manual(values = c("gray50", "#c07002", "#fcae44", "#0d4fb2", "#91BAd6")) + 
            scale_color_manual(values = c("gray50", "#c07002", "#fcae44", "#0d4fb2", "#91BAd6")) +
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            labs(x = "CCPA maternal vaccination (£)",
            y = "CCPA monolconal antibodies (£)", fill = "Most efficient CEP", color = "Most effective CEP", 
            title = "Cost-effective programmes given CCPA") +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5)) 

    p2 <- inmb_opt_20000_best %>% 
        ggplot() +  
            geom_tile(aes(x = ppd_mat, y = ppd_mab, fill = qaly_gain), alpha = 0.85, size = 1.1, width=4.3, height=4.5) + 
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            scale_fill_gradient2(low = "white", high = "green", labels = comma, limits = c(0, 5000)) +
            labs(x = "CCPA maternal vaccination (£)",
            y = "CCPA monolconal antibodies (£)", fill = "Discounted ICER gain", 
            title = "Health gain from choosing most effective CEP\ninstead of most efficient CEP") +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5)) 

    p3 <- cea_metrics$evpi %>% filter(threshold == 20000) %>% 
        ggplot() +  
            geom_tile(aes(x = ppd_mat, y = ppd_mab, fill = evpi), alpha = 0.85, size = 1.1, width=4.3, height=4.5) + 
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            labs(x = "CCPA maternal vaccination (£)",
            y = "CCPA monolconal antibodies (£)", fill = "EVPI", 
            title = "Expected value of perfect information") +
            scale_fill_gradient2(low = "white", high = "red", labels = comma) +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))

    (p1) / (p2 & p3) + plot_layout(heights = c(1.3, 1))
    ggsave(here::here("figs", "cea",  paste0(plot_name, "_impact.pdf")), width = 22, height = 20)


}

plot_grids_alt <- function(cea_metrics, plot_name) {


    relabel_sce <- c("base" = "Palivizumab progamme",
        "mab_sea" = "Seasonal LA-mAbs",
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
            geom_tile(aes(x = ppd_mat, y = ppd_mab, fill = scenario, color = uncert), alpha = 0.85, size = 1.1, width=4.3, height=4.5) + 
         #  scale_fill_manual(values = c("gray50", "#c07002", "#fcae44", "#0d4fb2", "#91BAd6")) + 
            scale_fill_manual(values = c("gray50", "#c07002", "#0d4fb2", "#fcae44",  "#91BAd6")) + 
            scale_color_manual(values = c("white", "gray50")) +
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            labs(x = "CCPA maternal vaccination (£)",
            y = "CCPA monolconal antibodies (£)", fill = "Optimal scenario", color = "Confidence (20,000 £/QALY)", 
            subtitle = "20,000£/QALY",
            title = "Optimal programme with using INMB") +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5)) 

    p3 <- inmb_opt_30000 %>% 
        ggplot() +  
            geom_tile(aes(x = ppd_mat, y = ppd_mab, fill = scenario, color = uncert), alpha = 0.85, size = 1.1, width=4.3, height=4.5) + 
            scale_fill_manual(values = c("gray50", "#c07002", "#fcae44", "#0d4fb2", "#91BAd6")) + 
            scale_color_manual(values = c("white", "black")) +
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            labs(x = "CCPA maternal vaccination (£)",
            y = "CCPA monolconal antibodies (£)", fill = "Optimal scenario", color = "Confidence (30,000 £/QALY)", 
            subtitle = "30,000£/QALY",
            title = "Optimal programme with using INMB") +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))

    p2 <- cea_metrics$evpi %>% filter(threshold == 20000) %>% 
        ggplot() +  
            geom_tile(aes(x = ppd_mat, y = ppd_mab, fill = evpi), alpha = 0.85, size = 1.1, width=4.3, height=4.5) + 
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            labs(x = "CCPA maternal vaccination (£)",
            y = "CCPA monolconal antibodies (£)", fill = "EVPI", color = "Confidence (30,000 £/QALY)", 
                    subtitle = "20,000£/QALY",
            title = "Expected value of perfect information") +
            scale_fill_gradient2(low = "white", high = "red", labels = comma) +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))

    p4 <- cea_metrics$evpi %>% filter(threshold == 30000) %>% 
        ggplot() +  
            geom_tile(aes(x = ppd_mat, y = ppd_mab, fill = evpi), alpha = 0.85, size = 1.1, width=4.3, height=4.5) + 
            theme_bw() + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
            labs(x = "CCPA maternal vaccination (£)",
            y = "CCPA monolconal antibodies (£)", fill = "EVPI", color = "Confidence (30,000 £/QALY)", 
                            subtitle = "30,000£/QALY",
            title = "Expected value of perfect information") +
                scale_fill_gradient2(low = "white", high = "red", labels = comma) +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))  +
        scale_y_continuous(expand = c(0, 0), breaks = seq(0, 200, 5))

    p0A <- p1 / p2 + plot_layout(guides = "collect")
    p0B <- p3 / p4 + plot_layout(guides = "collect")
    p0A | p0B 
    ggsave(here::here("figs", "cea", paste0(plot_name, ".pdf")), width = 30, height = 20)

}