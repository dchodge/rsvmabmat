
plot_burden_by_age_group <- function() {
    source(here::here("R", "utils.R"))

    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_pal.RData")) # RSV_mat_pal
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_pal.RData")) # RSV_mab_pal

    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_s.RData")) # RSV_mat_yr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_yr.RData")) # RSV_mat_yr

    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_s.RData")) # RSV_mab_yr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_s_cu.RData")) # RSV_mab_yr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_yr.RData")) # RSV_mab_yr

    base_mat_qaly <- RSV_mat_pal@outcomes$qaly %>% ungroup %>% summarise(qaly = sum(qaly), .by = c(s,  outcome, age_group)) %>%
        summarise(qaly = mean(qaly), .by = c( outcome, age_group) ) %>% as.data.frame %>% rename(base_qaly = qaly)

    base_mab_qaly <- RSV_mab_pal@outcomes$qaly %>% ungroup %>% summarise(qaly = sum(qaly), .by = c(s, outcome, age_group)) %>%
        summarise(qaly = mean(qaly), .by = c( outcome, age_group) ) %>% as.data.frame %>% rename(base_qaly = qaly)

    regroup <- c(rep("<6mo", 6), rep("6-11mo", 6), rep("1-4yr", 4), rep("5-14yr", 2), rep("15-44yr", 3), rep("45+yr", 4))

    summary_mab_qaly_s <- get_summary_outcome_qaly_age(RSV_mab_s, base_mab_qaly)
    summary_mab_qaly_s_cu <- get_summary_outcome_qaly_age(RSV_mab_s_cu, base_mab_qaly)
    summary_mab_qaly_yr <- get_summary_outcome_qaly_age(RSV_mab_yr, base_mab_qaly)

    summary_mat_qaly_s <- get_summary_outcome_qaly_age(RSV_mat_s, base_mat_qaly)
    summary_mat_qaly_yr <- get_summary_outcome_qaly_age(RSV_mat_yr, base_mat_qaly)

    summary_qaly <- bind_rows(
        summary_mab_qaly_s %>% summarise(qaly = sum(qaly_diff), .by = c(age_group2)) %>% mutate(type = "ll-mAB\n seasonal"),
        summary_mab_qaly_s_cu %>% summarise(qaly = sum(qaly_diff), .by = c(age_group2)) %>% mutate(type = "ll-mAB\n seasonal with catch-up"),
        summary_mab_qaly_yr %>% summarise(qaly = sum(qaly_diff), .by = c(age_group2)) %>% mutate(type = "ll-mAB\n year-round"),

        summary_mat_qaly_s %>% summarise(qaly = sum(qaly_diff), .by = c(age_group2))  %>% mutate(type = "MV\n seasonal"),
        summary_mat_qaly_yr %>% summarise(qaly = sum(qaly_diff), .by = c(age_group2))  %>% mutate(type = "MV\n year-round"),

    ) %>% mutate(age_group2 = factor(age_group2, levels = unique(regroup))) %>%
        group_by(type) %>% mutate(prop = qaly / sum(qaly))

    p1 <- summary_qaly %>%
        ggplot() + geom_col(aes(type, prop, fill = age_group2), position = "dodge") + 
        labs(x = "Programme", y = "Proportion of total QALY loss", fill = "Age group") + theme_bw() + 
        theme(text = element_text(size = 15))

    base_mat_cost <- RSV_mat_pal@outcomes$cost %>% ungroup %>% summarise(cost = sum(cost), .by = c(s,  outcome, age_group)) %>%
        summarise(cost = mean(cost), .by = c( outcome, age_group) ) %>% as.data.frame %>% rename(base_cost = cost)

    base_mab_cost <- RSV_mab_pal@outcomes$cost %>% ungroup %>% summarise(cost = sum(cost), .by = c(s, outcome, age_group)) %>%
        summarise(cost = mean(cost), .by = c( outcome, age_group) ) %>% as.data.frame %>% rename(base_cost = cost)

    summary_mab_cost_s <- get_summary_outcome_cost_age(RSV_mab_s, base_mab_cost)
    summary_mab_cost_s_cu <- get_summary_outcome_cost_age(RSV_mab_s_cu, base_mab_cost)
    summary_mab_cost_yr <- get_summary_outcome_cost_age(RSV_mab_yr, base_mab_cost)

    summary_mat_cost_s <- get_summary_outcome_cost_age(RSV_mat_s, base_mat_cost)
    summary_mat_cost_yr <- get_summary_outcome_cost_age(RSV_mat_yr, base_mat_cost)

    summary_cost <- bind_rows(
        summary_mab_cost_s %>% summarise(cost = sum(cost_diff), .by = c(age_group2)) %>% mutate(type = "ll-mAB\n seasonal"),
        summary_mab_cost_s_cu %>% summarise(cost = sum(cost_diff), .by = c(age_group2)) %>% mutate(type = "ll-mAB\n seasonal with catch-up"),
        summary_mab_cost_yr %>% summarise(cost = sum(cost_diff), .by = c(age_group2)) %>% mutate(type = "ll-mAB\n year-round"),

        summary_mat_cost_s %>% summarise(cost = sum(cost_diff), .by = c(age_group2))  %>% mutate(type = "MV\n seasonal"),
        summary_mat_cost_yr %>% summarise(cost = sum(cost_diff), .by = c(age_group2))  %>% mutate(type = "MV\n year-round"),

    ) %>% mutate(age_group2 = factor(age_group2, levels = unique(regroup))) %>%
        group_by(type) %>% mutate(prop = cost / sum(cost))

    p2 <- summary_cost %>%
        ggplot() + geom_col(aes(type, prop, fill = age_group2), position = "dodge") + 
        labs(x = "Programme", y = "Proportion of total cost saving", fill = "Age group") + theme_bw() + 
        theme(text = element_text(size = 15))


    p1 / p2 
}



plot_burden_by_outcomes <- function() {
    source(here::here("R", "utils.R"))

    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_pal.RData")) # RSV_mat_pal
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_pal.RData")) # RSV_mab_pal

    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_s.RData")) # RSV_mat_yr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mat_yr.RData")) # RSV_mat_yr

    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_s.RData")) # RSV_mab_yr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_s_cu.RData")) # RSV_mab_yr
    load(file = here::here("outputs", "scenarios", "opt_unbound_ss", "RSV_mab_yr.RData")) # RSV_mab_yr

    relabel_outcomes <- c("symptomatic" = "Symptomatic cases", "gp" = "GP consultations", 
        "death" = "Deaths", "hosp" = "Hospital cases", "icu" = "ICU admissions", "a_e" = "A+E visits") 

        
    base_mat_qaly <- RSV_mat_pal@outcomes$qaly %>% ungroup %>% summarise(qaly = sum(qaly), .by = c(s, outcome, age_group)) %>%
        summarise(qaly = mean(qaly), .by = c( outcome, age_group) ) %>% as.data.frame %>% rename(base_qaly = qaly)

    base_mab_qaly <- RSV_mab_pal@outcomes$qaly %>% ungroup %>% summarise(qaly = sum(qaly), .by = c(s, outcome, age_group)) %>%
        summarise(qaly = mean(qaly), .by = c( outcome, age_group) ) %>% as.data.frame %>% rename(base_qaly = qaly)

    regroup <- c(rep("<6mo", 6), rep("6-11mo", 6), rep("1-4yr", 4), rep("5-14yr", 2), rep("15-44yr", 3), rep("45+yr", 4))

    summary_mab_qaly_s <- get_summary_outcome_qaly(RSV_mab_s, base_mab_qaly)
    summary_mab_qaly_s_cu <- get_summary_outcome_qaly(RSV_mab_s_cu, base_mab_qaly)
    summary_mab_qaly_yr <- get_summary_outcome_qaly(RSV_mab_yr, base_mab_qaly)

    summary_mat_qaly_s <- get_summary_outcome_qaly(RSV_mat_s, base_mat_qaly)
    summary_mat_qaly_yr <- get_summary_outcome_qaly(RSV_mat_yr, base_mat_qaly)

    summary_qaly <- bind_rows(
        summary_mab_qaly_s %>% summarise(qaly = sum(qaly_diff), .by = c(outcome)) %>% mutate(type = "ll-mAB\n seasonal"),
        summary_mab_qaly_s_cu %>% summarise(qaly = sum(qaly_diff), .by = c(outcome)) %>% mutate(type = "ll-mAB\n seasonal with catch-up"),
        summary_mab_qaly_yr %>% summarise(qaly = sum(qaly_diff), .by = c(outcome)) %>% mutate(type = "ll-mAB\n year-round"),

        summary_mat_qaly_s %>% summarise(qaly = sum(qaly_diff), .by = c(outcome))  %>% mutate(type = "MV\n seasonal"),
        summary_mat_qaly_yr %>% summarise(qaly = sum(qaly_diff), .by = c(outcome))  %>% mutate(type = "MV\n year-round"),

    ) %>% 
        mutate(outcome = recode(outcome, !!!relabel_outcomes)) %>%
        group_by(type) %>% mutate(prop = qaly / sum(qaly))

    p3 <- summary_qaly %>%
        ggplot() + geom_col(aes(type, prop, fill = outcome), position = "dodge") + 
        labs(x = "Programme", y = "Proportion of total QALY loss", fill = "Healthcare outcome") + theme_bw() + 
        theme(text = element_text(size = 15)) + scale_fill_hue(l=40)

    base_mat_cost <- RSV_mat_pal@outcomes$cost %>% ungroup %>% summarise(cost = sum(cost), .by = c(s,  outcome, age_group)) %>%
        summarise(cost = mean(cost), .by = c( outcome, age_group) ) %>% as.data.frame %>% rename(base_cost = cost)

    base_mab_cost <- RSV_mab_pal@outcomes$cost %>% ungroup %>% summarise(cost = sum(cost), .by = c(s, outcome, age_group)) %>%
        summarise(cost = mean(cost), .by = c( outcome, age_group) ) %>% as.data.frame %>% rename(base_cost = cost)

    regroup <- c(rep("<6mo", 6), rep("6-11mo", 6), rep("1-4yr", 4), rep("5-14yr", 2), rep("15-44yr", 3), rep("45+yr", 4))

    summary_mab_cost_s <- get_summary_outcome_cost(RSV_mab_s, base_mab_cost)
    summary_mab_cost_s_cu <- get_summary_outcome_cost(RSV_mab_s_cu, base_mab_cost)
    summary_mab_cost_yr <- get_summary_outcome_cost(RSV_mab_yr, base_mab_cost)

    summary_mat_cost_s <- get_summary_outcome_cost(RSV_mat_s, base_mat_cost)
    summary_mat_cost_yr <- get_summary_outcome_cost(RSV_mat_yr, base_mat_cost)

    summary_cost <- bind_rows(
        summary_mab_cost_s %>% summarise(cost = sum(cost_diff), .by = c(outcome)) %>% mutate(type = "ll-mAB\n seasonal"),
        summary_mab_cost_s_cu %>% summarise(cost = sum(cost_diff), .by = c(outcome)) %>% mutate(type = "ll-mAB\n seasonal with catch-up"),
        summary_mab_cost_yr %>% summarise(cost = sum(cost_diff), .by = c(outcome)) %>% mutate(type = "ll-mAB\n year-round"),

        summary_mat_cost_s %>% summarise(cost = sum(cost_diff), .by = c(outcome))  %>% mutate(type = "MV\n seasonal"),
        summary_mat_cost_yr %>% summarise(cost = sum(cost_diff), .by = c(outcome))  %>% mutate(type = "MV\n year-round")
    )  %>% 
        mutate(outcome = recode(outcome, !!!relabel_outcomes)) %>%
        group_by(type) %>% mutate(prop = cost / sum(cost))

    p4 <- summary_cost %>%
        ggplot() + geom_col(aes(type, prop, fill = outcome), position = "dodge") +
        labs(x = "Programme", y = "Proportion of total cost saving", fill = "Healthcare outcome") + theme_bw() + 
        theme(text = element_text(size = 15)) + scale_fill_hue(l=40)



    p3 / p4 

}


run_plot_2d_cea_no_cu <- function(string_name, ppds, thresholds, scaling = c(1, 1, 1, 1), type_string = "A") {
    
    string_name <- "unbound_ss"
    scaling <- c(1, 1, 1, 1)
    cea_table <- load_cea_table_no_cu(paste0("opt_", string_name))


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
    write_rds(cea_metrics, file = here::here("outputs", "scenarios", paste0("opt_", string_name), paste0("cea_metrics_", type_string, ".rds")))
    if (type_string == "A") {
        string_name_plot <- string_name
    } else {
        string_name_plot <- paste0(string_name, "_", type_string)
    }
    plot_grids(cea_metrics, string_name_plot)
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



run_plot_2d_cea_zoom <- function(string_name, ppds, thresholds) {
    cea_table <- load_cea_table(paste0("opt_", string_name))
    cat("Generating ppd samples", string_name, "\n")
    full_psa <- generate_ppd_samples(cea_table, ppds)
    cea_metrics <- generate_cea_metrics(full_psa, ppds, thresholds)
    cea_metrics
}

run_plot_2d_cea_zoom_no_cu <- function(string_name, ppds, thresholds) {
    cea_table <- load_cea_table_no_cu(paste0("opt_", string_name))
    cat("Generating ppd samples", string_name, "\n")
    full_psa <- generate_ppd_samples(cea_table, ppds)
    cea_metrics <- generate_cea_metrics(full_psa, ppds, thresholds)
    cea_metrics
}

get_detailed_info <- function(string_name, string_name_alt, threshold) {
    unbound_ss_zoom_mat <- run_plot_2d_cea_zoom(string_name, ppds_zoom_mat, thresholds)$inmb %>% 
        mutate(product = "Maternal vac programme\n (if la-mAB expensive)") %>% rename(ppd = ppd_mat, ppd_fixed = ppd_mab)
    unbound_ss_zoom_mab <- run_plot_2d_cea_zoom(string_name, ppds_zoom_mab, thresholds)$inmb %>% 
        mutate(product = "LA-mAB programme\n (if mat-vac expensive)")  %>% rename(ppd = ppd_mab, ppd_fixed = ppd_mat)

    bind_rows(unbound_ss_zoom_mat, unbound_ss_zoom_mab) %>% filter(threshold == !!threshold) %>% mutate(sens = string_name_alt)
}

get_detailed_info_no_cu <- function(string_name, string_name_alt, threshold) {
    unbound_ss_zoom_mat <- run_plot_2d_cea_zoom_no_cu(string_name, ppds_zoom_mat, thresholds)$inmb %>% 
        mutate(product = "Maternal vac programme\n (if la-mAB expensive)") %>% rename(ppd = ppd_mat, ppd_fixed = ppd_mab)
    unbound_ss_zoom_mab <- run_plot_2d_cea_zoom_no_cu(string_name, ppds_zoom_mab, thresholds)$inmb %>% 
        mutate(product = "LA-mAB programme\n (if mat-vac expensive)")  %>% rename(ppd = ppd_mab, ppd_fixed = ppd_mat)

    bind_rows(unbound_ss_zoom_mat, unbound_ss_zoom_mab) %>% filter(threshold == !!threshold) %>% mutate(sens = string_name_alt)
}


generate_finer_df <- function() {

    summarise_detailed <- bind_rows(
        get_detailed_info("unbound_ss", "Base case", 20000),
        get_detailed_info("unbound_ss", "ICER at \n30,000 £/QALY", 30000),
        get_detailed_info_no_cu("unbound_ss", "60% mat vac\n90% la-maB", 20000),
        get_detailed_info("unbound_ss_50_90", "50% mat vac\n90% la-maB", 20000),
        get_detailed_info("unbound_ss_70_70", "70% mat vac\n70% la-maB", 20000),
        get_detailed_info("unbound_ss_70_90", "70% mat vac\n90% la-maB", 20000),
        get_detailed_info("unbound_ss_50_70", "50% mat vac\n70% la-maB", 20000),
        get_detailed_info("unbound_ss_90_70", "90% mat vac\n70% la-maB", 20000)
    )


    relabel_sce <- c("base" = "Palivizumab progamme",
        "mab_sea" = "Seasonal LA-mAbs",
        "mab_sea_cu" = "Seasonal LA-mAbs with catch-up", 
        "mat_sea" = "Seasonal maternal programme",
        "mat_yr" = "Year-round maternal programme",
        "mab_yr" = "Year-round LA-mAbs programme"
        )

    relabel_sens <- c(
        "Base case", 
        "ICER at \n30,000 £/QALY", 
        "60% mat vac\n90% la-maB", 
        "50% mat vac\n90% la-maB",  
        "70% mat vac\n70% la-maB",  
        "70% mat vac\n90% la-maB", 
        "50% mat vac\n70% la-maB", 
        "90% mat vac\n70% la-maB"
    )
    summarise_detailed %>% 
        mutate(scenario = recode(scenario, !!!relabel_sce)) %>% 
    # mutate(sens = recode(sens, !!!relabel_sens)) %>% 
        mutate(sens = factor(sens, levels = relabel_sens)) 
}

plot_finer <- function(summarise_detailed) {
    summarise_detailed %>% 
        ggplot() + geom_line(aes(x = ppd, y = prob, color = scenario), size = 3) + 
        facet_grid(vars(sens), vars(product)) + theme_bw() + 
        labs(x = "CCPA", y = "Probability optimal", color = "Scenario") + 
        scale_color_manual(values= c("gray", "#c07002", "#fcae44", "#0d4fb2","#f0de16",  "#91BAd6")) +
        scale_x_continuous(breaks = seq(0, 200, 20)) +
        theme(text = element_text(size = 16))
}