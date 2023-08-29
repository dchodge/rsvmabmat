get_mean_95ci <- function(value) {
    bounds <- quantile(value, c(0.025, 0.975)) %>% as.numeric
    data.frame(
        "mean" = mean(value),
        "lb" = bounds[1],
        "ub" = bounds[2]
    )
}

get_doses <- function(x, name, product, no_years = 10, discount = TRUE) {
        doses_vhr <- doses_mab <- doses_mat <- doses_lav <- 0
        if (product == "mAB") {
            cal_name <- "mAB_LR"
        } else if (product %in% c("mat", "none") ) {
            cal_name <- "mat_LR"
        } else if (product == "lav") {
            cal_name <- "LAV_LR"
        }
        discount_rate <- x@econ_par$discount_rate

        if (discount) {
            discount_factor <- exp(-(1:(no_years*365)) * discount_rate / 365)
        } else {
            discount_factor <- rep(1, no_years*365)
        }
        allvals <- t(x@dose_calendar$mAB_VHR) * (x@uk_data$populationAgeGroup * x@uk_data$pVHR) 
        doses_vhr <- (rep(allvals %>% apply(2, sum), no_years) * discount_factor) %>% sum
        if (x@cov_mat > 0) {
            allvals <- t(x@dose_calendar[[cal_name]]) * (x@uk_data$populationAgeGroup * x@uk_data$prop_mat) 
            doses <- ((rep(allvals %>% apply(2, sum), no_years) * discount_factor) %>% sum) * x@cov_mat
        } else {
            allvals <- t(x@dose_calendar[[cal_name]]) * (x@uk_data$populationAgeGroup) 
            doses <- ((rep(allvals %>% apply(2, sum), no_years) * discount_factor) %>% sum)
        }
        if (product == "mAB") {
            doses_mab <- doses
        } else if (product == "mat") {
            doses_mat <- doses
        } else if (product == "lav") {
            doses_lav <- doses
        }
        data.frame(
            "scenario" = name,
            doses_vhr = doses_vhr,
            doses_mab = doses_mab,
            doses_mat = doses_mat,
            doses_lav = doses_lav
        )
}

summarise_outcomes <- function(x) {
    x@outcomes$outcomes %>% group_by(s, outcome, age_group) %>% summarise(cases_total = sum(cases) )
}

get_summary_outcome_qaly_age <- function(inter, base) {
    regroup <- c(rep("<6mo", 6), rep("6-11mo", 6), rep("1-4yr", 4), rep("5-14yr", 2), rep("15-44yr", 3), rep("45+yr", 4))

    inter@outcomes$qaly %>% ungroup %>% summarise(qaly = sum(qaly), .by = c(s, outcome, age_group)) %>%
        summarise(qaly = mean(qaly), .by = c(outcome, age_group) ) %>% as.data.frame %>% left_join(base) %>%
        mutate(qaly_diff = (base_qaly - qaly) ) %>%
        summarise(qaly_diff = sum(qaly_diff), .by = c(age_group)) %>%
        mutate(age_group2 = regroup)
}

get_summary_outcome_cost_age <- function(inter, base) {
    regroup <- c(rep("<6mo", 6), rep("6-11mo", 6), rep("1-4yr", 4), rep("5-14yr", 2), rep("15-44yr", 3), rep("45+yr", 4))

    inter@outcomes$cost %>% ungroup %>% summarise(cost = sum(cost), .by = c(s, outcome, age_group)) %>%
        summarise(cost = mean(cost), .by = c(outcome, age_group) ) %>% as.data.frame %>% left_join(base) %>%
        mutate(cost_diff = (base_cost - cost) ) %>%
        summarise(cost_diff = sum(cost_diff), .by = c(age_group)) %>%
        mutate(age_group2 = regroup)
}

get_summary_outcome_qaly <- function(inter, base) {
    inter@outcomes$qaly %>% ungroup %>% summarise(qaly = sum(qaly), .by = c(s, outcome, age_group)) %>%
        summarise(qaly = mean(qaly), .by = c(outcome, age_group) ) %>% as.data.frame %>% left_join(base) %>%
        mutate(qaly_diff = (base_qaly - qaly) ) %>%
        summarise(qaly_diff = sum(qaly_diff), .by = c(outcome)) 
}

get_summary_outcome_cost <- function(inter, base) {
    inter@outcomes$cost %>% ungroup %>% summarise(cost = sum(cost), .by = c(s, outcome, age_group)) %>%
        summarise(cost = mean(cost), .by = c(outcome, age_group) ) %>% as.data.frame %>% left_join(base) %>%
        mutate(cost_diff = (base_cost - cost) ) %>%
        summarise(cost_diff = sum(cost_diff), .by = c(outcome))  
}


ppds_zoom_mab <- crossing(x = seq(0, 200, 1), y = 200)
ppds_zoom_mat <- crossing(y = seq(0, 200, 1), x = 200)
thresholds <- c(20000, 30000)