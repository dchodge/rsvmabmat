###
add_risks_and_economics <- function(RSVempty) {
    econ_raw_ss <- read.csv(file = here::here("data", "econ", "econ_pars_ss.csv")) 
    risks_vhr_raw <- read.csv(file = here::here("data", "econ", "outcome_risks_vhr.csv"))

    outcomes_incidence <- read.csv(file = here::here("data", "econ", "outcomes_incidence.csv"))
    model_cases_sample_mean_get <- load(file = here::here("data", "model_cases_sample_mean.RData"))
    model_cases_sample_mean <- get(model_cases_sample_mean_get)

    # This function then converts the incidence of the outcome to the risk per infection
    risks_raw <- covert_raw_to_risk(RSVempty, outcomes_incidence, model_cases_sample_mean)

    RSVempty_ss <- add_economics(RSVempty, econ_name = "E_W2023", econ_raw_ss, risks_raw, risks_vhr_raw)
}


## Function to run the scenarios for mats
run_scenarios <- function(RSVempty, immune_profile, file, cov) {
    if (!dir.exists(here::here("outputs", "scenarios", file))) {
        # create the folder if it doesn't exist
        dir.create(here::here("outputs", "scenarios", file))
    }

    # Load calendars programmes 
    ## Maternal programmes 
    cal_mat_none <- read.csv(file = here::here("data", "calendars", "cal_mat_none.csv")) 
    cal_vhr_s <- read.csv(file = here::here("data", "calendars", "cal_vhr_s.csv")) 
    cal_mat_s <- read.csv(file = here::here("data", "calendars", "cal_mat_s_opt.csv")) 
    cal_mat_yr <- read.csv(file = here::here("data", "calendars", "cal_mat_yr.csv")) 

    ## Monoclonal programmes 
    cal_none <- read.csv(file = here::here("data", "calendars", "cal_none.csv")) 
    cal_vhr_s <- read.csv(file = here::here("data", "calendars", "cal_vhr_s.csv")) 
    cal_mabs_s <- read.csv(file = here::here("data", "calendars", "cal_mabs_s_opt.csv")) 
    cal_mabs_s_cu <- read.csv(file = here::here("data", "calendars", "cal_mabs_s_cu_opt.csv")) 
    cal_mabs_yr <- read.csv(file = here::here("data", "calendars", "cal_mabs_yr.csv")) 

    # Change coverage maternal programmes
    mat_cov <- cov[1]
    cal_mat_s$coverage[cal_mat_s$coverage > 0] <- mat_cov
    cal_mat_yr$coverage[cal_mat_yr$coverage > 0] <- mat_cov

    # Change covarge monoclonal programmes
    mab_cov <- cov[2]
    cal_mabs_s$coverage[cal_mabs_s$coverage > 0] <- mab_cov
    cal_mabs_s_cu$coverage[cal_mabs_s_cu$coverage > 0] <- mab_cov
    cal_mabs_yr$coverage[cal_mabs_yr$coverage > 0] <- mab_cov

    # Create class to runn
    ## Maternal programme
    RSV_mat_pal <- add_programme(RSVempty, prog_name = "mat_pal", cal_mat_none, cal_vhr_s, immune_profile$mat)
    RSV_mat_vhr <- add_programme(RSVempty, prog_name = "mat_vhr", cal_mat_none, cal_vhr_s, immune_profile$mat_vhr)
    RSV_mat_s <- add_programme(RSVempty, prog_name = "mat_s", cal_mat_s, cal_vhr_s, immune_profile$mat)
    RSV_mat_yr <- add_programme(RSVempty, prog_name = "mat_yr", cal_mat_yr, cal_vhr_s, immune_profile$mat)
    ## Monoclonal programme
    RSV_mab_pal <- add_programme(RSVempty, prog_name = "pal", cal_none, cal_vhr_s, immune_profile$mabs)
    RSV_mab_vhr <- add_programme(RSVempty, prog_name = "mab_vhr", cal_none, cal_vhr_s, immune_profile$mabs_vhr)
    RSV_mab_s <- add_programme(RSVempty, prog_name = "mab_s", cal_mabs_s, cal_vhr_s, immune_profile$mabs)
    RSV_mab_s_cu <- add_programme(RSVempty, prog_name = "mab_s_cu", cal_mabs_s_cu, cal_vhr_s, immune_profile$mabs)
    RSV_mab_yr <- add_programme(RSVempty, prog_name = "mab_yr", cal_mabs_yr, cal_vhr_s, immune_profile$mabs)

    RSV_mat_pal <- rsvie::run(RSV_mat_pal)
    save(RSV_mat_pal, file = here::here("outputs", "scenarios", file, "RSV_mat_pal.RData"))
    rm(RSV_mat_pal)

    RSV_mat_vhr <- rsvie::run(RSV_mat_vhr)
    save(RSV_mat_vhr, file = here::here("outputs", "scenarios", file, "RSV_mat_vhr.RData"))
    rm(RSV_mat_vhr)

    RSV_mat_s <- rsvie::run(RSV_mat_s)
    save(RSV_mat_s, file = here::here("outputs", "scenarios", file, "RSV_mat_s.RData"))
    rm(RSV_mat_s)

    RSV_mat_yr <- rsvie::run(RSV_mat_yr)
    save(RSV_mat_yr, file = here::here("outputs", "scenarios", file, "RSV_mat_yr.RData"))
    rm(RSV_mat_yr)

    RSV_mab_pal <- rsvie::run(RSV_mab_pal)
    save(RSV_mab_pal, file = here::here("outputs", "scenarios", file, "RSV_mab_pal.RData"))
    rm(RSV_mab_pal)

    RSV_mab_vhr <- rsvie::run(RSV_mab_vhr)
    save(RSV_mab_vhr, file = here::here("outputs", "scenarios", file, "RSV_mab_vhr.RData"))
    rm(RSV_mab_vhr)

    RSV_mab_s <- rsvie::run(RSV_mab_s)
    save(RSV_mab_s, file = here::here("outputs", "scenarios", file, "RSV_mab_s.RData"))
    rm(RSV_mab_s)

    RSV_mab_s_cu <- rsvie::run(RSV_mab_s_cu)
    save(RSV_mab_s_cu, file = here::here("outputs", "scenarios", file, "RSV_mab_s_cu.RData"))
    rm(RSV_mab_s_cu)

    RSV_mab_yr <- rsvie::run(RSV_mab_yr)
    save(RSV_mab_yr, file = here::here("outputs", "scenarios", file, "RSV_mab_yr.RData"))
    rm(RSV_mab_yr)
}

# RSVIE package, load an empty programmes and add economics and risks
run_base_sims <- function(RSVempty) {
    immune_profiles_unbound <- readRDS(here::here("data", "efficacies", "immune_profiles_unbound.RDS"))

    run_scenarios(RSVempty, immune_profiles_unbound, "opt_unbound_ss", cov = c(0.6, 0.9))
}

run_coverages_sims <- function(RSVempty) {
    immune_profiles_unbound <- readRDS(here::here("data", "efficacies", "immune_profiles_unbound.RDS"))

    run_scenarios(RSVempty, immune_profiles_unbound, "opt_unbound_ss_50_70", cov = c(0.5, 0.7))
    run_scenarios(RSVempty, immune_profiles_unbound, "opt_unbound_ss_60_70", cov = c(0.6, 0.7))
    run_scenarios(RSVempty, immune_profiles_unbound, "opt_unbound_ss_70_70", cov = c(0.7, 0.7))
    run_scenarios(RSVempty, immune_profiles_unbound, "opt_unbound_ss_80_70", cov = c(0.8, 0.7))
    run_scenarios(RSVempty, immune_profiles_unbound, "opt_unbound_ss_90_70", cov = c(0.9, 0.7))


    run_scenarios(RSVempty, immune_profiles_unbound, "opt_unbound_ss_50_80", cov = c(0.5, 0.8))
    run_scenarios(RSVempty, immune_profiles_unbound, "opt_unbound_ss_60_80", cov = c(0.6, 0.8))
    run_scenarios(RSVempty, immune_profiles_unbound, "opt_unbound_ss_70_80", cov = c(0.7, 0.8))
    run_scenarios(RSVempty, immune_profiles_unbound, "opt_unbound_ss_80_80", cov = c(0.8, 0.8))
    run_scenarios(RSVempty, immune_profiles_unbound, "opt_unbound_ss_90_80", cov = c(0.9, 0.8))


    run_scenarios(RSVempty, immune_profiles_unbound, "opt_unbound_ss_50_90", cov = c(0.5, 0.9))
    run_scenarios(RSVempty, immune_profiles_unbound, "opt_unbound_ss_60_90", cov = c(0.6, 0.9))
    run_scenarios(RSVempty, immune_profiles_unbound, "opt_unbound_ss_70_90", cov = c(0.7, 0.9))
    run_scenarios(RSVempty, immune_profiles_unbound, "opt_unbound_ss_80_90", cov = c(0.8, 0.9))
    run_scenarios(RSVempty, immune_profiles_unbound, "opt_unbound_ss_90_90", cov = c(0.9, 0.9))
}
