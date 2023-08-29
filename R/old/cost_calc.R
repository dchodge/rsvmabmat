# Load libraries
library(here)
library(tidyverse)

# Read in data
read_csv_cost_collection <- function(file_name) {
    read.csv(here::here("data-raw", file_name)) %>% as.data.frame
}

# Clean data
clean_cost_collection_add_long <- function(df) {
    df %>% mutate(act_tot = Activity + Activity.Long.Stay,
        av_unit_cost = (Total.Cost + Total.Cost.Long.Stay) / (Activity + Activity.Long.Stay) )
}

clean_cost_collection_short_only <- function(df) {
    df %>% mutate(act_tot = Activity,
        av_unit_cost = (Total.Cost) / (Activity) )
}

# Calculate probabilities
calc_probs <- function(df, header) {
    header_l <- str2lang(header)
    df %>% mutate(probs = !!header_l / sum(!!header_l))
}

# Create dataframe
create_df <- function(df, outcome, header) {
    data.frame(
        label = df$Currency.Description,
        ppe = df[[header]],
        probs = df$prob,
        outcome = outcome
    )
}

# Plot and save outcome
plot_and_save_outcome <- function(df, filename) {
    df %>%
        ggplot() + geom_col(aes(x  = ppe, y = probs, fill = label)) +
        theme_bw() +
        facet_wrap(vars(outcome)) +
        labs(x = "Price per episode", y = "Probability",
            fill = "CC Description",
            title = "Cost for peadiatric hospitalisation by CC")
    ggsave(here::here("figs", paste0(filename, ".pdf")))
}

# Read in data
hosp_pead_ls <- read_csv_cost_collection("hosp_pead.csv") %>%
    clean_cost_collection_add_long %>% calc_probs("act_tot") %>%
        create_df("Hospitalisations peadiatric", "av_unit_cost")
hosp_pead_ss <- read_csv_cost_collection("hosp_pead.csv") %>%
    clean_cost_collection_short_only %>% calc_probs("act_tot") %>%
        create_df("Hospitalisations peadiatric", "av_unit_cost")

hosp_adults_ls <- read_csv_cost_collection("hosp_adults.csv") %>%
    clean_cost_collection_add_long %>% calc_probs("act_tot") %>%
    create_df("Hospitalisations adults", "av_unit_cost")
hosp_adults_ss <- read_csv_cost_collection("hosp_adults.csv") %>%
    clean_cost_collection_short_only %>% calc_probs("act_tot") %>%
    create_df("Hospitalisations adults", "av_unit_cost")

cc_peadiatric <- read_csv_cost_collection("cc_peadiatric.csv") %>%
    calc_probs("Activity") %>%
    create_df("ICU peadiatric", "Unit.Cost")
cc_adult <- read_csv_cost_collection("cc_adult.csv") %>%
    calc_probs("Activity") %>%
    create_df("ICU adults", "Unit.Cost")

a_e_all <- read_csv_cost_collection("a_e_costs.csv") %>% 
    calc_probs("Attendances") %>%
    create_df("A + E", "National.Average.Unit.Cost")

read_csv_cost_collection("a_e_costs.csv")$Attendances %>% sum
read_csv_cost_collection("a_e_costs.csv")$Total.Costs  %>% sum

emprical_sample <- function(probs, values, n) {
    sample(values, size = n, replace = TRUE, prob = probs)
}


# Plot and save outcomes
plot_and_save_outcome(hosp_pead, "hosp_pead_cost")
plot_and_save_outcome(hosp_adults, "hosp_adult_cost")
plot_and_save_outcome(cc_peadiatric, "icu_pead_cost")
plot_and_save_outcome(cc_adult, "icu_adult_cost")


# Boost sample size to 1000
boostrap_cost <- function(cost_table) {
    1:1000 %>% map(~(emprical_sample(cost_table$probs, cost_table$ppe, n = 5) %>% mean)) %>% unlist %>%
        quantile(c(0.025, 0.5, 0.975)) %>% as.numeric
}


boostrap_cost(hosp_pead_ls)
boostrap_cost(hosp_adults_ls)

boostrap_cost(hosp_pead_ss)
boostrap_cost(hosp_adults_ss)

boostrap_cost(cc_peadiatric)
boostrap_cost(cc_adult)

boostrap_cost(a_e_all)