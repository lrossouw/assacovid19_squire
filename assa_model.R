# libraries
require("squire")
require("lubridate")
require("dplyr")

# clear env

rm(list = ls())

assa_model <- function(start_date = as.Date("2020-03-01"),
                       R0 = 3,
                       lockdown_date = as.Date("2020-03-27"),
                       lockdown_days = 35,
                       R0_lockdown = 0.6 * R0,
                       R0_post_lockdown = 0.75 * R0,
                       assymptomatic = 1 / 3,
                       assymptomatic_already_incl = 1 / 3,
                       projection_end_date = as.Date("2020-12-31"),
                       hosp_bed_capacity = NULL,
                       ICU_bed_capacity = NULL,
                       replicates = 100) {
  parameters_default <-
    parameters_explicit_SEEIR(country = "South Africa")

  # remove the existing assymptomatic assumption and apply a new one on death assumptions
  # prob_non_severe_death_treatment <-
  #   parameters_default$prob_non_severe_death_treatment / (1 - assymptomatic_already_incl) * (1 - assymptomatic)
  # prob_non_severe_death_no_treatment  <-
  #   parameters_default$prob_non_severe_death_no_treatment  / (1 - assymptomatic_already_incl) * (1 - assymptomatic)
  # prob_severe_death_treatment  <-
  #   parameters_default$prob_severe_death_treatment  / (1 - assymptomatic_already_incl) * (1 - assymptomatic)
  # prob_severe_death_no_treatment  <-
  #   parameters_default$prob_severe_death_no_treatment  / (1 - assymptomatic_already_incl) * (1 - assymptomatic)
  prob_hosp   <-
    parameters_default$prob_hosp   / (1 - assymptomatic_already_incl) * (1 - assymptomatic)


  time_period <- (projection_end_date - start_date) / ddays(1)
  R0_vector <- c(R0, R0_lockdown, R0_post_lockdown)
  t_lockdown <- (lockdown_date - start_date) / ddays(1)
  t_lockdown_end <- t_lockdown + lockdown_days
  tt_R0 <- c(0, t_lockdown, t_lockdown_end)


  r <- run_explicit_SEEIR_model(
    country = "South Africa",
    R0 = R0_vector,
    tt_R0 = tt_R0,
    time_period = time_period,
    # prob_non_severe_death_treatment = prob_non_severe_death_treatment,
    # prob_non_severe_death_no_treatment = prob_non_severe_death_no_treatment,
    # prob_severe_death_treatment = prob_severe_death_treatment,
    # prob_severe_death_no_treatment = prob_severe_death_no_treatment,
    prob_hosp = prob_hosp,
    hosp_bed_capacity = hosp_bed_capacity,
    ICU_bed_capacity = ICU_bed_capacity,
    replicates = replicates
  )

  # o_states <- format_output(
  #   r,
  #   var_select=c("S","E","R","D"),
  #   reduce_age = FALSE,
  #   combine_compartments = TRUE,
  #   date_0 = start_date
  # )

  o <- format_output(
    r,
    var_select = c("infections", "deaths"),
    reduce_age = FALSE,
    combine_compartments = TRUE,
    date_0 = start_date
  )

  o_demand <- format_output(
    r,
    var_select = c(
      "hospital_occupancy",
      "ICU_occupancy",
      "hospital_demand",
      "ICU_demand"
    ),
    reduce_age = TRUE,
    date_0 = start_date
  )

  # end_state_age<- o_states %>%
  #   filter(t==time_period) %>%
  #   group_by(compartment, age_group, replicate) %>%
  #   summarise(y = sum(y))  %>%
  #   ungroup() %>%
  #   group_by(compartment, age_group) %>%
  #   summarise(y = mean(y))  %>%
  #   ungroup()
  #
  # end_state <- end_state_age %>%
  #   group_by(compartment) %>%
  #   summarise(y = sum(y))  %>%
  #   ungroup()


  infections_age <- o %>%
    filter(compartment == "infections") %>%
    group_by(age_group, replicate) %>%
    summarise(infections = sum(y)) %>%
    ungroup() %>%
    group_by(age_group) %>%
    summarise(infections = mean(infections)) %>%
    ungroup()

  deaths_age <- o %>%
    filter(compartment == "deaths") %>%
    group_by(age_group, replicate) %>%
    summarise(deaths = sum(y)) %>%
    ungroup() %>%
    group_by(age_group) %>%
    summarise(deaths = mean(deaths)) %>%
    ungroup()

  hospital_demand <- o_demand %>%
    filter(compartment == "hospital_demand") %>%
    group_by(date, t, replicate) %>%
    summarise(hospital_demand = sum(y)) %>%
    ungroup() %>%
    group_by(date, t) %>%
    summarise(hospital_demand = mean(hospital_demand)) %>%
    ungroup()
  peak_hospital_demand <- max(hospital_demand$hospital_demand)

  ICU_demand <- o_demand %>%
    filter(compartment == "ICU_demand") %>%
    group_by(date, t, replicate) %>%
    summarise(ICU_demand = sum(y)) %>%
    ungroup() %>%
    group_by(date, t) %>%
    summarise(ICU_demand = mean(ICU_demand)) %>%
    ungroup()
  peak_ICU_demand <- max(ICU_demand$ICU_demand)

  results_by_age <-
    data.frame(
      age_group = infections_age$age_group,
      population = r$parameters$population,
      infections = infections_age$infections,
      deaths = deaths_age$deaths
    )

  results_by_age$age_group_label = as.character(paste0(
    format((results_by_age$age_group - 1) * 5,
           width = 2,
           justify = "right"
    ),
    ifelse(results_by_age$age_group == 17, "+", paste0(
      "-", format((results_by_age$age_group) * 5,
                  width = 2,
                  justify = "right"
      )
    ))
  ))

  results_by_age <-  results_by_age %>%
    select(age_group, age_group_label, population, infections, deaths)
  results_by_age$infection_attack_ratio <-
    results_by_age$infections / results_by_age$population
  results_by_age$ifr <-
    results_by_age$deaths / results_by_age$infections
  results <-
    list(
      results_by_age = results_by_age,
      population = sum(results_by_age$population),
      infections = sum(results_by_age$infections),
      deaths = sum(results_by_age$deaths),
      infection_attack_ratio = sum(results_by_age$infections) / sum(results_by_age$population),
      infection_fatality_ratio = sum(results_by_age$deaths) / sum(results_by_age$infections),
      peak_hospital_demand = peak_hospital_demand,
      peak_ICU_demand = peak_ICU_demand
    )

  return(results)
}


replicates = 10

results_s1_incorrect <-
  assa_model(
    assymptomatic = 0.75,
    assymptomatic_already_incl = 0,
    hosp_bed_capacity = 1e6,
    ICU_bed_capacity = 1e6,
    replicates = replicates
  )

results_s2_incorrect <-
  assa_model(
    assymptomatic = 0.5,
    assymptomatic_already_incl = 0,
    hosp_bed_capacity = 1e6,
    ICU_bed_capacity = 1e6,
    replicates = replicates
  )

results_s3_incorrect <-
  assa_model(
    R0 = 3,
    R0_lockdown = 3 * 0.5,
    R0_post_lockdown = 3 * 0.7,
    assymptomatic = 0.75,
    assymptomatic_already_incl = 0,
    hosp_bed_capacity = 1e6,
    ICU_bed_capacity = 1e6,
    replicates = replicates
  )

results_s4_incorrect <-
  assa_model(
    R0 = 3,
    R0_lockdown = 3 * 0.7,
    R0_post_lockdown = 3 * 0.8,
    assymptomatic = 0.75,
    assymptomatic_already_incl = 0,
    hosp_bed_capacity = 1e6,
    ICU_bed_capacity = 1e6,
    replicates = replicates
  )

results_s1_fix1 <-
  assa_model(
    assymptomatic = 0.75,
    assymptomatic_already_incl = 1 / 3,
    hosp_bed_capacity = 1e6,
    ICU_bed_capacity = 1e6,
    replicates = replicates
  )

results_s2_fix1 <-
  assa_model(
    assymptomatic = 0.5,
    assymptomatic_already_incl = 1 / 3,
    hosp_bed_capacity = 1e6,
    ICU_bed_capacity = 1e6,
    replicates = replicates
  )

results_s3_fix1 <-
  assa_model(
    R0 = 3,
    R0_lockdown = 3 * 0.5,
    R0_post_lockdown = 3 * 0.7,
    assymptomatic = 0.75,
    assymptomatic_already_incl = 1 / 3,
    hosp_bed_capacity = 1e6,
    ICU_bed_capacity = 1e6,
    replicates = replicates
  )

results_s4_fix1 <-
  assa_model(
    R0 = 3,
    R0_lockdown = 3 * 0.7,
    R0_post_lockdown = 3 * 0.8,
    assymptomatic = 0.75,
    assymptomatic_already_incl = 1 / 3,
    hosp_bed_capacity = 1e6,
    ICU_bed_capacity = 1e6,
    replicates = replicates
  )

results_s1_fix2 <-
  assa_model(
    assymptomatic = 1/3,
    assymptomatic_already_incl = 1 / 3,
    hosp_bed_capacity = 1e6,
    ICU_bed_capacity = 1e6,
    replicates = replicates
  )

results_s2_fix2 <-
  assa_model(
    assymptomatic = 1/3,
    assymptomatic_already_incl = 1 / 3,
    hosp_bed_capacity = 1e6,
    ICU_bed_capacity = 1e6,
    replicates = replicates
  )

results_s3_fix2 <-
  assa_model(
    R0 = 3,
    R0_lockdown = 3 * 0.5,
    R0_post_lockdown = 3 * 0.7,
    assymptomatic = 1/3,
    assymptomatic_already_incl = 1 / 3,
    hosp_bed_capacity = 1e6,
    ICU_bed_capacity = 1e6,
    replicates = replicates
  )

results_s4_fix2 <-
  assa_model(
    R0 = 3,
    R0_lockdown = 3 * 0.7,
    R0_post_lockdown = 3 * 0.8,
    assymptomatic = 1/3,
    assymptomatic_already_incl = 1 / 3,
    hosp_bed_capacity = 1e6,
    ICU_bed_capacity = 1e6,
    replicates = replicates
  )


results_s1 <-
  assa_model(
    assymptomatic = 1/3,
    assymptomatic_already_incl = 1 / 3,
    replicates = replicates
  )

results_s2 <-
  assa_model(
    assymptomatic = 1/3,
    assymptomatic_already_incl = 1 / 3,
    replicates = replicates
  )

results_s3 <-
  assa_model(
    R0 = 3,
    R0_lockdown = 3 * 0.5,
    R0_post_lockdown = 3 * 0.7,
    assymptomatic = 1/3,
    assymptomatic_already_incl = 1 / 3,
    replicates = replicates
  )

results_s4 <-
  assa_model(
    R0 = 3,
    R0_lockdown = 3 * 0.7,
    R0_post_lockdown = 3 * 0.8,
    assymptomatic = 1/3,
    assymptomatic_already_incl = 1 / 3,
    replicates = replicates
  )


results <-
  data.frame(
    s = c(
      "s1_incorrect",
      "s2_incorrect",
      "s3_inccored",
      "s4_incorrect",
      "s1_fix1",
      "s2_fix1",
      "s3_fix1",
      "s4_fix1",
      "s1_fix2",
      "s2_fix2",
      "s3_fix2",
      "s4_fix2",
      "s1",
      "s2",
      "s3",
      "s4"
    ),
    population = c(
      results_s1_incorrect$population,
      results_s2_incorrect$population,
      results_s3_incorrect$population,
      results_s4_incorrect$population,
      results_s1_fix1$population,
      results_s2_fix1$population,
      results_s3_fix1$population,
      results_s4_fix1$population,
      results_s1_fix2$population,
      results_s2_fix2$population,
      results_s3_fix2$population,
      results_s4_fix2$population,
      results_s1$population,
      results_s2$population,
      results_s3$population,
      results_s4$population
    ),
    infections = c(
      results_s1_incorrect$infections,
      results_s2_incorrect$infections,
      results_s3_incorrect$infections,
      results_s4_incorrect$infections,
      results_s1_fix1$infections,
      results_s2_fix1$infections,
      results_s3_fix1$infections,
      results_s4_fix1$infections,
      results_s1_fix2$infections,
      results_s2_fix2$infections,
      results_s3_fix2$infections,
      results_s4_fix2$infections,
      results_s1$infections,
      results_s2$infections,
      results_s3$infections,
      results_s4$infections
    ),
    deaths = c(
      results_s1_incorrect$deaths,
      results_s2_incorrect$deaths,
      results_s3_incorrect$deaths,
      results_s4_incorrect$deaths,
      results_s1_fix1$deaths,
      results_s2_fix1$deaths,
      results_s3_fix1$deaths,
      results_s4_fix1$deaths,
      results_s1_fix2$deaths,
      results_s2_fix2$deaths,
      results_s3_fix2$deaths,
      results_s4_fix2$deaths,
      results_s1$deaths,
      results_s2$deaths,
      results_s3$deaths,
      results_s4$deaths
    ),
    peak_hospital_demand = c(
      results_s1_incorrect$peak_hospital_demand,
      results_s2_incorrect$peak_hospital_demand,
      results_s3_incorrect$peak_hospital_demand,
      results_s4_incorrect$peak_hospital_demand,
      results_s1_fix1$peak_hospital_demand,
      results_s2_fix1$peak_hospital_demand,
      results_s3_fix1$peak_hospital_demand,
      results_s4_fix1$peak_hospital_demand,
      results_s1_fix2$peak_hospital_demand,
      results_s2_fix2$peak_hospital_demand,
      results_s3_fix2$peak_hospital_demand,
      results_s4_fix2$peak_hospital_demand,
      results_s1$peak_hospital_demand,
      results_s2$peak_hospital_demand,
      results_s3$peak_hospital_demand,
      results_s4$peak_hospital_demand
    ),
    peak_ICU_demand = c(
      results_s1_incorrect$peak_ICU_demand,
      results_s2_incorrect$peak_ICU_demand,
      results_s3_incorrect$peak_ICU_demand,
      results_s4_incorrect$peak_ICU_demand,
      results_s1_fix1$peak_ICU_demand,
      results_s2_fix1$peak_ICU_demand,
      results_s3_fix1$peak_ICU_demand,
      results_s4_fix1$peak_ICU_demand,
      results_s1_fix2$peak_ICU_demand,
      results_s2_fix2$peak_ICU_demand,
      results_s3_fix2$peak_ICU_demand,
      results_s4_fix2$peak_ICU_demand,
      results_s1$peak_ICU_demand,
      results_s2$peak_ICU_demand,
      results_s3$peak_ICU_demand,
      results_s4$peak_ICU_demand
      )
  )

results$infection_attack_ratio <-
  results$infections / results$population
results$infection_falality_ratio <-
  results$deaths / results$infections

write.csv(results, "results.csv", row.names = FALSE)
