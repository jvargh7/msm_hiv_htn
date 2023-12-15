gc();rm(list=ls());source(".Rprofile")

selected_vars_msm = c("h1_hhidpn","h1_spouseidpn","h2_spouseidpn","coupleid",
                   "hh_children","hh_size",
                   "strata","psu", "hhsampleweight",
                   "indsampleweight", 
                   "h1_lengthmar",
                   "h1_age", "h2_age",
                   "h1_gender", "h2_gender",
                   "h1_raceeth","h2_raceeth",
                   "h1_height","h2_height",
                   "h1_bmi", "h2_bmi",
                   "h1_bmi_category", "h2_bmi_category",
                   "h1_sbp", "h2_sbp",
                   "h1_dbp", "h2_dbp",
                   "h1_htn", "h2_htn",
                   "h1_education_h", "h2_education_h",
                   "h1_married", "h2_married",
                   
                   "h1_moderate_pa", "h2_moderate_pa",
                   "h1_vigorous_pa", "h2_vigorous_pa",
                   "h1_alcohol", "h2_alcohol",
                   "h1_smoke","h2_smoke"
)

selected_vars_wsw = c("w1_hhidpn","w1_spouseidpn","w2_spouseidpn","coupleid",
                      "hh_children","hh_size",
                      "strata","psu","hhsampleweight",
                      "indsampleweight", 
                      "w1_lengthmar",
                      "w1_age", "w2_age",
                      "w1_gender", "w2_gender",
                      "w1_raceeth","w2_raceeth",
                      "w1_height","w2_height",
                      "w1_bmi", "w2_bmi",
                      "w1_bmi_category", "w2_bmi_category",
                      "w1_sbp", "w2_sbp",
                      "w1_dbp", "w2_dbp",
                      "w1_htn", "w2_htn",
                      "w1_education_h", "w2_education_h",
                      "w1_married", "w2_married",
                      
                      "w1_moderate_pa", "w2_moderate_pa",
                      "w1_vigorous_pa", "w2_vigorous_pa",
                      "w1_alcohol", "w2_alcohol",
                      "w1_smoke","w2_smoke"
)

msm_couples_wave14 <- readRDS(paste0(path_hrs_elsa_concordance_folder,"/working/hrs/G2A HRS Wave 14 msm couples.RDS")) %>% 
  dplyr::select(one_of(selected_vars_msm))
msm_couples_wave13 <- readRDS(paste0(path_hrs_elsa_concordance_folder,"/working/hrs/G2A HRS Wave 13 msm couples.RDS")) %>% 
  dplyr::select(one_of(selected_vars_msm))
msm_couples_wave12 <- readRDS(paste0(path_hrs_elsa_concordance_folder,"/working/hrs/G2A HRS Wave 12 msm couples.RDS")) %>% 
  dplyr::select(one_of(selected_vars_msm))
msm_couples_wave11 <- readRDS(paste0(path_hrs_elsa_concordance_folder,"/working/hrs/G2A HRS Wave 11 msm couples.RDS")) %>% 
  dplyr::select(one_of(selected_vars_msm))
msm_couples_wave10 <- readRDS(paste0(path_hrs_elsa_concordance_folder,"/working/hrs/G2A HRS Wave 10 msm couples.RDS")) %>% 
  dplyr::select(one_of(selected_vars_msm))
msm_couples_wave9 <- readRDS(paste0(path_hrs_elsa_concordance_folder,"/working/hrs/G2A HRS Wave 9 msm couples.RDS")) %>% 
  dplyr::select(one_of(selected_vars_msm))
msm_couples_wave8 <- readRDS(paste0(path_hrs_elsa_concordance_folder,"/working/hrs/G2A HRS Wave 8 msm couples.RDS")) %>% 
  dplyr::select(one_of(selected_vars_msm))


wsw_couples_wave14 <- readRDS(paste0(path_hrs_elsa_concordance_folder,"/working/hrs/G2A HRS Wave 14 wsw couples.RDS")) %>% 
  dplyr::select(one_of(selected_vars_wsw))
wsw_couples_wave13 <- readRDS(paste0(path_hrs_elsa_concordance_folder,"/working/hrs/G2A HRS Wave 13 wsw couples.RDS")) %>% 
  dplyr::select(one_of(selected_vars_wsw))
wsw_couples_wave12 <- readRDS(paste0(path_hrs_elsa_concordance_folder,"/working/hrs/G2A HRS Wave 12 wsw couples.RDS")) %>% 
  dplyr::select(one_of(selected_vars_wsw))
wsw_couples_wave11 <- readRDS(paste0(path_hrs_elsa_concordance_folder,"/working/hrs/G2A HRS Wave 11 wsw couples.RDS")) %>% 
  dplyr::select(one_of(selected_vars_wsw))
wsw_couples_wave10 <- readRDS(paste0(path_hrs_elsa_concordance_folder,"/working/hrs/G2A HRS Wave 10 wsw couples.RDS")) %>% 
  dplyr::select(one_of(selected_vars_wsw))
wsw_couples_wave9 <- readRDS(paste0(path_hrs_elsa_concordance_folder,"/working/hrs/G2A HRS Wave 9 wsw couples.RDS")) %>% 
  dplyr::select(one_of(selected_vars_wsw))
wsw_couples_wave8 <- readRDS(paste0(path_hrs_elsa_concordance_folder,"/working/hrs/G2A HRS Wave 8 wsw couples.RDS")) %>% 
  dplyr::select(one_of(selected_vars_wsw))

# Serial cross-sectional prevalence 
msm_analytic_dataset_scs <- bind_rows(
  msm_couples_wave8 %>% mutate(wave = 8),
  msm_couples_wave9 %>% mutate(wave = 9),
  msm_couples_wave10 %>% mutate(wave = 10),
  msm_couples_wave11 %>% mutate(wave = 11),
  msm_couples_wave12 %>% mutate(wave = 12),
  msm_couples_wave13 %>% mutate(wave = 13),
  msm_couples_wave14 %>% mutate(wave = 14)
) %>% 
  dplyr::filter((!is.na(h1_htn) & !is.na(h2_htn)) | (!is.na(h1_sbp) & !is.na(h2_sbp) & !is.na(h1_dbp) & !is.na(h2_dbp)))

wsw_analytic_dataset_scs <- bind_rows(
  wsw_couples_wave8 %>% mutate(wave = 8),
  wsw_couples_wave9 %>% mutate(wave = 9),
  wsw_couples_wave10 %>% mutate(wave = 10),
  wsw_couples_wave11 %>% mutate(wave = 11),
  wsw_couples_wave12 %>% mutate(wave = 12),
  wsw_couples_wave13 %>% mutate(wave = 13),
  wsw_couples_wave14 %>% mutate(wave = 14)
) %>% 
  dplyr::filter((!is.na(w1_htn) & !is.na(w2_htn)) | (!is.na(w1_sbp) & !is.na(w2_sbp) & !is.na(w1_dbp) & !is.na(w2_dbp)))

rm(list = ls()[regexpr("couples_wave",ls())>0])

# Restricting to unique couples
msm_analytic_dataset_unique <- msm_analytic_dataset_scs %>% 
  group_by(coupleid) %>% 
  mutate(n_rows = n(),
         bp_missing = case_when(is.na(h1_sbp) | is.na(h1_dbp) | is.na(h2_sbp) | is.na(h2_dbp) ~ 1,
                                TRUE ~ 0)) %>% 
  ungroup() %>% 
  dplyr::filter(n_rows == 1 | (n_rows > 1 & bp_missing == 0)) %>% 
  group_by(coupleid) %>% 
  dplyr::filter(wave == max(wave)) %>% 
  ungroup() %>% 
  group_by(wave) %>% 
  mutate(indsampleweight = indsampleweight/sum(indsampleweight)) %>% 
  mutate(indsampleweight = indsampleweight/n()) %>% 
  ungroup() %>% 
  mutate(indsampleweight = indsampleweight*n()) %>% 
  mutate(
         g_htn = case_when(h1_htn == 1 & h2_htn == 1 ~ "Both",
                           h1_htn == 1 & h2_htn == 0 ~ "One",
                           h1_htn == 0 & h2_htn == 1 ~ "One",
                           h1_htn == 0 & h2_htn == 0 ~ "Neither",
                           TRUE ~ "Missing"),
         g_owob = case_when(h1_bmi >= 25 & h2_bmi >= 25 ~ "Both",
                            h1_bmi >= 25 & h2_bmi < 25 ~ "One",
                            h1_bmi < 25 & h2_bmi >= 25 ~ "One",
                            h1_bmi < 25 & h2_bmi < 25 ~ "Neither",
                            TRUE ~ "Missing"),
         
         g_age_ge65 = case_when(h1_age >= 65 & h2_age >= 65 ~ "Both",
                                h1_age >= 65 & h2_age < 65 ~ "One",
                                h1_age < 65 & h2_age >= 65 ~ "One",
                                h1_age < 65 & h2_age < 65 ~ "Neither",
                                TRUE ~ "Missing"),
         
         c_htn = case_when(h1_htn == 1 & h2_htn == 1 ~ 1,
                           h1_htn == 1 & h2_htn == 0 ~ 0,
                           h1_htn == 0 & h2_htn == 1 ~ 0,
                           h1_htn == 0 & h2_htn == 0 ~ 0,
                           TRUE ~ NA_real_),
         c_owob = case_when(h1_bmi >= 25 & h2_bmi >= 25 ~ 1,
                            h1_bmi >= 25 & h2_bmi < 25 ~ 0,
                            h1_bmi < 25 & h2_bmi >= 25 ~ 0,
                            h1_bmi < 25 & h2_bmi < 25 ~ 0,
                            TRUE ~ NA_real_),
         
         c_age_ge65 = case_when(h1_age >= 65 & h2_age >= 65 ~ 1,
                                h1_age >= 65 & h2_age < 65 ~ 0,
                                h1_age < 65 & h2_age >= 65 ~ 0,
                                h1_age < 65 & h2_age < 65 ~ 0,
                                TRUE ~ NA_real_),
         
         c_raceeth_nhwhite = case_when(h1_raceeth == "NH White" & h2_raceeth == "NH White" ~ 1,
                                       h1_raceeth == "NH White" & h2_raceeth != "NH White" ~ 0,
                                       h1_raceeth != "NH White" & h2_raceeth == "NH White" ~ 0,
                                       h1_raceeth != "NH White" & h2_raceeth != "NH White" ~ 0,
                           TRUE ~ NA_real_),
         
         a_htn = case_when(h1_htn == 1 & h2_htn == 1 ~ 1,
                           h1_htn == 1 & h2_htn == 0 ~ 1,
                           h1_htn == 0 & h2_htn == 1 ~ 1,
                           h1_htn == 0 & h2_htn == 0 ~ 0,
                           TRUE ~ NA_real_),
         a_owob = case_when(h1_bmi >= 25 & h2_bmi >= 25 ~ 1,
                            h1_bmi >= 25 & h2_bmi < 25 ~ 1,
                            h1_bmi < 25 & h2_bmi >= 25 ~ 1,
                            h1_bmi < 25 & h2_bmi < 25 ~ 0,
                            TRUE ~ NA_real_),
         
         a_age_ge65 = case_when(h1_age >= 65 & h2_age >= 65 ~ 1,
                                h1_age >= 65 & h2_age < 65 ~ 1,
                                h1_age < 65 & h2_age >= 65 ~ 1,
                                h1_age < 65 & h2_age < 65 ~ 0,
                                TRUE ~ NA_real_),
         
         a_raceeth_nhwhite = case_when(h1_raceeth == "NH White" & h2_raceeth == "NH White" ~ 1,
                                       h1_raceeth == "NH White" & h2_raceeth != "NH White" ~ 1,
                                       h1_raceeth != "NH White" & h2_raceeth == "NH White" ~ 1,
                                       h1_raceeth != "NH White" & h2_raceeth != "NH White" ~ 0,
                                       TRUE ~ NA_real_),
         
         c_mean_age = (h1_age + h2_age)/2,
         c_mean_bmi = (h1_bmi + h2_bmi)/2,
         c_mean_sbp = (h1_sbp + h2_sbp)/2,
         c_mean_dbp = (h1_dbp + h2_dbp)/2)

msm_analytic_dataset_unique_svy = msm_analytic_dataset_unique %>% 
  
  # Need to impute and correct
  as_survey_design(.data = .,
                   # ids = psu,
                   # strata = strata,
                   weight = indsampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

table(msm_analytic_dataset_unique$h1_htn,msm_analytic_dataset_unique$h2_htn) %>%
  prop.table()

wsw_analytic_dataset_unique <- wsw_analytic_dataset_scs %>% 
  group_by(coupleid) %>% 
  mutate(n_rows = n(),
         bp_missing = case_when(is.na(w1_sbp) | is.na(w1_dbp) | is.na(w2_sbp) | is.na(w2_dbp) ~ 1,
                                TRUE ~ 0)) %>% 
  ungroup() %>% 
  dplyr::filter(n_rows == 1 | (n_rows > 1 & bp_missing == 0)) %>% 
  group_by(coupleid) %>% 
  dplyr::filter(wave == max(wave)) %>% 
  ungroup() %>% 
  group_by(wave) %>% 
  mutate(indsampleweight = indsampleweight/sum(indsampleweight)) %>% 
  mutate(indsampleweight = indsampleweight/n()) %>% 
  ungroup() %>% 
  mutate(indsampleweight = indsampleweight*n())  %>% 
  mutate(g_htn = case_when(w1_htn == 1 & w2_htn == 1 ~ "Both",
                            w1_htn == 1 & w2_htn == 0 ~ "One",
                            w1_htn == 0 & w2_htn == 1 ~ "One",
                            w1_htn == 0 & w2_htn == 0 ~ "Neither",
                            TRUE ~ NA_character_),
         g_owob = case_when(w1_bmi >= 25 & w2_bmi >= 25 ~ "Both",
                            w1_bmi >= 25 & w2_bmi < 25 ~ "One",
                            w1_bmi < 25 & w2_bmi >= 25 ~ "One",
                            w1_bmi < 25 & w2_bmi < 25 ~ "Neither",
                            TRUE ~ NA_character_),
         
         g_age_ge65 = case_when(w1_age >= 65 & w2_age >= 65 ~ "Both",
                                w1_age >= 65 & w2_age < 65 ~ "One",
                                w1_age < 65 & w2_age >= 65 ~ "One",
                                w1_age < 65 & w2_age < 65 ~ "Neither",
                                TRUE ~ NA_character_),
         
         
         c_htn = case_when(w1_htn == 1 & w2_htn == 1 ~ 1,
                           w1_htn == 1 & w2_htn == 0 ~ 0,
                           w1_htn == 0 & w2_htn == 1 ~ 0,
                           w1_htn == 0 & w2_htn == 0 ~ 0,
                           TRUE ~ NA_real_),
         c_owob = case_when(w1_bmi >= 25 & w2_bmi >= 25 ~ 1,
                            w1_bmi >= 25 & w2_bmi < 25 ~ 0,
                            w1_bmi < 25 & w2_bmi >= 25 ~ 0,
                            w1_bmi < 25 & w2_bmi < 25 ~ 0,
                            TRUE ~ NA_real_),
         
         c_age_ge65 = case_when(w1_age >= 65 & w2_age >= 65 ~ 1,
                                w1_age >= 65 & w2_age < 65 ~ 0,
                                w1_age < 65 & w2_age >= 65 ~ 0,
                                w1_age < 65 & w2_age < 65 ~ 0,
                                TRUE ~ NA_real_),
         
         c_raceeth_nhwhite = case_when(w1_raceeth == "NH White" & w2_raceeth == "NH White" ~ 1,
                                       w1_raceeth == "NH White" & w2_raceeth != "NH White" ~ 0,
                                       w1_raceeth != "NH White" & w2_raceeth == "NH White" ~ 0,
                                       w1_raceeth != "NH White" & w2_raceeth != "NH White" ~ 0,
                                       TRUE ~ NA_real_),
         
         a_htn = case_when(w1_htn == 1 & w2_htn == 1 ~ 1,
                           w1_htn == 1 & w2_htn == 0 ~ 1,
                           w1_htn == 0 & w2_htn == 1 ~ 1,
                           w1_htn == 0 & w2_htn == 0 ~ 0,
                           TRUE ~ NA_real_),
         a_owob = case_when(w1_bmi >= 25 & w2_bmi >= 25 ~ 1,
                            w1_bmi >= 25 & w2_bmi < 25 ~ 1,
                            w1_bmi < 25 & w2_bmi >= 25 ~ 1,
                            w1_bmi < 25 & w2_bmi < 25 ~ 0,
                            TRUE ~ NA_real_),
         
         a_age_ge65 = case_when(w1_age >= 65 & w2_age >= 65 ~ 1,
                                w1_age >= 65 & w2_age < 65 ~ 1,
                                w1_age < 65 & w2_age >= 65 ~ 1,
                                w1_age < 65 & w2_age < 65 ~ 0,
                                TRUE ~ NA_real_),
         
         a_raceeth_nhwhite = case_when(w1_raceeth == "NH White" & w2_raceeth == "NH White" ~ 1,
                                       w1_raceeth == "NH White" & w2_raceeth != "NH White" ~ 1,
                                       w1_raceeth != "NH White" & w2_raceeth == "NH White" ~ 1,
                                       w1_raceeth != "NH White" & w2_raceeth != "NH White" ~ 0,
                                       TRUE ~ NA_real_),
         
         
         c_mean_age = (w1_age + w2_age)/2,
         c_mean_bmi = (w1_bmi + w2_bmi)/2,
         c_mean_sbp = (w1_sbp + w2_sbp)/2,
         c_mean_dbp = (w1_dbp + w2_dbp)/2)

wsw_analytic_dataset_unique_svy = wsw_analytic_dataset_unique %>% 
  
  # Need to impute and correct
  as_survey_design(.data = .,
                   # ids = psu,
                   # strata = strata,
                   weight = indsampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

table(wsw_analytic_dataset_unique$w1_htn,wsw_analytic_dataset_unique$w2_htn) %>% 
  prop.table()


