

source("hrs/mhhhrs01_creating analytic datasets.R")

source("C:/code/external/functions/survey/svysummary.R")

svysummary(msm_analytic_dataset_unique_svy,
           p_vars = c("a_htn","c_htn","c_htn_diagnosed","a_htn_diagnosed",
                      "c_diag_undiag",
                      "a_owob","c_owob",
                      "c_age_ge65","a_age_ge65",
                      "c_raceeth_nhwhite","a_raceeth_nhwhite"),
           c_vars = c(
                      "c_mean_age","c_younger_age","c_older_age" #,"c_mean_bmi"
                      # "c_mean_sbp","c_mean_dbp"
                      ))

svysummary(wsw_analytic_dataset_unique_svy,
           p_vars = c("a_htn","c_htn","c_htn_diagnosed","a_htn_diagnosed",
                      "c_diag_undiag",
                      "a_owob","c_owob",
                      "c_age_ge65","a_age_ge65",
                      "c_raceeth_nhwhite","a_raceeth_nhwhite"),
           c_vars = c(
             "c_mean_age", "c_younger_age","c_older_age"#,"c_mean_bmi"
             # "c_mean_sbp","c_mean_dbp"
           ))

# msm_analytic_dataset_unique_svy %>% 
#   group_by(h1_married) %>% 
#   summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))
# 
msm_analytic_dataset_unique_svy %>%
  group_by(h1_married) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

msm_analytic_dataset_unique_svy %>%
  group_by(a_age_ge65) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

msm_analytic_dataset_unique_svy %>%
  group_by(c_age_ge65) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))


msm_analytic_dataset_unique_svy %>%
  group_by(a_raceeth_nhwhite) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

msm_analytic_dataset_unique_svy %>%
  group_by(c_raceeth_nhwhite) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

msm_analytic_dataset_unique_svy %>%
  group_by(a_owob) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

msm_analytic_dataset_unique_svy %>%
  group_by(c_owob) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

msm_analytic_dataset_unique_svy %>%
  group_by(a_htn) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

msm_analytic_dataset_unique_svy %>%
  group_by(c_htn) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

msm_analytic_dataset_unique_svy %>%
  group_by(c_diag_undiag) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))



wsw_analytic_dataset_unique_svy %>%
  group_by(w1_married) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

wsw_analytic_dataset_unique_svy %>%
  group_by(a_age_ge65) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

wsw_analytic_dataset_unique_svy %>%
  group_by(c_age_ge65) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))


wsw_analytic_dataset_unique_svy %>%
  group_by(a_raceeth_nhwhite) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

wsw_analytic_dataset_unique_svy %>%
  group_by(c_raceeth_nhwhite) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

wsw_analytic_dataset_unique_svy %>%
  group_by(a_owob) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

wsw_analytic_dataset_unique_svy %>%
  group_by(c_owob) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

wsw_analytic_dataset_unique_svy %>%
  group_by(a_htn) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

wsw_analytic_dataset_unique_svy %>%
  group_by(c_htn) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

wsw_analytic_dataset_unique_svy %>%
  group_by(c_diag_undiag) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))




# msm_analytic_dataset_unique_svy %>%
#   summarize(younger = survey_mean(c_younger_age,vartype = "ci"),
#             older = survey_mean(c_older_age,vartype = "ci"))
# 
# wsw_analytic_dataset_unique_svy %>%
#   summarize(younger = survey_mean(c_younger_age,vartype = "ci"),
#             older = survey_mean(c_older_age,vartype = "ci"))

