

source("hrs/mhhhrs_heterosexual couples.R")
source("C:/code/external/functions/survey/svysummary.R")


heterosexual_couples %>% 
  summarize(across(one_of(c("a_htn","c_htn","c_htn_diagnosed","a_htn_diagnosed",
                            "c_diag_undiag",
                            "a_owob","c_owob",
                            "c_age_ge65","a_age_ge65",
                            "c_raceeth_nhwhite","a_raceeth_nhwhite")),~mean(.,na.rm=TRUE)))


heterosexual_couples_svy <- heterosexual_couples %>% 
  as_survey_design(.,
                   strata = strata_weight,
                   weight = r_indweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")  

svysummary(heterosexual_couples_svy,
           p_vars = c("a_htn","c_htn","c_htn_diagnosed","a_htn_diagnosed",
                      "c_diag_undiag",
                      "a_owob","c_owob",
                      "c_age_ge65","a_age_ge65",
                      "c_raceeth_nhwhite","a_raceeth_nhwhite"),
           c_vars = c(
             "c_mean_age","c_younger_age","c_older_age" #,"c_mean_bmi"
             # "c_mean_sbp","c_mean_dbp"
           ))

heterosexual_couples_svy %>%
  group_by(hh_lengthmar>=0) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))


heterosexual_couples_svy %>%
  group_by(a_age_ge65) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

heterosexual_couples_svy %>%
  group_by(c_age_ge65) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))


heterosexual_couples_svy %>%
  group_by(a_raceeth_nhwhite) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

heterosexual_couples_svy %>%
  group_by(c_raceeth_nhwhite) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

heterosexual_couples_svy %>%
  group_by(a_owob) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

heterosexual_couples_svy %>%
  group_by(c_owob) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

heterosexual_couples_svy %>%
  group_by(a_htn) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

heterosexual_couples_svy %>%
  group_by(c_htn) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))

heterosexual_couples_svy %>%
  group_by(d_htn) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))


heterosexual_couples_svy %>%
  group_by(c_diag_undiag) %>%
  summarize(pct = survey_prop(proportion = TRUE,prop_method = "logit",vartype = "ci"))
