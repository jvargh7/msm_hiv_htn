

source("hrs/mhhhrs01_creating analytic datasets.R")
rm(msm_analytic_dataset_unique_svy,wsw_analytic_dataset_unique_svy)

msm_df = bind_rows(
  msm_analytic_dataset_unique %>% 
    rename(h_idpn = h1_hhidpn,
           h_spouseidpn = h1_spouseidpn,
           h_coupleid = coupleid,
           h_htn = h1_htn,
           spouse_htn = h2_htn) %>% 
    mutate(h_partnerid = 1) %>% 
    dplyr::select(starts_with("h_"),spouse_htn,strata,indsampleweight),
  
  msm_analytic_dataset_unique %>% 
    rename(h_idpn = h1_spouseidpn,
           h_spouseidpn = h1_hhidpn,
           h_coupleid = coupleid,
           h_htn = h2_htn,
           spouse_htn = h1_htn) %>% 
    mutate(h_partnerid = 2) %>% 
    dplyr::select(starts_with("h_"),spouse_htn,strata,indsampleweight)
  
  
)



glmer(spouse_htn ~ (1|h_coupleid),data=msm_df,family=poisson())

msm_svy <- msm_df %>% 
  
  # Need to impute and correct
  as_survey_design(.data = .,
                   ids = h_coupleid,
                   # strata = strata,
                   weight = indsampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

svyglm(spouse_htn ~ h_htn,design = msm_svy,family="poisson") %>% 
  broom::tidy(exponentiate = TRUE)
