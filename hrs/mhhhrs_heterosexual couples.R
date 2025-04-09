# gc();rm(list=ls());source(".Rprofile")

heterosexual_couples <- readRDS(paste0(path_g2a_family_folder,"/working/hrs/G2A HRS Couples mi_dfs.RDS")) %>% 
  .$data %>% 
  mutate(h_htn = case_when(h_sbp >= 130 | h_dbp >= 80 ~ 1,
                           h_sbp < 130 & h_dbp < 80 ~ 0,
                           TRUE ~ NA_real_),
         w_htn = case_when(w_sbp >= 130 | w_dbp >= 80 ~ 1,
                           w_sbp < 130 & w_dbp < 80 ~ 0,
                           TRUE ~ NA_real_)) %>% 
  mutate(h_htn = case_when(is.na(h_diagnosed_bp) | h_diagnosed_bp == 0 ~ h_htn,
                           TRUE ~ h_diagnosed_bp),
         w_htn = case_when(is.na(w_diagnosed_bp) | w_diagnosed_bp == 0 ~ w_htn,
                           TRUE ~ w_diagnosed_bp)) %>% 
  
  
  mutate( c_htn = case_when(h_htn == 1 & w_htn == 1 ~ 1,
                            h_htn == 1 & w_htn == 0 ~ 0,
                            h_htn == 0 & w_htn == 1 ~ 0,
                            h_htn == 0 & w_htn == 0 ~ 0,
                            TRUE ~ NA_real_),
          d1_htn = case_when(h_htn == 1 & w_htn == 1 ~ 0,
                             h_htn == 1 & w_htn == 0 ~ 1,
                             h_htn == 0 & w_htn == 1 ~ 0,
                             h_htn == 0 & w_htn == 0 ~ 0,
                             TRUE ~ NA_real_),
          
          d_htn = case_when(h_htn == 1 & w_htn == 1 ~ 0,
                             h_htn == 1 & w_htn == 0 ~ 1,
                             h_htn == 0 & w_htn == 1 ~ 1,
                             h_htn == 0 & w_htn == 0 ~ 0,
                             TRUE ~ NA_real_),
          
          d2_htn = case_when(h_htn == 1 & w_htn == 1 ~ 0,
                             h_htn == 1 & w_htn == 0 ~ 0,
                             h_htn == 0 & w_htn == 1 ~ 1,
                             h_htn == 0 & w_htn == 0 ~ 0,
                             TRUE ~ NA_real_),
          c_htn_free = 1 - c_htn - d1_htn - d2_htn,
          c_htn_free2 = 1 - c_htn - d_htn,
          
          c_raceethnicity = case_when(h_raceethnic == w_raceethnic ~ h_raceethnic,
                                      TRUE ~ "Mixed")
          
  ) %>% 
  # Based on Revision comments
  mutate(
    
    c_htn_diagnosed = case_when(h_diagnosed_bp == 1 & w_diagnosed_bp == 1 ~ 1,
                                h_diagnosed_bp == 1 & w_diagnosed_bp == 0 ~ 0,
                                h_diagnosed_bp == 0 & w_diagnosed_bp == 1 ~ 0,
                                h_diagnosed_bp == 0 & w_diagnosed_bp == 0 ~ 0,
                                TRUE ~ NA_real_),
    
    c_diag_undiag = case_when(c_htn == 1 & h_diagnosed_bp == 1 & w_diagnosed_bp == 0 ~ 1,
                              c_htn == 1 & h_diagnosed_bp == 0 & w_diagnosed_bp == 1  ~ 1,
                              c_htn == 1 & h_diagnosed_bp == 0 & w_diagnosed_bp == 0 ~ 1,
                              c_htn == 1 & h_diagnosed_bp == 1 & w_diagnosed_bp == 1 ~ 0,
                              TRUE ~ NA_real_),
    
    # c_diag_undiag = case_when(h_diagnosed_bp == 1 & w_diagnosed_bp == 0 & w_htn == 1 ~ 1,
    #                           h_diagnosed_bp == 0 & w_htn == 1 & w_diagnosed_bp == 1  ~ 1,
    #                           h_diagnosed_bp == 0 & w_htn == 1 & w_diagnosed_bp == 0 & w_htn == 1 ~ 1,
    #                           h_diagnosed_bp == 1 & w_diagnosed_bp == 1 ~ 0,
    #                           TRUE ~ NA_real_),
    # 
    c_owob = case_when(h_bmi >= 25 & w_bmi >= 25 ~ 1,
                       h_bmi >= 25 & w_bmi < 25 ~ 0,
                       h_bmi < 25 & w_bmi >= 25 ~ 0,
                       h_bmi < 25 & w_bmi < 25 ~ 0,
                       TRUE ~ NA_real_),
    
    c_age_ge65 = case_when(h_age >= 65 & w_age >= 65 ~ 1,
                           h_age >= 65 & w_age < 65 ~ 0,
                           h_age < 65 & w_age >= 65 ~ 0,
                           h_age < 65 & w_age < 65 ~ 0,
                           TRUE ~ NA_real_),
    
    c_raceeth_nhwhite = case_when(h_raceethnic == "NHWhite" & w_raceethnic == "NHWhite" ~ 1,
                                  h_raceethnic == "NHWhite" & w_raceethnic != "NHWhite" ~ 0,
                                  h_raceethnic != "NHWhite" & w_raceethnic == "NHWhite" ~ 0,
                                  h_raceethnic != "NHWhite" & w_raceethnic != "NHWhite" ~ 0,
                                  TRUE ~ NA_real_),
    
    a_htn = case_when(h_htn == 1 & w_htn == 1 ~ 1,
                      h_htn == 1 & w_htn == 0 ~ 1,
                      h_htn == 0 & w_htn == 1 ~ 1,
                      h_htn == 0 & w_htn == 0 ~ 0,
                      TRUE ~ NA_real_),
    
    a_htn_diagnosed = case_when(h_diagnosed_bp == 1 & w_diagnosed_bp == 1 ~ 1,
                                h_diagnosed_bp == 1 & w_diagnosed_bp == 0 ~ 1,
                                h_diagnosed_bp == 0 & w_diagnosed_bp == 1 ~ 1,
                                h_diagnosed_bp == 0 & w_diagnosed_bp == 0 ~ 0,
                                TRUE ~ NA_real_),
    
    # a_diagnosed_bp = case_when(h_diagnosed_bp == 1 & w_diagnosed_bp == 1 ~ 1,
    #                            h_diagnosed_bp == 1 & w_diagnosed_bp == 0 ~ 1,
    #                            h_diagnosed_bp == 0 & w_diagnosed_bp == 1 ~ 1,
    #                            h_diagnosed_bp == 0 & w_diagnosed_bp == 0 ~ 0,
    #                            TRUE ~ NA_real_),
    a_owob = case_when(h_bmi >= 25 & w_bmi >= 25 ~ 1,
                       h_bmi >= 25 & w_bmi < 25 ~ 1,
                       h_bmi < 25 & w_bmi >= 25 ~ 1,
                       h_bmi < 25 & w_bmi < 25 ~ 0,
                       TRUE ~ NA_real_),
    
    a_age_ge65 = case_when(h_age >= 65 & w_age >= 65 ~ 1,
                           h_age >= 65 & w_age < 65 ~ 1,
                           h_age < 65 & w_age >= 65 ~ 1,
                           h_age < 65 & w_age < 65 ~ 0,
                           TRUE ~ NA_real_),
    
    a_raceeth_nhwhite = case_when(h_raceethnic == "NHWhite" & w_raceethnic == "NHWhite" ~ 1,
                                  h_raceethnic == "NHWhite" & w_raceethnic != "NHWhite" ~ 1,
                                  h_raceethnic != "NHWhite" & w_raceethnic == "NHWhite" ~ 1,
                                  h_raceethnic != "NHWhite" & w_raceethnic != "NHWhite" ~ 0,
                                  TRUE ~ NA_real_),
    
    c_mean_age = (h_age + w_age)/2,
    c_mean_bmi = (h_bmi + w_bmi)/2,
    c_mean_sbp = (h_sbp + w_sbp)/2,
    c_mean_dbp = (h_dbp + w_dbp)/2,
    
    c_younger_age = pmin(h_age,w_age),
    c_older_age = pmax(h_age,w_age))




