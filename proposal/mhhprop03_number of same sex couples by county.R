
acs5y_s1101 <- read_csv(paste0(path_msm_hiv_htn_proposal,"/working/ACSST5Y2020.S1101_2024-02-08T103312/ACSST5Y2020.S1101-Data.csv"),
                        col_names = read_csv(paste0(path_msm_hiv_htn_proposal,"/working/ACSST5Y2020.S1101_2024-02-08T103312/ACSST5Y2020.S1101-Column-Metadata.csv")) %>% 
                          dplyr::select('Column Name') %>% 
                          pull(),skip = 2
                        ) %>% 
  dplyr::select(GEO_ID,
                NAME,
                S1101_C01_009E, #Total households
                S1101_C01_003E, #Total families
                S1101_C02_001E, #Married0couple family household - Total households
                S1101_C02_003E #Married0couple family household - Total families
                )

census2020_cishetero_samesex <- read_csv(paste0(path_msm_hiv_htn_proposal,"/working/cisheterosexism-county-same-sex-households/cisheterosexism-county-same-sex-households.csv")) %>% 
  mutate(NAME = paste0(county,", ",state))

metro_atlanta <- census2020_cishetero_samesex %>% 
  left_join(acs5y_s1101,
            by = "NAME") %>% 
  dplyr::filter(state == "Georgia",
                county %in% paste0(c("Fulton","DeKalb","Clayton","Cobb","Gwinnett",
                                     
                                     "Barrow","Bartow","Butts", "Carroll", "Cherokee", "Coweta", 
                                     "Dawson", "Douglas", "Fayette", 
                                     "Forsyth", "Haralson", "Heard" , "Henry", "Jasper", "Lamar", 
                                     "Meriwether","Morgan","Newton",
                                     "Paulding", "Pickens", "Pike", "Rockdale",
                                     "Spalding","Walton"
                                     
                                     )," County")) %>% 
  mutate(count_same_sex_unions = S1101_C01_003E*prop_same_sex_unions,
         count_same_sex_married_all_marriages = S1101_C02_003E*prop_same_sex_married_all_marriages,
         count_same_sex_households = S1101_C01_009E*prop_same_sex_households)

metro_atlanta %>% summarize(across(count_same_sex_unions:count_same_sex_households, ~sum(.)))
