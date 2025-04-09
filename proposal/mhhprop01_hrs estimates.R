path_g2a_family_folder <- "C:/Cloud/OneDrive - Emory University/Papers/Crossnational Family Clustering"
heterosexual_couples <- readRDS(paste0(path_g2a_family_folder,"/working/hrs/G2A HRS Couples mi_dfs.RDS")) %>% 
  .$data

w_selected = c("w_htn")
h_selected = c("h_htn")
hh_selected = "hhid"

wives = heterosexual_couples %>% 
  dplyr::select(one_of(w_selected),one_of(hh_selected)) %>% 
  rename_at(vars(one_of(w_selected)),~str_replace(.,"^w_",""))

husbands = heterosexual_couples %>% 
  dplyr::select(one_of(h_selected),one_of(hh_selected)) %>% 
  rename_at(vars(one_of(h_selected)),~str_replace(.,"^h_",""))

pooled <- bind_rows(wives %>% 
                      mutate(husband = 0) %>% 
                      left_join(husbands %>% 
                                  dplyr::select(hhid,htn) %>% 
                                  rename(partner_htn = htn),
                                by = "hhid"),
                    
                    husbands %>% 
                      mutate(husband = 1) %>% 
                      left_join(wives %>% 
                                  dplyr::select(hhid,htn) %>% 
                                  rename(partner_htn = htn),
                                by = "hhid"))

glmer_pooled <- glmer(htn ~ partner_htn + (1|hhid),data=pooled,family=binomial()) 

glmer_pooled %>% 
  summary()

performance::icc(glmer_pooled)


# https://stats.stackexchange.com/questions/429685/rxc-contingency-table-to-2x2-tables-for-local-correlation-analysis
# https://search.r-project.org/CRAN/refmans/confintr/html/cramersv.html
x = chisq.test(pooled$htn,pooled$partner_htn)
confintr::cramersv(x)

x = chisq.test(matrix(c(60,40,40,60)*0.5,nrow=2,ncol=2))
x
confintr::cramersv(x)
