source("hrs/mhhhrs01_creating analytic datasets.R")

source("C:/code/external/functions/survey/svysummary.R")
source("C:/code/external/functions/survey/svysd.R")

library(srvyr)

msm_df = svysummary(msm_analytic_dataset_unique_svy,
           p_vars = c("c_htn","d_htn","c_htn_free"))

path_g2a_family_folder <- "C:/Cloud/OneDrive - Emory University/Papers/_Published/Crossnational Family Clustering"

source("hrs/mhhhrs_heterosexual couples.R")


  

hrs_df = heterosexual_couples %>% 
  as_survey_design(.,
                   strata = strata_weight,
                   weight = r_indweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")  %>% 
  svysummary(.,
             c_vars = c("c_htn","d_htn",
                        # "d1_htn","d2_htn",
                        "c_htn_free")) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.*100,1))

wsw_df = svysummary(wsw_analytic_dataset_unique_svy,
                    p_vars = c("c_htn","d_htn","c_htn_free")) 



hetero_age_sd = heterosexual_couples %>% 
  as_survey_design(.,
                   strata = strata_weight,
                   weight = r_indweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")  %>% 
  svysd(.,
             c_vars = c("h_age","w_age"))

msm_age_sd = msm_analytic_dataset_unique_svy  %>% 
  svysd(.,
        c_vars = c("c_younger_age","c_older_age"))
wsw_age_sd = wsw_analytic_dataset_unique_svy  %>% 
  svysd(.,
        c_vars = c("c_younger_age","c_older_age"))


# hm_df <- data.frame(variable = c("c_htn","d_htn","c_htn_free"),
#                     estimate = c(37.9,64.5-37.9, 100-64.5),
#                     lci = c(35.8, ,100-62.5),
#                     uci = c(40.0, , 100-66.6)
#                     )



(fig_col = bind_rows(hrs_df %>% mutate(couple = "Heterosexual couples, 2016-17"),
          msm_df %>% mutate(couple = "Sexual Minority Men, 2006-2019"),
          wsw_df %>% mutate(couple = "Sexual Minority Women, 2006-2019")) %>% 
  mutate(status = case_when(variable == "c_htn" ~ "Concordant hypertension",
                            variable == "d_htn" ~ "Hypertension in only one male/female partner",
                            # variable == "d_htn" & couple == "Sexual Minority Men, 2006-2019" ~ "Hypertension in male partner",
                            # variable == "d_htn" & couple == "Sexual Minority Women, 2006-2019" ~ "Hypertension in female partner",
                            # variable == "d1_htn" ~ "Hypertension in male partner",
                            # variable == "d2_htn" ~ "Hypertension in female partner",
                            variable == "c_htn_free" ~ "No hypertension among both partners"
                            )) %>% 
  mutate(
         status = factor(status,levels=c("Concordant hypertension",
                                         "Hypertension in only one male/female partner",
                                         "No hypertension among both partners"),
                         ordered = TRUE),
         # status = factor(status,levels=c("Concordant hypertension","Hypertension in male partner",
         #                                 "Hypertension in female partner","No hypertension among both partners"),
         #                 ordered = TRUE),
         couple = factor(couple,levels = c("Sexual Minority Men, 2006-2019",
                                           "Sexual Minority Women, 2006-2019",
                                           "Heterosexual couples, 2016-17"))) %>% 
  
  ggplot(data=.,aes(x=couple,y=estimate,ymin=lci,ymax=uci,fill=status)) +
    # https://stackoverflow.com/questions/38101512/the-same-width-of-the-bars-in-geom-barposition-dodge
  geom_col(position = position_dodge2(preserve = "total",width = 0.9),width = 0.5) +
  scale_fill_manual(name = "",values = c("#375a66","#698994",
                                         # "#cad8de",
                                         "grey80")) +
    # https://stackoverflow.com/questions/21878974/wrap-long-axis-labels-via-labeller-label-wrap-in-ggplot2
  scale_x_discrete(labels=function(x) str_wrap(x,width = 20)) + 
  theme_bw() +
  # https://stackoverflow.com/questions/12323416/arranging-ggplot2-legend-items-in-a-grid
  ylab("Percentage in total (%)") +
  xlab("") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  guides(fill = guide_legend(nrow = 2,ncol=2))
  )

(fig_col) %>%
  ggsave(plot = ., filename = paste0(path_hrs_same_sex_concordance_paper,"/figures/figure_column plot of concordance without SE with 3 categories.tif"),width = 8, height = 4,dpi = 600)


(fig_col +
    geom_errorbar(position = position_dodge2(preserve = "total",width = 0.9), width = 0.5, color = "grey40")) %>%
  ggsave(plot = ., filename = paste0(path_hrs_same_sex_concordance_paper,"/figures/figure_column plot of concordance with SE with 3 categories.tif"),width = 8, height = 4,dpi = 600)


# (fig_col + 
#     geom_errorbar(position = position_dodge2(preserve = "total",width = 0.9), width = 0.5, color = "grey40")) %>% 
#   ggsave(plot = ., filename = paste0(path_msm_hiv_htn_proposal,"/figures/figure_column plot of concordance.jpg"),width = 8, height = 6)
# 
# (fig_col) %>% 
#   ggsave(plot = ., filename = paste0(path_msm_hiv_htn_proposal,"/figures/figure_column plot of concordance without SE.png"),width = 8, height = 4,dpi = 600)
# 
# (fig_col) %>% 
#   ggsave(plot = ., filename = paste0(path_hrs_same_sex_concordance_paper,"/figures/figure_column plot of concordance without SE.tif"),width = 8, height = 4,dpi = 600)
# 
# (fig_col) %>% 
#   ggsave(plot = ., filename = paste0(path_hrs_same_sex_concordance_paper,"/figures/figure_column plot of concordance without SE.png"),width = 8, height = 4,dpi = 600)
# 
