
source("C:/code/external/functions/survey/svysummary.R")
source("C:/code/external/functions/survey/svysd.R")

path_g2a_family_folder <- "C:/Cloud/OneDrive - Emory University/Papers/_Published/Crossnational Family Clustering"

source("hrs/mhhhrs_heterosexual couples.R")

grouped = heterosexual_couples %>% 
  as_survey_design(.,
                   strata = strata_weight,
                   weight = r_indweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")  %>% 
  svysummary(.,
             c_vars = c("c_htn","d1_htn","d2_htn","c_htn_free"),
             id_vars = "c_raceethnicity") %>% 
  mutate_at(vars(estimate,lci,uci),~round(.*100,1)) 

(fig_col = grouped %>% 
    mutate(status = case_when(variable == "c_htn" ~ "Concordant hypertension",
                              variable == "d1_htn" ~ "Hypertension in male partner",
                              variable == "d2_htn" ~ "Hypertension in female partner",
                              variable == "c_htn_free" ~ "No hypertension among both partners"
    )) %>% 
    mutate(status = factor(status,levels=c("Concordant hypertension","Hypertension in male partner",
                                           "Hypertension in female partner","No hypertension among both partners"),
                           ordered = TRUE),
           c_raceethnicity = factor(c_raceethnicity,levels=c("NHBlack","NHWhite","Hispanic","NHOther","Mixed"),
                                    labels=c("Black","White","Hispanic","Other","Mixed")),
           text_label = case_when(variable == "c_htn" ~ sprintf("%.01f",estimate),
                                  TRUE ~ NA_character_)) %>% 
    dplyr::filter(status == "Concordant hypertension") %>% 
    
    ggplot(data=.,aes(x=c_raceethnicity,y=estimate,ymin=lci,ymax=uci,fill=c_raceethnicity,label = text_label)) +
    # https://stackoverflow.com/questions/38101512/the-same-width-of-the-bars-in-geom-barposition-dodge
    geom_col(position = position_dodge(width = 0.9),width = 0.5) +
    geom_text(aes(y = estimate + 4),position = position_dodge(width = 0.9),width = 0.5,size=6) +
    scale_fill_manual(name = "",values = c("#FF6961","#375a66","#375a66","#375a66","#375a66")) +
    # https://stackoverflow.com/questions/21878974/wrap-long-axis-labels-via-labeller-label-wrap-in-ggplot2
    scale_x_discrete(labels=function(x) str_wrap(x,width = 20)) + 
    theme_bw() +
    # https://stackoverflow.com/questions/12323416/arranging-ggplot2-legend-items-in-a-grid
    ylab("Percentage in total (%)") +
    xlab("") +
    theme(legend.position = "none",
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12)))

(fig_col) %>% 
  ggsave(plot = ., filename = paste0(path_msm_hiv_htn_proposal,"/figures/figure_column plot of heterosexual concordance by raceeth.jpg"),width = 6, height = 3)
