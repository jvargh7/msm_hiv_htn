gc();rm(list=ls());source(".Rprofile")
source("hrs/mhhhrs01_creating analytic datasets.R")

path_g2a_family_folder <- "C:/Cloud/OneDrive - Emory University/Papers/Crossnational Family Clustering"
heterosexual_couples <- readRDS(paste0(path_g2a_family_folder,"/working/hrs/G2A HRS Couples mi_dfs.RDS")) %>% 
  .$data

heterosexual_couples <- readRDS(paste0(path_g2a_family_folder,"/working/lasi/G2A LASI Couples mi_dfs.RDS")) %>% 
  .$data


ggplot(data = heterosexual_couples,
       aes(x= w_sbp,
           y = h_sbp)) +
  geom_point() +
  geom_vline(xintercept = 140) +
  geom_hline(yintercept = 140)

m_test = lmer(hh_children ~ h1_sbp + h2_sbp + (1|coupleid/h1_spouseidpn),data=msm_analytic_dataset_scs)

rm(msm_analytic_dataset_unique_svy,wsw_analytic_dataset_unique_svy,
   wsw_analytic_dataset_scs,wsw_analytic_dataset_unique)

# Generating covariance at t = 1
r1 = cor(msm_analytic_dataset_unique$h1_sbp,msm_analytic_dataset_unique$h2_sbp)
r2 = cor(heterosexual_couples$h_sbp,heterosexual_couples$w_sbp)

cov1 = matrix(c(1,r1^2,r1^2,1),nrow=2)
cov2 = matrix(c(1,r2^2,r2^2,1),nrow=2)

m1 = c(mean(msm_analytic_dataset_unique$h1_sbp),mean(msm_analytic_dataset_unique$h2_sbp))
m2 = c(mean(heterosexual_couples$h_sbp),mean(heterosexual_couples$w_sbp))

sd1 = sd(c(msm_analytic_dataset_unique$h1_sbp,msm_analytic_dataset_unique$h2_sbp))
sd2 = sd(c(heterosexual_couples$h_sbp,heterosexual_couples$w_sbp))

icc_to_params = function(res_var,icc = 0.1){
  
  V1 = (icc/(1-icc))*res_var
  
  return(V1)
  
}


# Parameters -----------
N = 200
b0 = -3
b1 = 0.8
b2 = 0.3
b3 = 0.5
b = c(b0,b1,b2,b3)  # fixed intercept

s = sd1-2 # residual SD
V1 =  icc_to_params(s^2,icc = 0.1)# random intercept variance at level = household
V2 = icc_to_params(s^2,icc = 0.2) # random intercept variance at level = individual
V1_V2 = list(V1,V2)

# Data generation -----------
y_t1 = MASS::mvrnorm(n = N,mu = m1,Sigma = cov1,empirical = TRUE)

yi_t1 = y_t1[,1]
yp_t1 = y_t1[,2]


group_ids = factor(as.character(paste0("Group ",seq(1:N))))
id_1_ids = factor(as.character(paste0("ID1 ",seq(1:N)))) 
id_2_ids = factor(as.character(paste0("ID2 ",seq(1:N)))) 

# Two time points -------

# Parameters 

yi_tPrev1 = c(yi_t1,yp_t1)
yp_tPrev1 = c(yp_t1,yi_t1)

c1 = rep(group_ids,times=2*1)
X1 = bind_cols(yi_tPrev1,yp_tPrev1,c1)
names(X1) = c("yi_t1","yp_t1","c")


model1 <- makeLmer(y ~ yi_t1 + yp_t1 + (1|c1), fixef=b[1:3], VarCorr=V1, sigma=s, data=X1)
model1b <- makeGlmer(y ~ yi_t1 + yp_t1 + (1|c1), 
                     family = "binomial",
                     fixef=b[1:3], VarCorr=V1, data=X1)
print(model1)

powerSim(model1, nsim=100,test = fcompare(y~yi_t1))


# 3 time points ------------


change_yi = rnorm(N,0,2)
change_yp = rnorm(N,0,2)

yi_t2 = yi_t1 + change_yi
yp_t2 = yp_t1 + change_yp

yi_tPrev2 = c(yi_t1,yi_t2,yp_t1,yp_t2)
yp_tPrev2 = c(yp_t1,yp_t2,yi_t1,yi_t2)

c2 = rep(group_ids,times=2*2)
id_i = rep(id_1_ids,times=2)
id_p = rep(id_2_ids,times=2)
wave = rep(c(1:2),times=2,each=N)
X2 = bind_cols(yi_tPrev2,yp_tPrev2,c2,c(id_i,id_p),wave)
names(X2) = c("yi_t1","yp_t1","c","id","wave")


model2 <- makeLmer(y ~ yi_t1 + yp_t1 + wave + (1|c/id), fixef=b, VarCorr=V1_V2, sigma=s, data=X2)
print(model2)

powerSim(model2, nsim=100,test = fcompare(y~yi_t1))

# 4 time points --------------
change_yi_2 = rnorm(N,0,5)
change_yp_2 = rnorm(N,0,5)

yi_t3 = yi_t2 + change_yi_2
yp_t3 = yp_t2 + change_yp_2


yi_tPrev3 = c(yi_t1,yi_t2,yi_t3,yp_t1,yp_t2,yp_t3)
yp_tPrev3 = c(yp_t1,yp_t2,yp_t3,yi_t1,yi_t2,yi_t3)

c3 = rep(group_ids,times=2*3)
id_i = rep(id_1_ids,times=3)
id_p = rep(id_2_ids,times=3)
wave = rep(c(1:3),times=2,each=N)
X3 = bind_cols(yi_tPrev3,yp_tPrev3,c3,c(id_i,id_p),wave)
names(X3) = c("yi_t1","yp_t1","c","id","wave")


model3 <- makeLmer(y ~ yi_t1 + yp_t1 + wave + (1|c/id), fixef=b, VarCorr=V1_V2, sigma=s, data=X3)
print(model3)

powerSim(model3, nsim=100,test = fcompare(y~yi_t1))

doTest(model1,test = fixed("yp_t1","z"))
doTest(model2,test = fixed("yp_t1","z"))
doTest(model3,test = fixed("yp_t1","z"))


