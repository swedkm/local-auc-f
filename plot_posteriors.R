library(tidyverse)
library(patchwork)
library(brms)
library(readxl)
library(rstan)
#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre-1.8') # fixes error with xlsx not finding rJava
#library(xlsx)
library(bayesplot)
str(color_scheme_get()) ## gives color scheme for plots to use in other figures. 
# can change color scheme with color_scheme_set()

# set working directory
setwd("C:/Users/kswedber/OneDrive - Environmental Protection Agency (EPA)/Desktop/Adding Up Paper")

# read in all models and assign new names
model_names <- c('mrm2.sid.fit.st', 'mrm2ds3.sid.fit.st', 'mrm2ds4.sid.fit.st', 'mrm2ds5.sid.fit.st', 'mrm2ds6.sid.fit.st', 
                 'mrm2ds7.sid.fit.st', 'mrm2ds8.sid.fit.st', 'mrm2ds9.sid.fit.st', 'mrm2ds10.sid.fit.st', 'mrm2ds11.sid.fit.st',
                 'mrm2ds12.sid.fit.st', 'mrm2ds13.sid.fit.st', 'mrm2ds14.sid.fit.st', 'mrm2ds15.sid.fit.st', 'mrm2ds20.sid.fit.st', 'mrm2s.sid.fit.st')
names_list <- c()
for (m in model_names){
  x = load(paste('Results/mrm output/', m, '.rds', sep=''))
  assign(paste(str_sub(m,1,-11), 'qmid', sep=''), get(x))
  x = load(paste('Results/mrm output/no midpoint/', m, '.rds', sep=''))
  assign(paste(str_sub(m,1,-11), 'q0', sep=''), get(x))
  names_list <- c(names_list, paste(str_sub(m,1,-11), 'qmid', sep=''), paste(str_sub(m,1,-11), 'q0', sep=''))
}

ln_qmid_draws = as.data.frame(matrix(nrow=10000, ncol=0))
for (model in c('mrm2.qmid', 'mrm2ds6.qmid', 'mrm2s.qmid')){
  draws <- as.data.frame(as.data.frame(get(model))$b_ln_q_mid)
  colnames(draws) <- c(model)
  ln_qmid_draws <- cbind(ln_qmid_draws, draws)
}
ln_q0_draws = as.data.frame(matrix(nrow=10000, ncol=0))
for (model in c('mrm2.q0', 'mrm2ds6.q0', 'mrm2s.q0')){
  draws <- as.data.frame(as.data.frame(get(model))$b_ln_q0)
  colnames(draws) <- c(model)
  ln_q0_draws <- cbind(ln_q0_draws, draws)
}

a <- mcmc_areas(ln_qmid_draws, prob = 0.95) + labs(x = 'Midpoint', tag='A')
b <- mcmc_areas(ln_q0_draws, prob = 0.95) + labs(x = 'Baseline', tag='B')
a + b
ggsave('Results/output tables/qmid_q0_posteriors.png', width=6.5, height=4, dpi=1000)
