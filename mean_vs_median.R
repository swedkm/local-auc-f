library(tidyverse)
library(patchwork)
library(brms)
library(readxl)
library(rstan)
#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre-1.8') # fixes error with xlsx not finding rJava
#library(xlsx)
library(ggplot2)
library(bayestestR)
library(arrow)

# set working directory
setwd("C:/Users/kswedber/OneDrive - Environmental Protection Agency (EPA)/Desktop/Adding Up Paper")

# read in data
df <- read_excel("Data/RevisedMetadata_WithStreamcatLakecat_2021.12.13.xlsx", 
                 sheet = "Metadata")

# select observation from most recent version
df <- df[which(df$Drop_April2021 == 0 & df$Drop_June2021 == 0), ]

# calculate missing variables
df$ln_ar_ratio <- log(df$SA_AREA/df$ar_total_area)

# add additional control variables
df$tax_only <- ifelse(df$PAY_MECH == 1, 1, 0)
df$user_cost <- ifelse(df$PAY_MECH == 3, 1, 0)
df$IBI <- ifelse(grepl('IBI', df$WTP_desc) | grepl('river condition', df$WTP_desc), 1, 0)

# rename water quality variables to match regression
df$q1 <- df$Index_Post_High
df$q0 <- df$Index_Base_High
df$q_mid <- (df$q1 + df$q0)/2
df$ln_q_mid <- log(df$q_mid)
df$ln_q0 <- log(df$q0)

# for MRM2 models MWTP ~ log(WTP/q1-q0) at midpoint (q1+q0/2)
df$lnMWTP <- df$lnWTP - df$lnquality_ch

# adding study and sample size information
# we don't use inv_roots here but could if we want to simulate sigma function of sample size
df$studyid <- floor(df$ESTIMATE_ID)
df$inv_rootsz <- 1/sqrt(df$STUDY_SIZE)

# adding fields for dummy shift indicators
df$dwq <- abs(df$q1 - df$q0)
df$dwq6 <- ifelse(df$dwq > 6, df$lnquality_ch, 0)

# calculate total wtp across all observations
total_wtp <- sum(df$TOTAL_WTP)

# read in model outputs
models <- c('mrm2', 'mrm2ds6', 'mrm2s') 
model_names <- c()
for (m in models){
  x = load(paste("Results/mrm output/", m, '.sid.fit.st.rds', sep=''))
  assign(paste(m, 'qmid.sid.fit.st', sep=''), get(x))
  x = load(paste("Results/mrm output/no midpoint/", m, '.sid.fit.st.rds', sep=''))
  assign(paste(m, 'q0.sid.fit.st', sep=''), get(x))
  model_names <- c(model_names, paste(m, 'qmid.sid.fit.st', sep=''), paste(m, 'q0.sid.fit.st', sep=''))
}

# set estimation inputs
cores = 1
chains = 4
iter = 11000
warmup = 1000
thin = 4
adapt_delta = .8
max_treedepth = 10
seed = 127
priors.st = c(set_prior("normal(0, 10)", class = "Intercept"),
              set_prior("cauchy(0, 10)", class = "sigma"),
              set_prior("gamma(2, .1)", class = "nu"))
priors.st.sid = c(set_prior("normal(0, 10)", class = "Intercept"),
                  set_prior("cauchy(0, 10)", class = "sigma"),
                  set_prior("gamma(2, .1)", class = "nu"),
                  set_prior("cauchy(0, 10)", class = "sd"))

# run linear model with random effects for comparison
lin.fit.g <- brm(formula = lnWTP ~ 1 , data = df, family = 'gaussian',
                  prior   = priors.g,
                  control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
                  save_pars = save_pars(all=TRUE),
                  chains = chains, iter = iter, warmup = warmup, cores = cores, thin = thin, seed = seed)

lin.fit.st <- brm(formula = lnWTP ~ 1 , data = df, family = 'student',
                      prior   = priors.st,
                      control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
                      save_pars = save_pars(all=TRUE),
                      chains = chains, iter = iter, warmup = warmup, cores = cores, thin = thin, seed = seed)
lin.sid.fit.st <- brm(formula = lnWTP ~ 1 + (1|studyid), data = df, family = 'student',
                      prior   = priors.st.sid,
                      control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
                      save_pars = save_pars(all=TRUE),
                      chains = chains, iter = iter, warmup = warmup, cores = cores, thin = thin, seed = seed)

# initialize output dataframe
col_names <-c(model_names, 'lin.fit.st', 'lin.sid.fit.st')
out_df <- as.data.frame(matrix(nrow=4, ncol=length(col_names) + 1))
colnames(out_df) <- c('summary measure', col_names)
out_df[,'summary measure'] <- c('mean', 'mean drop .999', 'mean drop .99', 'median')

# iterate through models to predict mean and median
for (m in model_names){
  # predict WTP for each observation 
  mwtp <- as.data.frame(posterior_predict(get(m), df, re_formula=NA, seed=127, transform='exp'))
  
  # drop percentiles
  mwtp2 <- mwtp
  for (col in colnames(mwtp)){
    mwtp2[,col] <-ifelse(mwtp[,col]>quantile(mwtp[,col], .999), NA, mwtp[,col])
  }
  mwtp3 <- mwtp
  for (col in colnames(mwtp)){
    mwtp3[,col] <-ifelse(mwtp[,col]>quantile(mwtp[,col], .99), NA, mwtp[,col])
  }
  
  
  # summarize using mean and median
  mwtp.summary <- cbind(as.data.frame(apply(mwtp, 2, mean)), as.data.frame(apply(mwtp2, 2, mean,  na.rm=TRUE)),
                        as.data.frame(apply(mwtp3, 2, mean,  na.rm=TRUE)), as.data.frame(apply(mwtp, 2, median))) 
  
  # rename columns
  colnames(mwtp.summary) <- c('mwtp.mean', 'mwtp.mean_999', 'mwtp.mean_99', 'mwtp.median')
  
  # calculate wtp
  mwtp.summary$wtp.mean <- mwtp.summary$mwtp.mean * exp(df$lnquality_ch)
  mwtp.summary$wtp.mean_999 <- mwtp.summary$mwtp.mean_999 * exp(df$lnquality_ch)
  mwtp.summary$wtp.mean_99 <- mwtp.summary$mwtp.mean_99 * exp(df$lnquality_ch)
  mwtp.summary$wtp.median <- mwtp.summary$mwtp.median * exp(df$lnquality_ch)
  
  # calculate totals
  total_wtp_mean <- sum(mwtp.summary$wtp.mean)
  total_wtp_mean_999 <- sum(mwtp.summary$wtp.mean_999)
  total_wtp_mean_99 <- sum(mwtp.summary$wtp.mean_99)
  total_wtp_median <- sum(mwtp.summary$wtp.median)
  
  # update output dataframe
  out_df[,m] <- c(total_wtp_mean, total_wtp_mean_999, total_wtp_mean_99, total_wtp_median)
} 

# iterate through linear models to predict mean and median
for (m in c('lin.fit.st', 'lin.sid.fit.st')){
  # predict WTP for each observation 
  mwtp <- as.data.frame(posterior_predict(get(m), as.data.frame(matrix(nrow=1, ncol=1)), 
                                          re_formula=NA, seed=127, transform='exp'))
  
  # drop percentiles
  mwtp2 <- filter(mwtp, V1 <= quantile(mwtp$V1, .999))
  mwtp3 <- filter(mwtp, V1 <= quantile(mwtp$V1, .99))

  # summarize using mean and median
  mwtp.summary <- cbind(as.data.frame(apply(mwtp, 2, mean)), as.data.frame(apply(mwtp2, 2, mean,  na.rm=TRUE)),
                        as.data.frame(apply(mwtp3, 2, mean,  na.rm=TRUE)), as.data.frame(apply(mwtp, 2, median))) 
  
  # rename columns
  colnames(mwtp.summary) <- c('wtp.mean', 'wtp.mean_999', 'wtp.mean_99', 'wtp.median')
  
  # calculate totals
  total_wtp_mean <- (mwtp.summary$wtp.mean)*nrow(df)
  total_wtp_mean_999 <- (mwtp.summary$wtp.mean_999)*nrow(df)
  total_wtp_mean_99 <- (mwtp.summary$wtp.mean_99)*nrow(df)
  total_wtp_median <- (mwtp.summary$wtp.median)*nrow(df)
  
  # update output dataframe
  out_df[,m] <- c(total_wtp_mean, total_wtp_mean_999, total_wtp_mean_99, total_wtp_median)
} 

# add in total
out_df$metadata_total <- total_wtp

# output
write.csv(out_df, 'Results/output tables/mean_median_total_wtp.csv')
