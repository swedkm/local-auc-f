library(tidyverse)
library(patchwork)
library(brms)
library(readxl)
library(rstan)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre-1.8') # fixes error with xlsx not finding rJava
library(xlsx)
library(ggplot2)
library(bayestestR)
library(arrow)

# set working directory
setwd("C:/Users/kswedber/OneDrive - Environmental Protection Agency (EPA)/Desktop/Adding Up Paper")

# set estimation inputs
cores = 1
chains = 4
iter = 11000
warmup = 1000
thin = 4
adapt_delta = .8
max_treedepth = 10
seed = 127
priors.st.sid = c(set_prior("normal(0, 10)", class = "b"),
                  set_prior("normal(0, 10)", class = "Intercept"),
                  set_prior("cauchy(0, 10)", class = "sigma"),
                  set_prior("gamma(2, .1)", class = "nu"),
                  set_prior("cauchy(0, 10)", class = "sd"))

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
df$dwq3 <- ifelse(df$dwq > 3, df$lnquality_ch, 0)
df$dwq4 <- ifelse(df$dwq > 4, df$lnquality_ch, 0)
df$dwq5 <- ifelse(df$dwq > 5, df$lnquality_ch, 0)
df$dwq6 <- ifelse(df$dwq > 6, df$lnquality_ch, 0)
df$dwq7 <- ifelse(df$dwq > 7, df$lnquality_ch, 0)
df$dwq8 <- ifelse(df$dwq > 8, df$lnquality_ch, 0)
df$dwq9 <- ifelse(df$dwq > 9, df$lnquality_ch, 0)
df$dwq11 <- ifelse(df$dwq > 11, df$lnquality_ch, 0)
df$dwq16 <- ifelse(df$dwq > 16, df$lnquality_ch, 0)

# adding log(dq+1) to test continuous extrapolation
df$ln_dq1 <- log(df$dwq + 1)

# add dummy variable for river_stream
df$River_Stream <- ifelse(df$WATER_TYPE %in% c(1,3,6), 1, 0)

# splitting out testing sample
df_train <- filter(df, dwq >= 10)

#################################################################################
#ONLY RUN ONCE
# run mrm2, mrm2s, and mrm2ds6 on subsamples
# mrm2.qmid.sid <- (lnMWTP ~ ln_q_mid  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +
#                lnincome1 + census_south + census_midwest + census_west +
#                nonusers + swim_use + gamefish + volunt + tax_only + user_cost +
#                lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
# mrm2.qmid.fit <- brm(formula = mrm2.qmid.sid,
#                  data    = df_train, family = 'student',
#                  prior   = priors.st.sid,
#                  control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
#                  save_pars = save_pars(all=TRUE),
#                  chains = chains, iter = iter, warmup = warmup, seed = seed)
# save(mrm2.qmid.fit, file="Results/mrm output/train wqi 10/mrm2_qmid.rds")
# 
# mrm2.q0.sid <- (lnMWTP ~ ln_q0  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +
#                   lnincome1 + census_south + census_midwest + census_west +
#                   nonusers + swim_use + gamefish + volunt + tax_only + user_cost +
#                   lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
# mrm2.q0.fit <- brm(formula = mrm2.q0.sid,
#                  data    = df_train, family = 'student',
#                  prior   = priors.st.sid,
#                  control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
#                  save_pars = save_pars(all=TRUE),
#                  chains = chains, iter = iter, warmup = warmup, seed = seed)
# save(mrm2.q0.fit, file="Results/mrm output/train wqi 10/mrm2_q0.rds")
# 
# 
# mrm2ds11.q0.sid <- (lnMWTP ~ ln_q0 + dwq11  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +
#                   lnincome1 + census_south + census_midwest + census_west +
#                   nonusers + swim_use + gamefish + volunt + tax_only + user_cost +
#                   lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
# mrm2ds11.q0.fit <- brm(formula = mrm2ds11.q0.sid,
#                     data    = df_train, family = 'student',
#                     prior   = priors.st.sid,
#                     control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
#                     save_pars = save_pars(all=TRUE),
#                     chains = chains, iter = iter, warmup = warmup, seed = seed)
# save(mrm2ds11.q0.fit, file="Results/mrm output/train wqi 10/mrm2ds11_q0.rds")
# 
# mrm2ds11.qmid.sid <- (lnMWTP ~ ln_q_mid + dwq11  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +
#                       lnincome1 + census_south + census_midwest + census_west +
#                       nonusers + swim_use + gamefish + volunt + tax_only + user_cost +
#                       lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
# mrm2ds11.qmid.fit <- brm(formula = mrm2ds11.qmid.sid,
#                        data    = df_train, family = 'student',
#                        prior   = priors.st.sid,
#                        control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
#                        save_pars = save_pars(all=TRUE),
#                        chains = chains, iter = iter, warmup = warmup, seed = seed)
# save(mrm2ds11.qmid.fit, file="Results/mrm output/train wqi 10/mrm2ds11_qmid.rds")
# 
# mrm2ds16.q0.sid <- (lnMWTP ~ ln_q0 + dwq16  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +
#                       lnincome1 + census_south + census_midwest + census_west +
#                       nonusers + swim_use + gamefish + volunt + tax_only + user_cost +
#                       lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
# mrm2ds16.q0.fit <- brm(formula = mrm2ds16.q0.sid,
#                        data    = df_train, family = 'student',
#                        prior   = priors.st.sid,
#                        control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
#                        save_pars = save_pars(all=TRUE),
#                        chains = chains, iter = iter, warmup = warmup, seed = seed)
# save(mrm2ds16.q0.fit, file="Results/mrm output/train wqi 10/mrm2ds16_q0.rds")
# 
# mrm2ds16.qmid.sid <- (lnMWTP ~ ln_q_mid + dwq16  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +
#                         lnincome1 + census_south + census_midwest + census_west +
#                         nonusers + swim_use + gamefish + volunt + tax_only + user_cost +
#                         lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
# mrm2ds16.qmid.fit <- brm(formula = mrm2ds16.qmid.sid,
#                          data    = df_train, family = 'student',
#                          prior   = priors.st.sid,
#                          control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
#                          save_pars = save_pars(all=TRUE),
#                          chains = chains, iter = iter, warmup = warmup, seed = seed)
# save(mrm2ds16.qmid.fit, file="Results/mrm output/train wqi 10/mrm2ds16_qmid.rds")
# 
# mrm2s.q0.sid <- (lnMWTP ~ ln_q0 + lnquality_ch  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +
#                 lnincome1 + census_south + census_midwest + census_west +
#                 nonusers + swim_use + gamefish + volunt + tax_only + user_cost +
#                 lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
# mrm2s.q0.fit <- brm(formula = mrm2s.q0.sid,
#                  data    = df_train, family = 'student',
#                  prior   = priors.st.sid,
#                  control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
#                  save_pars = save_pars(all=TRUE),
#                  chains = chains, iter = iter, warmup = warmup, seed = seed)
# save(mrm2s.q0.fit, file="Results/mrm output/train wqi 10/mrm2s_q0.rds")
# 
# mrm2s.qmid.sid <- (lnMWTP ~ ln_q_mid + lnquality_ch  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +
#                 lnincome1 + census_south + census_midwest + census_west +
#                 nonusers + swim_use + gamefish + volunt + tax_only + user_cost +
#                 lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
# mrm2s.qmid.fit <- brm(formula = mrm2s.qmid.sid,
#                        data    = df_train, family = 'student',
#                        prior   = priors.st.sid,
#                        control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
#                        save_pars = save_pars(all=TRUE),
#                        chains = chains, iter = iter, warmup = warmup, seed = seed)
# save(mrm2s.qmid.fit, file="Results/mrm output/train wqi 10/mrm2s_qmid.rds")

##################################################################################
load("Results/mrm output/train wqi 10/mrm2_qmid.rds")
load("Results/mrm output/train wqi 10/mrm2_q0.rds")
load("Results/mrm output/train wqi 10/mrm2ds11_qmid.rds")
load("Results/mrm output/train wqi 10/mrm2ds11_q0.rds")
load("Results/mrm output/train wqi 10/mrm2ds16_qmid.rds")
load("Results/mrm output/train wqi 10/mrm2ds16_q0.rds")
load("Results/mrm output/train wqi 10/mrm2s_qmid.rds")
load("Results/mrm output/train wqi 10/mrm2s_q0.rds")

# subset test samples
df_test <- filter(df, dwq <= 5 & River_Stream == 1)

# initialize output dataframes
df_pred <- as.data.frame(matrix(nrow=nrow(df_test), ncol=0))
df_pred$Total_WTP <- df_test$TOTAL_WTP
df_violin <- as.data.frame(matrix(nrow=0, ncol=6))
colnames(df_violin) <- c('WTP', 'Model', 'pred.i', 'median', 'CI_low', 'CI_high')

# iterate through models to predict and update dataframe
models = c('mrm2.qmid', 'mrm2.q0', 'mrm2ds11.qmid', 'mrm2ds11.q0',
           'mrm2ds16.qmid', 'mrm2ds16.q0', 'mrm2s.qmid', 'mrm2s.q0',
           'mrm2smin.qmid', 'mrm2smin.q0')
ci= .90
for (model in models){
  # predict wtp
  if (model == 'mrm2smin.qmid'){
    # set input quality change for predictions to minimum of new sample
    df_test$lnquality_ch <- log(min(df_train$dwq))
    mwtp <- as.data.frame(posterior_predict(get(paste('mrm2s.qmid', '.fit', sep='')), df_test, re_formula=NA, seed=127, transform='exp'))
  }
  else if (model == 'mrm2smin.q0'){
    # set input quality change for predictions to minimum of new sample
    df_test$lnquality_ch <- log(min(df_train$dwq))
    mwtp <- as.data.frame(posterior_predict(get(paste('mrm2s.q0', '.fit', sep='')), df_test, re_formula=NA, seed=127, transform='exp'))
  }
  else {
    mwtp <- as.data.frame(posterior_predict(get(paste(model, '.fit', sep='')), df_test, re_formula=NA, seed=127, transform='exp'))
  }
  wtp <- mwtp * df_test$dwq
  
  # summarize predictions and concatenate
  wtp.summary <- ci(wtp, method='HDI', ci=ci)
  wtp.summary <- cbind(wtp.summary[,c('CI_low', 'CI_high')], as.data.frame(apply(wtp, 2, median)))
  
  # calculate error and flag underestimate
  wtp.summary$error <- df_pred$Total_WTP - wtp.summary[,'apply(wtp, 2, median)']
  wtp.summary$error2 <- wtp.summary$error^2
  wtp.summary$in_ci <- ifelse(df_pred$Total_WTP < wtp.summary$CI_high & df_pred$Total_WTP > wtp.summary$CI_low, 1, 0)
  wtp.summary$under <- ifelse(wtp.summary$error > 0, 1, 0)
  
  # set column names
  colnames(wtp.summary) <- c(paste(model, '.wtp.ci_', as.character(100*ci), '_low', sep=''), 
                             paste(model, '.wtp.ci_', as.character(100*ci), '_high', sep=''), 
                             paste(model, '.wtp.median', sep=''),
                             paste(model, '.wtp.error', sep=''),
                             paste(model, '.wtp.error2', sep=''),
                             paste(model, '.in_ci', sep=''),
                             paste(model, '.wtp.under', sep=''))
  
  # stack wtp predictions for violin plot
  stacked = c()
  pred.i = c()
  for (i in colnames(wtp)){
    wtp.i = wtp[,i]
    stacked = c(stacked, wtp.i)
    pred.i = c(pred.i, i)
  }
  
  # add additional columns
  stacked <- as.data.frame(stacked)
  colnames(stacked) <- c('WTP')
  stacked$Model <- model
  stacked$pred.i <- pred.i
  
  # summarize
  stacked.summary <- ci(stacked[,'WTP'], method='HDI', ci=ci)
  stacked$median <- median(stacked[,'WTP'])
  stacked$CI_low <- stacked.summary$CI_low
  stacked$CI_high <- stacked.summary$CI_high
  
  # add to summary
  df_pred[,paste(model, '.wtp.median.all', sep='')] <- median(stacked[,'WTP'])
  df_pred[,paste(model, '.wtp.CI_low.all', sep='')] <- stacked.summary$CI_low
  df_pred[,paste(model, '.wtp.CI_high.all', sep='')] <- stacked.summary$CI_high
  
  # filter
  stacked <- filter(stacked, WTP <= CI_high & WTP >= CI_low)
  
  # bind with output tables
  df_pred <- cbind(df_pred, wtp.summary)
  df_violin <- rbind(df_violin, stacked)
  
  # output progress
  message(as.character(model))
  
}

# add columns for generic errors from no modelling
df_pred$sample.median.wtp.error <- df_pred$Total_WTP - median(df_train$TOTAL_WTP)
df_pred$sample.median.wtp.error2 <- df_pred$sample.median.wtp.error^2
df_pred$sample.median.avgwtp.error <- df_pred$Total_WTP - (median(df_train$TOTAL_WTP/df_train$dwq)*df_test$dwq)
df_pred$sample.median.avgwtp.error2 <- df_pred$sample.median.avgwtp.error^2

# output finals predictions
write.csv(df_pred, file="Results/output tables/predictions_summary_trainwqi10.csv")
write_parquet(df_violin, "Results/output tables/predictions_violin_trainwqi10.pqt")

# summarize rmse and other measures
rmse = as.data.frame(matrix(nrow=6, ncol=0))
rmse$label <- c('median', 'CI_low', 'CI_high', 'rmse', 'percent under', 'percent in ci')
for (model in models){
  rmse[,model] <-c(df_pred[1,paste(model, '.wtp.median.all', sep='')],
                   df_pred[1,paste(model, '.wtp.CI_low.all', sep='')],
                   df_pred[1,paste(model, '.wtp.CI_high.all', sep='')],
                   sqrt(sum(df_pred[,paste(model, '.wtp.error2', sep='')])),
                   sum(df_pred[,paste(model, '.wtp.under', sep='')])/nrow(df_pred),
                   sum(df_pred[,paste(model, '.in_ci', sep='')])/nrow(df_pred))
  }
# add column for metadata test sample
rmse$train_sample <- c(paste('dq Range', as.character(min(df_train$dwq)), as.character(max(df_train$dwq))),
                       paste('WTP median', as.character(median(df_train$TOTAL_WTP))), 
                       paste('WTP Range', as.character(min(df_train$TOTAL_WTP)), as.character(max(df_train$TOTAL_WTP))),
                       paste('Avg WTP median', as.character(median(df_train$TOTAL_WTP/df_train$dwq))), 
                       paste('Avg WTP Range', as.character(min(df_train$TOTAL_WTP/df_train$dwq)), as.character(max(df_train$TOTAL_WTP/df_train$dwq))),
                       paste('n', as.character(nrow(df_train))))
rmse$test_sample <- c(paste('dq Range', as.character(min(df_test$dwq)), as.character(max(df_test$dwq))),
                      paste('WTP median', as.character(median(df_test$TOTAL_WTP))), 
                      paste('WTP Range', as.character(min(df_test$TOTAL_WTP)), as.character(max(df_test$TOTAL_WTP))),
                      paste('Avg WTP median', as.character(median(df_test$TOTAL_WTP/df_test$dwq))), 
                      paste('Avg WTP Range', as.character(min(df_test$TOTAL_WTP/df_test$dwq)), as.character(max(df_test$TOTAL_WTP/df_test$dwq))),
                      paste('n', as.character(nrow(df_test))))
rmse$nomodel <- c(paste('wtp median ', as.character(sqrt(sum(df_pred[,'sample.median.wtp.error2'])))),
                  paste('avgwtp median ', as.character(sqrt(sum(df_pred[,'sample.median.avgwtp.error2'])))),
                  NULL,NULL,NULL,NULL)

write.csv(rmse, file="Results/output tables/rmse_trainwqi10.csv")

# might want to clean by credible interval before stacking test after first

# may want summary measure in ci or not

# maybe try model that is s+ i.e. log(dq+1) then we've contributed two things see which is better
# then we'll have 2 results an adding up and not adding up compliant

# violin plot
ggplot(df_violin, aes(x=Model, y=WTP)) + 
  geom_violin(fill='blue') + 
  stat_summary(fun.y=median, geom="point", size=2, color="black") +
  theme_classic() +   
  geom_hline(yintercept=median(df_test$TOTAL_WTP), color='red') + 
  geom_hline(yintercept=max(df_test$TOTAL_WTP), color='red', linetype=2) +
  geom_hline(yintercept=min(df_test$TOTAL_WTP), color='red', linetype=2)

ggplot(filter(df_violin, Model%in%c('mrm2.q0', 'mrm2ds11.q0',
                                    'mrm2ds16.q0', 'mrm2s.q0',
                                    'mrm2smin.q0')), aes(x=Model, y=WTP)) + 
  geom_violin(fill='blue') + 
  stat_summary(fun.y=median, geom="point", size=2, color="black") +
  theme_classic() +   
  geom_hline(yintercept=median(df_test$TOTAL_WTP), color='red') + 
  geom_hline(yintercept=max(df_test$TOTAL_WTP), color='red', linetype=2) +
  geom_hline(yintercept=min(df_test$TOTAL_WTP), color='red', linetype=2)
