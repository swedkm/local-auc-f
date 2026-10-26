library(tidyverse)
library(patchwork)
library(brms)
library(readxl)
library(rstan)
library(coda)
library(ggplot2)
library(bayestestR)

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

# load model output
model_names <- c('mrm2.sid.fit.st', 'mrm2ds6.sid.fit.st', 'mrm2ds10.sid.fit.st', 
                 'mrm2ds20.sid.fit.st', 'mrm2s.sid.fit.st')
for (m in model_names){
  x = load(paste('Results/mrm output/', m, '.rds', sep=''))
  assign(m, get(x))
}

# set bt data to mean of meta-data and bensplash defaults
bt_data <- as.data.frame(matrix(nrow=1, ncol=20))
colnames(bt_data) <- c('lnyear', 'ln_ar_agr', 'ln_ar_ratio', 'sub_frac', 
                       'lnincome1', 'census_south', 'census_midwest', 'census_west',
                       'nonusers', 'swim_use', 'gamefish',
                       'volunt', 'tax_only', 'user_cost', 'lump_sum', 'OneShotVal',  
                       'thesis', 'non_reviewed','RUM', 'IBI')
bt_data[1,] <-  c(max(df$lnyear), mean(df$ln_ar_agr), mean(df$ln_ar_ratio),
                  mean(df$sub_frac), mean(df$lnincome1), 0, 1, 0, 0, 0, 0,
                  0, mean(filter(df, volunt == 0)$tax_only), mean(filter(df, volunt == 0)$user_cost),
                  0, mean(df$OneShotVal), 0, 0, mean(df$RUM), 0)

# initializing adding up output table
adding_up = as.data.frame(matrix(nrow=16, ncol=10))
colnames(adding_up) <- c('ratio', 'total_change', 'q0', 'q1', 'mrm2', 'mrm2ds6', 'mrm2ds10', 'mrm2ds20', 'mrm2s', 'mrm2smin')

# iterating through changes  
changes <- list(.01, .1, .5,  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80)
baselines <- list(10, 20, 50, 80)
ratios <- list(.25, .5, .75)
i = 1
for (r in ratios){
  message(r)
for (base in baselines){
  message(base)
for (ch in changes){
  message(ch)
  # initialize dataframe with baseline q0 and q1 with 25% and 75%
  wq_a <- as.data.frame(matrix(nrow=3, ncol=2))
  colnames(wq_a) <- c('q0', 'q1')
  wq_a$q0[1] <- base
  wq_a$q1[1] <- wq_a$q0[1] + ch
  wq_a$q0[2] <- wq_a$q0[1]
  wq_a$q1[2] <- wq_a$q0[2] + r * ch
  wq_a$q0[3] <- wq_a$q1[2]
  wq_a$q1[3] <- wq_a$q0[3] + (1-r) * ch
  
  # add wq data to output
  adding_up[i:(i+2), 'ratio'] <- c(r,r,r)
  adding_up[i:(i+2), 'total_change'] <- c(ch,ch,ch)
  adding_up[i:(i+2), 'q0'] <- wq_a$q0
  adding_up[i:(i+2), 'q1'] <- wq_a$q1
  
  # calculate additional variables
  wq_a$ln_q_mid <- log((wq_a$q1 + wq_a$q0) / 2)
  wq_a$lnquality_ch <- log(abs(wq_a$q1 - wq_a$q0))
  wq_a$dwq <- abs(wq_a$q1 - wq_a$q0)
  wq_a$dwq6 <- ifelse(wq_a$dwq > 6, wq_a$lnquality_ch, 0)
  wq_a$dwq10 <- ifelse(wq_a$dwq > 10, wq_a$lnquality_ch, 0)
  wq_a$dwq20 <- ifelse(wq_a$dwq > 20, wq_a$lnquality_ch, 0)
  
  # conduct different adding up tests for each model
  pred <- as.data.frame(posterior_predict(mrm2.sid.fit.st, cbind(wq_a[c('ln_q_mid')], bt_data), re_formula=NA, seed=127, transform='exp'))
  pred <- as.data.frame(apply(pred, 2, median))
  adding_up[i:(i+2), 'mrm2'] <- pred*wq_a$dwq
  
  pred <- as.data.frame(posterior_predict(mrm2ds6.sid.fit.st, cbind(wq_a[c('ln_q_mid', 'dwq6')], bt_data), re_formula=NA, seed=127, transform='exp'))
  pred <- as.data.frame(apply(pred, 2, median))
  adding_up[i:(i+2), 'mrm2ds6'] <- pred*wq_a$dwq
  
  pred <- as.data.frame(posterior_predict(mrm2ds10.sid.fit.st, cbind(wq_a[c('ln_q_mid', 'dwq10')], bt_data), re_formula=NA, seed=127, transform='exp'))
  pred <- as.data.frame(apply(pred, 2, median))
  adding_up[i:(i+2), 'mrm2ds10'] <- pred*wq_a$dwq
  
  pred <- as.data.frame(posterior_predict(mrm2ds20.sid.fit.st, cbind(wq_a[c('ln_q_mid', 'dwq20')], bt_data), re_formula=NA, seed=127, transform='exp'))
  pred <- as.data.frame(apply(pred, 2, median))
  adding_up[i:(i+2), 'mrm2ds20'] <- pred*wq_a$dwq
  
  pred <- as.data.frame(posterior_predict(mrm2s.sid.fit.st, cbind(wq_a[c('ln_q_mid', 'lnquality_ch')], bt_data), re_formula=NA, seed=127, transform='exp'))
  pred <- as.data.frame(apply(pred, 2, median))
  adding_up[i:(i+2), 'mrm2s'] <- pred*wq_a$dwq
  
  # change lnquality_ch to minimum of metadata before mrm2smin
  if (ch < 2.38){
    wq_a$lnquality_ch <- log(2.38)
  } else if (ch > 57.5){
    wq_a$lnquality_ch <- log(57.5)
  } else {
    wq_a$lnquality_ch <- log(ch)
  }
  pred <- as.data.frame(posterior_predict(mrm2s.sid.fit.st, cbind(wq_a[c('ln_q_mid', 'lnquality_ch')], bt_data), re_formula=NA, seed=127, transform='exp'))
  pred <- as.data.frame(apply(pred, 2, median))
  adding_up[i:(i+2), 'mrm2smin'] <- pred*wq_a$dwq
  
  # calculate percent bias from adding up
  adding_up[(i+3), 'ratio'] <- r
  adding_up[(i+3), 'total_change'] <- ch
  adding_up[(i+3), 'q0'] <- base
  adding_up[(i+3), 'mrm2'] <- (adding_up$mrm2[i] - (adding_up$mrm2[(i+1)] + adding_up$mrm2[(i+2)])) / adding_up$mrm2[i] * 100 
  adding_up[(i+3), 'mrm2ds6'] <- (adding_up$mrm2ds6[i] - (adding_up$mrm2ds6[(i+1)] + adding_up$mrm2ds6[(i+2)])) / adding_up$mrm2ds6[i] * 100
  adding_up[(i+3), 'mrm2ds10'] <- (adding_up$mrm2ds10[i] - (adding_up$mrm2ds10[(i+1)] + adding_up$mrm2ds10[(i+2)])) / adding_up$mrm2ds10[i] * 100
  adding_up[(i+3), 'mrm2ds20'] <- (adding_up$mrm2ds20[i] - (adding_up$mrm2ds20[(i+1)] + adding_up$mrm2ds20[(i+2)])) / adding_up$mrm2ds20[i] * 100
  adding_up[(i+3), 'mrm2s'] <- (adding_up$mrm2s[i] - (adding_up$mrm2s[(i+1)] + adding_up$mrm2s[(i+2)])) / adding_up$mrm2s[i] * 100
  adding_up[(i+3), 'mrm2smin'] <- (adding_up$mrm2smin[i] - (adding_up$mrm2smin[(i+1)] + adding_up$mrm2smin[(i+2)])) / adding_up$mrm2smin[i] * 100
  
  i = i+4
    
}
}
}  
write.csv(adding_up, file="Results/output tables/adding_up_compare_ds.csv", row.names=FALSE)
adding_up_bias <- filter(adding_up, is.na(q1))
write.csv(adding_up_bias, file="Results/output tables/adding_up_compare_ds_bias.csv", row.names=FALSE)

# subset specifically for paper tables
subset <- filter(adding_up, ratio == .25 & total_change %in% c(1, 10) & q0 %in% c(50, 50.25, 52.5))
write.csv(subset, file="Results/output tables/adding_up_compare_ds_subset.csv", row.names=FALSE)