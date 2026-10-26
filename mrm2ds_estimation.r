library(tidyverse)
library(patchwork)
library(brms)
library(readxl)
library(rstan)
library(xlsx)

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
df$dwq10 <- ifelse(df$dwq > 10, df$lnquality_ch, 0)
df$dwq11 <- ifelse(df$dwq > 11, df$lnquality_ch, 0)
df$dwq12 <- ifelse(df$dwq > 12, df$lnquality_ch, 0)
df$dwq13 <- ifelse(df$dwq > 13, df$lnquality_ch, 0)
df$dwq14 <- ifelse(df$dwq > 14, df$lnquality_ch, 0)
df$dwq15 <- ifelse(df$dwq > 15, df$lnquality_ch, 0)
df$dwq16 <- ifelse(df$dwq > 16, df$lnquality_ch, 0)
df$dwq17 <- ifelse(df$dwq > 17, df$lnquality_ch, 0)
df$dwq18 <- ifelse(df$dwq > 18, df$lnquality_ch, 0)
df$dwq19 <- ifelse(df$dwq > 19, df$lnquality_ch, 0)
df$dwq20 <- ifelse(df$dwq > 20, df$lnquality_ch, 0)
df$dwq25 <- ifelse(df$dwq > 25, df$lnquality_ch, 0)
df$dwq30 <- ifelse(df$dwq > 30, df$lnquality_ch, 0)
df$dwq35 <- ifelse(df$dwq > 35, df$lnquality_ch, 0)

##################################################################################
# writing models 
mrm2ds3.sid <- (lnMWTP ~ ln_q_mid + dwq3  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
                     lnincome1 + census_south + census_midwest + census_west + 
                     nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
                     lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
mrm2ds4.sid <- (lnMWTP ~ ln_q_mid + dwq4  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
                     lnincome1 + census_south + census_midwest + census_west + 
                     nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
                     lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
mrm2ds5.sid <- (lnMWTP ~ ln_q_mid + dwq5  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
                     lnincome1 + census_south + census_midwest + census_west + 
                     nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
                     lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
mrm2ds6.sid <- (lnMWTP ~ ln_q_mid + dwq6  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
                     lnincome1 + census_south + census_midwest + census_west + 
                     nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
                     lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
mrm2ds7.sid <- (lnMWTP ~ ln_q_mid + dwq7  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
                     lnincome1 + census_south + census_midwest + census_west + 
                     nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
                     lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
mrm2ds8.sid <- (lnMWTP ~ ln_q_mid + dwq8  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
                     lnincome1 + census_south + census_midwest + census_west + 
                     nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
                     lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
mrm2ds9.sid <- (lnMWTP ~ ln_q_mid + dwq9  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
                     lnincome1 + census_south + census_midwest + census_west + 
                     nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
                     lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
mrm2ds10.sid <- (lnMWTP ~ ln_q_mid + dwq10  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
                     lnincome1 + census_south + census_midwest + census_west + 
                     nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
                     lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
mrm2ds11.sid <- (lnMWTP ~ ln_q_mid + dwq11  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
                   lnincome1 + census_south + census_midwest + census_west + 
                   nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
                   lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
mrm2ds12.sid <- (lnMWTP ~ ln_q_mid + dwq12  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
                   lnincome1 + census_south + census_midwest + census_west + 
                   nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
                   lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
mrm2ds13.sid <- (lnMWTP ~ ln_q_mid + dwq13  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
                   lnincome1 + census_south + census_midwest + census_west + 
                   nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
                   lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
mrm2ds14.sid <- (lnMWTP ~ ln_q_mid + dwq14  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
                   lnincome1 + census_south + census_midwest + census_west + 
                   nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
                   lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
mrm2ds15.sid <- (lnMWTP ~ ln_q_mid + dwq15  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
                     lnincome1 + census_south + census_midwest + census_west + 
                     nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
                     lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
mrm2ds20.sid <- (lnMWTP ~ ln_q_mid + dwq20  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
                     lnincome1 + census_south + census_midwest + census_west + 
                     nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
                     lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
mrm2ds25.sid <- (lnMWTP ~ ln_q_mid + dwq25  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
                     lnincome1 + census_south + census_midwest + census_west + 
                     nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
                     lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
mrm2ds30.sid <- (lnMWTP ~ ln_q_mid + dwq30  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
                     lnincome1 + census_south + census_midwest + census_west + 
                     nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
                     lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
mrm2ds35.sid <- (lnMWTP ~ ln_q_mid + dwq35  + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
                     lnincome1 + census_south + census_midwest + census_west + 
                     nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
                     lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))

##################################################################################
# running models 

# iterate through models and store output
model_names = c('mrm2ds3.sid', 'mrm2ds4.sid', 'mrm2ds5.sid', 'mrm2ds6.sid', 'mrm2ds7.sid', 'mrm2ds8.sid', 
                'mrm2ds9.sid', 'mrm2ds10.sid', 'mrm2ds11.sid', 'mrm2ds12.sid', 'mrm2ds13.sid', 'mrm2ds14.sid', 
                'mrm2ds15.sid', 'mrm2ds20.sid', 'mrm2ds25.sid', 'mrm2ds30.sid', 'mrm2ds35.sid')

options(warn=1)
i = 1
for (m in model_names){
  # run student
  fit.st <- brm(formula = get(m), data = df, family = 'student',
                prior   = priors.st.sid,
                control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
                save_pars = save_pars(all=TRUE),
                chains = chains, iter = iter, warmup = warmup, cores = cores, thin = thin, seed = seed)
  save(fit.st, file=paste('results/mrm output/', m, '.fit.st.rds', sep=''))
  print(summary(fit.st))
  assign(paste(m, '.fit.st', sep=''), fit.st)
  print('#################################')
  print(i*100/length(model_names))
  print('#################################')
  i = i + 1
}

##################################################################################
# write output tables
options(warn=1)
for (m in model_names){
  if (grepl('sid', m)){
    output <- rbind(summary(get(paste(m, '.fit.st', sep='')))$fixed, 
                    summary(get(paste(m, '.fit.st', sep='')))$spec_pars, 
                    summary(get(paste(m, '.fit.st', sep='')))$random$studyid)
  } else{
    output <- rbind(summary(get(paste(m, '.fit.st', sep='')))$fixed, 
                    summary(get(paste(m, '.fit.st', sep='')))$spec_pars)
  }
  write.xlsx(output, file="Results/output tables/regression_output.xlsx", sheetName=paste(m, '.fit.st', sep=''), append=TRUE)
  
}

##################################################################################
# write bayes factors
x = load("results/mrm output/mrm2.sid.fit.st.rds")
assign('mrm2.sid.fit.st', get(x))
x = load("results/mrm output/mrm2s.sid.fit.st.rds")
assign('mrm2s.sid.fit.st', get(x))
names_list <- c('mrm2', 'mrm2ds3', 'mrm2ds4', 'mrm2ds5',
                'mrm2ds6', 'mrm2ds7', 'mrm2ds8', 'mrm2ds9',
                'mrm2ds10', 'mrm2ds11', 'mrm2ds12', 'mrm2ds13', 'mrm2ds14', 
                'mrm2ds15', 
                'mrm2ds20', 'mrm2ds25',
                'mrm2ds30', 'mrm2ds35', 'mrm2s')
model_list <- list(mrm2.sid.fit.st, mrm2ds3.sid.fit.st, mrm2ds4.sid.fit.st, mrm2ds5.sid.fit.st,
                   mrm2ds6.sid.fit.st, mrm2ds7.sid.fit.st, mrm2ds8.sid.fit.st, mrm2ds9.sid.fit.st,
                   mrm2ds10.sid.fit.st, mrm2ds11.sid.fit.st, mrm2ds12.sid.fit.st, mrm2ds13.sid.fit.st, mrm2ds14.sid.fit.st,
                   mrm2ds15.sid.fit.st, 
                   mrm2ds20.sid.fit.st, mrm2ds25.sid.fit.st,
                   mrm2ds30.sid.fit.st, mrm2ds35.sid.fit.st, mrm2s.sid.fit.st)

bayes_compare = as.data.frame(matrix(nrow=length(names_list), ncol=length(names_list)))
colnames(bayes_compare) <- names_list
rownames(bayes_compare) <- names_list

i_count = 1
for (i in names_list){
  j_count = 1
  for (j in names_list){
    if (i_count > j_count){
      bf <- bayes_factor(model_list[[i_count]], model_list[[j_count]])
      bayes_compare[i,j] <- bf$bf
    } else if (i_count == j_count){
      bayes_compare[i,j] <- 'NA'
    } else{
      bayes_compare[i,j] <- ''
    }
    j_count = j_count + 1
  }
  print('#################################')
  print(i_count/length(names_list)*100)
  print('#################################')
  i_count = i_count + 1
}
write.csv(bayes_compare, file="results/output tables/bayes_compare_ds.csv")


log_bayes_compare = as.data.frame(matrix(nrow=length(names_list), ncol=length(names_list)))
colnames(log_bayes_compare) <- names_list
rownames(log_bayes_compare) <- names_list
i_count = 1
for (i in names_list){
  j_count = 1
  for (j in names_list){
    if (i_count > j_count){
      lbf <- log(as.numeric(bayes_compare[i,j]), base=10)
      log_bayes_compare[i,j] <- lbf
    } else if (i_count == j_count){
      log_bayes_compare[i,j] <- 'NA'
    } else{
      log_bayes_compare[i,j] <- ''
    }
    j_count = j_count + 1
  }
  i_count = i_count + 1
}
write.csv(log_bayes_compare, file="results/output tables/log10_bayes_compare_ds.csv")
