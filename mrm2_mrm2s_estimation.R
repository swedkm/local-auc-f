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
priors.g = c(set_prior("normal(0, 10)", class = "b"),
             set_prior("normal(0, 10)", class = "Intercept"),
             set_prior("cauchy(0, 10)", class = "sigma"))
priors.st = c(set_prior("normal(0, 10)", class = "b"),
              set_prior("normal(0, 10)", class = "Intercept"),
              set_prior("cauchy(0, 10)", class = "sigma"),
              set_prior("gamma(2, .1)", class = "nu"))
priors.g.sid = c(set_prior("normal(0, 10)", class = "b"),
                 set_prior("normal(0, 10)", class = "Intercept"),
                 set_prior("cauchy(0, 10)", class = "sigma"),
                 set_prior("cauchy(0, 10)", class = "sd"))
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

##################################################################################
# writing models 
mrm2 <- (lnMWTP ~ ln_q_mid + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
           lnincome1 + census_south + census_midwest + census_west + 
           nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
           lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI)
mrm2.sid <- (lnMWTP ~ ln_q_mid + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
           lnincome1 + census_south + census_midwest + census_west + 
           nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
           lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI + (1|studyid))
mrm2s.sid <- (lnMWTP ~ ln_q_mid + lnquality_ch + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
           lnincome1 + census_south + census_midwest + census_west + 
           nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
           lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI  + (1|studyid))
mrm2s <- (lnMWTP ~ ln_q_mid + lnquality_ch + lnyear + ln_ar_agr + ln_ar_ratio + sub_frac +  
            lnincome1 + census_south + census_midwest + census_west + 
            nonusers + swim_use + gamefish + volunt + tax_only + user_cost + 
            lump_sum + OneShotVal + thesis + non_reviewed + RUM + IBI)


##################################################################################
# running models 

# iterate through models and store output
model_names = c('mrm2', 'mrm2.sid', 'mrm2s', 'mrm2s.sid')

options(warn=1)
i = 1
for (m in model_names){
  # choose priors based on m for gaussian
  if (grepl('sid', m)){
    priors = priors.g.sid
  } else{
    priors = priors.g
  }
  
  # run gaussian
  fit.g <- brm(formula = get(m), data = df, family = 'gaussian',
               prior   = priors,
               control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
               save_pars = save_pars(all=TRUE),
               chains = chains, iter = iter, warmup = warmup, cores = cores, thin = thin, seed = seed)
  save(fit.g, file=paste('results/mrm output/', m, '.fit.g.rds', sep=''))
  print(summary(fit.g))
  assign(paste(m, '.fit.g', sep=''), fit.g)
  
  # choose priors based on m for student
  if (grepl('sid', m)){
    priors = priors.st.sid
  } else{
    priors = priors.st
  }

  # run student
  fit.st <- brm(formula = get(m), data = df, family = 'student',
                prior   = priors,
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
write.xlsx('', file="Results/output tables/regression_output.xlsx", sheetName='blank')
options(warn=1)
for (m in model_names){
  if (grepl('sid', m)){
    output <- rbind(summary(get(paste(m, '.fit.g', sep='')))$fixed, 
                    summary(get(paste(m, '.fit.g', sep='')))$spec_pars, 
                    summary(get(paste(m, '.fit.g', sep='')))$random$studyid)
  } else{
    output <- rbind(summary(get(paste(m, '.fit.g', sep='')))$fixed, 
                    summary(get(paste(m, '.fit.g', sep='')))$spec_pars)
  }
  write.xlsx(output, file="Results/output tables/regression_output.xlsx", sheetName=paste(m, '.fit.g', sep=''), append=TRUE)
  
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
bayes_compare = as.data.frame(matrix(nrow=8, ncol=8))
names_list <- c('mrm2.g', 'mrm2.st', 'mrm2.sid.g', 'mrm2.sid.st', 
                'mrm2s.g', 'mrm2s.st', 'mrm2s.sid.g', 'mrm2s.sid.st')
model_list <- list(mrm2.fit.g, mrm2.fit.st, mrm2.sid.fit.g, mrm2.sid.fit.st, 
                mrm2s.fit.g, mrm2s.fit.st, mrm2s.sid.fit.g, mrm2s.sid.fit.st)
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
write.csv(bayes_compare, file="results/output tables/bayes_compare.csv")


log_bayes_compare = as.data.frame(matrix(nrow=8, ncol=8))
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
write.csv(log_bayes_compare, file="results/output tables/log10_bayes_compare.csv")
