library(tidyverse)
library(patchwork)
library(brms)
library(readxl)
library(rstan)
library(RSQLite)
library(dplyr)
library(bayestestR)
library(arrow)

# set working directory
setwd("C:/Users/kswedber/OneDrive - Environmental Protection Agency (EPA)/Desktop/Adding Up Paper")

# load model output
x = load("Results/mrm output/no midpoint/mrm2.sid.fit.st.rds")
assign('mrm2.sid.fit.st', get(x))
x = load("Results/mrm output/no midpoint/mrm2ds6.sid.fit.st.rds")
assign('mrm2ds6.sid.fit.st', get(x))
x = load("Results/mrm output/no midpoint/mrm2s.sid.fit.st.rds")
assign('mrm2s.sid.fit.st', get(x))

# load BenSPLASH default inputs
bt_data <- read.csv("Results/output tables/bt_inputs.csv")

# load BenSPLASH spatial inputs for pd1 and pd2
ben_out <- merge(read.csv('Data/BenSPLASH/se_PD1_output/Benefit Results (Grid Cell).csv'),
                 read.csv('Data/BenSPLASH/se_PD2_output/Benefit Results (Grid Cell).csv'),
                 by=c('X','Y'), suffixes=c('pd1', 'pd2'))

# extract relevant cells
ben_sp <- ben_out[,c('X', 'Y', "MRM2_2023.v_Q..2025.", "MRM2_2023.v_Q..2030.", "MRM2_2023.Yearly_WQI_Deltapd1", "MRM2_2023.Yearly_WQI_Deltapd2",
                     "MRM2_2023.v_sub_proportion..2025.", "MRM2_2023.v_ar_agrpd1", "MRM2_2023.v_ar_ratiopd1", "MRM2_2023.regionpd1",
                     "MRM2_2023.Income_BenefitYear.....Benefit.Year..2025.", "MRM2_2023.State_FIPSpd1")]

# add population 2025
hh_year <- read.csv('Data/BenSPLASH/se_PD1_output/Benefit Results (Grid Cell)_Adjust_NumHH.csv')
ben_sp <- merge(ben_sp, hh_year[,c('X', 'Y', "MRM2_2023.Adjust_NumHH.2025")], by=c('X','Y'))

# rename columns
colnames(ben_sp) <- c('X', 'Y', 'q_mid_pd1', 'q_mid_pd2', 'dwq_pd1', 'dwq_pd2', 'sub_frac', 'ar_agr', 'ar_ratio', 'region', 'income_2025', 'stfips', 'pop_2025')

# take relevant natural logs
ben_sp$ln_ar_agr <- log(ben_sp$ar_agr)
ben_sp$ln_ar_ratio <- log(ben_sp$ar_ratio)

# extract regions
ben_sp$census_south <- ifelse(ben_sp$region == 3, 1, 0)
ben_sp$census_midwest <- ifelse(ben_sp$region == 2, 1, 0)
ben_sp$census_west <- ifelse(ben_sp$region == 4, 1, 0)

# access bensplash databases to income multipliers for each year
con <- dbConnect(SQLite(), 'C:/Users/kswedber/OneDrive - Environmental Protection Agency (EPA)/Documents/BenSPLASH Files/Database/bensplash.sqlite')
as.data.frame(dbListTables(con))
pop_income_growth <- dbReadTable(con, 'pop_income_growth')
dbDisconnect(con)

# calculate yearly incomes and population
income <- filter(pop_income_growth, year >= 2025 & year < 2050)
ben_sp <- merge(income, ben_sp)
pop_income_2025 <- filter(pop_income_growth, year == 2025)
colnames(pop_income_2025) <- c('stfips', 'year', 'pop_rel2010_25', 'income_rel2010_25')
ben_sp <- merge(ben_sp, pop_income_2025[,c('stfips', 'pop_rel2010_25', 'income_rel2010_25')])
ben_sp$income <- (ben_sp$income_2025 / ben_sp$income_rel2010_25) * ben_sp$income_rel2010
ben_sp$lnincome1 <- log(ben_sp$income)
ben_sp$num_HH <- (ben_sp$pop_2025 / ben_sp$pop_rel2010_25) * ben_sp$pop_rel2010

# assign wq based on year
ben_sp$ln_q_mid <- ifelse(ben_sp$year > 29, log(ben_sp$q_mid_pd2), log(ben_sp$q_mid_pd1))
ben_sp$dwq <- ifelse(ben_sp$year > 29, ben_sp$dwq_pd2, ben_sp$dwq_pd1)
ben_sp$q0 = exp(ben_sp$ln_q_mid) - (ben_sp$dwq/2)
ben_sp$ln_q0 <- log(ben_sp$q0)
ben_sp$dwq6 <- 0

# set lnquality_ch for mrm2-s to min from metadata
ben_sp$lnquality_ch <- log(2.38)

# join with constant inputs
bt_data <- bt_data[,c('lnyear', 'nonusers', 'swim_use', 'gamefish',
                      'volunt', 'tax_only', 'user_cost', 'lump_sum', 'OneShotVal',  
                      'thesis', 'non_reviewed','RUM', 'IBI')]
bt_data <- bt_data[rep(seq_len(nrow(bt_data)), nrow(ben_sp)), ]
ben_sp <- cbind(ben_sp, bt_data)

# remove unneccessary data
remove(ben_out, bt_data, con, income, pop_income_growth, hh_year)

# define function to make bt predictions
bt_predict <- function(df, model, ci=.95, loop=FALSE, loop_n=NULL, ndraws=NULL){
  # """
  # This function predicts per household marginal willingess to pay (mwtp) for prespecified data and model. 
  
  # df is a dataframe. names must include the variable names in specified model.
  # model is a brmsfit model.
  # ci is a float from 0-1 indicating the credible interval used to summarize the results. default is .95.
  # loop indicates whether to split df into subsections when memory is insufficient. default is FALSE.
  # loop_n is the number of loop to split df over. default is NULL.
  # ndraws is the number of draws. default is NULL, which uses all draws from estimation.
  
  # mwtp.summary is the return variable which summarizes mwtp with the median, 90th, and 95th credible intervals
  # """
  tryCatch(
    {
    if (loop==FALSE){
      # predict mwtp (eventually transform option will be deprecated and will need 2 steps tp predict ln(mwtp) then take exp)
      mwtp <- as.data.frame(posterior_predict(model, df, re_formula=NA, seed=127, transform='exp', ndraws=ndraws))
      
      # summarize predictions and concatenate
      mwtp.summary <- ci(mwtp, method='HDI', ci=ci)
      mwtp.summary <- cbind(mwtp.summary[,c('CI_low', 'CI_high')], as.data.frame(apply(mwtp, 2, median)))
      
    } else{
      # initialize summary df
      mwtp.summary <- as.data.frame(matrix(nrow=0, ncol=3))
      
      # initialize increment length and left and right indexes
      increment <- floor(nrow(df)/loop_n)
      l <- 1
      r <- increment
      
      for (i in 1:loop_n+1){
        # predict mwtp (eventually transform option will be deprecated and will need 2 steps tp predict ln(mwtp) then take exp)
        mwtp <- as.data.frame(posterior_predict(model, df[l:r,], re_formula=NA, seed=127, transform='exp', ndraws=ndraws))
        
        # summarize predictions and concactenate
        i.summary <- ci(mwtp, method='HDI', ci=ci)
        i.summary <- cbind(i.summary[,c('CI_low', 'CI_high')], as.data.frame(apply(mwtp, 2, median)))
        mwtp.summary <- rbind(mwtp.summary, i.summary)
        remove(mwtp)
        
        # increment left and right indexes
        l <- r + 1
        if (i < loop_n){
          r <- l + increment - 1
        } else{
          r <- nrow(df)
        }
        
        # update progress
        message(round(i/loop_n*100))
      }
    }
    
    # set column names
    colnames(mwtp.summary) <- c(paste(as.character(model), '.mwtp.ci_', as.character(100*ci), '_low', sep=''), 
                                paste(as.character(model), '.mwtp.ci_', as.character(100*ci), '_high', sep=''), 
                                paste(as.character(model), '.mwtp.median', sep=''))
    message(c(paste(as.character(model), '.mwtp.ci_', as.character(100*ci), '_low', sep=''), 
              paste(as.character(model), '.mwtp.ci_', as.character(100*ci), '_high', sep=''), 
              paste(as.character(model), '.mwtp.median', sep='')))
    return(mwtp.summary)
    
    },
    error=function(e) {
      message('error')
      print(e)
      return(mwtp.summary)
    }
  )
}

# define function inputs
ci = .90
loop = TRUE
loop_n = 100
ndraws = NULL

# get mwtp predictions for mrm2ds6 and mrm2 with intermediate outputs
time <- proc.time()
mrm2ds6.mwtp.summary <- bt_predict(ben_sp, mrm2ds6.sid.fit.st, ci, loop, loop_n, ndraws)
print(proc.time() - time)
time <- proc.time()
mrm2.mwtp.summary <- bt_predict(ben_sp, mrm2.sid.fit.st, ci, loop, loop_n, ndraws)
print(proc.time() - time)
time <- proc.time()
mrm2s.mwtp.summary <- bt_predict(ben_sp, mrm2s.sid.fit.st, ci, loop, loop_n, ndraws)
print(proc.time() - time)

# change column names
colnames(mrm2ds6.mwtp.summary) <- c(paste('mrm2ds6.mwtp.ci_', as.character(100*ci), '_low', sep=''), 
                                    paste('mrm2ds6.mwtp.ci_', as.character(100*ci), '_high', sep=''), 
                                    paste('mrm2ds6.mwtp.median', sep=''))
colnames(mrm2.mwtp.summary) <- c(paste('mrm2.mwtp.ci_', as.character(100*ci), '_low', sep=''), 
                                 paste('mrm2.mwtp.ci_', as.character(100*ci), '_high', sep=''), 
                                 paste('mrm2.mwtp.median', sep=''))
colnames(mrm2s.mwtp.summary) <- c(paste('mrm2s.mwtp.ci_', as.character(100*ci), '_low', sep=''), 
                                  paste('mrm2s.mwtp.ci_', as.character(100*ci), '_high', sep=''), 
                                  paste('mrm2s.mwtp.median', sep=''))  
# join mwtp to ben_sp
ben_sp <- cbind(ben_sp, mrm2ds6.mwtp.summary)
ben_sp <- cbind(ben_sp, mrm2.mwtp.summary)
ben_sp <- cbind(ben_sp, mrm2s.mwtp.summary)

# multiple summary variables by delta for wtp
ben_sp$mrm2ds6.hhwtp.ci_90_low <- ben_sp$mrm2ds6.mwtp.ci_90_low * ben_sp$dwq
ben_sp$mrm2ds6.hhwtp.ci_90_high <- ben_sp$mrm2ds6.mwtp.ci_90_high * ben_sp$dwq
ben_sp$mrm2ds6.hhwtp.median <- ben_sp$mrm2ds6.mwtp.median * ben_sp$dwq
ben_sp$mrm2.hhwtp.ci_90_low <- ben_sp$mrm2.mwtp.ci_90_low * ben_sp$dwq
ben_sp$mrm2.hhwtp.ci_90_high <- ben_sp$mrm2.mwtp.ci_90_high * ben_sp$dwq
ben_sp$mrm2.hhwtp.median <- ben_sp$mrm2.mwtp.median * ben_sp$dwq
ben_sp$mrm2s.hhwtp.ci_90_low <- ben_sp$mrm2s.mwtp.ci_90_low * ben_sp$dwq
ben_sp$mrm2s.hhwtp.ci_90_high <- ben_sp$mrm2s.mwtp.ci_90_high * ben_sp$dwq
ben_sp$mrm2s.hhwtp.median <- ben_sp$mrm2s.mwtp.median * ben_sp$dwq

# multiply hhwtp by number of household each year
ben_sp$mrm2ds6.wtp.ci_90_low <- ben_sp$mrm2ds6.hhwtp.ci_90_low * ben_sp$num_HH
ben_sp$mrm2ds6.wtp.ci_90_high <- ben_sp$mrm2ds6.hhwtp.ci_90_high * ben_sp$num_HH
ben_sp$mrm2ds6.wtp.median <- ben_sp$mrm2ds6.hhwtp.median * ben_sp$num_HH
ben_sp$mrm2.wtp.ci_90_low <- ben_sp$mrm2.hhwtp.ci_90_low * ben_sp$num_HH
ben_sp$mrm2.wtp.ci_90_high <- ben_sp$mrm2.hhwtp.ci_90_high * ben_sp$num_HH
ben_sp$mrm2.wtp.median <- ben_sp$mrm2.hhwtp.median * ben_sp$num_HH
ben_sp$mrm2s.wtp.ci_90_low <- ben_sp$mrm2s.hhwtp.ci_90_low * ben_sp$num_HH
ben_sp$mrm2s.wtp.ci_90_high <- ben_sp$mrm2s.hhwtp.ci_90_high * ben_sp$num_HH
ben_sp$mrm2s.wtp.median <- ben_sp$mrm2s.hhwtp.median * ben_sp$num_HH

# discount by year with 3% discount rate starting in 2025
d_rate <- .03
d_year <- 2025
EndYear <- 2049
ben_sp$mrm2ds6.dwtp.ci_90_low <- ben_sp$mrm2ds6.wtp.ci_90_low / (1 + d_rate)^(ben_sp$year - d_year)
ben_sp$mrm2ds6.dwtp.ci_90_high <- ben_sp$mrm2ds6.wtp.ci_90_high / (1 + d_rate)^(ben_sp$year - d_year)
ben_sp$mrm2ds6.dwtp.median <- ben_sp$mrm2ds6.wtp.median / (1 + d_rate)^(ben_sp$year - d_year)
ben_sp$mrm2.dwtp.ci_90_low <- ben_sp$mrm2.wtp.ci_90_low / (1 + d_rate)^(ben_sp$year - d_year)
ben_sp$mrm2.dwtp.ci_90_high <- ben_sp$mrm2.wtp.ci_90_high / (1 + d_rate)^(ben_sp$year - d_year)
ben_sp$mrm2.dwtp.median <- ben_sp$mrm2.wtp.median / (1 + d_rate)^(ben_sp$year - d_year)
ben_sp$mrm2s.dwtp.ci_90_low <- ben_sp$mrm2s.wtp.ci_90_low / (1 + d_rate)^(ben_sp$year - d_year)
ben_sp$mrm2s.dwtp.ci_90_high <- ben_sp$mrm2s.wtp.ci_90_high / (1 + d_rate)^(ben_sp$year - d_year)
ben_sp$mrm2s.dwtp.median <- ben_sp$mrm2s.wtp.median / (1 + d_rate)^(ben_sp$year - d_year)

# output
write_parquet(as.data.frame(ben_sp), 'Results/benefits transfer/no midpoint/bt_grid_year.pqt')

# next steps are to calculate present value and adjusted annualized value for each cell
discount_rate_adjust <- (d_rate * (1 + d_rate)^(EndYear - d_year)) / ((1 + d_rate)^(EndYear - d_year + 1) - 1)
ben_sp_g = ben_sp %>% group_by(X,Y) %>%
  summarise(mrm2ds6.pvwtp.ci_90_low = sum(mrm2ds6.dwtp.ci_90_low),
            mrm2ds6.pvwtp.ci_90_high = sum(mrm2ds6.dwtp.ci_90_high),
            mrm2ds6.pvwtp.median = sum(mrm2ds6.dwtp.median),
            mrm2.pvwtp.ci_90_low = sum(mrm2.dwtp.ci_90_low),
            mrm2.pvwtp.ci_90_high = sum(mrm2.dwtp.ci_90_high),
            mrm2.pvwtp.median = sum(mrm2.dwtp.median),
            mrm2s.pvwtp.ci_90_low = sum(mrm2s.dwtp.ci_90_low),
            mrm2s.pvwtp.ci_90_high = sum(mrm2s.dwtp.ci_90_high),
            mrm2s.pvwtp.median = sum(mrm2s.dwtp.median),
            mrm2ds6.awtp.ci_90_low = sum(mrm2ds6.dwtp.ci_90_low) * discount_rate_adjust,
            mrm2ds6.awtp.ci_90_high = sum(mrm2ds6.dwtp.ci_90_high) * discount_rate_adjust,
            mrm2ds6.awtp.median = sum(mrm2ds6.dwtp.median) * discount_rate_adjust,
            mrm2.awtp.ci_90_low = sum(mrm2.dwtp.ci_90_low) * discount_rate_adjust,
            mrm2.awtp.ci_90_high = sum(mrm2.dwtp.ci_90_high) * discount_rate_adjust,
            mrm2.awtp.median = sum(mrm2.dwtp.median) * discount_rate_adjust,
            mrm2s.awtp.ci_90_low = sum(mrm2s.dwtp.ci_90_low) * discount_rate_adjust,
            mrm2s.awtp.ci_90_high = sum(mrm2s.dwtp.ci_90_high) * discount_rate_adjust,
            mrm2s.awtp.median = sum(mrm2s.dwtp.median) * discount_rate_adjust,
            .groups='drop')

# output
write_parquet(as.data.frame(ben_sp_g), 'Results/benefits transfer/no midpoint/bt_grid.pqt')

# summarize nationally and output
national = as.data.frame(t(colSums(ben_sp_g)))
write_csv(national, 'Results/benefits transfer/no midpoint/national_summary.csv')

# extract estimates for 2030
ben_sp_30 <- filter(ben_sp, year==2030)

# write summary table for models (will compare to Bensplash outputs in subsequent script)
summary_table <- as.data.frame(matrix(nrow=4, ncol=10))
colnames(summary_table) <- c('wtp_type', 'mrm2ds6.median', 'mrm2ds6.ci_90_low',  'mrm2ds6.ci_90_high', 
                             'mrm2.median', 'mrm2.ci_90_low',  'mrm2.ci_90_high', 'mrm2s.median', 'mrm2s.ci_90_low',  'mrm2s.ci_90_high')
summary_table[1,] <- c('grid_year mwtp 2025', median(ben_sp_30$mrm2ds6.mwtp.median), median(ben_sp_30$mrm2ds6.mwtp.ci_90_low), median(ben_sp_30$mrm2ds6.mwtp.ci_90_high),
                       median(ben_sp_30$mrm2.mwtp.median), median(ben_sp_30$mrm2.mwtp.ci_90_low), median(ben_sp_30$mrm2.mwtp.ci_90_high),
                       median(ben_sp_30$mrm2s.mwtp.median), median(ben_sp_30$mrm2s.mwtp.ci_90_low), median(ben_sp_30$mrm2s.mwtp.ci_90_high))
summary_table[2,] <- c('grid_year hhwtp 2025', median(ben_sp_30$mrm2ds6.hhwtp.median), median(ben_sp_30$mrm2ds6.hhwtp.ci_90_low), median(ben_sp_30$mrm2ds6.hhwtp.ci_90_high),
                       median(ben_sp_30$mrm2.hhwtp.median), median(ben_sp_30$mrm2.hhwtp.ci_90_low), median(ben_sp_30$mrm2.hhwtp.ci_90_high),
                       median(ben_sp_30$mrm2s.hhwtp.median), median(ben_sp_30$mrm2s.hhwtp.ci_90_low), median(ben_sp_30$mrm2s.hhwtp.ci_90_high))
summary_table[3,] <- c('national pvwtp', median(national$mrm2ds6.pvwtp.median), median(national$mrm2ds6.pvwtp.ci_90_low), median(national$mrm2ds6.pvwtp.ci_90_high),
                       median(national$mrm2.pvwtp.median), median(national$mrm2.pvwtp.ci_90_low), median(national$mrm2.pvwtp.ci_90_high),
                       median(national$mrm2s.pvwtp.median), median(national$mrm2s.pvwtp.ci_90_low), median(national$mrm2s.pvwtp.ci_90_high))
summary_table[4,] <- c('national awtp', median(national$mrm2ds6.awtp.median), median(national$mrm2ds6.awtp.ci_90_low), median(national$mrm2ds6.awtp.ci_90_high),
                       median(national$mrm2.awtp.median), median(national$mrm2.awtp.ci_90_low), median(national$mrm2.awtp.ci_90_high),
                       median(national$mrm2s.awtp.median), median(national$mrm2s.awtp.ci_90_low), median(national$mrm2s.awtp.ci_90_high))

# convert from 2019 to 2021 $ ysubg GDPDEF from FRED 
q4_2019 <- 104.541
q4_2021 <- 112.848
gdp_mult <- q4_2021/q4_2019
summary_table$mrm2ds6.median <- as.numeric(summary_table$mrm2ds6.median) * gdp_mult
summary_table$mrm2ds6.ci_90_low <- as.numeric(summary_table$mrm2ds6.ci_90_low) * gdp_mult
summary_table$mrm2ds6.ci_90_high <- as.numeric(summary_table$mrm2ds6.ci_90_high) * gdp_mult
summary_table$mrm2.median <- as.numeric(summary_table$mrm2.median) * gdp_mult
summary_table$mrm2.ci_90_low <- as.numeric(summary_table$mrm2.ci_90_low) * gdp_mult
summary_table$mrm2.ci_90_high <- as.numeric(summary_table$mrm2.ci_90_high) * gdp_mult
summary_table$mrm2s.median <- as.numeric(summary_table$mrm2s.median) * gdp_mult
summary_table$mrm2s.ci_90_low <- as.numeric(summary_table$mrm2s.ci_90_low) * gdp_mult
summary_table$mrm2s.ci_90_high <- as.numeric(summary_table$mrm2s.ci_90_high) * gdp_mult

# read in bensplash inputs for summary table
ben_out <- merge(read.csv('Data/BenSPLASH/se_PD1_output/Benefit Results (Grid Cell).csv'),
                 read.csv('Data/BenSPLASH/se_PD2_output/Benefit Results (Grid Cell).csv'),
                 by=c('X','Y'), suffixes=c('pd1', 'pd2'))
ben_out_nat1 <- read.csv('Data/BenSPLASH/se_PD1_output/Benefit Results (Nation).csv')
ben_out_nat2 <- read.csv('Data/BenSPLASH/se_PD2_output/Benefit Results (Nation).csv')

# calculate benefit year hh wtp for median and percentiles
ben_out$mrm2.hhwtp.median <- ben_out$MRM2_2023.MWTP_medianpd2 * ben_out$MRM2_2023.Yearly_WQI_Deltapd2
ben_out$mrm2.hhwtp.low <- ben_out$MRM2_2023.MWTP_Percentile1pd2 * ben_out$MRM2_2023.Yearly_WQI_Deltapd2
ben_out$mrm2.hhwtp.high <- ben_out$MRM2_2023.MWTP_Percentile2pd2 * ben_out$MRM2_2023.Yearly_WQI_Deltapd2
ben_out$mrm2s_low.hhwtp.median <- ben_out$MRM2.S_2023_Low.MWTP_medianpd2 * ben_out$MRM2_2023.Yearly_WQI_Deltapd2
ben_out$mrm2s_high.hhwtp.median <- ben_out$MRM2.S_2023_High.MWTP_medianpd2 * ben_out$MRM2_2023.Yearly_WQI_Deltapd2

# summarize bensplash inputs
ben_table <- as.data.frame(matrix(nrow=4, ncol=5))
colnames(ben_table) <- c('mrm2.median', 'mrm2.pct1', 'mrm2.pct2', 'mrm2s.low', 'mrm2s.high')
ben_table[1,] <- c(median(ben_out$MRM2_2023.MWTP_medianpd2),  median(ben_out$MRM2_2023.MWTP_Percentile1pd2),  median(ben_out$MRM2_2023.MWTP_Percentile2pd2),
                   median(ben_out$MRM2.S_2023_Low.MWTP_medianpd2), median(ben_out$MRM2.S_2023_High.MWTP_medianpd2))
ben_table[2,] <- c(median(ben_out$mrm2.hhwtp.median),  median(ben_out$mrm2.hhwtp.low),  median(ben_out$mrm2.hhwtp.high),
                   median(ben_out$mrm2s_low.hhwtp.median), median(ben_out$mrm2s_high.hhwtp.median))
ben_table[3,] <- c(ben_out_nat1$MRM2_2023.PVWTP1_median..Sum.HH. + ben_out_nat2$MRM2_2023.PVWTP1_median..Sum.HH.,
                   ben_out_nat1$MRM2_2023.PVWTP1_Percentile1..Sum.HH. + ben_out_nat2$MRM2_2023.PVWTP1_Percentile1..Sum.HH,
                   ben_out_nat1$MRM2_2023.PVWTP1_Percentile2..Sum.HH. + ben_out_nat2$MRM2_2023.PVWTP1_Percentile2..Sum.HH,
                   ben_out_nat1$MRM2.S_2023_Low.PVWTP1_median..Sum.HH. + ben_out_nat2$MRM2.S_2023_Low.PVWTP1_median..Sum.HH.,
                   ben_out_nat1$MRM2.S_2023_High.PVWTP1_median..Sum.HH. + ben_out_nat2$MRM2.S_2023_High.PVWTP1_median..Sum.HH.)
ben_table[4,] <- ben_table[3,] * discount_rate_adjust

# combine and output
summary_table <- cbind(summary_table, ben_table)
write_csv(summary_table, 'Results/benefits transfer/no midpoint/bt_summary.csv')




