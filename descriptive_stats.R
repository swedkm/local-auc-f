library(tidyverse)
library(patchwork)
library(readxl)
library(ggplot2)

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
df$dwq <- abs(df$q1 - df$q0)

# for MRM2 models MWTP ~ log(WTP/q1-q0) at midpoint (q1+q0/2)
df$lnMWTP <- df$lnWTP - df$lnquality_ch

# plot for wtp by dwq and wtp/dwq by dwq
p1 <- ggplot(df, aes(x=dwq, y=TOTAL_WTP)) +
  geom_point(color='#03396c', size=.5) + theme_bw() + 
  labs(x='dq', y='WTP (2019$)', tag='A')
p2 <- ggplot(df, aes(x=dwq, y=TOTAL_WTP/dwq)) +
  geom_point(color='#03396c', size=.5) + theme_bw() + 
  labs(x='dq', y='WTP/dq (2019$)', tag='B')
p <- p1 + p2 
ggsave('Results/output tables/wtp_dq_plots.png', width=6.5, height=3, dpi=1000)

p1 <- ggplot(df, aes(x=dwq, y=TOTAL_WTP)) +
  geom_point(color='#03396c', size=.5) + 
  geom_smooth(method='lm', formula = y~x, se=FALSE, color='black') +
  theme_bw() + 
  labs(x='dq', y='WTP (2019$)', tag='A')
p2 <- ggplot(df, aes(x=dwq, y=TOTAL_WTP/dwq)) +
  geom_point(color='#03396c', size=.5) + 
  geom_smooth(method='lm', formula = y ~ log(x), se=FALSE, color='black') +
  theme_bw() + 
  labs(x='dq', y='WTP/dq (2019$)', tag='B')
p <- p1 + p2 
ggsave('Results/output tables/wtp_dq_plots_trend.png', width=6.5, height=3, dpi=1000)

p3 <- ggplot(df, aes(x=dwq, y=lnMWTP)) +
  geom_point(color='#03396c', size=.5) + theme_bw() + 
  labs(x='dq', y='log(WTP/dq)', tag='A')
p4 <- ggplot(df, aes(x=lnquality_ch, y=lnMWTP)) +
  geom_point(color='#03396c', size=.5) + theme_bw() + 
  labs(x='log(dq)', y='log(WTP/dq)', tag='B')
p <- p3 + p4 
ggsave('Results/output tables/lnmwtp_dq_plots.png', width=6.5, height=3, dpi=1000)

# meta-data descriptive stats all
meta_stats <- as.data.frame(do.call(cbind, lapply(df[,c('q0', 'q1', 'q_mid', 'dwq', 'lnyear', 'ln_ar_agr', 'ln_ar_ratio', 'sub_frac', 
                                                        'lnincome1', 'census_south', 'census_midwest', 'census_west',
                                                        'nonusers', 'swim_use', 'gamefish',
                                                        'volunt', 'tax_only', 'user_cost', 'lump_sum', 'OneShotVal',  
                                                        'thesis', 'non_reviewed','RUM', 'IBI')], summary)))
meta_stats['count',] = nrow(df)
write.csv(meta_stats, "Results/output tables/meta_stats.csv", row.names=TRUE)

# set bt data to bensplash defaults
bt_data <- as.data.frame(matrix(nrow=1, ncol=22))
colnames(bt_data) <- c('q_mid', 'dwq', 'ln_ar_agr', 'ln_ar_ratio', 'sub_frac', 
                       'census_south', 'census_midwest', 'census_west', 'lnincome1',
                       'lnyear', 'nonusers', 'swim_use', 'gamefish',
                       'volunt', 'tax_only', 'user_cost', 'lump_sum', 'OneShotVal',  
                       'thesis', 'non_reviewed','RUM', 'IBI')
bt_data[1,] <-  c('Varies by grid cell', 'Varies by grid cell', 'Varies by grid cell', 'Varies by grid cell', 
                  'Varies by grid cell', 'Varies by grid cell', 'Varies by grid cell', 'Varies by grid cell', 
                  'Varies by grid cell and year', 3.6109, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0)
write.csv(bt_data, "Results/output tables/bt_inputs.csv")

# read in BenSPLASH inputs at comid level
ben_in <- merge(read.csv('Data/BenSPLASH/se_base_wqi_PD2.csv'), 
                read.csv('Data/BenSPLASH/se_policy_wqi_PD2.csv'), 
                by=c('COMID2', 'DATE', 'PARM'), suffixes=c('_base', '_policy'))
ben_in$dwq = ben_in$VALUE_policy - ben_in$VALUE_base
ben_in_stats <- as.data.frame(do.call(cbind, lapply(ben_in[,c('VALUE_base', 'VALUE_policy', 'dwq')], summary)))
ben_in_stats['count',] = nrow(ben_in)

# read in BenSPLASH outputs at gridded level
ben_out <- merge(read.csv('Data/BenSPLASH/se_PD2_output/Benefit Results (Grid Cell)_Base_WQI.csv'),
                 read.csv('Data/BenSPLASH/se_PD2_output/Benefit Results (Grid Cell)_Scen_WQI.csv'),
                 on=c('X', 'Y'), suffixes=c('_base', '_policy'))
ben_out$dwq <- ben_out$MRM2_2023.Scen_WQI.2030 - ben_out$MRM2_2023.Base_WQI.2030
ben_out_stats <- as.data.frame(do.call(cbind, lapply(ben_out[,c('MRM2_2023.Scen_WQI.2030', 'MRM2_2023.Base_WQI.2030', 'dwq')], summary)))
ben_out_stats['count',] = nrow(ben_out)

# output bensplash stats
write.csv(cbind(ben_in_stats, ben_out_stats), 'Results/output tables/bensplash wq stats.csv')