library(tidyverse)
library(patchwork)
library(brms)
library(readxl)
library(rstan)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre-1.8') # fixes error with xlsx not finding rJava
library(xlsx)

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

# write bayes factors
bayes_compare = as.data.frame(matrix(nrow=32, ncol=32))
colnames(bayes_compare) <- names_list
rownames(bayes_compare) <- names_list

i_count = 1
for (i in names_list){
  j_count = 1
  for (j in names_list){
    if (i_count > j_count){
      bf <- bayes_factor(get(names_list[[i_count]]), get(names_list[[j_count]]))
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
write.csv(bayes_compare, file="results/output tables/bayes_compare_all.csv")


log_bayes_compare = as.data.frame(matrix(nrow=32, ncol=32))
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
write.csv(log_bayes_compare, file="results/output tables/log10_bayes_compare_all.csv")





# collect all bayes factors here... will take a while

# might also want to summarize all here since loading...

# pull code from other scripts

# ignore rs's for time being they can go in full paper later. they would just be an appendix

