# Set seed, load datasets and libraries
set.seed(594709947L)
library(extraDistr)
library(pomp)
library(iterators)
library(doFuture)
library(tidyverse)
load("~/vaxMada-data/objects/data_obj/amba/hh_data.RData")
load("~/vaxMada-data/objects/data_obj/amba/sums.RData")
load("~/vaxMada-data/objects/data_obj/hh_data_vax.RData")
load("~/vaxMada-data/objects/data_obj/vax_nchicken.RData")

# Set constant number of sampled households
uniq_dates <- unique(hh_data$date)
n_hh_surv <- c()
for (uniq_date in uniq_dates) {
  n_hh_surv <- c(n_hh_surv, length(which(hh_data$date == uniq_date)))
}
if (any(uniq_dates != sums$date)) {
  stop('Error between two variables.')
}
max_n_hh_surv <- max(n_hh_surv)
for (i in 1:nrow(sums)) {
  if (n_hh_surv[i] < max_n_hh_surv) {
    sums$deaths[i] <- round(sums$deaths[i] * (max_n_hh_surv / n_hh_surv[i]))
    sums$denoms[i] <- round(sums$denoms[i] * (max_n_hh_surv / n_hh_surv[i]))
  }
}

# Imputation for missing monthly count
for (i in 2:nrow(sums)) {
  if (as.numeric((format(sums$date[i],"%m"))) == 1) {
    if (as.numeric(format(sums$date[i-1],"%m")) != 12) {
      print(i)
    }
  } else {
    if (as.numeric(format(sums$date[i],"%m")) != as.numeric((format(sums$date[i-1],"%m"))) + 1) {
      print(i)
    }
  }
}
new_row <- data.frame(matrix(c(rep(NA, 10)), ncol=10, nrow=1))
new_row$date <- as.Date('2014-01-01')
colnames(new_row) <- colnames(sums)
new_row$deaths <- round((12 + 11) / 2)
new_row$denoms <- round((270 + 264) /2)
sums <- rbind(sums[1:which(sums$date == as.Date('2013-12-01')),],
              new_row,
              sums[(which(sums$date == as.Date('2013-12-01')) + 1):nrow(sums),])

# Interpolate to generation interval (6 days)
sums$dt <- c(sums$deaths[2:nrow(sums)], NA) # Define each element 'in timestep' as synonymous with 'end of timestep'
sums$nt <- sums$denoms
sums$cumsum_deaths <- cumsum(sums$deaths)
sums$cumsum_deaths[1] <- round(sums$cumsum_deaths[1] / 5) # Estimate number from previous generation interval, not previous month
new_rows_toadd <- list()
for (row_dex in 1:(nrow(sums) - 1)) {
  new_rows <- data.frame(matrix(NA, ncol=ncol(sums), nrow=4))
  colnames(new_rows) <- colnames(sums)
  new_rows$date <- as.Date(new_rows$date, origin='1970-01-01')
  new_rows$date[1] <- sums$date[row_dex] + 6
  new_rows$date[2] <- sums$date[row_dex] + 12
  new_rows$date[3] <- sums$date[row_dex] + 18
  new_rows$date[4] <- sums$date[row_dex] + 24
  new_rows_toadd[[row_dex]] <- new_rows
}
sums_inter <- sums
for (row_dex in (nrow(sums_inter) - 1):1) {
  sums_inter <- rbind(sums_inter[1:row_dex,], new_rows_toadd[[row_dex]], sums_inter[(row_dex + 1):nrow(sums_inter),])
}
sums_inter <- sums_inter[,-c(3:10)] # Remove unnecessary chicken-type data
sums$days_since <- as.numeric(sums$date - sums$date[1])
sums_inter$days_since <- as.numeric(sums_inter$date - sums_inter$date[1])
sums_inter$predicted_nt <- approx(sums_inter$days_since, sums_inter$denoms, xout=sums_inter$days_since)$y
sums_inter$predicted_nt_round <- round(sums_inter$predicted_nt)
colnames(sums_inter)[which(colnames(sums_inter) == 'predicted_nt_round')] <- 'nt_inter'
sums_inter$predicted_nt <- NULL
smooth_out <- splinefun(x=sums$days_since, y=sums$cumsum_deaths, method="hyman")
predicted_deaths <- smooth_out(sums_inter$days_since)
predicted_deaths <- round(predicted_deaths)
sums_inter$predicted_deaths <- predicted_deaths
any(sums_inter$cumsum_deaths[which(!is.na(sums_inter$cumsum_deaths))] != sums_inter$predicted_deaths[which(!is.na(sums_inter$cumsum_deaths))])
diff_dt <- predicted_deaths[2:length(predicted_deaths)] - predicted_deaths[1:(length(predicted_deaths) - 1)]
sums_inter$dt_inter <- c(predicted_deaths[1], diff_dt)
colnames(sums_inter)[which(colnames(sums_inter) == 'dt')] <- 'dt_sums'
colnames(sums_inter)[which(colnames(sums_inter) == 'nt')] <- 'nt_sums'

# Create 'data' variable for pomp that takes needed columns from sums_inter
data <- sums_inter[,which(colnames(sums_inter) == 'date' |
                            colnames(sums_inter) == 'days_since' |
                            colnames(sums_inter) == 'dt_inter' |
                            colnames(sums_inter) == 'denoms')]
data <- data[,c('date', 'days_since', 'dt_inter', 'denoms')]
data$predicted_nt <- round(approx(data$days_since, data$denoms, xout=data$days_since)$y)
data$nt_inter <- data$predicted_nt
data$predicted_nt <- NULL
data$gen <- as.numeric(1:nrow(data))
data$reports <- data$dt_inter
data |>
  ggplot(aes(x=gen,y=nt_inter))+
  geom_line()#+
  #geom_point(aes(x=gen,y=dt_inter), col='red', size=0.5)

# Output data variable
save(data, file="~/vaxMada-data/objects/data_obj/data_vill2.RData")

