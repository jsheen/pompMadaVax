# Set seed, load datasets and libraries
set.seed(594709947L)
library(extraDistr)
library(pomp)
library(iterators)
library(doFuture)
library(tidyverse)
load("~/vaxMada-data/objects/data_obj/sums.RData")
load("~/vaxMada-data/objects/data_obj/hh_data.RData")
load("~/vaxMada-data/objects/data_obj/hh_data_vax.RData")
load('~/vaxMada-data/objects/data_obj/vax_nchicken.RData')
load("~/vaxMada-data/objects/data_obj/vax_inter.RData")
load("~/vaxMada-data/objects/data_obj/vax_nvax.RData")
load("~/vaxMada-data/objects/data_obj/hh_data_follow.RData")

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

# Subset to 2013 and after
sums <- sums[which(sums$date >= as.Date('2013-01-01')),]

# Data imputation for missing monthly count
new_row <- data.frame(matrix(c(rep(NA, 14)), ncol=14, nrow=1))
new_row$date <- as.Date('2014-01-01')
colnames(new_row) <- colnames(sums)
new_row$deaths <- round((72 + 19) / 2)
new_row$denoms <- round((250 + 293) /2)
sums <- rbind(sums[1:which(sums$date == as.Date('2013-12-01')),],
              new_row,
              sums[(which(sums$date == as.Date('2013-12-01')) + 1):nrow(sums),])

# Scale to population level
median_nchicken_post <- round(median(vax_nchicken))
median_nchicken_pre <- round(median(sums$denoms))
for (i in 1:nrow(sums)) {
  if (sums$date[i] == as.Date('2016-03-01')) {
    sums$deaths[i] <- round(sums$deaths[i] * (1144 / median_nchicken_pre))
    sums$denoms[i] <- 1144
  } else if (sums$date[i] == as.Date('2016-04-01')) {
    sums$deaths[i] <- round(sums$deaths[i] * (((1144 + 568) / 2) / median_nchicken_pre))
    sums$denoms[i] <- round((1144 + 568) / 2)
  } else if (sums$date[i] == as.Date('2016-05-01')) {
    sums$deaths[i] <- round(sums$deaths[i] * (568 / median_nchicken_pre))
    sums$denoms[i] <- 568
  } else {
    sums$deaths[i] <- round(sums$deaths[i] * (median_nchicken_post / median_nchicken_pre))
    sums$denoms[i] <- round(sums$denoms[i] * (median_nchicken_post / median_nchicken_pre))
  }
}

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
sums_inter <- sums_inter[,-c(3:14)] # Remove unnecessary chicken-type data
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

# Add post-vaccination period data
vax_inter <- vax_inter[1:(nrow(vax_inter) - 3),]
vax_inter$real_days_since <- NULL
vax_inter$diff_days_since <- NULL
vax_inter$days_diff <- NULL
vax_inter$real_dates <- seq.Date('2016-03-01', '2019-12-01', by='month')
vax_inter$days_since <- NULL
vax_inter$nvax[which(!is.na(vax_inter$nchicken))] <- vax_nvax
vax_inter$nvax <- ifelse(is.na(vax_inter$nvax), 0, vax_inter$nvax)

# Add death data from follow-up periods within post-vaccination period data
# '2018-08-11' follow-up occurred 2 months after '2018-06-01' vax., but unclear when started counting
# '2018-11-28' follow-up (double check occurred before Dec. vax) occurred almost 6 months after '2018-08-11' vax.
# and occurred 4 months after last follow-up
# '2019-02-03' follow-up occurred 2 months after '2018-12-01' vax.
# '2019-06-04' follow-up occurred 2 months after '2019-04-01' vax.
deaths_follow <- c()
nchicken_follow <- c()
follow_dates <- unique(hh_data_follow$date)
for (uniq_date in follow_dates) {
  deaths_follow <- c(deaths_follow, sum(hh_data_follow$deaths[which(hh_data_follow$date == uniq_date)], na.rm=T))
  nchicken_follow <- c(nchicken_follow, sum(hh_data_follow$denoms[which(hh_data_follow$date == uniq_date)], na.rm=T))
}
## Double check number of households sampled
nhh_follow <- c() 
for (uniq_date in unique(hh_data_follow$date)) {
  nhh_follow <- c(nhh_follow, length(which(hh_data_follow$date == uniq_date &
                                             !is.na(hh_data_follow$denoms))))
}
nhh_vax <- c()
for (uniq_date in unique(hh_data_vax$date)) {
  nhh_vax <- c(nhh_vax, length(which(hh_data_vax$date == uniq_date &
                                       !is.na(hh_data_vax$denoms))))# &
                                       #hh_data_vax$denoms > 0)))
}
nhh_vax[which(unique(hh_data_vax$date) == as.Date("2018-06-01"))]
nhh_follow[which(unique(hh_data_follow$date) == as.Date("2018-08-11"))]
nhh_follow[which(unique(hh_data_follow$date) == as.Date("2018-11-28"))]
nhh_vax[which(unique(hh_data_vax$date) == as.Date("2018-12-01"))]
nhh_follow[which(unique(hh_data_follow$date) == as.Date("2019-02-03"))]
nhh_vax[which(unique(hh_data_vax$date) == as.Date("2019-04-01"))]
nhh_follow[which(unique(hh_data_follow$date) == as.Date("2019-06-04"))]
nhh_vax[which(unique(hh_data_vax$date) == as.Date("2019-08-01"))]
## Manually add in data
vax_inter$deaths <- NA
follow_dates
vax_inter$deaths[max(which(vax_inter$real_dates < as.Date('2018-08-11')))] <- deaths_follow[1]
vax_inter$deaths[max(which(vax_inter$real_dates < as.Date('2018-11-28')))] <- deaths_follow[2]
vax_inter$deaths[max(which(vax_inter$real_dates < as.Date('2019-02-03')))] <- deaths_follow[3]
vax_inter$deaths[max(which(vax_inter$real_dates < as.Date('2019-06-04')))] <- deaths_follow[4]
vax_inter$nchicken[max(which(vax_inter$real_dates < as.Date('2018-08-11')))] <- nchicken_follow[1]
vax_inter$nchicken[max(which(vax_inter$real_dates < as.Date('2018-11-28')))] <- nchicken_follow[2]
vax_inter$nchicken[max(which(vax_inter$real_dates < as.Date('2019-02-03')))] <- nchicken_follow[3]
vax_inter$nchicken[max(which(vax_inter$real_dates < as.Date('2019-06-04')))] <- nchicken_follow[4]
vax_inter$deaths[which(vax_inter$real_dates == as.Date('2018-06-01'))] <- 0
vax_inter$deaths[which(vax_inter$real_dates == as.Date('2018-07-01'))] <- 0

# Interpolate to the generation interval level
for (i in nrow(vax_inter):2) {
  new_rows <- data.frame(matrix(NA, ncol=ncol(vax_inter), nrow=4))
  colnames(new_rows) <- colnames(vax_inter)
  new_rows$real_dates[1] <- vax_inter$real_dates[i] - 24
  new_rows$real_dates[2] <- vax_inter$real_dates[i] - 18
  new_rows$real_dates[3] <- vax_inter$real_dates[i] - 12
  new_rows$real_dates[4] <- vax_inter$real_dates[i] - 6
  vax_inter <- rbind(vax_inter[1:(i-1),], new_rows, vax_inter[i:nrow(vax_inter),])
}
vax_inter$days_since <- as.numeric(vax_inter$real_dates - vax_inter$real_dates[1])
vax_sub <- vax_inter[which(!is.na(vax_inter$deaths)),]
vax_sub$cumsum <- cumsum(vax_sub$deaths)
smooth_out <- splinefun(x=vax_sub$days_since, y=vax_sub$cumsum, method="hyman")
predicted_deaths <- smooth_out(vax_inter$days_since)
predicted_deaths <- round(predicted_deaths)
vax_inter$predicted_deaths <- predicted_deaths
vax_inter$predicted_deaths[which(vax_inter$real_dates < as.Date('2018-06-01') |
                                   vax_inter$real_dates > as.Date('2019-06-01'))] <- NA
vax_inter$dt <- c(NA, vax_inter$predicted_deaths[2:nrow(vax_inter)] - vax_inter$predicted_deaths[1:(nrow(vax_inter) - 1)])

vax_inter$predicted <- approx(vax_inter$days_since[which(!is.na(vax_inter$nchicken))], 
                              vax_inter$nchicken[which(!is.na(vax_inter$nchicken))], 
                              xout=vax_inter$days_since)$y
vax_inter$predicted <- round(vax_inter$predicted)

# Create 'data' variable for pomp that takes needed columns from sums_inter and vax_inter
data <- sums_inter[,which(colnames(sums_inter) == 'date' |
                            colnames(sums_inter) == 'days_since' |
                            colnames(sums_inter) == 'dt_inter' |
                            colnames(sums_inter) == 'denoms')]
data$nvax <- 0 # Manually add in overlap data
data$nvax[nrow(data)] <- 282 # Manually add in overlap data
data <- data[,c('date', 'days_since', 'dt_inter', 'denoms', 'nvax')]
data_vax <- vax_inter[,which(colnames(vax_inter) == 'nchicken' |
                               colnames(vax_inter) == 'real_dates' |
                               colnames(vax_inter) == 'dt' |
                               colnames(vax_inter) == 'days_since' |
                               colnames(vax_inter) == 'nvax')]
data_vax <- data_vax[which(data_vax$real_dates >= as.Date('2016-05-08')),]
colnames(data_vax) <- c('date', 'denoms', 'nvax', 'days_since', 'dt_inter')
data_vax <- data_vax[,c('date', 'days_since', 'dt_inter', 'denoms', 'nvax')]
data_vax$nvax <- ifelse(is.na(data_vax$nvax), 0, data_vax$nvax)
data_vax$days_since <- as.numeric(data_vax$date - data$date[1])
data <- rbind(data, data_vax)
colnames(data) <- c('date', 'days_since', 'dt_inter', 'nt_inter', 'nvax')
data$predicted_nt <- round(approx(data$days_since, data$nt_inter, xout=data$days_since)$y)
data$nt_inter <- data$predicted_nt
data$predicted_nt <- NULL
data$gen <- as.numeric(1:nrow(data))
data$reports <- data$dt_inter
data |>
  ggplot(aes(x=date,y=nt_inter))+
  ylim(0, 1250)+
  geom_line()+
  geom_line(aes(x=date, y=nvax), col='green')+
  geom_point(aes(x=date,y=dt_inter), col='red', size=0.5)

# Output data variable
save(data, file="~/vaxMada-data/objects/data_obj/data_vill1.RData")
