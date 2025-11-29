# ------------------------------------------------------------------------------
# @description: script to characterize household level data
# 
# @name: Justin Sheen
# @date: January 31, 2025
#
# @objective: This file is used to characterize household level data. Code blocks
#             to create figures are roughly aligned with when they are introduced
#             in the text.
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Part 0: Load libraries, helper functions and data
# ------------------------------------------------------------------------------
library(RColorBrewer)
library(plotfunctions)
source('~/vaxMada/code/0_helper_functions.R')
load("~/vaxMada-data/objects/data_obj/hh_data.RData")
load("~/vaxMada-data/objects/data_obj/sums.RData")
load("~/vaxMada-data/objects/data_obj/hh_data_vax.RData")
load("~/vaxMada-data/objects/data_obj/hh_data_follow.RData")

# ------------------------------------------------------------------------------
# (1) Number of households surveyed figure (Some other descriptive statistics are also calculated.)
# ------------------------------------------------------------------------------
# nhh in pre-vax period
uniq_dates <- unique(hh_data$date)
n_hh_surv <- c()
for (uniq_date in uniq_dates) {
  n_hh_surv <- c(n_hh_surv, length(which(hh_data$date == uniq_date)))
}
# nhh in vax period
hh_data_vax$denoms[1997] <- 9 # Minor fix
hh_data_vax$denoms[2017] <- 10 # Minor fix
vax_nhh <- c()
vax_nvhh <- c()
vax_nvaxvhh <- c()
vax_nchicken <- c()
vax_nvax <- c()
vax_nonresponse <- c()
vax_dates <- unique(hh_data_vax$date)[order(unique(hh_data_vax$date))]

# minor fix
hh_data_vax$denoms[which(hh_data_vax$date == as.Date('2018-12-01') & hh_data_vax$name == 'Jaoatody')[2]] <- 55
hh_data_vax <- hh_data_vax[-c(which(hh_data_vax$date == as.Date('2018-12-01') & hh_data_vax$name == 'Jaoatody')[1]),]

for (uniq_date in vax_dates) {
  sub_block <- hh_data_vax[which(hh_data_vax$date == uniq_date),]
  sub_block$denoms[which(sub_block$newnote == 'STATED NO CHICKEN')] <- 0
  
  # If more vaccinated than denominator, assume vaccine is correct
  sub_block$denoms[(which(sub_block$denoms < sub_block$vax))] <- sub_block$vax[(which(sub_block$denoms < sub_block$vax))]
  
  # Take care of imputation for nonresponse households
  nonres_dex <- which(sub_block$newnote == 'NONRESPONSE')
  zero_nonres <- round(length(nonres_dex) * length(which(sub_block$denoms == 0)) / length(which(!is.na(sub_block$denoms))))
  sub_block$denoms[nonres_dex[1:zero_nonres]] <- 0
  sub_block$denoms[nonres_dex[(zero_nonres + 1):length(nonres_dex)]] <- mean(sub_block$denoms[which(sub_block$denoms != 0)])

  # Check whether there are multiple observations in same time period
  for (name_check in unique(sub_block$name_norep)) {
    subsub_block <- sub_block[which(sub_block$name == name_check),]
    if (length(which(!is.na(subsub_block$denoms))) > 1 & (length(unique(subsub_block$denoms)) > 1
        | length(unique(subsub_block$vax)) > 1)) {
      stop(paste0('Error with household name: ', name_check, ' in period: ', uniq_date))
    }
  }
  vax_nonresponse <- c(vax_nonresponse, length(which(sub_block$newnote == 'NONRESPONSE')))
  vax_nhh <- c(vax_nhh, length(which(!is.na(sub_block$denoms))))
  vax_nvhh <- c(vax_nvhh, length(which(sub_block$denoms > 0 & sub_block$vax > 0)) / length(which(sub_block$denoms > 0)))
  vax_nchicken <- c(vax_nchicken, sum(sub_block$denoms, na.rm=T))
  vax_nvax <- c(vax_nvax, sum(sub_block$vax, na.rm=T))
  nvaxvhh <- sum(sub_block$vax, na.rm=T) / sum(sub_block$denoms[which(sub_block$vax > 0)], na.rm=T)
  vax_nvaxvhh <- c(vax_nvaxvhh, nvaxvhh)
  hist(sub_block$denoms, breaks=20)
  print(mean(sub_block$denoms[which(sub_block$denoms != 0)], na.rm=T))
}
plot(vax_dates, vax_nchicken, ylim=c(0, max(vax_nchicken)), pch=19)
points(vax_dates, vax_nvax, pch=19, col='green') # We assume those that did not respond did not vaccinate their poultry
vax_nhh
vax_nvhh
vax_nvaxvhh

png(filename="~/vaxMada-data/plots/vax_response.png", 
    width = 6,
    height = 4,
    units = "in",
    res = 200)
plot(vax_dates[2:11], vax_nhh[2:11] - vax_nonresponse[2:11], ylim=c(0, max(vax_nhh[2:11] - vax_nonresponse[2:11])),
     ylab='Num. HH', xlab='Date', pch=16)
points(vax_dates[2:11], vax_nonresponse[2:11], col='blue', pch=16)
legend(
  x = "topright",                  
  legend = c("Respondent", "Nonrespondent"),
  col = c("black", "blue"),    
  pch = c(16, 16),           
  cex = 0.8,                   
  horiz = FALSE              
)
dev.off()

vax_nonresponse / vax_nhh
mean(vax_nonresponse / vax_nhh)
mean(vax_nonresponse[which(vax_nonresponse != 0)] / vax_nhh[which(vax_nonresponse != 0)])
vax_dates
save(vax_nvax, file="~/vaxMada-data/objects/data_obj/vax_nvax.RData")
save(vax_nchicken, file="~/vaxMada-data/objects/data_obj/vax_nchicken.RData")
save(vax_dates, file="~/vaxMada-data/objects/data_obj/vax_dates.RData")
length(unique(hh_data_vax$name_norep[which(!is.na(hh_data_vax$denoms))]))
# nhh in follow-up period
follow_nhh <- c()
follow_nchicken <- c()
follow_ndead <- c()
follow_dates <- unique(hh_data_follow$date)[order(unique(hh_data_follow$date))]
for (uniq_date in follow_dates) {
  sub_block <- hh_data_follow[which(hh_data_follow$date == uniq_date),]
  follow_nhh <- c(follow_nhh, length(which(!is.na(sub_block$denoms))))
  follow_nchicken <- c(follow_nchicken, sum(sub_block$denoms, na.rm=T))
  follow_ndead <- c(follow_ndead, sum(sub_block$deaths, na.rm=T))
  hist(sub_block$denoms, breaks=20)
  print(mean(sub_block$denoms[which(sub_block$denoms != 0)], na.rm=T))
}
length(unique(hh_data_follow$name_norep[which(!is.na(hh_data_follow$denoms))]))
plot(follow_dates, follow_nhh, pch=19)
plot(follow_dates, follow_ndead, pch=19)
as.Date("2018-08-11") - as.Date("2018-06-01")
as.Date("2018-11-28") - as.Date("2018-06-01")
as.Date("2019-02-03") - as.Date("2018-12-01")
as.Date("2019-06-04") - as.Date("2019-04-01")
png(filename="~/vaxMada-data/plots/nhh.png", 
    width = 6,
    height = 4,
    units = "in",
    res = 200)
plot(uniq_dates, n_hh_surv, pch=0, main='Num. Households Observed', cex.lab=1.3,  cex.main=1.5,
     xlab='Date', ylab='Num. HH Obs.', ylim=c(0, max(vax_nhh) + 10), xlim=c(min(uniq_dates), max(vax_dates)))
points(vax_dates, vax_nhh, pch=1)
points(follow_dates, follow_nhh, pch=2)
abline(v=as.Date('2016-05-01'), lty='dashed')
legend("topleft", 
       legend = c("Pre-vaccination", "Vaccination", 'Follow-up'), 
       col = c('black', 'black', 'black'),
       pch = c(0, 1, 2), 
       pt.cex = 1.5, 
       cex = 1, 
       text.col = "black", 
       horiz = F, 
       inset = c(0.01, 0.02))
dev.off()
# ------------------------------------------------------------------------------
# (2) Raw data figure for each dataset
# ------------------------------------------------------------------------------
png(filename="~/vaxMada-data/plots/raw.png", 
    width = 10,
    height = 5.5,
    units = "in",
    res = 200)
par(mfrow = c(1,2))
par(mar=c(4.1, 4.1, 4.1, 2.1), xpd=TRUE)
plot(sums$date, sums$denoms, pch=15, col='black', 
     ylim=c(0, max(sums$denoms) + 10), 
     cex.lab=1.5,  cex.main=2.5, xlab='Date', ylab='Num. poultry.',
     main='Pre-vaccination', cex=1.3, type='l', lwd=3.5)
lines(sums$date, sums$deaths, pch=15, col='blue', cex=1.3, lwd=3.5)
legend("topright", 
       legend = c("Total", "Deaths"), 
       col = c('black', 'blue'),
       pch = c(15, 15), 
       pt.cex = 1.5, 
       cex = 1, 
       text.col = "black", 
       horiz = F, 
       inset = c(0.02,0.02))
par(mar=c(4.1, 4.1, 4.1, 2.1), xpd=TRUE)
plot(vax_dates[2:length(vax_dates)], vax_nchicken[2:length(vax_dates)], pch=16, col='black', ylim=c(0, max(vax_nchicken)), xlim=c(min(vax_dates[2:length(vax_dates)]) - 70, max(vax_dates)),
     cex.lab=1.5,  cex.main=2.5, xlab='Date', ylab='Num. poultry.',
     main='Vaccination', cex=1.5, type='l', lwd=3.5)
lines(vax_dates[2:length(vax_dates)], vax_nvax[2:length(vax_dates)], pch=16, col='green', cex=1.5, lwd=3.5)
lines(follow_dates, follow_ndead, pch=16, col='blue', cex=1.5, lwd=3.5)
legend("bottomleft", 
       legend = c("Total", "Vax.", 'Deaths'), 
       col = c('black', 'green', 'blue'),
       pch = c(16, 16, 16), 
       pt.cex = 1.5, 
       cex = 1, 
       text.col = "black", 
       horiz = F, 
       inset = c(0.02,0.02))
dev.off()

# ------------------------------------------------------------------------------
# (3) Number of households in the village
# ------------------------------------------------------------------------------
mean(vax_nhh)
sd(vax_nhh)
mean(follow_nhh)
sd(follow_nhh)
mean(c(vax_nhh, follow_nhh))
# ------------------------------------------------------------------------------
# (4) Similarities of household poultry distribution across three datasets
# ------------------------------------------------------------------------------
# Of the households with poultry, the mean number of poultry is 15
collect_denoms <- c()
collect_chicks <- c()
collect_juvs <- c()
collect_hens <- c()
collect_roosters <- c()
load("~/vaxMada-data/objects/data_obj/hh_data.RData")
collect_denoms <- c(collect_denoms, hh_data$denoms[which(hh_data$denoms != 0)])
mean(hh_data$denoms[which(hh_data$denoms != 0)], na.rm=T)
sd(hh_data$denoms[which(hh_data$denoms != 0)], na.rm=T)
collect_chicks <- c(collect_chicks, hh_data$sub.Chicks.you.own.as.of.today[which(hh_data_vax$denoms != 0)])
collect_juvs <- c(collect_juvs, hh_data$sub.Young.chickens.you.own.as.of.today[which(hh_data_vax$denoms != 0)])
collect_hens <- c(collect_hens, hh_data$sub.Hens.you.own.as.of.today[which(hh_data_vax$denoms != 0)])
collect_roosters <- c(collect_roosters, hh_data$sub.Roosters.you.own.as.of.today[which(hh_data_vax$denoms != 0)])
load("~/vaxMada-data/objects/data_obj/vill2/hh_data.RData")
collect_denoms <- c(collect_denoms, hh_data$denoms[which(hh_data$denoms != 0)])
mean(hh_data$denoms[which(hh_data$denoms != 0)], na.rm=T)
sd(hh_data$denoms[which(hh_data$denoms != 0)], na.rm=T)
collect_chicks <- c(collect_chicks, hh_data$sub.Chicks.you.own.as.of.today[which(hh_data_vax$denoms != 0)])
collect_juvs <- c(collect_juvs, hh_data$sub.Young.chickens.you.own.as.of.today[which(hh_data_vax$denoms != 0)])
collect_hens <- c(collect_hens, hh_data$sub.Hens.you.own.as.of.today[which(hh_data_vax$denoms != 0)])
collect_roosters <- c(collect_roosters, hh_data$sub.Roosters.you.own.as.of.today[which(hh_data_vax$denoms != 0)])
load("~/vaxMada-data/objects/data_obj/vill3/hh_data.RData")
collect_denoms <- c(collect_denoms, hh_data$denoms[which(hh_data$denoms != 0)])
mean(hh_data$denoms[which(hh_data$denoms != 0)], na.rm=T)
collect_chicks <- c(collect_chicks, hh_data$sub.Chicks.you.own.as.of.today[which(hh_data_vax$denoms != 0)])
collect_juvs <- c(collect_juvs, hh_data$sub.Young.chickens.you.own.as.of.today[which(hh_data_vax$denoms != 0)])
collect_hens <- c(collect_hens, hh_data$sub.Hens.you.own.as.of.today[which(hh_data_vax$denoms != 0)])
collect_roosters <- c(collect_roosters, hh_data$sub.Roosters.you.own.as.of.today[which(hh_data_vax$denoms != 0)])
sd(hh_data$denoms[which(hh_data$denoms != 0)], na.rm=T)
hist(collect_denoms, breaks=20, main='Pre-vaccination period', xlab='number of poultry')
mean(collect_denoms, na.rm=T)
sd(collect_denoms, na.rm=T)
# Of the households with poultry, the mean number of poultry is 12.3, very similar to the number
# of poultry in the pre-vaccination period
hist(hh_data_vax$denoms[which(hh_data_vax$denoms != 0 & hh_data_vax$date < as.Date("2018-12-01"))], breaks=20)
mean(hh_data_vax$denoms[which(hh_data_vax$denoms != 0 & hh_data_vax$date < as.Date("2018-12-01"))], na.rm=T)
sd(hh_data_vax$denoms[which(hh_data_vax$denoms != 0 & hh_data_vax$date < as.Date("2018-12-01"))], na.rm=T)
hist(hh_data_vax$denoms[which(hh_data_vax$denoms != 0)], breaks=20)
mean(hh_data_vax$denoms[which(hh_data_vax$denoms != 0)], na.rm=T)
sd(hh_data_vax$denoms[which(hh_data_vax$denoms != 0)], na.rm=T)
# Follow-up
hist(hh_data_follow$denoms[which(hh_data_follow$denoms != 0)], breaks=20)
mean(hh_data_follow$denoms[which(hh_data_follow$denoms != 0)], na.rm=T)
sd(hh_data_follow$denoms[which(hh_data_follow$denoms != 0)], na.rm=T)
# All together
together_pph <- c(collect_denoms,
                  hh_data_vax$denoms[which(hh_data_vax$denoms != 0)],
                  hh_data_follow$denoms[which(hh_data_follow$denoms != 0)])
mean(together_pph, na.rm=T)
sd(together_pph, na.rm=T)
png(filename="~/vaxMada-data/plots/hhDist.png", 
    width = 8,
    height = 3,
    units = "in",
    res = 200)
par(mfrow = c(1,2))
hist(collect_denoms, breaks=20, main='Pre-vaccination data', xlab='number of poultry', cex.lab=1.3,  cex.main=1.5, )
hist(hh_data_vax$denoms[which(hh_data_vax$denoms != 0 & hh_data_vax$date < as.Date("2018-12-01"))], breaks=20, main='Vaccination data', xlab='number of poultry', cex.lab=1.3,  cex.main=1.5, )
dev.off()
# Distributions by age group
mean(collect_chicks, na.rm=T)
sd(collect_chicks, na.rm=T)
mean(collect_juvs, na.rm=T)
sd(collect_juvs, na.rm=T)
mean(collect_hens, na.rm=T)
sd(collect_hens, na.rm=T)
mean(collect_roosters, na.rm=T)
sd(collect_roosters, na.rm=T)
load("~/vaxMada-data/objects/data_obj/hh_data.RData")  
load("~/vaxMada-data/objects/data_obj/vill2/hh_data.RData") 
load("~/vaxMada-data/objects/data_obj/vill3/hh_data.RData") # vill3
mean(hh_data$sub.Chicks.you.own.as.of.today[which(hh_data_vax$denoms != 0)], na.rm=T)
sd(hh_data$sub.Chicks.you.own.as.of.today[which(hh_data_vax$denoms != 0)], na.rm=T)
mean(hh_data$sub.Young.chickens.you.own.as.of.today[which(hh_data_vax$denoms != 0)], na.rm=T)
sd(hh_data$sub.Young.chickens.you.own.as.of.today[which(hh_data_vax$denoms != 0)], na.rm=T)
mean(hh_data$sub.Hens.you.own.as.of.today[which(hh_data_vax$denoms != 0)], na.rm=T)
sd(hh_data$sub.Hens.you.own.as.of.today[which(hh_data_vax$denoms != 0)], na.rm=T)
mean(hh_data$sub.Roosters.you.own.as.of.today[which(hh_data_vax$denoms != 0)], na.rm=T)
sd(hh_data$sub.Roosters.you.own.as.of.today[which(hh_data_vax$denoms != 0)], na.rm=T)
png(filename="~/vaxMada-data/plots/ageDist.png", 
    width = 9,
    height = 2.5,
    units = "in",
    res = 200)
par(mfrow = c(1,4))
hist(collect_chicks,
     main='Chicks', xlab='number of poultry', breaks=30, cex.lab=1.3, cex.main=1.5)
hist(collect_juvs, 
     main='Young chickens', xlab='number of poultry', breaks=30, cex.lab=1.3, cex.main=1.5)
hist(collect_hens, 
     main='Hens', xlab='number of poultry', breaks=10, cex.lab=1.3, cex.main=1.5)
hist(collect_roosters, 
     main='Roosters', xlab='number of poultry', breaks=5, cex.lab=1.3, cex.main=1.5)
dev.off()
# ------------------------------------------------------------------------------
# (5) Village attack rate
# ------------------------------------------------------------------------------
# Prevax
prevax_vill_attack <- c()
load("~/vaxMada-data/objects/data_obj/hh_data.RData") 
uniq_dates <- unique(hh_data$date)
for (uniq_date in uniq_dates) {
  sub_block <- hh_data[which(hh_data$date == uniq_date),]
  prevax_vill_attack <- c(prevax_vill_attack, 
                          length(which(sub_block$deaths > 0)) / nrow(sub_block))
}
load("~/vaxMada-data/objects/data_obj/vill2/hh_data.RData") 
uniq_dates <- unique(hh_data$date)
for (uniq_date in uniq_dates) {
  sub_block <- hh_data[which(hh_data$date == uniq_date),]
  prevax_vill_attack <- c(prevax_vill_attack, 
                          length(which(sub_block$deaths > 0)) / nrow(sub_block))
}
load("~/vaxMada-data/objects/data_obj/vill3/hh_data.RData")
uniq_dates <- unique(hh_data$date)
for (uniq_date in uniq_dates) {
  sub_block <- hh_data[which(hh_data$date == uniq_date),]
  prevax_vill_attack <- c(prevax_vill_attack, 
                          length(which(sub_block$deaths > 0)) / nrow(sub_block))
}
mean(prevax_vill_attack)
sd(prevax_vill_attack)
# Follow-up
follow_vill_attack <- c()
for (uniq_date in follow_dates) {
  sub_block <- hh_data_follow[which(hh_data_follow$date == uniq_date),]
  follow_vill_attack <- c(follow_vill_attack, 
                          length(which(sub_block$deaths > 0)) / nrow(sub_block))
}
mean(follow_vill_attack)
sd(follow_vill_attack)
# ------------------------------------------------------------------------------
# (6) Household attack rate for pre-vaccination period
# ------------------------------------------------------------------------------
# Prop. deaths per household (household attack rate)
get_hh_death <- function(hh_data) {
  hh_attack <- c()
  hh_attack_inczero <- c()
  hh_attack_inczero_flow <- c()
  hh_attack_dates <- c()
  hh_attack_names <- c()
  hh_attack_inczero_chick <- c()
  hh_attack_inczero_ychick <- c()
  hh_attack_inczero_hen <- c()
  hh_attack_inczero_rooster <- c()
  hh_attack_inczero_chick_flow <- c()
  hh_attack_inczero_ychick_flow <- c()
  hh_attack_inczero_hen_flow <- c()
  hh_attack_inczero_rooster_flow <- c()
  for (uniq_hh in unique(hh_data$name)) {
    sub_block <- hh_data[which(hh_data$name == uniq_hh),]
    sub_block <- sub_block[order(sub_block$date),]
    if (nrow(sub_block) > 2) {
      for (i in 1:(nrow(sub_block) - 1)) {
        if (!is.na(sub_block$denoms[i]) & !is.na(sub_block$deaths[i + 1])) {
          if (sub_block$denoms[i] != 0) {
            # Record names and dates
            hh_attack_names <- c(hh_attack_names, uniq_hh)
            hh_attack_dates <- c(hh_attack_dates, sub_block$date[i])
            
            # General
            hh_attack_inczero <- c(hh_attack_inczero, (sub_block$deaths[i + 1] / sub_block$denoms[i]))
            flow <- sub_block$denoms[i+1] - sub_block$denoms[i] + sub_block$deaths[i+1]
            hh_attack_inczero_flow <- c(hh_attack_inczero_flow, (sub_block$deaths[i + 1] / (sub_block$denoms[i] + flow)))
            
            # Subgroups
            hh_attack_inczero_chick <- c(hh_attack_inczero_chick, (sub_block$sub.Chicks.died.from.disease[i + 1] / sub_block$sub.Chicks.you.own.as.of.today[i]))
            flow_chick <- sub_block$sub.Chicks.you.own.as.of.today[i+1] - sub_block$sub.Chicks.you.own.as.of.today[i] + sub_block$sub.Chicks.died.from.disease[i+1]
            hh_attack_inczero_chick_flow <- c(hh_attack_inczero_chick_flow, (sub_block$sub.Chicks.died.from.disease[i + 1] / (sub_block$sub.Chicks.you.own.as.of.today[i] + flow_chick)))
            
            hh_attack_inczero_ychick <- c(hh_attack_inczero_ychick, (sub_block$sub.Young.chickens..died.from.disease[i + 1] / sub_block$sub.Young.chickens.you.own.as.of.today[i]))
            flow_ychick <- sub_block$sub.Young.chickens.you.own.as.of.today[i+1] - sub_block$sub.Young.chickens.you.own.as.of.today[i] + sub_block$sub.Young.chickens..died.from.disease[i+1]
            hh_attack_inczero_ychick_flow <- c(hh_attack_inczero_ychick_flow, (sub_block$sub.Young.chickens..died.from.disease[i + 1] / (sub_block$sub.Young.chickens.you.own.as.of.today[i] + flow_ychick)))
            
            hh_attack_inczero_hen <- c(hh_attack_inczero_hen, (sub_block$sub.Hens.died.from.disease[i + 1] / sub_block$sub.Hens.you.own.as.of.today[i]))
            flow_hen <- sub_block$sub.Hens.you.own.as.of.today[i+1] - sub_block$sub.Hens.you.own.as.of.today[i] + sub_block$sub.Hens.died.from.disease[i+1]
            hh_attack_inczero_hen_flow <- c(hh_attack_inczero_hen_flow, (sub_block$sub.Hens.died.from.disease[i + 1] / (sub_block$sub.Hens.you.own.as.of.today[i] + flow_hen)))
            
            hh_attack_inczero_rooster <- c(hh_attack_inczero_rooster, (sub_block$sub.Roosters..died.from.disease[i + 1] / sub_block$sub.Roosters.you.own.as.of.today[i]))
            flow_rooster <- sub_block$sub.Roosters.you.own.as.of.today[i+1] - sub_block$sub.Roosters.you.own.as.of.today[i] + sub_block$sub.Roosters..died.from.disease[i+1]
            hh_attack_inczero_rooster_flow <- c(hh_attack_inczero_rooster_flow, (sub_block$sub.Roosters..died.from.disease[i + 1] / (sub_block$sub.Roosters.you.own.as.of.today[i] + flow_rooster)))
            
            # Attack with at least one death
            if (sub_block$deaths[i + 1] >= 1) {
              hh_attack <- c(hh_attack, (sub_block$deaths[i + 1] / sub_block$denoms[i]))
            } else {
              hh_attack <- c(hh_attack, NA)
            }
          }
        }
      }
    }
  }
  uniq_dates <- unique(hh_data$date)
  prevax_vill_attack <- c()
  for (uniq_date in uniq_dates) {
    sub_block <- hh_data[which(hh_data$date == uniq_date),]
    prevax_vill_attack <- c(prevax_vill_attack, 
                            length(which(sub_block$deaths > 0)) / nrow(sub_block))
  }
  return(list(hh_attack_dates, hh_attack_names, 
              hh_attack, 
              hh_attack_inczero, hh_attack_inczero_flow,
              hh_attack_inczero_chick, hh_attack_inczero_ychick, hh_attack_inczero_hen, hh_attack_inczero_rooster,
              hh_attack_inczero_chick_flow, hh_attack_inczero_ychick_flow, hh_attack_inczero_hen_flow, hh_attack_inczero_rooster_flow,
              uniq_dates, prevax_vill_attack))
}
load("~/vaxMada-data/objects/data_obj/hh_data.RData")
hh_death <- get_hh_death(hh_data=hh_data)
load("~/vaxMada-data/objects/data_obj/vill2/hh_data.RData") 
vill2_hh_death <- get_hh_death(hh_data=hh_data)
load("~/vaxMada-data/objects/data_obj/vill3/hh_data.RData")
vill3_hh_death <- get_hh_death(hh_data=hh_data)

# Here, we assume any deaths greater than occurred in the previous timestep were 
# inflow deaths that should not be counted in attack rate, and that there is no outflow
hh_attack_dates <- c(hh_death[[1]], vill2_hh_death[[1]], vill3_hh_death[[1]])
hh_attack_names <- c(hh_death[[2]], vill2_hh_death[[2]], vill3_hh_death[[2]])
hh_attack <- c(hh_death[[3]], vill2_hh_death[[3]], vill3_hh_death[[3]])
hh_attack_inczero <- c(hh_death[[4]], vill2_hh_death[[4]], vill3_hh_death[[4]])
hh_attack_inczero_flow <- c(hh_death[[5]], vill2_hh_death[[5]], vill3_hh_death[[5]])
hh_attack_inczero_chick <- c(hh_death[[6]], vill2_hh_death[[6]], vill3_hh_death[[6]])
hh_attack_inczero_ychick <- c(hh_death[[7]], vill2_hh_death[[7]], vill3_hh_death[[7]])
hh_attack_inczero_hen <- c(hh_death[[8]], vill2_hh_death[[8]], vill3_hh_death[[8]])
hh_attack_inczero_rooster <- c(hh_death[[9]], vill2_hh_death[[9]], vill3_hh_death[[9]])
hh_attack_inczero_chick_flow <- c(hh_death[[10]], vill2_hh_death[[10]], vill3_hh_death[[10]])
hh_attack_inczero_ychick_flow <- c(hh_death[[11]], vill2_hh_death[[11]], vill3_hh_death[[11]])
hh_attack_inczero_hen_flow <- c(hh_death[[12]], vill2_hh_death[[12]], vill3_hh_death[[12]])
hh_attack_inczero_rooster_flow <- c(hh_death[[13]], vill2_hh_death[[13]], vill3_hh_death[[13]])

mean(hh_attack_inczero_flow[which(is.finite(hh_attack_inczero_flow) & hh_attack_inczero_flow > 0)], na.rm=T)
sd(hh_attack_inczero_flow[which(is.finite(hh_attack_inczero_flow) & hh_attack_inczero_flow > 0)], na.rm=T)

mean(hh_attack_inczero_flow[which(is.finite(hh_attack_inczero_flow))], na.rm=T)
sd(hh_attack_inczero_flow[which(is.finite(hh_attack_inczero_flow))], na.rm=T)

mean(hh_attack_inczero_chick_flow[which(is.finite(hh_attack_inczero_chick_flow))], na.rm=T)
sd(hh_attack_inczero_chick_flow[which(is.finite(hh_attack_inczero_chick_flow))], na.rm=T)

mean(hh_attack_inczero_ychick_flow[which(is.finite(hh_attack_inczero_ychick_flow))], na.rm=T)
sd(hh_attack_inczero_ychick_flow[which(is.finite(hh_attack_inczero_ychick_flow))], na.rm=T)

mean(hh_attack_inczero_hen_flow[which(is.finite(hh_attack_inczero_hen_flow))], na.rm=T)
sd(hh_attack_inczero_hen_flow[which(is.finite(hh_attack_inczero_hen_flow))], na.rm=T)

mean(hh_attack_inczero_rooster_flow[which(is.finite(hh_attack_inczero_rooster_flow))], na.rm=T)
sd(hh_attack_inczero_rooster_flow[which(is.finite(hh_attack_inczero_rooster_flow))], na.rm=T)

# Show no household dependence of monthly household attack rate either
# First order the entries
load("~/vaxMada-data/objects/data_obj/hh_data.RData") # 
hh_data <- hh_data
load("~/vaxMada-data/objects/data_obj/vill2/hh_data.RData") # vill2
hh_data_vill2 <- hh_data
load("~/vaxMada-data/objects/data_obj/vill3/hh_data.RData") # vill3
hh_data_vill3 <- hh_data
hh_datas <- list(hh_data, hh_data_vill2, hh_data_vill3)
death_stats <- list(hh_death, vill2_hh_death, vill3_hh_death)
png(filename="~/vaxMada-data/plots/deathHH.png", 
    width = 6,
    height = 8,
    units = "in",
    res = 300)
par(mfrow = c(3,2))
par(mar=c(4.1, 6.1, 4.1, 2.1), xpd=F)
ress <- c()
for (vill_count in 1:3) {
  hh_data <- hh_datas[[vill_count]]
  death_stat <- death_stats[[vill_count]]
  hh_attack_dates <- death_stat[[1]]
  hh_attack_names <- death_stat[[2]]
  hh_attack_inczero_flow <- death_stat[[5]]
  hh_attack_chick_flow <- death_stat[[10]]
  hh_attack_ychick_flow <- death_stat[[11]]
  hh_attack_hen_flow <- death_stat[[12]]
  hh_attack_rooster_flow <- death_stat[[13]]
  uniq_dates <- death_stat[[14]]
  prevax_vill_attack <- death_stat[[15]]
  mean_attack <- c()
  uniq_hhs <- unique(hh_data$name)
  for (unique_hh in uniq_hhs) {
    mean_attack <- c(mean_attack, mean(hh_attack_inczero_flow[which(hh_attack_names == unique_hh)], na.rm=T))
  }
  ordered_deaths <- data.frame(matrix(c(uniq_hhs, mean_attack), ncol=2))
  colnames(ordered_deaths) <- c('name', 'mean_attack')
  ordered_deaths$mean_attack <- as.numeric(ordered_deaths$mean_attack)
  ordered_deaths <- ordered_deaths[order(ordered_deaths$mean_attack, decreasing=T),]
  ordered_deaths$order <- 1:nrow(ordered_deaths)
  hh_attack_order <- c()
  for (i in 1:length(hh_attack_names)) {
    hh_attack_order <- c(hh_attack_order, ordered_deaths$order[which(ordered_deaths$name == hh_attack_names[i])])
  }
  hh_data$ordered <- NA
  for (i in 1:length(hh_attack_names)) {
    hh_data$ordered[which(hh_data$name == hh_attack_names[i])] <- hh_attack_order[i]
  }
  # Next color code by date (heat map)
  hh_data$color <- NA
  color_dates <- heat.colors(n=length(uniq_dates)) 
  is.unsorted(uniq_dates)
  hh_data_colors <- rep('', length(hh_attack_inczero_flow)) 
  for (uniq_date_dex in 1:length(uniq_dates)) {
    hh_data$color[which(hh_data$date == uniq_dates[uniq_date_dex])] <- color_dates[uniq_date_dex]
    hh_data_colors[which(hh_attack_dates == uniq_dates[uniq_date_dex])] <- color_dates[uniq_date_dex]
  }
  pw_res <- pairwise.wilcox.test(c(hh_attack_inczero_flow[which(!is.na(hh_attack_inczero_flow))], 0), 
                                 c(hh_attack_order[which(!is.na(hh_attack_inczero_flow))], 23),
                                 p.adjust.method = "BH")
  res <- sum(pw_res$p.value[which(!is.na(pw_res$p.value))] < 0.05) / length(pw_res$p.value[which(!is.na(pw_res$p.value))])
  ress <- c(ress, res)
  pw_res2 <- pairwise.wilcox.test(hh_data$deaths, 
                                  hh_data$ordered,
                                  p.adjust.method = "BH")
  res2 <- sum(pw_res2$p.value[which(!is.na(pw_res2$p.value))] < 0.05) / length(pw_res2$p.value[which(!is.na(pw_res2$p.value))])
  ress <- c(ress, res2)
  if (vill_count == 1) {
    plot(c(hh_attack_order, 23),  c(hh_attack_inczero_flow, 0), 
         ylim=c(0, 1), col=hh_data_colors, 
         main='HH month mortality rate', ylab=paste0('Village ', vill_count, '\nHH month mortality rate'), xlab=paste0('HH index Vill. ', vill_count),
         cex.lab=1.3, cex.main=1.4)
    gradientLegend(valRange=c(length(uniq_dates), 1), color='heat', 
                   nCol=length(uniq_dates), pos=0.86, inside=T, length=0.12, depth=0.05)
    text(22.6, 1.01, '# mo.', cex=0.8)
    plot(x= hh_data$ordered, y=hh_data$deaths, col=hh_data$color, 
         main='Monthly deaths due to disease', ylab='Deaths due to disease', xlab=paste0('HH index Vill. ', vill_count),
         cex.lab=1.3, cex.main=1.4)
    gradientLegend(valRange=c(length(uniq_dates), 1), color='heat', 
                   nCol=length(uniq_dates), pos=0.86, inside=T, length=0.12, depth=0.05)
    text(length(unique(hh_data$ordered)) - 1.5, max(hh_data$deaths, na.rm=T), '# mo.', cex=0.8)
  } else {
    plot(c(hh_attack_order, 23),  c(hh_attack_inczero_flow, 0), 
         ylim=c(0, 1), col=hh_data_colors, 
        ylab=paste0('Village ', vill_count, '\nHH month mortality rate'), xlab=paste0('HH index Vill. ', vill_count),
         cex.lab=1.3, cex.main=1.4)
    gradientLegend(valRange=c(length(uniq_dates), 1), color='heat', 
                   nCol=length(uniq_dates), pos=0.86, inside=T, length=0.12, depth=0.05)
    text(22.6, 1.01, '# mo.', cex=0.8)
    plot(x= hh_data$ordered, y=hh_data$deaths, col=hh_data$color, 
          ylab='Deaths due to disease', xlab=paste0('HH index Vill. ', vill_count),
         cex.lab=1.3, cex.main=1.4)
    gradientLegend(valRange=c(length(uniq_dates), 1), color='heat', 
                   nCol=length(uniq_dates), pos=0.86, inside=T, length=0.12, depth=0.05)
    text(length(unique(hh_data$ordered)) - 1.5, max(hh_data$deaths, na.rm=T), '# mo.', cex=0.8)
  }
}
dev.off()

# Chicken type dependence of deaths
png(filename="~/vaxMada-data/plots/ageDeath.png", 
    width = 12.5,
    height = 3,
    units = "in",
    res = 300)
par(mfrow = c(1, 4))
par(mar=c(4.1, 5.1, 4.1, 2.1), xpd=TRUE)
for (vill_count in 1) {
  if (vill_count == 1) {
    load("~/vaxMada-data/objects/data_obj/sums.RData") # 
  } else if (vill_count == 2) {
    load("~/vaxMada-data/objects/data_obj/vill2/sums.RData") # vill2
  } else {
    load("~/vaxMada-data/objects/data_obj/vill3/sums.RData") # vill3
  }
  plot(y=sums$denoms_chick[1:(nrow(sums) - 1)], x= sums$date[1:(nrow(sums) - 1)], pch = 19, lwd=0.1, ylim=c(0, max(sums$denoms_chick)),
       main='Chicks', ylab='number of poultry', xlab='Date', cex.lab=1.75, cex.main=2)
  points(y=sums$deaths_chick[2:nrow(sums)], x= sums$date[1:(nrow(sums) - 1)], pch = 19, lwd=0.1, col='red')
  legend("topright", 
         legend = c("Total", "Deaths"), 
         col = c('black', 'red'),
         pch = c(19, 19), 
         pt.cex = 1.5, 
         cex = 1, 
         text.col = "black", 
         horiz = F, 
         inset = c(0.01, 0.02))
  plot(y=sums$denoms_young[1:(nrow(sums) - 1)], x= sums$date[1:(nrow(sums) - 1)], pch = 19, lwd=0.1, ylim=c(0, max(sums$denoms_young)),
       main='Young chickens', ylab='number of poultry', xlab='Date', cex.lab=1.75, cex.main=2)
  points(y=sums$deaths_young[2:nrow(sums)], x= sums$date[1:(nrow(sums) - 1)], pch = 19, lwd=0.1, col='red')
  legend("topright", 
         legend = c("Total", "Deaths"), 
         col = c('black', 'red'),
         pch = c(19, 19), 
         pt.cex = 1.5, 
         cex = 1, 
         text.col = "black", 
         horiz = F, 
         inset = c(0.01, 0.02))
  plot(y=sums$denoms_hen[1:(nrow(sums) - 1)], x= sums$date[1:(nrow(sums) - 1)], pch = 19, lwd=0.1, ylim=c(0, max(sums$denoms_hen)),
       main='Hens', ylab='number of poultry', xlab='Date', cex.lab=1.75, cex.main=2)
  points(y=sums$deaths_hen[2:nrow(sums)], x= sums$date[1:(nrow(sums) - 1)], pch = 19, lwd=0.1, col='red')
  legend("topright", 
         legend = c("Total", "Deaths"), 
         col = c('black', 'red'),
         pch = c(19, 19), 
         pt.cex = 1.5, 
         cex = 1, 
         text.col = "black", 
         horiz = F, 
         inset = c(0.01, 0.02))
  plot(y=sums$denoms_rooster[1:(nrow(sums) - 1)], x= sums$date[1:(nrow(sums) - 1)], pch = 19, lwd=0.1, ylim=c(0, max(sums$denoms_rooster)),
       main='Roosters', ylab='number of poultry', xlab='Date', cex.lab=1.75, cex.main=2)
  points(y=sums$deaths_rooster[2:nrow(sums)], x= sums$date[1:(nrow(sums) - 1)], pch = 19, lwd=0.1, col='red')
  legend("topright", 
         legend = c("Total", "Deaths"), 
         col = c('black', 'red'),
         pch = c(19, 19), 
         pt.cex = 1.5, 
         cex = 1, 
         text.col = "black", 
         horiz = F, 
         inset = c(0.01, 0.02))
}
dev.off()

# ------------------------------------------------------------------------------
# (7) Vax statistics
# ------------------------------------------------------------------------------
mean(vax_nvhh[2:12])
sd(vax_nvhh[2:12])
mean(vax_nvax[2:12] / vax_nchicken[2:12])
sd(vax_nvax[2:12] / vax_nchicken[2:12])
mean(vax_nvaxvhh, na.rm=T)
sd(vax_nvaxvhh, na.rm=T)

# Dates pre and post move of vaccination
pre_move_dates <- vax_dates[1:8]
post_move_dates <- vax_dates[9:12]

# Break down of proportion of vaccination in each group post move when we have this data
postmove_hh_data_vax <- hh_data_vax[which(hh_data_vax$date %in% post_move_dates),]
postmove_chicks <- c()
postmove_ychicks <- c()
postmove_hens <- c()
postmove_roosters <- c()
for (post_date in post_move_dates) {
  sub_block <- postmove_hh_data_vax[which(postmove_hh_data_vax$date == post_date),]
  postmove_chicks <- c(postmove_chicks, sum(sub_block$v_chick, na.rm=T))
  postmove_ychicks <- c(postmove_ychicks, sum(sub_block$v_ychicken, na.rm=T))
  postmove_hens <- c(postmove_hens, sum(sub_block$v_hen, na.rm=T))
  postmove_roosters <- c(postmove_roosters, sum(sub_block$v_rooster, na.rm=T))
}
png(filename="~/vaxMada-data/plots/vaxAdd.png", 
    width = 9,
    height = 3,
    units = "in",
    res = 300)
par(mfrow = c(1,3))
plot(vax_dates[2:12], vax_nvhh[2:12], ylim=c(0, 1), pch=19,
     ylab='% HH vaccinated', xlab='Date', cex.lab=1.3)
plot(vax_dates[2:12], vax_nvaxvhh[2:12], ylim=c(0, 1), pch=19,
     ylab='% vaccinated per vaccinated HH', xlab='Date', cex.lab=1.3)
plot(post_move_dates, postmove_chicks, ylim=c(0, max(postmove_chicks)), pch=19, col='black',
     ylab='Number vaccinated', xlab='Date (December 2018 to December 2019)', main='Num. vaccinated by type', cex.lab=1.3, cex.main=1.5)
points(post_move_dates, postmove_ychicks, pch=19, col='purple')
points(post_move_dates, postmove_hens, pch=19, col='red')
points(post_move_dates, postmove_roosters, pch=19, col='blue')
legend("topright", 
       legend = c("Chicks", "Young chickens", 'Hens', 'Roosters'), 
       col = c('black', 'purple', 'red', 'blue'),
       pch = c(19, 19, 19, 19), 
       pt.cex = 1, 
       cex = 0.7, 
       text.col = "black", 
       horiz = F, 
       inset = c(0.01, 0.02))
dev.off()

# ------------------------------------------------------------------------------
# (8) N unique households and dates
# ------------------------------------------------------------------------------
load("~/vaxMada-data/objects/data_obj/hh_data.RData")
length(unique(hh_data$name))
load("~/vaxMada-data/objects/data_obj/vill2/hh_data.RData")
length(unique(hh_data$name))
load("~/vaxMada-data/objects/data_obj/vill3/hh_data.RData")
length(unique(hh_data$name))

load("~/vaxMada-data/objects/data_obj/hh_data.RData")
length(unique(hh_data$date))
load("~/vaxMada-data/objects/data_obj/vill2/hh_data.RData")
length(unique(hh_data$date))
load("~/vaxMada-data/objects/data_obj/vill3/hh_data.RData")
length(unique(hh_data$date))

# ------------------------------------------------------------------------------
# (9) Plot raw data households
# ------------------------------------------------------------------------------
png(filename="~/vaxMada-data/plots/nsamp_prevax.png", 
    width = 10,
    height = 3,
    units = "in",
    res = 300)
par(mfrow = c(1,3))
load("~/vaxMada-data/objects/data_obj/hh_data.RData")
uniq_dates <- unique(hh_data$date)
n_hh_surv <- c()
for (uniq_date in uniq_dates) {
  n_hh_surv <- c(n_hh_surv, length(which(hh_data$date == uniq_date)))
}
plot(uniq_dates, n_hh_surv, ylim=c(0, max(n_hh_surv)), xlab='Dates', ylab='Num. sampled', 
     main='Village 1', pch=16)
mean(n_hh_surv)
load("~/vaxMada-data/objects/data_obj/vill2/hh_data.RData")
uniq_dates <- unique(hh_data$date)
n_hh_surv <- c()
for (uniq_date in uniq_dates) {
  n_hh_surv <- c(n_hh_surv, length(which(hh_data$date == uniq_date)))
}
plot(uniq_dates, n_hh_surv, ylim=c(0, max(n_hh_surv)), xlab='Dates', ylab='Num. sampled', 
     main='Village 2', pch=16)
mean(n_hh_surv)
load("~/vaxMada-data/objects/data_obj/vill3/hh_data.RData")
uniq_dates <- unique(hh_data$date)
n_hh_surv <- c()
for (uniq_date in uniq_dates) {
  n_hh_surv <- c(n_hh_surv, length(which(hh_data$date == uniq_date)))
}
plot(uniq_dates, n_hh_surv, ylim=c(0, max(n_hh_surv)), xlab='Dates', ylab='Num. sampled', 
     main='Village 3', pch=16)
mean(n_hh_surv)
dev.off()



