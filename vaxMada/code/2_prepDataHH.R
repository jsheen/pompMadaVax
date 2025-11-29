# ------------------------------------------------------------------------------
# @description: script to prep data at the household level. To be run after prepData.R
# 
# @name: Justin Sheen
# @date: January 28, 2025
#
# @objective: This file is used to further prep data at the household level
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Part 0: Load helper functions and data
# ------------------------------------------------------------------------------
source('~/vaxMada/code/helper_functions.R')
load("~/vaxMada-data/objects/data_obj/makira_ownership.RData")
load("~/vaxMada-data/objects/data_obj/full_vax.RData")
load("~/vaxMada-data/objects/data_obj/jul_18.RData")
load("~/vaxMada-data/objects/data_obj/jun_19.RData")
load("~/vaxMada-data/objects/data_obj/hervet_recent_df.RData")

# ------------------------------------------------------------------------------
# Part 1: Prepare pre-vaccination data for analysis
# ------------------------------------------------------------------------------
# (1) Pre-vaccination data
new_dates <- paste0(makira_ownership$Year, "_", makira_ownership$Month)
makira_ownership$date <- sapply(new_dates, function(x) switch_date(x))
makira_ownership$date <- as.Date(makira_ownership$date, origin='1970-01-01')
makira_ownership[,5:40] <- numeric_clean(makira_ownership[,5:40])
# Make some names the same (these names are for Antaravato, but we take care of this in the Makira ownership step)
makira_ownership$Household.name[which(makira_ownership$Household.name == ' voavy clovis' | 
                                        makira_ownership$Household.name == 'Voavy  Clovis')] <- 'Voavy clovis'
makira_ownership$Household.name[which(makira_ownership$Household.name == 'alphonse' | 
                                        makira_ownership$Household.name == 'Be alphonse')] <- 'BE Aphonse'
makira_ownership$Household.name[which(makira_ownership$Household.name == 'be zackson' | 
                                        makira_ownership$Household.name == 'BE zackson' | 
                                        makira_ownership$Household.name == 'BE zakson' |
                                        makira_ownership$Household.name == 'Zackson')] <- 'BE JACKSON'
makira_ownership$Household.name[which(makira_ownership$Household.name == 'Bealanana Charle' | 
                                        makira_ownership$Household.name == 'Belalahy charles')] <- 'Bealanana charle'
makira_ownership$Household.name[which(makira_ownership$Household.name == 'Bezafy Romin')] <- 'Bezafy romain'
makira_ownership$Household.name[which(makira_ownership$Household.name == 'Ruchard')] <- 'Richard edme'
makira_ownership$Household.name[which(makira_ownership$Household.name == 'Moratombo Frederic' | 
                                        makira_ownership$Household.name == 'Moratombo frederic')] <- 'Frederic'
makira_ownership$Household.name[which(makira_ownership$Household.name == 'elizabeth' | 
                                        makira_ownership$Household.name == 'Elizabeth' |
                                        makira_ownership$Household.name == 'Elizabeth  moratombo Kalotsara' |
                                        makira_ownership$Household.name == 'Elizabeth Kalotsara' |
                                        makira_ownership$Household.name == 'Kalotsara')] <- 'elisabeth'
makira_ownership$Household.name[which(makira_ownership$Household.name == 'Prezalin')] <- 'Prezelin'
makira_ownership$Household.name[which(makira_ownership$Household.name == 'Radson' | 
                                        makira_ownership$Household.name == 'radison' |
                                        makira_ownership$Household.name == 'rasdison')] <- 'Radison'
makira_ownership$Household.name[which(makira_ownership$Household.name == 'Roger charle')] <- 'Roger Charle'
makira_ownership$Household.name[which(makira_ownership$Household.name == 'Tsaravelo  justin')] <- 'Tsaravelo Justin'
makira_ownership$Household.name[which(makira_ownership$Household.name == 'Zarason ')] <- 'Zarason'
antaravato_makira_ownership <- makira_ownership[which(makira_ownership$Town == 'Antaravato'),] #|
                                                        #makira_ownership$Town == 'ANTARAVATO'),] # TODO: Exclude the August data
antaravato_makira_ownership[,41:66] <- NULL # All NA or '' except one unclear deleted comment in 66
# Take care of a couple duplicates
antaravato_makira_ownership$Young.chickens.you.own.as.of.today[which(antaravato_makira_ownership$Household.name == 'Tsaravelo Justin' &
                                                                       antaravato_makira_ownership$Month == 'Fevrier' &
                                                                       antaravato_makira_ownership$Year == '2014' &
                                                                       antaravato_makira_ownership$Young.chickens.you.own.as.of.today == 3)] <- 4
zarason_dex <- which(antaravato_makira_ownership$Household.name == 'Zarason' &
                       antaravato_makira_ownership$Month == 'juillet' &
                       antaravato_makira_ownership$Year == '2013' &
                       antaravato_makira_ownership$Chicks.you.own.as.of.today == 3)
antaravato_makira_ownership$Chicks.you.own.as.of.today[zarason_dex] <- 12
antaravato_makira_ownership$Chicks.died.from.disease[zarason_dex] <- 15
antaravato_makira_ownership$Young.chickens.you.own.as.of.today[zarason_dex] <- 3
antaravato_makira_ownership$Young.chickens..eaten.by.household[zarason_dex] <- 2
antaravato_makira_ownership$Young.chickens..died.from.disease[zarason_dex] <- 3
antaravato_makira_ownership$Hens.you.own.as.of.today[zarason_dex] <- 6
antaravato_makira_ownership$Hens.died.from.disease[zarason_dex] <- 6
antaravato_makira_ownership$Roosters.you.own.as.of.today[zarason_dex] <- 2
# Check that the households sampled at each timestep are unique
cnt_mult <- 0
for (unique_date in unique(antaravato_makira_ownership$date)) {
  sub <- antaravato_makira_ownership[which(antaravato_makira_ownership$date == unique_date),]
  if (length(unique(sub$Household.name)) != nrow(sub)) {
    for (unique_hh in unique(sub$Household.name)) {
      sub2 <- sub[which(sub$Household.name == unique_hh),]
      cnt_mult <- cnt_mult + nrow(sub2) - 1
      for (j in 1:ncol(sub2)) {
        if (length(unique(sub2[,j])) > 1) {
          stop('Error.')
        }
      }
    }
  }
}
antaravato_makira_ownership <- antaravato_makira_ownership[which(!duplicated(antaravato_makira_ownership)),]

# Create object 'hh_data' and 'sums' which can be used to answer questions about village transmission
hh_data <- data.frame(matrix(ncol=16, nrow=0))
colnames(hh_data) <- c('deaths', 'denoms', 
                       'deaths_chick', 'denoms_chick',
                       'deaths_young', 'denoms_young',
                       'deaths_hen', 'denoms_hen',
                       'deaths_rooster', 'denoms_rooster',
                       'inflow', 'outflow',
                       'inflow_chicks', 'outflow_chicks',
                       'date', 'name')
sums <- data.frame(matrix(ncol=15, nrow=0))
colnames(sums) <- c('deaths', 'denoms', 
                    'deaths_chick', 'denoms_chick',
                    'deaths_young', 'denoms_young',
                    'deaths_hen', 'denoms_hen',
                    'deaths_rooster', 'denoms_rooster',
                    'inflow', 'outflow',
                    'inflow_chicks', 'outflow_chicks',
                    'date')
for (unique_date in unique(antaravato_makira_ownership$date)) {
  sub <- antaravato_makira_ownership[which(antaravato_makira_ownership$date == unique_date),]
  if (length(unique(sub$Household.name)) != nrow(sub)) {
    stop('Error.')
  }
  deaths <- sub$Chicks.died.from.disease + sub$Young.chickens..died.from.disease + 
    sub$Hens.died.from.disease + sub$Roosters..died.from.disease
  denoms <- sub$Chicks.you.own.as.of.today + sub$Young.chickens.you.own.as.of.today +
    sub$Hens.you.own.as.of.today + sub$Roosters.you.own.as.of.today
  #hist(deaths / denoms, breaks=30)
  temp <- cbind(data.frame(deaths), data.frame(denoms),
                data.frame(sub$Chicks.died.from.disease), data.frame(sub$Chicks.you.own.as.of.today),
                data.frame(sub$Young.chickens..died.from.disease), data.frame(sub$Young.chickens.you.own.as.of.today),
                data.frame(sub$Hens.died.from.disease), data.frame(sub$Hens.you.own.as.of.today),
                data.frame(sub$Roosters..died.from.disease), data.frame(sub$Roosters.you.own.as.of.today),
                data.frame(rowSums(sub[,c(6:8, 
                                  15:17,
                                  24:26,
                                  33:35)], na.rm=T)), 
                data.frame(rowSums(sub[,c(9,11:12, 
                                          18, 20:21,
                                          27, 29:30,
                                          36, 38:39)], na.rm=T)),
                data.frame(rowSums(sub[,c(6:8)], na.rm=T)), data.frame(rowSums(sub[,c(9,11:12)], na.rm=T)))
                
  temp$date <- as.Date(unique_date, origin='1970-01-01')
  temp$name <- sub$Household.name
  hh_data <- rbind(hh_data, temp)
  temp2 <- cbind(data.frame(sum(deaths, na.rm=T)), data.frame(sum(denoms, na.rm=T)),
                 data.frame(sum(sub$Chicks.died.from.disease, na.rm=T)), data.frame(sum(sub$Chicks.you.own.as.of.today, na.rm=T)),
                 data.frame(sum(sub$Young.chickens..died.from.disease, na.rm=T)), data.frame(sum(sub$Young.chickens.you.own.as.of.today, na.rm=T)),
                 data.frame(sum(sub$Hens.died.from.disease, na.rm=T)), data.frame(sum(sub$Hens.you.own.as.of.today, na.rm=T)),
                 data.frame(sum(sub$Roosters..died.from.disease, na.rm=T)), data.frame(sum(sub$Roosters.you.own.as.of.today, na.rm=T)),
                 data.frame(sum(sub[,c(6:8, 
                                       15:17,
                                       24:26,
                                       33:35)], na.rm=T)),
                data.frame(sum(sub[,c(9,11:12, 
                                       18, 20:21,
                                       27, 29:30,
                                       36, 38:39)], na.rm=T)),
                 data.frame(sum(sub[,c(6:8)], na.rm=T)), data.frame(sum(sub[,c(9,11:12)], na.rm=T)),
                 as.Date(unique_date, origin='1970-01-01'))
  colnames(temp2) <- c('deaths', 'denoms', 
                       'deaths_chick', 'denoms_chick',
                       'deaths_young', 'denoms_young',
                       'deaths_hen', 'denoms_hen',
                       'deaths_rooster', 'denoms_rooster',
                       'inflow', 'outflow',
                       'inflow_chicks', 'outflow_chicks',
                       'date')
  temp2$date <- as.Date(unique_date, origin='1970-01-01')
  sums <- rbind(sums, temp2)
}
hh_data <- hh_data[order(hh_data$date),]
sums <- sums[order(sums$date),]

# Subset to before March 2016
hh_data <- hh_data[which(hh_data$date <= as.Date('2016-05-01')),]
sums <- sums[which(sums$date <= as.Date('2016-05-01')),]

# ------------------------------------------------------------------------------
# Part 2: Prepare vaccination data. Note that there is no disease death data in this dataset
# ------------------------------------------------------------------------------
# There are 14 columns - duplicate 'Notes', 'Village Name', 'Household names = 11 unique questions:
# unique(trimws(unname(as.vector(full_vax[1,]))))
full_vax_anta <- full_vax[which(full_vax$X == "Antaravato"),]
# View(full_vax_anta[which(duplicated(full_vax_anta$X.1)),]) # Looks like 'Rakoto Elois' and 'Miliette' are duplicated. Should first collapse these into two.
# View(full_vax_anta[which(full_vax_anta$X.1 %in% c('Rakoto Elois', 'Miliette')),])
full_vax_anta[which(full_vax_anta$X.1 == 'Rakoto Elois')[1],c(4:8, 41:45, 57:65)] <- full_vax_anta[which(full_vax_anta$X.1 == 'Rakoto Elois')[2],c(4:8, 41:45, 57:65)]
full_vax_anta[which(full_vax_anta$X.1 == 'Miliette')[1],c(22:26)] <- full_vax_anta[which(full_vax_anta$X.1 == 'Miliette')[2],c(22:26)]
full_vax_anta <- full_vax_anta[-c(which(full_vax_anta$X.1 == 'Rakoto Elois')[2], which(full_vax_anta$X.1 == 'Miliette')[2]),]
full_vax_anta <- full_vax_anta[,-c(39)] # Removed column with no information
hh_data_vax <- data.frame(matrix(ncol=8, nrow=0))
colnames(hh_data_vax) <- c('name', 'denoms', 'vax', 'no_vax_notwant', 
                           'no_vax_notlocate', 'MGA paid', 'Note', 'date')
# First baseline survey in March 2016
new_block <- data.frame(matrix(c(full_vax_anta$X.1, full_vax_anta$Mar.16, 
                               rep("", times=nrow(full_vax_anta) * 5)),
                             ncol=7, nrow=nrow(full_vax_anta), byrow=F))
colnames(new_block) <- c('name', 'denoms', 'vax', 'no_vax_notwant', 
                           'no_vax_notlocate', 'MGA paid', 'Note')
new_block$date <- as.Date('2016-03-01')
hh_data_vax <- rbind(hh_data_vax, new_block)
# This loop is until Jan 2018
dates1 <- colnames(full_vax_anta)[seq(4, 40, 6)]
dates1_real <- c(as.Date('2016-05-01'), as.Date('2016-09-01'), as.Date('2017-01-01'), 
                 as.Date('2017-05-01'), as.Date('2017-09-01'), as.Date('2018-01-01'))
dates1_dex <- seq(4, 34, 6)
cntr <- 1
for (begin_col in dates1_dex) {
  new_block <- data.frame(matrix(c(full_vax_anta$X.1,
                                   full_vax_anta[,begin_col], full_vax_anta[,begin_col+3],
                                   full_vax_anta[,begin_col+1], full_vax_anta[,begin_col+2],
                                   full_vax_anta[,begin_col+4], full_vax_anta[,begin_col+5]),
                                 ncol=7, nrow=nrow(full_vax_anta), byrow=F))
  colnames(new_block) <- c('name', 'denoms', 'vax', 'no_vax_notwant', 
                           'no_vax_notlocate', 'MGA paid', 'Note')
  new_block$date <- dates1_real[cntr]
  cntr <- cntr+1
  hh_data_vax <- rbind(hh_data_vax, new_block)
}
# June 2018 -- This seems to be when some moving had occurred
new_block <- data.frame(matrix(c(full_vax_anta$X.1, 
                                 full_vax_anta[,40], full_vax_anta[,40+3],
                                 full_vax_anta[,40+1], full_vax_anta[,40+2],
                                 full_vax_anta[,40+4], full_vax_anta[,40+5], full_vax_anta[,40+6]),
                               ncol=8, nrow=nrow(full_vax_anta), byrow=F))
colnames(new_block) <- c('name', 'denoms', 'vax', 'no_vax_notwant', 
                         'no_vax_notlocate', 'MGA paid', 'Moving', 'Note')
# For this one, there are some inconsistent notes in MGA paid, Moving, and Note that we will condense into one Note column
# which(new_block$Moving != '' & new_block$note == '') # this is 0 so we can first combine Moving into Note
unique(new_block$denoms[which(new_block$Moving == 'SURVEYED')])
unique(new_block$Note[which(new_block$Moving == 'SURVEYED')]) #These are fine to leave as is
unique(new_block$Note[which(new_block$Moving == 'MOVED')])
unique(new_block$Moving[which(new_block$Note == 'nifindra ')]) # All that have nifindra in the notes column are also listed as MOVED
new_block$Note[which(new_block$Note == 'nifindra ')] <- 'MOVED' # I liked MOVED better than nifindra so fix this
new_block$Note[which(new_block$Moving == 'REPEATED NAME')]
new_block$Note[which(new_block$Moving == 'REPEATED NAME')] <- paste0('REPEATED NAME ', new_block$Note[which(new_block$Moving == 'REPEATED NAME')])
new_block$Note[which(new_block$Moving == 'andasy')] # This is fine to eliminate since it has a more descriptive Note
new_block$Note[which(new_block$Moving == 'NO MONEY')]
new_block$Note[which(new_block$Moving == 'NO MONEY')] <- paste0('NO MONEY ', new_block$Note[which(new_block$Moving == 'NO MONEY')])
new_block$Note[which(new_block$Moving == 'DIED')]
new_block$Note[which(new_block$Moving == 'DIED')] <- 'DIED'
new_block$Moving <- NULL
new_block$Note[which(new_block$`MGA paid` %in% c("NOT SURVEYED", " NOT SURVEYED"))]
new_block$Note[which(new_block$`MGA paid` %in% c("NOT SURVEYED", " NOT SURVEYED"))] <- 'NOT SURVEYED'
new_block$Note[which(new_block$`MGA paid` %in% c("NOT PARTICIPATE BUT SURVEYED") & new_block$Note == '')]
new_block$Note[which(new_block$`MGA paid` %in% c("NOT PARTICIPATE BUT SURVEYED") & new_block$Note == '')] <- 'NOT PARTICIPATE BUT SURVEYED'
new_block$Note[which(new_block$`MGA paid` %in% c("NOT PARTICIPATE BUT SURVEYED"))]
new_block$Note[which(new_block$`MGA paid` %in% c("SURVEYED ", "SURVEYED", "SURVEYED BUT NOT PAID"))] # no additional information for Note
new_block$date <- as.Date('2018-06-01')
hh_data_vax <- rbind(hh_data_vax, new_block)
missing <- full_vax_anta$Dec.18 == ''
full_vax_anta$Dec.18[missing] <- ifelse(full_vax_anta$X.38[missing] == 'SURVEYED/NO CHICKEN', 0, full_vax_anta$Dec.18[missing])
full_vax_anta$April.19[missing] <- ifelse(full_vax_anta$X.38[missing] == 'SURVEYED/NO CHICKEN', 0, full_vax_anta$April.19[missing])
full_vax_anta$Aug.19[missing] <- ifelse(full_vax_anta$X.38[missing] == 'SURVEYED/NO CHICKEN', 0, full_vax_anta$Aug.19[missing])
full_vax_anta$Dec.2019[missing] <- ifelse(full_vax_anta$X.38[missing] == 'SURVEYED/NO CHICKEN', 0, full_vax_anta$Dec.2019[missing])
# December 2018 -- for the remaining dates, we actually have information about which age group chickens were vaccinated, which is useful
if (sum(as.numeric(full_vax_anta$X.45), na.rm=T) != sum(as.numeric(full_vax_anta$X.39), # Double check that this vax column is equal to the sum of the others
                                                        as.numeric(full_vax_anta$X.40), 
                                                        as.numeric(full_vax_anta$X.41), 
                                                        as.numeric(full_vax_anta$X.42), na.rm=T)) {
  stop('Error in column.')
}
new_block <- data.frame(matrix(c(full_vax_anta$X.1, 
                                 full_vax_anta$Dec.18, full_vax_anta$X.45,
                                 full_vax_anta$X.43, full_vax_anta$X.44,
                                 full_vax_anta$X.46, rep(NA, nrow(full_vax_anta)), 
                                 full_vax_anta$X.39, full_vax_anta$X.40, full_vax_anta$X.41, full_vax_anta$X.42),
                               ncol=11, nrow=nrow(full_vax_anta), byrow=F))
colnames(new_block) <- c('name', 'denoms', 'vax', 'no_vax_notwant', 
                         'no_vax_notlocate', 'MGA paid', 'Note', 
                         'v_chick', 'v_ychicken', 'v_hen', 'v_rooster')
new_block$date <- as.Date('2018-12-01')
hh_data_vax$v_chick <- NA
hh_data_vax$v_ychicken <- NA
hh_data_vax$v_hen <- NA
hh_data_vax$v_rooster <- NA
hh_data_vax <- rbind(hh_data_vax, new_block)
# April 2019
if (sum(as.numeric(full_vax_anta$X.53), na.rm=T) != sum(as.numeric(full_vax_anta$X.47), # Double check that this vax column is equal to the sum of the others
                                                        as.numeric(full_vax_anta$X.48), 
                                                        as.numeric(full_vax_anta$X.49), 
                                                        as.numeric(full_vax_anta$X.50), na.rm=T)) {
  stop('Error in column.')
}
new_block <- data.frame(matrix(c(full_vax_anta$X.1, 
                                 full_vax_anta$April.19, full_vax_anta$X.53,
                                 full_vax_anta$X.51, full_vax_anta$X.52,
                                 full_vax_anta$X.54, rep(NA, nrow(full_vax_anta)), 
                                 full_vax_anta$X.47, full_vax_anta$X.48, full_vax_anta$X.49, full_vax_anta$X.50),
                               ncol=11, nrow=nrow(full_vax_anta), byrow=F))
colnames(new_block) <- c('name', 'denoms', 'vax', 'no_vax_notwant', 
                         'no_vax_notlocate', 'MGA paid', 'Note', 
                         'v_chick', 'v_ychicken', 'v_hen', 'v_rooster')
new_block$date <- as.Date('2019-04-01')
hh_data_vax <- rbind(hh_data_vax, new_block)
# August 2019
if (sum(as.numeric(full_vax_anta$X.61), na.rm=T) != sum(as.numeric(full_vax_anta$X.55), # Double check that this vax column is equal to the sum of the others
                                                        as.numeric(full_vax_anta$X.56), 
                                                        as.numeric(full_vax_anta$X.57), 
                                                        as.numeric(full_vax_anta$X.58), na.rm=T)) {
  stop('Error in column.')
}
new_block <- data.frame(matrix(c(full_vax_anta$X.1, 
                                 full_vax_anta$Aug.19, full_vax_anta$X.61,
                                 full_vax_anta$X.59, full_vax_anta$X.60,
                                 full_vax_anta$X.62, full_vax_anta$X.63, full_vax_anta$X.64,
                                 full_vax_anta$X.55, full_vax_anta$X.56, full_vax_anta$X.57, full_vax_anta$X.58),
                               ncol=12, nrow=nrow(full_vax_anta), byrow=F))
colnames(new_block) <- c('name', 'denoms', 'vax', 'no_vax_notwant', 
                         'no_vax_notlocate', 'MGA paid', 'Moving', 'Note', 
                         'v_chick', 'v_ychicken', 'v_hen', 'v_rooster')
new_block$Note[which(new_block$Moving != '')]
new_block$Note[which(new_block$Moving %in% c('Mijono', 'Mijono '))] <- 'MOVED'
new_block$Note[which(new_block$Moving == "Tsisy olo")] <- 'NO ONE HOME'
new_block$Moving <- NULL
new_block$date <- as.Date('2019-08-01')
hh_data_vax <- rbind(hh_data_vax, new_block)
# December 2019
if (sum(as.numeric(full_vax_anta$X.71), na.rm=T) != sum(as.numeric(full_vax_anta$X.65), # Double check that this vax column is equal to the sum of the others
                                                        as.numeric(full_vax_anta$X.66), 
                                                        as.numeric(full_vax_anta$X.67), 
                                                        as.numeric(full_vax_anta$X.68), na.rm=T)) {
  stop('Error in column.')
}
new_block <- data.frame(matrix(c(full_vax_anta$X.1, 
                                 full_vax_anta$Dec.2019, full_vax_anta$X.71,
                                 full_vax_anta$X.69, full_vax_anta$X.70,
                                 full_vax_anta$X.72, full_vax_anta$X.73, full_vax_anta$X.74,
                                 full_vax_anta$X.65, full_vax_anta$X.66, full_vax_anta$X.67, full_vax_anta$X.68),
                               ncol=12, nrow=nrow(full_vax_anta), byrow=F))
colnames(new_block) <- c('name', 'denoms', 'vax', 'no_vax_notwant', 
                         'no_vax_notlocate', 'MGA paid', 'Moving', 'Note', 
                         'v_chick', 'v_ychicken', 'v_hen', 'v_rooster')
new_block$Moving <- NULL
new_block$date <- as.Date('2019-12-01')
hh_data_vax <- rbind(hh_data_vax, new_block)

# Further cleaning
hh_data_vax[,c(2:5, 9:12)] <- numeric_clean(hh_data_vax[,c(2:5, 9:12)])
hh_data_vax <- replace(hh_data_vax, hh_data_vax=='', NA)
which(hh_data_vax[,c(2:5, 9:12)] == '999')
# Some minor discrepancies that we ignore for now TODO: Fix minor discrepancies. Also check if 'eny' might mean more than 1 poultry in the makira? Check if it is a big problem
#View(hh_data_vax[which(hh_data_vax$denoms != (hh_data_vax$vax+hh_data_vax$no_vax_notwant+hh_data_vax$no_vax_notlocate)),])

# Condense Notes
hh_data_vax$newnote <- hh_data_vax$Note
unique(hh_data_vax$`MGA paid`[which(hh_data_vax$date != as.Date('2018-06-01'))]) # First check there is no other hidden information in the MGA column other than in June 2018
unique(hh_data_vax$Note)
# First take care of repeats, which have the substring of 'Vady' or 'REPEATED NAME'
# First 'Vady'
vadies <- data.frame(matrix(c(hh_data_vax$Note[which(grepl('vady', hh_data_vax$Note, ignore.case=T))],
                    hh_data_vax$name[which(grepl('vady', hh_data_vax$Note, ignore.case=T))]),
                  nrow=length(which(grepl('vady', hh_data_vax$Note, ignore.case=T))), ncol=2,
                  byrow=F))
colnames(vadies) <- c('Note', 'name')
vadies$new <- NA
vadies$new[1] <- 'Jaofera'
vadies$new[2] <- 'jaozandry'
vadies$new[3] <- 'Be Rolin'
vadies$new[4] <- 'Roger Charles'
vadies$new[5] <- 'Delord'
vadies$new[6] <- 'Felix'
vadies$new[7] <- 'Randrianarison Pany'
vadies$new[8] <- 'Roger Charles'
vadies$new[9] <- 'Jaoatody'
vadies$new[10] <- 'Jaofera'
vadies$new[11] <- 'Jerome'
vadies$new[12] <- 'Be Rolin'
vadies$new[13] <- 'Delord'
vadies$new[14] <- 'Berthine I'
vadies$new[15] <- 'Felix'
vadies$new[16] <- 'Rakoto Elois'
vadies$new[17] <- 'Roger Charles'
hh_data_vax$name_norep <- hh_data_vax$name
for (i in 1:nrow(vadies)) {
  hh_data_vax$name_norep[which(hh_data_vax$name == vadies$name[i])] <- vadies$new[i]
}
length(unique(hh_data_vax$name_norep))
hh_data_vax$newnote[which(grepl('vady', hh_data_vax$Note, ignore.case=T))] <- NA
# Next the remaining repeated names
hh_data_vax$name[which(grepl('REPEATED NAME', hh_data_vax$newnote, ignore.case=T))]
rep_names <- data.frame(matrix(c(hh_data_vax$Note[which(grepl('REPEATED NAME', hh_data_vax$newnote, ignore.case=T))],
                                 hh_data_vax$name[which(grepl('REPEATED NAME', hh_data_vax$newnote, ignore.case=T))]),
                               nrow=length(which(grepl('REPEATED NAME', hh_data_vax$newnote, ignore.case=T))), 
                               ncol=2, byrow=F))
colnames(rep_names) <- c('note', 'name')
rep_names$new <- NA
hh_data_vax$name[which(hh_data_vax$name == "Laurent*")] <- "Laurent"
hh_data_vax$name_norep[which(hh_data_vax$name_norep == "Laurent*")] <- "Laurent"
rep_names$new[1] <- 'Laurent'
rep_names$new[2] <- 'Rakoto Elois'
rep_names$new[3] <- 'jaozandry'
rep_names$new[4] <- 'Emilien'
rep_names$new[5] <- 'Eddy'
rep_names$new[6] <- 'Victor'
rep_names$new[7] <- 'Tany'
for (i in 1:nrow(rep_names)) {
  hh_data_vax$name_norep[which(hh_data_vax$name == rep_names$name[i])] <- rep_names$new[i]
}
length(unique(hh_data_vax$name_norep))
hh_data_vax$newnote[which(grepl('REPEATED NAME', hh_data_vax$Note, ignore.case=T))] <- NA
# Take care of annoying last few repeated names
hh_data_vax$name_norep[which(hh_data_vax$Note == "Listed twice = Laurent, Odilon Laurent")] <- 'Laurent'
hh_data_vax$newnote[which(hh_data_vax$Note == "Listed twice = Laurent, Odilon Laurent")] <- NA
hh_data_vax$name_norep[which(hh_data_vax$Note == "Also listed as Be Frances")] <- 'Be Francel'
hh_data_vax$newnote[which(hh_data_vax$Note == "Also listed as Be Frances")] <- NA
# Take care of those who moved, died (removed from study, or not part of study)
hh_data_vax$newnote[which(hh_data_vax$newnote %in% c('Moved, no longer a part of the study',
                                                     'Died, no longer a part of study',
                                                     'Andasy ', 'andasy', '20 andasy',
                                                     'Nifindra', 'Nifindra ', 'Andasy',
                                                     'Andasy jiaby ', 'nifindra', "MOVED",
                                                     "DIED"))] <- 'NOT PART OF STUDY'
# Take care of those who were not home or refused, or didn't know number (nonresponse)
hh_data_vax$newnote[which(hh_data_vax$newnote %in% c('(Home but refused)', 'Not home',
                                                     'Tsy manome isa', 'Ts nanome isa',
                                                     'Ts tratra ', 'Nijono', 'Mijono',
                                                     'Mijono ', 'Ts nanome Isa',
                                                     'Tsy tratra', 'Ts nanome isa ',
                                                     'ts nanome isa', 'mijono',
                                                     'Ts tratra', 'HAVING CHICKEN/ NUMBER UNKNOWN',
                                                     "NOT SURVEYED", "NO ONE HOME", "Tsy tratra ",
                                                     'NOT PARTICIPATE BUT SURVEYED'))] <- 'NONRESPONSE'
# Take care of those who are new
hh_data_vax$newnote[which(hh_data_vax$newnote %in% c("Vaovao ", "Vaovao", "vaovao ",
                                                     "NEW HOUSEHOLD PARTICIPATING"))] <- 'NEW PARTICIPANT'
# Take care of those not yet residents
hh_data_vax$newnote[which(hh_data_vax$newnote %in% c('Not yet a resident'))] <- 'NOT YET PART OF STUDY'
# Take care of those who said there was no need (either because of vaccine, or no poultry? Unclear, and they are two very different things)
hh_data_vax$newnote[which(hh_data_vax$newnote %in% c('Ts mila', "Ts mila ",
                                                     "Tsy mila vaccine", "Tsy mila",
                                                     "tsy mila", "tsy mila ", "Tsy mila "))] <- 'NOT NEEDED'
# Take care of those who said they did not have chickens in village
hh_data_vax$newnote[which(hh_data_vax$newnote %in% c('SURVEYED/NO CHICKEN', "",
                                                     ' HAVING CHICKEN BUT NOT IN THE VILLAGE'))] <- 'STATED NO CHICKEN'
# Take care of those unclear or miscellaneous
hh_data_vax$newnote[which(hh_data_vax$newnote == 'Rakoto Elois ')] <- NA
hh_data_vax$newnote[which(hh_data_vax$newnote %in% c('Very', "Very jiaby", 'Very jiaby ',
                                                     'CV gets her chicken vaccinated for free',
                                                     "MAHERY TEAM", 
                                                     'NO MONEY HUSBAND HAVING CANCER/ ALWAYS PARTICIPATED TO OUR PREVIOUS CAMPAIGN'))] <- 'UNCLEAR OR MISC'
unique(hh_data_vax$newnote)

# ------------------------------------------------------------------------------
# Part 3: Prepare follow-up data
# ------------------------------------------------------------------------------
#Jul18 -- Single note is that Alexis died
jul_18[,3:71] <- numeric_clean(jul_18[,3:71])
jul_18$Date[which(jul_18$Date == '2019-08-16')] <- as.Date('2018-08-16')
jul_18$Date[which(jul_18$Date == '2019-08-19')] <- as.Date('2018-08-19')
jul_18anta <- jul_18[which(jul_18$village == 'ANTARAVATO'),]
# Since all dates are in same period, re-classify as 2018-08-11
# Dates are "2018-08-11" "2018-08-12" "2018-08-13" "2018-08-14" "2018-08-15" "2018-08-16"
# "2018-08-17" "2018-08-18" "2018-08-19" "2018-08-19" and will be classified as 2018-08-11
aug_surv_dates <- c(as.Date("2018-08-11"), as.Date("2018-08-12"), as.Date("2018-08-13"), 
                    as.Date("2018-08-14"), as.Date("2018-08-15"), as.Date("2018-08-16"), 
                    as.Date("2018-08-17"), as.Date("2018-08-18"), as.Date("2018-08-19"), 
                    as.Date("2018-08-19"))
which(!is.na(jul_18anta[49,2:(ncol(jul_18anta) - 1)])) # So can eliminate row 49
jul_18anta <- jul_18anta[-c(49),]
hh_data_follow <- data.frame(matrix(c(jul_18anta$Household,
                                      jul_18anta$`TOTAL number of Chickens owned today`,
                                      jul_18anta$`Total number of chicks (baby chickens) owned today`,
                                      jul_18anta$`Total number of young chickens owned today`,
                                      jul_18anta$`Total number of hens owned today`,
                                      jul_18anta$`Total number of cocks owned today`,
                                      jul_18anta$`Total number of chickens who died from disease since our last interview`,
                                      jul_18anta$`N chicks vaccinated`, jul_18anta$`N young chickens vaccinated`,
                                      jul_18anta$`N hens vaccinated`, jul_18anta$`N cocks vaccinated`),
                                    nrow=nrow(jul_18anta), ncol=11))
colnames(hh_data_follow) <- c('name', 
                         'denoms', 'denoms_chick', 'denoms_ychicken', 'denoms_hen', 'denoms_rooster',
                         'deaths',
                         'v_chick', 'v_ychicken', 'v_hen', 'v_rooster')
hh_data_follow$date <- as.Date('2018-08-11')
#Jun19 # This actually includes November 2018 data as well it seems. No vaccination data here. No relevant notes so can ignore
jun_19[,3:69] <- numeric_clean(jun_19[,3:69])
jun_19$Date[which(jun_19$Date == '2018-06-09')] <- as.Date('2019-06-09')
jun_19$Date[which(jun_19$Date == '2017-11-28')] <- as.Date('2018-11-28')
jun_19anta <- jun_19[which(jun_19$village == 'ANTARAVATO'),]
# These dates will be reclassified:
# "2018-11-28" "2018-11-26" "2018-11-27" "2018-11-28" will be classified as 2018-11-28
nov_surv_dates <- c(as.Date("2018-11-28"), as.Date("2018-11-26"), as.Date("2018-11-27"), as.Date("2018-11-28"))
# "2019-02-03" "2019-02-04" "2019-02-05" "2019-02-06" "2019-02-07" "2019-02-08" "2019-02-09" will be classified as 2019-02-03
feb_surv_dates <- c(as.Date("2019-02-03"), as.Date("2019-02-04"), as.Date("2019-02-05"),
                    as.Date("2019-02-06"), as.Date("2019-02-07"), as.Date("2019-02-08"),
                    as.Date("2019-02-09"))
# "2019-06-04" "2019-06-05" "2019-06-06" "2019-06-07" "2019-06-08" "2019-06-09" "2019-06-10" will be classified as 2019-06-04
jun_surv_dates <- c(as.Date("2019-06-04"), as.Date("2019-06-05"), as.Date("2019-06-06"),
                    as.Date("2019-06-07"), as.Date("2019-06-08"), as.Date("2019-06-09"),
                    as.Date("2019-06-10"))
# First, take care of November 2018
# Double check that this datasheet is just the same block copy and pasted three times
any(jun_19anta$Household[1:189] != jun_19anta$Household[190:378])
any(jun_19anta$Household[190:378] != jun_19anta$Household[379:567])
dates_tocheck <- list(nov_surv_dates, feb_surv_dates, jun_surv_dates)
for (cntr in 1:3) {
  i <- ifelse(cntr == 1, 1, 
              ifelse(cntr == 2, 190, 379))
  sub_block <- jun_19anta[i:(i+188),]
  if (any(!sub_block$Date[which(!is.na(sub_block$Date))] %in% dates_tocheck[[cntr]])) {
    stop('Error in date.')
  }
  # Rakoto Elois is a problem in each block
  #which(duplicated(sub_block$Household))
  #which(sub_block$Household == 'Rakoto Elois')
  if (cntr == 1) {
    # which(!is.na(sub_block[124,2:(ncol(sub_block) - 1)])) # So can eliminate row 124
    # View(sub_block[which(sub_block$Household == 'Rakoto Elois'),])
    sub_block <- sub_block[-c(124),]
  } else {
    # which(!is.na(sub_block[49,2:(ncol(sub_block) - 1)])) # So can eliminate row 49
    # View(sub_block[which(sub_block$Household == 'Rakoto Elois'),])
    sub_block <- sub_block[-c(49),]
  }
  if (nrow(sub_block) != 188) {
    stop('Error in getting rid of row.')
  }

  new_block <- data.frame(matrix(c(sub_block$Household,
                                   sub_block$`TOTAL number of Chickens owned today`,
                                   sub_block$`Total number of chicks (baby chickens) owned today`,
                                   sub_block$`Total number of young chickens owned today`,
                                   sub_block$`Total number of hens owned today`,
                                   sub_block$`Total number of cocks owned today`,
                                   sub_block$`Total number of chickens who died from disease since our last interview`,
                                   rep(NA, nrow(sub_block) * 4)),
                                 nrow=nrow(sub_block), ncol=11))
  colnames(new_block) <- c('name', 
                           'denoms', 'denoms_chick', 'denoms_ychicken', 'denoms_hen', 'denoms_rooster',
                           'deaths',
                           'v_chick', 'v_ychicken', 'v_hen', 'v_rooster')
  if (cntr == 1) {
    new_block$date <- as.Date('2018-11-28')
  } else if (cntr == 2) {
    new_block$date <- as.Date('2019-02-03')
  } else {
    new_block$date <- as.Date('2019-06-04')
  }
  hh_data_follow <- rbind(hh_data_follow, new_block)
}
# Check for discrepancies in denominator numbers
hh_data_follow$denoms <- as.numeric(hh_data_follow$denoms)
hh_data_follow$denoms_chick <- as.numeric(hh_data_follow$denoms_chick)
hh_data_follow$denoms_ychicken <- as.numeric(hh_data_follow$denoms_ychicken)
hh_data_follow$denoms_hen <- as.numeric(hh_data_follow$denoms_hen)
hh_data_follow$denoms_rooster <- as.numeric(hh_data_follow$denoms_rooster)
hh_data_follow$deaths <- as.numeric(hh_data_follow$deaths)
hh_data_follow$v_chick <- as.numeric(hh_data_follow$v_chick)
hh_data_follow$v_ychicken <- as.numeric(hh_data_follow$v_ychicken)
hh_data_follow$v_hen <- as.numeric(hh_data_follow$v_hen)
hh_data_follow$v_rooster <- as.numeric(hh_data_follow$v_rooster)
#View(hh_data_follow[which(hh_data_follow$denoms != unname(rowSums(hh_data_follow[,3:6], na.rm=T))),])
# Check for discrepancies in number vaccinated and number observed # TODO: Ignore vaccination data for now, especially since we seem to have gold standard vaccination data otherwise
#View(hh_data_follow[which(hh_data_follow$denoms_chick < hh_data_follow$v_chick),])
#View(hh_data_follow[which(hh_data_follow$denoms_ychicken < hh_data_follow$v_ychicken),])
#View(hh_data_follow[which(hh_data_follow$denoms_hen < hh_data_follow$v_hen),])
#View(hh_data_follow[which(hh_data_follow$denoms_rooster < hh_data_follow$v_rooster),])
# If you look at the following you can see that vaccination and follow-up are staggered except
# 2018-11-28 and 2018-12-01 are somewhat close to one another.
unique(hh_data_follow$date)
unique(hh_data_vax$date)
# Take care of repetitions found for hh_data_vax
hh_data_follow$name_norep <- hh_data_follow$name
rep_names$name %in% unique(hh_data_follow$name)
rep_names$new %in% unique(hh_data_follow$name)
for (i in 1:nrow(rep_names)) {
  hh_data_follow$name_norep[which(hh_data_follow$name == rep_names$name[i])] <- rep_names$new[i]
}
vadies$new %in% unique(hh_data_follow$name)
vadies$name %in% unique(hh_data_follow$name)
for (i in 1:nrow(vadies)) {
  if (i == 14) {
    hh_data_follow$name_norep[which(hh_data_follow$name == vadies$name[i])] <- "Bertine I"
  } else {
    hh_data_follow$name_norep[which(hh_data_follow$name == vadies$name[i])] <- vadies$new[i]
  }
}

# ------------------------------------------------------------------------------
# Part 4: Try to map the names of pre-vaccination period to vaccination and follow-up, as well as vax and follow-up
# ------------------------------------------------------------------------------
pre_vax_follow_names <- data.frame(matrix(c(unique(hh_data$name), rep(NA, 2*length(unique(hh_data$name)))), 
                                        nrow=length(unique(hh_data$name)), ncol=3, byrow=F))
colnames(pre_vax_follow_names) <- c('pre', 'vax', 'follow')
hh_data_vax$name[which(hh_data_vax$name == "Zarason/")] <- 'Zarason'
hh_data_vax$name_norep[which(hh_data_vax$name_norep == "Zarason/")] <- 'Zarason'
pre_vax_follow_names$vax <- c("Emilienne", "Theogene", "Voavy Clovis", "Laurent",
                             "Frederick", "Prezelin/Scolastique", "Bealanana Charles", 'Zarason',
                             "Be Alphonse", "Gabriel", "Bezafy Romin", "Radison",
                             "Be Zackson", "Tsaravelo Justin", 'Roger Charles', "Elise",
                             "Be Ratana Justin Edme", "Alexis", "Rabezava", 'Robert',
                             'Victor', "Bezafy Romin", "Richard Edme")
hh_data_follow$name[which(hh_data_follow$name == "Laurent*")] <- 'Laurent'
hh_data_follow$name_norep[which(hh_data_follow$name == "Laurent*")] <- 'Laurent'
hh_data_follow$name[which(hh_data_follow$name == "Zarason/")] <- 'Zarason'
hh_data_follow$name_norep[which(hh_data_follow$name == "Zarason/")] <- 'Zarason'
pre_vax_follow_names$follow <- c("Emilienne", "Theogene", "Voavy Clovis", "Laurent",
                              "Frederick", "Prezelin/Scolastique", "Bealanana Charles", 'Zarason',
                              "Be Alphonse", "Gabriel", "Bezafy Romin", "Radison",
                              "Be Zackson", "Tsaravelo Justin", 'Roger Charles', "Elise",
                              "Be Ratana Justin Edme", "Alexis", "Rabezava", 'Robert',
                              'Victor', "Bezafy Romin", "Richard Edme")
pre_vax_follow_names$check <- pre_vax_follow_names$follow %in% unique(hh_data_follow$name)
pre_vax_follow_names$follow[17] <- NA
pre_vax_follow_names$check <- NULL

vax_follow_names <- data.frame(matrix(c(unique(hh_data_vax$name), unique(hh_data_vax$name)),
                                      nrow=length(unique(hh_data_vax$name)), ncol=2))
colnames(vax_follow_names) <- c('vax', 'follow')
vax_follow_names$check <- ifelse(vax_follow_names$vax %in% unique(hh_data_follow$name), NA, 'change')
vax_follow_names$follow[16] <- "Be manantoine"
vax_follow_names$follow[17] <- "Be misangotra" 
vax_follow_names$follow[21] <- "Be Zartin" 
vax_follow_names$follow[22] <- "Be Zelda" 
vax_follow_names$follow[26] <- "Be nosy Raphael" 
vax_follow_names$follow[27] <- "Bertine I" 
vax_follow_names$follow[28] <- "Bertine II"
vax_follow_names$follow[33] <- "Celestine Velonjara"
vax_follow_names$follow[111] <- "Noel"
vax_follow_names$follow[161] <- "Tsaramody"
vax_follow_names$follow[174] <- "Jocylin"
vax_follow_names$follow[175] <- "Vastelin"
vax_follow_names$follow[180] <- "Francine"
vax_follow_names$follow[181] <- "Norovelo" 
vax_follow_names$follow[182] <- "Razaka Gene"
vax_follow_names$follow[183] <- "Marge Luxe"
vax_follow_names$follow[189] <- "Martial" 
vax_follow_names$follow[190] <- NA
vax_follow_names$follow[191] <- NA
vax_follow_names$follow[192] <- "Otheline"
vax_follow_names$follow[193] <- NA
vax_follow_names$check <- NULL
any(!(vax_follow_names$follow[which(!is.na(vax_follow_names$follow))] %in% unique(hh_data_follow$name)))

# ------------------------------------------------------------------------------
# Part 5: Save objects for analysis
# ------------------------------------------------------------------------------
save(hh_data, file="~/vaxMada-data/objects/data_obj/hh_data.RData")
save(sums, file="~/vaxMada-data/objects/data_obj/sums.RData")
save(hh_data_vax, file="~/vaxMada-data/objects/data_obj/hh_data_vax.RData")
save(hh_data_follow, file="~/vaxMada-data/objects/data_obj/hh_data_follow.RData")
save(pre_vax_follow_names, file="~/vaxMada-data/objects/data_obj/pre_vax_follow_names.RData")


# ------------------------------------------------------------------------------
# Repeat for Ambatavola
# ------------------------------------------------------------------------------
source('~/vaxMada/code/helper_functions.R')
load("~/vaxMada-data/objects/data_obj/hh_data_vax.RData")
load("~/vaxMada-data/objects/data_obj/vax_nchicken.RData")
load("~/vaxMada-data/objects/data_obj/vax_dates.RData")
load("~/vaxMada-data/objects/data_obj/makira_ownership.RData")
new_dates <- paste0(makira_ownership$Year, "_", makira_ownership$Month)
makira_ownership$date <- sapply(new_dates, function(x) switch_date(x))
makira_ownership$date <- as.Date(makira_ownership$date, origin='1970-01-01')
makira_ownership[,5:40] <- numeric_clean(makira_ownership[,5:40])
amba_makira_ownership <- makira_ownership[which(makira_ownership$Town == 'Ambatovola' |
                                                  makira_ownership$Town == 'AMBATOVOLO'),]
amba_makira_ownership[,41:66] <- NULL # All NA or '' except one unclear deleted comment in 66
amba_makira_ownership$Household.name[which(amba_makira_ownership$Household.name == 'algence Rifin')] <- 'alegence rufin'
amba_makira_ownership$Household.name[which(amba_makira_ownership$Household.name == 'BE Eugene' |
                                             amba_makira_ownership$Household.name == 'BE EUGENE')] <- 'Be Eugene'
amba_makira_ownership$Household.name[which(amba_makira_ownership$Household.name == 'beka')] <- 'Beka'
amba_makira_ownership$Household.name[which(amba_makira_ownership$Household.name == 'Fredin david' | 
                                             amba_makira_ownership$Household.name == 'FREDIN DAVID')] <- 'Fredin David'
amba_makira_ownership$Household.name[which(amba_makira_ownership$Household.name == 'Ranaivoson' | 
                                             amba_makira_ownership$Household.name == 'RANAIVOSON CAUNIEL')] <- 'Ranaivoson Cauniel'
amba_makira_ownership$Household.name[which(amba_makira_ownership$Household.name == 'Randrimahasoa ' | 
                                             amba_makira_ownership$Household.name == 'randriamahasoa beka' |
                                             amba_makira_ownership$Household.name == 'Randriamahasoa Beka' |
                                             amba_makira_ownership$Household.name == 'RANDRIAMAHASOA BEKA')] <- 'Randriamahasoa'
amba_makira_ownership$Household.name[which(amba_makira_ownership$Household.name == 'rifin')] <- 'Rifin'
amba_makira_ownership$Household.name[which(amba_makira_ownership$Household.name == 'sanadette' |
                                             amba_makira_ownership$Household.name == 'Soanadette' |
                                             amba_makira_ownership$Household.name == 'Sanadette')] <- 'Sonadette'
amba_makira_ownership$Household.name[which(amba_makira_ownership$Household.name == 'Saomandine' |
                                             amba_makira_ownership$Household.name == 'Soamandiny')] <- 'Soamandine'
amba_makira_ownership$Household.name[which(amba_makira_ownership$Household.name == 'tabavimena')] <- 'Tobavimena'
amba_makira_ownership$Household.name[which(amba_makira_ownership$Household.name == 'VIERCE')] <- 'Vierce'
cnt_mult <- 0
for (unique_date in unique(amba_makira_ownership$date)) {
  sub <- amba_makira_ownership[which(amba_makira_ownership$date == unique_date),]
  if (length(unique(sub$Household.name)) != nrow(sub)) {
    for (unique_hh in unique(sub$Household.name)) {
      sub2 <- sub[which(sub$Household.name == unique_hh),]
      cnt_mult <- cnt_mult + nrow(sub2) - 1
      for (j in 1:ncol(sub2)) {
        if (length(unique(sub2[,j])) > 1) {
          stop('Error.')
        }
      }
    }
  }
}
amba_makira_ownership <- amba_makira_ownership[which(!duplicated(amba_makira_ownership)),]
hh_data <- data.frame(matrix(ncol=12, nrow=0))
colnames(hh_data) <- c('deaths', 'denoms', 
                       'deaths_chick', 'denoms_chick',
                       'deaths_young', 'denoms_young',
                       'deaths_hen', 'denoms_hen',
                       'deaths_rooster', 'denoms_rooster',
                       'date', 'name')
sums <- data.frame(matrix(ncol=11, nrow=0))
colnames(sums) <- c('deaths', 'denoms', 
                    'deaths_chick', 'denoms_chick',
                    'deaths_young', 'denoms_young',
                    'deaths_hen', 'denoms_hen',
                    'deaths_rooster', 'denoms_rooster',
                    'date')
for (unique_date in unique(amba_makira_ownership$date)) {
  sub <- amba_makira_ownership[which(amba_makira_ownership$date == unique_date),]
  if (length(unique(sub$Household.name)) != nrow(sub)) {
    stop('Error.')
  }
  deaths <- sub$Chicks.died.from.disease + sub$Young.chickens..died.from.disease + 
    sub$Hens.died.from.disease + sub$Roosters..died.from.disease
  denoms <- sub$Chicks.you.own.as.of.today + sub$Young.chickens.you.own.as.of.today +
    sub$Hens.you.own.as.of.today + sub$Roosters.you.own.as.of.today
  #hist(deaths / denoms, breaks=30)
  temp <- cbind(data.frame(deaths), data.frame(denoms),
                data.frame(sub$Chicks.died.from.disease), data.frame(sub$Chicks.you.own.as.of.today),
                data.frame(sub$Young.chickens..died.from.disease), data.frame(sub$Young.chickens.you.own.as.of.today),
                data.frame(sub$Hens.died.from.disease), data.frame(sub$Hens.you.own.as.of.today),
                data.frame(sub$Roosters..died.from.disease), data.frame(sub$Roosters.you.own.as.of.today))
  temp$date <- as.Date(unique_date, origin='1970-01-01')
  temp$name <- sub$Household.name
  hh_data <- rbind(hh_data, temp)
  temp2 <- cbind(data.frame(sum(deaths, na.rm=T)), data.frame(sum(denoms, na.rm=T)),
                 data.frame(sum(sub$Chicks.died.from.disease, na.rm=T)), data.frame(sum(sub$Chicks.you.own.as.of.today, na.rm=T)),
                 data.frame(sum(sub$Young.chickens..died.from.disease, na.rm=T)), data.frame(sum(sub$Young.chickens.you.own.as.of.today, na.rm=T)),
                 data.frame(sum(sub$Hens.died.from.disease, na.rm=T)), data.frame(sum(sub$Hens.you.own.as.of.today, na.rm=T)),
                 data.frame(sum(sub$Roosters..died.from.disease, na.rm=T)), data.frame(sum(sub$Roosters.you.own.as.of.today, na.rm=T)),
                 as.Date(unique_date, origin='1970-01-01'))
  colnames(temp2) <- c('deaths', 'denoms', 
                       'deaths_chick', 'denoms_chick',
                       'deaths_young', 'denoms_young',
                       'deaths_hen', 'denoms_hen',
                       'deaths_rooster', 'denoms_rooster',
                       'date')
  temp2$date <- as.Date(unique_date, origin='1970-01-01')
  sums <- rbind(sums, temp2)
}
hh_data <- hh_data[order(hh_data$date),]
sums <- sums[order(sums$date),]

# Subset to before March 2016
hh_data <- hh_data[which(hh_data$date <= as.Date('2016-05-01')),]
sums <- sums[which(sums$date <= as.Date('2016-05-01')),]

save(hh_data, file="~/vaxMada-data/objects/data_obj/amba/hh_data.RData")
save(sums, file="~/vaxMada-data/objects/data_obj/amba/sums.RData")


# ------------------------------------------------------------------------------
# Repeat for Sahamborono
# ------------------------------------------------------------------------------

source('~/vaxMada/code/helper_functions.R')
load("~/vaxMada-data/objects/data_obj/hh_data_vax.RData")
load("~/vaxMada-data/objects/data_obj/vax_nchicken.RData")
load("~/vaxMada-data/objects/data_obj/vax_dates.RData")
load("~/vaxMada-data/objects/data_obj/makira_ownership.RData")

new_dates <- paste0(makira_ownership$Year, "_", makira_ownership$Month)
makira_ownership$date <- sapply(new_dates, function(x) switch_date(x))
makira_ownership$date <- as.Date(makira_ownership$date, origin='1970-01-01')
makira_ownership[,5:40] <- numeric_clean(makira_ownership[,5:40])
saha_makira_ownership <- makira_ownership[which(makira_ownership$Town == 'Sahamborono' |
                                                  makira_ownership$Town == 'Sahamborona'),]
saha_makira_ownership[,41:66] <- NULL # All NA or ''
saha_makira_ownership$Household.name[which(saha_makira_ownership$Household.name == 'Fabiene')] <- 'Fabien'
saha_makira_ownership$Household.name[which(saha_makira_ownership$Household.name == 'ORLNINE' |
                                       saha_makira_ownership$Household.name == 'Oroline')] <- 'Oreline'
saha_makira_ownership$Household.name[which(saha_makira_ownership$Household.name == 'Ortance')] <- 'Orthence'
saha_makira_ownership$Household.name[which(saha_makira_ownership$Household.name == 'randriamahasoa' |
                                       saha_makira_ownership$Household.name == 'Randriamahasoa' |
                                       saha_makira_ownership$Household.name == 'Randriamasoa' |
                                       saha_makira_ownership$Household.name == 'randriamiadana')] <- 'Randraiamahasoa'
saha_makira_ownership$Household.name[which(saha_makira_ownership$Household.name == 'rasolo')] <- 'Rasolo'
saha_makira_ownership$Household.name[which(saha_makira_ownership$Household.name == 'Ravao  Estine')] <- 'Ravao Estine'
saha_makira_ownership$Household.name[which(saha_makira_ownership$Household.name == 'ravaoline')] <- 'Ravaoline'
saha_makira_ownership$Household.name[which(saha_makira_ownership$Household.name == 'Riva' |
                                       saha_makira_ownership$Household.name == 'salomone' |
                                       saha_makira_ownership$Household.name == 'Solomona')] <- 'Riva Solome'
saha_makira_ownership$Household.name[which(saha_makira_ownership$Household.name == 'Toma Justin')] <- 'Thomas justin'
saha_makira_ownership$Household.name[which(saha_makira_ownership$Household.name == 'Zackaria' |
                                       saha_makira_ownership$Household.name == 'zakaria')] <- 'Zakaria'
saha_makira_ownership$Household.name[which(saha_makira_ownership$Household.name == 'Valavo')] <- 'Valavo jean'

cnt_mult <- 0
for (unique_date in unique(saha_makira_ownership$date)) {
  sub <- saha_makira_ownership[which(saha_makira_ownership$date == unique_date),]
  if (length(unique(sub$Household.name)) != nrow(sub)) {
    for (unique_hh in unique(sub$Household.name)) {
      sub2 <- sub[which(sub$Household.name == unique_hh),]
      cnt_mult <- cnt_mult + nrow(sub2) - 1
      for (j in 1:ncol(sub2)) {
        if (length(unique(sub2[,j])) > 1) {
          stop('Error.')
        }
      }
    }
  }
}
saha_makira_ownership <- saha_makira_ownership[which(!duplicated(saha_makira_ownership)),]
hh_data <- data.frame(matrix(ncol=12, nrow=0))
colnames(hh_data) <- c('deaths', 'denoms', 
                       'deaths_chick', 'denoms_chick',
                       'deaths_young', 'denoms_young',
                       'deaths_hen', 'denoms_hen',
                       'deaths_rooster', 'denoms_rooster',
                       'date', 'name')
sums <- data.frame(matrix(ncol=11, nrow=0))
colnames(sums) <- c('deaths', 'denoms', 
                    'deaths_chick', 'denoms_chick',
                    'deaths_young', 'denoms_young',
                    'deaths_hen', 'denoms_hen',
                    'deaths_rooster', 'denoms_rooster',
                    'date')
for (unique_date in unique(saha_makira_ownership$date)) {
  sub <- saha_makira_ownership[which(saha_makira_ownership$date == unique_date),]
  if (length(unique(sub$Household.name)) != nrow(sub)) {
    stop('Error.')
  }
  deaths <- sub$Chicks.died.from.disease + sub$Young.chickens..died.from.disease + 
    sub$Hens.died.from.disease + sub$Roosters..died.from.disease
  denoms <- sub$Chicks.you.own.as.of.today + sub$Young.chickens.you.own.as.of.today +
    sub$Hens.you.own.as.of.today + sub$Roosters.you.own.as.of.today
  #hist(deaths / denoms, breaks=30)
  temp <- cbind(data.frame(deaths), data.frame(denoms),
                data.frame(sub$Chicks.died.from.disease), data.frame(sub$Chicks.you.own.as.of.today),
                data.frame(sub$Young.chickens..died.from.disease), data.frame(sub$Young.chickens.you.own.as.of.today),
                data.frame(sub$Hens.died.from.disease), data.frame(sub$Hens.you.own.as.of.today),
                data.frame(sub$Roosters..died.from.disease), data.frame(sub$Roosters.you.own.as.of.today))
  temp$date <- as.Date(unique_date, origin='1970-01-01')
  temp$name <- sub$Household.name
  hh_data <- rbind(hh_data, temp)
  temp2 <- cbind(data.frame(sum(deaths, na.rm=T)), data.frame(sum(denoms, na.rm=T)),
                 data.frame(sum(sub$Chicks.died.from.disease, na.rm=T)), data.frame(sum(sub$Chicks.you.own.as.of.today, na.rm=T)),
                 data.frame(sum(sub$Young.chickens..died.from.disease, na.rm=T)), data.frame(sum(sub$Young.chickens.you.own.as.of.today, na.rm=T)),
                 data.frame(sum(sub$Hens.died.from.disease, na.rm=T)), data.frame(sum(sub$Hens.you.own.as.of.today, na.rm=T)),
                 data.frame(sum(sub$Roosters..died.from.disease, na.rm=T)), data.frame(sum(sub$Roosters.you.own.as.of.today, na.rm=T)),
                 as.Date(unique_date, origin='1970-01-01'))
  colnames(temp2) <- c('deaths', 'denoms', 
                       'deaths_chick', 'denoms_chick',
                       'deaths_young', 'denoms_young',
                       'deaths_hen', 'denoms_hen',
                       'deaths_rooster', 'denoms_rooster',
                       'date')
  temp2$date <- as.Date(unique_date, origin='1970-01-01')
  sums <- rbind(sums, temp2)
}
hh_data <- hh_data[order(hh_data$date),]
sums <- sums[order(sums$date),]

# Subset to before March 2016
hh_data <- hh_data[which(hh_data$date <= as.Date('2016-05-01')),]
sums <- sums[which(sums$date <= as.Date('2016-05-01')),]

save(hh_data, file="~/vaxMada-data/objects/data_obj/saha/hh_data.RData")
save(sums, file="~/vaxMada-data/objects/data_obj/saha/sums.RData")

