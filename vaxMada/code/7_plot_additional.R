# Overall statistics for all three villages
overall_rt <- c()
overall_r0 <- c()
overall_eta <- c()
overall_eta_best <- c()
load(file=paste0("~/vaxMada-data/objects/data_obj/esims.RData"))
overall_rt <- c(overall_rt, sims$rt)
overall_r0 <- c(overall_r0, sims$r0)
eta_dist_zero <- ifelse(sims$eta < 0, 0, sims$eta)
overall_eta <- c(overall_eta, eta_dist_zero)
overall_eta_best <- c(overall_eta_best, ifelse(sims$eta_best < 0, 0, sims$eta_best))
load(file=paste0("~/vaxMada-data/objects/data_obj/esims_vill2.RData"))
overall_rt <- c(overall_rt, sims$rt)
overall_r0 <- c(overall_r0, sims$r0)
eta_dist_zero <- ifelse(sims$eta < 0, 0, sims$eta)
overall_eta <- c(overall_eta, eta_dist_zero)
overall_eta_best <- c(overall_eta_best, ifelse(sims$eta_best < 0, 0, sims$eta_best))
load(file=paste0("~/vaxMada-data/objects/data_obj/esims_vill3.RData"))
overall_rt <- c(overall_rt, sims$rt)
overall_r0 <- c(overall_r0, sims$r0)
eta_dist_zero <- ifelse(sims$eta < 0, 0, sims$eta)
overall_eta <- c(overall_eta, eta_dist_zero)
overall_eta_best <- c(overall_eta_best, ifelse(sims$eta_best < 0, 0, sims$eta_best))
mean(overall_rt, na.rm=T)
sd(overall_rt, na.rm=T)
mean(overall_r0, na.rm=T)
sd(overall_r0, na.rm=T)
mean(overall_eta, na.rm=T)
sd(overall_eta, na.rm=T)
mean(overall_eta_best, na.rm=T)
sd(overall_eta_best, na.rm=T)

# Log-likelihood beta plots (pre-vaccination period)
load(file=paste0("~/vaxMada-data/objects/data_obj/p_beta.RData"))
load(file="~/vaxMada-data/objects/data_obj/mcap_res_beta.RData")
best_beta <- p_beta$Beta[which.max(p_beta$loglik)]
p1 <- ggplot(p_beta, aes(x=Beta, y=loglik)) + xlab('β') + ylab('log likelihood') + 
  ggtitle('Village 1') + geom_vline(xintercept=mcap_res_beta$ci, color='red', linetype='dashed') +
  geom_point(size=0.8) + theme_bw(base_size = 18) + geom_vline(xintercept=best_beta, color='blue')
load(file=paste0("~/vaxMada-data/objects/data_obj/p_beta_vill2.RData"))
load(file="~/vaxMada-data/objects/data_obj/mcap_res_beta_vill2.RData")
best_beta <- p_beta$Beta[which.max(p_beta$loglik)]
p2 <- ggplot(p_beta, aes(x=Beta, y=loglik)) + xlab('β') + ylab('log likelihood') + 
  ggtitle('Village 2') + geom_vline(xintercept=mcap_res_beta$ci, color='red', linetype='dashed') +
  geom_point(size=0.8) + theme_bw(base_size = 18) + geom_vline(xintercept=best_beta, color='blue')
load(file=paste0("~/vaxMada-data/objects/data_obj/p_beta_vill3.RData"))
load(file="~/vaxMada-data/objects/data_obj/mcap_res_beta_vill3.RData")
best_beta <- p_beta$Beta[which.max(p_beta$loglik)]
p3 <- ggplot(p_beta, aes(x=Beta, y=loglik)) + xlab('β') + ylab('log likelihood') + 
  ggtitle('Village 3') + geom_vline(xintercept=mcap_res_beta$ci, color='red', linetype='dashed') +
  geom_point(size=0.8) + theme_bw(base_size = 18) + geom_vline(xintercept=best_beta, color='blue')
png(filename="~/vaxMada-data/plots/loglik_beta.png", 
    width = 15,
    height = 4,
    units = "in",
    res = 400)
plot_grid(p1, p2, p3, align = "h", nrow=1, rel_widths = c(1/3, 1/3, 1/3))
dev.off()

# Rt plots (pre-vaccination period)
load(file=paste0("~/vaxMada-data/objects/data_obj/sims_confint.RData"))
sims_confint <- sims_confint[which(sims_confint$date < as.Date('2016-05-01')),]
sims_confint$date <- as.Date(sims_confint$date)
sims_confint$month <- format(sims_confint$date, "%m")
r1 <- ggplot(data = sims_confint, aes(x = date, y = rt_means)) +
  geom_ribbon(aes(ymin = rt_lower, ymax = rt_upper), alpha = 0.5) +
  geom_line() + theme_bw(base_size = 18) + xlab('Date') + ylab(expression(R[t])) +
  geom_hline(yintercept=1, lty='dashed') + ggtitle('Village 1')
sims_confint$season <- ifelse((sims_confint$month == '12' | sims_confint$month == '01' | 
                               sims_confint$month == '02'), 'Winter', 
                            ifelse((sims_confint$month == '03' | sims_confint$month == '04' | 
                                      sims_confint$month == '05'), 'Spring',
                                   ifelse((sims_confint$month == '06' | sims_confint$month == '07' | 
                                             sims_confint$month == '08'), 'Summer', 'Fall')))
sims_confint$season_factor <- factor(sims_confint$season, levels=c("Winter", "Spring", "Summer", "Fall"))
r1s <- ggplot(data = sims_confint, aes(x=season_factor, y=rt_means)) +
  geom_boxplot() + theme_bw(base_size=18) + xlab('Season') + ylab(expression(R[t])) #+ geom_hline(yintercept=1, lty='dashed')
r1m <- ggplot(data = sims_confint, aes(x=month, y=rt_means)) +
  geom_boxplot() + theme_bw(base_size=18) + xlab('Month') + ylab(expression(R[t]))
load(file=paste0("~/vaxMada-data/objects/data_obj/sims_confint_vill2.RData"))
sims_confint$date <- as.Date(sims_confint$date)
sims_confint$month <- format(sims_confint$date, "%m")
r2 <- ggplot(data = sims_confint, aes(x = date, y = rt_means)) +
  geom_ribbon(aes(ymin = rt_lower, ymax = rt_upper), alpha = 0.5) +
  geom_line() + theme_bw(base_size = 18) + xlab('Date') + ylab(expression(R[t])) +
  geom_hline(yintercept=1, lty='dashed')  + ggtitle('Village 2')
sims_confint$season <- ifelse((sims_confint$month == '12' | sims_confint$month == '01' | 
                               sims_confint$month == '02'), 'Winter', 
                            ifelse((sims_confint$month == '03' | sims_confint$month == '04' | 
                                      sims_confint$month == '05'), 'Spring',
                                   ifelse((sims_confint$month == '06' | sims_confint$month == '07' | 
                                             sims_confint$month == '08'), 'Summer', 'Fall')))
sims_confint$season_factor <- factor(sims_confint$season, levels=c("Winter", "Spring", "Summer", "Fall"))
r2s <- ggplot(data = sims_confint, aes(x=season_factor, y=rt_means)) +
  geom_boxplot() + theme_bw(base_size=18) + xlab('Season') + ylab(expression(R[t])) #+ geom_hline(yintercept=1, lty='dashed')
r2m <- ggplot(data = sims_confint, aes(x=month, y=rt_means)) +
  geom_boxplot() + theme_bw(base_size=18) + xlab('Month') + ylab(expression(R[t]))
load(file=paste0("~/vaxMada-data/objects/data_obj/sims_confint_vill3.RData"))
sims_confint$date <- as.Date(sims_confint$date)
sims_confint$month <- format(sims_confint$date, "%m")
r3 <- ggplot(data = sims_confint, aes(x = date, y = rt_means)) +
  geom_ribbon(aes(ymin = rt_lower, ymax = rt_upper), alpha = 0.5) +
  geom_line() + theme_bw(base_size = 18) + xlab('Date') + ylab(expression(R[t])) +
  geom_hline(yintercept=1, lty='dashed') + ggtitle('Village 3')
sims_confint$season <- ifelse((sims_confint$month == '12' | sims_confint$month == '01' | 
                               sims_confint$month == '02'), 'Winter', 
                            ifelse((sims_confint$month == '03' | sims_confint$month == '04' | 
                                      sims_confint$month == '05'), 'Spring',
                                   ifelse((sims_confint$month == '06' | sims_confint$month == '07' | 
                                             sims_confint$month == '08'), 'Summer', 'Fall')))
sims_confint$season_factor <- factor(sims_confint$season, levels=c("Winter", "Spring", "Summer", "Fall"))
r3s <- ggplot(data = sims_confint, aes(x=season_factor, y=rt_means)) +
  geom_boxplot() + theme_bw(base_size=18) + xlab('Season') + ylab(expression(R[t])) #+ geom_hline(yintercept=1, lty='dashed')
r3m <- ggplot(data = sims_confint, aes(x=month, y=rt_means)) +
  geom_boxplot() + theme_bw(base_size=18) + xlab('Month') + ylab(expression(R[t]))
png(filename="~/vaxMada-data/plots/rt_season.png", 
    width = 16,
    height = 12,
    units = "in",
    res = 400)
plot_grid(r1, r2, r3, r1s, r2s, r3s, r1m, r2m, r3m, align = "h", nrow=3, rel_widths = c(1/3, 1/3, 1/3))
dev.off()

# Comparison plots (pre-vaccination period)
load(file=paste0("~/vaxMada-data/objects/data_obj/sims_confint.RData"))
sims_confint <- sims_confint[which(sims_confint$date < as.Date('2016-05-01')),]
c1 <- ggplot(data = sims_confint, aes(x = date, y = reports)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  geom_line(aes(color='B')) + theme_bw(base_size=18)+
  theme(
    panel.grid.major = element_blank(), # Removes major grid lines
    panel.grid.minor = element_blank()  # Removes minor grid lines
  )+xlab('')+ggtitle("Village 1") + ylab('Reported cases') +
  scale_x_date(
    limits = c(min(as.Date(sims_confint$date)), max(as.Date(sims_confint$date))), # Set limits from earliest to latest date
    date_breaks = "1 year", # Example: Show breaks every month
    date_labels = "%Y" # Example: Format as "Jan 2023"
  )+
  geom_line(aes(x=date,y=means, color='A')) +
  labs(color = "") + scale_color_manual(
    values = c("A" = "black", "B" = "blue"),
    labels = c("Simulation.", 'Data')
  ) + theme(
    legend.position = c(0.7, 0.85),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10), 
    legend.key.size = unit(0.5, "cm")
  )   + guides(color = guide_legend(direction = "horizontal", nrow = 1))
load(file=paste0("~/vaxMada-data/objects/data_obj/sims_confint_vill2.RData"))
c2 <- ggplot(data = sims_confint, aes(x = date, y = reports)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  geom_line(aes(color='B')) + theme_bw(base_size=18)+
  theme(
    panel.grid.major = element_blank(), # Removes major grid lines
    panel.grid.minor = element_blank()  # Removes minor grid lines
  )+xlab('')+ggtitle("Village 2") + ylab('Reported cases') +
  scale_x_date(
    limits = c(min(as.Date(sims_confint$date)), max(as.Date(sims_confint$date))), # Set limits from earliest to latest date
    date_breaks = "1 year", # Example: Show breaks every month
    date_labels = "%Y" # Example: Format as "Jan 2023"
  )+
  geom_line(aes(x=date,y=means, color='A')) +
  labs(color = "") + scale_color_manual(
    values = c("A" = "black", "B" = "blue"),
    labels = c("Simulation.", 'Data')
  ) + theme(
    legend.position = c(0.7, 0.85),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10), 
    legend.key.size = unit(0.5, "cm")
  )   + guides(color = guide_legend(direction = "horizontal", nrow = 1))
load(file=paste0("~/vaxMada-data/objects/data_obj/sims_confint_vill3.RData"))
c3 <- ggplot(data = sims_confint, aes(x = date, y = reports)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  geom_line(aes(color='B')) + theme_bw(base_size=18)+
  theme(
    panel.grid.major = element_blank(), # Removes major grid lines
    panel.grid.minor = element_blank()  # Removes minor grid lines
  )+xlab('')+ggtitle("Village 3") + ylab('Reported cases') +
  scale_x_date(
    limits = c(min(as.Date(sims_confint$date)), max(as.Date(sims_confint$date))), # Set limits from earliest to latest date
    date_breaks = "1 year", # Example: Show breaks every month
    date_labels = "%Y" # Example: Format as "Jan 2023"
  )+
  geom_line(aes(x=date,y=means, color='A')) +
  labs(color = "") + scale_color_manual(
    values = c("A" = "black", "B" = "blue"),
    labels = c("Simulation.", 'Data')
  ) + theme(
    legend.position = c(0.7, 0.85),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10), 
    legend.key.size = unit(0.5, "cm")
  )   + guides(color = guide_legend(direction = "horizontal", nrow = 1))
png(filename="~/vaxMada-data/plots/comparison.png", 
    width = 15,
    height = 4,
    units = "in",
    res = 400)
plot_grid(c1, c2, c3, align = "h", nrow=1, rel_widths = c(1/3, 1/3, 1/3))
dev.off()

# Log-likelihood psi plot (vaccination period)
load(file=paste0("~/vaxMada-data/objects/data_obj/p_psi.RData"))
load(file=paste0("~/vaxMada-data/objects/data_obj/mcap_res_psi.RData"))
png(filename="~/vaxMada-data/plots/ll_psi.png", 
    width = 4,
    height = 4,
    units = "in",
    res = 400)
plot(p_psi$psi, p_psi$loglik, pch=16, ylab='log likelihood', xlab='Ψ', main='Village 1')
loess_model <- loess(p_psi$loglik ~ p_psi$psi, span = 0.75)
lines(p_psi$psi, predict(loess_model), col='grey', lwd=3)
1 - rev(mcap_res_psi$ci) # ci VE
mle_psi <- p_psi$psi[which.max(p_psi$loglik)]
mle_VE <- 1 - mle_psi 
mle_VE # mle VE
abline(v=mcap_res_psi$ci, col='red', lty='dashed', lwd=2)
best_psi <- mle_psi
abline(v=best_psi, col='blue', lwd=2)
dev.off()

# Example of herd immunity effects (vaccination period)
load(file="~/vaxMada-data/objects/data_obj/HI_confints.RData")
colors <- c("0.3" = "red", "0.5" = "orange", "0.7" = "yellow", "0.87 (MLE)" = "black")
png(filename="~/vaxMada-data/plots/HI.png", 
    width = 8,
    height = 4,
    units = "in",
    res = 400)
ggplot(data = HI_confints[[2]], aes(x = as.Date(date), y = means, color='0.5')) +
  geom_line(lwd=1)+
  #geom_line(data = HI_confints[[1]], aes(x=date, y=medians, color='0.3'),  lwd=1)+
  geom_line(data = HI_confints[[3]], aes(x=date, y=means, color='0.7'), lwd=1)+
  geom_line(data = HI_confints[[4]], aes(x=date, y=means, color='0.87 (MLE)'), lwd=1)+
  geom_line(data = HI_confints[[1]][which(HI_confints[[1]]$.id == 'data'),], aes(x=date, y=dt_inter),  color='blue', lwd=1)+
  labs(x = "Date",
       y = "Mean reported deaths",
       color = "Vax. Eff.") +
  scale_color_manual(values = colors)+
  scale_x_date() + theme_bw(base_size=18)
dev.off()

# R0 estimate and eta over time
load(paste0("~/vaxMada-data/objects/data_obj/esims_confint.RData"))
r01 <- ggplot(data = sims_confint[1:200,], aes(x = as.Date(date), y = r0_means)) +
  geom_ribbon(aes(ymin = r0_lower, ymax = r0_upper), alpha = 0.5) +
  geom_line() + theme_bw(base_size = 18) + xlab('Date') + ylab(expression(R[0])) +
  ggtitle('Village 1')
eta1 <- ggplot(data = sims_confint[1:200,], aes(x = as.Date(date), y = ifelse(eta_best_means < 0, 0, eta_best_means))) +
  geom_ribbon(aes(ymin = ifelse(eta_best_lower < 0, 0, eta_best_lower), ymax = ifelse(eta_best_upper < 0, 0, eta_best_upper)), alpha = 0.5) +
  geom_line() + theme_bw(base_size = 18) + xlab('Date') + ylab('η') +
  ggtitle('Village 1')
load(paste0("~/vaxMada-data/objects/data_obj/esims_confint_amba.RData"))
r02 <- ggplot(data = sims_confint, aes(x = as.Date(date), y = r0_means)) +
  geom_ribbon(aes(ymin = r0_lower, ymax = r0_upper), alpha = 0.5) +
  geom_line() + theme_bw(base_size = 18) + xlab('Date') + ylab(expression(R[0])) +
  ggtitle('Village 2')
eta2 <- ggplot(data = sims_confint, aes(x = as.Date(date), y = ifelse(eta_best_means < 0, 0, eta_best_means))) +
  geom_ribbon(aes(ymin = ifelse(eta_best_lower < 0, 0, eta_best_lower), ymax = ifelse(eta_best_upper < 0, 0, eta_best_upper)), alpha = 0.5) +
  geom_line() + theme_bw(base_size = 18) + xlab('Date') + ylab('η') +
  ggtitle('Village 2')
load(paste0("~/vaxMada-data/objects/data_obj/esims_confint_vill3.RData"))
r03 <- ggplot(data = sims_confint, aes(x = as.Date(date), y = r0_means)) +
  geom_ribbon(aes(ymin = r0_lower, ymax = r0_upper), alpha = 0.5) +
  geom_line() + theme_bw(base_size = 18) + xlab('Date') + ylab(expression(R[0])) +
  ggtitle('Village 3')
eta3 <- ggplot(data = sims_confint, aes(x = as.Date(date), y = ifelse(eta_best_means < 0, 0, eta_best_means))) +
  geom_ribbon(aes(ymin = ifelse(eta_best_lower < 0, 0, eta_best_lower), ymax = ifelse(eta_best_upper < 0, 0, eta_best_upper)), alpha = 0.5) +
  geom_line() + theme_bw(base_size = 18) + xlab('Date') + ylab('η') +
  ggtitle('Village 3')
png(filename="~/vaxMada-data/plots/r0_eta.png", 
    width = 15,
    height = 6,
    units = "in",
    res = 400)
plot_grid(r01, r02, r03, eta1, eta2, eta3, align = "h", nrow=2, rel_widths = c(1/3, 1/3, 1/3))
dev.off()

ggplot(data = sims_confint, aes(x = as.Date(date), y = ifelse(eta_means < 0, 0, eta_means))) +
  geom_ribbon(aes(ymin = ifelse(eta_lower < 0, 0, eta_lower), ymax = ifelse(eta_upper < 0, 0, eta_upper)), alpha = 0.5) +
  geom_line() + theme_bw(base_size = 18) + xlab('Date') + ylab('η') +
  ggtitle('Village 3')
