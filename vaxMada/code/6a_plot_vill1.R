# Load libraries and results
set.seed(594709947L)
library(ggplot2)
library(cowplot)
library(extraDistr)
library(pomp)
library(iterators)
library(doFuture)
library(tidyverse)
load(file=paste0("~/vaxMada-data/objects/data_obj/vax_dates.RData"))
load(file="~/vaxMada-data/objects/data_obj/data_vill1.RData")
load(file=paste0("~/vaxMada-data/objects/data_obj/p_beta.RData"))
load(file=paste0("~/vaxMada-data/objects/data_obj/mcap_res_beta.RData"))
load(file=paste0("~/vaxMada-data/objects/data_obj/p_psi.RData"))
load(file=paste0("~/vaxMada-data/objects/data_obj/mcap_res_psi.RData"))
best_beta <- p_beta$Beta[which.max(p_beta$loglik)]
best_psi <- p_psi$psi[which.max(p_psi$loglik)]

# Re-load pomp objects for simulation
immune_perc <- 0.136
mort_perc <- 0.9
rho_input <- 0.9 
k_input <- 1
import_input <- 1 / 50 
waning_input <- 4
delta.t_input <- 1 / 6 
N_tot <- c(data$nt_inter[1], data$nt_inter)
vax_vec_times <- data$gen[which(data$nvax > 0)]
vax_vec <- data$nvax[which(data$nvax > 0)]
sir_step <- function (cnt, f_size, rt, r0, eta, eta_best, S, I, V_S, V_I, R, D, H, W, Beta, N_tot, gamma, phi, psi, immune_perc, waning, mort_perc, import, delta.t,...)
{
  # Flows
  if (abs(cnt - round(cnt)) < 0.1) {
    curr_N <- N_tot[round(cnt)]
    W <- curr_N - (S+I+V_S+V_I+R)
    if (curr_N > (S + I + V_S+V_I+R)) {
      pos_flow <- curr_N - (S + I + V_S + V_I + R)
      SR_pos_flow <- pos_flow
      S_in <- rbinom(n=1, size=SR_pos_flow, prob=(1 - immune_perc))
      R_in <- pos_flow - S_in
      S <- S + S_in
      R <- R + R_in
    } else {
      neg_flow <- (S + I + V_S + V_I + R) - curr_N
      res_out <- rmvhyper(1, c(S, I, V_S, V_I, R), neg_flow)
      S_out <- res_out[1,1]
      I_out <- res_out[1,2]
      V_S_out <- res_out[1,3]
      V_I_out <- res_out[1,4]
      R_out <- res_out[1,5]
      S <- S - S_out
      I <- I - I_out
      V_S <- V_S - V_S_out
      V_I <- V_I - V_I_out
      R <- R - R_out
    }
    if ((S + I + V_S + V_I + R) != curr_N) {
      stop('Error in pop. size.')
    }
  }
  
  # Vaccination
  #vax_vec_times <- c(271, 291, 311, 331, 351, 371, 396, 426, 446, 466, 486) + 1
  #vax_vec <- c(282, 429, 427, 371, 703, 714, 520, 492, 882, 627, 337)
  vax_vec_times <- c(201, 221, 241, 261, 281, 301, 326, 356, 376, 396, 416) + 1  # Cnt starts at 1 instead of 0
  vax_vec <- c(282, 429, 427, 371, 703, 714, 520, 492, 882, 627, 337)
  
  if ((round(cnt) %in% vax_vec_times) & (abs(cnt - round(cnt)) < 0.1)) {
    to_vax <- vax_vec[which(vax_vec_times == round(cnt))]
    res_out_vax <- rmvhyper(1, c(S, V_S, V_I, R), to_vax)
    S_out_vax <- rbinom(n=1, res_out_vax[1], 1 - phi) # number that at least prevent disease
    S_out_vax_prevdis <- rbinom(n=1, S_out_vax, prob=psi) # number that prevent disease, but not infection
    S_out_vax_prevdisinf <- S_out_vax -  S_out_vax_prevdis # number that prevent both disease and infection
    VS_out_vax <- rbinom(n=1, res_out_vax[2], (1 - phi)*(1 - psi))
    VI_out_vax <- rbinom(n=1, res_out_vax[3], (1 - phi)*(1 - psi))
    
    S <- S - S_out_vax
    V_S <- V_S + S_out_vax_prevdis - VS_out_vax
    V_I <- V_I - VI_out_vax
    R <- R + S_out_vax_prevdisinf + VS_out_vax + VI_out_vax
    if (S < 0) {
      stop(paste0('error.', S))
    }
  }
  
  # Transitions
  dN_SI <- rbinom(n=1,size=S,prob=1-exp(-Beta*(I+V_I)*delta.t))
  dN_gam <- rbinom(n=1, size=I, prob=1-exp(-gamma*delta.t))
  dN_ID <- rbinom(n=1, size=dN_gam, prob=mort_perc)
  dN_IR <- dN_gam - dN_ID
  
  dN_VS_VI <- rbinom(n=1,size=V_S, prob=1-exp(-Beta*(I+V_I)*delta.t))
  if ((V_S - dN_VS_VI) > 0) {
    dN_VS_S <- rbinom(n=1, size=(V_S - dN_VS_VI), prob=1-exp(-(1/(5*waning))*delta.t))
  } else {
    dN_VS_S <- 0
  }
  dN_VI_VS <- rbinom(n=1, size=V_I, prob=1-exp(-gamma*delta.t))
  if((V_I - dN_VI_VS) > 0) {
    dN_VI_S <- rbinom(n=1, size=(V_I - dN_VI_VS), prob=1-exp(-(1/(5*waning))*delta.t))
  } else {
    dN_VI_S <- 0
  }
  
  dN_RS <- rbinom(n=1, size=R, prob=1-exp(-(1/(5*waning))*delta.t))
  I_in <- rbinom(n=1, size=10, prob=1-exp(-import*delta.t))
  S <- S - dN_SI + dN_RS + dN_VS_S + dN_VI_S
  I <- I + dN_SI - dN_gam + I_in
  
  V_S <- V_S - dN_VS_VI - dN_VS_S + dN_VI_VS
  V_I <- V_I + dN_VS_VI - dN_VI_VS - dN_VI_S
  
  R <- R + dN_IR - dN_RS
  D <- D + dN_ID
  H <- H + dN_ID
  
  cnt <- cnt + 1/6
  f_size <- f_size + dN_SI + I_in + dN_VS_VI
  rt <- ((1 - exp(-Beta * delta.t)) / (1 - exp(-1 * delta.t))) * (S + V_S)
  r0 <- ((1 - exp(-Beta * delta.t)) / (1 - exp(-1 * delta.t))) * (S + I + V_S + V_I + R)
  eta <- (1 - (1 / r0) - ((I + V_I + R) / (S + I + V_S + V_I + R))) #* (1 / (1 - phi)) * (1 / (1 - psi))
  eta_best <- (1 - (1 / r0))
  #eta <- (1 - (1 / r0)) * (1 / (1 - phi)) * (1 / (1 - psi))
  #eta_best <- (1 - (1 / r0)) * (1 / (1 - phi)) * (1 / (1 - best_psi))
  
  c(cnt = cnt, f_size = f_size, rt=rt, r0=r0, eta=eta, eta_best=eta_best, S = S, I = I, V_S=V_S, V_I=V_I, R = R, D = D, H=H, W=W)
}
# Initial conditions
N0 <- 1189
I_init <- round(77 / mort_perc / rho_input)
S_init <- round(N0 * (1 - immune_perc)) - I_init
R_init <- N0 - S_init - I_init
sir_rinit <- function (...) {
  c(cnt=1, f_size = 0, rt=0, r0=0, eta=0, eta_best=0, S = 932, I = 95, V_S=0, V_I=0,R = 162, D=77, H = 77, W=0)
}
sir_rmeas <- function (k, rho, H, ...) {
  c(reports=rnbinom(n=1, size=k, mu=rho*H))
}
sir_dmeas <- function (reports, H, rho, k, log,...) {
  if (is.na(reports)) {
    ifelse(log, 0, 1)
  } else {
    dnbinom(x=reports, size=k, mu=rho*H, log=log)
  }
}

# Sample simulations of MLE to check trajectory
data |>
  pomp(times='gen', t0=0,
       rprocess=euler(sir_step, delta.t=1/6),
       rinit=sir_rinit, accumvars="H"
  ) -> mod
mod |>
  pomp(
    userdata = list(N_tot=N_tot),
    rmeasure=sir_rmeas,
    dmeasure=sir_dmeas,
    params=c(Beta=best_beta,
             rho=rho_input, k=k_input, 
             import=import_input, 
             gamma=1,
             mort_perc=mort_perc, immune_perc=immune_perc, psi=best_psi,
             waning=waning_input,
             phi=0.05)
  ) -> mod
mod |>
  pomp(params=c(Beta=best_beta,
                import=import_input, 
                k=k_input, 
                rho=rho_input, 
                gamma=1,
                mort_perc=mort_perc, immune_perc=immune_perc, psi=best_psi,
                waning=waning_input,
                phi=0.05)) |>
  simulate(nsim=1,format="data.frame",include.data=TRUE,
           userdata=list(N_tot=N_tot)) -> simpSims
simpSims |>
  ggplot(mapping=aes(x=gen,y=reports,group=.id,color=(.id=="data")))+
  scale_color_manual(values=c(`TRUE`="black",`FALSE`="grey50"),
                     labels=c(`FALSE`="simulation",`TRUE`="data"))+
  labs(color="")+
  geom_line()+
  facet_wrap(~.id)+
  theme_bw()+
  geom_line(mapping=aes(gen, y=(V_I)), col='pink') +
  geom_line(mapping=aes(gen, y=(I)), col='red') +
  geom_line(mapping=aes(gen, y=R), col='purple') +
  geom_line(mapping=aes(gen, y=(S+I+V_S+V_I+R)), col='green') +
  geom_line(mapping=aes(gen, y=S), col='blue')+
  geom_line(mapping=aes(gen, y=V_S), col='turquoise')

# Simulations across 95% confidence intervals
nsim <- 1000
row_names <- c('Beta', 'import', 'k', 'rho', 'gamma',
                  'mort_perc', 'immune_perc', 'psi', 'waning', 'phi', 
               'best_psi')
params_mat <- matrix(c(runif(nsim, mcap_res_beta$ci[1], mcap_res_beta$ci[2]),
                 rep(import_input, nsim),
                 rep(k_input, nsim),
                 rep(rho_input, nsim),
                 rep(1, nsim),
                 rep(mort_perc, nsim),
                 rep(immune_perc, nsim),
                 runif(nsim, mcap_res_psi$ci[1], mcap_res_psi$ci[2]),
                 rep(waning_input, nsim),
                 rep(0.05, nsim),
                 rep(best_psi, nsim)),
                 byrow=T, nrow=11, ncol=nsim, dimnames = list(row_names, NULL))
mod |>
  simulate(
    params=params_mat,
    nsim=1,format="data.frame",include.data=TRUE,userdata=list(N_tot=N_tot)
  ) -> sims
sims_confint <- sims[which(sims$.id == 'data'),]
lower <- c()
upper <- c()
means <- c()
medians <- c()
Ss_lower <- c()
Ss_upper <- c()
Ss_means <- c()
Is_lower <- c()
Is_upper <- c()
Is_means <- c()
VI_means <- c()
VS_means <- c()
s_props_lower <- c()
s_props_upper <- c()
s_props_means <- c()
rt_lower <- c()
rt_upper <- c()
rt_means <- c()
r0_lower <- c()
r0_upper <- c()
r0_means <- c()
eta_lower <- c()
eta_upper <- c()
eta_means <- c()
eta_best_lower <- c()
eta_best_upper <- c()
eta_best_means <- c()
for (tri_mon in unique(sims$gen)) {
  res_conf <- quantile(sims$reports[which(sims$gen == tri_mon)], c(0.025, 0.975), na.rm=T)
  lower <- c(lower, res_conf[1])
  upper <- c(upper, res_conf[2])
  means <- c(means, mean(sims$reports[which(sims$gen == tri_mon)], na.rm=T))
  medians <- c(medians, median(sims$reports[which(sims$gen == tri_mon)], na.rm=T))
  
  # Suscep.
  Ss_res_conf <- quantile(sims$S[which(sims$gen == tri_mon)], c(0.025, 0.975), na.rm=T)
  Ss_lower <- c(Ss_lower, Ss_res_conf[1])
  Ss_upper <- c(Ss_upper, Ss_res_conf[2])
  Ss_means <- c(Ss_means, mean(sims$S[which(sims$gen == tri_mon)], na.rm=T))
  
  # Infect
  Is_res_conf <- quantile(sims$I[which(sims$gen == tri_mon)], c(0.025, 0.975), na.rm=T)
  Is_lower <- c(Is_lower, Is_res_conf[1])
  Is_upper <- c(Is_upper, Is_res_conf[2])
  Is_means <- c(Is_means, mean(sims$I[which(sims$gen == tri_mon)], na.rm=T))
  
  # V_I
  VI_means <- c(VI_means, mean(sims$V_I[which(sims$gen == tri_mon)], na.rm=T))
  
  # V_S
  VS_means <- c(VS_means, mean(sims$V_S[which(sims$gen == tri_mon)], na.rm=T))
  
  # s_prop
  s_props_conf <- quantile(rowSums(sims[which(sims$gen == tri_mon),c('S', 'V_S')]) /  rowSums(sims[which(sims$gen == tri_mon),c('S','I','R','V_S','V_I')]), c(0.025, 0.975), na.rm=T)
  s_props_lower <- c(s_props_lower, s_props_conf[1])
  s_props_upper <- c(s_props_upper, s_props_conf[2])
  s_props_means <- c(s_props_means, mean(rowSums(sims[which(sims$gen == tri_mon),c('S', 'V_S')]) / rowSums(sims[which(sims$gen == tri_mon),c('S','I','R','V_S','V_I')]),na.rm=T))
  
  # rt
  rt_res_conf <- quantile(sims$rt[which(sims$gen == tri_mon)], c(0.025, 0.975), na.rm=T)
  rt_lower <- c(rt_lower, rt_res_conf[1])
  rt_upper <- c(rt_upper, rt_res_conf[2])
  rt_means <- c(rt_means, mean(sims$rt[which(sims$gen == tri_mon)], na.rm=T))
  
  # r0
  r0_res_conf <- quantile(sims$r0[which(sims$gen == tri_mon)], c(0.025, 0.975), na.rm=T)
  r0_lower <- c(r0_lower, r0_res_conf[1])
  r0_upper <- c(r0_upper, r0_res_conf[2])
  r0_means <- c(r0_means, mean(sims$r0[which(sims$gen == tri_mon)], na.rm=T))
  
  # eta
  eta_res_conf <- quantile(sims$eta[which(sims$gen == tri_mon)], c(0.025, 0.975), na.rm=T)
  eta_lower <- c(eta_lower, eta_res_conf[1])
  eta_upper <- c(eta_upper, eta_res_conf[2])
  eta_means <- c(eta_means, mean(sims$eta[which(sims$gen == tri_mon)], na.rm=T))
  
  # eta best
  eta_best_res_conf <- quantile(sims$eta_best[which(sims$gen == tri_mon)], c(0.025, 0.975), na.rm=T)
  eta_best_lower <- c(eta_best_lower, eta_best_res_conf[1])
  eta_best_upper <- c(eta_best_upper, eta_best_res_conf[2])
  eta_best_means <- c(eta_best_means, mean(sims$eta_best[which(sims$gen == tri_mon)], na.rm=T))
}
sims_confint$lower <- lower
sims_confint$upper <- upper
sims_confint$means <- means
sims_confint$medians <- medians
sims_confint$Ss_lower <- Ss_lower
sims_confint$Ss_upper <- Ss_upper
sims_confint$Ss_means <- Ss_means
sims_confint$Is_lower <- Is_lower
sims_confint$Is_upper <- Is_upper
sims_confint$Is_means <- Is_means
sims_confint$VI_means <- VI_means
sims_confint$s_props_lower <- s_props_lower
sims_confint$s_props_upper <- s_props_upper
sims_confint$s_props_means <- s_props_means
sims_confint$rt_lower <- rt_lower
sims_confint$rt_upper <- rt_upper
sims_confint$rt_means <- rt_means
sims_confint$r0_lower <- r0_lower
sims_confint$r0_upper <- r0_upper
sims_confint$r0_means <- r0_means
sims_confint$eta_lower <- eta_lower
sims_confint$eta_upper <- eta_upper
sims_confint$eta_means <- eta_means
sims_confint$eta_best_lower <- eta_best_lower
sims_confint$eta_best_upper <- eta_best_upper
sims_confint$eta_best_means <- eta_best_means
vertical_lines <- sims_confint$date[which(sims_confint$nvax > 0)]
ggplot(data = sims_confint, aes(x = date, y = reports)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  geom_point(size=0.5) +
  geom_line(aes(x=date,y=Is_means+VI_means),col='red') +
  geom_line(aes(x=date,y=VI_means),col='pink')
ggplot(data = sims_confint, aes(x = date, y = rt_means)) +
  geom_ribbon(aes(ymin = rt_lower, ymax = rt_upper), alpha = 0.5) +
  geom_line(size=0.5)+ geom_hline(yintercept =1)
ggplot(data = sims_confint, aes(x = date, y = r0_means)) +
  geom_ribbon(aes(ymin = r0_lower, ymax = r0_upper), alpha = 0.5) +
  geom_line(size=0.5) 
ggplot(data = sims_confint, aes(x = date, y = ifelse(eta_means < 0, 0, eta_means))) +
  geom_ribbon(aes(ymin = ifelse(eta_lower < 0, 0, eta_lower), ymax = ifelse(eta_upper < 0, 0, eta_upper)), alpha = 0.5) +
  geom_line(size=0.5) + ylim(0, max(sims_confint$eta_upper))
ggplot(data = sims_confint, aes(x = date, y = ifelse(eta_best_means < 0, 0, eta_best_means))) +
  geom_ribbon(aes(ymin = ifelse(eta_best_lower < 0, 0, eta_best_lower), ymax = ifelse(eta_best_upper < 0, 0, eta_best_upper)), alpha = 0.5) +
  geom_line(size=0.5) + ylim(0, max(sims_confint$eta_best_upper))
ggplot(data = sims_confint, aes(x = date, y = s_props_means)) +
  geom_ribbon(aes(ymin = s_props_lower, ymax = s_props_upper), alpha = 0.5) +
  geom_line(size=0.5) + ylim(0, 1)
#save(sims, file=paste0("~/vaxMada-data/objects/data_obj/esims.RData"))
#save(sims_confint, file=paste0("~/vaxMada-data/objects/data_obj/esims_confint.RData"))
load(file=paste0("~/vaxMada-data/objects/data_obj/esims.RData"))
load(file=paste0("~/vaxMada-data/objects/data_obj/esims_confint.RData"))

# Statistics for Table 2
## For Vill 1, look at solely pre-vaccination period for Rt
data$gen[which(data$date == as.Date('2016-05-01')) - 1]
mean(sims$rt[which(sims$gen <= 200)], na.rm=T)
sd(sims$rt[which(sims$gen <= 200)], na.rm=T)
mean(sims$r0, na.rm=T)
sd(sims$r0, na.rm=T)
mean(ifelse(sims$eta[which(sims$gen <= 200)] < 0, 0, sims$eta[which(sims$gen <= 200)]), na.rm=T)
sd(ifelse(sims$eta[which(sims$gen <= 200)] < 0, 0, sims$eta[which(sims$gen <= 200)]), na.rm=T)
mean(ifelse(sims$eta_best[which(sims$gen <= 200)] < 0, 0, sims$eta_best[which(sims$gen <= 200)]), na.rm=T)
sd(ifelse(sims$eta_best[which(sims$gen <= 200)] < 0, 0, sims$eta_best[which(sims$gen <= 200)]), na.rm=T)

# Simulations @ no vaccination
mod |>
  simulate(
    params=c(Beta=best_beta,
             import=import_input, 
             k=k_input, 
             rho=rho_input, gamma=1,
             mort_perc=mort_perc, immune_perc=immune_perc, psi=1,
             waning=waning_input,
             phi=1,
             best_psi=best_psi),
    nsim=nsim,format="data.frame",include.data=TRUE,userdata=list(N_tot=N_tot)
  ) -> sims2
sims_confint2 <- sims2[which(sims2$.id == 'data'),]
lower <- c()
upper <- c()
means <- c()
rt_lower <- c()
rt_upper <- c()
rt_means <- c()
for (tri_mon in unique(sims2$gen)) {
  res_conf <- quantile(sims2$reports[which(sims2$gen == tri_mon)], c(0.025, 0.975), na.rm=T)
  lower <- c(lower, res_conf[1])
  upper <- c(upper, res_conf[2])
  means <- c(means, mean(sims2$reports[which(sims2$gen == tri_mon)], na.rm=T))
  
  # rt
  rt_res_conf <- quantile(sims2$rt[which(sims$gen == tri_mon)], c(0.025, 0.975), na.rm=T)
  rt_lower <- c(rt_lower, rt_res_conf[1])
  rt_upper <- c(rt_upper, rt_res_conf[2])
  rt_means <- c(rt_means, mean(sims2$rt[which(sims$gen == tri_mon)], na.rm=T))
}
sims_confint2$lower <- lower
sims_confint2$upper <- upper
sims_confint2$means <- means
sims_confint2$rt_lower <- rt_lower
sims_confint2$rt_upper <- rt_upper
sims_confint2$rt_means <- rt_means
#save(sims2, file=paste0("~/vaxMada-data/objects/data_obj/sims2.RData"))
#save(sims_confint2, file=paste0("~/vaxMada-data/objects/data_obj/sims_confint2.RData"))
load(file=paste0("~/vaxMada-data/objects/data_obj/sims2.RData"))
load(file=paste0("~/vaxMada-data/objects/data_obj/sims_confint2.RData"))

# Plot simulated counterfactuals with and without vaccination
sims_confint2_plot <- sims_confint2[data$gen[which(data$date == vax_dates[2])]:nrow(sims_confint2),]
vertical_lines <- data$date[which(data$nvax > 0)]
cases <- ggplot(data = sims_confint, aes(x = as.Date(date), y = reports)) +
  geom_ribbon(data=sims_confint2_plot, aes(ymin=lower, ymax=upper), alpha=0.25, fill='red') +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  xlab('Date') + ylab('Reported cases')+
  geom_line(aes(x=as.Date(date), y=means, color='A')) +
  geom_line(data=sims_confint2_plot, aes(x=as.Date(date), y=means, color='B')) +
  geom_line(aes(color='C')) + theme_bw()+ 
  scale_x_date(
    limits = c(min(as.Date(sims_confint$date)), max(as.Date(sims_confint$date))),
    date_breaks = "1 year", 
    date_labels = "%Y"
  )+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )+ggtitle("C") +
  theme(plot.title = element_text(vjust = 2)) +
  labs(color = "") +
  scale_color_manual(
    values = c("A" = "black", "B" = "red", "C" = "blue"),
    labels = c("Counterfactual Vax.", "Counterfactual No Vax.", 'Data')
  ) +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10), 
    legend.key.size = unit(0.5, "cm")
  )   +
  guides(color = guide_legend(direction = "horizontal", nrow = 1))
suscep <- ggplot(data = sims_confint, aes(x = as.Date(date), y = s_props_means)) +
  geom_ribbon(aes(ymin = s_props_lower, ymax = s_props_upper), alpha = 0.5) +
  xlab('') + ylab('Susceptible proportion')+
  geom_line(col='black')+
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+ylim(0,1)+
  scale_x_date(
    limits = c(min(as.Date(sims_confint$date)), max(as.Date(sims_confint$date))), # Set limits from earliest to latest date
    date_breaks = "1 year", 
    date_labels = "%Y" 
  )+ggtitle("A") +
  theme(plot.title = element_text(vjust = 2)) +
geom_vline(xintercept = vertical_lines, color = "black", linetype = "dashed", lwd=0.5)
Rt <- ggplot(data = sims_confint, aes(x = date, y = rt_means)) +
  geom_ribbon(aes(ymin = rt_lower, ymax = rt_upper), alpha = 0.5) +
  geom_line() + theme_bw()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+xlab('')+ggtitle("B") +
  theme(plot.title = element_text(vjust = 2)) +
  scale_x_date(
    limits = c(min(as.Date(sims_confint$date)), max(as.Date(sims_confint$date))),
    date_breaks = "1 year",
    date_labels = "%Y"
  )+ylab("Effective reproduction number")+
  geom_hline(yintercept=1, lty='dashed')
my_plot <- plot_grid(suscep, Rt, cases, align = "v", nrow = 3, rel_heights = c(3/10, 3/10, 4/10))
ggsave("~/vaxMada-data/plots/Rt.png", plot = my_plot, device = "png", width = 8, height = 8, units = "in",  dpi = 800)

# Difference in final size with and without vaccination
fs_mle <- c()
fs_novax <- c()
for (i in 1:nsim) {
  f_sim <- sims[which(sims$.id == i),]
  fs_mle <- c(fs_mle, f_sim$f_size[nrow(f_sim)] - f_sim$f_size[which(f_sim$date == as.Date('2016-05-01'))])
  f_sim2 <- sims2[which(sims2$.id == i),]
  fs_novax <- c(fs_novax, f_sim2$f_size[nrow(f_sim2)] - f_sim2$f_size[which(f_sim2$date == as.Date('2016-05-01'))])
}
hist(fs_novax, col = rgb(1, 0, 0, 0.5), breaks=20, xlim=c(0,max(fs_novax)))
hist(fs_mle, col = rgb(0, 0, 1, 0.5), add=T, breaks=30)
ave_inf_novax <- mean(fs_novax) / ((nrow(f_sim2)-which(f_sim2$date == as.Date('2016-05-01'))+1) / 5)
ave_inf_novax
ave_inf_mle <- mean(fs_mle) / ((nrow(f_sim2) - which(f_sim2$date == as.Date('2016-05-01'))+1) / 5)
ave_inf_mle
diff_inf_novax_mle <- (mean(fs_novax) - mean(fs_mle)) / ((nrow(f_sim2)-which(f_sim2$date == as.Date('2016-05-01'))+1) / 5)
diff_inf_novax_mle
diff_inf_novax_mle / ave_inf_novax

# Difference in Rt means with and without vaccination
hist(sims_confint2$rt_means[which(as.Date(sims_confint2$date) == as.Date('2016-05-01')):nrow(sims_confint2)], col = rgb(1, 0, 0, 0.5), xlim=c(0.25, 1.5), breaks=20)
hist(sims_confint$rt_means[which(as.Date(sims_confint$date) == as.Date('2016-05-01')):nrow(sims_confint)], col = rgb(0, 0, 1, 0.5), add=T, breaks=30)
abline(v=mean(sims_confint2$rt_means[which(as.Date(sims_confint2$date) == as.Date('2016-05-01')):nrow(sims_confint2)], na.rm=T), col='red', lty='dashed')
abline(v=mean(sims_confint$rt_means[which(as.Date(sims_confint$date) == as.Date('2016-05-01')):nrow(sims_confint)], na.rm=T), col='blue', lty='dashed')
Rt_novax <- mean(sims_confint2$rt_means[which(as.Date(sims_confint2$date) == as.Date('2016-05-01')):nrow(sims_confint2)], na.rm=T)
Rt_vax <- mean(sims_confint$rt_means[which(as.Date(sims_confint$date) == as.Date('2016-05-01')):nrow(sims_confint)], na.rm=T)
Rt_novax - Rt_vax

# Percentage Rt less than 1 with and without vaccination
less1_novax <- length(which(sims_confint2$rt_means >= 1)) / nrow(sims_confint2)
less1_vax <- length(which(sims_confint$rt_means[which(data$nvax > 0)[1]:nrow(sims_confint)] >= 1)) / nrow(sims_confint[which(data$nvax > 0)[1]:nrow(sims_confint),])
less1_novax - less1_vax

# Month seasonality in Rt
sims_confint$date <- as.Date(sims_confint$date)
sims_confint$month <- format(sims_confint$date, "%m")
ggplot(data = sims_confint[1:270,], aes(x=month, y=rt_means)) +
  geom_boxplot()

# Seasonality in Rt
sims_confint$season <- ifelse((sims_confint$month == '12' | sims_confint$month == '01' | 
                             sims_confint$month == '02'), 'Winter', 
                            ifelse((sims_confint$month == '03' | sims_confint$month == '04' | 
                                      sims_confint$month == '05'), 'Spring',
                                   ifelse((sims_confint$month == '06' | sims_confint$month == '07' | 
                                      sims_confint$month == '08'), 'Summer', 'Fall')))
sims_confint$season_factor <- factor(sims_confint$season, levels=c("Winter", "Spring", "Summer", "Fall"))
ggplot(data = sims_confint[1:270,], aes(x=season_factor, y=rt_means)) +
  geom_boxplot()

# Herd immunity effect for range of VE
HI_confints <- list()
HI_confints_dex <- 1
for (VE in c(0.1, 0.5, 0.7, 0.87)) {
  nsim <- 500
  mod |>
    simulate(
      params=c(Beta=best_beta,
               import=import_input, 
               k=k_input, 
               rho=rho_input, 
               gamma=1,
               mort_perc=mort_perc, immune_perc=immune_perc, psi=(1-VE),
               waning=waning_input,
               phi=0.05,
               best_psi=best_psi),
      nsim=nsim,format="data.frame",include.data=TRUE,userdata=list(N_tot=N_tot)
    ) -> sims
  HI_confint <- sims[which(sims$.id == 'data'),]
  lower <- c()
  upper <- c()
  means <- c()
  medians <- c()
  Ss_lower <- c()
  Ss_upper <- c()
  Ss_means <- c()
  Is_lower <- c()
  Is_upper <- c()
  Is_means <- c()
  for (tri_mon in unique(sims$gen)) {
    res_conf <- quantile(sims$reports[which(sims$gen == tri_mon)], c(0.025, 0.975), na.rm=T)
    lower <- c(lower, res_conf[1])
    upper <- c(upper, res_conf[2])
    means <- c(means, mean(sims$reports[which(sims$gen == tri_mon)], na.rm=T))
    medians <- c(medians, median(sims$reports[which(sims$gen == tri_mon)], na.rm=T))
    
    # Suscep.
    Ss_res_conf <- quantile(sims$S[which(sims$gen == tri_mon)], c(0.025, 0.975), na.rm=T)
    Ss_lower <- c(Ss_lower, Ss_res_conf[1])
    Ss_upper <- c(Ss_upper, Ss_res_conf[2])
    Ss_means <- c(Ss_means, mean(sims$S[which(sims$gen == tri_mon)], na.rm=T))
    
    # Infect
    Is_res_conf <- quantile(sims$I[which(sims$gen == tri_mon)], c(0.025, 0.975), na.rm=T)
    Is_lower <- c(Is_lower, Is_res_conf[1])
    Is_upper <- c(Is_upper, Is_res_conf[2])
    Is_means <- c(Is_means, mean(sims$I[which(sims$gen == tri_mon)], na.rm=T))
  }
  HI_confint$lower <- lower
  HI_confint$upper <- upper
  HI_confint$means <- means
  HI_confint$medians <- medians
  HI_confint$Ss_lower <- Ss_lower
  HI_confint$Ss_upper <- Ss_upper
  HI_confint$Ss_means <- Ss_means
  HI_confint$Is_lower <- Is_lower
  HI_confint$Is_upper <- Is_upper
  HI_confint$Is_means <- Is_means
  HI_confint$date <- as.Date(HI_confint$date)
  HI_confints[[HI_confints_dex]] <- HI_confint[which(HI_confint$nvax > 0)[1]:nrow(HI_confint),]
  HI_confints_dex <- HI_confints_dex + 1
}
colors <- c("0.1" = "red", "0.5" = "orange", "0.7" = "yellow", "0.87 (MLE)" = "black")
vertical_lines <- HI_confint$date[which(HI_confint$nvax > 0)]
ggplot(data = HI_confints[[2]], aes(x = as.Date(date), y = means, color='0.5')) +
  geom_line(lwd=1)+
  geom_line(data = HI_confints[[1]], aes(x=date, y=means, color='0.1'),  lwd=1)+
  geom_line(data = HI_confints[[3]], aes(x=date, y=means, color='0.7'), lwd=1)+
  geom_line(data = HI_confints[[4]], aes(x=date, y=means, color='0.87 (MLE)'), lwd=1)+
  geom_line(data = HI_confints[[1]][which(HI_confint$.id == 'data'),], aes(x=date, y=dt_inter),  
            color='blue', lwd=1)+
  labs(x = "Date",
       y = "Mean reported infections",
       color = "Vax. Eff.") +
  scale_color_manual(values = colors)+
  scale_x_date() + theme_bw(base_size=18)
save(HI_confints, file="~/vaxMada-data/objects/data_obj/HI_confints.RData")
