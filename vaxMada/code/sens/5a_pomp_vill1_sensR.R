# Set seed, load datasets and libraries
set.seed(594709947L)
library(extraDistr)
library(pomp)
library(iterators)
library(doFuture)
library(tidyverse)
load(file="~/vaxMada-data/objects/data_obj/data_vill1.RData")

# Fixed parameters
immune_perc <- 0.136 # 13.6% of apparently healthy are immune
mort_perc <- 0.9 # 90% mortality
rho_input <- 0.8 # 90% reporting rate of deaths
k_input <- 1 # Moderate overdispersion reporting uncertainty
import_input <- 1 / 50 # 5 infectious import per month on average
waning_input <- 4 # 4 months average duration of vaccinal and natural protective immunity from infection
delta.t_input <- 1 / 6 # update daily

# User variables
N_tot <- c(data$nt_inter[1], data$nt_inter)
vax_vec_times <- data$gen[which(data$nvax > 0)]
vax_vec <- data$nvax[which(data$nvax > 0)]

# Step function
sir_step <- function (cnt, S, I, V_S, V_I, R, D, H, W, Beta, N_tot, gamma, phi, psi, immune_perc, waning, mort_perc, import, delta.t,...)
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
  c(cnt = cnt, S = S, I = I, V_S=V_S, V_I=V_I, R = R, D = D, H=H, W=W)
}
# Initial conditions
N0 <- 1189
I_init <- round(77 / mort_perc / rho_input)
S_init <- round(N0 * (1 - immune_perc)) - I_init
R_init <- N0 - S_init - I_init
sir_rinit <- function (...) {
  c(cnt=1, S = 932, I = 95, V_S=0, V_I=0, R = 162, D=77, H = 77, W=0)
}
# Measurement model
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

# Estimate transmission rate using the pre-vaccination data
data_prevax <- data[1:(which(data$nvax > 0)[1] - 1),]
data_prevax |>
  pomp(times='gen', t0=0,
       rprocess=euler(sir_step, delta.t=1/6),
       rinit=sir_rinit, accumvars="H"
  ) -> mod
mod |>
  pomp(
    userdata = list(N_tot=N_tot),
    rmeasure=sir_rmeas,
    dmeasure=sir_dmeas,
    params=c(Beta=0.0008,
             rho=rho_input, k=k_input, 
             import=import_input, 
             gamma=1,
             mort_perc=mort_perc, immune_perc=immune_perc, psi=0.13,
             waning=waning_input)
  ) -> mod
# Check properly working
mod |>
  pfilter(Np=1000) -> pf
logLik(pf)

## First, do coarse search
# Bounds used: beta * S (which is nt_inter*(1-immune_perc)) = Rt
# Lower: 0.5/mean(data$nt_inter * (1-immune_perc))
# Upper: 4.3/mean(data$nt_inter * (1-immune_perc))
slice_design(
  center=coef(mod),
  Beta=rep(seq(from=0.0005942191,to=0.005110284,length.out=20),each=1)
) -> p
plan(multisession)
foreach (
  theta=iter(p,"row"),
  .combine=rbind,
  .options.future=list(seed=108028909)
) %dofuture% {
  library(extraDistr)
  mod |> pfilter(params=theta,Np=2000) -> pf
  theta$loglik <- logLik(pf)
  theta
} -> p_beta
plot(p_beta$Beta, p_beta$loglik, pch=16)
loess_model_beta <- loess(p_beta$loglik ~ p_beta$Beta)
lines(p_beta$Beta, predict(loess_model_beta), col='red', lwd=3)
mcap_res_beta <- mcap(p_beta$loglik, p_beta$Beta)
mcap_res_beta$ci
mcap_res_beta$mle
abline(v=mcap_res_beta$ci, col='red')
best_beta <- p_beta$Beta[which.max(p_beta$loglik)]
#save(p_beta, file=paste0("~/vaxMada-data/objects/data_obj/p_beta_coarse_sensL.RData"))
#save(mcap_res_beta, file="~/vaxMada-data/objects/data_obj/mcap_res_beta_coarse_sensL.RData")
## Split in half for finer grain search
slice_design(
  center=coef(mod),
  Beta=rep(seq(from=0.0005942191,to=(0.005110284/2),length.out=50),each=1)
) -> p
plan(multisession)
foreach (
  theta=iter(p,"row"),
  .combine=rbind,
  .options.future=list(seed=108028909)
) %dofuture% {
  library(extraDistr)
  mod |> pfilter(params=theta,Np=2000) -> pf
  theta$loglik <- logLik(pf)
  theta
} -> p_beta
plot(p_beta$Beta, p_beta$loglik, pch=16)
loess_model_beta <- loess(p_beta$loglik ~ p_beta$Beta, span=0.25)
lines(p_beta$Beta, predict(loess_model_beta), col='red', lwd=3)
mcap_res_beta <- mcap(p_beta$loglik, p_beta$Beta, span=0.25)
mcap_res_beta$ci
mcap_res_beta$mle
abline(v=mcap_res_beta$ci, col='red', lty='dashed')
best_beta <- p_beta$Beta[which.max(p_beta$loglik)]
abline(v=best_beta, col='blue')
save(p_beta, file=paste0("~/vaxMada-data/objects/data_obj/p_beta_sensR.RData"))
save(mcap_res_beta, file="~/vaxMada-data/objects/data_obj/mcap_res_beta_sensR.RData")

# Estimate psi using post-vaccination data
load(file=paste0("~/vaxMada-data/objects/data_obj/p_beta_sensR.RData"))
load(file="~/vaxMada-data/objects/data_obj/mcap_res_beta_sensR.RData")
best_beta <- p_beta$Beta[which.max(p_beta$loglik)]
data_postvax <- data[(which(data$nvax > 0)[1] - 1):nrow(data),]
data_postvax$reports[1] <- NA
data_postvax$reports[2] <- NA
# From 1000 sims of the pre-vax model
mean(sims$S[which(sims$gen == max(sims$gen, na.rm=T))],na.rm=T)
mean(sims$I[which(sims$gen == max(sims$gen, na.rm=T))],na.rm=T)
mean(sims$R[which(sims$gen == max(sims$gen, na.rm=T))],na.rm=T)
mean(sims$D[which(sims$gen == max(sims$gen, na.rm=T))],na.rm=T)
mean(sims$H[which(sims$gen == max(sims$gen, na.rm=T))],na.rm=T)
mean(sims$W[which(sims$gen == max(sims$gen, na.rm=T))],na.rm=T)
mean(sims$cnt[which(sims$gen == max(sims$gen, na.rm=T))],na.rm=T)
sir_rinit <- function (...) {
  c(cnt=201, S = 571, I = 13, V_S=0, V_I=0, R = 87, D=5789, H = 13, W=-41)
}
data_postvax |>
  pomp(times='gen', t0=200,
       rprocess=euler(sir_step, delta.t=1/6),
       rinit=sir_rinit, accumvars="H"
  ) -> mod
mod |>
  pomp(
    userdata = list(N_tot=N_tot),
    rmeasure=sir_rmeas,
    dmeasure=sir_dmeas,
    params=c(Beta=best_beta, 
             rho=rho_input, 
             k=k_input, 
             import=import_input, 
             gamma=1,
             mort_perc=mort_perc,
             immune_perc=immune_perc, psi=0.1, 
             waning=waning_input,
             phi=0.05)
  ) -> mod
slice_design(
  center=coef(mod),
  psi=rep(seq(0, 1, length.out=101), each=1)
) -> p
start.time <- Sys.time()
plan(multisession)
foreach (
  theta=iter(p,"row"),
  .combine=rbind,
  .options.future=list(seed=0)
) %dofuture% {
  library(extraDistr)
  mod |> pfilter(params=theta,Np=4000) -> pf
  theta$loglik <- logLik(pf)
  theta
} -> p_psi
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
plot(p_psi$psi, p_psi$loglik, pch=16)
loess_model <- loess(p_psi$loglik ~ p_psi$psi, span = 0.75)
lines(p_psi$psi, predict(loess_model), col='red', lwd=3)
mcap_res_psi <- mcap(p_psi$loglik, p_psi$psi, level=0.95, span=0.75)
1 - rev(mcap_res_psi$ci) # ci VE
mle_psi <- p_psi$psi[which.max(p_psi$loglik)]
mle_VE <- 1 - mle_psi 
mle_VE # mle VE
abline(v=mcap_res_psi$ci, col='red', lty='dashed')
best_psi <- mle_psi
abline(v=best_psi, col='blue')
(1 - mcap_res_psi$ci) * 0.95
(1 - best_psi) * 0.95
#save(p_psi, file=paste0("~/vaxMada-data/objects/data_obj/p_psi_sensR.RData"))
#save(mcap_res_psi, file=paste0("~/vaxMada-data/objects/data_obj/mcap_res_psi_sensR.RData"))
