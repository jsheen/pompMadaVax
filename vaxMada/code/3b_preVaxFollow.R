# ------------------------------------------------------------------------------
# @description: analyze solely the houses that have information in pre, vax, and follow
# 
# @name: Justin Sheen
# @date: February 15, 2025
#
# @objective: analyze solely the houses that have information in pre, vax, and follow
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# (0) Load helper functions and data
# ------------------------------------------------------------------------------
load("~/vaxMada-data/objects/data_obj/hh_data.RData")
load("~/vaxMada-data/objects/data_obj/hh_data_vax.RData")
load("~/vaxMada-data/objects/data_obj/hh_data_follow.RData")
load("~/vaxMada-data/objects/data_obj/pre_vax_follow_names.RData")

# ------------------------------------------------------------------------------
# (1) Relationship between deaths and vaccination
# ------------------------------------------------------------------------------
xs <- c()
ys <- c()
zs <- c()
for (i in 1:nrow(pre_vax_follow_names)) {
  pre <- hh_data[which(hh_data$name == pre_vax_follow_names$pre[i]),]
  vax <- hh_data_vax[which(hh_data_vax$name == pre_vax_follow_names$vax[i]),]
  follow <- hh_data_follow[which(hh_data_follow$name == pre_vax_follow_names$follow[i]),]
  
  if (nrow(pre) > 1) {
    pre$flows <- c(NA, pre$denoms[2:nrow(pre)] - pre$denoms[1:(nrow(pre) - 1)] + pre$deaths[2:nrow(pre)])
    x <- pre$deaths[2:nrow(pre)] / (pre$denoms[1:(nrow(pre) - 1)] + pre$flows[1:(nrow(pre) - 1)])
    x <- x[which(!is.na(x) & is.finite(x))]
    x <- mean(x)
    xs <- c(xs, x)
    
    vax$vax <- ifelse(is.na(vax$vax), 0, vax$vax)
    y <- mean(vax$vax[which(!is.na(vax$denoms) & vax$denoms > 0)] / 
                vax$denoms[which(!is.na(vax$denoms) & vax$denoms > 0)])
    ys <- c(ys, y)
    z <- mean(follow$deaths, na.rm=T)
    zs <- c(zs, z)
  }
}
png(filename="~/vaxMada-data/plots/link.png",
    width = 5.5,
    height = 4.5,
    units = "in",
    res = 300)
par(mfrow = c(1,1))
par(mar=c(4.1, 5.1, 4.1, 2.1), xpd=TRUE)
plot(xs, ys, xlab='Avg. monthly mortality rate (pre-vax period)', 
     ylab='Avg. vax rate (vax-period)', ylim=c(0, 1), xlim=c(0, max(xs)), cex.lab=1.3,
    cex.main=1.5)
res <- cor.test(ys, xs, method='spearman')
text(max(xs) - 0.05, 0.9, labels='p-value = 0.87')
dev.off()
plot(ys, zs)
  
  

  

