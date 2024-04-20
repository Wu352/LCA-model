pkgs_overall <- c("data.table", "reshape2", "ggplot2", "ggrepel", "scales", "paletteer")
pkgs_regress <- c("survival", "survminer", "RISCA", "cmprsk", "forestplot") #"fastcmprsk", 
pkgs_treat <- c("survival", "survminer", "forestplot")
sapply(pkgs_overall, require, character.only = TRUE, quietly = TRUE)
sapply(pkgs_regress, require, character.only = TRUE, quietly = TRUE)
sapply(pkgs_treat, require, character.only = TRUE, quietly = TRUE)
setwd("./data")
source("../scripts/LCAC-master/Rscript/FuncsCols_Revise.r")

load("dat_surv.RData")

### Cox regression 
vclass <- levels(dat_surv$vclass)
stage <- levels(dat_surv$stage)
site <- levels(dat_surv$site)
res <- data.frame()

for (iclass in vclass) {
  for (istage in stage) {
    for (isite in site) {
idat <- dat_surv[vclass == iclass & stage == istage & site == isite]
  if (nrow(idat)==0|nrow(idat)==1) {
    ires <- c(iclass, istage, isite,table(idat$radiation), rep("",7))
  }else {
    ifit <- coxph(Surv(as.numeric(surv.month, surv.death)) ~ radiation, data = idat)
res_coef <- summary(ifit)$coef[1, c(1, 3, 4, 5)]
res_conf <- summary(ifit)$conf[1, c(1, 3, 4)]
ires <- c(iclass, istage, isite, table(idat$radiation), res_coef, res_conf)
}
names(ires) <- c("Class", "Stage", "Site", "nonRadio", "Radio", "beta", "se", "zval", "pval", "hr", "l95", "u95")
ires <- data.frame(ires) %>% 
  t(.)
res <- rbind(res,ires)
  }
  }
}
res <- as.data.table(res)
res[, hr:= as.numeric(hr)]
res[, l95:= as.numeric(l95)]
res[, u95:= as.numeric(u95)]
res[, pval:= as.numeric(pval)]
res[, hr_ci:= ifelse(is.na(hr),"       -    ",paste0(sprintf("%.2f", hr), " (", sprintf("%.2f", l95), "-", sprintf("%.2f", u95), ")"))]
res[, pval_use:= ifelse(is.na(pval),"       -    ",sapply(pval, function(x) trans_pval(x)))]
