pkgs_overall <- c("data.table", "reshape2", "ggplot2", "ggrepel", "scales", "paletteer")
pkgs_surv <- c("poLCA", "survival", "survminer", "cmprsk")
sapply(pkgs_overall, require, character.only = TRUE, quietly = TRUE)
sapply(pkgs_surv, require, character.only = TRUE, quietly = TRUE)
setwd("./data/")
library(dplyr)
source("../scripts/FuncsCols_Revise.r")

load("dat_base_JCO.RData")


dat_gi <- copy(dat_base)
dat_gi[, OS.new:=as.numeric(fustat)]
dat_gi[, OS.new.year:= as.numeric(futime/12)]
dat_gi[, PFS.new:=as.numeric(PFS_stat)]
dat_gi[, PFS.new.year:= as.numeric(PFS/12)]

dat_gi <- dat_gi[subtype != "FAIL"]
dat_gi <- droplevels(dat_gi)


ires_para <- data.frame()
  for (surv.type in c("PFS","OS")) {
    dat_score0 <- copy(dat_gi)
    setnames(dat_score0, isub, "group")
    dat_score0 <- dat_score0[complete.cases(dat_score0[, .SD, .SDcols = c("vclass", "group")])]
    comp_sub <- combn(levels(dat_score0$group), 2)
    
    for (icomp in 0:ncol(comp_sub)) {
      if (icomp == 0) {
        dat_score <- copy(dat_score0)
        ilab_comp <- "all"
        if (isub == "subtype") {
          cols <- col_cms[as.numeric(unique(dat_score$group))]
        }
      } else {
        dat_score <- dat_score0[group %in% comp_sub[, icomp]]
        if (isub == "subtype") {
          cols <- col_cms[as.numeric(unique(dat_score$group))]
        }
        dat_score <- droplevels(dat_score)
        ilab_comp <- paste(comp_sub[, icomp], collapse = "_")
      }
      
      if (min(table(dat_score$group)) >= 3) {
        ###### Original survival
        ifit <- eval(parse(text = paste0("survfit(as.formula(Surv(", surv.type, ".new.year, ", surv.type, ".new) ~ group), data = dat_score)")))
        idiff <- eval(parse(text = paste0("survdiff(as.formula(Surv(", surv.type, ".new.year, ", surv.type, ".new) ~ group), data = dat_score)")))
        iplgr <- get_psurv(idiff)
        plg_sig <- ifelse(iplgr < 0.001, "***", ifelse(iplgr < 0.01, "**", ifelse(iplgr < 0.05, "*", "")))
        
        icox <- eval(parse(text = paste0("coxph(as.formula(Surv(", surv.type, ".new.year, ", surv.type, ".new) ~ group), data = dat_score)")))
        icoef <- as.data.table(summary(icox)$coef, keep.rownames = TRUE)
        setnames(icoef, "Pr(>|z|)", "pval")
        icoef[, psig:= ifelse(pval < 0.001, "***", ifelse(pval < 0.01, "**", ifelse(pval < 0.05, "*", "")))]
        iconf <- as.data.table(summary(icox)$conf, keep.rownames = TRUE)
        ires <- merge(icoef[, .(rn, psig)], iconf, by = "rn")
        if (nrow(ires) == 1) {
          ihr <- paste0(sprintf("%.2f", ires[1, 3]),
                        " (",
                        sprintf("%.2f", ires[1, 5]),
                        "-",
                        sprintf("%.2f", ires[1, 6]),
                        ")",
                        ires[1, 2])
          ilabel <- paste0("P for log-rank test: ", iplgr, plg_sig, "\n", "HR: ", ihr)
        } else {
          ilabel <- paste0("P for log-rank test: ", iplgr, plg_sig)
          for (i in 1:nrow(ires)) {
            ilabel <- paste0(ilabel,
                             "\nHR of ",
                             sub("group", "", ires$rn[i]),
                             ": ",
                             sprintf("%.2f", ires[i, 3]),
                             " (",
                             sprintf("%.2f", ires[i, 5]),
                             "-",
                             sprintf("%.2f", ires[i, 6]), ")",
                             ires[i, 2])
          }
        }
        
        ires_para_org <- data.table(subtype=isub, 
                                    surv=surv.type,
                                    compare=ilab_comp,
                                    adjust=0,
                                    score=summary(icox)$sctest["test"],
                                    df=summary(icox)$sctest["df"],
                                    pscore=summary(icox)$sctest["pvalue"],
                                    concordance=summary(icox)$concordance["C"],
                                    se.cindex=summary(icox)$concordance["se(C)"])
        
        res_surv <- ggsurvplot(ifit,
                               pval = FALSE,
                               risk.table = TRUE,
                               risk.table.col = "strata",
                               fontsize = 3,
                               color = "strata",
                               palette = cols,
                               size = 0.5,
                               censor = TRUE,
                               censor.size = 2,
                               legend = c(0.9, 0.925),
                               legend.title = "",
                               legend.labs = gsub("group=", "", names(ifit$strata)),
                               ggtheme = theme_survminer(),
                               xlab = "Time (year)",
                               ylab = paste0("Survival probability (", surv.type, ")")
        )
        res_surv$plot <- res_surv$plot +
          ggplot2::annotate("text", x = 0, y = 0, label = ilabel, hjust = 0, vjust = 0, size = 3)
        
        
        ###### LCAC-adjusted survival
        idat_adj <- copy(dat_score)
        
        ### Scaled weighting
        index <- with(idat_adj, cbind(as.numeric(vclass), as.numeric(group)))
        tab1 <- with(idat_adj, table(vclass))/ nrow(idat_adj)
        tab2 <- with(idat_adj, table(vclass, group))/nrow(idat_adj)
        tab3 <- with(idat_adj, table(group)) / nrow(idat_adj)
        rwt <- rep(tab1, length(tab3))/tab2
        idat_adj$rwt <- rwt[index] # add per subject weights to the data set
        
        wtscale <- table(idat_adj$group)/tapply(idat_adj$rwt, idat_adj$group, sum)
        idat_adj$wt_use <- c(idat_adj$rwt * wtscale[idat_adj$group])
      }
      ifit_adj <- eval(parse(text = paste0("survfit(as.formula(Surv(", surv.type, ".new.year, ", surv.type, ".new) ~ group), data = idat_adj, weight=wt_use)")))
      iid <- 1:nrow(idat_adj)
      icox_adj <- eval(parse(text = paste0("coxph(as.formula(Surv(", surv.type, ".new.year, ", surv.type, ".new) ~ group), data = idat_adj, cluster=iid, weight=wt_use)")))
      iplgr_adj <- trans_pval(summary(icox_adj)$robscore["pvalue"])
      plg_sig_adj <- ifelse(iplgr_adj < 0.001, "***", ifelse(iplgr_adj < 0.01, "**", ifelse(iplgr_adj < 0.05, "*", "")))
      icoef_adj <- as.data.table(summary(icox_adj)$coef, keep.rownames = TRUE)
      setnames(icoef_adj, "Pr(>|z|)", "pval")
      icoef_adj[, psig:= ifelse(pval < 0.001, "***", ifelse(pval < 0.01, "**", ifelse(pval < 0.05, "*", "")))]
      iconf_adj <- as.data.table(summary(icox_adj)$conf, keep.rownames = TRUE)
      ires_adj <- merge(icoef_adj[, .(rn, psig)], iconf_adj, by = "rn")
      if (nrow(ires_adj) == 1) {
        ihr_adj <- paste0(sprintf("%.2f", ires_adj[1, 3]), " (", sprintf("%.2f", ires_adj[1, 5]), "-", sprintf("%.2f", ires_adj[1, 6]), ")", ires_adj[1, 2])
        ilabel_adj <- paste0("P for corrected log-rank test: ", iplgr_adj, plg_sig_adj, "\n", "HR: ", ihr_adj)
      } else {
        ilabel_adj <- paste0("P for corrected log-rank test: ", iplgr_adj, plg_sig_adj)
        for (i in 1:nrow(ires_adj)) {
          ilabel_adj <- paste0(ilabel_adj, "\nHR of ", sub("group", "", ires_adj$rn[i]), ": ", sprintf("%.2f", ires_adj[i, 3]), " (", sprintf("%.2f", ires_adj[i, 5]), "-", sprintf("%.2f", ires_adj[i, 6]), ")", ires_adj[i, 2])
        }
      }
      
      res_surv_adj <- ggsurvplot(ifit_adj,
                                 pval = FALSE,
                                 risk.table = TRUE,
                                 risk.table.col = "strata",
                                 fontsize = 3,
                                 color = "strata",
                                 palette = cols,
                                 size = 0.5,
                                 censor = TRUE,
                                 censor.size = 2,
                                 legend = c(0.9, 0.925), 
                                 legend.title = "",
                                 legend.labs = gsub("group=", "", names(ifit_adj$strata)),
                                 ggtheme = theme_survminer(), xlab = "Time (year)",
                                 ylab = paste0("Survival probability (", surv.type, ")")
      )
      res_surv_adj$plot <- res_surv_adj$plot +
        ggplot2::annotate("text", x = 0, y = 0, label = ilabel_adj, hjust = 0, vjust = 0, size = 3)
      
      ires_para_adj <- data.table(subtype=isub, 
                                  surv=surv.type,
                                  compare=ilab_comp, 
                                  adjust=1,
                                  score=summary(icox_adj)$robscore["test"],
                                  df=summary(icox_adj)$robscore["df"],
                                  pscore=summary(icox_adj)$robscore["pvalue"],
                                  concordance=summary(icox_adj)$concordance["C"],
                                  se.cindex=summary(icox_adj)$concordance["se(C)"])
      
      }
  }

