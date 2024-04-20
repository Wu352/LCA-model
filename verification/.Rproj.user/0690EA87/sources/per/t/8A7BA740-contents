library(data.table)
library(dplyr)
library(tidyr)
setwd("./data/")
dat_org <- fread("DS_JCO.csv")


############ Data filtering ############

dat_filter1 <- dat_org[complete.cases(dat_org[,c(19:24)]),]
dat_filter1 <- dat_filter1[MAX_TUMOUR >= 0]
dat_filter2 <- dat_filter1[AGE != "n/a"]
dat_filter5 <- dat_filter2[STAGE == "Stage I"| STAGE == "Stage II"|STAGE == "Stage III"|STAGE == "Stage IV"]

name_org <- names(dat_filter5)[c(8:15,19,21,22,24)]
setnames(dat_filter5, name_org , c("age","sex","stage","ECOG","LDH","IPI","site","tumor_size","fustat","futime","PFS_stat","PFS"))
setnames(dat_filter5,c("GEO_sample_name","trial_class","RESP_ASSESS"),c("patient.id","subtype","Therapy_response"))


# ### Re-define Stage
dat_filter5[, sex:= factor(sex, levels = c("Female","Male"))]
dat_filter5[age < 60, age:= 0]
dat_filter5[age >= 60, age:= 1]
dat_filter5[, age:= factor(age, levels = c("0", "1"),labels = c("<60 years","60+ years"))]
dat_filter5[, stage:= factor(stage, levels = c("Stage I", "Stage II","Stage III","Stage IV"),labels = c("I/II","I/II","III/IV","III/IV"))]
dat_filter5[LDH < 500, LDH:= 0]
dat_filter5[LDH >= 500, LDH:= 1]
dat_filter5[, LDH:= factor(LDH, levels = c("0", "1"),labels = c("low","high"))]
dat_filter5[ECOG <= 1, ECOG:= 0]
dat_filter5[ECOG > 1, ECOG:= 1]
dat_filter5[, ECOG:= factor(ECOG, levels = c("0", "1"),labels = c("lower","higher"))]
dat_filter5[, site:= factor(site, levels = c("0", "1"),labels = c("NHL − Nodal","NHL − Extranodal"))]
dat_filter5[IPI <= 1, IPI:= 0]
dat_filter5[IPI > 1 & IPI <= 3, IPI:= 1]
dat_filter5[IPI > 3, IPI:= 2]
dat_filter5[, IPI:= factor(IPI, levels = c("0", "1","2"),labels = c("Low", "Intermediate","High"))]
dat_filter5[`tumor_size` <= 10, `tumor_size`:= 0]
dat_filter5[`tumor_size` > 10, `tumor_size`:= 1]
dat_filter5[, tumor_size:= factor(tumor_size, levels = c("0", "1"),labels = c("small","large"))]
dat_filter5[, subtype:= factor(subtype)]

dat_final <- dat_filter5


#############################

vari_use <- c("patient.id","subtype","age","sex","site","stage",
              "ECOG","LDH","tumor_size","IPI",
              "fustat","futime","PFS_stat","PFS"
)
dat_use <- dat_final[, .SD, .SDcols = vari_use]