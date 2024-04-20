library(data.table)
library(dplyr)
library(tidyr)
setwd("./data/")
source("../scripts/LCAC-master/Rscript/FuncsCols_Revise.r")


dat_org <- fread("seer.csv")
vari_tab <- fread("../scripts/LCAC-master/Rscript/vari.csv")

test <- as.data.frame(colnames(dat_org))
colnames(test) <- "name_org"
test <- merge(test,vari_tab, by = "name_org",all.x = TRUE)
setnames(dat_org, test$name_org, test$name_set)


############ Data filtering ############
# Total number=386642
#age_unknown=12
#stage != "Blank(s)"|"N/A"|"Unknown" #remove 134187
#Type of Reporting Source != "Autopsy only" & "Death certificate"#remove 282
dat_filter1 <- dat_org[diag.confirm == "Positive histology"]#remove 27771
dat_filter1[, age_num:= as.numeric(ifelse(age_org == "Unknown", NA, ifelse(age_org == "90+ years", 90, gsub(" years", "", age_org, fixed = TRUE))))]
dat_filter5 <- dat_filter1[race != "Non-Hispanic Unknown Race"]#remove 948

dat_filter5[surg_prim == 0, dsurg_prim:= "No Surgery"]
dat_filter5[(surg_prim >= 10 & surg_prim <= 80) | surg_prim == 90, dsurg_prim:= "Surgery"]
dat_filter5[surg_prim %in% c(98, 99, 126), dsurg_prim:= "Unknown"]
dat_filter5[, dsurg_prim:= factor(dsurg_prim, levels = c("No Surgery", "Surgery", "Unknown"))]


dat_filter8 <- dat_filter5[surv.month != "Unknown"]
dat_final <- dat_filter8[!(cod.site == "State DC not available or state DC available but no COD")]
#remove 1630


vari_use <- c("patient.id", "age_num", "sex", "race", "marital", "rural", "income", 
              "year.diag", "site","kind","B_sym", "histology", "stage",
              "dsurg_prim", "reason_nosurg","radiation","chemotherapy","surg_rad",
                "cod.site",
              "surv.month", "vital"
)
dat_use <- dat_final[, .SD, .SDcols = vari_use]

############ Data re-code ############
dat_clean <- copy(dat_use)
dat_clean[age_num <=17, age:= "0-17 years"]
dat_clean[age_num >= 18 & age_num <= 44, age:= "18-44 years"]
dat_clean[age_num >= 45 & age_num <= 69, age:= "45-69 years"]
dat_clean[age_num >= 70, age:= "70+ years"]
dat_clean <- na.omit(dat_clean)
dat_clean[, age:= factor(age, levels = c("0-17 years","18-44 years", "45-69 years", "70+ years"))]
table(dat_clean$age)

dat_clean[, sex:= factor(sex, levels = c("Female", "Male"))]
dat_clean[, marital:= factor(marital, levels = c("Married (including common law)", "Unmarried or Domestic Partner", "Separated", "Single (never married)", "Divorced", "Widowed"), labels = c("Married", "Partner", "Separated", "Single", "Divorced", "Widowed"))]
dat_clean[, rural:= factor(rural, levels = c("Counties in metropolitan areas ge 1 million pop", "Counties in metropolitan areas of 250,000 to 1 million pop", "Counties in metropolitan areas of lt 250 thousand pop", "Nonmetropolitan counties adjacent to a metropolitan area", "Nonmetropolitan counties not adjacent to a metropolitan area", "Unknown/missing/no match (Alaska or Hawaii - Entire State)"), labels = c(">1Mmetro", "<1Mmetro", "<250Kmetro", "adjMetro", "nonMetro", "Alaska"))]
dat_clean[, vital:= factor(vital, levels = c("Alive", "Dead"))]

dat_clean[race %in% c("Non-Hispanic Asian or Pacific Islander", "Non-Hispanic American Indian/Alaska Native"), race:= "Other"]
dat_clean[, race:= factor(race, levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic (All Races)", "Other"), labels = c("White", "Black", "Hispanic", "Other"))]

dat_clean[income %in% c("$35,000 - $39,999", "$40,000 - $44,999", "$45,000 - $49,999"), income:= "$35,000 - $49,999"]
dat_clean[income %in% c("$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999", "$70,000 - $74,999"), income:= "$50,000 - $74,999"]
dat_clean[, income:= factor(income, levels = c("< $35,000", "$35,000 - $49,999", "$50,000 - $74,999", "$75,000+"), labels = c("< 35K", "35K-49K", "50K-74K", "75K+"))]

dat_clean[radiation %in% c("Beam radiation", "Radiation, NOS  method or source not specified", "Radioactive implants (includes brachytherapy) (1988+)", "Radioisotopes (1988+)", "Combination of beam with implants or isotopes"), radiation:= "Yes"]
dat_clean[radiation %in% c("None/Unknown", "Refused (1988+)", "Recommended, unknown if administered"), radiation:= "No"]
dat_clean[, radiation:= factor(radiation, levels = c("No", "Yes"))]

dat_clean[chemotherapy %in% c("No/Unknown"), chemotherapy := "No"]
dat_clean[chemotherapy %in% c("Yes"), chemotherapy := "Yes"]
dat_clean[, chemotherapy:= factor(chemotherapy, levels = c("No", "Yes"))]

levels_surg_rad <- c("No radiation and/or cancer-directed surgery", "Radiation prior to surgery", "Intraoperative radiation", "Radiation after surgery", "Radiation before and after surgery", "Surgery both before and after radiation", "Sequence unknown, but both were given")
labels_surg_rad <- c("None", "RadPri", "RadIntra", "RadAfter", "RadBoth", "SurgBoth", "Unknown")
dat_clean[surg_rad %in% c("Intraoperative rad with other rad before/after surgery", "Intraoperative radiation"), surg_rad:= "Intraoperative radiation"]
dat_clean[, surg_rad:= factor(surg_rad, levels = levels_surg_rad, label = labels_surg_rad)]

levels_reason <- c("Surgery performed", "Recommended but not performed, patient refused", "Not performed, patient died prior to recommended surgery", "Recommended but not performed, unknown reason", "Not recommended", "UnknownSurg")
labels_reason <- c("Performed", "Refused", "Died", "UnknownReas", "NotRecomm", "UnknownSurg")
dat_clean[reason_nosurg %in% c("Not recommended, contraindicated due to other cond; autopsy only (1973-2002)", "Not recommended"), reason_nosurg:= "Not recommended"]
dat_clean[reason_nosurg %in% c("Recommended, unknown if performed", "Unknown; death certificate; or autopsy only (2003+)"), reason_nosurg:= "UnknownSurg"]
dat_clean[, reason_nosurg:= factor(reason_nosurg, levels = levels_reason, labels_reason)]

cod_tab <- fread("../data/cod_type.csv")
for (itype in unique(cod_tab$Type)) {
  dat_clean[cod.site %in% cod_tab[Type == itype]$cod, cod:= itype]
}
dat_clean[, cod:= factor(cod, levels = c("Alive", "Benign.or.unknown", "Other.causes", "External.causes", "Infectious", "Repiratory", "Cardiovascular", "Diabetes.Mellitus", "Alzheimers", "Renal", "GI.and.liver", "Other.cancers","Lymphoma"))]
dat_clean[, caus.death:= factor(ifelse(cod == "Alive", "Alive", ifelse(cod == "Lymphoma", "Lymphoma", "nonLymphoma")), levels = c("Alive", "Lymphoma", "nonLymphoma"))]



dat_clean[surv.month == "Unknown", surv.month:= NA]
dat_clean[, surv.month:= as.numeric(surv.month)]
dat_clean[, surv.death:= ifelse(vital == "Dead", 1, 0)]
dat_clean$site <- as.factor(dat_clean$site)
dat_clean$stage <- as.factor(dat_clean$stage)
table(dat_clean$B_sym)
dat_clean[B_sym=="Blank(s)",B_sym:= NA]
dat_clean[B_sym=="Any B symptom(s)-Night sweats, fever, weight loss, NOS; Phys classified as B",B_sym:= "B"]
dat_clean[B_sym=="No B symptoms (asymptomatic); Classified as A by physician when asymptomatic",B_sym:= "A"]
dat_clean[B_sym=="Not documented in medical record; B symptoms not assessed or unknown if assessed",B_sym:= "unknown"]
dat_clean[,B_sym:= factor(B_sym)]

