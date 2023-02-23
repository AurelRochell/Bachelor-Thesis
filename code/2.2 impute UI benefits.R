# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Bachelor thesis
# 2.2 impute UI benefits
# by Aurel Rochell  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Description ------------------------------------------------------------------
# This script imputes Economic Impact Payments in the analysis sample.


# Set up -----------------------------------------------------------------------
rm(list = ls())   # cleaning the environment
graphics.off()    # closing all plots

library(tidyverse)
library(openxlsx)
library(tidycensus)

setwd("../data/2 impute EIPs and UI benefits")  # needs to be adjusted accordingly


# prepare data -----------------------------------------------------------------
load("main sample EIPs.RData")

# create more general variables for occupation, industry, education and race
main.sample$occ = ifelse(main.sample$OCC %in% c(0, 9840), 0, ifelse(main.sample$OCC %in% c(10:960), 960, ifelse(main.sample$OCC %in% c(1005:3550), 3550,
                                                                                                                ifelse(main.sample$OCC %in% c(3601:4655), 4655, ifelse(main.sample$OCC %in% c(4700:4965), 4965, ifelse(main.sample$OCC %in% c(5000:5940), 5940,
                                                                                                                                                                                                                       ifelse(main.sample$OCC %in% c(6005:6130), 6130, ifelse(main.sample$OCC %in% c(6200:6950), 6950, ifelse(main.sample$OCC %in% c(7000:7640), 7640,
                                                                                                                                                                                                                                                                                                                              ifelse(main.sample$OCC %in% c(7700:8990), 8990, ifelse(main.sample$OCC %in% c(9005:9760), 9760, NA)))))))))))
main.sample$ind = ifelse(main.sample$IND == 0, 0, ifelse(main.sample$IND %in% c(170:290), 290, ifelse(main.sample$IND %in% c(370:490), 490,
                                                                                                      ifelse(main.sample$IND == 770, 770, ifelse(main.sample$IND %in% c(1070:3990), 3990, ifelse(main.sample$IND %in% c(4070:5790), 5790,
                                                                                                                                                                                                 ifelse(main.sample$IND %in% c(570:690, 6070:6390), 6390, ifelse(main.sample$IND %in% c(6470:6780), 6780, ifelse(main.sample$IND %in% c(6870:7190), 7190,
                                                                                                                                                                                                                                                                                                                 ifelse(main.sample$IND %in% c(7270:7790), 7790, ifelse(main.sample$IND %in% c(7860:8470), 8470, ifelse(main.sample$IND %in% c(8560:8690), 8690, 
                                                                                                                                                                                                                                                                                                                                                                                                                        ifelse(main.sample$IND %in% c(8770:9290), 9290, ifelse(main.sample$IND %in% c(9370:9590), 9590, ifelse(main.sample$IND == 9890, 9890,  NA)))))))))))))))
main.sample$educ = ifelse(main.sample$EDUC %in% c(1, 2, 10:14, 20, 21, 22, 30, 31, 32), 30, ifelse(main.sample$EDUC %in% c(40, 50, 60, 71), 71, ifelse(main.sample$EDUC == 73, 73, ifelse(main.sample$EDUC %in% c(80, 81, 90, 91, 92, 100, 110), 110,
                                                                                                                                                                                          ifelse(main.sample$EDUC == 111, 111, ifelse(main.sample$EDUC == 123, 123, ifelse(main.sample$EDUC == 124, 124, ifelse(main.sample$EDUC == 125, 125, NA))))))))
main.sample$race = ifelse(main.sample$RACE == 100, 100,
                          ifelse(main.sample$RACE == 200, 200,
                                 ifelse(main.sample$RACE %in% c(651, 652), 650,
                                        ifelse((main.sample$RACE == 300 | main.sample$RACE > 800), 800, 900))))


# identify benefit recipients --------------------------------------------------
# regular UI eligibility
main.sample$UI.eligible = ifelse(main.sample$EMPSTAT %in% c(21) & !(main.sample$CLASSWKR %in% c(13, 14) & main.sample$WHYUNEMP %in% c(1, 2, 3)), 1, 0)
main.sample$UI.eligible = ifelse(main.sample$YEAR == 2020 & main.sample$MONTH %in% c(1, 2, 3), 0, main.sample$UI.eligible)
main.sample$UI.eligible = ifelse(main.sample$YEAR == 2020 & main.sample$MONTH %in% c(1, 2, 3), 0, main.sample$UI.eligible)

# PUA eligibility
main.sample$PUA.eligible = ifelse((main.sample$EMPSTAT %in% c(21) & main.sample$CLASSWKR %in% c(13, 14)) |
                                    (main.sample$EMPSTAT %in% c(22)) |
                                    (main.sample$EMPSTAT %in% c(12) & (main.sample$WHYABSNT %in% c(6, 7, 8, 15) | (main.sample$COVIDUNAW == 2 & !is.na(main.sample$COVIDUNAW)))) |
                                    (main.sample$EMPSTAT %in% c(34) & (main.sample$WNLOOK %in% c(1, 2, 6, 7, 11) | (main.sample$COVIDLOOK == 2 & !is.na(main.sample$COVIDLOOK)))) & (main.sample$COVIDPAID != 2 | is.na(main.sample$COVIDPAID)) & main.sample$UI.eligible == 0, 1, 0)
main.sample$PUA.eligible = ifelse(main.sample$YEAR == 2020 & main.sample$MONTH %in% c(1, 2, 3), 0, main.sample$PUA.eligible)

# create variables to determine UI status in the following month
load("UI_next_month.RData")
unemp.data2 = Data.UI.next.month[which(Data.UI.next.month$MISH == 2),names(Data.UI.next.month) %in% c("CPSIDP", "DURUNEMP", "EMPSTAT", "CLASSWKR", "WHYABSNT", "WNLOOK", "COVIDLOOK", "COVIDUNAW", "COVIDPAID")]
main.sample = merge(main.sample, unemp.data2, by = "CPSIDP", all.x = TRUE)
main.sample = dplyr::rename(main.sample, "DURUNEMP" = DURUNEMP.x, "DURUNEMP2" = DURUNEMP.y, "EMPSTAT" = EMPSTAT.x, "EMPSTAT2" = EMPSTAT.y, "CLASSWKR" = CLASSWKR.x, "CLASSWKR2" = CLASSWKR.y,
                            "WHYABSNT" = WHYABSNT.x, "WHYABSNT2" = WHYABSNT.y, "WNLOOK" = WNLOOK.x, "WNLOOK2" = WNLOOK.y, "COVIDLOOK" = COVIDLOOK.x, "COVIDLOOK2" = COVIDLOOK.y, "COVIDUNAW" = COVIDUNAW.x,
                            "COVIDUNAW2" = COVIDUNAW.y, "COVIDPAID" = COVIDPAID.x, "COVIDPAID2" = COVIDPAID.y)
main.sample$DURUNEMP2[which(is.na(main.sample$DURUNEMP2) & main.sample$MISH == 1)] = 999
main.sample$EMPSTAT2[which(is.na(main.sample$EMPSTAT2) & main.sample$MISH == 1)] = main.sample$EMPSTAT[which(is.na(main.sample$EMPSTAT2) & main.sample$MISH == 1)]
main.sample$CLASSWKR2[which(is.na(main.sample$CLASSWKR2) & main.sample$MISH == 1)] = main.sample$CLASSWKR[which(is.na(main.sample$CLASSWKR2) & main.sample$MISH == 1)]
main.sample$WHYABSNT2[which(is.na(main.sample$WHYABSNT2) & main.sample$MISH == 1)] = ifelse(main.sample$EMPSTAT2[which(is.na(main.sample$WHYABSNT2) & main.sample$MISH == 1)] == 10, 0, main.sample$WHYABSNT[which(is.na(main.sample$WHYABSNT2) & main.sample$MISH == 1)])
main.sample$WNLOOK2[which(is.na(main.sample$WNLOOK2) & main.sample$MISH == 1)] = ifelse(main.sample$EMPSTAT2[which(is.na(main.sample$WNLOOK2) & main.sample$MISH == 1)] == 10, 0, main.sample$WNLOOK[which(is.na(main.sample$WNLOOK2) & main.sample$MISH == 1)])
main.sample$COVIDLOOK2[which(is.na(main.sample$COVIDLOOK2) & main.sample$MISH == 1)] = ifelse(is.na(main.sample$COVIDLOOK[which(is.na(main.sample$COVIDLOOK2) & main.sample$MISH == 1)]), 1, main.sample$COVIDLOOK[which(is.na(main.sample$COVIDLOOK2) & main.sample$MISH == 1)])
main.sample$COVIDUNAW2[which(is.na(main.sample$COVIDUNAW2) & main.sample$MISH == 1)] = ifelse(is.na(main.sample$COVIDUNAW[which(is.na(main.sample$COVIDUNAW2) & main.sample$MISH == 1)]), 1, main.sample$COVIDUNAW[which(is.na(main.sample$COVIDUNAW2) & main.sample$MISH == 1)])
main.sample$COVIDPAID2[which(is.na(main.sample$COVIDPAID2) & main.sample$MISH == 1)] = ifelse(is.na(main.sample$COVIDPAID[which(is.na(main.sample$COVIDPAID2) & main.sample$MISH == 1)]), 1, main.sample$COVIDPAID[which(is.na(main.sample$COVIDPAID2) & main.sample$MISH == 1)])
main.sample$PUA.eligible2 = ifelse((main.sample$EMPSTAT2 %in% c(21) & main.sample$CLASSWKR2 %in% c(13, 14)) |
                                     (main.sample$EMPSTAT2 %in% c(12) & main.sample$WHYABSNT2 %in% c(6, 7, 8, 15)) |
                                     (main.sample$WNLOOK2 %in% c(1, 2, 6, 7, 11) | main.sample$COVIDLOOK2 == 2 | main.sample$COVIDUNAW2 == 2) & main.sample$COVIDPAID2 != 2, 1, 0)
unemp.data6 = Data.UI.next.month[which(Data.UI.next.month$MISH == 6),names(Data.UI.next.month) %in% c("CPSIDP", "DURUNEMP", "EMPSTAT", "CLASSWKR", "WHYABSNT", "WNLOOK", "COVIDLOOK", "COVIDUNAW", "COVIDPAID")]
main.sample = merge(main.sample, unemp.data6, by = "CPSIDP", all.x = TRUE)
main.sample = dplyr::rename(main.sample, "DURUNEMP" = DURUNEMP.x, "DURUNEMP6" = DURUNEMP.y, "EMPSTAT" = EMPSTAT.x, "EMPSTAT6" = EMPSTAT.y, "CLASSWKR" = CLASSWKR.x, "CLASSWKR6" = CLASSWKR.y,
                            "WHYABSNT" = WHYABSNT.x, "WHYABSNT6" = WHYABSNT.y, "WNLOOK" = WNLOOK.x, "WNLOOK6" = WNLOOK.y, "COVIDLOOK" = COVIDLOOK.x, "COVIDLOOK6" = COVIDLOOK.y, "COVIDUNAW" = COVIDUNAW.x,
                            "COVIDUNAW6" = COVIDUNAW.y, "COVIDPAID" = COVIDPAID.x, "COVIDPAID6" = COVIDPAID.y)
main.sample$DURUNEMP6[which(is.na(main.sample$DURUNEMP6) & main.sample$MISH == 5)] = 999
main.sample$EMPSTAT6[which(is.na(main.sample$EMPSTAT6) & main.sample$MISH == 5)] = main.sample$EMPSTAT[which(is.na(main.sample$EMPSTAT6) & main.sample$MISH == 5)]
main.sample$CLASSWKR6[which(is.na(main.sample$CLASSWKR6) & main.sample$MISH == 5)] = main.sample$CLASSWKR[which(is.na(main.sample$CLASSWKR6) & main.sample$MISH == 5)]
main.sample$WHYABSNT6[which(is.na(main.sample$WHYABSNT6) & main.sample$MISH == 5)] = ifelse(main.sample$EMPSTAT6[which(is.na(main.sample$WHYABSNT6) & main.sample$MISH == 5)] == 10, 0, main.sample$WHYABSNT[which(is.na(main.sample$WHYABSNT6) & main.sample$MISH == 5)])
main.sample$WNLOOK6[which(is.na(main.sample$WNLOOK6) & main.sample$MISH == 5)] = ifelse(main.sample$EMPSTAT6[which(is.na(main.sample$WNLOOK6) & main.sample$MISH == 5)] == 10, 0, main.sample$WNLOOK[which(is.na(main.sample$WNLOOK6) & main.sample$MISH == 5)])
main.sample$COVIDLOOK6[which(is.na(main.sample$COVIDLOOK6) & main.sample$MISH == 5)] = ifelse(is.na(main.sample$COVIDLOOK[which(is.na(main.sample$COVIDLOOK6) & main.sample$MISH == 5)]), 5, main.sample$COVIDLOOK[which(is.na(main.sample$COVIDLOOK6) & main.sample$MISH == 5)])
main.sample$COVIDUNAW6[which(is.na(main.sample$COVIDUNAW6) & main.sample$MISH == 5)] = ifelse(is.na(main.sample$COVIDUNAW[which(is.na(main.sample$COVIDUNAW6) & main.sample$MISH == 5)]), 5, main.sample$COVIDUNAW[which(is.na(main.sample$COVIDUNAW6) & main.sample$MISH == 5)])
main.sample$COVIDPAID6[which(is.na(main.sample$COVIDPAID6) & main.sample$MISH == 5)] = ifelse(is.na(main.sample$COVIDPAID[which(is.na(main.sample$COVIDPAID6) & main.sample$MISH == 5)]), 5, main.sample$COVIDPAID[which(is.na(main.sample$COVIDPAID6) & main.sample$MISH == 5)])
main.sample$PUA.eligible6 = ifelse((main.sample$EMPSTAT6 %in% c(21) & main.sample$CLASSWKR6 %in% c(13, 14)) |
                                     (main.sample$EMPSTAT6 %in% c(12) & main.sample$WHYABSNT6 %in% c(6, 7, 8, 15)) |
                                     (main.sample$WNLOOK6 %in% c(1, 2, 6, 7, 11) | main.sample$COVIDLOOK6 == 2 | main.sample$COVIDUNAW6 == 2) & main.sample$COVIDPAID6 != 2, 1, 0)
rm(unemp.data2, unemp.data6, Data.UI.next.month)


# Match pre-separation earnings ------------------------------------------------
load("Earnings_matching.RData")
Earnings.matching = Earnings.matching[which(Earnings.matching$EARNWEEK != 9999.99),]  # create data for scaling imputed preseparation earnings

# use earner study supplement to match weekly earnings
main.sample = merge(main.sample, Earnings.matching[Earnings.matching$MISH == 4, which(names(Earnings.matching) %in% c("EARNWEEK", "CPSIDP"))], by = "CPSIDP", all.x = TRUE)
main.sample = dplyr::rename(main.sample, EARNWEEK4 = EARNWEEK.y, EARNWEEK = EARNWEEK.x)
main.sample = merge(main.sample, Earnings.matching[Earnings.matching$MISH == 8, which(names(Earnings.matching) %in% c("EARNWEEK", "CPSIDP"))], by = "CPSIDP", all.x = TRUE)
main.sample = rename(main.sample, EARNWEEK8 = EARNWEEK.y, EARNWEEK = EARNWEEK.x)
rm(Earnings.matching)

# check for differences between preseparation and postseparation earnings
mean(main.sample$EARNWEEK8[!is.na(main.sample$EARNWEEK4) & !is.na(main.sample$EARNWEEK8) & main.sample$UI.eligible == 1 & main.sample$MISH == 5] - main.sample$EARNWEEK4[!is.na(main.sample$EARNWEEK4) & !is.na(main.sample$EARNWEEK8) & main.sample$UI.eligible == 1 & main.sample$MISH == 5])

# use preseparation earnings when possible, otherwise use later earnings
main.sample$presepEARNmat = case_when(main.sample$MISH == 5 & !is.na(main.sample$EARNWEEK4) ~ main.sample$EARNWEEK4,
                                      main.sample$MISH == 5 & !is.na(main.sample$EARNWEEK8) ~ main.sample$EARNWEEK8,
                                      main.sample$MISH == 1 & !is.na(main.sample$EARNWEEK4) ~ main.sample$EARNWEEK4,
                                      main.sample$MISH == 1 & !is.na(main.sample$EARNWEEK8) ~ main.sample$EARNWEEK8)
main.sample$presepEARNmat[which((main.sample$UI.eligible == 0 & (main.sample$PUA.eligible == 0 | main.sample$CLASSWKR %in% c(13, 14))))] = NA
length(which(main.sample$UI.eligible == 1 & !is.na(main.sample$presepEARNmat))) / length(which(main.sample$UI.eligible == 1))  # for about 54% of UI eligible
length(which(main.sample$PUA.eligible == 1 & !is.na(main.sample$presepEARNmat) & main.sample$EMPSTAT != 22)) / length(which(main.sample$PUA.eligible == 1 & main.sample$EMPSTAT != 22))  # for about 37% of PUA eligible individuals which wba is based on employment income


# impute preseparation wage income ---------------------------------------------
# load data
load("raw data 2020-2021.RData")
main.sample$presepEARNimp = NA
model.data = raw.data[raw.data$AGE >= 15 & !(raw.data$CLASSWKR %in% c(13, 14)) & ((raw.data$EMPSTAT == 21 & raw.data$WHYUNEMP %in% c(1, 2, 3)) | 
                                                                                    (raw.data$EMPSTAT == 12 & (raw.data$WHYABSNT %in% c(6, 7, 8, 15) | (raw.data$COVIDUNAW == 2 & !is.na(raw.data$COVIDUNAW)))) |
                                                                                    (raw.data$EMPSTAT %in% c(34) & (raw.data$WNLOOK %in% c(1, 2, 6, 7, 11) | (raw.data$COVIDLOOK == 2 & !is.na(raw.data$COVIDLOOK))))),which(names(raw.data) != "EARNWEEK")]
model.data = inner_join(model.data, raw.data[raw.data$MISH %in% c(4, 8) & raw.data$EARNWEEK != 9999.99,which(names(raw.data) %in% c("CPSIDP", "EARNWEEK"))], by = c("CPSIDP"))
rm(raw.data)

# prepare data
model.data$GQTYPE = ifelse(model.data$GQTYPE == 2, 1, ifelse(model.data$GQTYPE == 5, 3, ifelse(model.data$GQTYPE == 10, 9, ifelse(model.data$GQTYPE == 8, 6, model.data$GQTYPE))))
model.data$occ = ifelse(model.data$OCC %in% c(0, 9840), 0, ifelse(model.data$OCC %in% c(10:960), 960, ifelse(model.data$OCC %in% c(1005:3550), 3550,
                                                                                                             ifelse(model.data$OCC %in% c(3601:4655), 4655, ifelse(model.data$OCC %in% c(4700:4965), 4965, ifelse(model.data$OCC %in% c(5000:5940), 5940,
                                                                                                                                                                                                                  ifelse(model.data$OCC %in% c(6005:6130), 6130, ifelse(model.data$OCC %in% c(6200:6950), 6950, ifelse(model.data$OCC %in% c(7000:7640), 7640,
                                                                                                                                                                                                                                                                                                                       ifelse(model.data$OCC %in% c(7700:8990), 8990, ifelse(model.data$OCC %in% c(9005:9760), 9760, NA)))))))))))
model.data$ind = ifelse(model.data$IND %in% c(0, 9890), 0, ifelse(model.data$IND %in% c(170:290), 290, ifelse(model.data$IND %in% c(370:490), 490,
                                                                                                              ifelse(model.data$IND == 770, 770, ifelse(model.data$IND %in% c(1070:3990), 3990, ifelse(model.data$IND %in% c(4070:5790), 5790,
                                                                                                                                                                                                       ifelse(model.data$IND %in% c(570:690, 6070:6390), 6390, ifelse(model.data$IND %in% c(6470:6780), 6780, ifelse(model.data$IND %in% c(6870:7190), 7190,
                                                                                                                                                                                                                                                                                                                     ifelse(model.data$IND %in% c(7270:7790), 7790, ifelse(model.data$IND %in% c(7860:8470), 8470, ifelse(model.data$IND %in% c(8560:8690), 8690, 
                                                                                                                                                                                                                                                                                                                                                                                                                          ifelse(model.data$IND %in% c(8770:9290), 9290, ifelse(model.data$IND %in% c(9370:9590), 9590, NA))))))))))))))
model.data$educ = ifelse(model.data$EDUC %in% c(1, 2, 10:14, 20, 21, 22, 30, 31, 32), 30, ifelse(model.data$EDUC %in% c(40, 50, 60, 71), 71, ifelse(model.data$EDUC == 73, 73, ifelse(model.data$EDUC %in% c(80, 81, 90, 91, 92, 100, 110), 110,
                                                                                                                                                                                      ifelse(model.data$EDUC == 111, 111, ifelse(model.data$EDUC == 123, 123, ifelse(model.data$EDUC == 124, 124, ifelse(model.data$EDUC == 125, 125, NA))))))))
model.data$race = ifelse(model.data$RACE == 100, 100,
                         ifelse(model.data$RACE == 200, 200,
                                ifelse(model.data$RACE %in% c(651, 652), 650,
                                       ifelse((model.data$RACE == 300 | model.data$RACE > 800), 800, 900))))
model.data$yexp = ifelse(model.data$EDUC == 20, 6, ifelse(model.data$EDUC == 30, 8, ifelse(model.data$EDUC == 40, 9, ifelse(model.data$EDUC == 50, 10, ifelse(model.data$EDUC == 60, 11, ifelse(model.data$EDUC == 71 | model.data$EDUC == 73, 12, ifelse(model.data$EDUC == 81, 13, ifelse(model.data$EDUC == 91 | model.data$EDUC == 92, 14, ifelse(model.data$EDUC == 111, 16, ifelse(model.data$EDUC == 123, 18, ifelse(model.data$EDUC == 124 | model.data$EDUC == 125, 20, 4)))))))))))
model.data$yexp = pmax(model.data$AGE - model.data$yexp - 6, 0)
model.data[,c("GQTYPE", "REGION", "CBSASZ", "CITIZEN", "occ", "ind", "educ", "DIFFANY", "SEX", "race", "EMPSTAT")] = lapply(model.data[,c("GQTYPE", "REGION", "CBSASZ", "CITIZEN", "occ", "ind", "educ", "DIFFANY", "SEX", "race", "EMPSTAT")], factor)

# compute linear model
model = lm(formula = EARNWEEK ~ GQTYPE + REGION + CBSASZ + NFAMS + AGE + SEX + race + FAMSIZE + NCHILD + CITIZEN + occ + ind + educ + DIFFANY + yexp + DURUNEMP:EMPSTAT + EMPSTAT, data = model.data)

# impute weekly earnings based on linear model
model.data = main.sample[which((main.sample$UI.eligible == 1 | (main.sample$PUA.eligible == 1 & !(main.sample$CLASSWKR %in% c(13, 14)))) & !(main.sample$OCC %in% c(0, 9840))),]
model.data$GQTYPE = ifelse(model.data$GQTYPE == 2, 1, ifelse(model.data$GQTYPE == 5, 3, ifelse(model.data$GQTYPE == 10, 9, ifelse(model.data$GQTYPE == 8, 6, model.data$GQTYPE))))
model.data$yexp = ifelse(model.data$EDUC == 20, 6, ifelse(model.data$EDUC == 30, 8, ifelse(model.data$EDUC == 40, 9, ifelse(model.data$EDUC == 50, 10, ifelse(model.data$EDUC == 60, 11, ifelse(model.data$EDUC == 71 | model.data$EDUC == 73, 12, ifelse(model.data$EDUC == 81, 13, ifelse(model.data$EDUC == 91 | model.data$EDUC == 92, 14, ifelse(model.data$EDUC == 111, 16, ifelse(model.data$EDUC == 123, 18, ifelse(model.data$EDUC == 124 | model.data$EDUC == 125, 20, 4)))))))))))
model.data$yexp = pmax(model.data$AGE - model.data$yexp - 6, 0)
model.data[,c("GQTYPE", "REGION", "CBSASZ", "CITIZEN", "occ", "ind", "educ", "DIFFANY", "SEX", "race", "EMPSTAT")] = lapply(model.data[,c("GQTYPE", "REGION", "CBSASZ", "CITIZEN", "occ", "ind", "educ", "DIFFANY", "SEX", "race", "EMPSTAT")], factor)
main.sample$presepEARNimp[which((main.sample$UI.eligible == 1 | (main.sample$PUA.eligible == 1 & !(main.sample$CLASSWKR %in% c(13, 14)))) & !(main.sample$OCC %in% c(0, 9840)))] = predict(model, newdata = model.data)
main.sample$presepEARNimp[which((main.sample$UI.eligible == 1 | (main.sample$PUA.eligible == 1 & !(main.sample$CLASSWKR %in% c(13, 14)))) & main.sample$OCC %in% c(0, 9840))] = mean(main.sample$presepEARNimp, na.rm = TRUE)

rm(model, model.data)


# impute preseparation self-employment income ----------------------------------
# load data
load("ASEC merge wages.RData")
model.data = ASEC[ASEC$ASECFLAG == 1 & !is.na(ASEC$ASECFLAG) & ASEC$YEAR %in% c(2020, 2021), which(names(ASEC) %in% c("YEAR", "CPSIDP", "INCWAGE", "INCFARM", "INCBUS"))]
model.data[,which(names(model.data) %in% c("INCWAGE", "INCFARM", "INCBUS"))] = apply(model.data[,which(names(model.data) %in% c("INCWAGE", "INCFARM", "INCBUS"))], 2, FUN = function(x) as.integer(gsub("99999999", "0", x)))
model.data = inner_join(model.data, ASEC[(ASEC$ASECFLAG == 2 | is.na(ASEC$ASECFLAG)) & (ASEC$EMPSTAT == 21 | ASEC$EMPSTAT == 12) & ASEC$CLASSWKR %in% c(13, 14),which(!(names(ASEC) %in% c("INCWAGE", "INCFARM", "INCBUS")))], by = c("YEAR", "CPSIDP"))
model.data$income = model.data$INCWAGE + model.data$INCBUS + model.data$INCFARM
rm(ASEC)

# prepare data
model.data$GQTYPE = ifelse(model.data$GQTYPE == 2, 1, ifelse(model.data$GQTYPE == 5, 3, ifelse(model.data$GQTYPE == 10, 9, ifelse(model.data$GQTYPE == 8, 6, model.data$GQTYPE))))
model.data$occ = ifelse(model.data$OCC %in% c(0, 9840), 0, ifelse(model.data$OCC %in% c(10:960), 960, ifelse(model.data$OCC %in% c(1005:3550), 3550,
                                                                                                             ifelse(model.data$OCC %in% c(3601:4655), 4655, ifelse(model.data$OCC %in% c(4700:4965), 4965, ifelse(model.data$OCC %in% c(5000:5940), 5940,
                                                                                                                                                                                                                  ifelse(model.data$OCC %in% c(6005:6130), 6130, ifelse(model.data$OCC %in% c(6200:6950), 6950, ifelse(model.data$OCC %in% c(7000:7640), 7640,
                                                                                                                                                                                                                                                                                                                       ifelse(model.data$OCC %in% c(7700:8990), 8990, ifelse(model.data$OCC %in% c(9005:9760), 9760, NA)))))))))))
model.data$ind = ifelse(model.data$IND %in% c(0, 9890), 0, ifelse(model.data$IND %in% c(170:290), 290, ifelse(model.data$IND %in% c(370:490), 490,
                                                                                                              ifelse(model.data$IND == 770, 770, ifelse(model.data$IND %in% c(1070:3990), 3990, ifelse(model.data$IND %in% c(4070:5790), 5790,
                                                                                                                                                                                                       ifelse(model.data$IND %in% c(570:690, 6070:6390), 6390, ifelse(model.data$IND %in% c(6470:6780), 6780, ifelse(model.data$IND %in% c(6870:7190), 7190,
                                                                                                                                                                                                                                                                                                                     ifelse(model.data$IND %in% c(7270:7790), 7790, ifelse(model.data$IND %in% c(7860:8470), 8470, ifelse(model.data$IND %in% c(8560:8690), 8690, 
                                                                                                                                                                                                                                                                                                                                                                                                                          ifelse(model.data$IND %in% c(8770:9290), 9290, ifelse(model.data$IND %in% c(9370:9590), 9590, NA))))))))))))))
model.data$educ = ifelse(model.data$EDUC %in% c(1, 2, 10:14, 20, 21, 22, 30, 31, 32), 30, ifelse(model.data$EDUC %in% c(40, 50, 60, 71), 71, ifelse(model.data$EDUC == 73, 73, ifelse(model.data$EDUC %in% c(80, 81, 90, 91, 92, 100, 110), 110,
                                                                                                                                                                                      ifelse(model.data$EDUC == 111, 111, ifelse(model.data$EDUC == 123, 123, ifelse(model.data$EDUC == 124, 124, ifelse(model.data$EDUC == 125, 125, NA))))))))
model.data$race = ifelse(model.data$RACE == 100, 100,
                         ifelse(model.data$RACE == 200, 200,
                                ifelse(model.data$RACE %in% c(651, 652), 650,
                                       ifelse((model.data$RACE == 300 | model.data$RACE > 800), 800, 900))))
model.data$yexp = ifelse(model.data$EDUC == 20, 6, ifelse(model.data$EDUC == 30, 8, ifelse(model.data$EDUC == 40, 9, ifelse(model.data$EDUC == 50, 10, ifelse(model.data$EDUC == 60, 11, ifelse(model.data$EDUC == 71 | model.data$EDUC == 73, 12, ifelse(model.data$EDUC == 81, 13, ifelse(model.data$EDUC == 91 | model.data$EDUC == 92, 14, ifelse(model.data$EDUC == 111, 16, ifelse(model.data$EDUC == 123, 18, ifelse(model.data$EDUC == 124 | model.data$EDUC == 125, 20, 4)))))))))))
model.data$yexp = pmax(model.data$AGE - model.data$yexp - 6, 0)
model.data[,c("GQTYPE", "REGION", "CBSASZ", "CITIZEN", "occ", "ind", "educ", "DIFFANY", "SEX", "race", "EMPSTAT")] = lapply(model.data[,c("GQTYPE", "REGION", "CBSASZ", "CITIZEN", "occ", "ind", "educ", "DIFFANY", "SEX", "race", "EMPSTAT")], factor)

# compute linear model
model = lm(formula = income ~ YEAR + GQTYPE + REGION + CBSASZ + NFAMS + AGE + SEX + race + FAMSIZE + NCHILD + CITIZEN + occ + ind + educ + DIFFANY + yexp + EMPSTAT + EMPSTAT:DURUNEMP, data = model.data)

# impute weekly earnings based on linear model
model.data = main.sample[which((main.sample$PUA.eligible == 1 & main.sample$CLASSWKR %in% c(13, 14)) & !(main.sample$OCC %in% c(0, 9840))),]
model.data$GQTYPE = ifelse(model.data$GQTYPE == 2, 1, ifelse(model.data$GQTYPE == 5, 3, ifelse(model.data$GQTYPE == 10, 9, ifelse(model.data$GQTYPE == 8, 6, model.data$GQTYPE))))
model.data$yexp = ifelse(model.data$EDUC == 20, 6, ifelse(model.data$EDUC == 30, 8, ifelse(model.data$EDUC == 40, 9, ifelse(model.data$EDUC == 50, 10, ifelse(model.data$EDUC == 60, 11, ifelse(model.data$EDUC == 71 | model.data$EDUC == 73, 12, ifelse(model.data$EDUC == 81, 13, ifelse(model.data$EDUC == 91 | model.data$EDUC == 92, 14, ifelse(model.data$EDUC == 111, 16, ifelse(model.data$EDUC == 123, 18, ifelse(model.data$EDUC == 124 | model.data$EDUC == 125, 20, 4)))))))))))
model.data$yexp = pmax(model.data$AGE - model.data$yexp - 6, 0)
model.data[,c("GQTYPE", "REGION", "CBSASZ", "CITIZEN", "occ", "ind", "educ", "DIFFANY", "SEX", "race", "EMPSTAT")] = lapply(model.data[,c("GQTYPE", "REGION", "CBSASZ", "CITIZEN", "occ", "ind", "educ", "DIFFANY", "SEX", "race", "EMPSTAT")], factor)
main.sample$presepEARNimp[which((main.sample$PUA.eligible == 1 & main.sample$CLASSWKR %in% c(13, 14)) & !(main.sample$OCC %in% c(0, 9840)))] = predict(model, newdata = model.data) / 52
main.sample$presepEARNimp[which((main.sample$PUA.eligible == 1 & main.sample$CLASSWKR %in% c(13, 14)) & main.sample$OCC %in% c(0, 9840))] = mean(main.sample$presepEARNimp[which((main.sample$PUA.eligible == 1 & main.sample$CLASSWKR %in% c(13, 14)) & !(main.sample$OCC %in% c(0, 9840)))], na.rm = TRUE)

rm(model, model.data)


# calculate weekly benefit amount ----------------------------------------------
# preseparation earnings
main.sample$presepEARN = ifelse(main.sample$UI.eligible == 1 | main.sample$PUA.eligible == 1, 
                                ifelse(is.na(main.sample$presepEARNmat), main.sample$presepEARNimp * 52, main.sample$presepEARNmat * 52),
                                NA)
main.sample$presepEARN[which(main.sample$EMPSTAT == 22 & main.sample$PUA.eligible == 1)] = 0
main.sample$presepEARN[which(main.sample$YEAR == 2020 & main.sample$quart == 1)] = NA

# compute wba
main.sample$wba = NA
for(i in which(!is.na(main.sample$presepEARN))) {
  if(main.sample$STATEFIP[i] %in% c(1, 5, 11, 12, 16, 27, 28, 31, 37, 39, 45, 46, 47, 30)) {
    main.sample$wba[i] = (1/26) * (main.sample$presepEARN[i] / 4)
  }
  if(main.sample$STATEFIP[i] %in% c(4, 29, 32, 48, 51, 55, 56)) {
    main.sample$wba[i] = (1/25) * (main.sample$presepEARN[i] / 4)
  }
  if(main.sample$STATEFIP[i] == 2) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$wba[i] = 0.0155 * main.sample$presepEARN[i] + min(24 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 72)
    } else {
      main.sample$wba[i] = 0.0155 * main.sample$presepEARN[i] + 75 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i])
    }
  }
  if(main.sample$STATEFIP[i] == 6) {
    main.sample$wba[i] = (1/24.5) * (main.sample$presepEARN[i] / 4)
  }
  if(main.sample$STATEFIP[i] == 8) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$wba[i] = max(min(0.6 * (1/26) * (main.sample$presepEARN[i] / 2), 561), min(0.5 * (main.sample$presepEARN[i] / 52), 618))
    } else {
      main.sample$wba[i] = max(min(0.6 * (1/26) * (main.sample$presepEARN[i] / 2), 590), min(0.5 * (main.sample$presepEARN[i] / 52), 649))
    }
  }
  if(main.sample$STATEFIP[i] == 9) {
    main.sample$wba[i] = (1/26) * (main.sample$presepEARN[i] / 4) + min(min(15 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 75), (1/26) * (main.sample$presepEARN[i] / 4))
  }
  if(main.sample$STATEFIP[i] == 10) {
    main.sample$wba[i] = (1/46) * (main.sample$presepEARN[i] / 2)
  }
  if(main.sample$STATEFIP[i] %in% c(13, 15)) {
    main.sample$wba[i] = (1/42) * (main.sample$presepEARN[i] / 2)
  }
  if(main.sample$STATEFIP[i] %in%  c(17, 18)) {
    main.sample$wba[i] =  0.47 * (main.sample$presepEARN[i] / 52)
  }
  if(main.sample$STATEFIP[i] == 19) {
    main.sample$wba[i] = (main.sample$presepEARN[i] / 4) * ifelse((main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]) == 0, (1/23),
                                                                  ifelse((main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]) == 1, (1/22),
                                                                         ifelse((main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]) == 2, (1/21),
                                                                                ifelse((main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]) == 3, (1/20), (1/19)))))
  }
  if(main.sample$STATEFIP[i] == 20) {
    main.sample$wba[i] =  0.0425 * (main.sample$presepEARN[i] / 4)
  }
  if(main.sample$STATEFIP[i] == 21) {
    main.sample$wba[i] =  0.011923 * main.sample$presepEARN[i]
  }
  if(main.sample$STATEFIP[i] == 22) {
    main.sample$wba[i] = (1/25) * (main.sample$presepEARN[i] / 4) * 1.05 * 1.15
  }
  if(main.sample$STATEFIP[i] == 23) {
    main.sample$wba[i] = (1/22) * (main.sample$presepEARN[i] / 4) + min(10 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), (1/22) * (main.sample$presepEARN[i] / 4) * 0.5)
  }
  if(main.sample$STATEFIP[i] == 24) {
    main.sample$wba[i] = (1/24) * (main.sample$presepEARN[i] / 4) + min(8 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 40)
  }
  if(main.sample$STATEFIP[i] == 25) {
    main.sample$wba[i] = 0.5 * (main.sample$presepEARN[i] / 52) + min(25 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 0.25 * (main.sample$presepEARN[i] / 52))
  }
  if(main.sample$STATEFIP[i] == 26) {
    main.sample$wba[i] = 0.041 * (main.sample$presepEARN[i] / 4) + min(6 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 30)
  }
  if(main.sample$STATEFIP[i] == 33) {
    main.sample$wba[i] =  0.0105 * main.sample$presepEARN[i]
  }
  if(main.sample$STATEFIP[i] == 34) {
    main.sample$wba[i] =  0.6 * (main.sample$presepEARN[i] / 52)
  }
  if(main.sample$STATEFIP[i] == 35) {
    main.sample$wba[i] =  0.535 * (main.sample$presepEARN[i] / 52)
  }
  if(main.sample$STATEFIP[i] == 36) {
    main.sample$wba[i] =  (1/25.5) * (main.sample$presepEARN[i] / 4)
  }
  if(main.sample$STATEFIP[i] == 38) {
    main.sample$wba[i] =  (1/65) * ((main.sample$presepEARN[i] / 4) * 2.5)
  }
  if(main.sample$STATEFIP[i] == 40) {
    main.sample$wba[i] =  (1/23) * (main.sample$presepEARN[i] / 4)
  }
  if(main.sample$STATEFIP[i] == 41) {
    main.sample$wba[i] =  0.0125 * main.sample$presepEARN[i]
  }
  if(main.sample$STATEFIP[i] == 42) {
    main.sample$wba[i] =  0.04 * (main.sample$presepEARN[i] / 4) * 0.98 + ifelse((main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]) == 1, 5, ifelse((main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]) >= 2, 8, 0))
  }
  if(main.sample$STATEFIP[i] == 44) {
    main.sample$wba[i] =  0.0385 * (main.sample$presepEARN[i] / 4) + min(ifelse(0.05 * (0.0385 * (main.sample$presepEARN[i] / 4)) > 15, 
                                                                                0.05 * (0.0385 * (main.sample$presepEARN[i] / 4)) * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]),
                                                                                15 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i])), max(50, 0.25 * (0.0385 * (main.sample$presepEARN[i] / 4))))
  }
  if(main.sample$STATEFIP[i] == 49) {
    main.sample$wba[i] = (1/26) * (main.sample$presepEARN[i] / 4) - 5
  }
  if(main.sample$STATEFIP[i] == 50) {
    main.sample$wba[i] = (1/45) * (main.sample$presepEARN[i] / 2)
  }
  if(main.sample$STATEFIP[i] == 53) {
    main.sample$wba[i] = 0.0385 * (main.sample$presepEARN[i] / 4)
  }
  if(main.sample$STATEFIP[i] == 54) {
    main.sample$wba[i] = 0.55 * (main.sample$presepEARN[i] / 52)
  }
}

# apply caps
caps = data.frame("STATEFIP" = c(1, 5, 6, 10, 11, 12, 13, 18, 22, 28, 29, 37, 45, 47, 51, 54, 55),
                  "minWB" = c(45, 81, 40, 20, 50, 32, 55, 37, 10, 30, 35, 15, 42, 30, 60, 24, 54),
                  "maxWB" = c(275, 451, 450, 400, 444, 275, 365, 390, 247, 235, 320, 350, 326, 275, 378, 424, 370))
main.sample = merge(main.sample, caps, by = "STATEFIP", all.x = TRUE)
for(i in which(is.na(main.sample$maxWB) & !(is.na(main.sample$presepEARN)))) {
  if(main.sample$STATEFIP[i] == 2) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 56 + min(24 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 72)
      main.sample$maxWB[i] = 370 + min(24 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 72)
    } else {
      main.sample$minWB[i] = 56 + min(24 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 72)
      main.sample$maxWB[i] = 370 + min(24 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 72)
    }
  }
  if(main.sample$STATEFIP[i] == 4) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 187
      main.sample$maxWB[i] = 240
    } else {
      main.sample$minWB[i] = 190
      main.sample$maxWB[i] = 240
    }
  }
  if(main.sample$STATEFIP[i] == 8) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 25
      main.sample$maxWB[i] = 618
    } else {
      main.sample$minWB[i] = 25
      main.sample$maxWB[i] = 649
    }
  }
  if(main.sample$STATEFIP[i] == 9) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 15
      main.sample$maxWB[i] = 649 + min(15 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 75)
    } else {
      main.sample$minWB[i] = 15
      main.sample$maxWB[i] = min(667 + min(15 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 75), 724)
    }
  }
  if(main.sample$STATEFIP[i] == 15) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 5
      main.sample$maxWB[i] = 648
    } else {
      main.sample$minWB[i] = 5
      main.sample$maxWB[i] = 639
    }
  }
  if(main.sample$STATEFIP[i] == 16) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 72
      main.sample$maxWB[i] = 448
    } else {
      main.sample$minWB[i] = 72
      main.sample$maxWB[i] = 463
    }
  }
  if(main.sample$STATEFIP[i] == 17) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 51
      main.sample$maxWB[i] = 667
    } else {
      main.sample$minWB[i] = 51
      main.sample$maxWB[i] = 693
    }
  }
  if(main.sample$STATEFIP[i] == 19) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 72 + min(4 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 15)
      main.sample$maxWB[i] = 481 + min(28 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 110)
    } else {
      main.sample$minWB[i] = 73 + min(4 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 16)
      main.sample$maxWB[i] = 605 + min(28 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 112)
    }
  }
  if(main.sample$STATEFIP[i] == 20) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 122
      main.sample$maxWB[i] = 488
    } else {
      main.sample$minWB[i] = 125
      main.sample$maxWB[i] = 503
    }
  }
  if(main.sample$STATEFIP[i] == 21) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 39
      main.sample$maxWB[i] = 552
    } else {
      main.sample$minWB[i] = 39
      main.sample$maxWB[i] = 569
    }
  }
  if(main.sample$STATEFIP[i] == 23) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 77 + min(10 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 77 * 0.5)
      main.sample$maxWB[i] = 445 + min(10 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 445 * 0.5)
    } else {
      main.sample$minWB[i] = 80 + min(10 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 80 * 0.5)
      main.sample$maxWB[i] = 462 + min(10 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 462 * 0.5)
    }
  }
  if(main.sample$STATEFIP[i] == 24) {
    main.sample$minWB[i] = 50 + min(5 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 40)
    main.sample$maxWB[i] = 430
  }
  if(main.sample$STATEFIP[i] == 25) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 98 + min(25 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 98 * 0.5)
      main.sample$maxWB[i] = 823 + min(25 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 823 * 0.5)
    } else {
      main.sample$minWB[i] = 103 + min(25 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 103 * 0.5)
      main.sample$maxWB[i] = 855 + min(25 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 855 * 0.5)
    }
  }
  if(main.sample$STATEFIP[i] == 26) {
    main.sample$minWB[i] = 150 + min(6 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]), 30)
    main.sample$maxWB[i] = 362
  }
  if(main.sample$STATEFIP[i] == 27) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 28
      main.sample$maxWB[i] = 740
    } else {
      main.sample$minWB[i] = 29
      main.sample$maxWB[i] = 762
    }
  }
  if(main.sample$STATEFIP[i] == 30) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 163
      main.sample$maxWB[i] = 552
    } else {
      main.sample$minWB[i] = 169
      main.sample$maxWB[i] = 572
    }
  }
  if(main.sample$STATEFIP[i] == 31) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 70
      main.sample$maxWB[i] = 440
    } else {
      main.sample$minWB[i] = 70
      main.sample$maxWB[i] = 456
    }
  }
  if(main.sample$STATEFIP[i] == 32) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 16
      main.sample$maxWB[i] = 469
    } else {
      main.sample$minWB[i] = 16
      main.sample$maxWB[i] = 483
    }
  }
  if(main.sample$STATEFIP[i] == 33) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 32
      main.sample$maxWB[i] = 427
    } else {
      main.sample$minWB[i] = 100
      main.sample$maxWB[i] = 427
    }
  }
  if(main.sample$STATEFIP[i] == 34) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 120
      main.sample$maxWB[i] = 713
    } else {
      main.sample$minWB[i] = 132
      main.sample$maxWB[i] = 731
    }
  }
  if(main.sample$STATEFIP[i] == 35) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 86
      main.sample$maxWB[i] = 461
    } else {
      main.sample$minWB[i] = 90
      main.sample$maxWB[i] = 484
    }
  }
  if(main.sample$STATEFIP[i] == 36) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 104
      main.sample$maxWB[i] = 504
    } else {
      main.sample$minWB[i] = 108
      main.sample$maxWB[i] = 504
    }
  }
  if(main.sample$STATEFIP[i] == 38) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 43
      main.sample$maxWB[i] = 618
    } else {
      main.sample$minWB[i] = 43
      main.sample$maxWB[i] = 640
    }
  }
  if(main.sample$STATEFIP[i] == 39) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 135
      main.sample$maxWB[i] = 480
    } else {
      main.sample$minWB[i] = 140
      main.sample$maxWB[i] = 498
    }
  }
  if(main.sample$STATEFIP[i] == 40) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 16
      main.sample$maxWB[i] = 539
    } else {
      main.sample$minWB[i] = 16
      main.sample$maxWB[i] = 461
    }
  }
  if(main.sample$STATEFIP[i] == 41) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 151
      main.sample$maxWB[i] = 648
    } else {
      main.sample$minWB[i] = 157
      main.sample$maxWB[i] = 673
    }
  }
  if(main.sample$STATEFIP[i] == 42) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 68  + ifelse((main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]) == 1, 5, ifelse((main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]) >= 2, 8, 0))
      main.sample$maxWB[i] = 572 + ifelse((main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]) == 1, 5, ifelse((main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]) >= 2, 8, 0))
    } else {
      main.sample$minWB[i] = 68 + ifelse((main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]) == 1, 5, ifelse((main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]) >= 2, 8, 0))
      main.sample$maxWB[i] = 583 + ifelse((main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]) == 1, 5, ifelse((main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]) >= 2, 8, 0))
    }
  }
  if(main.sample$STATEFIP[i] == 44) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 53 + min(ifelse(0.05 * 53 > 15, 0.05 * 53 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]),
                                             15 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i])), max(50, 0.25 * 53))
      main.sample$maxWB[i] = 586 + min(ifelse(0.05 * 586 > 15, 0.05 * 586 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]),
                                              15 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i])), max(50, 0.25 * 586))
    } else {
      main.sample$minWB[i] = 59 + min(ifelse(0.05 * 59 > 15, 0.05 * 59 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]),
                                             15 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i])), max(50, 0.25 * 59))
      main.sample$maxWB[i] = 599 + min(ifelse(0.05 * 599 > 15, 0.05 * 599 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i]),
                                              15 * (main.sample$qualifying.children[i] + main.sample$qualifying.dependent[i])), max(50, 0.25 * 599))
    }
  }
  if(main.sample$STATEFIP[i] == 46) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 28
      main.sample$maxWB[i] = 414
    } else {
      main.sample$minWB[i] = 28
      main.sample$maxWB[i] = 428
    }
  }
  if(main.sample$STATEFIP[i] == 48) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 69
      main.sample$maxWB[i] = 521
    } else {
      main.sample$minWB[i] = 70
      main.sample$maxWB[i] = 535
    }
  }
  if(main.sample$STATEFIP[i] == 49) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 32
      main.sample$maxWB[i] = 580
    } else {
      main.sample$minWB[i] = 35
      main.sample$maxWB[i] = 617
    }
  }
  if(main.sample$STATEFIP[i] == 50) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 72
      main.sample$maxWB[i] = 513
    } else {
      main.sample$minWB[i] = 74
      main.sample$maxWB[i] = 531
    }
  }
  if(main.sample$STATEFIP[i] == 53) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 188
      main.sample$maxWB[i] = 790
    } else {
      main.sample$minWB[i] = 201
      main.sample$maxWB[i] = 844
    }
  }
  if(main.sample$STATEFIP[i] == 56) {
    if(main.sample$YEAR[i] == 2020) {
      main.sample$minWB[i] = 36
      main.sample$maxWB[i] = 508
    } else {
      main.sample$minWB[i] = 38
      main.sample$maxWB[i] = 526
    }
  }
}
main.sample$wba[which(!is.na(main.sample$presepEARN))] = ifelse(main.sample$wba[which(!is.na(main.sample$presepEARN))] < main.sample$minWB[which(!is.na(main.sample$presepEARN))],
                                                                main.sample$minWB[which(!is.na(main.sample$presepEARN))], main.sample$wba[which(!is.na(main.sample$presepEARN))])
main.sample$wba[which(!is.na(main.sample$presepEARN))] = ifelse(main.sample$wba[which(!is.na(main.sample$presepEARN))] > main.sample$maxWB[which(!is.na(main.sample$presepEARN))],
                                                                main.sample$maxWB[which(!is.na(main.sample$presepEARN))], main.sample$wba[which(!is.na(main.sample$presepEARN))])

# apply additional minimum for PUA
min.PUA = data.frame("STATEFIP" = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
                     "min.PUA" = c(113, 133, 117, 132, 167, 223, 198, 133, 179, 125, 149, 263, 168, 198, 149, 203, 192, 176, 107, 172, 176, 267, 160, 234, 106, 133, 184, 173, 181, 168, 230, 169, 178, 132, 228, 189, 189, 205, 195, 183, 131, 172, 120, 207, 211, 190, 158, 235, 158, 163, 193))
main.sample  = merge(main.sample, min.PUA, by = "STATEFIP", all.x = TRUE)
main.sample$PUA.wba = NA
main.sample$PUA.wba[which(main.sample$PUA.eligible == 1 & !is.na(main.sample$wba))] = ifelse(main.sample$wba[which(main.sample$PUA.eligible == 1 & !is.na(main.sample$wba))] < main.sample$min.PUA[which(main.sample$PUA.eligible == 1 & !is.na(main.sample$wba))], main.sample$min.PUA[which(main.sample$PUA.eligible == 1 & !is.na(main.sample$wba))], main.sample$wba[which(main.sample$PUA.eligible == 1 & !is.na(main.sample$wba))])
rm(min.PUA, caps)


# create groups for capping ----------------------------------------------------
# retrieve recipiency rates from DoL and create ranks
recipiency.rates = read.xlsx("Recipiency Rates States.xlsx")
recipiency.rates$X15 = as.numeric(recipiency.rates$X15)
recipiency.rates = recipiency.rates[!(recipiency.rates$State %in% c("PR", "AS", "GU", "VI", "US") | (recipiency.rates$Year == 2020 & recipiency.rates$Quarter == 1)),]
Rank = mutate(aggregate(cbind(X15) ~ State,  recipiency.rates, FUN = mean), "rank" = NA)
Rank$rank[order(Rank$X15)] = 1:nrow(Rank)
recipiency.rates = left_join(recipiency.rates, Rank[,which(names(Rank) %in% c("State", "rank"))], by = c("State"))
recipiency.rates = recipiency.rates %>% mutate(
  group = case_when(
    rank %in% 1:17 ~ 1, rank %in% 18:34 ~ 2, rank %in% 35:51 ~ 3
  )
)
recipiency.rates = unique(recipiency.rates[-1, which(names(recipiency.rates) %in% c("State", "group"))])


# create caps for regular State UI ---------------------------------------------
State.UI = read_csv("State UI Activities.csv")
State.UI$YEAR = c(rep(2020, 648), rep(2021, 648))
State.UI$MONTH = rep(c(rep(1, 54), rep(2, 54), rep(3, 54), rep(4, 54), rep(5, 54), rep(6, 54), rep(7, 54), rep(8, 54), rep(9, 54), rep(10, 54), rep(11, 54), rep(12, 54)), 2)

# add grouping for capping
State.UI = left_join(State.UI, unique(fips_codes[,which(names(fips_codes) %in% c("state", "state_code", "state_name"))]), by = c("STATE" = "state_name"))
State.UI$state_code = as.integer(State.UI$state_code)
State.UI = left_join(State.UI, recipiency.rates, by = c("state" = "State"))
State.UI$group[which(is.na(State.UI$group))] = 0

# compute aggregate amounts
State.UI = State.UI[!(State.UI$YEAR == 2020 & State.UI$MONTH %in% 1:3),]
State.UI$MONTHid = c(rep(1, 54), rep(2, 54), rep(3, 54), rep(4, 54), rep(5, 54), rep(6, 54), rep(7, 54), rep(8, 54), rep(9, 54), rep(10, 54), rep(11, 54), rep(12, 54), 
                     rep(13, 54), rep(14, 54), rep(15, 54), rep(16, 54), rep(17, 54), rep(18, 54), rep(19, 54), rep(20, 54), rep(21, 54))
State.UI$aggregated.amount = rep(NA, 1134)
for(i in 1:nrow(State.UI)) {
  State.UI$aggregated.amount[i] = sum(0.5 * State.UI$`Benefits Paid`[State.UI$STATE == State.UI$STATE[i] & State.UI$MONTHid == State.UI$MONTHid[i]],
                                      State.UI$`Benefits Paid`[State.UI$STATE == State.UI$STATE[i] & State.UI$MONTHid < State.UI$MONTHid[i] & State.UI$MONTHid > State.UI$MONTHid[i] - 12],
                                      State.UI$`Benefits Paid`[State.UI$STATE == State.UI$STATE[i] & State.UI$MONTHid == State.UI$MONTHid[i] - 12] * 0.5)
}


# create caps for PEUC program -------------------------------------------------
PEUC = read.csv("PEUC Activities.csv")
PEUC = PEUC[,which(names(PEUC) %in% c("st", "rptdate", "c35", "c29"))]
PEUC$YEAR = as.integer(word(PEUC$rptdate, 3, 3, sep = fixed("/")))
PEUC$MONTH = as.integer(word(PEUC$rptdate, 1, 1, sep = fixed("/")))

# add grouping for capping
PEUC = left_join(PEUC, unique(fips_codes[,which(names(fips_codes) %in% c("state", "state_code", "state_name"))]), by = c("st" = "state"))
PEUC$state_code = as.integer(PEUC$state_code)
PEUC = left_join(PEUC, recipiency.rates, by = c("st" = "State"))
PEUC$group[which(is.na(PEUC$group))] = 0

# compute aggregate amounts
PEUC = PEUC[!((PEUC$YEAR == 2020 & PEUC$MONTH %in% 1:3) | PEUC$YEAR > 2021),]
PEUC$MONTHid = rep(1:21, 53)
PEUC$aggregated.amount = rep(NA, 1113)
for(i in 1:nrow(PEUC)) {
  PEUC$aggregated.amount[i] = sum(0.5 * PEUC$c35[PEUC$st == PEUC$st[i] & PEUC$MONTHid == PEUC$MONTHid[i]],
                                  PEUC$c35[PEUC$st == PEUC$st[i] & PEUC$MONTHid < PEUC$MONTHid[i] & PEUC$MONTHid > PEUC$MONTHid[i] - 12],
                                  PEUC$c35[PEUC$st == PEUC$st[i] & PEUC$MONTHid == PEUC$MONTHid[i] - 12] * 0.5)
}


# create caps for PUA program --------------------------------------------------
PUA = read.csv("PUA Activities.csv")
PUA = PUA[,which(names(PUA) %in% c("st", "rptdate", "c6", "c5"))]
PUA = rbind(PUA, data.frame("st" = c("ID", "RI"), "rptdate" = rep("10/31/2021", 2), "c6" = rep(0, 2), "c5" = rep(0, 2)))
PUA$YEAR = as.integer(word(PUA$rptdate, 3, 3, sep = fixed("/")))
PUA$MONTH = as.integer(word(PUA$rptdate, 1, 1, sep = fixed("/")))

# add grouping for capping
PUA = left_join(PUA, unique(fips_codes[,which(names(fips_codes) %in% c("state", "state_code", "state_name"))]), by = c("st" = "state"))
PUA$state_code = as.integer(PUA$state_code)
PUA = left_join(PUA, recipiency.rates, by = c("st" = "State"))
PUA$group[which(is.na(PUA$group))] = 0

# compute aggregate amounts
PUA = PUA[!((PUA$YEAR == 2020 & PUA$MONTH %in% 1:3) | PUA$YEAR > 2021),]
PUA = left_join(PUA, unique(PEUC[,which(names(PEUC) %in% c("YEAR", "MONTH", "MONTHid"))]), by = c("YEAR", "MONTH"))
PUA$aggregated.amount = rep(NA, 1113)
for(i in 1:nrow(PUA)) {
  PUA$aggregated.amount[i] = sum(0.5 * PUA$c6[PUA$st == PUA$st[i] & PUA$MONTHid == PUA$MONTHid[i]],
                                 PUA$c6[PUA$st == PUA$st[i] & PUA$MONTHid < PUA$MONTHid[i] & PUA$MONTHid > PUA$MONTHid[i] - 12],
                                 PUA$c6[PUA$st == PUA$st[i] & PUA$MONTHid == PUA$MONTHid[i] - 12] * 0.5)
}
rm(recipiency.rates, Rank)

# merge group id to analysis sample
main.sample = left_join(main.sample, State.UI[,which(names(State.UI) %in% c("state_code", "YEAR", "MONTH", "group"))], by = c("STATEFIP" = "state_code", "YEAR", "MONTH"))


# compute regular UI recipiency ------------------------------------------------
# cap number of weeks for which UI can be received
reg.UI.max.weeks = data.frame("STATEFIP" = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
                              "max.weeks.UI" = c(20, 26, 26, 16, 26, 26, 26, 26, 26, 19, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 20, 28, 26, 26, 26, 26, 26, 26, 16, 26, 26, 26, 26, 26, 26, 20, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26))
main.sample = merge(main.sample, reg.UI.max.weeks, by = "STATEFIP", all.x = TRUE)
rm(reg.UI.max.weeks)

# cap weeks of CARES act provisions
UI.max.weeks = data.frame("YEAR" = c(rep(2020, 12), rep(2021, 12)),
                          "MONTH" = c(1:12, 1:12),
                          "weeks.passed" = c(0, 0, 0, 2, 7, 11, 15, 20, 24, 29, 33, 37, 42, 46, 50, 55, 59, 63, 68, 72, 76, 81, 85, 89),
                          "weeks.passed.Jul20" = c(0, 0, 0, 0, 0, 0, 0, 3, 7, 12, 16, 20, 25, 29, 33, 38, 42, 46, 51, 55, 59, 64, 68, 72),
                          "weeks.passed.Dez20" = c(rep(0, 12), 3, 7, 11, 16, 20, 24, 29, 33, 37, 42, 46, 50),
                          "expired.weeks.Jun" = c(rep(0, 18), 4, 8, 12, 17, 21, 25),
                          "expired.weeks.Sep" = c(rep(0, 20), 1, 6, 10, 14),
                          "expired.weeks.Lousiana" = c(rep(0, 19), 2, 6, 11, 15, 19),
                          "expired.weeks.Tennessee" = c(rep(0, 18), 2, 6, 10, 15, 19, 23),
                          "expired.weeks.Arizona" = c(rep(0, 18), 1, 5, 9, 14, 18, 22))
main.sample = merge(main.sample, UI.max.weeks, by = c("YEAR", "MONTH"), all.x = TRUE)
rm(UI.max.weeks)

# compute amount of regular UI for each individual received which we observe 
# (apply caps, includes amount of UI not qualifying for FPUC)
i = which(main.sample$UI.eligible == 1 & !is.na(main.sample$wba))
main.sample$UI = 0
main.sample$UI[i] = main.sample$wba[i] * (pmax(ifelse(main.sample$DURUNEMP[i] < main.sample$weeks.passed[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$weeks.passed[i]), 0) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0)), 0))

# compute amount FPUC for each individual under regular UI (apply caps and 
# subtract the weeks during which no FPUC was paid out)
main.sample$FPUC.UI.2020 = 0
main.sample$FPUC.UI.2021 = 0
for (i in which(main.sample$UI.eligible == 1 & !is.na(main.sample$wba))) {
  if(main.sample$STATEFIP[i] %in% c(1, 2, 5, 12, 13, 16, 19, 28, 29, 30, 31, 33, 38, 39, 40, 45, 46, 48, 49, 54, 56)) {
    main.sample$FPUC.UI.2020[i] = 600 * pmax((ifelse(main.sample$DURUNEMP[i] < main.sample$weeks.passed[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$weeks.passed[i]), 0) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0)) -
                                                ifelse(main.sample$DURUNEMP[i] < main.sample$weeks.passed.Jul20[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$weeks.passed.Jul20[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$weeks.passed.Jul20[i]), 0) - pmax(pmin(main.sample$weeks.passed.Jul20[i], main.sample$DURUNEMP[i]) - 52, 0))), 0)
    main.sample$FPUC.UI.2021[i] = 300 * pmax((ifelse(main.sample$DURUNEMP[i] < main.sample$weeks.passed.Dez20[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$weeks.passed.Dez20[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i]), 0) - pmax(pmin(main.sample$weeks.passed.Dez20[i], main.sample$DURUNEMP[i]) - 52, 0)) -
                                                ifelse(main.sample$DURUNEMP[i] < main.sample$expired.weeks.Jun[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$expired.weeks.Jun[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$expired.weeks.Jun[i]), 0) - pmax(pmin(main.sample$expired.weeks.Jun[i], main.sample$DURUNEMP[i]) - 52, 0))), 0)
  } else {
    if(main.sample$STATEFIP[i] == 47) {
      main.sample$FPUC.UI.2020[i] = 600 * pmax((ifelse(main.sample$DURUNEMP[i] < main.sample$weeks.passed[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$weeks.passed[i]), 0) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0)) -
                                                  ifelse(main.sample$DURUNEMP[i] < main.sample$weeks.passed.Jul20[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$weeks.passed.Jul20[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$weeks.passed.Jul20[i]), 0) - pmax(pmin(main.sample$weeks.passed.Jul20[i], main.sample$DURUNEMP[i]) - 52, 0))), 0)
      main.sample$FPUC.UI.2021[i] = 300 * pmax((ifelse(main.sample$DURUNEMP[i] < main.sample$weeks.passed.Dez20[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$weeks.passed.Dez20[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i]), 0) - pmax(pmin(main.sample$weeks.passed.Dez20[i], main.sample$DURUNEMP[i]) - 52, 0)) -
                                                  ifelse(main.sample$DURUNEMP[i] < main.sample$expired.weeks.Tennessee[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$expired.weeks.Tennessee[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$expired.weeks.Tennessee[i]), 0) - pmax(pmin(main.sample$expired.weeks.Tennessee[i], main.sample$DURUNEMP[i]) - 52, 0))), 0)
    } else {
      if(main.sample$STATEFIP[i] == 22) {
        main.sample$FPUC.UI.2020[i] = 600 * pmax((ifelse(main.sample$DURUNEMP[i] < main.sample$weeks.passed[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$weeks.passed[i]), 0) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0)) -
                                                    ifelse(main.sample$DURUNEMP[i] < main.sample$weeks.passed.Jul20[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$weeks.passed.Jul20[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$weeks.passed.Jul20[i]), 0) - pmax(pmin(main.sample$weeks.passed.Jul20[i], main.sample$DURUNEMP[i]) - 52, 0))), 0)
        main.sample$FPUC.UI.2021[i] = 300 * pmax((ifelse(main.sample$DURUNEMP[i] < main.sample$weeks.passed.Dez20[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$weeks.passed.Dez20[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i]), 0) - pmax(pmin(main.sample$weeks.passed.Dez20[i], main.sample$DURUNEMP[i]) - 52, 0)) -
                                                    ifelse(main.sample$DURUNEMP[i] < main.sample$expired.weeks.Lousiana[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$expired.weeks.Lousiana[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$expired.weeks.Lousiana[i]), 0) - pmax(pmin(main.sample$expired.weeks.Lousiana[i], main.sample$DURUNEMP[i]) - 52, 0))), 0)
      } else {
        if(main.sample$STATEFIP[i] == 4) {
          main.sample$FPUC.UI.2020[i] = 600 * pmax((ifelse(main.sample$DURUNEMP[i] < main.sample$weeks.passed[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$weeks.passed[i]), 0) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0)) -
                                                      ifelse(main.sample$DURUNEMP[i] < main.sample$weeks.passed.Jul20[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$weeks.passed.Jul20[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$weeks.passed.Jul20[i]), 0) - pmax(pmin(main.sample$weeks.passed.Jul20[i], main.sample$DURUNEMP[i]) - 52, 0))), 0)
          main.sample$FPUC.UI.2021[i] = 300 * pmax((ifelse(main.sample$DURUNEMP[i] < main.sample$weeks.passed.Dez20[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$weeks.passed.Dez20[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i]), 0) - pmax(pmin(main.sample$weeks.passed.Dez20[i], main.sample$DURUNEMP[i]) - 52, 0)) -
                                                      ifelse(main.sample$DURUNEMP[i] < main.sample$expired.weeks.Arizona[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$expired.weeks.Arizona[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$expired.weeks.Arizona[i]), 0) - pmax(pmin(main.sample$expired.weeks.Arizona[i], main.sample$DURUNEMP[i]) - 52, 0))), 0)
        } else {
          main.sample$FPUC.UI.2020[i] = 600 * pmax((ifelse(main.sample$DURUNEMP[i] < main.sample$weeks.passed[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$weeks.passed[i]), 0) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0)) -
                                                      ifelse(main.sample$DURUNEMP[i] < main.sample$weeks.passed.Jul20[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$weeks.passed.Jul20[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$weeks.passed.Jul20[i]), 0) - pmax(pmin(main.sample$weeks.passed.Jul20[i], main.sample$DURUNEMP[i]) - 52, 0))), 0)
          main.sample$FPUC.UI.2021[i] = 300 * pmax((ifelse(main.sample$DURUNEMP[i] < main.sample$weeks.passed.Dez20[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$weeks.passed.Dez20[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i]), 0) - pmax(pmin(main.sample$weeks.passed.Dez20[i], main.sample$DURUNEMP[i]) - 52, 0)) -
                                                      ifelse(main.sample$DURUNEMP[i] < main.sample$expired.weeks.Sep[i], pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - pmax(pmin(main.sample$expired.weeks.Sep[i], main.sample$DURUNEMP[i]) - 52, 0), pmax(pmin(main.sample$DURUNEMP[i], main.sample$max.weeks.UI[i]) - (main.sample$DURUNEMP[i] - main.sample$expired.weeks.Sep[i]), 0) - pmax(pmin(main.sample$expired.weeks.Sep[i], main.sample$DURUNEMP[i]) - 52, 0))), 0)
        }
      }
    }
  }
}


# simulate amount of regular UI to match with administrative values 
UI.coverage = data.frame("YEAR" = c(rep(2020, 9), rep(2021, 12)),
                         "MONTH" = c(4:12, 1:12),
                         "aggregated.amount" = c(9164335000, 30185360000, 53074997000, 73404501000, 90877345000, 105746377000, 116602758000, 124105135000, 130638609000, 136642528000, 141985791000, 147193100000, 142736315000, 125708393000, 106771141000, 90164180000, 76144000000, 64341994000, 55957744000, 50695732000, 46370000000),
                         "allocated.amount" = rep(NA, 21),
                         "average.wba" = rep(NA, 21))

main.sample$initial.mon = 24
main.sample$weeks.after = 0
main.sample$initial.CPSIDP = main.sample$CPSIDP
for(i in 1:nrow(UI.coverage)) {
  for (k in which(main.sample$YEAR == UI.coverage$YEAR[i-1] & main.sample$MONTH == UI.coverage$MONTH[i-1] & main.sample$UI > 0 & main.sample$initial.mon >= (i-12) & (ifelse(main.sample$MISH == 1, main.sample$EMPSTAT2, main.sample$EMPSTAT6) != 21 | main.sample$UI.eligible == 0))) {
    temp = main.sample[k,]
    if (length(which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$UI == 0 & main.sample$UI.eligible == 0 & main.sample$EMPSTAT == 10 & main.sample$PUA.eligible == 0 & between(main.sample$FTOTVAL, temp$FTOTVAL * 9/10, max(temp$FTOTVAL * 11/10, temp$FTOTVAL + 3000)) & main.sample$group == temp$group)) > 0) {
      j = sample(length(which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$UI == 0 & main.sample$UI.eligible == 0 & main.sample$EMPSTAT == 10 & main.sample$PUA.eligible == 0 & between(main.sample$FTOTVAL, temp$FTOTVAL * 9/10, max(temp$FTOTVAL * 11/10, temp$FTOTVAL + 3000)) & main.sample$group == temp$group)), 1)
      j = which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$UI == 0 & main.sample$UI.eligible == 0 & main.sample$EMPSTAT == 10 & main.sample$PUA.eligible == 0 & between(main.sample$FTOTVAL, temp$FTOTVAL * 9/10, max(temp$FTOTVAL * 11/10, temp$FTOTVAL + 3000)) & main.sample$group == temp$group)[j]
      main.sample$weeks.after[j] = ifelse(temp$initial.mon == 24, ifelse((temp$DURUNEMP + 3) <= main.sample$max.weeks.UI[j], sample(0:3, 1), 
                                                                         ifelse((temp$DURUNEMP+ 2) <= main.sample$max.weeks.UI[j], sample(0:2, 1),
                                                                                ifelse((temp$DURUNEMP + 1) <= main.sample$max.weeks.UI[j], sample(0:1, 1), 0))), temp$weeks.after)
      main.sample$UI[j] = ifelse(temp$initial.mon == 24, temp$UI / temp$wba, min(temp$UI / temp$wba, max((52 - (main.sample$weeks.passed[j] - main.sample$weeks.passed[which(main.sample$YEAR == UI.coverage$YEAR[temp$initial.mon] & main.sample$MONTH == UI.coverage$MONTH[temp$initial.mon])])) - ifelse(temp$DURUNEMP > temp$max.weeks.UI, temp$DURUNEMP - temp$max.weeks.UI, 0) + temp$weeks.after, 0))) * temp$wba + ifelse(temp$initial.mon == 24, main.sample$weeks.after[j] * temp$wba, 0)
      main.sample$wba[j] = temp$wba
      main.sample$max.weeks.UI[j] = temp$max.weeks.UI
      main.sample$DURUNEMP[j] = temp$DURUNEMP
      main.sample$FPUC.UI.2020[j] = ifelse(temp$initial.mon == 24, temp$FPUC.UI.2020 / 600, min(temp$FPUC.UI.2020 / 600, max((52 - (main.sample$weeks.passed[j] - main.sample$weeks.passed[which(main.sample$YEAR == UI.coverage$YEAR[temp$initial.mon] & main.sample$MONTH == UI.coverage$MONTH[temp$initial.mon])])) - max(ifelse(temp$DURUNEMP > temp$max.weeks.UI, temp$DURUNEMP - temp$max.weeks.UI, 0), main.sample$weeks.passed.Jul20[which(main.sample$YEAR == UI.coverage$YEAR[temp$initial.mon] & main.sample$MONTH == UI.coverage$MONTH[temp$initial.mon])]) + temp$weeks.after, 0))) * 600 + ifelse(temp$initial.mon == 24 & temp$YEAR == 2020 & temp$MONTH %in% c(4:7), main.sample$weeks.after[j] * 600, 0) 
      main.sample$FPUC.UI.2021[j] = temp$FPUC.UI.2021 + ifelse(temp$initial.mon == 24 & temp$YEAR == 2021 & temp$MONTH %in% c(1:7), main.sample$weeks.after[j] * 300, 0)
      main.sample$initial.mon[j] = ifelse(temp$initial.mon == 24, i-1, temp$initial.mon)
      main.sample$initial.CPSIDP[j] = ifelse(temp$initial.mon == 24, temp$CPSIDP, temp$initial.CPSIDP)
    }
  }
  for(z in 1:3) {
    if(sum(main.sample$UI[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$initial.mon != 24 & main.sample$group == z)] * main.sample$WTFINL[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$initial.mon != 24 & main.sample$group == z)], na.rm = TRUE) > sum(State.UI$aggregated.amount[State.UI$YEAR == UI.coverage$YEAR[i] & State.UI$MONTH == UI.coverage$MONTH[i] & State.UI$group == z])) {
      main.sample$FPUC.UI.2020[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$UI > 0 & main.sample$initial.mon == 24 & main.sample$group == z)] = 0
      main.sample$FPUC.UI.2021[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$UI > 0 & main.sample$initial.mon == 24 & main.sample$group == z)] = 0
      main.sample$UI[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$UI > 0 & main.sample$initial.mon == 24 & main.sample$group == z)] = 0
    } else {
      if(sum(main.sample$UI[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$group == z)] * main.sample$WTFINL[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$group == z)], na.rm = TRUE) > sum(State.UI$aggregated.amount[State.UI$YEAR == UI.coverage$YEAR[i] & State.UI$MONTH == UI.coverage$MONTH[i] & State.UI$group == z])) {
        wba.group = sum(State.UI$`Benefits Paid`[State.UI$YEAR == UI.coverage$YEAR[i] & State.UI$MONTH == UI.coverage$MONTH[i] & State.UI$group == z]) / sum(State.UI$`Weeks Compensated`[State.UI$YEAR == UI.coverage$YEAR[i] & State.UI$MONTH == UI.coverage$MONTH[i] & State.UI$group == z])
        while(sum(main.sample$UI[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$group == z)] * main.sample$WTFINL[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$group == z)], na.rm = TRUE) > sum(State.UI$aggregated.amount[State.UI$YEAR == UI.coverage$YEAR[i] & State.UI$MONTH == UI.coverage$MONTH[i] & State.UI$group == z])) {
          if(mean(main.sample$wba[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$group == z & main.sample$UI > 0 & main.sample$initial.mon == 24)]) > wba.group) {
            j = sample(length(main.sample$UI[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$UI > 0 & main.sample$initial.mon == 24 & main.sample$wba > wba.group & main.sample$group == z)]), 1)
            main.sample$FPUC.UI.2020[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$UI > 0 & main.sample$initial.mon == 24 & main.sample$wba > wba.group & main.sample$group == z)][j] = 0
            main.sample$FPUC.UI.2021[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$UI > 0 & main.sample$initial.mon == 24 & main.sample$wba > wba.group & main.sample$group == z)][j] = 0
            main.sample$UI[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$UI > 0 & main.sample$initial.mon == 24 & main.sample$wba > wba.group & main.sample$group == z)][j] = 0
          } else {
            j = sample(length(main.sample$UI[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$UI > 0 & main.sample$initial.mon == 24 & main.sample$wba < wba.group & main.sample$group == z)]), 1)
            main.sample$FPUC.UI.2020[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$UI > 0 & main.sample$initial.mon == 24 & main.sample$wba < wba.group & main.sample$group == z)][j] = 0
            main.sample$FPUC.UI.2021[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$UI > 0 & main.sample$initial.mon == 24 & main.sample$wba < wba.group & main.sample$group == z)][j] = 0
            main.sample$UI[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$UI > 0 & main.sample$initial.mon == 24 & main.sample$wba < wba.group & main.sample$group == z)][j] = 0
          }
        }
      }
    }
  }
  UI.coverage$average.wba[i] = mean(main.sample$wba[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i] & main.sample$UI > 0 & main.sample$initial.mon == 24)])
}
main.sample$FPUC.UI = main.sample$FPUC.UI.2020 + main.sample$FPUC.UI.2021

# compute allocated amounts per month
for(i in 1:nrow(UI.coverage)) {
  UI.coverage$allocated.amount[i] = sum(main.sample$UI[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i])] * main.sample$WTFINL[which(main.sample$YEAR == UI.coverage$YEAR[i] & main.sample$MONTH == UI.coverage$MONTH[i])], na.rm = TRUE)
}


# compute PEUC recipiency ------------------------------------------------------
# cap number of weeks for which PEUC can be received
PEUC.max.weeks = data.frame("YEAR" = c(rep(2020, 9), rep(2021, 12)),
                            "MONTH" = c(4:12, 1:12),
                            "max.weeks.PEUC.2020" = c(2, 7, 11, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13),
                            "max.weeks.PEUC.2021" = c(rep(0, 9), 3, 7, 11, 16, 20, 24, 29, 33, 37, 42, 46, 50))
main.sample = merge(main.sample, PEUC.max.weeks, by = c("YEAR", "MONTH"), all.x = TRUE)
rm(PEUC.max.weeks)

# compute amount of PEUC for each individual received which we observe (apply caps
# and subtract the weeks after which no more PEUC was paid out)
i = which(main.sample$DURUNEMP > main.sample$max.weeks.UI & main.sample$UI.eligible == 1 & !is.na(main.sample$wba))
main.sample$PEUC.2020 = 0
main.sample$PEUC.2021 = 0
main.sample$PEUC.2020[i] = pmax(pmin(pmax(main.sample$DURUNEMP[i]  - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed[i], main.sample$max.weeks.UI[i]) - main.sample$weeks.passed.Dez20[i], 0), 13) - pmax(pmin(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i] - main.sample$max.weeks.UI[i]) - 52, 13), 0), 0) * main.sample$wba[i]
main.sample$PEUC.2021[i] = pmin(pmax(main.sample$DURUNEMP[i]  - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], main.sample$max.weeks.UI[i]) - main.sample$expired.weeks.Jun[i], 0), main.sample$max.weeks.PEUC.2021[i]) * main.sample$wba[i]
main.sample$PEUC = main.sample$PEUC.2020 + main.sample$PEUC.2021

# compute amount FPUC for each individual under PEUC (apply caps and subtract 
# the weeks during which no FPUC was paid out)
main.sample$FPUC.PEUC.2020 = 0
main.sample$FPUC.PEUC.2021 = 0
for (i in which(main.sample$DURUNEMP > main.sample$max.weeks.UI & main.sample$UI.eligible == 1 & !is.na(main.sample$wba))) {
  if(main.sample$STATEFIP[i] %in% c(1, 2, 5, 12, 13, 16, 19, 28, 29, 30, 31, 33, 38, 39, 40, 45, 46, 48, 49, 54, 56)) {
    main.sample$FPUC.PEUC.2020[i] = 600 * pmax(pmin(pmax(main.sample$DURUNEMP[i]  - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed[i], main.sample$max.weeks.UI[i]) - main.sample$weeks.passed.Jul20[i], 0), 13) - pmax(pmin(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i] - main.sample$max.weeks.UI[i]) - 52, 13), 0), 0) 
    main.sample$FPUC.PEUC.2021[i] = 300 * pmin(pmax(main.sample$DURUNEMP[i]  - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], main.sample$max.weeks.UI[i]) - main.sample$expired.weeks.Jun[i], 0), main.sample$max.weeks.PEUC.2021[i])
  } else {
    if(main.sample$STATEFIP[i] == 47) {
      main.sample$FPUC.PEUC.2020[i] = 600 * pmax(pmin(pmax(main.sample$DURUNEMP[i]  - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed[i], main.sample$max.weeks.UI[i]) - main.sample$weeks.passed.Jul20[i], 0), 13) - pmax(pmin(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i] - main.sample$max.weeks.UI[i]) - 52, 13), 0), 0)
      main.sample$FPUC.PEUC.2021[i] = 300 * pmin(pmax(main.sample$DURUNEMP[i]  - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], main.sample$max.weeks.UI[i]) - main.sample$expired.weeks.Tennessee[i], 0), main.sample$max.weeks.PEUC.2021[i])
    } else {
      if(main.sample$STATEFIP[i] == 22) {
        main.sample$FPUC.PEUC.2020[i] = 600 * pmax(pmin(pmax(main.sample$DURUNEMP[i]  - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed[i], main.sample$max.weeks.UI[i]) - main.sample$weeks.passed.Jul20[i], 0), 13) - pmax(pmin(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i] - main.sample$max.weeks.UI[i]) - 52, 13), 0), 0)
        main.sample$FPUC.PEUC.2021[i] = 300 * pmin(pmax(main.sample$DURUNEMP[i]  - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], main.sample$max.weeks.UI[i]) - main.sample$expired.weeks.Lousiana[i], 0), main.sample$max.weeks.PEUC.2021[i])
      } else {
        if(main.sample$STATEFIP[i] == 4) {
          main.sample$FPUC.PEUC.2020[i] = 600 * pmax(pmin(pmax(main.sample$DURUNEMP[i]  - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed[i], main.sample$max.weeks.UI[i]) - main.sample$weeks.passed.Jul20[i], 0), 13) - pmax(pmin(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i] - main.sample$max.weeks.UI[i]) - 52, 13), 0), 0)
          main.sample$FPUC.PEUC.2021[i] = 300 * pmin(pmax(main.sample$DURUNEMP[i]  - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], main.sample$max.weeks.UI[i]) - main.sample$expired.weeks.Arizona[i], 0), main.sample$max.weeks.PEUC.2021[i])
        } else {
          main.sample$FPUC.PEUC.2020[i] = 600 * pmax(pmin(pmax(main.sample$DURUNEMP[i]  - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed[i], main.sample$max.weeks.UI[i]) - main.sample$weeks.passed.Jul20[i], 0), 13) - pmax(pmin(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i] - main.sample$max.weeks.UI[i]) - 52, 13), 0), 0)
          main.sample$FPUC.PEUC.2021[i] = 300 * pmin(pmax(main.sample$DURUNEMP[i]  - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], main.sample$max.weeks.UI[i]) - main.sample$expired.weeks.Sep[i], 0), main.sample$max.weeks.PEUC.2021[i])
        }
      }
    }
  }
}

# simulate amount of PEUC for individuals we do not observe to match with administrative
# values 
PEUC.coverage = data.frame("YEAR" = c(rep(2020, 9), rep(2021, 12)),
                           "MONTH" = c(4:12, 1:12),
                           "aggregated.amount" = c(90002392, 451218441, 1571147380, 3335475083, 5381292623, 8077103494, 12330427892, 18395174374, 25157478796, 31206209596, 37344718309, 45075526987, 52718757477, 59416654692, 65587774647, 70221355501, 73696151032, 74862456990, 71809227699, 66005758472, 59408041475),
                           "allocated.amount" = rep(NA, 21),
                           "average.wba" = rep(NA, 21))

main.sample$initial.mon = 24
main.sample$weeks.after.2020 = 0
main.sample$weeks.after.2021 = 0
for(i in 1:nrow(PEUC.coverage)) {
  for (k in which(main.sample$YEAR == PEUC.coverage$YEAR[i-1] & main.sample$MONTH == PEUC.coverage$MONTH[i-1] & main.sample$PEUC > 0 & main.sample$initial.mon >= (i-11) & (ifelse(main.sample$MISH == 1, main.sample$EMPSTAT2, main.sample$EMPSTAT6) != 21 | main.sample$UI.eligible == 0))) {
    temp = main.sample[k,]
    if (length(which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC == 0 & main.sample$EMPSTAT == 10 & main.sample$PUA.eligible == 0 & main.sample$UI.eligible == 0 & between(main.sample$FTOTVAL, temp$FTOTVAL * 9/10, max(temp$FTOTVAL * 11/10, temp$FTOTVAL + 3000)) & main.sample$group == temp$group)) > 0) {
      j = sample(length(which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC == 0 & main.sample$EMPSTAT == 10 & main.sample$PUA.eligible == 0 & main.sample$UI.eligible == 0 & between(main.sample$FTOTVAL, temp$FTOTVAL * 9/10, max(temp$FTOTVAL * 11/10, temp$FTOTVAL + 3000)) & main.sample$group == temp$group)), 1)
      j = which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC == 0 & main.sample$EMPSTAT == 10 & main.sample$PUA.eligible == 0 & main.sample$UI.eligible == 0 & between(main.sample$FTOTVAL, temp$FTOTVAL * 9/10, max(temp$FTOTVAL * 11/10, temp$FTOTVAL + 3000)) & main.sample$group == temp$group)[j]
      main.sample$weeks.after.2020[j] = ifelse(temp$initial.mon == 24, ifelse((temp$PEUC.2020 / temp$wba) + 3 <= main.sample$max.weeks.PEUC.2020[j], sample(0:3, 1), 
                                                                              ifelse((temp$PEUC.2020 / temp$wba) + 2 <= main.sample$max.weeks.PEUC.2020[j], sample(0:2, 1),
                                                                                     ifelse((temp$PEUC.2020 / temp$wba) + 1 <= main.sample$max.weeks.PEUC.2020[j], sample(0:1, 1), 0))), 0)
      main.sample$weeks.after.2021[j] = ifelse(temp$initial.mon == 24, ifelse((temp$PEUC.2021 / temp$wba) + 3 <= main.sample$max.weeks.PEUC.2021[j] & !(temp$YEAR == 2021 & temp$MONTH %in% c(8:12)), sample(0:3, 1), 
                                                                              ifelse((temp$PEUC.2021 / temp$wba) + 2 <= main.sample$max.weeks.PEUC.2021[j] & !(temp$YEAR == 2021 & temp$MONTH %in% c(8:12)), sample(0:2, 1),
                                                                                     ifelse((temp$PEUC.2021 / temp$wba) + 1 <= main.sample$max.weeks.PEUC.2021[j] & !(temp$YEAR == 2021 & temp$MONTH %in% c(8:12)), sample(0:1, 1), 0))), 0)
      main.sample$PEUC.2020[j] = ifelse(temp$initial.mon == 24, temp$PEUC.2020 / temp$wba,  min(temp$PEUC.2020 / temp$wba, max(52 - (main.sample$weeks.passed[j] - main.sample$weeks.passed[which(main.sample$YEAR == PEUC.coverage$YEAR[temp$initial.mon] & main.sample$MONTH == PEUC.coverage$MONTH[temp$initial.mon])]) - ifelse(main.sample$weeks.passed[which(main.sample$YEAR == PEUC.coverage$YEAR[temp$initial.mon] & main.sample$MONTH == PEUC.coverage$MONTH[temp$initial.mon])] - temp$PEUC.2020 / temp$wba > main.sample$weeks.passed[j] - 52, main.sample$weeks.passed[which(main.sample$YEAR == PEUC.coverage$YEAR[temp$initial.mon] & main.sample$MONTH == PEUC.coverage$MONTH[temp$initial.mon])] - temp$PEUC.2020 / temp$wba - (main.sample$weeks.passed[j] - 52), 0), 0))) * temp$wba + ifelse(temp$initial.mon == 24 & temp$YEAR == 2020 & temp$MONTH %in% c(4:11) , main.sample$weeks.after.2020[j] * temp$wba, 0)
      main.sample$PEUC.2021[j] = temp$PEUC.2021 + ifelse(temp$initial.mon == 24 & temp$YEAR == 2021 & temp$MONTH %in% c(1:8), main.sample$weeks.after.2021[j] * temp$wba, 0)
      main.sample$PEUC[j] = main.sample$PEUC.2020[j] + main.sample$PEUC.2021[j]
      main.sample$wba[j] = temp$wba
      main.sample$max.weeks.UI[j] = temp$max.weeks.UI
      main.sample$DURUNEMP[j] = temp$DURUNEMP
      main.sample$FPUC.PEUC.2020[j] = ifelse(temp$initial.mon == 24, temp$FPUC.PEUC.2020 / 600,  min(temp$FPUC.PEUC.2020 / 600, max(52 - (main.sample$weeks.passed[j] - main.sample$weeks.passed[which(main.sample$YEAR == PEUC.coverage$YEAR[temp$initial.mon] & main.sample$MONTH == PEUC.coverage$MONTH[temp$initial.mon])]) - ifelse(main.sample$weeks.passed[which(main.sample$YEAR == PEUC.coverage$YEAR[temp$initial.mon] & main.sample$MONTH == PEUC.coverage$MONTH[temp$initial.mon])] - temp$FPUC.PEUC.2020 / 600 > main.sample$weeks.passed[j] - 52, main.sample$weeks.passed[which(main.sample$YEAR == PEUC.coverage$YEAR[temp$initial.mon] & main.sample$MONTH == PEUC.coverage$MONTH[temp$initial.mon])] - temp$FPUC.PEUC.2020 / 600 - (main.sample$weeks.passed[j] - 52), 0), 0))) * 600 + ifelse(temp$initial.mon == 24 & temp$YEAR == 2020 & temp$MONTH %in% c(4:7) , main.sample$weeks.after.2020[j] * 600, 0)
      main.sample$FPUC.PEUC.2021[j] = temp$FPUC.PEUC.2021 + ifelse(temp$initial.mon == 24 & temp$YEAR == 2021 & temp$MONTH %in% c(1:7), main.sample$weeks.after.2021[j] * 300, 0)
      main.sample$initial.mon[j] = ifelse(temp$initial.mon == 24, i-1, temp$initial.mon) 
    }
  }
  for (z in 1:3) {
    if(sum(main.sample$PEUC[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH ==  PEUC.coverage$MONTH[i] & main.sample$initial.mon != 24 & main.sample$group == z)] * main.sample$WTFINL[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$initial.mon != 24 & main.sample$group == z)], na.rm = TRUE) > sum(PEUC$aggregated.amount[PEUC$YEAR == UI.coverage$YEAR[i] & PEUC$MONTH == UI.coverage$MONTH[i] & PEUC$group == z])) {
      main.sample$FPUC.PEUC.2020[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC > 0  & main.sample$initial.mon == 24 & main.sample$group == z)] = 0
      main.sample$FPUC.PEUC.2021[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC > 0  & main.sample$initial.mon == 24 & main.sample$group == z)] = 0
      main.sample$PEUC.2020[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC > 0 & main.sample$initial.mon == 24 & main.sample$group == z)] = 0
      main.sample$PEUC.2021[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC > 0 & main.sample$initial.mon == 24 & main.sample$group == z)] = 0
      main.sample$PEUC[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC > 0 & main.sample$initial.mon == 24 & main.sample$group == z)] = 0
    } else {
      if(sum(main.sample$PEUC[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$group == z)] * main.sample$WTFINL[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$group == z)], na.rm = TRUE) > sum(PEUC$aggregated.amount[PEUC$YEAR == UI.coverage$YEAR[i] & PEUC$MONTH == UI.coverage$MONTH[i] & PEUC$group == z])) {
        wba.group = sum(PEUC$c35[PEUC$YEAR == PEUC.coverage$YEAR[i] & PEUC$MONTH == PEUC.coverage$MONTH[i] & PEUC$group == z]) / sum(PEUC$c29[PEUC$YEAR == PEUC.coverage$YEAR[i] & PEUC$MONTH == PEUC.coverage$MONTH[i] & PEUC$group == z])
        while(sum(main.sample$PEUC[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$group == z)] * main.sample$WTFINL[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$group == z)], na.rm = TRUE) > sum(PEUC$aggregated.amount[PEUC$YEAR == UI.coverage$YEAR[i] & PEUC$MONTH == UI.coverage$MONTH[i] & PEUC$group == z])) {
          if(mean(main.sample$wba[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & (main.sample$PEUC > 0) & main.sample$initial.mon == 24 & main.sample$group == z)]) > wba.group) {
            j = sample(length(main.sample$PEUC[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC > 0 & main.sample$initial.mon == 24 & main.sample$wba > wba.group & main.sample$group == z)]), 1)
            main.sample$FPUC.PEUC.2020[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC > 0  & main.sample$initial.mon == 24 & main.sample$wba > wba.group & main.sample$group == z)][j] = 0
            main.sample$FPUC.PEUC.2021[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC > 0  & main.sample$initial.mon == 24 & main.sample$wba > wba.group & main.sample$group == z)][j] = 0
            main.sample$PEUC.2020[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC > 0 & main.sample$initial.mon == 24 & main.sample$wba > wba.group & main.sample$group == z)][j] = 0
            main.sample$PEUC.2021[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC > 0 & main.sample$initial.mon == 24 & main.sample$wba > wba.group & main.sample$group == z)][j] = 0
            main.sample$PEUC[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC > 0 & main.sample$initial.mon == 24 & main.sample$wba > wba.group & main.sample$group == z)][j] = 0
          } else {
            j = sample(length(main.sample$PEUC[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC > 0 & main.sample$initial.mon == 24 & main.sample$wba < wba.group & main.sample$group == z)]), 1)
            main.sample$FPUC.PEUC.2020[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC > 0  & main.sample$initial.mon == 24 & main.sample$wba < wba.group & main.sample$group == z)][j] = 0
            main.sample$FPUC.PEUC.2021[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC > 0  & main.sample$initial.mon == 24 & main.sample$wba < wba.group & main.sample$group == z)][j] = 0
            main.sample$PEUC.2020[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC > 0 & main.sample$initial.mon == 24 & main.sample$wba < wba.group & main.sample$group == z)][j] = 0
            main.sample$PEUC.2021[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC > 0 & main.sample$initial.mon == 24 & main.sample$wba < wba.group & main.sample$group == z)][j] = 0
            main.sample$PEUC[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & main.sample$PEUC > 0 & main.sample$initial.mon == 24 & main.sample$wba < wba.group & main.sample$group == z)][j] = 0
          }
        }
      }
    }
  }
  PEUC.coverage$average.wba[i] = mean(main.sample$wba[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i] & (main.sample$PEUC > 0) & main.sample$initial.mon == 24)])
}
main.sample$FPUC.PEUC = main.sample$FPUC.PEUC.2020 + main.sample$FPUC.PEUC.2021


# compute allocated amounts per month
for(i in 1:nrow(PEUC.coverage)) {
  PEUC.coverage$allocated.amount[i] = sum(main.sample$PEUC[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i])] * main.sample$WTFINL[which(main.sample$YEAR == PEUC.coverage$YEAR[i] & main.sample$MONTH == PEUC.coverage$MONTH[i])], na.rm = TRUE)
}


# compute PUA recipiency -------------------------------------------------------
# assign duration of unemployment to individuals for which we do not observe unemployment
for (i in which((main.sample$DURUNEMP == 999 | is.na(main.sample$DURUNEMP)) & main.sample$PUA.eligible == 1 & !is.na(main.sample$wba))) {
  if(main.sample$MISH[i] == 1) {
    main.sample$DURUNEMP[i] = main.sample$DURUNEMP[which(main.sample$DURUNEMP != 999 & main.sample$YEAR == main.sample$YEAR[i] & main.sample$MONTH == main.sample$MONTH[i] & main.sample$DURUNEMP <= (main.sample$weeks.passed + 3) & ifelse(main.sample$EMPSTAT2 == 10, main.sample$EMPSTAT2 == main.sample$EMPSTAT2[i], main.sample$EMPSTAT2[i] %in% 0:40))][sample(1:length(which(main.sample$DURUNEMP != 999 & main.sample$YEAR == main.sample$YEAR[i] & main.sample$MONTH == main.sample$MONTH[i] & main.sample$DURUNEMP <= (main.sample$weeks.passed + 3) & ifelse(main.sample$EMPSTAT2 == 10, main.sample$EMPSTAT2 == main.sample$EMPSTAT2[i], main.sample$EMPSTAT2[i] %in% 0:40))), 1)]
  } else {
    main.sample$DURUNEMP[i] = main.sample$DURUNEMP[which(main.sample$DURUNEMP != 999 & main.sample$YEAR == main.sample$YEAR[i] & main.sample$MONTH == main.sample$MONTH[i] & main.sample$DURUNEMP <= (main.sample$weeks.passed + 3) & ifelse(main.sample$EMPSTAT6 == 10, main.sample$EMPSTAT6 == main.sample$EMPSTAT6[i], main.sample$EMPSTAT6[i] %in% 0:40))][sample(1:length(which(main.sample$DURUNEMP != 999 & main.sample$YEAR == main.sample$YEAR[i] & main.sample$MONTH == main.sample$MONTH[i] & main.sample$DURUNEMP <= (main.sample$weeks.passed + 3) & ifelse(main.sample$EMPSTAT6 == 10, main.sample$EMPSTAT6 == main.sample$EMPSTAT6[i], main.sample$EMPSTAT6[i] %in% 0:40))), 1)]
  }
}

# cap number of weeks for which PUA can be received
PUA.max.weeks = data.frame("YEAR" = c(rep(2020, 9), rep(2021, 12)),
                           "MONTH" = c(4:12, 1:12),
                           "max.weeks.PUA.2020" = c(11, 16, 20, 24, 29, 33, 38, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39),
                           "max.weeks.PUA.2021" = c(rep(0, 9), 3, 7, 11, 16, 20, 24, 29, 33, 37, 42, 46, 50))
main.sample = merge(main.sample, PUA.max.weeks, by = c("YEAR", "MONTH"), all.x = TRUE)
rm(PUA.max.weeks)

# compute amount of PUA each individual received which we observe (apply caps and
# subtract the weeks after which no more PUA was paid out)
main.sample$PUA.2020 = 0
main.sample$PUA.2021 = 0
for (i in which(main.sample$PUA.eligible == 1 & !is.na(main.sample$wba))) {
  if(main.sample$STATEFIP[i] %in% c(1, 2, 5, 12, 13, 16, 19, 28, 29, 30, 31, 33, 38, 39, 40, 45, 46, 48, 49, 54, 56)) {
    main.sample$PUA.2020[i] = main.sample$PUA.wba[i] * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - pmax(main.sample$weeks.passed[i], main.sample$max.weeks.PUA.2020[i]), 0) - main.sample$weeks.passed.Dez20[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - pmax(main.sample$weeks.passed[i], main.sample$max.weeks.PUA.2020[i]), 0) - main.sample$weeks.passed.Dez20[i], 39)) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0), 0)
    main.sample$PUA.2021[i] = main.sample$PUA.wba[i] * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Jun[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Jun[i], main.sample$max.weeks.PUA.2021[i])), 0)
  } else {
    if(main.sample$STATEFIP[i] == 47) {
      main.sample$PUA.2020[i] = main.sample$PUA.wba[i] * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - pmax(main.sample$weeks.passed[i], main.sample$max.weeks.PUA.2020[i]), 0) - main.sample$weeks.passed.Dez20[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - pmax(main.sample$weeks.passed[i], main.sample$max.weeks.PUA.2020[i]), 0) - main.sample$weeks.passed.Dez20[i], 39)) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0), 0)
      main.sample$PUA.2021[i] = main.sample$PUA.wba[i] * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Tennessee[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Tennessee[i], main.sample$max.weeks.PUA.2021[i])), 0)
    } else {
      if(main.sample$STATEFIP[i] == 22) {
        main.sample$PUA.2020[i] = main.sample$PUA.wba[i] * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - pmax(main.sample$weeks.passed[i], main.sample$max.weeks.PUA.2020[i]), 0) - main.sample$weeks.passed.Dez20[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - pmax(main.sample$weeks.passed[i], main.sample$max.weeks.PUA.2020[i]), 0) - main.sample$weeks.passed.Dez20[i], 39)) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0), 0)
        main.sample$PUA.2021[i] = main.sample$PUA.wba[i] * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Lousiana[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Lousiana[i], main.sample$max.weeks.PUA.2021[i])), 0)
      } else {
        main.sample$PUA.2020[i] = main.sample$PUA.wba[i] * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - pmax(main.sample$weeks.passed[i], main.sample$max.weeks.PUA.2020[i]), 0) - main.sample$weeks.passed.Dez20[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - pmax(main.sample$weeks.passed[i], main.sample$max.weeks.PUA.2020[i]), 0) - main.sample$weeks.passed.Dez20[i], 39)) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0), 0)
        main.sample$PUA.2021[i] = main.sample$PUA.wba[i] * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Sep[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Sep[i], main.sample$max.weeks.PUA.2021[i])), 0)
      }
    }
  }
}
main.sample$PUA = main.sample$PUA.2020 + main.sample$PUA.2021

# compute amount FPUC for each individual under PUA (apply caps and subtract 
# the weeks during which no FPUC was paid out)
main.sample$FPUC.PUA.2020 = 0
main.sample$FPUC.PUA.2021 = 0
for (i in which(main.sample$PUA.eligible == 1 & !is.na(main.sample$wba))) {
  if(main.sample$STATEFIP[i] %in% c(1, 2, 5, 12, 13, 16, 19, 28, 29, 30, 31, 33, 38, 39, 40, 45, 46, 48, 49, 54, 56)) {
    main.sample$FPUC.PUA.2020[i] = 600 * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed[i], 0) - main.sample$weeks.passed.Jul20[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed[i], 0) - main.sample$weeks.passed.Jul20[i], 39)) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0), 0)
    main.sample$FPUC.PUA.2021[i] = 300 * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Jun[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Jun[i], main.sample$max.weeks.PUA.2021[i])), 0)
  } else {
    if(main.sample$STATEFIP[i] == 47) {
      main.sample$FPUC.PUA.2020[i] = 600 * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed[i], 0) - main.sample$weeks.passed.Jul20[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed[i], 0) - main.sample$weeks.passed.Jul20[i], 39)) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0), 0)
      main.sample$FPUC.PUA.2021[i] = 300 * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Tennessee[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Tennessee[i], main.sample$max.weeks.PUA.2021[i])), 0)
    } else {
      if(main.sample$STATEFIP[i] == 22) {
        main.sample$FPUC.PUA.2020[i] = 600 * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed[i], 0) - main.sample$weeks.passed.Jul20[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed[i], 0) - main.sample$weeks.passed.Jul20[i], 39)) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0), 0)
        main.sample$FPUC.PUA.2021[i] = 300 * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Lousiana[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Lousiana[i], main.sample$max.weeks.PUA.2021[i])), 0)
      } else {
        if(main.sample$STATEFIP[i] == 4) {
          main.sample$FPUC.PUA.2020[i] = 600 * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed[i], 0) - main.sample$weeks.passed.Jul20[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed[i], 0) - main.sample$weeks.passed.Jul20[i], 39)) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0), 0)
          main.sample$FPUC.PUA.2021[i] = 300 * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Arizona[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Arizona[i], main.sample$max.weeks.PUA.2021[i])), 0)
        } else {
          main.sample$FPUC.PUA.2020[i] = 600 * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed[i], 0) - main.sample$weeks.passed.Jul20[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed[i], 0) - main.sample$weeks.passed.Jul20[i], 39)) - pmax(pmin(main.sample$weeks.passed[i], main.sample$DURUNEMP[i]) - 52, 0), 0)
          main.sample$FPUC.PUA.2021[i] = 300 * pmax(pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Sep[i], pmin(main.sample$DURUNEMP[i] - pmax(main.sample$DURUNEMP[i] - main.sample$weeks.passed.Dez20[i], 0) - main.sample$expired.weeks.Sep[i], main.sample$max.weeks.PUA.2021[i])), 0)
        }
      }
    }
  }
}

# simulate amount of PUA for individuals we do not observe to match with administrative
# values
PUA.coverage = data.frame("YEAR" = c(rep(2020, 9), rep(2021, 12)),
                          "MONTH" = c(4:12, 1:12),
                          "aggregated.amount" = c(568345287, 5000963241, 14872815988, 27226556749, 40389116494, 51773326233, 60129266618, 68000986079, 76005560633, 82924242816, 89530264567, 96709898941, 102539678411, 103794965600, 99603923927, 92346321249, 84179202118, 76595802159, 69580159001, 62111445342, 54352533632),
                          "allocated.amount" = rep(NA, 21),
                          "average.wba" = rep(NA, 21))

main.sample$initial.mon = 24
main.sample$weeks.after.2020 = 0
main.sample$weeks.after.2021 = 0
for(i in 1:nrow(PUA.coverage)) {
  for (k in which(main.sample$YEAR == PUA.coverage$YEAR[i-1] & main.sample$MONTH == PUA.coverage$MONTH[i-1] & !is.na(main.sample$PUA) & main.sample$PUA > 0 & main.sample$initial.mon >= (i-11) & (ifelse(main.sample$MISH == 1, main.sample$PUA.eligible2, main.sample$PUA.eligible6) == 0 | main.sample$PUA.eligible == 0))) {
    temp = main.sample[k,]
    if (length(which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$PUA == 0 & main.sample$EMPSTAT == 10 & main.sample$UI == 0 & between(main.sample$FTOTVAL, temp$FTOTVAL * 9/10, max(temp$FTOTVAL * 11/10, temp$FTOTVAL + 5000)) & main.sample$group == temp$group)) > 0) {
      j = sample(length(which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$PUA == 0 & main.sample$EMPSTAT == 10 & main.sample$UI == 0 & between(main.sample$FTOTVAL, temp$FTOTVAL * 9/10, max(temp$FTOTVAL * 11/10, temp$FTOTVAL + 5000)) & main.sample$group == temp$group)), 1)
      j = which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$PUA == 0 & main.sample$EMPSTAT == 10 & main.sample$UI == 0 & between(main.sample$FTOTVAL, temp$FTOTVAL * 9/10, max(temp$FTOTVAL * 11/10, temp$FTOTVAL + 5000)) & main.sample$group == temp$group)[j]
      main.sample$weeks.after.2020[j] = ifelse(temp$initial.mon == 24, ifelse((temp$PUA.2020 / temp$PUA.wba) + 3 <= main.sample$max.weeks.PUA.2020[j], sample(0:3, 1), 
                                                                              ifelse((temp$PUA.2020 / temp$PUA.wba) + 3 <= main.sample$max.weeks.PUA.2020[j], sample(0:2, 1),
                                                                                     ifelse((temp$PUA.2020 / temp$PUA.wba) + 3 <= main.sample$max.weeks.PUA.2020[j], sample(0:1, 1), 0))), 0)
      main.sample$weeks.after.2021[j] = ifelse(temp$initial.mon == 24, ifelse((temp$PUA.2021 / temp$PUA.wba) + 3 <= main.sample$max.weeks.PUA.2021[j] & !(temp$YEAR == 2021 & temp$MONTH %in% c(8:12)), sample(0:3, 1), 
                                                                              ifelse((temp$PUA.2021 / temp$PUA.wba) + 3 <= main.sample$max.weeks.PUA.2021[j] & !(temp$YEAR == 2021 & temp$MONTH %in% c(8:12)), sample(0:2, 1),
                                                                                     ifelse((temp$PUA.2021 / temp$PUA.wba) + 3 <= main.sample$max.weeks.PUA.2021[j] & !(temp$YEAR == 2021 & temp$MONTH %in% c(8:12)), sample(0:1, 1), 0))), 0)
      main.sample$weeks.after[j] = main.sample$weeks.after.2020[j] + main.sample$weeks.after.2021[j]
      main.sample$PUA.2020[j] = ifelse(temp$initial.mon == 24, temp$PUA.2020 / temp$PUA.wba,  min(temp$PUA.2020 / temp$PUA.wba, max(52 - (main.sample$weeks.passed[j] - main.sample$weeks.passed[which(main.sample$YEAR == PUA.coverage$YEAR[temp$initial.mon] & main.sample$MONTH == PUA.coverage$MONTH[temp$initial.mon])]) - ifelse(main.sample$weeks.passed[which(main.sample$YEAR == PUA.coverage$YEAR[temp$initial.mon] & main.sample$MONTH == PUA.coverage$MONTH[temp$initial.mon])] - temp$PUA.2020 / temp$PUA.wba > main.sample$weeks.passed[j] - 52, main.sample$weeks.passed[which(main.sample$YEAR == PUA.coverage$YEAR[temp$initial.mon] & main.sample$MONTH == PUA.coverage$MONTH[temp$initial.mon])] - temp$PUA.2020 / temp$PUA.wba - (main.sample$weeks.passed[j] - 52), 0), 0))) * temp$PUA.wba + ifelse(temp$initial.mon == 24 & temp$YEAR == 2020 & temp$MONTH %in% c(4:11) , main.sample$weeks.after.2020[j] * temp$PUA.wba, 0)
      main.sample$PUA.2021[j] = temp$PUA.2021 + ifelse(temp$initial.mon == 24 & temp$YEAR == 2021 & temp$MONTH %in% c(1:7), main.sample$weeks.after.2021[j] * temp$PUA.wba, 0)
      main.sample$PUA[j] = main.sample$PUA.2020[j] + main.sample$PUA.2021[j]
      main.sample$PUA.wba[j] = temp$PUA.wba
      main.sample$max.weeks.PUA[j] = temp$max.weeks.PUA
      main.sample$DURUNEMP[j] = temp$DURUNEMP
      main.sample$FPUC.PUA.2020[j] = ifelse(temp$initial.mon == 24, temp$FPUC.PUA.2020 / 600,  min(temp$FPUC.PUA.2020 / 600, max(52 - (main.sample$weeks.passed[j] - main.sample$weeks.passed[which(main.sample$YEAR == PUA.coverage$YEAR[temp$initial.mon] & main.sample$MONTH == PUA.coverage$MONTH[temp$initial.mon])]) - ifelse(main.sample$weeks.passed[which(main.sample$YEAR == PUA.coverage$YEAR[temp$initial.mon] & main.sample$MONTH == PUA.coverage$MONTH[temp$initial.mon])] - temp$FPUC.PUA.2020 / 600 > main.sample$weeks.passed[j] - 52, main.sample$weeks.passed[which(main.sample$YEAR == PUA.coverage$YEAR[temp$initial.mon] & main.sample$MONTH == PUA.coverage$MONTH[temp$initial.mon])] - temp$FPUC.PUA.2020 / 600 - (main.sample$weeks.passed[j] - 52), 0), 0))) * 600 + ifelse(temp$initial.mon == 24 & temp$YEAR == 2020 & temp$MONTH %in% c(4:7) , main.sample$weeks.after.2020[j] * 600, 0)
      main.sample$FPUC.PUA.2021[j] = temp$FPUC.PUA.2021 + ifelse(temp$initial.mon == 24 & temp$YEAR == 2021 & temp$MONTH %in% c(1:7), main.sample$weeks.after.2021[j] * 300, 0)
      main.sample$initial.mon[j] = ifelse(temp$initial.mon == 24, i-1, temp$initial.mon)
    }
  }
  for (z in 1:3) {
    if(sum(main.sample$PUA[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH ==  PUA.coverage$MONTH[i] & main.sample$initial.mon != 24 & main.sample$group == z)] * main.sample$WTFINL[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$initial.mon != 24 & main.sample$group == z)], na.rm = TRUE) > sum(PUA$aggregated.amount[PUA$YEAR == UI.coverage$YEAR[i] & PUA$MONTH == UI.coverage$MONTH[i] & PUA$group == z])) {
      main.sample$FPUC.PUA.2020[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & !is.na(main.sample$PUA) & main.sample$PUA > 0 & main.sample$initial.mon == 24 & main.sample$group == z)] = 0
      main.sample$FPUC.PUA.2021[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & !is.na(main.sample$PUA) & main.sample$PUA > 0 & main.sample$initial.mon == 24 & main.sample$group == z)] = 0
      main.sample$PUA.2020[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & !is.na(main.sample$PUA) & main.sample$PUA > 0 & main.sample$initial.mon == 24 & main.sample$group == z)] = 0
      main.sample$PUA.2021[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & !is.na(main.sample$PUA) & main.sample$PUA > 0 & main.sample$initial.mon == 24 & main.sample$group == z)] = 0
      main.sample$PUA[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & !is.na(main.sample$PUA) & main.sample$PUA > 0 & main.sample$initial.mon == 24 & main.sample$group == z)] = 0
    } else {
      if(sum(main.sample$PUA[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$group == z)] * main.sample$WTFINL[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$group == z)], na.rm = TRUE) > sum(PUA$aggregated.amount[PUA$YEAR == UI.coverage$YEAR[i] & PUA$MONTH == UI.coverage$MONTH[i] & PUA$group == z])) {
        wba.group = sum(PUA$c6[PUA$YEAR == UI.coverage$YEAR[i] & PUA$MONTH == UI.coverage$MONTH[i] & PUA$group == z]) / sum(PUA$c5[PUA$YEAR == UI.coverage$YEAR[i] & PUA$MONTH == UI.coverage$MONTH[i] & PUA$group == z])
        while(sum(main.sample$PUA[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$group == z)] * main.sample$WTFINL[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$group == z)], na.rm = TRUE) > sum(PUA$aggregated.amount[PUA$YEAR == UI.coverage$YEAR[i] & PUA$MONTH == UI.coverage$MONTH[i] & PUA$group == z])) {
          if(mean(main.sample$PUA.wba[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$group == z & main.sample$PUA > 0 & main.sample$initial.mon == 24)]) > wba.group) {
            j = sample(length(main.sample$PUA[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$PUA > 0 & main.sample$initial.mon == 24 & main.sample$PUA.wba > wba.group & main.sample$group == z)]), 1)
            main.sample$FPUC.PUA.2020[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$PUA > 0 & main.sample$initial.mon == 24 & main.sample$PUA.wba > wba.group & main.sample$group == z)][j] = 0
            main.sample$FPUC.PUA.2021[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$PUA > 0 & main.sample$initial.mon == 24 & main.sample$PUA.wba > wba.group & main.sample$group == z)][j] = 0
            main.sample$PUA.2020[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$PUA > 0 & main.sample$initial.mon == 24 & main.sample$PUA.wba > wba.group & main.sample$group == z)][j] = 0
            main.sample$PUA.2021[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$PUA > 0 & main.sample$initial.mon == 24 & main.sample$PUA.wba > wba.group & main.sample$group == z)][j] = 0
            main.sample$PUA[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$PUA > 0 & main.sample$initial.mon == 24 & main.sample$PUA.wba > wba.group & main.sample$group == z)][j] = 0
          } else {
            j = sample(length(main.sample$PUA[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$PUA > 0 & main.sample$initial.mon == 24 & main.sample$PUA.wba < wba.group & main.sample$group == z)]), 1)
            main.sample$FPUC.PUA.2020[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$PUA > 0 & main.sample$initial.mon == 24 & main.sample$PUA.wba < wba.group & main.sample$group == z)][j] = 0
            main.sample$FPUC.PUA.2021[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$PUA > 0 & main.sample$initial.mon == 24 & main.sample$PUA.wba < wba.group & main.sample$group == z)][j] = 0
            main.sample$PUA.2020[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$PUA > 0 & main.sample$initial.mon == 24 & main.sample$PUA.wba < wba.group & main.sample$group == z)][j] = 0
            main.sample$PUA.2021[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$PUA > 0 & main.sample$initial.mon == 24 & main.sample$PUA.wba < wba.group & main.sample$group == z)][j] = 0
            main.sample$PUA[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$PUA > 0 & main.sample$initial.mon == 24 & main.sample$PUA.wba < wba.group & main.sample$group == z)][j] = 0
          }
        }
      }
    }
  }
  PUA.coverage$average.wba[i] = mean(main.sample$PUA.wba[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$PUA > 0 & main.sample$initial.mon == 24)]) 
}
main.sample$FPUC.PUA = main.sample$FPUC.PUA.2020 + main.sample$FPUC.PUA.2021

for(i in 1:nrow(PUA.coverage)) {
  PUA.coverage$data.no.PUA[i] = sum(main.sample$WTFINL[which(main.sample$PUA > 0 & main.sample$PUA.eligible == 1 & main.sample$DURUNEMP <= main.sample$max.weeks.PUA & main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i])])
  PUA.coverage$allocated.amount[i] = sum(main.sample$PUA[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i])] * main.sample$WTFINL[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i])], na.rm = TRUE)
  PUA.coverage$carried.over[i] = sum(main.sample$PUA[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$initial.mon != 24)] * main.sample$WTFINL[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i] & main.sample$initial.mon != 24)], na.rm = TRUE)
  PUA.coverage$allocated.amount.FPUC[i] = sum(main.sample$FPUC.PUA[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i])] * main.sample$WTFINL[which(main.sample$YEAR == PUA.coverage$YEAR[i] & main.sample$MONTH == PUA.coverage$MONTH[i])], na.rm = TRUE)
}


# compute FPUC recipiency ------------------------------------------------------
main.sample$FPUC.UI = main.sample$FPUC.UI.2020 + main.sample$FPUC.UI.2021
main.sample$FPUC.PEUC = main.sample$FPUC.PEUC.2020 + main.sample$FPUC.PEUC.2021
main.sample$FPUC.PUA = main.sample$FPUC.PUA.2020 + main.sample$FPUC.PUA.2021
main.sample$FPUC = main.sample$FPUC.UI  + main.sample$FPUC.PEUC + main.sample$FPUC.PUA

FPUC.coverage = data.frame("YEAR" = c(rep(2020, 9), rep(2021, 12)),
                           "MONTH" = c(4:12, 1:12),
                           "aggreagated.amount" = c(10011207712, 52284944166, 123296399039, 198395332828, 246327192862, 261100936976, 265916394549, 268877746361, 271257773468, 280766374025, 300371314472, 323681872020, 335397093475, 312540009774, 260758164407, 202383211504, 168753881637, 163879247252, 162606641068, 160610507906, 158857952341))

for (i in 1:nrow(FPUC.coverage)) {
  FPUC.coverage$amount[i] = sum(main.sample$FPUC[which(main.sample$YEAR == FPUC.coverage$YEAR[i] & main.sample$MONTH == FPUC.coverage$MONTH[i])] * main.sample$WTFINL[which(main.sample$YEAR == FPUC.coverage$YEAR[i] & main.sample$MONTH == FPUC.coverage$MONTH[i])], na.rm = TRUE)
}

#save(list = c("FPUC.coverage", "PEUC.coverage", "PUA.coverage", "UI.coverage"), file = paste0("../results/UI/UI stats10.RData"))
#save(main.sample, file = paste0("../results/analysis sample10.RData"))

