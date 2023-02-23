# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Bachelor thesis
# 1.2 create sample
# by Aurel Rochell
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Description ------------------------------------------------------------------
# This script creates the analysis sample. It excludes individuals for which no
# income information is available, transforms the income bins from the CPS into
# a continuous distribution and adjusts the survey weights.


# Set up -----------------------------------------------------------------------
rm(list = ls())   # cleaning the environment
graphics.off()    # closing all plots

library(tidyverse)
library(data.table)
library(cpsR)

setwd("../data/1 create sample")  # needs to be adjusted accordingly


# Import data and add variables of interest ------------------------------------
# load cps covering 2020 and 2021
load("main sample raw.RData")

# load original cps data and merge on family ids (this is needed for the subfamily id of the Census, which is not available for the monthly CPS from IPUMS)
raw.CPS = data.frame()
for(i in c(2020, 2021)) {
  for(j in 1:12) {
    raw.CPS = rbind(raw.CPS, get_basic(year = i, month = j, vars = c("HRYEAR4", "HRMONTH", "HRHHID", "HRHHID2", "PULINENO", "PRFAMNUM")))
  }
}
raw.CPS$hrhhid = as.character(raw.CPS$hrhhid)
raw.CPS$hrhhid2 = as.character(raw.CPS$hrhhid2)
raw.data = inner_join(raw.data, raw.CPS, by = c("YEAR" = "hryear4", "MONTH" = "hrmonth", "HRHHID" = "hrhhid", "HRHHID2" = "hrhhid2", "LINENO" = "pulineno"))

main.sample = raw.data
rm(raw.data, raw.CPS)


# exclusion decisions ----------------------------------------------------------

# create overview of number of observations in each month and how these change as
# sample is restricted
sample.size = data.frame("year" = c(rep(2020, 12),rep(2021, 12)), "month" = c(1:12, 1:12), "number.HH" = rep(0, 24), "number.IND" = rep(0, 24), "response.rate" = rep(0, 24))
for (i in 1:nrow(sample.size)) {
  sample.size$HH.raw[i] = nrow(unique(main.sample[which(main.sample$YEAR == sample.size$year[i] & main.sample$MONTH == sample.size$month[i]),which(colnames(main.sample) %in% c("HRHHID", "HRHHID2"))]))
  sample.size$IND.raw[i] = length(which(main.sample$YEAR == sample.size$year[i] & main.sample$MONTH == sample.size$month[i]))
  sample.size$HH.1.5[i] = nrow(unique(main.sample[which(main.sample$YEAR == sample.size$year[i] & main.sample$MONTH == sample.size$month[i] & main.sample$MISH %in% c(1, 5)),which(colnames(main.sample) %in% c("HRHHID", "HRHHID2"))]))
  sample.size$IND.1.5[i] = length(which(main.sample$YEAR == sample.size$year[i] & main.sample$MONTH == sample.size$month[i] & main.sample$MISH %in% c(1, 5)))
  sample.size$IND.final[i] = length(which(main.sample$YEAR == sample.size$year[i] & main.sample$MONTH == sample.size$month[i] & main.sample$MISH %in% c(1, 5) & main.sample$FTYPE %in% c(1, 2, 3)))
  sample.size$IND.imputed.CPS[i] = length(which(main.sample$YEAR == sample.size$year[i] & main.sample$MONTH == sample.size$month[i] & main.sample$MISH %in% c(1, 5) & main.sample$FTYPE %in% c(1, 2, 3) & main.sample$QFAMINC != 0))
}
rm(sample.size)

# number of individuals unrelated to head of household is about 5.3 percent
sum(main.sample$WTFINL[!(main.sample$FTYPE %in% c(1, 2, 3))]) / sum(main.sample$WTFINL)

# number of individuals in my analysis sample living in households not giving a 
# response to the family income question when being asked is about 26.7 percent
sum(main.sample$WTFINL[main.sample$MISH %in% c(1, 5) & main.sample$FTYPE %in% c(1, 2, 3) & main.sample$QFAMINC != 0]) / sum(main.sample$WTFINL[main.sample$MISH %in% c(1, 5) & main.sample$FTYPE %in% c(1, 2, 3)])

# focus on individuals in the 1st and 5th month in sample
main.sample = main.sample[main.sample$MISH %in% c(1, 5),]

# exclude unrelated individuals
main.sample = main.sample[main.sample$FTYPE %in% c(1, 2, 3),]


# create income variable -------------------------------------------------------

# overview of income bins and individuals in such bins
mutate(aggregate(WTFINL ~ FAMINC, main.sample, FUN = sum), share = WTFINL / sum(main.sample$WTFINL))

# prepare matching with ASEC  (same as in "1.1 sample creation historic validation.R")
main.sample$refyear = ifelse(main.sample$MONTH %in% 1:6, main.sample$YEAR - 1, main.sample$YEAR)
main.sample$FAMINC = as.character(main.sample$FAMINC)
load("cps asec 2009-2022.RData")
ASEC$FTOTVAL = as.integer(ASEC$FTOTVAL)
ASEC$refyear = as.integer(ASEC$YEAR) - 1
ASEC = ASEC[ASEC$refyear %in% unique(main.sample$refyear),]
ASEC = ASEC[ASEC$FTYPE %in% c("1", "2", "3"),]

# aggregate subfamily income to family income
ASEC = inner_join(ASEC, rename(aggregate(FTOTVAL ~ YEAR + refyear + HRHHID + HRHHID2, unique(ASEC[,which(names(ASEC) %in% c("YEAR", "refyear", "HRHHID", "HRHHID2", "FAMID", "FTOTVAL"))]), FUN = sum), "family.inc" = FTOTVAL), by = c("YEAR", "refyear", "HRHHID", "HRHHID2"))

# create demographic matching variables in the CPS ASEC
ASEC = inner_join(ASEC, rename(aggregate(AGE ~ YEAR + refyear + HRHHID + HRHHID2, ASEC, FUN = function(x) length(x)), "famsize" = AGE), by = c("YEAR", "refyear", "HRHHID", "HRHHID2"))
ASEC = left_join(ASEC, rename(aggregate(AGE ~ YEAR + refyear + HRHHID + HRHHID2, ASEC[ASEC$RELATE != "101",], FUN = function(x) length(which(x < 18))), "nochildren" = AGE), by = c("YEAR", "refyear", "HRHHID", "HRHHID2"))
ASEC = inner_join(ASEC, rename(aggregate(as.integer(AGE) ~ YEAR + refyear + HRHHID + HRHHID2, ASEC[ASEC$RELATE %in% c("101"),], FUN = function(x) length(which(x >= 65))), "is.65" = `as.integer(AGE)`), by = c("YEAR", "refyear", "HRHHID", "HRHHID2"))
ASEC$famsize = ifelse(ASEC$famsize >= 5, 5, ASEC$famsize)
ASEC$nochildren = ifelse(is.na(ASEC$nochildren), 0, ifelse(ASEC$nochildren >= 2, 2, ASEC$nochildren))
ASEC$is.65 = ifelse(ASEC$famsize >= 3, 2, ifelse(ASEC$famsize == 2 & ASEC$nochildren == 1, 2, ASEC$is.65))  # in the CPS ASEC, there are no families of size 2 with a single children where the head of household is above 65

# create income bins in CPS ASEC
ASEC = data.frame(unique(ASEC[,which(names(ASEC) %in% c("refyear", "family.inc", "famsize", "nochildren", "is.65"))]), "FAMINC" = NA)
ASEC$FAMINC = ifelse(ASEC$family.inc < 5000, "100", ifelse(ASEC$family.inc < 7500, "210", ifelse(ASEC$family.inc < 10000, "300", ifelse(ASEC$family.inc < 12500, "430", ifelse(ASEC$family.inc < 15000, "470", ifelse(ASEC$family.inc < 20000, "500", ifelse(ASEC$family.inc < 25000, "600", ifelse(ASEC$family.inc < 30000, "710", ifelse(ASEC$family.inc < 35000, "720", ifelse(ASEC$family.inc < 40000, "730", ifelse(ASEC$family.inc < 50000, "740", ifelse(ASEC$family.inc < 60000, "820", ifelse(ASEC$family.inc < 75000, "830", ifelse(ASEC$family.inc < 100000, "841", ifelse(ASEC$family.inc < 150000, "842", ifelse(ASEC$family.inc >= 150000, "843", NA))))))))))))))))

# create demographic matching variables in the analysis sample
main.sample = inner_join(main.sample, rename(aggregate(AGE ~ YEAR + refyear + HRHHID + HRHHID2, main.sample, FUN = function(x) length(x)), "famsize" = AGE), by = c("YEAR", "refyear", "HRHHID", "HRHHID2"))
main.sample = left_join(main.sample, rename(aggregate(AGE ~ YEAR + refyear + HRHHID + HRHHID2, main.sample[main.sample$RELATE != "101",], FUN = function(x) length(which(x < 18))), "nochildren" = AGE), by = c("YEAR", "refyear", "HRHHID", "HRHHID2"))
main.sample = inner_join(main.sample, rename(aggregate(as.integer(AGE) ~ YEAR + refyear + HRHHID + HRHHID2, main.sample[main.sample$RELATE %in% c("101"),], FUN = function(x) length(which(x >= 65))), "is.65" = `as.integer(AGE)`), by = c("YEAR", "refyear", "HRHHID", "HRHHID2"))
main.sample$famsize = ifelse(main.sample$famsize >= 5, 5, main.sample$famsize)
main.sample$nochildren = ifelse(is.na(main.sample$nochildren), 0, ifelse(main.sample$nochildren >= 2, 2, main.sample$nochildren))
main.sample$is.65 = ifelse(main.sample$famsize >= 3, 2, ifelse(main.sample$famsize == 2 & main.sample$nochildren == 1, 2, main.sample$is.65))

# match family income from analysis sample to main.sample ASEC
family.sample = unique(main.sample[,which(names(main.sample) %in% c("YEAR", "MONTH", "refyear", "HRHHID", "HRHHID2", "FAMINC", "famsize", "nochildren", "is.65"))])
family.sample = data.frame(family.sample, as.data.frame(setDT(ASEC)[setDT(family.sample), on = .(refyear, famsize, nochildren, is.65, FAMINC),
                                                                    {ri <- sample(.N, size = 1L)
                                                                    .(family.inc = family.inc[ri])}, by = .EACHI]))
main.sample = inner_join(main.sample, family.sample[,which(names(family.sample) %in% c("YEAR", "MONTH", "refyear", "HRHHID", "HRHHID2", "FAMINC", "family.inc"))], by = c("YEAR", "MONTH", "refyear", "HRHHID", "HRHHID2", "FAMINC"))
rm(family.sample, ASEC)


# reweight sample --------------------------------------------------------------

# compute sum of raw weights
load("main sample raw.RData")
individuals.raw = unique(raw.data[,c("CPSIDP", "YEAR", "MONTH", "WTFINL")])
individuals.raw = aggregate(formula = WTFINL ~ YEAR + MONTH, data = individuals.raw, FUN = sum)

# compute sum of weights in main sample
individuals.main = unique(main.sample[,c("CPSIDP", "YEAR", "MONTH", "WTFINL")])
individuals.main = aggregate(formula = WTFINL ~ YEAR + MONTH, data = individuals.main, FUN = sum)
individuals.main$reweight.fac.IND = individuals.raw$WTFINL / individuals.main$WTFINL

# reweight individual weights
main.sample = left_join(main.sample, individuals.main[,which(names(individuals.main) %in% c("YEAR", "MONTH", "reweight.fac.IND"))], by = c("YEAR", "MONTH"))
main.sample$WTFINL = main.sample$WTFINL * (main.sample$reweight.fac.IND)


# save -------------------------------------------------------------------------
#save(main.sample, file = ../2 impute EIPs and UI benefits/main sample.RData")

