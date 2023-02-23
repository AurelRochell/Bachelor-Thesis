# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Bachelor thesis
# 2.1 impute Economic Impact Payments
# by Aurel Rochell  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Description ------------------------------------------------------------------
# This script imputes Economic Impact Payments in the analysis sample.


# Set up -----------------------------------------------------------------------
rm(list = ls())   # cleaning the environment
graphics.off()    # closing all plots

library(tidyverse)

setwd("../data/2 impute EIPs and UI benefits")  # needs to be adjusted accordingly


# prepare data -----------------------------------------------------------------
load("main sample.RData")
main.sample$FTOTVAL = main.sample$family.inc
main.sample = main.sample[,which(names(main.sample) != "family.inc")]


# identify tax units -----------------------------------------------------------
# identify tax units
main.sample$tax.unit = paste0(main.sample$FTYPE, "-", main.sample$prfamnum, "-", main.sample$LINENO)
main.sample$tax.unit = case_when(
  main.sample$FTYPE == 1 & (main.sample$FAMREL %in% c(1, 2) | (main.sample$FAMREL %in% c(3, 4) & !(main.sample$MARST %in% c(1, 2)) & main.sample$AGE <= 23 & !(main.sample$WKSTAT %in% c(11, 12, 13, 50)))) ~ paste0(main.sample$FTYPE, "-", main.sample$prfamnum, "-0"),
  main.sample$FTYPE == 1 ~ paste0(main.sample$FTYPE, "-", main.sample$prfamnum, "-", main.sample$LINENO),
  main.sample$FTYPE == 2 ~ paste0(main.sample$FTYPE, "-", main.sample$prfamnum, "-", main.sample$LINENO),
  main.sample$FTYPE == 3 ~ paste0(main.sample$FTYPE, "-", main.sample$prfamnum, "-0"))

# compute tax unit weights by taking a mean across individual weights
tax.unit.weight = rename(aggregate(WTFINL ~ YEAR + MONTH + HRHHID + HRHHID2 + tax.unit, main.sample, FUN = mean), "tax.unit.WTFINL" = WTFINL)
main.sample = inner_join(main.sample, tax.unit.weight, by = c("YEAR", "MONTH", "HRHHID", "HRHHID2", "tax.unit"))


# allocate income across tax units ---------------------------------------------
# number of adults in each householder family and tax unit
adults.HHfam = aggregate(AGE ~ YEAR + MONTH + HRHHID + HRHHID2, main.sample[main.sample$AGE >= 18 | main.sample$WKSTAT %in% c(11, 12, 13, 50),], FUN = function(x) length(x))
adults.HHfam = left_join(unique(main.sample[,which(names(main.sample) %in% c("YEAR", "MONTH", "HRHHID", "HRHHID2"))]), adults.HHfam, by = c("YEAR", "MONTH", "HRHHID", "HRHHID2"))
adults.tax.unit = aggregate(AGE ~ YEAR + MONTH + HRHHID + HRHHID2 + tax.unit, main.sample[main.sample$AGE >= 18 | main.sample$WKSTAT %in% c(11, 12, 13, 50),], FUN = function(x) length(x))
adults.tax.unit = left_join(unique(main.sample[,which(names(main.sample) %in% c("YEAR", "MONTH", "HRHHID", "HRHHID2", "tax.unit"))]), adults.tax.unit, by = c("YEAR", "MONTH", "HRHHID", "HRHHID2", "tax.unit"))

# compute share of family adults in each tax unit
FINC.share = inner_join(adults.HHfam, adults.tax.unit, by = c("YEAR", "MONTH", "HRHHID", "HRHHID2"))
FINC.share$AGE.y[is.na(FINC.share$AGE.x)] = 1  # there are 9 households in which no adult or full time working individual exists. Fortunately, in all cases there is only one tax unit, thus I can allocate all income to this single tax unit
FINC.share$AGE.x[is.na(FINC.share$AGE.x)] = 1
FINC.share$AGE.y[is.na(FINC.share$AGE.y)] = 0  # in other cases where no adult or full time working individual exists in the tax unit, but does in a different tax unit from the same household, the former tax unit does not earn any share
FINC.share$FINC.share = FINC.share$AGE.y / FINC.share$AGE.x

# merge to main sample
main.sample = left_join(main.sample, FINC.share[,names(FINC.share) %in% c("YEAR", "MONTH", "HRHHID", "HRHHID2", "tax.unit", "FINC.share")], by = c("YEAR", "MONTH", "HRHHID", "HRHHID2", "tax.unit"))
sum(main.sample$WTFINL[main.sample$FINC.share != 1]) / sum(main.sample$WTFINL)  # 26.4% of individuals live in tax units where tax unit income is a share of family income

# compute tax unit income
main.sample$FTOTVAL.tax.unit = main.sample$FTOTVAL * main.sample$FINC.share


# compute characteristics of tax units -----------------------------------------
# compute number of qualifying children in each tax unit
qualifying.children = rename(aggregate(AGE ~ YEAR + MONTH + HRHHID + HRHHID2 + tax.unit, main.sample[(main.sample$FTYPE == 1 & main.sample$FAMREL %in% c(3, 4) & main.sample$AGE < 18 & !(main.sample$MARST %in% c(1, 2)) & !(main.sample$WKSTAT %in% c(11, 12, 13, 50))) | (main.sample$FTYPE == 3 & main.sample$FAMREL == 3 & main.sample$AGE < 18),], FUN = function(x) length(x)), "qualifying.children" = AGE)
main.sample = left_join(main.sample, qualifying.children, by = c("YEAR", "MONTH", "HRHHID", "HRHHID2", "tax.unit"))
main.sample$qualifying.children[is.na(main.sample$qualifying.children)] = 0

# compute number of qualifying adult dependents in each tax unit
qualifying.dependent = rename(aggregate(AGE ~ YEAR + MONTH + HRHHID + HRHHID2 + tax.unit, main.sample[(main.sample$tax.unit == "1-1-0" & !(main.sample$FAMREL %in% c(1, 2)) & main.sample$AGE >= 18),], FUN = function(x) length(x)), "qualifying.dependent" = AGE)
main.sample = left_join(main.sample, qualifying.dependent, by = c("YEAR", "MONTH", "HRHHID", "HRHHID2", "tax.unit"))
main.sample$qualifying.dependent[is.na(main.sample$qualifying.dependent)] = 0

# indicate which families file jointly
file.jointly = rename(aggregate(AGE ~ YEAR + MONTH + HRHHID + HRHHID2 + tax.unit, main.sample[main.sample$MARST == 1,], FUN = function(x) length(x)), "file.jointly" = AGE)
file.jointly$file.jointly = ifelse(file.jointly$file.jointly == 2, 1, NA)
main.sample = left_join(main.sample, file.jointly, by = c("YEAR", "MONTH", "HRHHID", "HRHHID2", "tax.unit"))
main.sample$file.jointly[is.na(main.sample$file.jointly)] = 0

# indicate which individuals can file as heads of households
main.sample$file.HH = ifelse(main.sample$file.jointly == 0 & (main.sample$qualifying.children > 0 | main.sample$qualifying.dependent > 0), 1, 0)


# assign 1st rounds of EIPs ----------------------------------------------------
tax.units = unique(main.sample[,names(main.sample) %in% c("HRHHID", "HRHHID2", "YEAR", "MONTH", "tax.unit", "FTOTVAL.tax.unit", "qualifying.children", "qualifying.dependent", "file.jointly", "file.HH", "tax.unit.WTFINL")])
tax.units$EIPs1 = ifelse((tax.units$YEAR == 2020 & tax.units$MONTH %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12)) | (tax.units$YEAR == 2021 & tax.units$MONTH != 12),
                        ifelse(tax.units$file.jointly == 1,
                               2400 + 500 * tax.units$qualifying.children - ifelse(tax.units$FTOTVAL.tax.unit > 150000, 0.05 * (tax.units$FTOTVAL.tax.unit - 150000), 0),
                               ifelse(tax.units$file.HH == 1,
                                      1200 + 500 * tax.units$qualifying.children - ifelse(tax.units$FTOTVAL.tax.unit > 112500, 0.05 * (tax.units$FTOTVAL.tax.unit - 112500), 0),
                                      1200 + 500 * tax.units$qualifying.children - ifelse(tax.units$FTOTVAL.tax.unit > 75000, 0.05 * (tax.units$FTOTVAL.tax.unit - 75000), 0))),
                        0)
tax.units$EIPs1[tax.units$EIPs1 < 0] = 0

# I estimate about 154 million payments worth $282 billion
mean(aggregate((EIPs1 * tax.unit.WTFINL) ~ YEAR + MONTH, data = tax.units[tax.units$YEAR == 2020 & tax.units$MONTH %in% 4:12,], FUN = sum)$`(EIPs1 * tax.unit.WTFINL)`)
mean(aggregate(tax.unit.WTFINL ~ YEAR + MONTH, data = tax.units[tax.units$YEAR == 2020 & tax.units$MONTH %in% 4:12 & tax.units$EIPs1 > 0,], FUN = sum)$`tax.unit.WTFINL`)

# cap recipiency of EIPs1 as of April 17 to $160 billion across 89.5 million payments
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 4)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2020 & tax.units$MONTH == 4)]) > 160000000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 4 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 4 & tax.units$EIPs1 > 0)]), 2)] = 0
}
# cap recipiency of EIPs1 as of May 22 to $259 billion across 152.2 million payments
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 5)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2020 & tax.units$MONTH == 5)]) > 259000000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 5 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 5 & tax.units$EIPs1 > 0)]), 2)] = 0
}
# cap recipiency of EIPs1 in June to $260.8 billion (assuming linear trend)
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 6)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2020 & tax.units$MONTH == 6)]) > 260800000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 6 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 6 & tax.units$EIPs1 > 0)]), 2)] = 0
}
# cap recipiency of EIPs1 in July to $262.5 billion (assuming linear trend)
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 7)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2020 & tax.units$MONTH == 7)]) > 262500000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 7 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 7 & tax.units$EIPs1 > 0)]), 2)] = 0
}
# cap recipiency of EIPs1 in August to $264.3 billion (assuming linear trend)
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 8)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2020 & tax.units$MONTH == 8)]) > 264300000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 8 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 8 & tax.units$EIPs1 > 0)]), 2)] = 0
}
# cap recipiency of EIPs1 in September to $266.1 billion (assuming linear trend)
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 9)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2020 & tax.units$MONTH == 9)]) > 266100000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 9 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 9 & tax.units$EIPs1 > 0)]), 2)] = 0
}
# cap recipiency of EIPs1 in October to $267.9 billion (assuming linear trend)
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 10)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2020 & tax.units$MONTH == 10)]) > 267900000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 10 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 10 & tax.units$EIPs1 > 0)]), 2)] = 0
}
# cap recipiency of EIPs1 in November to $269.6 billion (assuming linear trend)
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 11)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2020 & tax.units$MONTH == 11)]) > 269900000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 11 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 11 & tax.units$EIPs1 > 0)]), 2)] = 0
}
# cap recipiency of EIPs1 in December to March at $271.4 billion across 161.9 million payments
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 12)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2020 & tax.units$MONTH == 12)]) > 271400000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 12 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2020 & tax.units$MONTH == 12 & tax.units$EIPs1 > 0)]), 2)] = 0
}
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 1)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 1)]) > 271400000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 1 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 1 & tax.units$EIPs1 > 0)]), 2)] = 0
}
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 2)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 2)]) > 271400000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 2 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 2 & tax.units$EIPs1 > 0)]), 2)] = 0
}
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 3)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 3)]) > 271400000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 3 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 3 & tax.units$EIPs1 > 0)]), 2)] = 0
}
# cap recipiency of EIPs1 in April 2021 to December 2021 by subtracting EIPs paid out a year ago or earlier from the end total in December 2020
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 4)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 4)]) > 111400000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 4 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 4 & tax.units$EIPs1 > 0)]), 2)] = 0
}
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 5)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 5)]) > 12400000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 5 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 5 & tax.units$EIPs1 > 0)]), 2)] = 0
}
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 6)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 6)]) > 10600000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 6 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 6 & tax.units$EIPs1 > 0)]), 2)] = 0
}
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 7)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 7)]) > 8900000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 7 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 7 & tax.units$EIPs1 > 0)]), 2)] = 0
}
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 8)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 8)]) > 7100000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 8 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 8 & tax.units$EIPs1 > 0)]), 2)] = 0
}
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 9)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 9)]) > 5300000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 9 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 9 & tax.units$EIPs1 > 0)]), 2)] = 0
}
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 10)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 10)]) > 3500000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 10 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 10 & tax.units$EIPs1 > 0)]), 2)] = 0
}
while(sum(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 11)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 11)]) > 1800000000) {
  tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 11 & tax.units$EIPs1 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 11 & tax.units$EIPs1 > 0)]), 2)] = 0
}

# number of payments and outlays per month
EIPs1.stats = inner_join(aggregate(tax.unit.WTFINL ~ YEAR + MONTH, data = tax.units[tax.units$EIPs1 > 0,], sum), aggregate(tax.unit.WTFINL * EIPs1 ~ YEAR + MONTH, data = tax.units[tax.units$EIPs1 > 0,], sum), by = c("YEAR", "MONTH"))
EIPs1.stats$Recipiency.rate = EIPs1.stats$tax.unit.WTFINL / 154000000


# assign 2nd round of EIPs -----------------------------------------------------
tax.units$EIPs2 = ifelse(tax.units$YEAR == 2021 & tax.units$MONTH %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                         ifelse(tax.units$file.jointly == 1,
                                1200 + 600 * tax.units$qualifying.children - ifelse(tax.units$FTOTVAL.tax.unit > 150000, 0.05 * (tax.units$FTOTVAL.tax.unit - 150000), 0),
                                ifelse(tax.units$file.HH == 1,
                                       600 + 600 * tax.units$qualifying.children - ifelse(tax.units$FTOTVAL.tax.unit > 112500, 0.05 * (tax.units$FTOTVAL.tax.unit - 112500), 0),
                                       600 + 600 * tax.units$qualifying.children - ifelse(tax.units$FTOTVAL.tax.unit > 75000, 0.05 * (tax.units$FTOTVAL.tax.unit - 75000), 0))),
                         0)
tax.units$EIPs2[tax.units$EIPs2 < 0] = 0

# I estimate about 154 million payments worth $161 billion
mean(aggregate((EIPs2 * tax.unit.WTFINL) ~ YEAR + MONTH, data = tax.units[tax.units$YEAR == 2021 & tax.units$MONTH %in% 1:12,], FUN = sum)$`(EIPs2 * tax.unit.WTFINL)`)
mean(aggregate(tax.unit.WTFINL ~ YEAR + MONTH, data = tax.units[tax.units$YEAR == 2021 & tax.units$MONTH %in% 1:12 & tax.units$EIPs2 > 0,], FUN = sum)$`tax.unit.WTFINL`)

# cap recipiency of EIPs2 to $128.6 billion (133 out of 147 million payments by 15th of January)
while(sum(tax.units$EIPs2[which(tax.units$YEAR == 2021 & tax.units$MONTH == 1)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 1)]) > 128600000000) {
  tax.units$EIPs2[which(tax.units$YEAR == 2021 & tax.units$MONTH == 1 & tax.units$EIPs2 > 0)][sample(length(tax.units$EIPs2[which(tax.units$YEAR == 2021 & tax.units$MONTH == 1 & tax.units$EIPs2 > 0)]), 2)] = 0
}
# cap recipiency of EIPs2 to $142.1 billion in all months following January (147 million payments)
for (i in 2:12) {
  while(sum(tax.units$EIPs2[which(tax.units$YEAR == 2021 & tax.units$MONTH == i)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == i)]) > 142100000000) {
    tax.units$EIPs2[which(tax.units$YEAR == 2021 & tax.units$MONTH == i & tax.units$EIPs2 > 0)][sample(length(tax.units$EIPs2[which(tax.units$YEAR == 2021 & tax.units$MONTH == i & tax.units$EIPs2 > 0)]), 2)] = 0
  }
}

# number of payments and outlays per month
EIPs2.stats = inner_join(aggregate(tax.unit.WTFINL ~ YEAR + MONTH, data = tax.units[tax.units$EIPs2 > 0,], sum), aggregate(tax.unit.WTFINL * EIPs2 ~ YEAR + MONTH, data = tax.units[tax.units$EIPs2 > 0,], sum), by = c("YEAR", "MONTH"))
EIPs2.stats$Recipiency.rate = EIPs2.stats$tax.unit.WTFINL / 151000000


# assign 3rd round of EIPs -----------------------------------------------------
tax.units = left_join(tax.units, rename(tax.units[tax.units$YEAR == 2020 & tax.units$MONTH %in% c(3:12), which(names(tax.units) %in% c("HRHHID", "HRHHID2", "tax.unit", "FTOTVAL.tax.unit", "qualifying.children", "qualifying.dependent"))], "FTOTVAL.tax.unit.alt" = FTOTVAL.tax.unit, "qualifying.children.alt" = qualifying.children, "qualifying.dependent.alt" = qualifying.dependent), by = c("HRHHID", "HRHHID2", "tax.unit"))
tax.units$EIPs3 = ifelse(tax.units$YEAR == 2021 & tax.units$MONTH %in% c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                         ifelse(tax.units$file.jointly == 1,
                                (2800 + 1400 * (tax.units$qualifying.children + tax.units$qualifying.dependent)) * ifelse(tax.units$FTOTVAL.tax.unit > 150000, 1 - ((tax.units$FTOTVAL.tax.unit - 150000) / 10000), 1),
                                ifelse(tax.units$file.HH == 1,
                                       (1400 + 1400 * (tax.units$qualifying.children + tax.units$qualifying.dependent)) * ifelse(tax.units$FTOTVAL.tax.unit > 112500, 1 - ((tax.units$FTOTVAL.tax.unit - 112500) / 7500), 1),
                                       (1400 + 1400 * (tax.units$qualifying.children + tax.units$qualifying.dependent)) * ifelse(tax.units$FTOTVAL.tax.unit > 75000, 1 - ((tax.units$FTOTVAL.tax.unit - 75000) / 5000), 1))),
                         0)
tax.units$EIPs3[tax.units$EIPs3 < 0] = 0
tax.units$EIPs3.alt = ifelse(tax.units$YEAR == 2021 & tax.units$MONTH %in% c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                           ifelse(tax.units$file.jointly == 1,
                                  (2800 + 1400 * (tax.units$qualifying.children.alt + tax.units$qualifying.dependent.alt)) * ifelse(tax.units$FTOTVAL.tax.unit.alt > 150000, 1 - ((tax.units$FTOTVAL.tax.unit.alt - 150000) / 10000), 1),
                                  ifelse(tax.units$file.HH == 1,
                                         (1400 + 1400 * (tax.units$qualifying.children.alt + tax.units$qualifying.dependent.alt)) * ifelse(tax.units$FTOTVAL.tax.unit.alt > 112500, 1 - ((tax.units$FTOTVAL.tax.unit.alt - 112500) / 7500), 1),
                                         (1400 + 1400 * (tax.units$qualifying.children.alt + tax.units$qualifying.dependent.alt)) * ifelse(tax.units$FTOTVAL.tax.unit.alt > 75000, 1 - ((tax.units$FTOTVAL.tax.unit.alt - 75000) / 5000), 1))),
                           NA)
tax.units$EIPs3.alt[tax.units$EIPs3.alt < 0 & !is.na(tax.units$EIPs3.alt)] = 0
tax.units$plus_up = ifelse(!is.na(tax.units$EIPs3.alt) & tax.units$EIPs3.alt < tax.units$EIPs3, tax.units$EIPs3  - tax.units$EIPs3.alt, 0)
tax.units$EIPs3[!is.na(tax.units$EIPs3.alt)] = tax.units$EIPs3.alt[!is.na(tax.units$EIPs3.alt)]

# I estimate about 146 million payments worth $378 billion in EIPs, and up to $13 billion across 6 million plus-up payments
mean(aggregate((EIPs3 * tax.unit.WTFINL) ~ YEAR + MONTH, data = tax.units[tax.units$YEAR == 2021 & tax.units$MONTH %in% 3:12,], FUN = sum)$`(EIPs3 * tax.unit.WTFINL)`)
mean(aggregate((plus_up * tax.unit.WTFINL) ~ YEAR + MONTH, data = tax.units[tax.units$YEAR == 2021 & tax.units$MONTH %in% 3:12,], FUN = sum)$`(plus_up * tax.unit.WTFINL)`)
mean(aggregate(tax.unit.WTFINL ~ YEAR + MONTH, data = tax.units[tax.units$YEAR == 2021 & tax.units$MONTH %in% 3:12 & tax.units$EIPs3 > 0,], FUN = sum)$`tax.unit.WTFINL`)
mean(aggregate(tax.unit.WTFINL ~ YEAR + MONTH, data = tax.units[tax.units$YEAR == 2021 & tax.units$MONTH %in% 3:12 & tax.units$plus_up > 0,], FUN = sum)$`tax.unit.WTFINL`)

# cap recipiency of EIPs3 in March to $242 billion (90 million payments)
while(sum(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 3)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 3)]) > 242000000000) {
  tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 3 & tax.units$EIPs3 > 0)][sample(length(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 3 & tax.units$EIPs3 > 0)]), 2)] = 0
}
tax.units$plus_up[which(tax.units$YEAR == 2021 & tax.units$MONTH == 3)] = 0  # no plus up payments in march yet
# cap recipiency of EIPs3 in April to $376 billion (159 million payments)
# plus-up payments started in April, in the round ending on the 7th more than 1 million plus-up payments worth nearly $3 billion, in the round ending on the 14th more than 700,000 worth $1.2 billion (https://www.irs.gov/newsroom/irs-treasury-disburse-more-economic-impact-payments-under-the-american-rescue-plan-total-tops-130-million-with-more-to-come, https://www.irs.gov/newsroom/irs-treasury-disburse-25-million-more-economic-impact-payments-under-the-american-rescue-plan, https://www.irs.gov/newsroom/irs-treasury-disburse-2-million-more-economic-impact-payments-under-the-american-rescue-plan-va-beneficiaries-bring-total-to-approximately-159-million-as-payments-continue)
# total of 1.7 million plus-up payments worth $4.2 billion (at least)
while(sum(tax.units$plus_up[which(tax.units$YEAR == 2021 & tax.units$MONTH == 4)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 4)]) > 4200000000) {
  tax.units$plus_up[which(tax.units$YEAR == 2021 & tax.units$MONTH == 4 & tax.units$plus_up > 0)][sample(length(tax.units$plus_up[which(tax.units$YEAR == 2021 & tax.units$MONTH == 4 & tax.units$plus_up > 0)]), 2)] = 0
}
while(sum(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 4)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 4)]) > 376000000000 - sum(tax.units$plus_up[which(tax.units$YEAR == 2021 & tax.units$MONTH == 4)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 4)])) {
  tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 4 & tax.units$EIPs3 > 0)][sample(length(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 4 & tax.units$EIPs3 > 0)]), 2)] = 0
}
# cap recipiency of EIPs3 in May to $389.9 billion (163.5 million payments)
# plus-up payments of an additional 700,000 worth $1.2 billion in the round ending on the 21nd, additional 730,000 worth $1.3 billion in the round ending on the 28th, additional 570,000 worth nearly $1 billion in the round ending on the 5th, additional 460,000 worth $800 million in the round ending on the 12th (total of more than 6 million payments) (https://www.irs.gov/newsroom/two-million-more-economic-impact-payments-disbursed-under-the-american-rescue-plan-total-reaches-approximately-161-million-as-payments-continue, https://www.irs.gov/newsroom/nearly-2-million-more-economic-impact-payments-disbursed-under-the-american-rescue-plan-continuing-payments-reach-approximately-163-million, https://www.irs.gov/newsroom/more-than-1-point-1-million-additional-economic-impact-payments-disbursed-under-the-american-rescue-plan-payments-total-approximately-164-million, https://www.irs.gov/newsroom/nearly-1-million-additional-economic-impact-payments-disbursed-under-the-american-rescue-plan-total-payments-reach-nearly-165-million)
# total of 4.16 million plus-up payments worth $8.5 billion (self-computed, at least)
while(sum(tax.units$plus_up[which(tax.units$YEAR == 2021 & tax.units$MONTH == 5)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 5)]) > 8500000000) {
  tax.units$plus_up[which(tax.units$YEAR == 2021 & tax.units$MONTH == 5 & tax.units$plus_up > 0)][sample(length(tax.units$plus_up[which(tax.units$YEAR == 2021 & tax.units$MONTH == 5 & tax.units$plus_up > 0)]), 2)] = 0
}
while(sum(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 5)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 5)]) > 389900000000 - sum(tax.units$plus_up[which(tax.units$YEAR == 2021 & tax.units$MONTH == 5)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 5)])) {
  tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 5 & tax.units$EIPs3 > 0)][sample(length(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 5 & tax.units$EIPs3 > 0)]), 2)] = 0
}
# cap recipiency of EIPs3 in June to $391.6 billion (assuming linear trend)
# more than 8 million plus-up payments (https://www.irs.gov/newsroom/more-than-2-point-3-million-additional-economic-impact-payments-disbursed-under-the-american-rescue-plan-total-payments-top-169-million)
# no more capping plus-up payments, as my imputation model does not impute more than 8 million payments
while(sum(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 6)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 6)]) > 391600000000 - sum(tax.units$plus_up[which(tax.units$YEAR == 2021 & tax.units$MONTH == 6)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 6)])) {
  tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 6 & tax.units$EIPs3 > 0)][sample(length(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 6 & tax.units$EIPs3 > 0)]), 2)] = 0
}
# cap recipiency of EIPs3 in July to $393.2 billion (assuming linear trend)
# more than 9 million plus-up payments worth $18.5 billion (https://www.irs.gov/newsroom/more-than-2-point-2-million-additional-economic-impact-payments-disbursed-under-the-american-rescue-plan)
while(sum(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 7)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 7)]) > 393200000000 - sum(tax.units$plus_up[which(tax.units$YEAR == 2021 & tax.units$MONTH == 7)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 7)])) {
  tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 7 & tax.units$EIPs3 > 0)][sample(length(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 7 & tax.units$EIPs3 > 0)]), 2)] = 0
}
# cap recipiency of EIPs3 in August to $394.9 billion (assuming linear trend)
while(sum(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 8)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 8)]) > 394900000000 - sum(tax.units$plus_up[which(tax.units$YEAR == 2021 & tax.units$MONTH == 8)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 8)])) {
  tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 8 & tax.units$EIPs3 > 0)][sample(length(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 8 & tax.units$EIPs3 > 0)]), 2)] = 0
}
# cap recipiency of EIPs3 in September to $396.5 billion (assuming linear trend)
while(sum(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 9)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 9)]) > 396500000000 - sum(tax.units$plus_up[which(tax.units$YEAR == 2021 & tax.units$MONTH == 9)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 9)])) {
  tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 9 & tax.units$EIPs3 > 0)][sample(length(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 9 & tax.units$EIPs3 > 0)]), 2)] = 0
}
# cap recipiency of EIPs3 in October to $398.2 billion (assuming linear trend)
while(sum(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 10)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 10)]) > 398200000000 - sum(tax.units$plus_up[which(tax.units$YEAR == 2021 & tax.units$MONTH == 10)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 10)])) {
  tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 10 & tax.units$EIPs3 > 0)][sample(length(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 10 & tax.units$EIPs3 > 0)]), 2)] = 0
}
# cap recipiency of EIPs3 in November to $399.8 billion (assuming linear trend)
while(sum(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 11)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 11)]) > 399800000000 - sum(tax.units$plus_up[which(tax.units$YEAR == 2021 & tax.units$MONTH == 11)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 11)])) {
  tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 11 & tax.units$EIPs3 > 0)][sample(length(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 11 & tax.units$EIPs3 > 0)]), 2)] = 0
}
# cap recipiency of EIPs3 in December to 167.6 million payments worth $401.5 billion
while(sum(tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 12)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 12)]) > 401500000000 - sum(tax.units$plus_up[which(tax.units$YEAR == 2021 & tax.units$MONTH == 12)] * tax.units$tax.unit.WTFINL[which(tax.units$YEAR == 2021 & tax.units$MONTH == 12)])) {
  tax.units$EIPs3[which(tax.units$YEAR == 2021 & tax.units$MONTH == 12 & tax.units$EIPs3 > 0)][sample(length(tax.units$EIPs1[which(tax.units$YEAR == 2021 & tax.units$MONTH == 12 & tax.units$EIPs3 > 0)]), 2)] = 0
}

# number of payments and outlays per month
EIPs3.stats = left_join(inner_join(aggregate(tax.unit.WTFINL ~ YEAR + MONTH, data = tax.units[tax.units$EIPs3 > 0,], sum), aggregate(tax.unit.WTFINL * EIPs3 ~ YEAR + MONTH, data = tax.units[tax.units$EIPs3 > 0,], sum), by = c("YEAR", "MONTH")), inner_join(aggregate(tax.unit.WTFINL ~ YEAR + MONTH, data = tax.units[tax.units$plus_up > 0,], sum), aggregate(tax.unit.WTFINL * plus_up ~ YEAR + MONTH, data = tax.units[tax.units$plus_up > 0,], sum), by = c("YEAR", "MONTH")), by = c("YEAR", "MONTH"))
EIPs3.stats$total.payments = EIPs3.stats$tax.unit.WTFINL.x + EIPs3.stats$tax.unit.WTFINL.y
EIPs3.stats$total.amount = EIPs3.stats$`tax.unit.WTFINL * EIPs3` + EIPs3.stats$`tax.unit.WTFINL * plus_up`
EIPs3.stats$Recipiency.rate = EIPs3.stats$tax.unit.WTFINL.x / 146000000

# add plus-up payments to EIPs
tax.units$EIPs3 = tax.units$EIPs3 + tax.units$plus_up


# aggregate on household level and save ----------------------------------------
# merge EIPs on tax unit level to main sample
main.sample = merge(main.sample, tax.units[,names(tax.units) %in% c("HRHHID", "HRHHID2", "YEAR", "MONTH", "tax.unit", "EIPs1", "EIPs2", "EIPs3")], by = c("HRHHID", "HRHHID2", "YEAR", "MONTH", "tax.unit"), all.x = TRUE)

# aggregate EIPs on family level
EIPs.family = aggregate(cbind(EIPs1, EIPs2, EIPs3) ~ YEAR + MONTH + HRHHID + HRHHID2, unique(main.sample[,which(names(main.sample) %in% c("HRHHID", "HRHHID2", "YEAR", "MONTH", "EIPs1", "EIPs2", "EIPs3", "tax.unit"))]), FUN = sum)
main.sample = main.sample[,which(!(names(main.sample) %in% c("EIPs1", "EIPs2", "EIPs3")))]
main.sample = left_join(main.sample, EIPs.family, by = c("YEAR", "MONTH", "HRHHID", "HRHHID2"))

# save
#save(main.sample, file = "main sample EIPs.RData")
#save(list = c("EIPs1.stats", "EIPs2.stats", "EIPs3.stats"), file = paste0("../results/EIPs/EIP stats 10.RData"))

