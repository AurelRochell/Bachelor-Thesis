# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Bachelor thesis
# 1.1 sample creation historic validation
# by Aurel Rochell
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Description ------------------------------------------------------------------
# This scripts assesses the suitability of creating a representative sample for
# computing poverty rates and benchmarks the approach to annually reported poverty rates.


# Set up -----------------------------------------------------------------------
rm(list = ls())   # cleaning the environment
graphics.off()    # closing all plots

library(tidyverse)
library(data.table)

setwd("../data/1 create sample")  # needs to be adjusted accordingly


# Create datasets --------------------------------------------------------------
# load cps monthly data
load("cps 2017-2022.RData")  # due to the size of the datasets, years 2010-2012, 2013-2016 and 2017-2022 must be computed separately. Change which dataset should be loaded here
CPS$refyear = ifelse(CPS$MONTH %in% c("1", "2", "3", "4", "5", "6"), as.character(as.integer(CPS$YEAR) - 1), CPS$YEAR)  # create variable denoting which poverty thresholds to use (from which year)

# load ASEC data
load("cps asec 2009-2022.RData")
ASEC$FTOTVAL = as.integer(ASEC$FTOTVAL)
ASEC$refyear = as.character(as.integer(ASEC$YEAR) - 1)  # CPS is questioned in March of the following year, thus the year referred to is always the previous year (CPS March 2020 asks for income in 2019)
ASEC = ASEC[ASEC$refyear %in% unique(CPS$refyear),]  # select those ASEC years that are later needed for the CPS conversion

# select families of the head of household
CPS = CPS[CPS$FTYPE %in% c("1", "2", "3"),]
ASEC = ASEC[ASEC$FTYPE %in% c("1", "2", "3"),]

# load official poverty thresholds
load("poverty thresholds 2009-2021.RData")
pov.thresholds$year = as.character(pov.thresholds$year)


# Create matching variables in the CPS ASEC ------------------------------------

# aggregate subfamily income to family income
ASEC = inner_join(ASEC, rename(aggregate(FTOTVAL ~ YEAR + refyear + HRHHID + HRHHID2, unique(ASEC[,which(names(ASEC) %in% c("YEAR", "refyear", "HRHHID", "HRHHID2", "FAMID", "FTOTVAL"))]), FUN = sum), "family.inc" = FTOTVAL), by = c("YEAR", "refyear", "HRHHID", "HRHHID2"))

# create demographic matching variables
ASEC = inner_join(ASEC, rename(aggregate(AGE ~ YEAR + refyear + HRHHID + HRHHID2, ASEC, FUN = function(x) length(x)), "famsize" = AGE), by = c("YEAR", "refyear", "HRHHID", "HRHHID2"))
ASEC = left_join(ASEC, rename(aggregate(AGE ~ YEAR + refyear + HRHHID + HRHHID2, ASEC[ASEC$RELATE != "101",], FUN = function(x) length(which(x < 18))), "nochildren" = AGE), by = c("YEAR", "refyear", "HRHHID", "HRHHID2"))
ASEC = inner_join(ASEC, rename(aggregate(as.integer(AGE) ~ YEAR + refyear + HRHHID + HRHHID2, ASEC[ASEC$RELATE %in% c("101"),], FUN = function(x) length(which(x >= 65))), "is.65" = `as.integer(AGE)`), by = c("YEAR", "refyear", "HRHHID", "HRHHID2"))
ASEC$famsize = ifelse(ASEC$famsize >= 5, 5, ASEC$famsize)
ASEC$nochildren = ifelse(is.na(ASEC$nochildren), 0, ifelse(ASEC$nochildren >= 2, 2, ASEC$nochildren))
ASEC$is.65 = ifelse(ASEC$famsize >= 3, 2, ifelse(ASEC$famsize == 2 & ASEC$nochildren == 1, 2, ASEC$is.65))  # in the CPS ASEC, there are no families of size 2 with a single children where the head of household is above 65

# create income bins in CPS ASEC
ASEC = data.frame(unique(ASEC[,which(names(ASEC) %in% c("refyear", "family.inc", "famsize", "nochildren", "is.65"))]), "FAMINC" = NA)
ASEC$FAMINC = ifelse(ASEC$family.inc < 5000, "100", ifelse(ASEC$family.inc < 7500, "210", ifelse(ASEC$family.inc < 10000, "300", ifelse(ASEC$family.inc < 12500, "430", ifelse(ASEC$family.inc < 15000, "470", ifelse(ASEC$family.inc < 20000, "500", ifelse(ASEC$family.inc < 25000, "600", ifelse(ASEC$family.inc < 30000, "710", ifelse(ASEC$family.inc < 35000, "720", ifelse(ASEC$family.inc < 40000, "730", ifelse(ASEC$family.inc < 50000, "740", ifelse(ASEC$family.inc < 60000, "820", ifelse(ASEC$family.inc < 75000, "830", ifelse(ASEC$family.inc < 100000, "841", ifelse(ASEC$family.inc < 150000, "842", ifelse(ASEC$family.inc >= 150000, "843", NA))))))))))))))))


# create matching variables in CPS ---------------------------------------------

# create demographic variables relevant to poverty thresholds
CPS = inner_join(CPS, rename(aggregate(AGE ~ YEAR + refyear + HRHHID + HRHHID2, CPS, FUN = function(x) length(x)), "famsize" = AGE), by = c("YEAR", "refyear", "HRHHID", "HRHHID2"))
CPS = left_join(CPS, rename(aggregate(AGE ~ YEAR + refyear + HRHHID + HRHHID2, CPS[CPS$RELATE != "101",], FUN = function(x) length(which(x < 18))), "nochildren" = AGE), by = c("YEAR", "refyear", "HRHHID", "HRHHID2"))
CPS = inner_join(CPS, rename(aggregate(as.integer(AGE) ~ YEAR + refyear + HRHHID + HRHHID2, CPS[CPS$RELATE %in% c("101"),], FUN = function(x) length(which(x >= 65))), "is.65" = `as.integer(AGE)`), by = c("YEAR", "refyear", "HRHHID", "HRHHID2"))
CPS$famsize = ifelse(CPS$famsize >= 9, 9, CPS$famsize)
CPS$nochildren = ifelse(is.na(CPS$nochildren), 0, ifelse(CPS$nochildren >= 8, 8, CPS$nochildren))
CPS$is.65 = ifelse(CPS$famsize >= 3, 2, CPS$is.65)

# apply poverty thresholds
CPS = inner_join(CPS, pov.thresholds, by = c("refyear" = "year", "famsize", "nochildren", "is.65"))

# adjust demographic variables for matching (using the original subgroups used for poverty thresholds is not possible because some groups do not contain any observations in the CPS ASEC)
CPS$famsize = ifelse(CPS$famsize >= 5, 5, CPS$famsize)
CPS$nochildren = ifelse(CPS$nochildren >= 2, 2, CPS$nochildren)
CPS$is.65 = ifelse(CPS$famsize == 2 & CPS$nochildren == 1, 2, CPS$is.65)


# match income from CPS with CPS ASEC ------------------------------------------

poverty = data.frame()

for(i in 1:20) {  # run 20 iterations to explore variation in poverty estimates
  
  # create sample of family incomes in the CPS ASEC
  family.sample = unique(CPS[,which(names(CPS) %in% c("YEAR", "MONTH", "refyear", "HRHHID", "HRHHID2", "FAMINC", "famsize", "nochildren", "is.65"))])
  family.sample = data.frame(family.sample, as.data.frame(setDT(ASEC)[setDT(family.sample), on = .(refyear, famsize, nochildren, is.65, FAMINC),
                                                                            {ri <- sample(.N, size = 1L)
                                                                            .(family.inc = family.inc[ri])}, by = .EACHI]))
  
  # match family income back to individuals
  CPS = inner_join(CPS, family.sample[,which(names(family.sample) %in% c("YEAR", "MONTH", "refyear", "HRHHID", "HRHHID2", "FAMINC", "family.inc"))], by = c("YEAR", "MONTH", "refyear", "HRHHID", "HRHHID2", "FAMINC"))
  rm(family.sample)
  
  
  # compute poverty rates --------------------------------------------------------
  
  # compute poverty
  CPS$WTFINL = as.integer(CPS$WTFINL)
  poverty.temp = inner_join(aggregate(WTFINL ~ YEAR + MONTH, CPS[CPS$family.inc < CPS$cutoff,], FUN = sum), aggregate(WTFINL ~ YEAR + MONTH, CPS, FUN = sum), by = c("YEAR", "MONTH"))
  poverty.temp$pov.rate = (poverty.temp$WTFINL.x / poverty.temp$WTFINL.y) * 100
  poverty.temp = data.frame(poverty.temp[,which(names(poverty.temp) %in% c("YEAR", "MONTH", "pov.rate"))], "poverty measure" = "monthly")
  
  # set up next iteration of loop
  CPS = CPS[,which(names(CPS) != "family.inc")]
  poverty = rbind(poverty, poverty.temp)
}

# compute poverty measure statistics
poverty = inner_join(aggregate(pov.rate ~ YEAR + MONTH, poverty, FUN = mean), aggregate(pov.rate ~ YEAR + MONTH, poverty, FUN = sd), by = c("YEAR", "MONTH"))
poverty = rename(poverty, "mean" = pov.rate.x, "sd" = pov.rate.y)

#save
#save(poverty, file = "monthly poverty 2017-2022.RData")  # save computation for given years


# plot historic poverty --------------------------------------------------------
# uses the three time series for historic poverty rates created above

rm(list = ls())

# load historic poverty
historic.poverty = data.frame()
for (i in c("monthly poverty 2010-2012.RData", "monthly poverty 2013-2016.RData", "monthly poverty 2017-2022.RData")) {
  load(i)
  historic.poverty = rbind(historic.poverty, data.frame(poverty, "poverty.measure" = "monthly CPS"))
}
historic.poverty$YYYYMM = as.Date(paste0(historic.poverty$YEAR, "-", ifelse(nchar(historic.poverty$MONTH) == 1, paste0("0", historic.poverty$MONTH),historic.poverty$MONTH), "-01"))
historic.poverty = historic.poverty[order(historic.poverty$YYYYMM),]

# I use the official poverty rates as a benchmark, imported from https://cps.ipums.org/cps/poverty_notes.shtml.
historic.poverty = rbind(historic.poverty, data.frame("YEAR" = 2009:2021, "MONTH" = NA, "mean" = c(14.3, 15.1, 15, 15, 14.5, 14.8, 13.5, 12.7, 12.3, 11.8, 10.5, 11.4, 11.6), "sd" = rep(0, 13), "YYYYMM" = paste0(2009:2021, "-12-01"), "poverty.measure" = rep("CPS ASEC", 13)))

# also using the ACS poverty rates, imported from https://data.census.gov/table?q=ACS+poverty&tid=ACSST1Y2021.S1701
historic.poverty = rbind(historic.poverty, data.frame("YEAR" = c(2009:2019, 2021), "MONTH" = NA, "mean" = c(14.3, 15.3, 15.9, 15.9, 15.8, 15.5, 14.7, 14, 13.4, 13.1, 12.3, 12.8), "sd" = rep(0, 12), "YYYYMM" = paste0(c(2009:2019, 2021), "-12-01"), "poverty.measure" = rep("ACS", 12)))

historic.poverty = historic.poverty[historic.poverty$YEAR != 2022,]

# create plot
ggplot(historic.poverty, aes(x = YYYYMM, y = mean, color = poverty.measure)) +
  geom_line() +
  scale_x_date(date_labels = "%b-%y", breaks = seq(from = as.Date("2009-12-01") , to = as.Date("2021-12-01"), by = "6 month")) +
  labs(x = "", y = "poverty rate [%]", color = "Poverty measure") +
  theme_light() +
  theme(legend.position=c(.9,.9), axis.title.x=element_blank())

#ggsave("historic validation plot.png", device = "png")

