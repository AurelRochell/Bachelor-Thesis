# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Bachelor thesis
# 3. analysis
# by Aurel Rochell  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Description ------------------------------------------------------------------
# This script analyzes simulated counterfactual poverty rates and creates results.


# Set up -----------------------------------------------------------------------
rm(list = ls())   # cleaning the environment
graphics.off()    # closing all plots

library(tidyverse)
library(Rmisc)
library(dplyr)

setwd("../data/results")  # needs to be adjusted accordingly


# allocated amount of EIPs -----------------------------------------------------
EIPs1 = data.frame()
EIPs2 = data.frame()
EIPs3 = data.frame()

for(i in 1:10) {
  load(paste0("EIPs/EIP stats ", i, ".RData"))
  EIPs1 = rbind(EIPs1, EIPs1.stats)
  EIPs2 = rbind(EIPs2, EIPs2.stats)
  EIPs3 = rbind(EIPs3, EIPs3.stats)
}

summarySE(data = EIPs1, measurevar = "tax.unit.WTFINL * EIPs1", groupvars = c("YEAR", "MONTH"), na.rm = TRUE)
summarySE(data = EIPs1, measurevar = "tax.unit.WTFINL", groupvars = c("YEAR", "MONTH"), na.rm = TRUE)
summarySE(data = EIPs1, measurevar = "Recipiency.rate", groupvars = c("YEAR", "MONTH"), na.rm = TRUE)

summarySE(data = EIPs2, measurevar = "tax.unit.WTFINL * EIPs2", groupvars = c("YEAR", "MONTH"), na.rm = TRUE)
summarySE(data = EIPs2, measurevar = "tax.unit.WTFINL", groupvars = c("YEAR", "MONTH"), na.rm = TRUE)
summarySE(data = EIPs2, measurevar = "Recipiency.rate", groupvars = c("YEAR", "MONTH"), na.rm = TRUE)

summarySE(data = EIPs3, measurevar = "total.amount", groupvars = c("YEAR", "MONTH"), na.rm = TRUE)
summarySE(data = EIPs3, measurevar = "total.payments", groupvars = c("YEAR", "MONTH"), na.rm = TRUE)
summarySE(data = EIPs3, measurevar = "Recipiency.rate", groupvars = c("YEAR", "MONTH"), na.rm = TRUE)

rm(EIPs1, EIPs2, EIPs3, EIPs1.stats, EIPs2.stats, EIPs3.stats)


# allocated amount of UI -------------------------------------------------------
state.UI = data.frame()
PEUC = data.frame()
PUA = data.frame()
FPUC = data.frame()

for(i in 1:10) {
  load(paste0("UI/UI stats", i, ".RData"))
  state.UI = rbind(state.UI, UI.coverage)
  PEUC = rbind(PEUC, PEUC.coverage)
  PUA = rbind(PUA, PUA.coverage)
  FPUC = rbind(FPUC, FPUC.coverage)
}

summarySE(data = state.UI, measurevar = "aggregated.amount", groupvars = c("YEAR", "MONTH"), na.rm = TRUE)
summarySE(data = PEUC, measurevar = "aggregated.amount", groupvars = c("YEAR", "MONTH"), na.rm = TRUE)
summarySE(data = PUA, measurevar = "aggregated.amount", groupvars = c("YEAR", "MONTH"), na.rm = TRUE)
summarySE(data = FPUC, measurevar = "amount", groupvars = c("YEAR", "MONTH"), na.rm = TRUE)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Results

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

rm(list = ls())


# compute results from 10 run interations --------------------------------------

load("poverty thresholds 2010-2021.RData")
pov.thresholds$cutoff = as.integer(pov.thresholds$cutoff)
pov.thresholds$is.65[which(is.na(pov.thresholds$is.65))] = 2

poverty.original = data.frame()
poverty.EIPs = data.frame()
poverty.UI = data.frame()
poverty.transfers = data.frame()
EIP.effect = data.frame()

states.poverty.original = data.frame()
states.poverty.EIPs = data.frame()
states.poverty.UI = data.frame()
states.poverty.transfers = data.frame()
difference = data.frame()

deep.poverty.original = data.frame()
deep.poverty.EIPs = data.frame()
deep.poverty.UI = data.frame()
deep.poverty.transfers = data.frame()

for (i in 1:10) {
  # load a sample
  load(paste0("analysis sample", i, ".RData"))
  
  # still have to aggreagte UI benefits on family level
  UI.family = aggregate(cbind(PEUC, PUA, FPUC) ~ YEAR + MONTH + HRHHID + HRHHID2, main.sample[,which(names(main.sample) %in% c("HRHHID", "HRHHID2", "YEAR", "MONTH", "PEUC", "PUA", "FPUC"))], FUN = sum)
  main.sample = main.sample[,which(!(names(main.sample) %in% c("PEUC", "PUA", "FPUC")))]
  main.sample = left_join(main.sample, UI.family, by = c("YEAR", "MONTH", "HRHHID", "HRHHID2"))
  
  # derive poverty status of each family
  main.sample$refyear = ifelse(main.sample$MONTH %in% c(1:6), main.sample$YEAR - 1, main.sample$YEAR)
  main.sample = main.sample[,which(!(names(main.sample) %in% c("famsize", "nochildren", "is.65")))]
  main.sample = inner_join(main.sample, dplyr::rename(aggregate(AGE ~ YEAR + refyear + HRHHID + HRHHID2, main.sample, FUN = function(x) length(x)), "famsize" = AGE), by = c("YEAR", "refyear", "HRHHID", "HRHHID2"))
  main.sample = left_join(main.sample, dplyr::rename(aggregate(AGE ~ YEAR + refyear + HRHHID + HRHHID2, main.sample[main.sample$RELATE != "101",], FUN = function(x) length(which(x < 18))), "nochildren" = AGE), by = c("YEAR", "refyear", "HRHHID", "HRHHID2"))
  main.sample = inner_join(main.sample, dplyr::rename(aggregate(as.integer(AGE) ~ YEAR + refyear + HRHHID + HRHHID2, main.sample[main.sample$RELATE %in% c("101"),], FUN = function(x) length(which(x >= 65))), "is.65" = `as.integer(AGE)`), by = c("YEAR", "refyear", "HRHHID", "HRHHID2"))
  main.sample$famsize = ifelse(main.sample$famsize >= 9, 9, main.sample$famsize)
  main.sample$nochildren = ifelse(is.na(main.sample$nochildren), 0, ifelse(main.sample$nochildren >= 8, 8, main.sample$nochildren))
  main.sample$is.65 = ifelse(main.sample$famsize >= 3, 2, main.sample$is.65)
  main.sample = inner_join(main.sample, pov.thresholds, by = c("refyear" = "year", "famsize", "nochildren", "is.65"))
  main.sample$YYYYMM = paste0(main.sample$YEAR, main.sample$MONTH)
  
  main.sample$WTFINL = as.integer(main.sample$WTFINL)
  

  # compute poverty --------------------------------------------------------------
  temp = inner_join(aggregate(WTFINL ~ YYYYMM, main.sample[main.sample$FTOTVAL < main.sample$cutoff,], FUN = sum), aggregate(WTFINL ~ YYYYMM, main.sample, FUN = sum), by = c("YYYYMM"))
  temp$pov.rate = (temp$WTFINL.x / temp$WTFINL.y) * 100
  poverty.original = rbind(poverty.original, temp)
  
  temp = inner_join(aggregate(WTFINL ~ YYYYMM, main.sample[main.sample$FTOTVAL - main.sample$EIPs1 - main.sample$EIPs2 - main.sample$EIPs3 < main.sample$cutoff,], FUN = sum), aggregate(WTFINL ~ YYYYMM, main.sample, FUN = sum), by = c("YYYYMM"))
  temp$pov.rate = (temp$WTFINL.x / temp$WTFINL.y) * 100
  poverty.EIPs = rbind(poverty.EIPs, temp)
  
  temp = inner_join(aggregate(WTFINL ~ YYYYMM, main.sample[main.sample$FTOTVAL - main.sample$PEUC - main.sample$PUA - main.sample$FPUC < main.sample$cutoff,], FUN = sum), aggregate(WTFINL ~ YYYYMM, main.sample, FUN = sum), by = c("YYYYMM"))
  temp$pov.rate = (temp$WTFINL.x / temp$WTFINL.y) * 100
  poverty.UI = rbind(poverty.UI, temp)
  
  temp = inner_join(aggregate(WTFINL ~ YYYYMM, main.sample[main.sample$FTOTVAL - main.sample$EIPs1 - main.sample$EIPs2 - main.sample$EIPs3 - main.sample$PEUC - main.sample$PUA - main.sample$FPUC < main.sample$cutoff,], FUN = sum), aggregate(WTFINL ~ YYYYMM, main.sample, FUN = sum), by = c("YYYYMM"))
  temp$pov.rate = (temp$WTFINL.x / temp$WTFINL.y) * 100
  poverty.transfers = rbind(poverty.transfers, temp)
  

  temp = inner_join(aggregate(WTFINL ~ YEAR + MONTH, main.sample[which(main.sample$FTOTVAL - main.sample$EIPs1 - main.sample$EIPs2 - main.sample$EIPs3 < main.sample$cutoff & main.sample$FTOTVAL > main.sample$cutoff),], FUN = sum),
                    aggregate(WTFINL ~ YEAR + MONTH, main.sample[which(main.sample$FTOTVAL - main.sample$EIPs1 - main.sample$EIPs2 - main.sample$EIPs3 < main.sample$cutoff & main.sample$FTOTVAL > main.sample$cutoff & (main.sample$PUA > 0 | main.sample$PEUC > 0 | main.sample$FPUC > 0)),], FUN = sum), by = c("YEAR", "MONTH"))
  temp$share = temp$WTFINL.y / temp$WTFINL.x
  EIP.effect = rbind(EIP.effect, temp)

  # variation between state groups -----------------------------------------------
  main.sample$group = as.character(main.sample$group)

  temp = inner_join(aggregate(WTFINL ~ YYYYMM + group, main.sample[main.sample$FTOTVAL < main.sample$cutoff,], FUN = sum), aggregate(WTFINL ~ YYYYMM + group, main.sample, FUN = sum), by = c("YYYYMM", "group"))
  temp$pov.rate = (temp$WTFINL.x / temp$WTFINL.y) * 100
  states.poverty.original = rbind(states.poverty.original, temp)
  
  temp = inner_join(aggregate(WTFINL ~ YYYYMM + group, main.sample[main.sample$FTOTVAL - main.sample$PEUC - main.sample$PUA - main.sample$FPUC < main.sample$cutoff,], FUN = sum), aggregate(WTFINL ~ YYYYMM + group, main.sample, FUN = sum), by = c("YYYYMM", "group"))
  temp$pov.rate = (temp$WTFINL.x / temp$WTFINL.y) * 100
  states.poverty.UI = rbind(states.poverty.UI, temp)
  
  temp = inner_join(aggregate(WTFINL ~ YYYYMM + group, main.sample[main.sample$FTOTVAL - main.sample$EIPs1 - main.sample$EIPs2 - main.sample$EIPs3 < main.sample$cutoff,], FUN = sum), aggregate(WTFINL ~ YYYYMM + group, main.sample, FUN = sum), by = c("YYYYMM", "group"))
  temp$pov.rate = (temp$WTFINL.x / temp$WTFINL.y) * 100
  states.poverty.EIPs = rbind(states.poverty.EIPs, temp)
  
  temp = inner_join(states.poverty.original, states.poverty.UI, by = c("YYYYMM", "group"))
  temp$difference = temp$pov.rate.y - temp$pov.rate.x
  difference = rbind(difference, temp)

  
  # deep poverty -----------------------------------------------------------------
  temp = inner_join(aggregate(WTFINL ~ YYYYMM, main.sample[main.sample$FTOTVAL < main.sample$cutoff * 0.5,], FUN = sum), aggregate(WTFINL ~ YYYYMM, main.sample, FUN = sum), by = c("YYYYMM"))
  temp$pov.rate = (temp$WTFINL.x / temp$WTFINL.y) * 100
  deep.poverty.original = rbind(deep.poverty.original, temp)
  
  temp = inner_join(aggregate(WTFINL ~ YYYYMM, main.sample[main.sample$FTOTVAL - main.sample$EIPs1 - main.sample$EIPs2 - main.sample$EIPs3 < main.sample$cutoff * 0.5,], FUN = sum), aggregate(WTFINL ~ YYYYMM, main.sample, FUN = sum), by = c("YYYYMM"))
  temp$pov.rate = (temp$WTFINL.x / temp$WTFINL.y) * 100
  deep.poverty.EIPs = rbind(deep.poverty.EIPs, temp)
  
  temp = inner_join(aggregate(WTFINL ~ YYYYMM, main.sample[main.sample$FTOTVAL - main.sample$PEUC - main.sample$PUA - main.sample$FPUC < main.sample$cutoff * 0.5,], FUN = sum), aggregate(WTFINL ~ YYYYMM, main.sample, FUN = sum), by = c("YYYYMM"))
  temp$pov.rate = (temp$WTFINL.x / temp$WTFINL.y) * 100
  deep.poverty.UI = rbind(deep.poverty.UI, temp)
  
  temp = inner_join(aggregate(WTFINL ~ YYYYMM, main.sample[main.sample$FTOTVAL - main.sample$EIPs1 - main.sample$EIPs2 - main.sample$EIPs3 - main.sample$PEUC - main.sample$PUA - main.sample$FPUC < main.sample$cutoff * 0.5,], FUN = sum), aggregate(WTFINL ~ YYYYMM, main.sample, FUN = sum), by = c("YYYYMM"))
  temp$pov.rate = (temp$WTFINL.x / temp$WTFINL.y) * 100
  deep.poverty.transfers = rbind(deep.poverty.transfers, temp)
  rm(main.sample)
}


# poverty on national level ----------------------------------------------------
poverty.original$YYYYMM = as.Date(paste0(substr(poverty.original$YYYYMM, 1, 4), "-", substr(poverty.original$YYYYMM, 5, 6), "-01"))
summarySE(data = poverty.original, measurevar = "pov.rate", groupvars = c("YYYYMM"), na.rm = TRUE)

poverty.transfers$YYYYMM = as.Date(paste0(substr(poverty.transfers$YYYYMM, 1, 4), "-", substr(poverty.transfers$YYYYMM, 5, 6), "-01"))
summarySE(data = poverty.transfers, measurevar = "pov.rate", groupvars = c("YYYYMM"), na.rm = TRUE)

poverty.EIPs$YYYYMM = as.Date(paste0(substr(poverty.EIPs$YYYYMM, 1, 4), "-", substr(poverty.EIPs$YYYYMM, 5, 6), "-01"))
summarySE(data = poverty.EIPs, measurevar = "pov.rate", groupvars = c("YYYYMM"), na.rm = TRUE)

poverty.UI$YYYYMM = as.Date(paste0(substr(poverty.UI$YYYYMM, 1, 4), "-", substr(poverty.UI$YYYYMM, 5, 6), "-01"))
summarySE(data = poverty.UI, measurevar = "pov.rate", groupvars = c("YYYYMM"), na.rm = TRUE)

poverty = data.frame("YYYYMM" = c(summarySE(data = poverty.original, measurevar = "pov.rate", groupvars = c("YYYYMM"), na.rm = TRUE)$YYYYMM, summarySE(data = poverty.transfers, measurevar = "pov.rate", groupvars = c("YYYYMM"), na.rm = TRUE)$YYYYMM), 
                     "pov.rate" = c(summarySE(data = poverty.original, measurevar = "pov.rate", groupvars = c("YYYYMM"), na.rm = TRUE)$pov.rate, summarySE(data = poverty.transfers, measurevar = "pov.rate", groupvars = c("YYYYMM"), na.rm = TRUE)$pov.rate), "estimate" = c(rep("observed poverty", 24), rep("poverty absent of all transfers", 24)))
poverty$estimate <- factor(poverty$estimate, rev(c("observed poverty", "poverty absent of all transfers")))
ggplot(poverty, aes(x = YYYYMM, y = pov.rate)) +
  geom_line(aes(color = estimate)) +
  geom_point(color = "black") +
  scale_x_date(date_labels = "%b-%y", breaks = seq(from = as.Date("2020-01-01") , to = as.Date("2021-12-01"), by = "1 month")) +
  scale_y_continuous(breaks = c(10, 12, 14, 16, 18, 20, 22)) +
  labs(x = element_blank(), y = "poverty rate [%]", color = "Poverty estimate") +
  theme_light() +
  theme(legend.position=c(.9,.9))

#ggsave("regular poverty plot.png", device = "png")

EIP.effect$YYYYMM = as.Date(paste0(EIP.effect$YEAR, "-", EIP.effect$MONTH, "-01"))
summarySE(data = EIP.effect, measurevar = "share", groupvars = c("YYYYMM"), na.rm = TRUE)


# state-group analysis ---------------------------------------------------------
difference$YYYYMM = as.Date(paste0(substr(difference$YYYYMM, 1, 4), "-", substr(difference$YYYYMM, 5, 6), "-01"))
difference = summarySE(data = difference, measurevar = "difference", groupvars = c("YYYYMM", "group"), na.rm = TRUE)
difference$group = as.factor(difference$group)
levels(difference$group) = c("high-recipiency", "medium-recipiency", "low-recipiency")
ggplot(difference, aes(x = YYYYMM, y = difference)) +
  geom_line(aes(color = group)) +
  geom_point(color = "black") +
  scale_x_date(date_labels = "%b-%y", breaks = seq(from = as.Date("2020-01-01") , to = as.Date("2021-12-01"), by = "1 month")) +
  scale_y_continuous(breaks = c(0:6)) +
  labs(x = element_blank(), y = "percentage points", color = "State group") +
  theme_light() +
  theme(legend.position=c(.9,.9))

#ggsave("marginal state effects.png", device = "png")


# deep poverty -----------------------------------------------------------------
deep.poverty.original$YYYYMM = as.Date(paste0(substr(deep.poverty.original$YYYYMM, 1, 4), "-", substr(deep.poverty.original$YYYYMM, 5, 6), "-01"))
summarySE(data = deep.poverty.original, measurevar = "pov.rate", groupvars = c("YYYYMM"), na.rm = TRUE)

deep.poverty.transfers$YYYYMM = as.Date(paste0(substr(deep.poverty.transfers$YYYYMM, 1, 4), "-", substr(deep.poverty.transfers$YYYYMM, 5, 6), "-01"))
summarySE(data = deep.poverty.transfers, measurevar = "pov.rate", groupvars = c("YYYYMM"), na.rm = TRUE)

deep.poverty.EIPs$YYYYMM = as.Date(paste0(substr(deep.poverty.EIPs$YYYYMM, 1, 4), "-", substr(deep.poverty.EIPs$YYYYMM, 5, 6), "-01"))
summarySE(data = deep.poverty.EIPs, measurevar = "pov.rate", groupvars = c("YYYYMM"), na.rm = TRUE)

deep.poverty.UI$YYYYMM = as.Date(paste0(substr(deep.poverty.UI$YYYYMM, 1, 4), "-", substr(deep.poverty.UI$YYYYMM, 5, 6), "-01"))
summarySE(data = deep.poverty.UI, measurevar = "pov.rate", groupvars = c("YYYYMM"), na.rm = TRUE)

poverty = data.frame("YYYYMM" = c(summarySE(data = deep.poverty.original, measurevar = "pov.rate", groupvars = c("YYYYMM"), na.rm = TRUE)$YYYYMM, summarySE(data = deep.poverty.transfers, measurevar = "pov.rate", groupvars = c("YYYYMM"), na.rm = TRUE)$YYYYMM), 
                     "pov.rate" = c(summarySE(data = deep.poverty.original, measurevar = "pov.rate", groupvars = c("YYYYMM"), na.rm = TRUE)$pov.rate, summarySE(data = deep.poverty.transfers, measurevar = "pov.rate", groupvars = c("YYYYMM"), na.rm = TRUE)$pov.rate), "estimate" = c(rep("observed poverty", 24), rep("poverty absent of all transfers", 24)))
poverty$estimate <- factor(poverty$estimate, rev(c("observed poverty", "poverty absent of all transfers")))
ggplot(poverty, aes(x = YYYYMM, y = pov.rate)) +
  geom_line(aes(color = estimate)) +
  geom_point(color = "black") +
  scale_x_date(date_labels = "%b-%y", breaks = seq(from = as.Date("2020-01-01") , to = as.Date("2021-12-01"), by = "1 month")) +
  scale_y_continuous(breaks = c(4, 6, 8, 10, 12)) +
  labs(x = element_blank(), y = "poverty rate [%]", color = "Poverty estimate (<50% OPL)") +
  theme_light() +
  theme(legend.position=c(.9,.9))

#ggsave("deep poverty plot.png", device = "png")

