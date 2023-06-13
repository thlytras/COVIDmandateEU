# Data available from ECDC: https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/xlsx/data.xlsx

library(readxl)
dat.ecdc <- read_excel("data.xlsx")
dat.ecdc <- as.data.frame(subset(dat.ecdc, ReportingCountry %in% c("AT","IT","EL")))
dat.ecdc <- subset(dat.ecdc, nchar(Region)==2)

# Aggregate over all vaccine formulations
dat <- aggregate(dat.ecdc[,"FirstDose",drop=FALSE], dat.ecdc[,c("YearWeekISO", "ReportingCountry", "Denominator", "TargetGroup")], sum, na.rm=TRUE)
# Convert week into a more convenient format
dat$YearWeekISO <- as.integer(gsub("-W", "", dat$YearWeekISO, fixed=TRUE))
names(dat)[1] <- "wk"


# Function to take part of the dataset over a certain period, aggregate first doses and add unvaccinated column
aggrBy <- function(e, week=c(202052,202201), label=NA) {
  e <- substitute(e)
  res <- subset(dat, eval(e))
  res <- aggregate(res[,c("Denominator","FirstDose")], res[,c("wk","ReportingCountry")], sum)
  names(res) <- c("wk", "country", "N", "firstDoses")
  res <- res[order(res$wk),]
  res$unvacc <- res$N - cumsum(res$firstDose) # Calculate how many are unvaccinated yet
  res$label <- label  # Put a label over the part of the dataset we're selecting
  res <- subset(res, wk>=week[1] & wk<=week[2]) # Select the period of interest
  res$t <- 1:nrow(res)
  res
}

# Easily identify the ID of a certain week in the aggregate datasets
iwk <- function(w, d=datGR) match(w, sort(unique(d$wk)))


# Now build the datasets that we will further process
# Study period is week 35/2021 to 50/2022 in all cases

# Austria
datAT <- rbind(
  aggrBy(ReportingCountry=="AT" & TargetGroup=="ALL", c(202135,202250), "All")
)

# Greece
datGR <- rbind(
  aggrBy(ReportingCountry=="EL" & TargetGroup=="Age50_59", c(202135,202250), "50-59"),
  aggrBy(ReportingCountry=="EL" & TargetGroup=="1_Age60+", c(202135,202250), "60+")
)

# Italy
datIT <- rbind(
  aggrBy(ReportingCountry=="IT" & TargetGroup=="Age25_49", c(202135,202250), "25-49"),
  aggrBy(ReportingCountry=="IT" & TargetGroup %in% c("Age50_59", "Age60_69", "Age70_79", "Age80+"), c(202135,202250), "50+")
)


# Add extra columns: 
# -- indicators for the mandate sub-periods
# -- slopes for the mandate sub-periods
# -- Observation-level random effect (OLRE)

# Greece
# 1. Mandate announcement: Tue 30 Nov = week 48/2021
# 2. Effective: Sun 16 Jan = week 2/2022 (3/2022 first week after)
# 3. Suspended: Fri 15 April = week 15/2022 (16/2022 first week after)
# Announcement period: 202148 - 202202
# Effective period: 202203 - 202215

rownames(datGR) <- NULL
datGR$mandate <- 1
datGR$mandate[with(datGR, label=="60+" & (wk>=202148 & wk<=202202))] <- 2
datGR$mandate[with(datGR, label=="60+" & (wk>=202203 & wk<=202215))] <- 3
datGR$mandate[with(datGR, label=="60+" & wk>=202216)] <- 4
datGR$olre <- 1:nrow(datGR)
for (i in 2:4) {
  datGR[,paste0("sl",i)] <- 0
  datGR[datGR$mandate==i, paste0("sl",i)] <- 1:nrow(datGR[datGR$mandate==i,]) - 1
}

# Italy
# 1. Mandate announcement: Wed Jan 5 = week 1/2022 (2/2022 first week after)
# 2. Workplace restrictions effective: Tue Feb 15 = week 7/2022
# 3. €100 fine effective: Wed Jun 15 = week 24/2022
# Stop announced: Mon Oct 31 = week 44/2022
# Announcement period: 202202 - 202206
# Effective period A: 202207 - 202223
# Effective period B: 202224 - 202243

rownames(datIT) <- NULL
datIT$mandate <- 1
datIT$mandate[with(datIT, label=="50+" & (wk>=202202 & wk<=202206))] <- 2
datIT$mandate[with(datIT, label=="50+" & (wk>=202207 & wk<=202223))] <- 3
datIT$mandate[with(datIT, label=="50+" & (wk>=202224 & wk<=202243))] <- 4
datIT$mandate[with(datIT, label=="50+" & wk>=202244)] <- 5
datIT$olre <- 1:nrow(datIT)
for (i in 2:5) {
  datIT[,paste0("sl",i)] <- 0
  datIT[datIT$mandate==i, paste0("sl",i)] <- 1:nrow(datIT[datIT$mandate==i,]) - 1
}


# Austria
# 1. Mandate announced: Fri Nov 19 = week 46/2021 (47/2021 first week after)
# 2. Suspension of mandate: Wed Mar 9 = week 10/2022 (11/2022 first week after)
# Law cancelled: Thu Jun 23 = week 25/2022 (26/2022 first week after)

rownames(datAT) <- NULL
datAT$mandate <- 1
datAT$mandate[with(datAT, wk>=202147 & wk<=202210)] <- 2
datAT$mandate[with(datAT, wk>=202211)] <- 3
datAT$olre <- 1:nrow(datAT)
for (i in 1:3) {
  datAT[,paste0("sl",i)] <- 0
  datAT[datAT$mandate==i, paste0("sl",i)] <- 1:nrow(datAT[datAT$mandate==i,]) - 1
}
