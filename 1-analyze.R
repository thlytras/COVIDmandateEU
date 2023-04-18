if (!exists("dat")) source("0-prep.R")

library(lme4)

mGR <- glmer(firstDoses ~ label + factor(mandate) + sl2 + sl3 + (1|wk) + (1|olre), offset=log(unvacc), data=datGR, family="poisson")
mGRns <- glmer(firstDoses ~ label + factor(mandate) + (1|wk) + (1|olre), offset=log(unvacc), data=datGR, family="poisson")
mIT <- glmer(firstDoses ~ label + factor(mandate) + sl2 + sl3 + sl4 + (1|wk) + (1|olre), offset=log(unvacc), data=datIT, family="poisson")
mITns <- glmer(firstDoses ~ label + factor(mandate) + (1|wk) + (1|olre), offset=log(unvacc), data=datIT, family="poisson")
mAT <- glmer(firstDoses ~ factor(mandate) + sl1 + sl2 + sl3 + (1|olre), offset=log(unvacc), data=datAT, family="poisson")

# Calculate Rate Ratios from model matrix (fixed-effects only)
RR_from_mm <- function(m, mm) {
  betas <- fixef(m)
  vcv <- vcov(m)
  pe <- mm %*% betas
  vr <- diag(mm %*% tcrossprod(vcv, mm))
  res <- as.data.frame.matrix(exp(cbind(pe, sqrt(vr)) %*% rbind(1, qnorm(c(0.5, 0.025, 0.975)))))
  colnames(res) <- c("RR", "RR.lo", "RR.hi")
  res  
}

# Calculate Rate Ratios at the ITS breakpoints
breakpointsRR <- function(m) {
  mm <- model.matrix(m)
  idx <- unname(sapply(grep("mandate", colnames(mm)), function(i) which(mm[,i]==1)[1])-1)
  mm <- do.call(rbind, lapply(idx, function(i) {
    mm[i+1,,drop=FALSE] - mm[i,,drop=FALSE]
  }))
  rownames(mm) <- NULL
  RR_from_mm(m, mm)
}

# Calculate the effect of the mandate periods
mandateRR <- function(m) {
  mm <- model.matrix(m)
  mm <- mm[sapply(grep("mandate", colnames(mm)), function(i) which(mm[,i]==1)[1]),]
  mm[,!grepl("mandate", colnames(mm))] <- 0
  RR_from_mm(m, mm)
}

durationRR <- function(m) {
  mm <- model.matrix(m)
  i <- which(mm[,"factor(mandate)2"]==1)[1]
  wk <- rownames(ranef(m)$wk)
  wk <- wk[(i-length(wk)):length(wk)]
  mm <- mm[i:nrow(mm),]
  mm[,1:2] <- 0
  rownames(mm) <- NULL
  RR_from_mm(m, mm)
}

if (file.exists("bootstrap.RData")) {
  load("bootstrap.RData")
} else {
  source("bootstrap.R")
}


calcBenefit <- function(m, dat, bb) {
  bbCumprod <- apply(1-exp(bb$t), 1, function(x) cumprod(x))
  bbCumprod <- rbind(1, bbCumprod)
  iM <- which(dat$mandate==2)[1]
  res <- as.data.frame(t(round(apply(dat$unvacc[iM-1] * bbCumprod, 1, quantile, probs=c(0.5,0.025,0.975)))))
  names(res) <- paste0("unvacc.noMnd", c("", ".lo", ".hi"))
  res$wk <- dat$wk[(iM-1):nrow(dat)]
  res$unvacc.obs <- dat$unvacc[(iM-1):nrow(dat)]
  res[,c(4:5,1:3)]
}

benefitGR <- calcBenefit(mGR, datGR, bb.GR)
benefitIT <- calcBenefit(mIT, datIT, bb.IT)
