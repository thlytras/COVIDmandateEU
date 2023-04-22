if (!exists("mGR") || !exists("mIT")) source("analyze.R")

prdatGR <- subset(datGR, mandate>1)
prdatGR$mandate <- 1
prdatGR[,grep("^sl", names(prdatGR))] <- 0

prdatIT <- subset(datIT, mandate>1)
prdatIT$mandate <- 1
prdatIT[,grep("^sl", names(prdatIT))] <- 0

cat("Bootstrapping for Greece: ")
elapsed.time.GR <- system.time({ 
  bb.GR <- bootMer(mGR, function(m) {
    cat(".")
    predict(m,prdatGR)
  }, nsim=5000, ncpus=4, parallel="multicore", seed=42, re.form=NULL)
})

cat("\n\nBootstrapping for Italy: ")
elapsed.time.IT <- system.time({ 
  bb.IT <- bootMer(mIT, function(m) {
    cat(".")
    predict(m,prdatIT)
  }, nsim=5000, ncpus=4, parallel="multicore", seed=42, re.form=NULL)
})

save(prdatGR, prdatIT, bb.GR, bb.IT, elapsed.time.GR, elapsed.time.IT, file="bootstrap.RData")

