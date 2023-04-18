fig1f <- list()
fig1f$axisWk <- function(x, d=datGR) {
  axis(1, at=iwk(c(202135, 202150, 202200+(1:5)*10), d), labels=c("35/2021", "50/2021", paste0((1:5)*10,"/2022")))
}
fig1f$ylim <- c(2,10000)/100000
fig1f$axisRate <- function() {
  axis(2, at=c(2,5,10)*rep(10^(-5:-2),each=3), labels=c(2,5,10)*rep(10^(0:3),each=3))
}
fig1f$cols <- c("red","blue","green")
fig1f$ylab <- "New first-dose vaccinations\nper 100,000 unvaccinated (log scale)"


cairo_pdf("Figure1.pdf", width=6, height=9)
par(family="Fira Sans", mfcol=c(3,1), mar=c(5,5.5,4,2))

with(subset(datGR, label=="50-59"), plot(firstDoses/unvacc, type="n",
  log="y", xaxt="n", yaxt="n", bty="l", ylim=fig1f$ylim, 
  xlab="Week number", ylab=fig1f$ylab,
  main="Greece"))
fig1f$axisWk()
fig1f$axisRate()
abline(v=iwk(c(202148, 202203, 202216), datGR), col=fig1f$cols[3], lwd=2)
with(subset(datGR, label=="50-59"), points(firstDoses/unvacc, type="l", col=fig1f$cols[2], lwd=2))
with(subset(datGR, label=="60+"), points(firstDoses/unvacc, type="l", col=fig1f$cols[1], lwd=2))
legend("topright", c("Age 60+", "Age 50-59 (control)"), col=fig1f$cols[1:2], lwd=2, seg.len=3, bty="n")

with(subset(datIT, label=="25-49"), plot(firstDoses/unvacc, type="n",
  log="y", xaxt="n", yaxt="n", bty="l", ylim=fig1f$ylim, 
  xlab="Week number", ylab=fig1f$ylab,
  main="Italy"))
fig1f$axisWk()
fig1f$axisRate()
abline(v=iwk(c(202202, 202207, 202224, 202244), datIT), col=fig1f$cols[3], lwd=2)
with(subset(datIT, label=="25-49"), points(firstDoses/unvacc, type="l", col=fig1f$cols[2], lwd=2))
with(subset(datIT, label=="50+"), points(firstDoses/unvacc, type="l", col=fig1f$cols[1], lwd=2))
legend("topright", c("Age 50+", "Age 25-49 (control)"), col=fig1f$cols[1:2], lwd=2, seg.len=3, bg="white", box.col="white")

with(datAT, plot(firstDoses/unvacc, type="n",
  log="y", xaxt="n", yaxt="n", bty="l", ylim=fig1f$ylim, 
  xlab="Week number", ylab=fig1f$ylab,
  main="Austria"))
fig1f$axisWk()
fig1f$axisRate()
abline(v=iwk(c(202147, 202211), datAT), col=fig1f$cols[3], lwd=2)
with(datAT, points(firstDoses/unvacc, type="l", col=fig1f$cols[1], lwd=2))
legend("topright", c("All ages"), col=fig1f$cols[1], lwd=2, seg.len=3, bty="n")

dev.off()
