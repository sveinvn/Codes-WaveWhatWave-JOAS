#Exponential simulated model testing for demographic impact from the Garth tsunami in Western Norway
#Load the workpackages
library(rcarbon)
library(readr)

#Load the dataset (STAGED23052020) from a local source
STAGED <- read_csv2("~/Github/STAGED23052020.csv", col_names = TRUE)

#Define a regional dataframe
western <- STAGED[-c(1:52, 84:93, 105:110, 131:214, 459:473, 781:975, 1360:1558), ]
#Make subset with suitable BP span
subset <- subset(western, BP <= 7000)

#Calibration
caldates <- calibrate(x=subset$BP, errors=subset$SD, calCurves='intcal13', normalised=FALSE, calMatric=TRUE)

#Bins and SPD
bins <- binPrep(subset$Site_ID, subset$BP, h=100)
spd <-spd(x = caldates, timeRange = c(8500,2500), runm = 100)
plot(spd, calendar = "BCAD", xlim = c(-6300,-800))
plot(spd, calendar = "BCAD", xlim = c(-4600,-2400))

#modelTest
exponential <- modelTest(caldates, errors = subset$SD, bins = bins, method = "uncalsample", nsim = 1000, runm = 100, timeRange = c(8200,2800), model = "exponential", raw = TRUE)

#Plot and summary
plot(exponential, calendar = "BCAD", xlim = c(-6300,-800))
plot(exponential, calendar = "BCAD", xlim = c(-4500,-2500))
legend("topleft", legend = c("SPD, Western Norway (n bins=489, running mean=150)", "Simulated envelope (n sim=1000)", "Global p-value=0.002"), col = c("black", "grey", "white"), lwd = 2)
summary(exponential)
