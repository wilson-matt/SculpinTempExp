# Set Working Directory
setwd("C:/GitHub/SculpinTempExp")

#Read in stream profile data
SculpinTrials_all <- read.csv("C:/GitHub/SculpinTempExp/data/Dailyobservations.csv", header = TRUE)

SculpinTrials <- subset(SculpinTrials_all, !SculpinTrials_all$Treatment == "ACC")

#calculate standard error
se <- function(x) sd(x)/sqrt(length(x))

#Dissolved oxygen conversion:
calc_do_percent <- function(temp_c, do_mgl) {
  # 1. Convert Celsius to Kelvin
  T_K <- temp_c + 273.15
  
  # 2. Benson & Krause (1984) standard formula for 100% DO saturation concentration (mg/L)
  ln_do_sat <- -139.34411 + 
    (1.575701e5 / T_K) - 
    (6.642308e7 / T_K^2) + 
    (1.243800e10 / T_K^3) - 
    (8.621949e11 / T_K^4)
  
  do_sat_100 <- exp(ln_do_sat)
  
  # 3. Calculate percent saturation (element-wise vector division)
  do_percent <- (do_mgl / do_sat_100) * 100
  
  return(do_percent)
}

SculpinTrials$DOmgL <- calc_do_percent(SculpinTrials$Temperature, SculpinTrials$DO)
mean(SculpinTrials$DOmgL)
se(SculpinTrials$DOmgL)

#Channel, trial, and treatment
WQ_summary_mean <- aggregate(.~Channel+Trial+Treatment, data=SculpinTrials[,c("Trial","Channel","Treatment","Temperature","pH","DOmgL","Discharge")], FUN = mean)
WQ_summary_se <- aggregate(.~Channel+Trial+Treatment, data=SculpinTrials[,c("Trial","Channel","Treatment","Temperature","pH","DOmgL","Discharge")], FUN = se)


#Trial and treatment
WQ_summary_mean <- aggregate(.~Trial+Treatment, data=SculpinTrials[,c("Trial","Treatment","Temperature","pH","DOmgL","Discharge")], FUN = mean)
WQ_summary_se <- aggregate(.~Trial+Treatment, data=SculpinTrials[,c("Trial","Treatment","Temperature","pH","DOmgL","Discharge")], FUN = se)


#Treatment only
WQ_summary_mean <- aggregate(.~Treatment, data=SculpinTrials[,c("Treatment","Temperature","pH","DOmgL","Discharge")], FUN = mean)
WQ_summary_se <- aggregate(.~Treatment, data=SculpinTrials[,c("Treatment","Temperature","pH","DOmgL","Discharge")], FUN = se)



#DO not different between groups:
mod <- lm(DOmgL~Treatment, data = SculpinTrials)
summary(mod)
anova(mod)

# pH different between groups, but not biologically so (8.03-8.36) - or 4.03% difference
mod <- lm(pH~Treatment +Channel + Trial, data = SculpinTrials)
summary(mod)
anova(mod)


#Discharge
mod <- lm(Discharge~Treatment+Channel+Trial, data = SculpinTrials)
summary(mod)
anova(mod)

