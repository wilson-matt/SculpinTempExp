#Read in stream profile data
SculpinTrials <- read.csv("C:/GitHub/SculpinTempExp/data/SculpinAll_lengthweight.csv", header = TRUE)

#Making the boxplots
SculpinTrialsOG <- as.data.frame(SculpinTrials)
MortDay <- subset(SculpinTrialsOG, SculpinTrialsOG$MortDays > 0 &SculpinTrialsOG$MortDays < 21)
# Calculate delta (difference between Pre-Weight and Post-Weight)
SculpinTrials <- SculpinTrialsOG[!grepl("X",SculpinTrialsOG$Mortality),]


SculpinTrials$Delta.Weight <- SculpinTrials$Pre.Weight - as.numeric(as.character(SculpinTrials$Post.Weight))

FCF <- function(Wt_g,Length_mm) (Wt_g/((Length_mm)^3) * 100000)

SculpinTrials$PreFCF <- FCF(SculpinTrials$Pre.Weight,SculpinTrials$Pre.Length)
SculpinTrials$PostFCF <- FCF(as.numeric(as.character(SculpinTrials$Post.Weight)),SculpinTrials$Pre.Length)
SculpinTrials$DeltaFCF <- SculpinTrials$PostFCF - SculpinTrials$PreFCF


#####With FCF instead of weight####
# Create a boxplot between temperatures

jpeg(filename = "bodycondition.jpg", width = 6, height = 6, units = "in", 
     pointsize = 12, quality = 500, res = 500)
ggplot(SculpinTrials, aes(x = as.factor(Temperature), y = DeltaFCF, group = as.factor(Temperature))) +
  # Solid white boxes with black outlines
  # outlier.shape = 16 (solid circle), outlier.size = 2
  geom_boxplot(fill = "grey", color = "black", width = 3, outlier.shape = NA) +
  geom_jitter(color = "black", width = 0.15, height = 0, shape = 16, size = 2, alpha = 0.4) +
  # Custom X category names
  scale_x_discrete(labels = c("10" = "10°C", "16" = "16°C", "22" = "22°C")) +
  
  # Labels
  labs(
       x = "Temperature",
       y = "Change in Fulton's Condition Factor") +
  
  # Strict black and white theme with no gridlines
  theme_classic() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(color = "black", size = 12)
  )
dev.off()


#Analysis
hist(SculpinTrials$DeltaFCF)#distribution is remarkably normal

#Include Trial -- best model for AIC, R-squared, and residuals
#Trial is NOT interactive with temp (good), can explain the ambient conditions overwinter with Trial importance
mod <- glm(DeltaFCF ~ Temperature+as.factor(Trial), data = SculpinTrials)
#mod <- gam(DeltaFCF ~ Temperature+s(Trial, bs = "re"), data = SculpinTrials)

summary(mod)
anova(mod)
plot(mod$residuals)
hist(mod$residuals)
AIC(mod)#-221.12
1 - mod$deviance/mod$null.deviance

aggregate(DeltaFCF~Trial, data = SculpinTrials, FUN = mean)


mod <- glm(DeltaFCF ~ Temperature+as.factor(Trial)+Pre.Length, data = SculpinTrials)


