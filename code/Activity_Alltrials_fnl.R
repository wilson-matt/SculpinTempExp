library(ggplot2)
library(MASS)
library(mgcv)
fishies <- read.csv("C:/GitHub/SculpinTempExp/data/Behavior_Alltrials.csv", header=TRUE)
#setwd("c:/GitHub/ExpSculpin/plots")

fishies$Height <- rep(c(24, 23.5, 17.5, 23,    17,    19, 12.5, 16.5), 3)  
fishies$Flow <- rep(c(26,   25.5, 24.5,   24,  20.5,    20, 18.5,   17), 3)
fishies$ChannelDistance1<- rep(c(4,6,8,10,12,14,16,18),3)

#reorder temp
fishies$Temp <- factor(fishies$Temp, levels = c("Hot", "Warm", "Cold"))


####Daily activity####

allfishies <- read.csv("C:/GitHub/SculpinTempExp/data/DailyObservations.csv", header=TRUE)

#remove acclimation dates
allfishies <- subset(allfishies, allfishies$Treatment != "ACC")

allfishies$Treatment <- factor(allfishies$Treatment, levels = c("Hot", "Warm", "Cold"))


#Need to create a channel distance column and put data in a tidy format:
AB <- allfishies[,c("Trial", "Date", "Time", "Channel","Treatment", "Temperature", "pH",
                    "DO", "Discharge", "Possiblefish", "AB")]
AB$ChannelDistance <- rep(4, nrow(AB))
names(AB)[names(AB) == 'AB'] <- 'Count'
C <- allfishies[,c("Trial", "Date", "Time", "Channel","Treatment", "Temperature", "pH",
                    "DO", "Discharge", "Possiblefish", "C")]
C$ChannelDistance <- rep(6, nrow(C))
names(C)[names(C) == 'C'] <- 'Count'
D <- allfishies[,c("Trial", "Date", "Time", "Channel","Treatment", "Temperature", "pH",
                    "DO", "Discharge", "Possiblefish", "D")]
D$ChannelDistance <- rep(8, nrow(D))
names(D)[names(D) == 'D'] <- 'Count'
E <- allfishies[,c("Trial", "Date", "Time", "Channel","Treatment", "Temperature", "pH",
                    "DO", "Discharge", "Possiblefish", "E")]
E$ChannelDistance <- rep(10, nrow(E))
names(E)[names(E) == 'E'] <- 'Count'
FF <- allfishies[,c("Trial", "Date", "Time", "Channel","Treatment", "Temperature", "pH",
                    "DO", "Discharge", "Possiblefish", "F")]
FF$ChannelDistance <- rep(12, nrow(FF))
names(FF)[names(FF) == 'F'] <- 'Count'
G <- allfishies[,c("Trial", "Date", "Time", "Channel","Treatment", "Temperature", "pH",
                    "DO", "Discharge", "Possiblefish", "G")]
G$ChannelDistance <- rep(14, nrow(G))
names(G)[names(G) == 'G'] <- 'Count'
H <- allfishies[,c("Trial", "Date", "Time", "Channel","Treatment", "Temperature", "pH",
                    "DO", "Discharge", "Possiblefish", "H")]
H$ChannelDistance <- rep(16, nrow(H))
names(H)[names(H) == 'H'] <- 'Count'
IJ <- allfishies[,c("Trial", "Date", "Time", "Channel","Treatment", "Temperature", "pH",
                    "DO", "Discharge", "Possiblefish", "IJ")]
IJ$ChannelDistance <- rep(18, nrow(IJ))
names(IJ)[names(IJ) == 'IJ'] <- 'Count'

tidyfishies <- rbind(AB,C,D,E,FF,G,H,IJ)
tidyfishies$Ratio <- as.numeric(as.character(tidyfishies$Count))/tidyfishies$Possiblefish
tidyfishies$Ratio[is.na(tidyfishies$Ratio)] <- 0

sumfishies <- aggregate(tidyfishies$Ratio~tidyfishies$ChannelDistance+tidyfishies$Treatment, FUN = mean)
se <- function(x) sd(x)/sqrt(length(x))

sumfishies$se <- aggregate(tidyfishies$Ratio~tidyfishies$ChannelDistance+tidyfishies$Treatment, FUN = se)[,3]
colnames(sumfishies) <- c("ChannelDistance", "Treatment", "Mean", "SE")
sumfishies$Height <- rep(c(24, 23.5, 17.5, 23,    17,    19, 12.5, 16.5), 3)  
sumfishies$Flow <- rep(c(26,   25.5, 24.5,   24,  20.5,    20, 18.5,   17), 3)
sumfishies$SE <- as.numeric(as.character(sumfishies$SE))
sumfishies$ChannelDistancem <- round(sumfishies$ChannelDistance*0.3048, 1)

#Actual plot
jpeg(filename = "movementfig.jpg", width = 12, height = 9, units = "in", 
      pointsize = 12, quality = 500, res = 500)
  ggplot(sumfishies, aes(y=Mean, x=factor(ChannelDistancem), fill=Treatment)) +
  geom_bar(stat="identity", position=position_dodge(0.9))+
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.4, position=position_dodge(0.9))+
  geom_line(aes(x = (ChannelDistance/2)*1.1-1.54, y = Flow/70/4), color = "#97D5C9", alpha = 0.3) +
  geom_line(aes(x = (ChannelDistance/2)*1.1-1.54, y = Height/70/4), color = "black", linewidth = 1.5, alpha = 0.3) +
  geom_ribbon(aes(x = (ChannelDistance/2)*1.1-1.54, ymin = Flow/70/4, ymax = Height/70/4), fill = "#97D5C9", alpha = 0.3) +
  scale_fill_manual(values = c("Hot" = "#9E292E", "Warm" = "#FF6A14", "Cold" = "#869954"), 
                    name = "Temperature",
                    labels = c("Hot" = "22°C", "Warm" = "16°C", "Cold" = "10°C")) +
  geom_text(aes(x = 5.25, y = .055, label = "Riffle"), color = "grey35", vjust = -3.5, size = 5, alpha = 0.5) +
  geom_text(aes(x = 2.7, y = .054, label = "Pool"), color = "grey35", vjust = -5.5, size = 5, alpha = 0.5) +
  geom_text(aes(x = 7, y = .052, label = "Pool"), color = "grey35", vjust = -1.5, size = 5, alpha = 0.5) +
  theme_bw() +
  labs(x = "Channel distance (m)", y = "Proportion days visible") +
       theme(
         axis.title = element_text(size = 18),
         axis.text = element_text(size = 14)
       )+
  theme(legend.background = element_rect(fill="white", size=0))+
  theme(legend.direction = "vertical")+
  theme(legend.text = element_text(size = 14))+
  theme(legend.title=element_text(size = 14))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position=c(0.9, 0.85))
dev.off()



####Movement analysis####
library(mgcv)
#family = cnorm() is designed for censored data in GAM.
#other family options are tobit1 and tobit2 from cenGAM package. Realistically the only three options because of the censored data.
#used tobit1 because it allows for left and right censor values to be specified. 
#create unique ID for repeated measures (i.e. each channel in each trial)
tidyfishies$ID <- paste(tidyfishies$Trial, tidyfishies$Channel, sep = "_")
library(cenGAM)
mod <- gam(Ratio~ChannelDistance*Temperature + Channel +s(Temperature, as.factor(ID), bs = "fs")+ s(Trial, bs = "re"), data = tidyfishies, 
           method = "REML", na.action=na.omit, family = tobit1(link="identity", left.threshold=0, right.threshold=Inf,
                                                               theta=NULL, initial.theta=0))
summary(mod)
AIC(mod)
anova(mod)
library(itsadug)
mod <- gam(Ratio~s(ChannelDistance, k=6)+s(Trial, bs = "re"), data = tidyfishies)

plot_smooth(mod, view = "ChannelDistance", rm.ranef = TRUE)

####Total activity by temperature, not directional movement####
#Ordered Kruskal-Wallis from hot to cold seems most appropriate given the very different distributions by trial*temp.
mean_fishes <- aggregate(TotActive~Trial+Channel+Treatment, data = allfishies, FUN = "mean")

kw <- kruskal.test(TotActive ~ Treatment, data = mean_fishes)
kw
plot(TotActive ~ Treatment, data = )

allfishies$block_id <- interaction(allfishies$Trial, allfishies$Channel, drop = TRUE)

library(ARTool)
library(emmeans)
citation("ARTool")
# 1. Make sure factors are properly formatted (replace 'Time' with your actual time column name)
allfishies$TotActive <- as.numeric(allfishies$TotActive)
allfishies$Treatment <- factor(allfishies$Treatment)
allfishies$Date      <- factor(allfishies$Date)
allfishies$block_id  <- factor(allfishies$block_id)

# 2. Fit the nonparametric mixed ANOVA
model <- art(TotActive ~ Treatment * Date + (1 | block_id), data = allfishies)

# 3. View ANOVA table (tests Treatment, Time, and Treatment:Time interaction)
anova(model)

# 4. Post-hoc comparisons (example: compare Treatments at each Time point)
art.con(model, "Treatment:Date", adjust = "holm")


#So cold fishies move more than warm fishies, and still maintain higher condition factor.
#This also makes some sense if we look at mean and se:
means <- aggregate(allfishies$TotActive~allfishies$Treatment, FUN = mean)
se <- aggregate(allfishies$TotActive~allfishies$Treatment, FUN = se)
summary_data <- cbind(means, se$`allfishies$TotActive`)
colnames(summary_data) <- c("Treatment", "mean", "se")


jpeg(filename = "activityfig.jpg", width = 5, height = 6, units = "in", 
     pointsize = 12, quality = 500, res = 500)

ggplot(allfishies, aes(x = Treatment, y = TotActive, fill = Treatment)) +
  # Draw the bars
  stat_summary(fun = mean, geom = "bar", fill = "grey", color = "black", width = 0.75) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, lwd = 0.5) +
  
  geom_jitter(color = "black", width = 0.15, height = 0, shape = 16, size = 1, alpha = 0.3) +
  scale_x_discrete(limits = c("Cold", "Warm", "Hot"),
                   labels = c("Hot" = "22°C", "Warm" = "16°C", "Cold" = "10°C")) +
  scale_y_continuous(limits = c(0, 1)) +
  # Aesthetics
  theme_bw() +
  labs( x = "Temperature",
        y = "Proportion active per day") +
  theme_minimal() +
  theme_classic()+
  theme(legend.position = "none")# Hide redundant legend
dev.off()

