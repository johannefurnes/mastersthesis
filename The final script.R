### Installing required packages
library(haven)
library(lmtest)
library(multiwayvcov)
library(MASS)
library(ggplot2)
#library(ggthemes)
library(ggpubr)
library(broom)
library(foreign)
library(stargazer)
library(boot)
library(VGAM)
library(DataCombine)
library(reshape2)
library(tidyr)
library(dplyr)
library(readr)
library(e1071)
library(xlsx)
library(rJava)
library(sjPlot)
library(jtools)
library(lme4)
#library(panelr)#might not need
library(rlang)
library(vars)
#library(panelView)
library(tidyverse)
library(car)
library(corrplot)
library(plm)
library(interplot)
library(faraway)
library(clubSandwich)

########################
##### PREPARATIONS #####
########################
##make characters numeric
dta$checks <- as.numeric(dta$checks)
dta$polcon_v <- as.numeric(dta$polcon_v)
dta$green_pres <- as.numeric(dta$green_pres)
dta$gov_pol <- as.numeric(dta$gov_pol)
dta$ccode <- as.numeric(dta$ccode)
dta$corruption <- as.numeric(dta$corruption)
dta$Year <- as.numeric(dta$Year)
dta$Stringency <- as.numeric(dta$Stringency)
dta$Year <- as.numeric(dta$Year)

##Creating natural log for GDP and trade openness
dta$GDPlog <- log(dta$GDP2010, base = exp(1))
dta$tradelog <- log(dta$tradeopen, base = exp(1))
dta$Stringency1 <- log(dta$Stringency, base = exp(1))##for robustness check


VARselect(dta$Stringency) #2 years lowest AIC
##creating panel data frame
df.pd <- pdata.frame(dta, index = c("ccode", "Year"))

##first lag two years
df.pd <-transform(df.pd, l_string=lag(Stringency,2))   
df.pd <-transform(df.pd, l_string=lag(Stringency,2))

##Then forward dependent variable two years, which means one year lag for IVs
df.pd <-transform(df.pd, l_string=lead(Stringency,2))   
df.pd <-transform(df.pd, l_string=lead(Stringency,2))   

head(df.pd)##done correctly

write.xlsx(df.pd, 
           file = "C:/Users/johan/OneDrive/Skrivebord/Master/data/Raw/All countries/panel.xlsx")

########################
##### REGRESSIONS ######
########################
##simple means models
naive <- lm(l_string ~
              polcon_v,data = df.pd)

naive2 <- lm(l_string ~
               checks +
               factor(Year) +
               factor(ccode), data = df.pd)

stargazer(naive, naive2, type = "text",
          omit = c("Year", "ccode"))

##POLCON V with correlated independent
FEpolc3 <- lm(l_string ~ 
           polcon_v+
           EU +
           tradelog+
           GDPlog +
           green_pres+
           gov_pol+
           corruption +
           polyarchy +
           as.factor(Year) +
           as.factor(Country), 
         data = df.pd)

##clustering standard errors
clusteredVCOV3 <- cluster.vcov(FEpolc3,
                              cluster = df.pd$ccode,
                              df_correction = F)#no correction of degreesoff


FEpolc3$clusteredVCOV3 <- clusteredVCOV3 #adding to model

##test with interaction term
stargazer(FEpolc3, type = "text",  
          se = list(sqrt(diag(FEpolc3$clusteredVCOV3))),
          omit = c("Year", "Country"))

##POLCON V without democracy
FEpolc2 <- lm(l_string ~ 
                polcon_v+
                EU +
                gov_pol+
                green_pres +
                GDPlog +
                corruption+
                as.factor(Year) +
                as.factor(Country), 
              data = df.pd)

#clustering standard errors
clusteredVCOV2 <- cluster.vcov(FEpolc2,
                              cluster = df.pd$ccode,
                              df_correction = F)#no correction of degreesoff

FEpolc2$clusteredVCOV2 <- clusteredVCOV2 #adding to model



##POLCON V with correlated independent
FEpolc6 <- lm(l_string ~ 
                polcon_v+
                EU +
                tradelog+
                gov_pol +
                GDPlog +
                green_pres+
                corruption +
                polyarchy +
                polcon_v:corruption+
                as.factor(Year) +
                as.factor(Country), 
              data = df.pd)


##clustering standard errors
clusteredVCOV6 <- cluster.vcov(FEpolc6,
                               cluster = df.pd$ccode,
                               df_correction = F)#no correction of degreesoff

FEpolc6$clusteredVCOV6 <- clusteredVCOV6 #adding to model

stargazer(FEpolc2, FEpolc3, FEpolc6, type = "text",  
          se = list( sqrt(diag(FEpolc2$clusteredVCOV2)),
                     sqrt(diag(FEpolc3$clusteredVCOV3)),
                     sqrt(diag(FEpolc6$clusteredVCOV6))),
          omit = c("Year", "Country", "ccode"))


#########################
### DEMOCRATIC SAMPLE ###
#########################
dem$checks <- as.numeric(dem$checks)
dem$polcon_v <- as.numeric(dem$polcon_v)
dem$green_pres <- as.numeric(dem$green_pres)
dem$gov_pol <- as.numeric(dem$gov_pol)
dem$ccode <- as.numeric(dem$ccode)
dem$corruption <- as.numeric(dem$corruption)
dem$Year <- as.numeric(dem$Year)
dem$Stringency <- as.numeric(dem$Stringency)

##Creating natural log for GDP and trade openness
dem$GDPlog <- log(dem$GDP2010, base = exp(1))
dem$tradelog <- log(dem$tradeopen, base = exp(1))
#dem$Stringency1 <- log(dem$Stringency, base = exp(1))

VARselect(dem$Stringency) #2 years lowest AIC
##create panel data
dem.pd <- pdata.frame(dem, index = c("ccode", "Year"))

##first lag two years
dem.pd <-transform(dem.pd, l_string=lag(Stringency,2))   
dem.pd <-transform(dem.pd, l_string=lag(Stringency,2))

##Then forward dependent variable two years, which means one year lag for IVs
dem.pd <-transform(dem.pd, l_string=lead(Stringency,2))   
dem.pd <-transform(dem.pd, l_string=lead(Stringency,2))   
head(df.pd)##done correctly

###
dem.pd <-transform(dem.pd, l_string=lag(Stringency1,3))   
dem.pd <-transform(dem.pd, l_string=lag(Stringency1,3))

##Then forward dependent variable two years, which means one year lag for IVs
dem.pd <-transform(dem.pd, l_string=lead(Stringency1,3))   
dem.pd <-transform(dem.pd, l_string=lead(Stringency1,3))   
head(df.pd)##done correctly

##POLCON V on democratic sample
FEpolc4 <- lm(l_string ~ 
                polcon_v+
                EU +
                gov_pol+
                tradelog+
                green_pres +
                GDPlog +
                corruption +
                as.factor(Year) +
                as.factor(ccode), 
              data = dem.pd)

#clustering standard errors
clusteredVCOV4 <- cluster.vcov(FEpolc4,
                               cluster = dem.pd$ccode,
                               df_correction = F)#no correction of degreesoff

FEpolc4$clusteredVCOV4 <- clusteredVCOV4 #adding to model

##POLCON V without democracy
FEpolc5 <- lm(l_string ~ 
                polcon_v+
                EU +
                gov_pol+
                green_pres +
                tradelog+
                GDPlog +
                corruption +
                polcon_v:corruption+
                as.factor(Year) +
                as.factor(ccode), 
              data = dem.pd)

#clustering standard errors
clusteredVCOV5 <- cluster.vcov(FEpolc5,
                               cluster = dem.pd$ccode,
                               df_correction = F)#no correction of degreesoff

FEpolc5$clusteredVCOV5 <- clusteredVCOV5 #adding to model


##table for latex
stargazer(FEpolc4, FEpolc5, type = "text",  
          se = list( sqrt(diag(FEpolc4$clusteredVCOV4)),
                     sqrt(diag(FEpolc5$clusteredVCOV5))),
          omit = c("Year", "ccode"))


##################
####CHECKS REG####
##################
FEchecks <- lm(l_string ~ 
                 checks +
                 GDPlog+
                 EU+
                 green_pres+
                 corruption+
                 polyarchy+
                 gov_pol+
                 as.factor(Year)+
                 as.factor(ccode), 
               data = df.pd)


##Clustering standard errors
clusteredVCOV <- cluster.vcov(FEchecks,
                               cluster = df.pd$ccode,
                               df_correction = F)#no correction of degreesoff

FEchecks$clusteredVCOV <- clusteredVCOV #adding to model

#checks with corruption
FEchecks2 <- lm(l_string ~ 
            checks +
            GDPlog+
            EU+
            green_pres+
            corruption+
            polyarchy+
            gov_pol+
            checks:corruption +
            factor(Year) +
            factor(ccode), 
          data = df.pd)

##Clustering standard errors
clusteredVCOV2 <- cluster.vcov(FEchecks2,
                               cluster = df.pd$ccode,
                               df_correction = F)#no correction of degreesoff

FEchecks2$clusteredVCOV2 <- clusteredVCOV2 #adding to model

##all countries
stargazer(FEchecks, FEchecks2,type = "text",
          se = list(
            sqrt(diag(FEchecks$clusteredVCOV)),
            sqrt(diag(FEchecks2$clusteredVCOV2))),
          omit = c("Year", "ccode"))

##Democratic sample
FEchecks3 <- lm(l_string ~ 
                  checks +
                  GDPlog+
                  tradelog+
                  EU+
                  green_pres+
                  corruption+
                  gov_pol+
                  as.factor(Year)+
                  as.factor(ccode), 
                data = dem.pd)


##Clustering standard errors
clusteredVCOV3 <- cluster.vcov(FEchecks3,
                               cluster = dem.pd$ccode,
                               df_correction = F)#no correction of degreesoff

FEchecks3$clusteredVCOV3 <- clusteredVCOV3 #adding to model

##with corruption interaction
FEchecks4 <- lm(l_string ~ 
                  checks +
                  GDPlog+
                  tradelog+
                  EU+
                  green_pres+
                  corruption+
                  gov_pol+
                  corruption*checks+
                  as.factor(Year)+
                  as.factor(ccode), 
                data = dem.pd)


##Clustering standard errors
clusteredVCOV4 <- cluster.vcov(FEchecks4,
                               cluster = dem.pd$ccode,
                               df_correction = F)#no correction of degreesoff

FEchecks4$clusteredVCOV4 <- clusteredVCOV4 #adding to model

##table for latex
stargazer(FEchecks, FEchecks2, FEchecks3, FEchecks4,type = "text",
          se = list(
            sqrt(diag(FEchecks$clusteredVCOV)),
            sqrt(diag(FEchecks2$clusteredVCOV2)),
            sqrt(diag(FEchecks3$clusteredVCOV3)),
            sqrt(diag(FEchecks4$clusteredVCOV4))),
          omit = c("Year", "ccode"))

#####INTERACTION WITHOUT CONTROLLING FOR DEMOCRACY##
FEpolc3 <- lm(l_string ~ 
                polcon_v+
                EU +
                tradelog+
                green_pres +
                gov_pol +
                GDPlog +
                corruption +
                corruption*polcon_v +
                as.factor(Year) +
                as.factor(Country), 
              data = df.pd)

##clustering standard errors
clusteredVCOV3 <- cluster.vcov(FEpolc3,
                               cluster = df.pd$ccode,
                               df_correction = F)#no correction of degreesoff

FEpolc3$clusteredVCOV3 <- clusteredVCOV3 #adding to model

##test with interaction term
stargazer(FEpolc3, type = "text",  
          se = list(sqrt(diag(FEpolc3$clusteredVCOV3))),
          omit = c("Year", "Country"))



########################
##### DESCRIPTIVE ######
########################
summary(df.pd)
sd(df.pd$Stringency)
sd(dta$polcon_v,  na.rm=TRUE)
sd(df.pd$checks,  na.rm=TRUE)
sd(df.pd$green_pres)
sd(df.pd$gov_pol, na.rm=TRUE)
sd(df.pd$GDP2010, na.rm=TRUE)
sd(df.pd$tradeopen, na.rm = TRUE)
sd(df.pd$EU)
sd(df.pd$corruption, na.rm=TRUE)
sd(df.pd$polyarchy, na.rm=TRUE)

summary(dem)
sd(dem$Stringency)
sd(dem$polcon_v,  na.rm=TRUE)
sd(dem$checks,  na.rm=TRUE)
sd(dem$green_pres)
sd(dem$gov_pol, na.rm=TRUE)
sd(dem$GDP2010, na.rm=TRUE)
sd(dem$tradeopen, na.rm = TRUE)
sd(dem$EU)
sd(dem$corruption, na.rm=TRUE)
sd(polyarchy, na.rm=TRUE)

par(mfrow = c(2,2))
hist(df.pd$l_string,na.rm = TRUE, main = "Histogram for stringency", 
     col = "#F2F3F4")
hist(df.pd$polcon_v, na.rm = TRUE, main = "Histogram for polcon v",
     col = "#F2F3F4")
hist(df.pd$checks, na.rm = TRUE, main = "Histogram for checks",
     col = "#F2F3F4")
hist(df.pd$corruption, na.rm = TRUE, main = "Histogram for corruption",
     col = "#F2F3F4")
dev.off()


par(mfrow = c(3,2))
hist(df.pd$polyarchy, na.rm = TRUE, main = "Histogram for polyarchy",
     col = "#F2F3F4")
hist(df.pd$GDPlog,na.rm = TRUE, main = "Histogram for GDP(log)",
     col = "#F2F3F4")
hist(df.pd$tradelog, na.rm = TRUE, main = "Histogram for trade open(log)",
     col = "#F2F3F4")
hist(df.pd$green_pres, na.rm = TRUE, main = "Histogram for green presence",
     col = "#F2F3F4")
hist(df.pd$gov_pol, na.rm = TRUE, main = "Histogram for government polarisation",
     col = "#F2F3F4")

########################
##### DIAGNOSTICS ######
########################
##AIC
AIC(FEchecks)
AIC(FEchecks2)
AIC(FEchecks3)
AIC(FEchecks4)
AIC(FEpolc2)
AIC(FEpolc3)
AIC(FEpolc4)
AIC(FEpolc5)
AIC(FEpolc6)
##BIC
BIC(FEchecks)
BIC(FEchecks2)
BIC(FEchecks3)
BIC(FEchecks4)
BIC(FEpolc2)
BIC(FEpolc3)
BIC(FEpolc6)
BIC(FEpolc4)
BIC(FEpolc5)

hist(residuals(FEpolc3), xlab = 'Residuals')

with(dta, plot(polcon_v, Stringency,
                     main = 'hi'))

##vif test for all models
car::vif(FEpolc2)
car::vif(FEpolc3)
car::vif(FEpolc6)
car::vif(FEpolc4)
car::vif(FEpolc5)
car::vif(FEchecks)
car::vif(FEchecks2)
car::vif(FEchecks3)
car::vif(FEchecks4)
##To clarify: I use the GVIF to determine multicollinearity. See:
##https://stats.stackexchange.com/questions/70679/which-variance-inflation-factor-should-i-be-using-textgvif-or-textgvif

df.pd[772,]

### DIAGNOSTICS ###
par(mfrow = c(2, 2))

plot(FEpolc6)#model 7

plot(FEpolc3)

plot(FEchecks3)#model 3

plot(FEpolc2)

dev.off()

df.pd[44,]
df.pd[745,]
df.pd[716,]

df.pd[688,]
df.pd[650,]
df.pd[46,]

#### VARIATION ####
dta_na <- dta[complete.cases(dta$polcon_v, dta$checks), ]
dta_na <- dta[complete.cases(dta$polcon_v1, dta$checks), ]
var(dta_na$polcon_v)
var(dta_na$checks)

########################
######## PLOTS #########
########################
##FIGURE X.X
##Average stringency across countries
##creating color palette for graphs
color<- c("#000033",	"#000066",	"#000099",	"#0000CC",	"#0000FF",
          "#003300",	"#003333",	"#003366",	"#003399",	"#0033CC",	"#0033FF",
          "#006600",	"#006633",	"#006666",	"#006699",	"#0066CC",	"#0066FF",
          "#009900",	"#009933",	"#009966",	"#009999",	"#0099CC",	"#0099FF",
          "#00CC00",	"#00CC33",	"#00CC66",	"#00CC99",	"#00CCCC",	"#00CCFF",
          "#00FF00",	"#00FF33",	"#00FF99",	"#00FFCC",	"#00FFFF",
          "#336633",	"#336666",	"#336699", "#00FF66")


##FIGURE XX and XX
##Correlation plot for all variables
col0 <- colorRampPalette(c("white", "#BADDFF", "#007FFF", "blue","#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
cormatrix <- subset(df.pd, 
                        select=c(checks, polcon_v, green_pres,
                                 gov_pol, GDPlog, corruption, 
                                 tradelog, EU, polyarchy))

corrplot <- cor(cormatrix, use="pairwise.complete.obs")
corrplot.mixed(corrplot, lower.col = "black", upper.col = col2(10))

## Correlation plot ##
corrplot(cormatrix)

dev.off()
###Stringency across countries
dta4 <- dta %>% group_by(Country) %>% 
  summarise(Stringency = mean(Stringency))

dta4 %>%
  mutate(Country = fct_reorder(Country, Stringency)) %>%#reordering bars
ggplot(aes(x = Country, y = Stringency))+ 
  geom_bar(stat = "identity", color = "#000066",fill = "#003366", 
           alpha = 0.5)+
  theme_bw()+
 # scale_fill_manual(values = color)+
  theme(legend.position="none")+
  labs(x = "",
       y = "",
       subtitle = "",
       title= "")+
  theme(axis.text.x = element_text(size = 17, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 17)) +
  ylim(0,3)


##FIGURE XX
# make categorical
dta$Country <- as.factor(dta$Country)

#fit model with interaction
pd = position_dodge(0.1)
plot_model(FEpolc6, type = "pred", terms = c("ccode"))+
  theme_bw()

color1 = c("#00a393","#000033",	"#009933","#000066",	"#000099",	"#00CC99",
           "#0000CC",	"#0000FF",
          "#003300",	"#003333",	"#003366",	"#003399",	"#0033CC",	"#0033FF",
          "#006600",	"#006633",	"#006666",	"#006699",	"#0066CC",	"#0066FF",
          "#009900",		"#009966",	"#009999",	"#0099CC",	"#0099FF",
          "#00CC00",	"#00CC33",	"#00CC66",		"#00CCCC",	"#00CCFF",
          "#00FF00",	"#00FF33",	"#00FF99",	"#00FFCC",	"#00FFFF",
          "#336633",	"#336666",	"#336699", "#00FF66")

############################
#### INTERACTION EFFECT ####
############################

##Plotting significant interaction effects###
summary(dta$corruption)#to get min and max for plotting
summary(dta$polyarchy)
summary(dta$polcon_v)

plot_model(FEpolc6, type = "int", 
           terms = c("polcon_v", "corruption[0.002, 0.96]"),
           line.size = 0.8)+
  labs(title="Polcon V on EPS conditional on low and 
          high levels of corruption",
       x ="POLCON V",
       y = "Stringency") +
  ggtitle("") +
  scale_colour_manual(values=color1,guide = guide_legend(title = "Corruption")) + 
  scale_fill_manual(values=color1, labels = c("Low", "High")) +
  theme_bw()


plot_model(FEpolc6, type = "pred", 
           terms = c("polcon_v", "corruption[0.96]"),
           line.size = 0.8,
           color = color1)+
  ggtitle("Conditional effect for veto players with low and high corruption") +
  scale_colour_manual(values=color1,guide = guide_legend(title = "Corruption")) +
  theme_bw()

coef <- vcovCR(FEpolc6,
        cluster = "ccode")

#######################
##### Forest plot #####
#######################
plot_model(FEpolc6, type = "est", 
           terms = c("polcon_v", "corruption", "EU"),
           line.size = 0.8,
           show.values = TRUE,
           robust = TRUE,
           SE = FEpolc6$clusteredVCOV6) +
  ggtitle("")+
  theme_bw()+
  geom_vline(xintercept = 0)+
  scale_colour_manual(values=color1) 

##with democracy
plot_model(FEpolc3, type = "est", 
           terms = c("polcon_v", "corruption", "EU"),
           line.size = 0.8,
           show.values = TRUE,
           robust = TRUE,
           se = FEpolc3$clusteredVCOV3) +
  ggtitle("")+
  theme_bw()+
  scale_colour_manual(values=color1) 


##without democracy
plot_model(FEpolc2, type = "est", 
           terms = c("polcon_v", "corruption", "EU"),
           line.size = 0.8,
           show.values = TRUE,
           robust = TRUE,
           SE = FEpolc2$clusteredVCOV2) +
  ggtitle("")+
  theme_bw()+
  geom_vline(xintercept = 0)+
  scale_colour_manual(values=color1) 


##Direct effect
iris$cluster <- factor(rep(LETTERS[1:8], length.out = nrow(FEpolc2)))

plot_model(FEpolc2, type = "pred", 
           terms = c("polcon_v"),
           line.size = 0.8,
           p.threshold = 0.05,
           pred.type = "fe",
           colors = "#00ff00",
           robust = TRUE,
           vcov.fun = "vcovCL",
           vcov_type = "HC0",
           vcov_args = list(cluster = FEpolc2$cluster)) +
  theme_bw()+
  ggtitle("") 

coeftest(FEpolc6,
         vcov. = FEpolc6$clusterVCOV)

FEpolc3$cluster<- vcovCL(FEpolc3,
         vcov. = FEpolc3$clusterVCOV)

FEpolc3$cluster<- vcovCL(FEpolc3,
                         vcov. = FEpolc3$clusterVCOV)

##################################
##### STRINGENCY ACROSS TIME #####
##################################
##laggards
dta4 <- subset(dta, 
               (Country %in% c("Portugal", "Poland",
                               "Hungary", "Czech Rep.", "Slovakia", 
                               "Greece", "Turkey", "United States", 
                               "Australia", "Belgium", "Ireland")) 
               & Year & Stringency)

dta4 <- dta4 %>%
  group_by(Year) %>%
  summarise (Stringency = mean(Stringency, na.rm = TRUE))%>%
  filter(Year %in% c(1990:2012))

##leaders
dta3 <- subset(dta, 
               (Country %in% c( "Canada", "United Kingdom", 
                                "Netherlands", "France", "Switzerland",
                                 "Germany","Austria", "Finland", "Sweden",
                                "Norway", "Denmark",
                                "Korea", "Japan", "Italy", "Spain")) 
               & Year & Stringency)

dta3 <- dta3 %>%
  group_by(Year) %>%
  summarise (Stringency = mean(Stringency, na.rm = TRUE))%>%
  filter(Year %in% c(1990:2012))

##BRIICS
dta2 <- subset(dta, 
               (Country %in% c("China", "Brazil", "Indonesia","Russia",
                               "South Africa")) 
               & Year & Stringency)

dta2 <- dta2 %>%
  group_by(Year) %>%
  summarise (Stringency = mean(Stringency, na.rm = TRUE))%>%
  filter(Year %in% c(1990:2012))


##All countries in data set
dta5 <- dta %>%
  group_by(Year) %>%
  summarise (Stringency = mean(Stringency, na.rm = TRUE))%>%
  filter(Year %in% c(1990:2012))

##Plotting
ggplot() + 
  geom_line(data = dta4, aes(x = Year, y = Stringency), size = 1.5,
            color =  "#00CC99", alpha = 0.5) +
  geom_line(data = dta3, aes(x = Year, y = Stringency), 
            color = "#009966", size = 1.5, alpha = 0.5) +
  geom_line(data = dta5, aes(x = Year, y = Stringency), 
           color= "#000033", size = 1.5, linetype="dashed") +
  geom_line(data = dta2, aes(x = Year, y = Stringency), 
            color= "#000066", size = 1.5, alpha = 0.5) +
  labs(x='Year', 
  y='Stringency') +
  scale_linetype_manual(values = c("dashed","solid"), guide = "none")+
  scale_color_manual(values = color)+
  theme_bw()+
  theme(axis.title.y = element_text(size = 14, face = "plain", angle = 0, hjust = .5, vjust = .5 ),
        axis.title.x = element_text(size = 14, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.x = element_text(size = 12))



##### COEFFICIENT PLOT #####
jtools::plot_summs(FEpolc3, scale = TRUE,
                   omit.coefs = c("GDPlog", "tradelog", "EU", "gov_pol", 
                                  "polyarchy","corruption", "green_pres",
                                  "(Intercept)"),
                   colors = c("#000099", "#00CC99"),
                   ci_level = 0.95)+
                   theme_bw()


##########################
####### APPENDIX C #######
##########################

##POLCON V with correlated independent
FEpolc7 <- lm(l_string ~ 
                polcon_v+
                EU +
                tradelog+
                green_pres +
                gov_pol +
                GDPlog +
                corruption2 +
                polyarchy +
                as.factor(Year) +
                as.factor(Country), 
              data = df.pd)

##clustering standard errors
clusteredVCOV7 <- cluster.vcov(FEpolc7,
                               cluster = df.pd$ccode,
                               df_correction = F)#no correction of degreesoff

FEpolc7$clusteredVCOV7 <- clusteredVCOV7 #adding to model

##POLCON V without democracy
FEpolc8 <- lm(l_string ~ 
                polcon_v+
                EU +
                tradelog+
                gov_pol+
                green_pres +
                GDPlog +
                corruption2+
                as.factor(Year) +
                as.factor(Country), 
              data = df.pd)

#clustering standard errors
clusteredVCOV8 <- cluster.vcov(FEpolc8,
                               cluster = df.pd$ccode,
                               df_correction = F)#no correction of degreesoff

FEpolc8$clusteredVCOV8 <- clusteredVCOV8 #adding to model



##POLCON V with correlated independent
FEpolc9 <- lm(l_string ~ 
                polcon_v+
                EU +
                tradelog+
                green_pres +
                gov_pol +
                GDPlog +
                corruption2 +
                polyarchy +
                polcon_v:corruption2 +
                as.factor(Year) +
                as.factor(Country), 
              data = df.pd)


##clustering standard errors
clusteredVCOV9 <- cluster.vcov(FEpolc9,
                               cluster = df.pd$ccode,
                               df_correction = F)#no correction of degreesoff

FEpolc9$clusteredVCOV9 <- clusteredVCOV9 #adding to model

stargazer(FEpolc8, FEpolc7, FEpolc9, type = "latex",  
          se = list(sqrt(diag(FEpolc7$clusteredVCOV7)),
                     sqrt(diag(FEpolc8$clusteredVCOV8)),
                     sqrt(diag(FEpolc9$clusteredVCOV9))),
          omit = c("Year", "Country", "ccode"))
