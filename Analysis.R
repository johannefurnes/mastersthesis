### Installing required packages
library(haven)
library(lmtest)
library(multiwayvcov)
library(MASS)
library(ggplot2)
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
library(panelr)
library(rlang)
library(vars)
library(panelView)
library(tidyverse)
library(car)
library(corrplot)
library(plm)
library(interplot)
library(faraway)
library(clubSandwich)    
library(gplots)    
library(tseries)

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
                GDPlog +
                green_pres+
                gov_pol+
                tradelog+
                corruption +
                polyarchy +
                as.factor(Year) +
                as.factor(ccode), 
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
                tradelog+
                as.factor(Year) +
                as.factor(ccode), 
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
                gov_pol +
                GDPlog +
                green_pres+
                corruption +
                polyarchy +
                tradelog+
                polcon_v:corruption+
                as.factor(Year) +
                as.factor(ccode), 
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


####################
#### CHECKS REG ####
####################
FEchecks <- lm(l_string ~ 
                 checks +
                 GDPlog+
                 tradelog+
                 EU+
                 green_pres+
                 corruption+
                 polyarchy+
                 gov_pol+
                 as.factor(Year)+
                 as.factor(Country), 
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
                  tradelog+
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

## VIF test for all models ##
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


### DIAGNOSTIC plots ###
par(mfrow = c(2, 2))

plot(FEpolc6)#model 7

plot(FEpolc3)

plot(FEchecks3)#model 3

plot(FEpolc2)

dev.off()

##checking numbered residuals
df.pd[44,]
df.pd[745,]
df.pd[716,]
df.pd[772,]

dem.pd[688,]
dem.pd[650,]
dem.pd[46,]

##testing for heteroscedasticity
bptest(FEpolc6)
bptest(FEchecks3)

## checking variation 
dta_na <- dta[complete.cases(dta$polcon_v, dta$checks), ]
dta_na <- dta[complete.cases(dta$polcon_v1, dta$checks), ]
var(dta_na$polcon_v)
var(dta_na$checks)

########################
######## PLOTS #########
########################

##creating color palette for graphs
color1 = c("#00a393","#000033",	"#009933","#000066",	"#000099",	"#00CC99",
           "#0000CC",	"#0000FF", "#003300",	"#003333",	"#003366",	"#003399",	
           "#0033CC",	"#0033FF", "#006600",	"#006633",	"#006666",	"#006699",	
           "#0066CC",	"#0066FF", "#009900",		"#009966",	"#009999",	"#0099CC",	
           "#0099FF", "#00CC00",	"#00CC33",	"#00CC66",		"#00CCCC",	
           "#00CCFF", "#00FF00",	"#00FF33",	"#00FF99",	"#00FFCC",	"#00FFFF",
           "#336633",	"#336666",	"#336699", "#00FF66")


##### MEAN STRINGENCY ACROSS COUNTRIES #####
dta4 <- dta %>% group_by(Country) %>% 
  summarise(Stringency = mean(Stringency))

dta4 %>%
  mutate(Country = fct_reorder(Country, Stringency)) %>%#reordering bars
  ggplot(aes(x = Country, y = Stringency))+ 
  geom_bar(stat = "identity", color = "#000066",fill = "#003366", 
           alpha = 0.5)+
  theme_bw()+
  theme(legend.position="none")+
  labs(x = "",
       y = "",
       subtitle = "",
       title= "")+
  theme(axis.text.x = element_text(size = 17, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 17)) +
  ylim(0,3)


##### CORRELATION PLOT FOR INDEPENDENT VARIABLES #####
col0 <- colorRampPalette(c("white", "#BADDFF", "#007FFF", "blue","#00007F"))

cormatrix <- subset(df.pd, 
                    select=c(checks, polcon_v, green_pres,
                             gov_pol, GDPlog, corruption, 
                             tradelog, EU, polyarchy))

corrplot <- cor(cormatrix, use="pairwise.complete.obs")
corrplot.mixed(corrplot, lower.col = "black", upper.col = col2(10))

##### INTERACTION EFFECT #####
##Plotting significant interaction effect###
summary(dta$corruption)#to get min and max for plotting
summary(dta$polyarchy)
summary(dta$polcon_v)

plot_model(FEpolc6, type = "int", 
           terms = c("polcon_v", "corruption[0.002, 0.96]"),
           line.size = 0.8,
           vcoc.fun = "vcovCR",
           vcov.type = "HC3",
           vcov.args = "ccode")+
  labs(title="Polcon V on EPS conditional on low and 
          high levels of corruption",
       x ="POLCON V",
       y = "Stringency") +
  ggtitle("") +
  scale_colour_manual(values=color1,guide = guide_legend(title = "Corruption")) + 
  scale_fill_manual(values=color1, labels = c("Low", "High")) +
  theme_bw()


##### STRINGENCY ACROSS TIME #####
##laggards
dta6 <- subset(dta, 
               (Country %in% c("Portugal", "Poland",
                               "Hungary", "Czech Rep.", "Slovakia", 
                               "Greece", "Turkey", "United States", 
                               "Australia", "Belgium", "Ireland")) 
               & Year & Stringency)

dta6 <- dta6 %>%
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
  geom_line(data = dta6, aes(x = Year, y = Stringency), size = 1.5,
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
  theme(axis.title.y = element_text(size = 14, face = "plain", angle = 0, 
                                    hjust = .5, vjust = .5 ),
        axis.title.x = element_text(size = 14, angle = 0, hjust = .5, 
                                    vjust = .5, face = "plain"),
        axis.text.x = element_text(size = 12))


#### DATA OVERVIEW ####
ggplot(dta, aes(x=polcon_v, y=Stringency, size = corruption,
                color= corruption)) +
  geom_point(alpha=0.7)+
  theme_bw()+
  theme(legend.position="bottom") +
  ylab("Stringency") +
  xlab("POLCON V")

ggplot(dta, aes(x=checks, y=Stringency, size = corruption,
                color= corruption)) +
  geom_point(alpha=0.7)+
  theme_bw()+
  theme(legend.position="bottom") +
  ylab("Stringency") +
  xlab("CHECKS")

###########################################################
###### MAIN VARIABLES ACROSS TIME FOR ALL COUNTRIES #######
###########################################################
##figure 6.1 and 6.2 and B.1.

Italy <- dta[dta$Country == "Italy",]
Spain <- dta[dta$Country == "Spain",]
Greece <- dta[dta$Country == "Greece",]
Portugal <- dta[dta$Country == "Portugal",]
UK <- dta[dta$Country == "United Kingdom",]
US <- dta[dta$Country == "United States",]
Switzerland <- dta[dta$Country == "Switzerland",]
Sweden <- dta[dta$Country == "Sweden",]
China <- dta[dta$Country == "China",]
Australia <- dta[dta$Country == "Australia",]
Austria <- dta[dta$Country == "Austria",]
Belgium <- dta[dta$Country == "Belgium",]
Canada <- dta[dta$Country == "Canada",]
Czech <- dta[dta$Country == "Czech Republic",]
Denmark <- dta[dta$Country == "Denmark",]
Finland <- dta[dta$Country == "Finland",]
France <- dta[dta$Country == "France",]
Germany<- dta[dta$Country == "Germany",]
Hungary <- dta[dta$Country == "Hungary",]
Ireland <- dta[dta$Country == "Ireland",]
Japan <- dta[dta$Country == "Japan",]
SK <- dta[dta$Country == "South Korea",]
Netherlands <- dta[dta$Country == "Netherlands",]
Norway <- dta[dta$Country == "Norway",]
Poland <- dta[dta$Country == "Poland",]
Russia <- dta[dta$Country == "Russia",]
SA <- dta[dta$Country == "South Africa",]
Turkey <- dta[dta$Country == "Turkey",]
Slovakia <- dta[dta$Country == "Slovakia",]
Brazil <- dta[dta$Country == "Brazil",]
India <- dta[dta$Country == "India",]
Indonesia <- dta[dta$Country == "Indonesia",]

#######################
###### POLCON V #######
#######################

China1 <- ggplot(China) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="China")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Italy1 <- ggplot(Italy) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Italy")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Spain1 <- ggplot(Spain) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Spain")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Greece1 <- ggplot(Greece) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Greece")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Portugal1 <- ggplot(Portugal) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Portugal")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

UK1 <- ggplot(UK) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="United Kingdom")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

US1 <- ggplot(US) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="United States")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Switzerland1 <- ggplot(Switzerland) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Switzerland")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Sweden1 <- ggplot(Sweden) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Sweden")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Australia1 <- ggplot(Australia) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Australia")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))


Austria1 <- ggplot(Austria) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Austria")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Belgium1 <- ggplot(Belgium) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Belgium")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Canada1 <- ggplot(Canada) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Canada")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Czech1 <- ggplot(Czech) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Czech Republic")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Denmark1 <- ggplot(Denmark) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Denmark")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Finland1 <- ggplot(Finland) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Finland")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

France1 <- ggplot(France) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="France")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Germany1 <- ggplot(Germany) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Germany")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Hungary1 <- ggplot(Hungary) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Hungary")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Ireland1 <- ggplot(Ireland) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Ireland")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Japan1 <- ggplot(Japan) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Japan")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

SK1 <- ggplot(SK) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="South Korea")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Netherlands1 <- ggplot(Netherlands) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Netherlands")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Norway1 <- ggplot(Norway) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Norway")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Poland1 <- ggplot(Poland) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Poland")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Russia1 <- ggplot(Russia) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Russia")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

SA1 <- ggplot(SA) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="South Africa")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Turkey1 <- ggplot(Turkey) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Turkey")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Slovakia1 <- ggplot(Slovakia) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Slovakia")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Brazil1 <- ggplot(Brazil) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Brazil")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

India1 <- ggplot(India) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="India")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))

Indonesia1 <- ggplot(Indonesia) +
  geom_line(aes(x=Year, y= polcon_v)) +
  theme_bw()+
  labs(title="Indonesia")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 1))
Indonesia1


ggarrange(Australia1, Austria1, Belgium1, Canada1, Czech1, Denmark1, Finland1, 
          France1, Germany1, Greece1, Hungary1, Ireland1, Italy1, Japan1, SK1,
          Netherlands1, Norway1, Poland1, Portugal1, Russia1, SA1, 
          Spain1, Sweden1, Switzerland1, Turkey1, US1, UK1, Slovakia1, Brazil1, 
          China1, India1, Indonesia1,
          legend = c("right", "none"),
          common.legend = TRUE,
          heights = 2)


#####################
###### CHECKS #######
#####################

China2 <- ggplot(China) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="China")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))


Italy2 <- ggplot(Italy) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Italy")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Spain2 <- ggplot(Spain) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Spain")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Greece2 <- ggplot(Greece) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Greece")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Portugal2 <- ggplot(Portugal) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Portugal")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

UK2 <- ggplot(UK) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="United Kingdom")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

US2 <- ggplot(US) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="United States")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Switzerland2 <- ggplot(Switzerland) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Switzerland")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Sweden2 <- ggplot(Sweden) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Sweden")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Australia2 <- ggplot(Australia) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Australia")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Austria2 <- ggplot(Austria) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Austria")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Belgium2 <- ggplot(Belgium) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Belgium")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Canada2 <- ggplot(Canada) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Canada")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Czech2 <- ggplot(Czech) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Czech Republic")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Denmark2 <- ggplot(Denmark) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Denmark")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Finland2 <- ggplot(Finland) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Finland")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

France2 <- ggplot(France) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="France")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Germany2 <- ggplot(Germany) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Germany")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Hungary2 <- ggplot(Hungary) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Hungary")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Ireland2 <- ggplot(Ireland) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Ireland")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Japan2 <- ggplot(Japan) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Japan")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

SK2 <- ggplot(SK) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="South Korea")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Netherlands2 <- ggplot(Netherlands) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Netherlands")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Norway2 <- ggplot(Norway) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Norway")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Poland2 <- ggplot(Poland) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Poland")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))


Russia2 <- ggplot(Russia) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Russia")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))


SA2 <- ggplot(SA) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="South Africa")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Turkey2 <- ggplot(Turkey) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Turkey")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Slovakia2 <- ggplot(Slovakia) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Slovakia")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Brazil2 <- ggplot(Brazil) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Brazil")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

India2 <- ggplot(India) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="India")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))
India2

Indonesia2 <- ggplot(Indonesia) +
  geom_line(aes(x=Year, y= checks)) +
  theme_bw()+
  labs(title="Indonesia")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 18))

Indonesia2


ggarrange(Australia2, Austria2, Belgium2, Canada2, Czech2, Denmark2, Finland2, 
          France2, Germany2, Greece2, Hungary2, Ireland2, Italy2, Japan2, SK2,
          Netherlands2, Norway2, Poland2, Portugal2, Russia2, SA2, 
          Spain2, Sweden2, Switzerland2, Turkey2, US2, UK2, Slovakia2, Brazil2, 
          China2, India2, Indonesia2,
          legend = c("right", "none"),
          common.legend = TRUE,
          heights = 2)

