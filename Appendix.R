##########################
####### APPENDIX A #######
##########################
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


## Histograms
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


## Panel view ##
panelchecks <- panelView(Stringency ~ checks, data = df.pd, index = c("Country", "Year"), 
                         axis.lab.gap = c(0,0), main = "CHECKS")

panelpolcon <- panelView(Stringency ~ polcon_v, data = df.pd, index = c("Country", "Year"), 
                         axis.lab.gap = c(0,0), main = "POLCON V")


## DEMOCRATIC SAMPLE ##
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

## Histograms
dev.off()
par(mfrow = c(2,2))
hist(dem.pd$l_string,na.rm = TRUE, main = "Histogram for stringency", 
     col = "#F2F3F4")
hist(dem.pd$polcon_v, na.rm = TRUE, main = "Histogram for polcon v",
     col = "#F2F3F4")
hist(dem.pd$checks, na.rm = TRUE, main = "Histogram for checks",
     col = "#F2F3F4")
hist(dem.pd$corruption, na.rm = TRUE, main = "Histogram for corruption",
     col = "#F2F3F4")
dev.off()

hist(dem.pd$GDPlog,na.rm = TRUE, main = "Histogram for GDP(log)",
     col = "#F2F3F4")
hist(dem.pd$tradelog, na.rm = TRUE, main = "Histogram for trade open(log)",
     col = "#F2F3F4")
hist(dem.pd$green_pres, na.rm = TRUE, main = "Histogram for green presence",
     col = "#F2F3F4")
hist(dem.pd$gov_pol, na.rm = TRUE, main = "Histogram for government polarisation",
     col = "#F2F3F4")


##########################
####### APPENDIX B #######
##########################

####################################
########### STRINGENCY #############
####################################
dta2 <- dta %>%
  group_by(Country, Year) %>%
  summarise (Stringency = mean(Stringency, na.rm = TRUE))

Italy4 <- dta2[dta2$Country == "Italy",]
Spain4 <- dta2[dta2$Country == "Spain",]
Greece4 <- dta2[dta2$Country == "Greece",]
Portugal4 <- dta2[dta2$Country == "Portugal",]
UK4 <- dta2[dta2$Country == "United Kingdom",]
US4 <- dta2[dta2$Country == "United States",]
Switzerland4 <- dta2[dta2$Country == "Switzerland",]
Sweden4 <- dta2[dta2$Country == "Sweden",]
China4 <- dta2[dta2$Country == "China",]
Australia4 <- dta2[dta2$Country == "Australia",]
Austria4 <- dta2[dta2$Country == "Austria",]
Belgium4 <- dta2[dta2$Country == "Belgium",]
Canada4 <- dta2[dta2$Country == "Canada",]
Czech4 <- dta2[dta2$Country == "Czech Republic",]
Denmark4 <- dta2[dta2$Country == "Denmark",]
Finland4 <- dta2[dta2$Country == "Finland",]
France4 <- dta2[dta2$Country == "France",]
Germany4<- dta2[dta2$Country == "Germany",]
Hungary4 <- dta2[dta2$Country == "Hungary",]
Ireland4 <- dta2[dta2$Country == "Ireland",]
Japan4 <- dta2[dta2$Country == "Japan",]
SK4 <- dta2[dta2$Country == "South Korea",]
Netherlands4 <- dta2[dta2$Country == "Netherlands",]
Norway4 <- dta2[dta2$Country == "Norway",]
Poland4 <- dta2[dta2$Country == "Poland",]
Russia4 <- dta2[dta2$Country == "Russia",]
SA4 <- dta2[dta2$Country == "South Africa",]
Turkey4 <- dta2[dta2$Country == "Turkey",]
Slovakia4 <- dta2[dta2$Country == "Slovakia",]
Brazil4 <- dta2[dta2$Country == "Brazil",]
India4 <- dta2[dta2$Country == "India",]
Indonesia4 <- dta2[dta2$Country == "Indonesia",]



China3 <- ggplot(China4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="China")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Italy3 <- ggplot(Italy4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Italy")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Spain3 <- ggplot(Spain4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Spain")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Greece3 <- ggplot(Greece4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Greece")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Portugal3 <- ggplot(Portugal4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Portugal")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

UK3 <- ggplot(UK4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="United Kingdom")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

US3 <- ggplot(US4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="United States")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Switzerland3 <- ggplot(Switzerland4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Switzerland")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Sweden3 <- ggplot(Sweden4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Sweden")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Australia3 <- ggplot(Australia4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Australia")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Austria3 <- ggplot(Austria4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Austria")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Belgium3 <- ggplot(Belgium4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Belgium")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Canada3 <- ggplot(Canada4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Canada")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Czech3 <- ggplot(Czech4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Czech Republic")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Denmark3 <- ggplot(Denmark4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Denmark")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Finland3 <- ggplot(Finland4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Finland")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

France3 <- ggplot(France4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="France")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Germany3 <- ggplot(Germany4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Germany")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Hungary3 <- ggplot(Hungary4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Hungary")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Ireland3 <- ggplot(Ireland4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Ireland")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Japan3 <- ggplot(Japan4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Japan")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

SK3 <- ggplot(SK4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="South Korea")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Netherlands3 <- ggplot(Netherlands4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Netherlands")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Norway3 <- ggplot(Norway4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Norway")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Poland3 <- ggplot(Poland4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Poland")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Russia3 <- ggplot(Russia4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Russia")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

SA3 <- ggplot(SA4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="South Africa")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Turkey3 <- ggplot(Turkey4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Turkey")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Slovakia3 <- ggplot(Slovakia4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Slovakia")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Brazil3 <- ggplot(Brazil4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Brazil")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

India3 <- ggplot(India4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="India")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))

Indonesia3 <- ggplot(Indonesia4) +
  geom_line(aes(x=Year, y= Stringency)) +
  theme_bw()+
  labs(title="Indonesia")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+
  scale_y_continuous(limits=c(0, 5))


ggarrange(Australia3, Austria3, Belgium3, Canada3, Czech3, Denmark3, Finland3, 
          France3, Germany3, Greece3, Hungary3, Ireland3, Italy3, Japan3, SK3,
          Netherlands3, Norway3, Poland3, Portugal3, Russia3, SA3, 
          Spain3, Sweden3, Switzerland3, Turkey3, US3, UK3, Slovakia3, Brazil3, 
          China3, India3, Indonesia3,
          legend = c("right", "none"),
          common.legend = TRUE,
          heights = 2)


plotmeans(l_string ~ Year, data = df.pd)

plotmeans(l_string ~ Year, data = dem.pd)

jtools::plot_summs(FEpolc3, FEpolc6, scale = TRUE,
                   omit.coefs = c("GDPlog", "tradelog", "EU", "gov_pol", 
                                  "polyarchy","corruption", "green_pres",
                                  "(Intercept)", "as.factor(Country)"),
                   colors = c("#000099", "#00CC99", "#000000"),
                   model.names = c("Model 5",
                                   "Model 7"),
                   ci_level = 0.95)+
  theme_bw()


