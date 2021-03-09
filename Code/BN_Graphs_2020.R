########################  Setup ######################## 

rm(list = ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(writexl)
library(tibbletime)
library(ggpubr)
library(quantmod)
library(gridExtra)

########################  Read in and compile MOH deaths-by-cause data ######################## 

deaths <- read_xlsx("../Data/Glasgow_MOH_Deaths_1898-1972.xlsx", sheet = "Deaths_per_Million", na = c("-",""))
colnames(deaths) #<- sub(pattern = " ", replacement = "_", x = protocol[1,])
head(deaths)
str(deaths)

# The dataset needs to be transformed to be able to make figures using ggplot
dths <- gather(data = deaths, key = "MOH_Disease_Name", value = "Death_Rate", -Year)
head(dths); tail(dths)

# Read in key
key <- read_xlsx("../Data/Glasgow_MOH_Deaths_1898-1972.xlsx", sheet = "Key", na = c("","-"))

# Join dths and key to be able to group by other variables
dths <- left_join(x=dths, y=key, by=c("MOH_Disease_Name" = "MOH_Disease_Variable"))
dths.all <- dths %>% filter(Cause_condensed == "All Causes")

dths.Cause_condensed <- dths %>% group_by(Cause_condensed, Year) %>%
  summarise(Death_Rate = sum(Death_Rate, na.rm=T))
dths.stacked <- dths.Cause_condensed  %>%  filter(Cause_condensed != "All Causes") 


########################  Create Stacked Version of the Graph with condensed Causes every bar summing to 1 ######################## 

cols<-c("darkorange","steelblue", "darkred")

g <- ggplot()+geom_area(data=dths.stacked, aes(x=Year, y=Death_Rate,fill=forcats::fct_rev(Cause_condensed)),position = 'fill' )+ theme(legend.key = element_rect(colour = NA, fill = NA),legend.box.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.text=element_text(size=6),axis.title=element_text(size=8,face="bold"),axis.title.x=element_blank(),legend.title = element_blank(),legend.position="bottom",legend.text=element_text(size=6),legend.box="vertical" ,legend.margin=margin())+scale_fill_manual(values=cols, labels =c("Other or \nmixed causes", "Non-communicable \ndiseases", "Infectious \ndiseases")) + scale_color_discrete(breaks = levels(dths.stacked$Cause_condensed))+scale_y_continuous(name = "Share of total deaths",labels=c("0.00" = "0%", "0.25" = "25%", "0.50" = "50%", "0.75" = "75%", "1.00" = "100%"))+guides(fill=guide_legend(ncol=3,nrow=1,byrow=TRUE,reverse=TRUE)); g

ggsave(plot = g, path = "../Figures/", filename = "Figure_1.pdf", width = 8.59, height = 6.54, units = "cm")




######################## Infectious & Noncommunicable ###############################


dths.Noncom <- dths %>%  filter(Cause_condensed == "Non-communicable Diseases" ) %>% group_by(Cause_condensed, Year) %>%
  summarise(Death_Rate = sum(Death_Rate, na.rm=T))
dths.infectious <- dths %>%  filter(Cause_condensed == "Infectious Diseases" ) %>% group_by(Cause_condensed, Year) %>%
  summarise(Death_Rate = sum(Death_Rate, na.rm=T))
g <- ggplot()+geom_rect(aes(xmin=1914, xmax=1918, ymin=-Inf, ymax=Inf), fill ="grey", alpha =0.25)+geom_rect(aes(xmin=1939, xmax=1945, ymin=-Inf, ymax=Inf), fill ="grey", alpha =0.25)+geom_line(data=dths.Noncom, aes(x=Year, y=Death_Rate,color="Non-communicable diseases"), size=0.75)+geom_line(data=dths.infectious, aes(x=Year, y=Death_Rate,color="Infectious diseases"), size=0.75)+scale_color_manual(values = c("darkred", "steelblue"), labels =c( "Infectious \ndiseases","Non-communicable \ndiseases"))+ theme(legend.title = element_blank())+theme(legend.key = element_rect(colour = NA, fill = NA),legend.box.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="bottom",legend.text=element_text(size=8),axis.text=element_text(size=6),axis.title.x=element_blank(),axis.title=element_text(size=8,face="bold"))+scale_y_continuous(name = "Deaths per million"); g
ggsave(plot = g, path = "../Figures/", filename = "Figure_2.pdf", width = 8.4, height =6.32, units = "cm")


######################## 1918 Influenza ###############################

deaths <- read_xlsx("../Data/MOH Spanish Influenza Deaths.xlsx", na = c("-","")) 
deaths$Death_Rate <-deaths$Death_Rate*1000

deathscurrent <- read_xlsx("../Data/GGC-QuarterlyDeathRates.xlsx", na = c("-","")) 
dths <- deaths %>%  filter( YearQuarter =="1917-1" | YearQuarter =="1917-2" | YearQuarter =="1917-3" |YearQuarter == "1917-4"|YearQuarter =="1918-1" | YearQuarter =="1918-2" | YearQuarter =="1918-3" |YearQuarter == "1918-4"|YearQuarter == "1919-1")
area.color_1918 <-c("Winter Quarter","Summer Quarter","Summer Quarter","Winter Quarter","Winter Quarter","Summer Quarter","Summer Quarter","Winter Quarter","Winter Quarter")
g1 <- ggplot(dths,aes(x=YearQuarter,y=Death_Rate, fill=area.color_1918)) + geom_bar(stat="identity", width=0.5)+ylim(0,27500)+ ylab("Annualised deaths per million") +xlab("")+scale_x_discrete(breaks=c("1917-1","1917-2","1917-3","1917-4","1918-1","1918-2","1918-3","1918-4","1919-1"),labels=c("Q1\n1917", "Q2", "Q3", "Q4", "Q1\n1918", "Q2", "Q3", "Q4", "Q1\n1919"))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="none",legend.title=element_blank(),legend.text=element_blank(),axis.text=element_text(size=6),axis.title=element_text(size=8,face="bold"))+scale_fill_manual(values=c("Winter Quarter"="steelblue", "Summer Quarter" ="darkred"));g1

area.color_2020 <-c("Summer Quarter","Summer Quarter","Winter Quarter","Winter Quarter","Summer Quarter","Summer Quarter","Winter Quarter","Winter Quarter","Summer Quarter")
g2 <- ggplot(deathscurrent,aes(x=YearQuarter,y=Death_Rate, fill=area.color_2020)) + geom_bar(data=deathscurrent,stat="identity", width=0.5)+ylim(0,27500)+ ylab("") +xlab("") +scale_x_discrete(breaks=c("2018-2","2018-3","2018-4","2019-1","2019-2","2019-3","2019-4","2020-1","2020-2"),labels=c("Q2", "Q3", "Q4", "Q1\n2019", "Q2", "Q3", "Q4", "Q1\n2020"," Q2"))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = c(0.7, 0.9),legend.key.size = unit(0.25, "cm"),legend.text=element_text(size=6),legend.title=element_blank(),axis.text=element_text(size=6),axis.title=element_text(size=8,face="bold"))+scale_fill_manual(values=c("Winter Quarter"="steelblue", "Summer Quarter" ="darkred"));g2

g3 <- ggarrange(g1 , g2); g3
ggsave(plot = g3, path =  "../Figures/", filename = "Figure_3.pdf", width = 8.89, height =6.8, units = "cm")

############################### Measles, Whooping Cough, Pneumonia, Bronchitis & Influenza  ###############################

deaths.pnbrin <- read_xlsx("../Data/HP_filtered_RespDiseases.xlsx", na = c("-","")) 

g1 <- ggplot() +scale_color_manual(values = c( "darkorange"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="bottom",legend.text=element_text(size=6),axis.text=element_text(size=6),axis.title=element_text(size=8,face="bold"))+
  geom_line(data=deaths.pnbrin, aes(x=Year, y=Pneumonia,color=""), size=0.5)+  theme(legend.position = "none",axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(name = "Pneumonia \n");
g2 <- ggplot()+  scale_color_manual(values = c( "purple"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="bottom",legend.text=element_text(size=6),axis.text=element_text(size=6),axis.title=element_text(size=8,face="bold"))+
  geom_line(data=deaths.pnbrin, aes(x=Year, y=Bronchitis,color="Bronchitis"), size=0.5)+  theme(legend.position = "none",axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(name = "Bronchitis \n");

g3 <- ggplot()+  scale_color_manual(values = c( "darkred"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="bottom",legend.text=element_text(size=6),axis.text=element_text(size=6),axis.title=element_text(size=8,face="bold"),axis.title.x=element_blank())+
  geom_line(data=deaths.pnbrin, aes(x=Year, y=Influenza,color="Influenza"), size=0.5)+  theme(legend.position = "none",axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(name = "Influenza \n");

g4 <- ggplot()+  scale_color_manual(values = c( "steelblue"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="bottom",legend.text=element_text(size=6),axis.text=element_text(size=6),axis.title=element_text(size=8,face="bold"))+
  geom_line(data=deaths.pnbrin, aes(x=Year, y=Pulmory_Tuberculosis,color="Pulmonary tuberculosis"), size=0.5)+  theme(legend.position = "none",axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(name = "Pulmonary \ntuberculosis");
g5 <- ggplot()+  scale_color_manual(values = c( "darkgreen"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="bottom",legend.text=element_text(size=6),axis.text=element_text(size=6),axis.title=element_text(size=8,face="bold"))+
  geom_line(data=deaths.pnbrin, aes(x=Year, y=Whooping_Cough,color="Whooping cough"), size=0.5)+  theme(legend.position = "none",axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(name = "Whooping \ncough");
g6 <- ggplot()+  scale_color_manual(values = c( "black"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="bottom",legend.text=element_text(size=6),axis.text=element_text(size=6),axis.title=element_text(size=8,face="bold"))+
  geom_line(data=deaths.pnbrin, aes(x=Year, y=All_Resp,color="Selected respiratory \ndiseases"), size=0.5)+  theme(legend.position = "none",axis.title.x=element_blank())+scale_y_continuous(name = "Selected \nrespiratory");

p = grid.arrange(ncol = 1, nrow = 6, g5, g4, g2,g1,g3, g6 );p
ggsave(plot = p, path = "../Figures/", filename = "Figure_4.pdf", width = 8.87, height =1.2*8.87, units = "cm")

g1 <- ggplot()+  scale_color_manual(values = c( "darkorange"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="bottom",legend.text=element_text(size=6),axis.text=element_text(size=6),axis.title=element_text(size=8,face="bold"))+
  geom_line(data=deaths.pnbrin, aes(x=Year, y=hp_Pneumonia,color=""), size=0.5)+  theme(legend.position = "none",axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(name = "Pneumonia \n", limit=c(-1000,1250));
g2 <- ggplot()+  scale_color_manual(values = c( "purple"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="bottom",legend.text=element_text(size=6),axis.text=element_text(size=6),axis.title=element_text(size=8,face="bold"))+
  geom_line(data=deaths.pnbrin, aes(x=Year, y=hp_Bronchitis,color="Bronchitis"), size=0.5)+  theme(legend.position = "none",axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(name = "Bronchitis \n", limit=c(-1000,1250));
g3 <- ggplot()+  scale_color_manual(values = c( "darkred"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="bottom",legend.text=element_text(size=6),axis.text=element_text(size=6),axis.title=element_text(size=8,face="bold"),axis.title.x=element_blank())+
  geom_line(data=deaths.pnbrin, aes(x=Year, y=hp_Influenza,color="Influenza"), size=0.5)+  theme(legend.position = "none",axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(name = "Influenza \n", limit=c(-1000,1250));
g4 <- ggplot()+  scale_color_manual(values = c( "steelblue"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="bottom",legend.text=element_text(size=6),axis.text=element_text(size=6),axis.title=element_text(size=8,face="bold"))+
  geom_line(data=deaths.pnbrin, aes(x=Year, y=hp_Pulmory_Tuberculosis,color="Pulmonary tuberculosis"), size=0.5)+  theme(legend.position = "none",axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(name = "Pulmonary \ntuberculosis", limit=c(-1000,1250));
g5 <- ggplot()+  scale_color_manual(values = c( "darkgreen"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="bottom",legend.text=element_text(size=6),axis.text=element_text(size=6),axis.title=element_text(size=8,face="bold"))+
  geom_line(data=deaths.pnbrin, aes(x=Year, y=hp_whooping,color="Whooping cough"), size=0.5)+  theme(legend.position = "none",axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(name = "Whooping \ncough",limit=c(-1000,1250));
g6 <- ggplot()+  scale_color_manual(values = c( "black"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="bottom",legend.text=element_text(size=6),axis.text=element_text(size=6),axis.title=element_text(size=8,face="bold"))+
  geom_line(data=deaths.pnbrin, aes(x=Year, y=hp_AllResp,color="Certain respiratory \ndiseases"), size=0.5)+  theme(legend.position = "none",axis.title.x=element_blank())+scale_y_continuous(name = "Selected \nrespiratory",limit=c(-1000,1250));

p = grid.arrange(ncol = 1, nrow = 6, g5, g4, g2,g1, g3, g6 );p
ggsave(plot = p, path = "../Figures/", filename = "Figure_5.pdf", width = 8.87, height =1.2*8.87, units = "cm")


############################### Variances & Counterfactuals ############################### 

deaths.resp <- read_xlsx("../Data/Resp_StandardDev.xlsx", na = c("-","")) 
deaths.respcounterfactual <- read_xlsx("../Data/Resp_StandardDevCounterfactual.xlsx", na = c("-","")) 

g <- ggplot()+geom_rect(aes(xmin=1918, xmax=1940, ymin=-Inf, ymax=Inf), fill ="grey", alpha =0.25)+ geom_point(data = deaths.respcounterfactual,size = 1, aes(x = Year, y = sd_total,colour ="Counterfactual"))+geom_line(data=deaths.respcounterfactual,  aes(x = Year, y = sd_total,colour ="Counterfactual")) + geom_point(size = 1, data=deaths.resp, aes(x = Year, y = sd_total,colour ="Observed")) +geom_line(data=deaths.resp, aes(x = Year, y = sd_total, colour ="Observed")) +  theme(legend.key = element_rect(colour = NA, fill = NA),legend.box.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="bottom",legend.title=element_blank(),legend.text=element_text(size=6),axis.text=element_text(size=6),axis.title=element_text(size=8,face="bold"),axis.title.x=element_blank())+scale_color_manual(values = c( "darkred", "steelblue"), guide = guide_legend(reverse=TRUE))+scale_y_continuous(name = "Standard deviation of death rates \n from selected respiratory diseases   \n (10-year rolling window)");g

ggsave(plot = g, path = "../Figures/", filename = "Figure_6.pdf",  width = 9.29, height =7.11, units = "cm")



