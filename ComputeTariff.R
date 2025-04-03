rm(list=ls())
library(tidyverse)
library(data.table)
library(grid)
library(gridExtra)

dir<-c("C:\\Users\\Yukikaze\\Downloads\\DataJobID-2840104_2840104_TwUS.csv")

dta<-read_csv(dir)%>%as.data.table()
dta<-dta[`Reporter Name`!=`Partner Name`,]
dta<-dta[Reporter=="158",`Reporter Name`:="Taiwan"]
dta<-dta[Partner=="158",`Partner Name`:="Taiwan"]

dta<-dta[,.(`Trade Year`,`Reporter Name`,`Partner Name`,`DutyType`,
            `Product Name`,`Product`,
            `Imports Value in 1000 USD`,`Weighted Average`)]
dta<-dta[,Sec2d:=gsub("\\d{2}$", "", Product)]

dta<-dta[,`:=`(Tariff2d=weighted.mean(x=`Weighted Average`,w=`Imports Value in 1000 USD`,na.rm=T),
               Import2d=sum(`Imports Value in 1000 USD`,na.rm=T)),
         by=.(`Trade Year`,`Reporter Name`,`Partner Name`,`DutyType`,`Sec2d`)]
dta<-dta[,`:=`(TariffAgg=weighted.mean(x=`Weighted Average`,w=`Imports Value in 1000 USD`,na.rm=T),
               ImportAgg=sum(`Imports Value in 1000 USD`,na.rm=T)),
         by=.(`Trade Year`,`Reporter Name`,`Partner Name`,`DutyType`)]


ahs<-copy(dta)[DutyType=="AHS",.(`Trade Year`,`Reporter Name`,`Partner Name`,
                                 Sec2d,Import2d,Tariff2d)]%>%unique()
setorder(ahs,`Trade Year`,`Reporter Name`,`Sec2d`)
bnd<-copy(dta)[DutyType=="BND",.(`Trade Year`,`Reporter Name`,`Partner Name`,
                                 Sec2d,Import2d,Tariff2d)]%>%unique()
setorder(bnd,`Trade Year`,`Reporter Name`,`Sec2d`)


ahsAgg<-copy(dta)[DutyType=="AHS",.(`Trade Year`,`Reporter Name`,`Partner Name`,
                                    ImportAgg,TariffAgg)]%>%unique()
setorder(ahsAgg,`Trade Year`,`Reporter Name`)

bndAgg<-copy(dta)[DutyType=="BND",.(`Trade Year`,`Reporter Name`,`Partner Name`,
                                    ImportAgg,TariffAgg)]%>%unique()
setorder(bndAgg,`Trade Year`,`Reporter Name`)

tmp<-dta$`Trade Year`%>%unique()%>%sort()

sty<-list(theme_minimal(),
          theme(axis.title.x=element_text(size=16),
                axis.text.x=element_text(size=12,angle=90),
                axis.title.y=element_text(size=16),
                axis.text.y=element_text(size=12),
                legend.text=element_text(size=12),
                legend.title=element_text(size=12),
                legend.key.size=unit(15, 'pt'),
                legend.position="bottom",
                axis.line = element_line(color = "black"),
                plot.title = element_text(size=15,hjust = 0.5),
                plot.background = element_rect(color="white")))

sty2<-list(theme_minimal(),
          theme(axis.title.x=element_text(size=16),
                axis.text.x=element_text(size=12,angle=90),
                axis.title.y=element_text(size=16),
                axis.text.y=element_text(size=12),
                legend.text=element_text(size=12),
                legend.title=element_text(size=12),
                legend.key.size=unit(15, 'pt'),
                legend.position="right",
                axis.line = element_line(color = "black"),
                plot.title = element_text(size=15,hjust = 0.5),
                plot.background = element_rect(color="white")))

ggplot(data=ahsAgg,
       aes(x=`Trade Year`,y=TariffAgg,color=`Reporter Name`,shape=`Reporter Name`))+
  geom_point(size=2)+geom_line(linewidth=1.2)+
  sty+
  labs(y="%",x="",color="",fill="",
       title = "Average Effectively Applied Import Tariff Rates (weighted by Import Value)")+
  scale_x_continuous(breaks = tmp)+
  scale_color_manual(name="Reporting Country",
                     values=c("#0B3D91", "#D6675A"),
                     breaks=c("Taiwan","United States"),
                     labels=c("Taiwan","USA"))+
  scale_shape_manual(name="Reporting Country",
                     values=c(16,17),
                     breaks=c("Taiwan","United States"),
                     labels=c("Taiwan","USA"))

prim<-c("01", "02", "05", "10", "11", "13", "14")
sec1<-c("15", "16", "17", "18", "19", "20", "21", "22")
pcpm<-c("23", "24", "25", "26", "27", "28")
mcee<-c("29", "30", "31", "32", "33")
othr<-c("34", "35", "36", "74", "92", "99", "40", "93")

plttw<-list()
pltus<-list()
pltout<-list()
sec<-list(prim,sec1,pcpm,mcee,othr)
owncol<-c("#2CBF00", "#FF0000", "#0000FF", "#AA00AA",
          "#FFAA00", "#00CED1", "#8B7513", "#FFA1DC")

for (i in 1:length(sec)) {
  dtmp<-copy(ahs)[Sec2d%in%sec[[i]],]
  plttw[[i]]<-ggplot(data=copy(dtmp)[`Reporter Name`=="Taiwan",],
                     aes(x=`Trade Year`,y=Tariff2d,color=Sec2d))+
    geom_point(size=2,shape=16)+geom_line(linewidth=1.2)+
    sty+
    labs(y="%",x="",color="",fill="",
         title = "Taiwan Side")+
    scale_x_continuous(breaks = tmp)+
    scale_color_manual(name="2d ISIC R3 Sectors",
                       values=owncol)+
    ylim(0,40)
  
  pltus[[i]]<-ggplot(data=copy(dtmp)[`Reporter Name`=="United States",],
                     aes(x=`Trade Year`,y=Tariff2d,color=Sec2d))+
    geom_point(size=2,shape=17)+geom_line(linewidth=1.2)+
    sty+
    labs(y="%",x="",color="",fill="",
         title = "US Side")+
    scale_x_continuous(breaks = tmp)+
    scale_color_manual(name="2d ISIC R3 Sectors",
                       values=owncol)+
    ylim(0,40)
  
  pltout[[i]]<-grid.arrange(grobs=list(plttw[[i]],pltus[[i]]),
                            layout_matrix = matrix(1:2,1,2),
                            widths=c(1,1),
                            heights=c(1),
                            top="Average Effectively Applied Import Tariff Rates (weighted by Import Value)")
}

for (i in 1:length(pltout)) {
  ggsave(pltout[[i]],
         filename=paste("C:\\Users\\Yukikaze\\Downloads\\Tariff_",i,".png",sep=""),
                  units="cm",height=17,width=35)
}

twimp<-copy(ahs)[`Reporter Name`=="Taiwan",]
twimp<-twimp[,Import_Yr:=sum(Import2d),
             by=.(`Trade Year`)]
twimp<-twimp[,Protect:=0]
twimp<-twimp[Sec2d%in%c("01","05","15","16","34"),Protect:=1]
twimp<-twimp[,Import_YrProt:=sum(Import2d),
             by=.(`Trade Year`,Protect)]
twimp<-twimp[,.(`Trade Year`,Protect,Import_Yr,Import_YrProt)]%>%unique()
twimp<-twimp[,ProtShr:=100*Import_YrProt/Import_Yr]
twimp<-twimp[Protect==1,]%>%unique()
