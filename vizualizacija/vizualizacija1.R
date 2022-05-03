# vizualizacija - vse brez časov

source("~/Faks/mag 1 letnik/MzR/Time-to-yes-contract-and-money/uvoz/uvoz.R", encoding="UTF-8")

# prvo po atributih

# PRODUKTI
# delež v celem letu in po mesecih
del.produktov <- as.data.frame(prop.table(table(podatki$produkt))*100)
delez.produktov <- rename(del.produktov, c("produkt" = "Var1", "delez" = "Freq"))

graf1 <- ggplot(delez.produktov, aes(x="", y=delez, fill =produkt)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_label(aes(x="", y = delez + 2, label = delez), 
            position = position_dodge(width = 0.9), show.legend = FALSE) +
  ggtitle("Delež posameznih produktov (v odstotkih) v celem letu")
#print(graf1)

graf1.2 <- ggplot(delez.produktov, aes(x=1, y=delez, fill=produkt)) +
  geom_col() +
  geom_text(aes(label = delez), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + 
  theme_void() + ggtitle("Delež posameznih produktov (v odstotkih) v celem letu") +
  scale_fill_discrete(labels = c("avtomobilski", "hipotekarni",
                                     "investicijski", "izobraževalni", "osebni", "startup", "študentski")) 
  
#print(graf1.2)


mes.del.prod <- subset(podatki, select = c(mesec, produkt))
mes.delez.produktov <- mes.del.prod %>% group_by(mesec) %>% count(produkt) %>% 
                        summarise(delez = round(n/sum(n) * 100,2), produkt)
df.jan <- data.frame("Jan",0,"avtomobilski")

names(df.jan) <- c("mesec", "delez", "produkt")

mes.delez.produktov <- rbind(mes.delez.produktov, df.jan)


graf2.jan <- mes.delez.produktov %>% filter(mesec == "Jan") %>% 
  ggplot(aes(x=1, y=delez, fill=produkt)) +
  geom_col() +
  #geom_text(aes(label = delez), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + scale_fill_brewer(palette="Dark2") + 
  theme_void()
#print(graf2.jan)

graf2.feb <- mes.delez.produktov %>% filter(mesec == "Feb") %>% 
  ggplot(aes(x=1, y=delez, fill=produkt)) +
  geom_col() +
  #geom_text(aes(label = delez), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + scale_fill_brewer(palette="Dark2") + 
  theme_void()
#print(graf2.feb)

graf2.mar <- mes.delez.produktov %>% filter(mesec == "Mar") %>% 
  ggplot(aes(x=1, y=delez, fill=produkt)) +
  geom_col() +
  #geom_text(aes(label = delez), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + scale_fill_brewer(palette="Dark2") + 
  theme_void()
#print(graf2.mar)

graf2.apr <- mes.delez.produktov %>% filter(mesec == "Apr") %>% 
  ggplot(aes(x=1, y=delez, fill=produkt)) +
  geom_col() +
  #geom_text(aes(label = delez), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + scale_fill_brewer(palette="Dark2") + 
  theme_void()
#print(graf2.apr)

graf2.maj <- mes.delez.produktov %>% filter(mesec == "Maj") %>% 
  ggplot(aes(x=1, y=delez, fill=produkt)) +
  geom_col() +
  #geom_text(aes(label = delez), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + scale_fill_brewer(palette="Dark2") + 
  theme_void()
#print(graf2.maj)

graf2.jun <- mes.delez.produktov %>% filter(mesec == "Jun") %>% 
  ggplot(aes(x=1, y=delez, fill=produkt)) +
  geom_col() +
  #geom_text(aes(label = delez), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + scale_fill_brewer(palette="Dark2") + 
  theme_void()
#print(graf2.jun)

graf2.jul <- mes.delez.produktov %>% filter(mesec == "Jul") %>% 
  ggplot(aes(x=1, y=delez, fill=produkt)) +
  geom_col() +
  #geom_text(aes(label = delez), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + scale_fill_brewer(palette="Dark2") + 
  theme_void()
#print(graf2.jul)

graf2.aug <- mes.delez.produktov %>% filter(mesec == "Aug") %>% 
  ggplot(aes(x=1, y=delez, fill=produkt)) +
  geom_col() +
  #geom_text(aes(label = delez), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + scale_fill_brewer(palette="Dark2") + 
  theme_void()
#print(graf2.aug)

graf2.sep <- mes.delez.produktov %>% filter(mesec == "Sep") %>% 
  ggplot(aes(x=1, y=delez, fill=produkt)) +
  geom_col() +
  #geom_text(aes(label = delez), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + scale_fill_brewer(palette="Dark2") + 
  theme_void()
#print(graf2.sep)

graf2.oct <- mes.delez.produktov %>% filter(mesec == "Oct") %>% 
  ggplot(aes(x=1, y=delez, fill=produkt)) +
  geom_col() +
  #geom_text(aes(label = delez), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + scale_fill_brewer(palette="Dark2") + 
  theme_void()
#print(graf2.oct)

graf2.nov <- mes.delez.produktov %>% filter(mesec == "Nov") %>% 
  ggplot(aes(x=1, y=delez, fill=produkt)) +
  geom_col() +
  #geom_text(aes(label = delez), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + scale_fill_brewer(palette="Dark2") + 
  theme_void()
#print(graf2.nov)

graf2.dec <- mes.delez.produktov %>% filter(mesec == "Dec") %>% 
  ggplot(aes(x=1, y=delez, fill=produkt)) +
  geom_col() +
  #geom_text(aes(label = delez), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + scale_fill_brewer(palette="Dark2") + 
  theme_void()
#print(graf2.dec)

prod.po.mes <- ggarrange(graf2.jan, graf2.feb, graf2.mar, graf2.apr,graf2.maj, graf2.jun, graf2.jul,
          graf2.aug, graf2.sep, graf2.oct, graf2.nov, graf2.dec, ncol=3, nrow=4, 
          labels = c("JAN", "FEB", "MAR", "APR", "MAJ", "JUN", "JUL", "AVG", "SEP", "OKT", "NOV", "DEC"),
           common.legend = TRUE, legend="bottom") + 
  scale_colour_discrete(labels = c("avtomobilski", "hipotekarni","investicijski", "izobraževalni", "osebni", "startup", "študentski"))
prod.po.mes <- annotate_figure(prod.po.mes, top = text_grob("Delež produktov po mesecih", 
                                      color = "blue", face = "bold", size = 14))

#odstrani procente
#print(prod.po.mes)


one.year <- c("Jan","Feb","Mar","Apr","Maj","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
mes.delez.produktov.2 <- mes.delez.produktov
mes.delez.produktov.2$mesec <- factor(mes.delez.produktov.2$mesec, levels=one.year)

prod.po.mes.2 <- ggplot(mes.delez.produktov.2, aes(x=mesec, y=delez, group=produkt, colour=produkt)) + 
                 geom_line() + geom_point() + ggtitle("Delež posameznih produktov (v odstotkih) po mesecih") +
                 ylab("delež (v %)") + scale_colour_discrete(labels = c("avtomobilski", "hipotekarni",
                  "investicijski", "izobraževalni", "osebni", "startup", "študentski"))
#print(prod.po.mes.2)


# TIPI
# delež v celem letu in po mesecih
del.tipov <- as.data.frame(prop.table(table(podatki$tip))*100)
delez.tipov <- rename(del.tipov, c("tip" = "Var1", "delez" = "Freq"))

graf3 <- ggplot(delez.tipov, aes(x="", y=delez, fill = tip)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_label(aes(x="", y = delez + 2, label = delez), 
             position = position_dodge(width = 0.9), show.legend = FALSE) + 
  ggtitle("Delež posameznih tipov (v odstotkih) v celem letu") + xlab("tip") + ylab("delež (v %)")
#print(graf3)

graf3.2 <- ggplot(delez.tipov, aes(x=1, y=delez, fill=tip)) +
  geom_col() +
  geom_text(aes(label = delez), position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") + scale_fill_brewer(palette="Dark2") + 
  theme_void() + ggtitle("Delež posameznih tipov (v odstotkih) v celem letu")
#print(graf3.2)

mes.del.tip <- subset(podatki, select = c(mesec, tip))
mes.delez.tipov <- mes.del.tip %>% group_by(mesec) %>% count(tip) %>% 
  summarise(delez = n/sum(n) * 100, tip)
mes.delez.tipov$mesec <- factor(mes.delez.tipov$mesec, levels=one.year)

tip.po.mes <- ggplot(mes.delez.tipov, aes(x=mesec, y=delez, group=tip, colour=tip)) + 
  geom_line() + geom_point() + ggtitle("Delež posameznih tipov (v odstotkih) po mesecih") +
  ylab("delež (v %)")
#print(tip.po.mes)

prod.tip <- subset(podatki, select = c(produkt, tip))
prod.del.tip <- prod.tip %>% group_by(produkt) %>% count(tip) %>% 
  summarise(delez = n/sum(n) * 100, tip)

graf3.3 <- ggplot(prod.tip, aes(produkt)) +
  geom_bar(aes(fill=tip), position = "fill") +
  ggtitle("Delež posameznih tipov znotraj posameznega produkta") + xlab("produkt") + ylab("delež") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
#print(graf3.3)


#MESEC
mes.del.prod$mesec <- factor(mes.del.prod$mesec, levels=one.year)
mesec.produkti <- mes.del.prod %>% group_by(mesec) %>% count(produkt)
mesec.produkti <- rename(mesec.produkti, "stevilo" = "n")


graf4 <- ggplot(mesec.produkti, aes(x=mesec, y=stevilo, group=produkt, colour=produkt)) + 
  geom_line() + geom_point() + ggtitle("Število posameznih produktov po mesecih") +
  ylab("število") + scale_colour_discrete(labels = c("avtomobilski", "hipotekarni",
                                                         "investicijski", "izobraževalni", "osebni", "startup", "študentski"))
#print(graf4)

graf4.1 <- ggplot(mes.del.prod, aes(mesec)) +
  geom_bar(aes(fill=produkt), position = "fill") +
  ggtitle("Delež posameznih tipov znotraj posameznega meseca") + xlab("mesec") + ylab("delež")
#print(graf4.1)


mesec.produkti.skupaj <- mesec.produkti %>% summarise(vseh = sum(stevilo))
graf4.2 <- ggplot(mesec.produkti.skupaj, aes(x=mesec, y=vseh, group=1)) + 
  geom_smooth() + geom_point() + ggtitle("Število vseh produktov po mesecih") +
  ylab("število")
#print(graf4.2)
paste("Največ produktov je bilo", mesec.produkti.skupaj[[which.max(mesec.produkti.skupaj$vseh),1]], "in sicer", 
      max(mesec.produkti.skupaj$vseh), "najmanj pa", mesec.produkti.skupaj[[which.min(mesec.produkti.skupaj$vseh),1]],
      "in sicer", min(mesec.produkti.skupaj$vseh))                   

mesec.povp <- mean(mesec.produkti.skupaj$vseh)
mesec.med <- median(mesec.produkti.skupaj$vseh)
mesec.max <- max(mesec.produkti.skupaj$vseh) 
mesec.min <- min(mesec.produkti.skupaj$vseh)
mesec.odst.gor <- mesec.max - mesec.povp
mesec.odst.dol <- mesec.povp - mesec.min

graf4.3 <- graf4.2 + geom_line(aes(y=mesec.povp, colour="Povprečje")) + 
           geom_line(aes(y=mesec.med, colour = "Mediana")) +
  scale_colour_manual("", breaks = c("Povprečje", "Mediana"),
                      values = c("Povprečje"="red", "Mediana"="green"))
#print(graf4.3)


mes.del.tip$mesec <- factor(mes.del.tip$mesec, levels=one.year)
mesec.tipi <- mes.del.tip %>% group_by(mesec) %>% count(tip)
mesec.tipi <- rename(mesec.tipi, "stevilo" = "n")

mesec.tipi.skupaj <- mesec.tipi %>% summarise(vseh = sum(stevilo))

graf4.4 <- ggplot(mesec.tipi.skupaj, aes(x=mesec, y=vseh, group=1)) + 
  geom_smooth() + geom_point() + ggtitle("Število vseh produktov po mesecih") +
  ylab("število")
#print(graf4.4)
# isti graf kot prej
#paste("Največ tipov je bilo", mesec.tipi.skupaj[which.max(mesec.tipi.skupaj$vseh),1], "in sicer", max(mesec.tipi.skupaj$vseh),
#      "najmanj pa", mesec.tipi.skupaj[which.min(mesec.tipi.skupaj$vseh),1], "in sicer", min(mesec.tipi.skupaj$vseh))                   
# nima smisla, mora biti enako kot pri produktih


# REGIJA
regija <- subset(podatki, select = c(produkt, mesec, tip, regija, poslovalnica))

graf5 <- ggplot(regija, aes(y=produkt, fill = regija)) +
  geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
  ggtitle("Število posameznih produktov v posamezni regiji") + 
  xlab("število")
#print(graf5)


prod.regija <- regija %>% group_by(regija) %>% count(produkt, mesec)
prod.regija <- rename(prod.regija, "st_prod" = "n")

tip.regija <- regija %>% group_by(regija) %>% count(tip, mesec)
tip.regija <- rename(tip.regija, "st_tip" = "n")

regije.skupaj <- prod.regija %>% summarise(vseh = sum(st_prod))
paste("Več obelanih kreditov je v vzhodni regiji, kar je logično saj je tam tudi več poslovalnic")

vzhodna.prod <- prod.regija %>% filter(regija == "vzhodna")
paste("Največ produktov v vzhodni regiji je", as.character(vzhodna.prod[which.max(vzhodna.prod$st_prod),2]), 
      "in sicer", as.character(vzhodna.prod[which.max(vzhodna.prod$st_prod),3]), "najmanj pa",
      as.character(vzhodna.prod[which.min(vzhodna.prod$st_prod),2]), "in sicer", 
      as.character(vzhodna.prod[which.min(vzhodna.prod$st_prod),3]))

vzhodna.prod$mesec <- factor(vzhodna.prod$mesec, labels = one.year)
graf5.1 <- ggplot(vzhodna.prod, aes(x=mesec, y=st_prod, group=produkt, colour=produkt)) + 
  geom_line() + geom_point() + ggtitle("Število posameznih produktov v vzhodni regiji po mesecih") +
  ylab("število") + scale_colour_discrete(labels = c("avtomobilski", "hipotekarni",
                                                     "investicijski", "izobraževalni", "osebni", "startup", "študentski"))
#print(graf5.1)

vzhodna.tip <- tip.regija %>% filter(regija == "vzhodna")
paste("Največ tipov v vzhodni regiji je", as.character(vzhodna.tip[which.max(vzhodna.tip$st_tip),2]), 
      "in sicer", as.character(vzhodna.tip[which.max(vzhodna.tip$st_tip),3]), "najmanj pa",
      as.character(vzhodna.tip[which.min(vzhodna.tip$st_tip),2]), "in sicer",
      as.character(vzhodna.tip[which.min(vzhodna.tip$st_tip),3]))

vzhodna.tip$mesec <- factor(vzhodna.tip$mesec, labels = one.year)
graf5.2 <- ggplot(vzhodna.tip, aes(x=mesec, y=st_tip, group=tip, colour=tip)) + 
  geom_line() + geom_point() + ggtitle("Število posameznih tipov v vzhodni regiji po mesecih") +
  ylab("število")
#print(graf5.2)


zahodna.prod <- prod.regija %>% filter(regija == "zahodna")
paste("Največ produktov v zahodni regiji je", as.character(zahodna.prod[which.max(zahodna.prod$st_prod),2]), 
      "in sicer", as.character(zahodna.prod[which.max(zahodna.prod$st_prod),3]), "najmanj pa",
      as.character(zahodna.prod[which.min(zahodna.prod$st_prod),2]), "in sicer", 
      as.character(zahodna.prod[which.min(zahodna.prod$st_prod),3]))

zahodna.prod$mesec <- factor(zahodna.prod$mesec, labels = one.year)
graf5.3 <- ggplot(zahodna.prod, aes(x=mesec, y=st_prod, group=produkt, colour=produkt)) + 
  geom_line() + geom_point() + ggtitle("Število posameznih produktov v zahodni regiji po mesecih") +
  ylab("število")
#print(graf5.3)

zahodna.tip <- tip.regija %>% filter(regija == "zahodna")
paste("Največ tipov v zahodni regiji je", as.character(vzhodna.tip[which.max(zahodna.tip$st_tip),2]), 
      "in sicer", as.character(zahodna.tip[which.max(zahodna.tip$st_tip),3]), "najmanj pa",
      as.character(zahodna.tip[which.min(zahodna.tip$st_tip),2]), "in sicer",
      as.character(zahodna.tip[which.min(zahodna.tip$st_tip),3]))

zahodna.tip$mesec <- factor(zahodna.tip$mesec, labels = one.year)
graf5.4 <- ggplot(zahodna.tip, aes(x=mesec, y=st_tip, group=tip, colour=tip)) + 
  geom_line() + geom_point() + ggtitle("Število posameznih tipov v zahodni regiji po mesecih") +
  ylab("število")
#print(graf5.4)



# POSLOVALNICA
posl.regija <- regija %>% group_by(regija) %>% count(poslovalnica)
posl.regija <- rename(posl.regija, "st_poslov" = "n")
paste("Največ poslov opravi", posl.regija[which.max(posl.regija$st_poslov), 2],"v regiji", 
      as.character(posl.regija[which.max(posl.regija$st_poslov), 1]), "in sicer", posl.regija[which.max(posl.regija$st_poslov), 3],
      "najmanj pa", posl.regija[which.min(posl.regija$st_poslov), 2], "v regiji", 
      as.character(posl.regija[which.min(posl.regija$st_poslov), 1]), "in sicer", posl.regija[which.min(posl.regija$st_poslov), 3])

graf6 <- ggplot(regija, aes(y=poslovalnica, fill = regija)) +
  geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
  ggtitle("Število obdelanih vlog v posamezni poslovalnici") + 
  xlab("število")
#print(graf6)

posl.mesec <- regija %>% group_by(mesec) %>% count(poslovalnica)
posl.mesec <- rename(posl.mesec, "st_poslov" = "n")
posl.mesec$mesec <- factor(posl.mesec$mesec, levels = one.year)

graf6.1 <- ggplot(posl.mesec, aes(x=mesec, y=st_poslov, group=poslovalnica, colour=poslovalnica)) + 
  geom_line() + geom_point() + ggtitle("Število poslov v posamezni posovalnici po mesecih") +
  ylab("število")
#print(graf6.1)

posl.mesec.skupni <- posl.mesec %>% group_by(mesec) %>% summarise(skupaj = sum(st_poslov))
graf6.2 <- ggplot(posl.mesec.skupni, aes(x=mesec, y=skupaj, group=1)) + 
  geom_smooth() + geom_point() + ggtitle("Število vseh poslov po mesecih") +
  ylab("število")
#print(graf6.2)
# isti graf kot pri produktih :)


graf6.3 <- ggplot(regija, aes(y=poslovalnica, fill = produkt)) +
  geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
  ggtitle("Število posameznih produktov v posamezni poslovalnici") + 
  xlab("število")
#print(graf6.3)

graf6.3.2 <- ggplot(regija, aes(y=produkt, fill = poslovalnica)) +
  geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
  ggtitle("Število posameznih produktov v posamezni poslovalnici") + 
  xlab("število")
#print(graf6.3.2)

graf6.4 <- ggplot(regija, aes(y=poslovalnica, fill = tip)) +
  geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
  ggtitle("Število posameznih tipov v posamezni poslovalnici") + 
  xlab("število")
#print(graf6.4)

graf6.4.2 <- ggplot(regija, aes(y=tip, fill = poslovalnica)) +
  geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
  ggtitle("Število posameznih tipov v posamezni poslovalnici") + 
  xlab("število")
#print(graf6.4.2)

#prve verzije boljše


posl.mesec.prod <- regija %>% group_by(mesec) %>% count(produkt, poslovalnica)
posl.mesec.prod <- rename(posl.mesec.prod, "st_prod" = "n")


posl.mesec.tip <- regija %>% group_by(mesec) %>% count(tip, poslovalnica)
posl.mesec.tip <- rename(posl.mesec.tip, "st_poslov" = "n")


# ZNESEK
znesek <- subset(podatki, select = c(produkt, mesec, tip, znesek, regija, poslovalnica))
znesek$mesec <- factor(znesek$mesec, levels = one.year)

# znesek-produkt
znesek.prod.povp <- znesek %>% group_by(produkt) %>% summarise(povpr_prod = mean(znesek))

graf7.1 <- ggplot(znesek.prod.povp, aes(x = produkt, y=povpr_prod, fill = produkt)) + geom_col() + 
           ylab("Povprečen znesek") + ggtitle("Povprečni zneski glede na produkt") + scale_fill_brewer("Accent")
#print(graf7.1)

graf7.1.2 <- ggplot(znesek, aes(x= produkt, y=znesek, fill = produkt)) + 
             geom_boxplot(outlier.color = "blue") + scale_fill_brewer(palette = "Dark2") +
             stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
             ggtitle("Graf kvantilov zneskov po produktih") + 
             theme(legend.position="none")
#print(graf7.1.2)

znesek.mesec.povp <- znesek %>% group_by(mesec) %>% summarise(povpr_prod = mean(znesek))
graf7.2 <- ggplot(znesek.mesec.povp, aes(x=mesec, y=povpr_prod, group=1)) + 
  geom_smooth() + geom_point() + ggtitle("Povprečen znesek po mesecih") +
  ylab("popvrečen znesek")
#print(graf7.2)

graf7.2.2 <- graf7.2 + geom_line(aes(y=mean(povpr_prod), colour="Povprečje")) + 
  geom_line(aes(y=median(povpr_prod), colour = "Mediana")) +
  scale_colour_manual("", breaks = c("Povprečje", "Mediana"),
                      values = c("Povprečje"="red", "Mediana"="green"))
#print(graf7.2.2)

graf7.2.3 <- ggplot(znesek, aes(x= mesec, y=znesek, fill = mesec)) + 
             geom_boxplot(outlier.color = "blue") +
             stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
             ggtitle("Graf kvantilov zneskov po mesecih")
#print(graf7.2.3)


znesek.tip.povp <- znesek %>% group_by(tip) %>% summarise(povpr_prod = mean(znesek))

graf7.3 <- ggplot(znesek.tip.povp, aes(x = tip, y=povpr_prod, fill = tip)) + geom_col() + 
  ylab("Povprečen znesek") + ggtitle("Povprečni zneski glede na tip")
#print(graf7.3)

graf7.3.2 <- ggplot(znesek, aes(x= tip, y=znesek, fill = tip)) + 
  geom_boxplot(outlier.color = "blue") + scale_fill_brewer(palette = "Dark2") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("Graf kvantilov zneskov po tipih")
#print(graf7.3.2)


znesek.regija.povp <- znesek %>% group_by(regija) %>% summarise(povpr_prod = mean(znesek))

graf7.4 <- ggplot(znesek.regija.povp, aes(x = regija, y=povpr_prod, fill = regija)) + geom_col() + 
  ylab("Povprečen znesek") + ggtitle("Povprečni zneski glede na regijo")
#print(graf7.4)

graf7.4.2 <- ggplot(znesek, aes(x= regija, y=znesek, fill = regija)) + 
  geom_boxplot(outlier.color = "blue") + scale_fill_brewer(palette = "Dark2") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("Graf kvantilov zneskov po regijah")
#print(graf7.4.2)


znesek.posl.povp <- znesek %>% group_by(poslovalnica) %>% summarise(povpr_prod = mean(znesek))
graf7.5 <- ggplot(znesek.posl.povp, aes(x = poslovalnica, y=povpr_prod, fill = poslovalnica)) + geom_col() + 
  ylab("Povprečen znesek") + ggtitle("Povprečni zneski glede na poslovalnico")
#print(graf7.5)

graf7.5.2 <- ggplot(znesek, aes(x= poslovalnica, y=znesek, fill = poslovalnica)) + 
  geom_boxplot(outlier.color = "blue") + scale_fill_brewer(palette = "Dark2") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("Graf kvantilov zneskov po poslovalnicah")
#print(graf7.5.2)

