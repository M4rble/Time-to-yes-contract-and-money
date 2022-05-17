### VSI POMEMBNI GRAFI V ENEM SCRIPTU

source("~/Faks/mag 1 letnik/MzR/Time-to-yes-contract-and-money/uvoz/uvoz.R", encoding="UTF-8")

# PRODUKTI
# delež v celem letu in po mesecih
del.produktov <- as.data.frame(prop.table(table(podatki$produkt))*100)
delez.produktov <- rename(del.produktov, c("produkt" = "Var1", "delez" = "Freq"))

mes.del.prod <- subset(podatki, select = c(mesec, produkt))
mes.delez.produktov <- mes.del.prod %>% group_by(mesec) %>% count(produkt) %>% 
  mutate(mes_delez = round(n/sum(n) * 100,2))
df.jan <- data.frame("Jan",0,"avtomobilski", 0)
names(df.jan) <- c("mesec", "n", "produkt", "mes_delez")
mes.delez.produktov <- rbind(mes.delez.produktov, df.jan)
one.year <- c("Jan","Feb","Mar","Apr","Maj","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
mes.delez.produktov$mesec <- factor(mes.delez.produktov$mesec, levels=one.year)

delez.produktov2 <- left_join(delez.produktov, mes.delez.produktov)

graf1.1 <- ggplot(podatki, aes(y=produkt, fill =produkt)) +
           geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
           ylab("produkt") + xlab("število") +
           ggtitle("Število posameznih produktov v celem letu") + 
           theme(axis.text.x=element_text(angle=45, hjust=1))
#print(graf1.1)


graf1.2 <- ggplot(delez.produktov2, aes(x="", y=delez, fill =produkt)) +
  geom_col(width=0.7, position = position_dodge(width = 1)) + 
  geom_label(aes(x="", y = delez + 2, label = delez), 
             position = position_dodge(width = 1), show.legend = FALSE) + xlab("produkt") + 
  ggtitle("Delež posameznih produktov (v odstotkih) v celem letu")
#print(graf1.1)


graf1.3 <- ggplot(delez.produktov2, aes(x=mesec, y=mes_delez, group=produkt, colour=produkt)) + 
  geom_line() + geom_point() + ggtitle("Delež posameznih produktov (v odstotkih) po mesecih") +
  ylab("delež (v %)") + scale_colour_discrete(labels = c("avtomobilski", "hipotekarni",
                                                         "investicijski", "izobraževalni", "osebni", "startup", "študentski"))
#print(graf1.3)



# TIPI

graf3.1 <- ggplot(podatki, aes(y=tip, fill = tip)) +
           geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
           ylab("produkt") + xlab("število") +
           ggtitle("Število posameznih tipov v celem letu") + 
           theme(axis.text.x=element_text(angle=45, hjust=1))
#print(graf3.1)


del.tipov <- as.data.frame(prop.table(table(podatki$tip))*100)
delez.tipov <- rename(del.tipov, c("tip" = "Var1", "delez" = "Freq"))

graf3.2 <- ggplot(delez.tipov, aes(x="", y=delez, fill =tip)) +
           geom_col(width=0.7, position = position_dodge(width = 1)) + 
           geom_label(aes(x="", y = delez + 3, label = delez), 
             position = position_dodge(width = 1), show.legend = FALSE) + xlab("tip") + 
           ggtitle("Delež posameznih tipov (v odstotkih) v celem letu") + ylab("delež (v %)")
#print(graf3.2)

mes.del.tip <- subset(podatki, select = c(mesec, tip))
mes.delez.tipov <- mes.del.tip %>% group_by(mesec) %>% count(tip) %>% 
  summarise(delez = n/sum(n) * 100, tip)
mes.delez.tipov$mesec <- factor(mes.delez.tipov$mesec, levels=one.year)

graf3.3 <- ggplot(mes.delez.tipov, aes(x=mesec, y=delez, group=tip, colour=tip)) + 
  geom_line() + geom_point() + ggtitle("Delež posameznih tipov (v odstotkih) po mesecih") +
  ylab("delež (v %)")
#print(graf3.3)

prod.tip <- subset(podatki, select = c(produkt, tip))
prod.del.tip <- prod.tip %>% group_by(produkt) %>% count(tip) %>% 
  summarise(delez = n/sum(n) * 100, tip)

graf3.4 <- ggplot(prod.del.tip, aes(x=produkt, y=delez, fill=tip)) + 
  geom_col(position = position_dodge(width = 0.9)) +
  ggtitle("Delež posameznih tipov znotraj posameznega produkta") + xlab("produkt") + 
  ylab("delež (v %") + theme(axis.text.x=element_text(angle=45, hjust=1))
#print(graf3.4)



# MESECI

mes.del.prod$mesec <- factor(mes.del.prod$mesec, levels=one.year)
mesec.produkti <- mes.del.prod %>% group_by(mesec) %>% count(produkt) %>% 
  mutate(delez = round(n/sum(n) * 100,2))
mesec.produkti <- rename(mesec.produkti, "stevilo" = "n")

graf4.1 <- ggplot(mesec.produkti, aes(x=mesec, y=stevilo, group=produkt, colour=produkt)) + 
  geom_line() + geom_point() + ggtitle("Število posameznih produktov po mesecih") +
  ylab("število") + scale_colour_discrete(labels = c("avtomobilski", "hipotekarni",
                                                     "investicijski", "izobraževalni", "osebni", "startup", "študentski"))
#print(graf4.1)

mesec.produkti.skupaj <- mesec.produkti %>% summarise(vseh = sum(stevilo))
graf4.2 <- ggplot(mesec.produkti.skupaj, aes(x=mesec, y=vseh, group=1)) + 
  geom_smooth() + geom_point() + ggtitle("Število vseh produktov po mesecih") +
  ylab("število")
#print(graf4.2)

mesec.povp <- mean(mesec.produkti.skupaj$vseh)
mesec.med <- median(mesec.produkti.skupaj$vseh)

graf4.3 <- graf4.2 + geom_line(aes(y=mesec.povp, colour="Povprečje")) + 
  geom_line(aes(y=mesec.med, colour = "Mediana")) +
  scale_colour_manual("", breaks = c("Povprečje", "Mediana"),
                      values = c("Povprečje"="red", "Mediana"="green"))
#print(graf4.3)



# REGIJE
regija <- subset(podatki, select = c(produkt, mesec, tip, regija, poslovalnica))

graf5.1 <- ggplot(regija, aes(y=produkt, fill = regija)) +
  geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
  ggtitle("Število posameznih produktov v posamezni regiji") + 
  xlab("število") + theme(axis.text.x=element_text(angle=45, hjust=1))
#print(graf5.1)

prod.regija <- regija %>% group_by(regija) %>% count(produkt, mesec)
prod.regija <- rename(prod.regija, "st_prod" = "n")

tip.regija <- regija %>% group_by(regija) %>% count(tip, mesec)
tip.regija <- rename(tip.regija, "st_tip" = "n")

regije.skupaj <- prod.regija %>% summarise(vseh = sum(st_prod))
vzhodna.prod <- prod.regija %>% filter(regija == "vzhodna")

vzhodna.prod$mesec <- factor(vzhodna.prod$mesec, labels = one.year)
graf5.2 <- ggplot(vzhodna.prod, aes(x=mesec, y=st_prod, group=produkt, colour=produkt)) + 
  geom_line() + geom_point() + ggtitle("Število posameznih produktov v vzhodni regiji po mesecih") +
  ylab("število") + scale_colour_discrete(labels = c("avtomobilski", "hipotekarni",
                                                     "investicijski", "izobraževalni", "osebni", "startup", "študentski"))
#print(graf5.2)

zahodna.prod <- prod.regija %>% filter(regija == "zahodna")
zahodna.prod$mesec <- factor(zahodna.prod$mesec, labels = one.year)
graf5.3 <- ggplot(zahodna.prod, aes(x=mesec, y=st_prod, group=produkt, colour=produkt)) + 
  geom_line() + geom_point() + ggtitle("Število posameznih produktov v zahodni regiji po mesecih") +
  ylab("število")
#print(graf5.3)

vzhodna.tip <- tip.regija %>% filter(regija == "vzhodna")
vzhodna.tip$mesec <- factor(vzhodna.tip$mesec, labels = one.year)
graf5.4 <- ggplot(vzhodna.tip, aes(x=mesec, y=st_tip, group=tip, colour=tip)) + 
  geom_line() + geom_point() + ggtitle("Število posameznih tipov v vzhodni regiji po mesecih") +
  ylab("število")
#print(graf5.4)

zahodna.tip <- tip.regija %>% filter(regija == "zahodna")
zahodna.tip$mesec <- factor(zahodna.tip$mesec, labels = one.year)
graf5.5 <- ggplot(zahodna.tip, aes(x=mesec, y=st_tip, group=tip, colour=tip)) + 
  geom_line() + geom_point() + ggtitle("Število posameznih tipov v zahodni regiji po mesecih") +
  ylab("število")
#print(graf5.5)



# POSLOVALNICA
posl.regija <- regija %>% group_by(regija) %>% count(poslovalnica)
posl.regija <- rename(posl.regija, "st_poslov" = "n")

graf6.1 <- ggplot(regija, aes(y=poslovalnica, fill = regija)) +
  geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
  ggtitle("Število obdelanih vlog v posamezni poslovalnici") + 
  xlab("število")
#print(graf6.1)

posl.mesec <- regija %>% group_by(mesec) %>% count(poslovalnica)
posl.mesec <- rename(posl.mesec, "st_poslov" = "n")
posl.mesec$mesec <- factor(posl.mesec$mesec, levels = one.year)

graf6.2 <- ggplot(posl.mesec, aes(x=mesec, y=st_poslov, group=poslovalnica, colour=poslovalnica)) + 
  geom_line() + geom_point() + ggtitle("Število poslov v posamezni posovalnici po mesecih") +
  ylab("število") 
#print(graf6.2)

graf6.3 <- ggplot(regija, aes(y=poslovalnica, fill = produkt)) +
  geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
  ggtitle("Število posameznih produktov v posamezni poslovalnici") + 
  xlab("število") 
#print(graf6.3)

graf6.4 <- ggplot(regija, aes(y=poslovalnica, fill = tip)) +
  geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
  ggtitle("Število posameznih tipov v posamezni poslovalnici") + 
  xlab("število")
#print(graf6.4)



# ZNESEK
znesek <- subset(podatki, select = c(produkt, mesec, tip, znesek, regija, poslovalnica))
znesek$mesec <- factor(znesek$mesec, levels = one.year)

# znesek-produkt
znesek.prod.povp <- znesek %>% group_by(produkt) %>% summarise(povpr_prod = mean(znesek))

graf7.1 <- ggplot(znesek.prod.povp, aes(x = produkt, y=povpr_prod, fill = produkt)) + geom_col() + 
  ylab("Povprečen znesek") + ggtitle("Povprečni zneski glede na produkt") + scale_fill_brewer("Accent")
#print(graf7.1)

graf7.2 <- ggplot(znesek, aes(x= produkt, y=znesek, fill = produkt)) + 
  geom_boxplot(outlier.color = "blue") + scale_fill_brewer(palette = "Dark2") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("Graf kvantilov zneskov po produktih") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
#print(graf7.2)

znesek.mesec.povp <- znesek %>% group_by(mesec) %>% summarise(povpr_prod = mean(znesek))
graf7.3 <- ggplot(znesek.mesec.povp, aes(x=mesec, y=povpr_prod, group=1)) + 
  geom_smooth() + geom_point() + geom_line() + 
  ggtitle("Povprečen znesek po mesecih") +  ylab("povprečen znesek")
#print(graf7.3)
#conf int = 0.95

graf7.3.2 <- graf7.3 + geom_line(aes(y=mean(povpr_prod), colour="Povprečje")) + 
  geom_line(aes(y=median(povpr_prod), colour = "Mediana")) +
  scale_colour_manual("", breaks = c("Povprečje", "Mediana"),
                      values = c("Povprečje"="red", "Mediana"="green"))
#print(graf7.3.2)

graf7.4 <- ggplot(znesek, aes(x= mesec, y=znesek, fill = mesec)) + 
  geom_boxplot(outlier.color = "blue") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("Graf kvantilov zneskov po mesecih")
#print(graf7.4)


znesek.tip.povp <- znesek %>% group_by(tip) %>% summarise(povpr_prod = round(mean(znesek),2))

graf7.5 <- ggplot(znesek.tip.povp, aes(x = tip, y=povpr_prod, fill = tip)) + geom_col() + 
  geom_label(aes(x=tip, y = povpr_prod + 4, label = povpr_prod), 
             position = position_dodge(width = 1), show.legend = FALSE) +
  ylab("Povprečen znesek") + ggtitle("Povprečni zneski glede na tip")
#print(graf7.5)

graf7.6 <- ggplot(znesek, aes(x= tip, y=znesek, fill = tip)) + 
  geom_boxplot(outlier.color = "blue") + scale_fill_brewer(palette = "Dark2") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("Graf kvantilov zneskov po tipih")
#print(graf7.6)


znesek.regija.povp <- znesek %>% group_by(regija) %>% summarise(povpr_prod = round(mean(znesek),2))

graf7.7 <- ggplot(znesek.regija.povp, aes(x = regija, y=povpr_prod, fill = regija)) + geom_col() + 
  geom_label(aes(x=regija, y = povpr_prod + 4, label = povpr_prod), 
             position = position_dodge(width = 1), show.legend = FALSE) +
  ylab("Povprečen znesek") + ggtitle("Povprečni zneski glede na regijo")
#print(graf7.7)

graf7.8 <- ggplot(znesek, aes(x= regija, y=znesek, fill = regija)) + 
  geom_boxplot(outlier.color = "blue") + scale_fill_brewer(palette = "Dark2") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("Graf kvantilov zneskov po regijah")
#print(graf7.8)


znesek.posl.povp <- znesek %>% group_by(poslovalnica) %>% summarise(povpr_prod = round(mean(znesek),2))

graf7.9 <- ggplot(znesek.posl.povp, aes(x = poslovalnica, y=povpr_prod, fill = poslovalnica)) + geom_col() + 
  geom_label(aes(x=poslovalnica, y = povpr_prod + 5, label = povpr_prod), 
             position = position_dodge(width = 1), show.legend = FALSE) +
  ylab("Povprečen znesek") + ggtitle("Povprečni zneski glede na poslovalnico")
#print(graf7.9)

graf7.10 <- ggplot(znesek, aes(x= poslovalnica, y=znesek, fill = poslovalnica)) + 
  geom_boxplot(outlier.color = "blue") + scale_fill_brewer(palette = "Dark2") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("Graf kvantilov zneskov po poslovalnicah")
#print(graf7.10)

graf7.11 <- ggplot(podatki, aes(x=znesek)) + geom_histogram(color = "black", fill="white", binwidth=5) +
  ggtitle("Histogram zneskov") + xlab("znesek") + ylab("število")
#print(graf7.11)

graf7.12 <- ggplot(podatki, aes(x=znesek)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=5) + 
  geom_density(alpha=0.2, color = "red", size = 1.3, fill = "red") +
  ggtitle("Histogram in gostota zneskov") + xlab("znesek") + ylab("število")
#print(graf7.12)


# ČASI
# ==========


# histogrami

graf8.1 <- ggplot(podatki, aes(x=TTY)) + geom_histogram(color = "black", fill="white", binwidth=1) +
  ggtitle("Histogram časov do odobritve") + xlab("čas") + ylab("število")
#print(graf16.1)

graf8.2 <- ggplot(podatki, aes(x=TTY)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=1) + 
  geom_density(alpha=0.2, color = "red", size = 1.3, fill = "red") + 
  ggtitle("Histogram in gostota časov do odobritve") + xlab("čas") + ylab("število")
#print(graf16.2)

graf8.3 <- ggplot(podatki, aes(x=TTC)) + geom_histogram(color = "black", fill="white", binwidth=1) + 
  ggtitle("Histogram časov do pogodbe") + xlab("čas") + ylab("število")
#print(graf16.3)

graf8.4 <- ggplot(podatki, aes(x=TTC)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=1) + 
  geom_density(alpha=0.2, color = "red", size = 1.3, fill="red") + 
  ggtitle("Histogram in gostota časov do pogodbe") + xlab("čas") + ylab("število")
#print(graf16.4)

graf8.5 <- ggplot(podatki, aes(x=TTM)) + geom_histogram(color = "black", fill="white", binwidth=1) + 
  ggtitle("Histogram časov do prejema denarja") + xlab("čas") + ylab("število")
#print(graf16.5)

graf8.6<- ggplot(podatki, aes(x=TTM)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=1) + 
  geom_density(alpha=0.2, color = "red", size = 1.3, fill = "red") +
  ggtitle("Histogram in gostota časov do prejema denarja") + xlab("čas") + ylab("število")
#print(graf16.6)



# glede na produkt

casi.produkt <- podatki %>% select(TTY,TTC,TTM,produkt)
casi.produkt <- casi.produkt %>%  pivot_longer(!produkt, names_to="TTi", values_to = "cas")
casi.produkt <- within(casi.produkt, TTi <- factor(TTi, levels=c("TTY", "TTC", "TTM")) )
casi.produkt <- casi.produkt %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))

casi.produkt.plt <- ggplot(casi.produkt, aes(x=produkt, y=cas, fill=produkt)) + 
  geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
  geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=0.8, lty=3) + 
  geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=0.8, lty=3) +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
  scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                    values = palette("Dark2")) + 
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(casi.produkt.plt)

casi.produkt.rac <- podatki %>% summarise(produkt, TTY, TTC-TTY, TTM-TTC)
casi.produkt.rac <- casi.produkt.rac %>% pivot_longer(!produkt, names_to = "TTi", values_to = "cas")
casi.produkt.rac <- within(casi.produkt.rac, TTi <- factor(TTi, levels = c("TTY", "TTC - TTY", "TTM - TTC")))
casi.produkt.rac <- casi.produkt.rac %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))

casi.produkt.rac.plt <- ggplot(casi.produkt.rac, aes(x=produkt, y=cas, fill=produkt)) + 
  facet_wrap(vars(TTi)) +  geom_boxplot(outlier.colour = "blue") + 
  geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=0.8, lty=3) + 
  geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=0.8, lty=3) +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
  scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                    values = palette("Dark2")) + 
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(casi.produkt.rac.plt)


# glede na mesec

casi.mesec <- podatki %>% select(TTY,TTC,TTM,mesec)
casi.mesec <- casi.mesec %>%  pivot_longer(!mesec, names_to="TTi", values_to = "cas")
casi.mesec <- within(casi.mesec, TTi <- factor(TTi, levels=c("TTY", "TTC", "TTM")) )
casi.mesec <- casi.mesec %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))

casi.mesec.plt <- ggplot(casi.mesec, aes(x=mesec, y=cas, fill=mesec)) + 
  geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
  geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=0.8, lty=3) + 
  geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=0.8, lty=3) +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  ggtitle("Grafi kvantilov časov po mesecih") + ylab("Čas (v dnevih)") +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(casi.mesec.plt)

casi.mesec.rac <- podatki %>% summarise(mesec, TTY, TTC-TTY, TTM-TTC)
casi.mesec.rac <- casi.mesec.rac %>% pivot_longer(!mesec, names_to = "TTi", values_to = "cas")
casi.mesec.rac <- within(casi.mesec.rac, TTi <- factor(TTi, levels = c("TTY", "TTC - TTY", "TTM - TTC")))
casi.mesec.rac <- casi.mesec.rac %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))

casi.mesec.rac.plt <- ggplot(casi.mesec.rac, aes(x=mesec, y=cas, fill=mesec)) + 
  facet_wrap(vars(TTi)) +  geom_boxplot(outlier.colour = "blue") + 
  geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=0.8, lty=3) + 
  geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=0.8, lty=3) +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  ggtitle("Grafi kvantilov časov po mesecih") + ylab("Čas (v dnevih)") +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(casi.mesec.rac.plt)


#glede na tip

casi.tip <- podatki %>% select(TTY,TTC,TTM,tip)
casi.tip <- casi.tip %>%  pivot_longer(!tip, names_to="TTi", values_to = "cas")
casi.tip <- within(casi.tip, TTi <- factor(TTi, levels=c("TTY", "TTC", "TTM")) )
casi.tip <- casi.tip %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))

casi.tip.plt <- ggplot(casi.tip, aes(x=tip, y=cas, fill=tip)) + 
  geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
  geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=0.8, lty=3) + 
  geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=0.8, lty=3) +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  ggtitle("Grafi kvantilov časov po tipih") + ylab("Čas (v dnevih)") +
  scale_fill_manual(values = palette("Dark2")) + 
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(casi.tip.plt)

casi.tip.rac <- podatki %>% summarise(tip, TTY, TTC-TTY, TTM-TTC)
casi.tip.rac <- casi.tip.rac %>% pivot_longer(!tip, names_to = "TTi", values_to = "cas")
casi.tip.rac <- within(casi.tip.rac, TTi <- factor(TTi, levels = c("TTY", "TTC - TTY", "TTM - TTC")))
casi.tip.rac <- casi.tip.rac %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))

casi.tip.rac.plt <- ggplot(casi.tip.rac, aes(x=tip, y=cas, fill=tip)) + 
  facet_wrap(vars(TTi)) +  geom_boxplot(outlier.colour = "blue") + 
  geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=0.8, lty=3) + 
  geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=0.8, lty=3) +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  ggtitle("Grafi kvantilov racionaliziranih časov glede na TTY po tipih") + ylab("Čas (v dnevih)") +
  scale_fill_manual(values = palette("Dark2")) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(casi.tip.rac.plt)


#glede na regijo

casi.regija <- podatki %>% select(TTY,TTC,TTM,regija)
casi.regija <- casi.regija %>%  pivot_longer(!regija, names_to="TTi", values_to = "cas")
casi.regija <- within(casi.regija, TTi <- factor(TTi, levels=c("TTY", "TTC", "TTM")) )
casi.regija <- casi.regija %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))

casi.regija.plt <- ggplot(casi.regija, aes(x=regija, y=cas, fill=regija)) + 
  geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
  geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=0.8, lty=3) + 
  geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=0.8, lty=3) +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  ggtitle("Grafi kvantilov časov po regijah") + ylab("Čas (v dnevih)") +
  scale_fill_manual(values = palette("Dark2")) + 
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(casi.regija.plt)

casi.regija.rac <- podatki %>% summarise(regija, TTY, TTC-TTY, TTM-TTC)
casi.regija.rac <- casi.regija.rac %>% pivot_longer(!regija, names_to = "TTi", values_to = "cas")
casi.regija.rac <- within(casi.regija.rac, TTi <- factor(TTi, levels = c("TTY", "TTC - TTY", "TTM - TTC")))
casi.regija.rac <- casi.regija.rac %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))

casi.regija.rac.plt <- ggplot(casi.regija.rac, aes(x=regija, y=cas, fill=regija)) + 
  facet_wrap(vars(TTi)) +  geom_boxplot(outlier.colour = "blue") + 
  geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=0.8, lty=3) + 
  geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=0.8, lty=3) +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  ggtitle("Grafi kvantilov racionaliziranih časov glede na TTY po regijah") + ylab("Čas (v dnevih)") +
  scale_fill_manual(values = palette("Dark2")) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(casi.regija.rac.plt)


# glede na poslovalnico

casi.poslovalnica <- podatki %>% select(TTY,TTC,TTM,poslovalnica)
casi.poslovalnica <- casi.poslovalnica %>%  pivot_longer(!poslovalnica, names_to="TTi", values_to = "cas")
casi.poslovalnica <- within(casi.poslovalnica, TTi <- factor(TTi, levels=c("TTY", "TTC", "TTM")) )
casi.poslovalnica <- casi.poslovalnica %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))

casi.poslovalnica.plt <- ggplot(casi.poslovalnica, aes(x=poslovalnica, y=cas, fill=poslovalnica)) + 
  geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
  geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=0.8, lty=3) + 
  geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=0.8, lty=3) +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  ggtitle("Grafi kvantilov časov po poslovalnicah") + ylab("Čas (v dnevih)") +
  scale_fill_manual(values = palette("Dark2")) + 
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(casi.poslovalnica.plt)

casi.poslovalnica.rac <- podatki %>% summarise(poslovalnica, TTY, TTC-TTY, TTM-TTC)
casi.poslovalnica.rac <- casi.poslovalnica.rac %>% pivot_longer(!poslovalnica, names_to = "TTi", values_to = "cas")
casi.poslovalnica.rac <- within(casi.poslovalnica.rac, TTi <- factor(TTi, levels = c("TTY", "TTC - TTY", "TTM - TTC")))
casi.poslovalnica.rac <- casi.poslovalnica.rac %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))

casi.poslovalnica.rac.plt <- ggplot(casi.poslovalnica.rac, aes(x=poslovalnica, y=cas, fill=poslovalnica)) + 
  facet_wrap(vars(TTi)) +  geom_boxplot(outlier.colour = "blue") + 
  geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=0.8, lty=3) + 
  geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=0.8, lty=3) +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  ggtitle("Grafi kvantilov racionaliziranih časov glede na TTY po poslovalnicah") + ylab("Čas (v dnevih)") +
  scale_fill_manual(values = palette("Dark2")) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(casi.poslovalnica.rac.plt)



