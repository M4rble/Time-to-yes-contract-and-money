# modeliranje in vizualizacija TTY, TTC in TTM

source("~/Faks/mag 1 letnik/MzR/Time-to-yes-contract-and-money/vizualizacija/vizualizacija1.R", encoding = "UTF-8")

# Boxploti za times vs produkt
require(gridExtra)

a <- min(min(podatki$TTY, min(podatki$TTC), min(podatki$TTM)))
b <- max(max(podatki$TTY, max(podatki$TTC), max(podatki$TTM)))

graf8.1 <- ggplot(podatki, aes(x= produkt, y=TTY, fill = produkt)) + 
  geom_boxplot(outlier.color = "blue") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTY") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                    values = palette("Dark2")) + scale_y_continuous(limits = c(a,b))
#print(graf8.1)

graf8.1.2 <- graf8.1 + geom_line(aes(y=mean(TTY), group=TTY, colour="Povprečje vseh"), lwd=1.3) + 
  geom_line(aes(y=median(TTY), group=TTY, colour = "Mediana vseh"), lwd = 1) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="green", "Mediana vseh"="black"))
#print(graf8.1.2)


graf8.2 <- ggplot(podatki, aes(x= produkt, y=TTC, fill = produkt)) + 
  geom_boxplot(outlier.color = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTC") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                    values = palette("Dark2"))  + scale_y_continuous(limits = c(a,b))
#print(graf8.2)

graf8.2.2 <- graf8.2 + geom_line(aes(y=mean(TTC), group=TTC, colour="Povprečje vseh"), lwd=1.3) + 
  geom_line(aes(y=median(TTC), group=TTC, colour = "Mediana vseh"), lwd = 1) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="green", "Mediana vseh"="black"))
#print(graf8.2.2)


graf8.3 <- ggplot(podatki, aes(x= produkt, y=TTM, fill = produkt)) + 
  geom_boxplot(outlier.color = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTM") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                    values = palette("Dark2"))  + scale_y_continuous(limits = c(a,b))
#print(graf8.3)

graf8.3.2 <- graf8.3 + geom_line(aes(y=mean(TTM), group=TTM, colour="Povprečje vseh"), lwd=1.3) + 
  geom_line(aes(y=median(TTM), group=TTM, colour = "Mediana vseh"), lwd = 1) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="green", "Mediana vseh"="black"))
#print(graf8.3.2)

casi.produkt <- ggarrange(graf8.1.2, graf8.2.2, graf8.3.2, ncol=3, nrow=1, common.legend = TRUE, legend = "bottom")
casi.produkt <- annotate_figure(casi.produkt, top = text_grob("Grafi kvantilov časov po produktih", 
                                                            color = "blue", face = "bold", size = 14))
#print(casi.produkt)


# times vs mesec
graf9.1 <- ggplot(podatki, aes(x= mesec, y=TTY, fill = mesec)) + 
  geom_boxplot(outlier.color = "blue") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTY") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_y_continuous(limits = c(a,b))
#print(graf9.1)

graf9.1.2 <- graf9.1 + geom_line(aes(y=mean(TTY), group=TTY, colour="Povprečje vseh"), lwd = 1.3) + 
  geom_line(aes(y=median(TTY), group=TTY, colour = "Mediana vseh"), lwd =1) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="green", "Mediana vseh"="black"))
#print(graf9.1.2)


graf9.2 <- ggplot(podatki, aes(x= mesec, y=TTC, fill = mesec)) + 
  geom_boxplot(outlier.color = "blue") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTC") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  scale_y_continuous(limits = c(a,b))
#print(graf9.2)

graf9.2.2 <- graf9.2 + geom_line(aes(y=mean(TTC), group=TTC, colour="Povprečje vseh"), lwd=1.3) + 
  geom_line(aes(y=median(TTC), group=TTC, colour = "Mediana vseh"), lwd=1) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="green", "Mediana vseh"="black"))
#print(graf9.2.2)


graf9.3 <- ggplot(podatki, aes(x= mesec, y=TTM, fill = mesec)) + 
  geom_boxplot(outlier.color = "blue") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTM") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  scale_y_continuous(limits = c(a,b))
#print(graf9.3)

graf9.3.2 <- graf9.3 + geom_line(aes(y=mean(TTM), group=TTM, colour="Povprečje vseh"), lwd=1.3) + 
  geom_line(aes(y=median(TTM), group=TTM, colour = "Mediana vseh"), lwd=1) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="green", "Mediana vseh"="black"))
#print(graf9.3.2)

casi.mesec <- ggarrange(graf9.1.2, graf9.2.2, graf9.3.2, ncol=3, nrow=1, common.legend = TRUE, legend = "bottom")
casi.mesec <- annotate_figure(casi.mesec, top = text_grob("Grafi kvantilov časov po mesecih", 
                                                              color = "blue", face = "bold", size = 14))
#print(casi.mesec)


# times vs tip 

graf10.1 <- ggplot(podatki, aes(x= tip, y=TTY, fill = tip)) + 
  geom_boxplot(outlier.color = "blue") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTY") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2")) + scale_y_continuous(limits = c(a,b))
#print(graf10.1)

graf10.1.2 <- graf10.1 + geom_line(aes(y=mean(TTY), group=TTY, colour="Povprečje vseh"), lwd=1.3) + 
  geom_line(aes(y=median(TTY), group=TTY, colour = "Mediana vseh"), lwd = 1) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="green", "Mediana vseh"="black"))
#print(graf10.1.2)


graf10.2 <- ggplot(podatki, aes(x= tip, y=TTC, fill = tip)) + 
  geom_boxplot(outlier.color = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTC") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))  + scale_y_continuous(limits = c(a,b))
#print(graf10.2)

graf10.2.2 <- graf10.2 + geom_line(aes(y=mean(TTC), group=TTC, colour="Povprečje vseh"), lwd=1.3) + 
  geom_line(aes(y=median(TTC), group=TTC, colour = "Mediana vseh"), lwd = 1) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="green", "Mediana vseh"="black"))
#print(graf10.2.2)


graf10.3 <- ggplot(podatki, aes(x= tip, y=TTM, fill = tip)) + 
  geom_boxplot(outlier.color = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTM") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))  + scale_y_continuous(limits = c(a,b))
#print(graf10.3)

graf10.3.2 <- graf10.3 + geom_line(aes(y=mean(TTM), group=TTM, colour="Povprečje vseh"), lwd=1.3) + 
  geom_line(aes(y=median(TTM), group=TTM, colour = "Mediana vseh"), lwd = 1) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="green", "Mediana vseh"="black"))
#print(graf10.3.2)

casi.tip <- ggarrange(graf10.1.2, graf10.2.2, graf10.3.2, ncol=3, nrow=1, common.legend = TRUE, legend = "bottom")
casi.tip <- annotate_figure(casi.tip, top = text_grob("Grafi kvantilov časov po tipih", 
                                                              color = "blue", face = "bold", size = 14))
#print(casi.tip)


# time vs regija

graf11.1 <- ggplot(podatki, aes(x= regija, y=TTY, fill = regija)) + 
  geom_boxplot(outlier.color = "blue") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTY") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2")) + scale_y_continuous(limits = c(a,b))
#print(graf11.1)

graf11.1.2 <- graf11.1 + geom_line(aes(y=mean(TTY), group=TTY, colour="Povprečje vseh"), lwd=1.3) + 
  geom_line(aes(y=median(TTY), group=TTY, colour = "Mediana vseh"), lwd = 1) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="green", "Mediana vseh"="black"))
#print(graf11.1.2)


graf11.2 <- ggplot(podatki, aes(x= regija, y=TTC, fill = regija)) + 
  geom_boxplot(outlier.color = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTC") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))  + scale_y_continuous(limits = c(a,b))
#print(graf11.2)

graf11.2.2 <- graf11.2 + geom_line(aes(y=mean(TTC), group=TTC, colour="Povprečje vseh"), lwd=1.3) + 
  geom_line(aes(y=median(TTC), group=TTC, colour = "Mediana vseh"), lwd = 1) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="green", "Mediana vseh"="black"))
#print(graf11.2.2)


graf11.3 <- ggplot(podatki, aes(x= regija, y=TTM, fill = regija)) + 
  geom_boxplot(outlier.color = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTM") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))  + scale_y_continuous(limits = c(a,b))
#print(graf11.3)

graf11.3.2 <- graf11.3 + geom_line(aes(y=mean(TTM), group=TTM, colour="Povprečje vseh"), lwd=1.3) + 
  geom_line(aes(y=median(TTM), group=TTM, colour = "Mediana vseh"), lwd = 1) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="green", "Mediana vseh"="black"))
#print(graf11.3.2)

casi.regija <- ggarrange(graf11.1.2, graf11.2.2, graf11.3.2, ncol=3, nrow=1, common.legend = TRUE, legend = "bottom")
casi.regija <- annotate_figure(casi.regija, top = text_grob("Grafi kvantilov časov po regijah", 
                                                              color = "blue", face = "bold", size = 14))
#print(casi.regija)


# time vs poslovalnica

graf12.1 <- ggplot(podatki, aes(x= poslovalnica, y=TTY, fill = poslovalnica)) + 
  geom_boxplot(outlier.color = "blue") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTY") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2")) + scale_y_continuous(limits = c(a,b))
#print(graf12.1)

graf12.1.2 <- graf12.1 + geom_line(aes(y=mean(TTY), group=TTY, colour="Povprečje vseh"), lwd=1.3) + 
  geom_line(aes(y=median(TTY), group=TTY, colour = "Mediana vseh"), lwd = 1) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="green", "Mediana vseh"="black"))
#print(graf12.1.2)


graf12.2 <- ggplot(podatki, aes(x= poslovalnica, y=TTC, fill = poslovalnica)) + 
  geom_boxplot(outlier.color = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTC") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))  + scale_y_continuous(limits = c(a,b))
#print(graf12.2)

graf12.2.2 <- graf12.2 + geom_line(aes(y=mean(TTC), group=TTC, colour="Povprečje vseh"), lwd=1.3) + 
  geom_line(aes(y=median(TTC), group=TTC, colour = "Mediana vseh"), lwd = 1) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="green", "Mediana vseh"="black"))
#print(graf12.2.2)


graf12.3 <- ggplot(podatki, aes(x= poslovalnica, y=TTM, fill = poslovalnica)) + 
  geom_boxplot(outlier.color = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTM") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))  + scale_y_continuous(limits = c(a,b))
#print(graf12.3)

graf12.3.2 <- graf12.3 + geom_line(aes(y=mean(TTM), group=TTM, colour="Povprečje vseh"), lwd=1.3) + 
  geom_line(aes(y=median(TTM), group=TTM, colour = "Mediana vseh"), lwd = 1) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="green", "Mediana vseh"="black"))
#print(graf12.3.2)

casi.poslovalnica <- ggarrange(graf12.1.2, graf12.2.2, graf12.3.2, ncol=3, nrow=1, common.legend = TRUE, legend = "bottom")
casi.poslovalnica <- annotate_figure(casi.poslovalnica, top = text_grob("Grafi kvantilov časov po poslovalnicah", 
                                                              color = "blue", face = "bold", size = 14))
#print(casi.poslovalnica)


# korelacije times - znesek
graf13.1 <- ggplot(podatki, aes(x=TTY, y=znesek)) + geom_point() + 
           ggtitle("Korelacija med TTY in zneskom")
#print(graf13.1)
graf13.2 <- ggplot(podatki, aes(x=TTC, y=znesek)) + geom_point() +
           ggtitle("Korelacija med TTC in zneskom")
#print(graf13.2)
graf13.3 <- ggplot(podatki, aes(x=TTM, y=znesek)) + geom_point() + 
           ggtitle("Korelacija med TTM in zneskom")
#print(graf13.3)

# korelacija times - poslovalnica

graf14.1 <- ggplot(podatki, aes(x=poslovalnica, y=mean(TTY))) + geom_col() + 
  ggtitle("Korelacija med TTY in poslovalnico")
#print(graf14.1)
# povprečje glede na število obdelanih znsekov?
poslovalnica.avg <- podatki %>% group_by(poslovalnica) %>% summarise(povprecje_TTY = mean(TTY))

# avtokorelacija itd - ne deluje zares, ker ni časovna vrsta
#acf(podatki$TTY)
#pacf(podatki$TTY)
#ccf(podatki$TTY, podatki$znesek)


# povprečni na mesec
mesec.times <- podatki %>% select(mesec, TTY, TTC, TTM)
mesec.avg <- mesec.times %>% pivot_longer(!mesec, names_to = "čas", values_to = "dolžina") %>% 
             group_by(mesec, čas) %>% summarise(povprecje = round(mean(dolžina),2), med = round(median(dolžina),2))
mesec.avg$mesec <- factor(mesec.avg$mesec, levels = one.year)

graf15.1 <- ggplot(mesec.avg, aes(x=mesec, y=povprecje, group=čas, color = čas)) + 
  geom_line(size=1.3) + geom_point(size=1.3) + ggtitle("Povprečje in mediana posameznega časa glede na mesec") +
  ylab("čas") + geom_line(aes(y=med, group=čas)) + geom_point(aes(y=med, group=čas), shape=17)
#print(graf15.1)


graf16.1.1 <- ggplot(podatki, aes(x=TTY)) + geom_histogram(color = "black", fill="white", binwidth=1) +
              ggtitle("Histogram časov do odobritve") + xlab("čas") + ylab("število")
print(graf16.1.1)

graf16.1.2 <- ggplot(podatki, aes(x=TTY)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=1) + 
              geom_density(alpha=0.2, color = "red", size = 1.3, fill = "red") + 
              ggtitle("Histogram in gostota časov do odobritve") + xlab("čas") + ylab("število")
print(graf16.1.2)


graf16.2.1 <- ggplot(podatki, aes(x=TTC)) + geom_histogram(color = "black", fill="white", binwidth=1) + 
              ggtitle("Histogram časov do pogodbe") + xlab("čas") + ylab("število")
print(graf16.2.1)

graf16.2.2 <- ggplot(podatki, aes(x=TTC)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=1) + 
              geom_density(alpha=0.2, color = "red", size = 1.3, fill="red") + 
              ggtitle("Histogram in gostota časov do pogodbe") + xlab("čas") + ylab("število")
print(graf16.2.2)


graf16.3.1 <- ggplot(podatki, aes(x=TTM)) + geom_histogram(color = "black", fill="white", binwidth=1) + 
              ggtitle("Histogram časov do prejema denarja") + xlab("čas") + ylab("število")
print(graf16.3.1)

graf16.3.2 <- ggplot(podatki, aes(x=TTM)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=1) + 
              geom_density(alpha=0.2, color = "red", size = 1.3, fill = "red") +
              ggtitle("Histogram in gostota časov do prejema denarja") + xlab("čas") + ylab("število")
print(graf16.3.2)


graf16.4.1 <- ggplot(podatki, aes(x=znesek)) + geom_histogram(color = "black", fill="white", binwidth=5) +
              ggtitle("Histogram zneskov") + xlab("znesek") + ylab("število")
print(graf16.4.1)

graf16.4.2 <- ggplot(podatki, aes(x=znesek)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=5) + 
  geom_density(alpha=0.2, color = "red", size = 1.3, fill = "red") +
  ggtitle("Histogram in gostota zneskov") + xlab("znesek") + ylab("število")
print(graf16.4.2)
