# modeliranje in vizualizacija TTY, TTC in TTM

source("~/Faks/mag 1 letnik/MzR/Time-to-yes-contract-and-money/vizualizacija/vizualizacija1.R", encoding = "UTF-8")

# Boxploti za times vs produkt


graf8.1 <- ggplot(podatki, aes(x= produkt, y=TTY, fill = produkt)) + 
  geom_boxplot(outlier.color = "blue") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTY") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                    values = palette("Dark2"))
#print(graf8.1)

graf8.1.2 <- graf8.1 + geom_line(aes(y=mean(TTY), group=TTY, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTY), group=TTY, colour = "Mediana vseh"), lwd = 0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf8.1.2)


graf8.2 <- ggplot(podatki, aes(x= produkt, y=TTC, fill = produkt)) + 
  geom_boxplot(outlier.color = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTC") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                    values = palette("Dark2"))
#print(graf8.2)

graf8.2.2 <- graf8.2 + geom_line(aes(y=mean(TTC), group=TTC, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTC), group=TTC, colour = "Mediana vseh"), lwd = 0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf8.2.2)


graf8.2.3 <- ggplot(podatki, aes(x=produkt, y=TTC-TTY, fill = produkt)) + 
  geom_boxplot(outlier.colour = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTC - TTY") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                    values = palette("Dark2"))
#print(graf8.2.3)

graf8.2.4 <- graf8.2.3 + geom_line(aes(y=mean(TTC - TTY), group=TTC, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTC-TTY), group=TTC, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf8.2.4)

graf8.3 <- ggplot(podatki, aes(x= produkt, y=TTM, fill = produkt)) + 
  geom_boxplot(outlier.color = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTM") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                    values = palette("Dark2"))
#print(graf8.3)

graf8.3.2 <- graf8.3 + geom_line(aes(y=mean(TTM), group=TTM, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTM), group=TTM, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf8.3.2)

graf8.3.3 <- ggplot(podatki, aes(x=produkt, y=TTM-TTC, fill = produkt)) + 
  geom_boxplot(outlier.colour = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTM - TTC") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                    values = palette("Dark2"))
#print(graf8.3.3)

graf8.3.4 <- graf8.3.3 + geom_line(aes(y=mean(TTM - TTC), group=TTM, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTM-TTC), group=TTM, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf8.3.4)

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



# times vs mesec


graf9.1 <- ggplot(podatki, aes(x= mesec, y=TTY, fill = mesec)) + 
  geom_boxplot(outlier.color = "blue") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTY") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
#print(graf9.1)


graf9.1.2 <- graf9.1 + geom_line(aes(y=mean(TTY), group=TTY, colour="Povprečje vseh"), lwd = 0.8, lty=3) + 
  geom_line(aes(y=median(TTY), group=TTY, colour = "Mediana vseh"), lwd =0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf9.1.2)


graf9.2 <- ggplot(podatki, aes(x= mesec, y=TTC, fill = mesec)) + 
  geom_boxplot(outlier.color = "blue") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTC") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
#print(graf9.2)

graf9.2.2 <- graf9.2 + geom_line(aes(y=mean(TTC), group=TTC, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTC), group=TTC, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf9.2.2)

graf9.2.3 <- ggplot(podatki, aes(x=mesec, y=TTC-TTY, fill = mesec)) + 
  geom_boxplot(outlier.colour = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTC - TTY") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
#print(graf9.2.3)

graf9.2.4 <- graf9.2.3 + geom_line(aes(y=mean(TTC - TTY), group=TTC, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTC-TTY), group=TTC, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf9.2.4)


graf9.3 <- ggplot(podatki, aes(x= mesec, y=TTM, fill = mesec)) + 
  geom_boxplot(outlier.color = "blue") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTM") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
#print(graf9.3)

graf9.3.2 <- graf9.3 + geom_line(aes(y=mean(TTM), group=TTM, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTM), group=TTM, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf9.3.2)

graf9.3.3 <- ggplot(podatki, aes(x=mesec, y=TTM-TTC, fill = mesec)) + 
  geom_boxplot(outlier.colour = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTM - TTC") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
#print(graf9.3.3)

graf9.3.4 <- graf9.3.3 + geom_line(aes(y=mean(TTM - TTC), group=TTM, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTM-TTC), group=TTC, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf9.3.4)



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




# times vs tip 

graf10.1 <- ggplot(podatki, aes(x= tip, y=TTY, fill = tip)) + 
  geom_boxplot(outlier.color = "blue") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTY") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))
#print(graf10.1)

graf10.1.2 <- graf10.1 + geom_line(aes(y=mean(TTY), group=TTY, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTY), group=TTY, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf10.1.2)


graf10.2 <- ggplot(podatki, aes(x= tip, y=TTC, fill = tip)) + 
  geom_boxplot(outlier.color = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTC") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))
#print(graf10.2)

graf10.2.2 <- graf10.2 + geom_line(aes(y=mean(TTC), group=TTC, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTC), group=TTC, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf10.2.2)

graf10.2.3 <- ggplot(podatki, aes(x=tip, y=TTC-TTY, fill = tip)) + 
  geom_boxplot(outlier.colour = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTC - TTY") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))
#print(graf10.2.3)

graf10.2.4 <- graf10.2.3 + geom_line(aes(y=mean(TTC - TTY), group=TTC, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTC-TTY), group=TTC, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf10.2.4)


graf10.3 <- ggplot(podatki, aes(x= tip, y=TTM, fill = tip)) + 
  geom_boxplot(outlier.color = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTM") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))
#print(graf10.3)

graf10.3.2 <- graf10.3 + geom_line(aes(y=mean(TTM), group=TTM, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTM), group=TTM, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf10.3.2)

graf10.3.3 <- ggplot(podatki, aes(x=tip, y=TTM-TTC, fill = tip)) + 
  geom_boxplot(outlier.colour = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTM - TTC") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))
#print(graf10.3.3)

graf10.3.4 <- graf10.3.3 + geom_line(aes(y=mean(TTM - TTC), group=TTM, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTM-TTC), group=TTC, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf10.3.4)



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



# time vs regija

graf11.1 <- ggplot(podatki, aes(x= regija, y=TTY, fill = regija)) + 
  geom_boxplot(outlier.color = "blue") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTY") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))
#print(graf11.1)

graf11.1.2 <- graf11.1 + geom_line(aes(y=mean(TTY), group=TTY, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTY), group=TTY, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf11.1.2)


graf11.2 <- ggplot(podatki, aes(x= regija, y=TTC, fill = regija)) + 
  geom_boxplot(outlier.color = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTC") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))
#print(graf11.2)

graf11.2.2 <- graf11.2 + geom_line(aes(y=mean(TTC), group=TTC, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTC), group=TTC, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf11.2.2)

graf11.2.3 <- ggplot(podatki, aes(x=regija, y=TTC-TTY, fill = regija)) + 
  geom_boxplot(outlier.colour = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTC - TTY") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))
#print(graf11.2.3)

graf11.2.4 <- graf11.2.3 + geom_line(aes(y=mean(TTC - TTY), group=TTC, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTC-TTY), group=TTC, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf11.2.4)


graf11.3 <- ggplot(podatki, aes(x= regija, y=TTM, fill = regija)) + 
  geom_boxplot(outlier.color = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTM") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))  
#print(graf11.3)

graf11.3.2 <- graf11.3 + geom_line(aes(y=mean(TTM), group=TTM, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTM), group=TTM, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf11.3.2)

graf11.3.3 <- ggplot(podatki, aes(x=regija, y=TTM-TTC, fill = regija)) + 
  geom_boxplot(outlier.colour = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTM - TTC") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))
#print(graf11.3.3)

graf11.3.4 <- graf11.3.3 + geom_line(aes(y=mean(TTM - TTC), group=TTM, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTM-TTC), group=TTM, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf11.3.4)



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




# time vs poslovalnica

graf12.1 <- ggplot(podatki, aes(x= poslovalnica, y=TTY, fill = poslovalnica)) + 
  geom_boxplot(outlier.color = "blue") +
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTY") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))
#print(graf12.1)

graf12.1.2 <- graf12.1 + geom_line(aes(y=mean(TTY), group=TTY, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTY), group=TTY, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf12.1.2)


graf12.2 <- ggplot(podatki, aes(x= poslovalnica, y=TTC, fill = poslovalnica)) + 
  geom_boxplot(outlier.color = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTC") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))
#print(graf12.2)

graf12.2.2 <- graf12.2 + geom_line(aes(y=mean(TTC), group=TTC, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTC), group=TTC, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf12.2.2)

graf12.2.3 <- ggplot(podatki, aes(x=poslovalnica, y=TTC-TTY, fill = poslovalnica)) + 
  geom_boxplot(outlier.colour = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTC - TTY") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))
#print(graf12.2.3)

graf12.2.4 <- graf12.2.3 + geom_line(aes(y=mean(TTC - TTY), group=TTC, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTC-TTY), group=TTC, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf12.2.4)

graf12.3 <- ggplot(podatki, aes(x= poslovalnica, y=TTM, fill = poslovalnica)) + 
  geom_boxplot(outlier.color = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTM") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))
#print(graf12.3)

graf12.3.2 <- graf12.3 + geom_line(aes(y=mean(TTM), group=TTM, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTM), group=TTM, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf12.3.2)

graf12.3.3 <- ggplot(podatki, aes(x=poslovalnica, y=TTM-TTC, fill = poslovalnica)) + 
  geom_boxplot(outlier.colour = "blue") + 
  stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
  ggtitle("TTM - TTC") + ylab("Čas (v dnevih)") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  scale_fill_manual(values = palette("Dark2"))
#print(graf12.3.3)

graf12.3.4 <- graf12.3.3 + geom_line(aes(y=mean(TTM - TTC), group=TTM, colour="Povprečje vseh"), lwd=0.8, lty=3) + 
  geom_line(aes(y=median(TTM-TTC), group=TTM, colour = "Mediana vseh"), lwd=0.8, lty=3) +
  scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                      values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
#print(graf12.3.4)


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





# korelacije times - znesek
graf13.1 <- ggplot(podatki, aes(x=TTY, y=znesek, col=poslovalnica)) + geom_point() + geom_smooth() +
           ggtitle("Korelacija med TTY in zneskom") + facet_wrap(~produkt)
#print(graf13.1)
graf13.2 <- ggplot(podatki, aes(x=TTC, y=znesek)) + geom_point() +
           ggtitle("Korelacija med TTC in zneskom")
#print(graf13.2)
graf13.2.2 <- ggplot(podatki, aes(x=TTC-TTY, y=znesek)) + geom_point() +
  ggtitle("Korelacija med TTC-TTY in zneskom")
#print(graf13.2.2)
graf13.3 <- ggplot(podatki, aes(x=TTM, y=znesek)) + geom_point() + 
           ggtitle("Korelacija med TTM in zneskom")
#print(graf13.3)
graf13.3.2 <- ggplot(podatki, aes(x=TTM-TTC, y=znesek)) + geom_point() +
  ggtitle("Korelacija med TTM-TTC in zneskom")
#print(graf13.3.2)


# iskanje korelacij, med ostalimi atributi, ki bi vplivale na čase

#produkt-tip-znesek
graf14.1 <- ggplot(podatki, aes(x=TTY, y=znesek, col=tip)) + geom_point() + geom_smooth() +
  ggtitle("Korelacija med TTY, zneskom in tipom po produktih") + facet_wrap(~produkt)
#print(graf14.1)

#poslovalnica-regija-znesek
graf14.2 <- ggplot(podatki, aes(x=TTY, y=znesek, col=poslovalnica)) + geom_point() + geom_smooth() +
  ggtitle("Korelacija med TTY, zneskom in poslovalnico po regijah") + facet_wrap(~regija)
#print(graf14.2)

#mesec-poslovalnica-znesek
graf14.3 <- ggplot(podatki, aes(x=TTY, y=znesek, col=poslovalnica)) + geom_point() + geom_smooth() +
  ggtitle("Korelacija med TTY, zneskom in poslovalnico po mesecih") + facet_wrap(~mesec)
#print(graf14.3)

#produkt-regija-znesek
graf14.4 <- ggplot(podatki, aes(x=TTY, y=znesek, col=produkt)) + geom_point() + geom_smooth() +
  ggtitle("Korelacija med TTY, zneskom in produktom po regijah") + facet_wrap(~regija)
#print(graf14.4)

#produkt-poslovalnica-znesek
graf14.5 <- ggplot(podatki, aes(x=TTY, y=znesek, col=produkt)) + geom_point() + geom_smooth() +
  ggtitle("Korelacija med TTY, zneskom in produktom po poslovalnicah") + facet_wrap(~poslovalnica)
#print(graf14.5)

#produkt-tip-regija
graf14.6 <- ggplot(podatki, aes(x=produkt, y=TTY, col=tip)) + geom_point() + geom_smooth() +
  ggtitle("Korelacija med produktom, TTY in tipi po regijah") + facet_wrap(~regija)
#print(graf14.6)

#Ni NEKIH OČITNIH KORELACIJ :/

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
#print(graf16.1.1)

graf16.1.2 <- ggplot(podatki, aes(x=TTY)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=1) + 
              geom_density(alpha=0.2, color = "red", size = 1.3, fill = "red") + 
              ggtitle("Histogram in gostota časov do odobritve") + xlab("čas") + ylab("število")
#print(graf16.1.2)

graf16.1.3 <- ggplot(podatki, aes(x=log(TTY+1))) + geom_histogram(color = "black", fill="white", binwidth=0.1) +
  ggtitle("Histogram logaritmranih časov do odobritve") + xlab("čas") + ylab("število")
#print(graf16.1.3)


graf16.2.1 <- ggplot(podatki, aes(x=TTC)) + geom_histogram(color = "black", fill="white", binwidth=1) + 
              ggtitle("Histogram časov do pogodbe") + xlab("čas") + ylab("število")
#print(graf16.2.1)

graf16.2.2 <- ggplot(podatki, aes(x=TTC)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=1) + 
              geom_density(alpha=0.2, color = "red", size = 1.3, fill="red") + 
              ggtitle("Histogram in gostota časov do pogodbe") + xlab("čas") + ylab("število")
#print(graf16.2.2)

graf16.2.3 <- ggplot(podatki, aes(x=log(TTC+1))) + geom_histogram(color = "black", fill="white", binwidth=0.1) +
  ggtitle("Histogram logaritmiranih časov do pogodbe") + xlab("čas") + ylab("število")
#print(graf16.2.3)


graf16.3.1 <- ggplot(podatki, aes(x=TTM)) + geom_histogram(color = "black", fill="white", binwidth=1) + 
              ggtitle("Histogram časov do prejema denarja") + xlab("čas") + ylab("število")
#print(graf16.3.1)

graf16.3.2 <- ggplot(podatki, aes(x=TTM)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=1) + 
              geom_density(alpha=0.2, color = "red", size = 1.3, fill = "red") +
              ggtitle("Histogram in gostota časov do prejema denarja") + xlab("čas") + ylab("število")
#print(graf16.3.2)

graf16.3.3 <- ggplot(podatki, aes(x=log(TTM+1))) + geom_histogram(color = "black", fill="white", binwidth=0.1) +
  ggtitle("Histogram logaritmiranih časov do prejema denarja") + xlab("čas") + ylab("število")
#print(graf16.3.3)


graf16.4.1 <- ggplot(podatki, aes(x=znesek)) + geom_histogram(color = "black", fill="white", binwidth=5) +
              ggtitle("Histogram zneskov") + xlab("znesek") + ylab("število")
#print(graf16.4.1)

graf16.4.2 <- ggplot(podatki, aes(x=znesek)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=5) + 
  geom_density(alpha=0.2, color = "red", size = 1.3, fill = "red") +
  ggtitle("Histogram in gostota zneskov") + xlab("znesek") + ylab("število")
#print(graf16.4.2)

graf16.4.3 <- ggplot(podatki, aes(x=log(znesek))) + geom_histogram(color = "black", fill="white", binwidth=0.1) +
  ggtitle("Histogram logaritmranih zneskov") + xlab("znesek") + ylab("število")
#print(graf16.4.3)
