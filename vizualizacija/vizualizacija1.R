# vizualizacija - vse brez časov

source("~/Faks/mag 1 letnik/MzR/Time-to-yes-contract-and-money/uvoz/uvoz.R")

# prvo po atributih


# PRODUKTI
# delež v celem letu in po mesecih
del.produktov <- as.data.frame(prop.table(table(podatki$produkt))*100)
delez.produktov <- rename(del.produktov, c("produkt" = "Var1", "delez" = "Freq"))

mes.del.prod <- subset(podatki, select = c(mesec, produkt))
mes.delez.produktov <- mes.del.prod %>% group_by(mesec) %>% count(produkt) %>% 
                        summarise(delez = n/sum(n) * 100, produkt)

# TIPI
# delež v celem letu in po mesecih
del.tipov <- as.data.frame(prop.table(table(podatki$tip))*100)
delez.tipov <- rename(del.tipov, c("tip" = "Var1", "delez" = "Freq"))

mes.del.tip <- subset(podatki, select = c(mesec, tip))
mes.delez.tipov <- mes.del.tip %>% group_by(mesec) %>% count(tip) %>% 
  summarise(delez = n/sum(n) * 100, tip)

prod.tip <- subset(podatki, select = c(produkt, tip))
prod.del.tip <- prod.tip %>% group_by(produkt) %>% count(tip) %>% 
  summarise(delez = n/sum(n) * 100, tip)


#MESEC
mesec.produkti <- mes.del.prod %>% group_by(mesec) %>% count(produkt)
mesec.produkti <- rename(mesec.produkti, "stevilo" = "n")

mesec.produkti.skupaj <- mesec.produkti %>% summarise(vseh = sum(stevilo))
paste("Največ produktov je bilo", mesec.produkti.skupaj[which.max(mesec.produkti.skupaj$vseh),1], "in sicer", 
      max(mesec.produkti.skupaj$vseh), "najmanj pa", mesec.produkti.skupaj[which.min(mesec.produkti.skupaj$vseh),1],
      "in sicer", min(mesec.produkti.skupaj$vseh))                   

mesec.povp <- mean(mesec.produkti.skupaj$vseh)
mesec.med <- median(mesec.produkti.skupaj$vseh)
mesec.max <- max(mesec.produkti.skupaj$vseh) 
mesec.min <- min(mesec.produkti.skupaj$vseh)
mesec.odst.gor <- mesec.max - mesec.povp
mesec.odst.dol <- mesec.povp - mesec.min


mesec.tipi <- mes.del.tip %>% group_by(mesec) %>% count(tip)
mesec.tipi <- rename(mesec.tipi, "stevilo" = "n")

mesec.tipi.skupaj <- mesec.tipi %>% summarise(vseh = sum(stevilo))
#paste("Največ tipov je bilo", mesec.tipi.skupaj[which.max(mesec.tipi.skupaj$vseh),1], "in sicer", max(mesec.tipi.skupaj$vseh),
#      "najmanj pa", mesec.tipi.skupaj[which.min(mesec.tipi.skupaj$vseh),1], "in sicer", min(mesec.tipi.skupaj$vseh))                   
# nima smisla, mora biti enako kot pri produktih

# REGIJA
regija <- subset(podatki, select = c(produkt, mesec, tip, regija, poslovalnica))
prod.regija <- regija %>% group_by(regija) %>% count(produkt, mesec, tip)
prod.regija <- rename(prod.regija, "st_prod" = "n")


regije.skupaj <- prod.regija %>% summarise(vseh = sum(stevilo))
paste("Več obelanih kreditov je v vzhodni regiji, kar je logično saj je tam tudi več poslovalnic")

vzhodna <- prod.regija %>% filter(regija == "vzhodna")
paste("Največ produktov v vzhodni regiji je", as.character(vzhodna[which.max(vzhodna$stevilo),2]), 
      "in sicer", as.character(vzhodna[which.max(vzhodna$stevilo),3]), "najmanj pa",
      as.character(vzhodna[which.min(vzhodna$stevilo),2]), "in sicer", as.character(vzhodna[which.min(vzhodna$stevilo),3]))

vzhodna.mesec.prod <- vzhodna %>% group_by(mesec) %>% count(produkt)
vzhodna.mesec.prod <- rename(vzhodna.mesec.prod, "st_prod" = "n")
vzhodna.mesec.tip <- vzhodna %>% group_by(mesec) %>% count(tip)
vzhodna.mesec.tip <- rename(vzhodna.mesec.tip, "st_prod" = "n")


zahodna <- prod.regija %>% filter(regija == "zahodna")
paste("Največ produktov v zahodni regiji je", as.character(zahodna[which.max(zahodna$stevilo),2]), 
      "in sicer", as.character(zahodna[which.max(zahodna$stevilo),3]), "najmanj pa",
      as.character(zahodna[which.min(zahodna$stevilo),2]), "in sicer", as.character(zahodna[which.min(zahodna$stevilo),3]))

zahodna.mesec.prod <- zahodna %>% group_by(mesec) %>% count(produkt)
zahodna.mesec.prod <- rename(zahodna.mesec.prod, "st_prod" = "n")
zahodna.mesec.tip <- zahodna %>% group_by(mesec) %>% count(tip)
zahodna.mesec.tip <- rename(zahodna.mesec.tip, "st_prod" = "n")


# POSLOVALNICA
posl.regija <- regija %>% group_by(regija) %>% count(poslovalnica)
posl.regija <- rename(posl.regija, "st_poslov" = "n")
paste("Največ poslov opravi", posl.regija[which.max(posl.regija$st_poslov), 2],"v regiji", 
      as.character(posl.regija[which.max(posl.regija$st_poslov), 1]), "in sicer", posl.regija[which.max(posl.regija$st_poslov), 3],
      "najmanj pa", posl.regija[which.min(posl.regija$st_poslov), 2], "v regiji", 
      as.character(posl.regija[which.min(posl.regija$st_poslov), 1]), "in sicer", posl.regija[which.min(posl.regija$st_poslov), 3])

posl.mesec.prod <- regija %>% group_by(mesec) %>% count(produkt, poslovalnica)
posl.mesec.prod <- rename(posl.mesec.prod, "st_poslov" = "n")
posl.mesec.tip <- regija %>% group_by(mesec) %>% count(tip, poslovalnica)
posl.mesec.tip <- rename(posl.mesec.tip, "st_poslov" = "n")


# ZNESEK
znesek <- subset(podatki, select = c(produkt, mesec, tip, znesek, regija, poslovalnica))

# znesek-produkt
znesek.prod.povp <- znesek %>% group_by(produkt) %>% summarise(povpr_prod = mean(znesek))
znesek.mesec.povp <- znesek %>% group_by(mesec) %>% summarise(povpr_prod = mean(znesek))
znesek.tip.povp <- znesek %>% group_by(tip) %>% summarise(povpr_prod = mean(znesek))
znesek.regija.povp <- znesek %>% group_by(regija) %>% summarise(povpr_prod = mean(znesek))
znesek.posl.povp <- znesek %>% group_by(poslovalnica) %>% summarise(povpr_prod = mean(znesek))
