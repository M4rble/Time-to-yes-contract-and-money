## Napoved trendov s stronjim učenjem

# glavne ideje:
# - ustvarjanje novih atributov
# - PAZI NA ID! - vsi z istim ID so 1 podatek - dodaj nove stolpce kot recimo število kreditov (in koliko katerih), 0 in 1
# - učno in testno množico je treba ustrezno razdeliti - VSI z istim ID v eno ali drugo!!
# - KNN, random forests, lin regression, decision trees? Probaj vse


# za osamelce - SVM (metoda podpornih vektorjev - za odkrivanje osamelcev)


source("~/Faks/mag 1 letnik/MzR/Time-to-yes-contract-and-money/vizualizacija/vizualizacija2.R", encoding = "UTF-8")

podatki2 <- podatki %>% group_by(ID) %>% count(ID)
podatki2 <- rename(podatki2, "st_poslov" = "n")

podatki.ml <- left_join(podatki, podatki2)
podatki.ml <- podatki.ml %>% group_by(ID) %>% mutate(skupni_znesek = sum(znesek)) %>% ungroup() %>% 
              mutate(povpr_znesek = round(skupni_znesek/st_poslov,2)) %>% get_dummies.(c(produkt,mesec,tip,regija)) %>%
              select(-c(produkt, mesec, tip, regija))
podatki.ml$ID <- as.numeric(podatki.ml$ID)
podatki.ml$poslovalnica <- as.numeric(podatki.ml$poslovalnica)


podatki.TTY <- podatki.ml %>% select(-c(TTC,TTM))
# korelacijska matrika
cor_TTY <- round(cor(podatki.TTY),2)
cor_TTY2 <- rcorr(as.matrix(podatki.TTY))
#print(cor_TTY2)
symnum(cor_TTY)
corrplot(cor_TTY, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


podatki.TTC <- podatki.ml %>% select(-c(TTY,TTM))
# korelacijska matrika
cor_TTC <- round(cor(podatki.TTC),2)
cor_TTC2 <- rcorr(as.matrix(podatki.TTC))
#print(cor_TTC2)
symnum(cor_TTC)
corrplot(cor_TTC, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


podatki.TTM <- podatki.ml %>% select(-c(TTC,TTY))
# korelacijska matrika
cor_TTM <- round(cor(podatki.TTM),2)
cor_TTM2 <- rcorr(as.matrix(podatki.TTM))
#print(cor_TTM2)
symnum(cor_TTM)
corrplot(cor_TTM, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

