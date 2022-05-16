## Napoved trendov s stronjim učenjem

# glavne ideje:
# - ustvarjanje novih atributov
# - PAZI NA ID! - vsi z istim ID so 1 podatek - dodaj nove stolpce kot recimo število kreditov (in koliko katerih), 0 in 1
# - učno in testno množico je treba ustrezno razdeliti - VSI z istim ID v eno ali drugo!!
# - KNN, random forests, lin regression, decision trees, lmer? Probaj vse


# za osamelce - SVM (metoda podpornih vektorjev - za odkrivanje osamelcev)



# - scaling (done)
# - sin cos meseci - positional encoding (done)
# - st_poslov (done) in povpr_znesek po času (done)
# - log(časi)
# - učenje po mesecih do avg in test na sep - dec

source("~/Faks/mag 1 letnik/MzR/Time-to-yes-contract-and-money/vizualizacija/vizualizacija2.R", encoding = "UTF-8")

# meseci v sin-cos relacijo
podatki2 <- podatki
podatki2$mesec <- gsub("Maj", "May", podatki2$mesec)
podatki2$mesec_num <- match(podatki2$mesec, month.abb)
mesec_sincos <- podatki2 %>% select(-mesec) %>% 
  summarise(mesec_num, mesec_sin = format(round(sin((podatki2$mesec_num-1)*(2*pi/12)),6), scientific = FALSE),
            mesec_cos = format(round(cos((podatki2$mesec_num-1)*(2*pi/12)),6), scientific = FALSE))

# st_poslov in skupni_znesek po mesecih
podatki3 <- cbind(podatki, mesec_sincos)
podatki3 <- select(podatki3, -mesec_num)
podatki3$mesec <- factor(podatki3$mesec, levels = one.year)
podatki3$ID <- as.numeric(podatki3$ID)
st_poslov <- podatki3 %>% group_by(ID, mesec) %>% count(ID) %>% 
             ungroup() %>% group_by(ID) %>% mutate(st_poslov = cumsum(n)) %>% select(-n)
skupni_znesek <- podatki3 %>% group_by(ID, mesec) %>% 
                 summarise(skupni_znesek = sum(znesek)) %>% 
                 mutate(skupni_znesek = cumsum(skupni_znesek))
skupaj <- left_join(st_poslov, skupni_znesek)

podatki.2 <- left_join(podatki3, skupaj)

podatki.ml <- podatki.2 %>% group_by(ID) %>% mutate(povpr_znesek = round(skupni_znesek/st_poslov,2)) %>% 
              get_dummies.(c(produkt, tip)) %>% mutate(regija = ifelse(regija =="vzhodna",1,0)) %>%
              select(-c(produkt, mesec, tip))
podatki.ml$poslovalnica <- as.numeric(podatki.ml$poslovalnica)
podatki.ml$mesec_sin <- as.numeric(podatki.ml$mesec_sin)
podatki.ml$mesec_cos <- as.numeric(podatki.ml$mesec_cos)

id <- seq(1,nrow(podatki.ml))
podatki.ml <- cbind(id, podatki.ml)


# scaling
podatki.ml.scaled <- podatki.ml %>%  mutate_at(-c(1,3,4), funs(c(scale(.))))


podatki.TTY <- podatki.ml %>% select(-c(TTC,TTM))
podatki.TTY.sc <- podatki.ml.scaled %>% select(-c(TTC,TTM))




# prečno preverjanje in učenje
library(lme4)

napaka.cv.tty <- function(podatki_vsi, podatki_id, formula, k){
  set.seed(42)
  # za k-kratno prečno preverjanje najprej podatke razdelimo na k enako velikih delov

  # najprej naključno premešamo id-je
  r <- unique(sample(podatki_id))
  # razrežemo na k intervalov
  razrez <- cut(seq_along(r), k, labels = FALSE)
  # Razbijemo vektor na k seznamov na osnovi razreza intervalov
  razbitje <- split(r, razrez)
  # zdaj imamo dane indekse za vsakega od k-tih delov
  
  pp.napovedi <- rep(0, nrow(podatki_vsi))
  pp.napovedi2 <- rep(0, nrow(podatki_vsi))
  pp.napovedi3 <- rep(0, nrow(podatki_vsi))
  pp.napovedi4 <- rep(0, nrow(podatki_vsi))
  pp.napovedi5 <- rep(0, nrow(podatki_vsi))
  pp.napovedi6 <- rep(0, nrow(podatki_vsi))
  pp.napovedi7 <- rep(0, nrow(podatki_vsi))
  pp.napovedi8 <- rep(0, nrow(podatki_vsi))
  pp.napovedi9 <- rep(0, nrow(podatki_vsi))
  
  # prečno preverjanje
  for (i in 1:length(razbitje)){
    # učni podatki krediti neodvisni
    data <- podatki_vsi %>% mutate(contains = ID %in% razbitje[[i]])
    train.data <- data %>% filter(contains == FALSE) %>% select(-contains)
    # testni podatki krediti neodvisni
    test.data <- data %>% filter(contains == TRUE) %>% select(-contains)
    
    # učni podatki 1 id v 1 trainu
    train.data.2 <- data %>% group_by(ID) %>% sample_n(1) %>% 
      filter(contains == FALSE) %>% select(-contains)
    # testni podatki 1 id v 1 testu
    test.data.2 <- data %>% group_by(ID) %>% sample_n(1) %>%
      filter(contains == TRUE) %>% select(-contains)
    
    # naučimo model
    #mod.RF <- randomForest(TTY ~ ., data = train.data, mtry = 3, importance = TRUE, na.action = na.omit)
    mod.L <- lm(data = train.data[,-c(1,2)], formula = formula)
    mod.L2 <- lm(data = train.data.2[,-c(1,2)], formula = formula)
    mod.L3 <- lm(data = train.data[,-c(1,2)], 
                 formula = log(TTY+1)~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                 produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                 produkt_izobraževalni + produkt_osebni + produkt_startup +  povpr_znesek +
                 tip_Novo + tip_Obnova + tip_Podaljšanje)
    mod.lmer <- lmer(data = train.data[,-c(1,2)], 
                formula = TTY~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                produkt_izobraževalni + produkt_osebni + produkt_startup +  povpr_znesek +
                tip_Novo + tip_Obnova + tip_Podaljšanje + (znesek | regija))
    mod.lmer2 <- lmer(data = train.data.2[,-c(1,2)],
                 formula = TTY~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                 produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                 produkt_izobraževalni + produkt_osebni + produkt_startup +  povpr_znesek +
                 tip_Novo + tip_Obnova + tip_Podaljšanje + (znesek | regija))
    mod.glm <- glm(data = train.data[,-c(1,2)], 
                   formula = TTY~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                   produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                   produkt_izobraževalni + produkt_osebni + produkt_startup + povpr_znesek +
                   tip_Novo + tip_Obnova + tip_Podaljšanje + znesek:regija)
    mod.glm.log <- glm(data = train.data[,-c(1,2)], 
                   formula = TTY+1~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                   produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                   produkt_izobraževalni + produkt_osebni + produkt_startup + povpr_znesek + 
                   tip_Novo + tip_Obnova + tip_Podaljšanje, family = "gaussian"(link= "log"))
    mod.glm2 <- glm(data = train.data.2[,-c(1,2)],
                    formula = TTY~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                    produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                    produkt_izobraževalni + produkt_osebni + produkt_startup +  povpr_znesek +
                    tip_Novo + tip_Obnova + tip_Podaljšanje + znesek:regija)
    mod.glm.log2 <- glm(data = train.data.2[,-c(1,2)], 
                   formula = TTY+1~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                   produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                   produkt_izobraževalni + produkt_osebni + produkt_startup + povpr_znesek +
                   tip_Novo + tip_Obnova + tip_Podaljšanje, family = "gaussian"(link= "log"))
    
    # napovemo za testne podatke
    napovedi <- predict(mod.L, newdata = test.data[,-c(1,2)])
    napovedi2 <- predict(mod.L2, newdata = test.data.2[,-c(1,2)])
    napovedi3 <- predict(mod.L3, newdata = test.data[,-c(1,2)])
    napovedi4 <- predict(mod.lmer, newdata = test.data[,-c(1,2)])
    napovedi5 <- predict(mod.lmer2, newdata = test.data.2[,-c(1,2)])
    napovedi6 <- predict(mod.glm, newdata = test.data[,-c(1,2)])
    napovedi7 <- predict(mod.glm.log, newdata = test.data[,-c(1,2)])
    napovedi8 <- predict(mod.glm2, newdata = test.data.2[,-c(1,2)])
    napovedi9 <- predict(mod.glm.log2, newdata = test.data.2[,-c(1,2)])
    
    pp.napovedi[ test.data$id ] <- napovedi
    pp.napovedi2[ test.data.2$id ] <- napovedi2
    pp.napovedi3[test.data$id] <- exp(napovedi3)-1
    pp.napovedi4[ test.data$id ] <- napovedi4
    pp.napovedi5[ test.data.2$id ] <- napovedi5
    pp.napovedi6[ test.data$id ] <- napovedi6
    pp.napovedi7[ test.data$id ] <- exp(napovedi7)-1
    pp.napovedi8[ test.data.2$id ] <- napovedi8
    pp.napovedi9[ test.data.2$id ] <- exp(napovedi9)-1
    
  }
# izračunamo MSE
nenule1 <- which(pp.napovedi2 != 0)
pp.napovedi2 <- pp.napovedi2[nenule1]

nenule2 <- which(pp.napovedi5 != 0)
pp.napovedi5 <- pp.napovedi5[nenule2]

nenule3 <- which(pp.napovedi8 != 0)
pp.napovedi8 <- pp.napovedi8[nenule3]

nenule4 <- which(pp.napovedi9 != 0)
pp.napovedi9 <- pp.napovedi9[nenule4]
  
napaka = mean((pp.napovedi - podatki_vsi$TTY) ^ 2)
napaka2 = mean((pp.napovedi2 - podatki.TTY[id %in% nenule1]$TTY) ^ 2)
napaka3 = mean((pp.napovedi3 - podatki_vsi$TTY) ^ 2)
napaka4 = mean((pp.napovedi4 - podatki_vsi$TTY) ^ 2)
napaka5 = mean((pp.napovedi5 - podatki.TTY[id %in% nenule2]$TTY) ^ 2)
napaka6 = mean((pp.napovedi6 - podatki_vsi$TTY) ^ 2)
napaka7 = mean((pp.napovedi7 - podatki_vsi$TTY) ^ 2)
napaka8 = mean((pp.napovedi8 - podatki.TTY[id %in% nenule3]$TTY) ^ 2)
napaka9 = mean((pp.napovedi9 - podatki.TTY[id %in% nenule4]$TTY) ^ 2)

izvoz <- list("napaka.lm" = napaka, "napaka.lm2" = napaka2, "napaka.lm.log" = napaka3, 
              "napaka.lmer" = napaka4, "napaka.lmer2" = napaka5, "napaka.glm" = napaka6,
              "napaka.glm.log" = napaka7, "napaka.glm2" = napaka8, "napaka.glm.log2" = napaka9)

return(izvoz)
}



mod.L <- lm(data = train.data[,-c(1,2,18,22)], formula = TTY~.)
summary(mod.L)
cor(train.data[,-c(1,17,21)])
alias(mod.L)
napovedi <- predict(mod.L, newdata = test.data[,-c(1,2,6)])
# ugotovimo, da sta stolpca produkt_študentski in tip_Spremembra popolnoma korelirana - ju odstranimo iz modela

mod.L2 <- lm(data = train.data.2[,-c(1,2,18,22)], formula = TTY~.)
summary(mod.L2)
cor(train.data.2[,-c(1,2,18,22)])
alias(mod.L2)
# enako za drugi data set

mod.lmer <- lmer(data = train.data[,-c(1,2)], 
                 formula = TTY~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                   produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                   produkt_izobraževalni + produkt_osebni + produkt_startup + 
                   tip_Novo + tip_Obnova + tip_Podaljšanje + (0 + poslovalnica | regija))
summary(mod.lmer)

mod.lmer2 <- lmer(data = train.data[,-c(1,2)], 
                 formula = TTY~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                   produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                   produkt_izobraževalni + produkt_osebni + produkt_startup + 
                   tip_Novo + tip_Obnova + tip_Podaljšanje + (regija | znesek))
summary(mod.lmer2)

mod.glm.log <- glm(data = train.data[,-c(1,2)], 
                   formula = TTY+1~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                     produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                     produkt_izobraževalni + produkt_osebni + produkt_startup + povpr_znesek + 
                     tip_Novo + tip_Obnova + tip_Podaljšanje, family = "gaussian"(link= "log"))
summary(mod.glm.log)


# iskanje najboljšega modela po obeh pristopih
# najprej vključimo vse generirane spremenljivke
lin.mod.vsi <- napaka.cv(podatki.TTY[,-c(18,22)], podatki.TTY$ID, formula=TTY~., 10)
lin.mod.vsi$napaka.lm
lin.mod.vsi$napaka.lm2
lin.mod.vsi$napaka.lm.log
lin.mod.vsi$napaka.lmer
lin.mod.vsi$napaka.lmer2
lin.mod.vsi$napaka.glm
lin.mod.vsi$napaka.glm.log
lin.mod.vsi$napaka.glm2
lin.mod.vsi$napaka.glm.log2


#nobene od novo generiranih spremenljivk
lin.mod.nobene <- napaka.cv(podatki.TTY[,-c(9,10,11,18,22)], podatki.TTY$ID, formula=TTY~., 10)
lin.mod.nobene$napaka.lm
lin.mod.nobene$napaka.lm2
lin.mod.nobene$napaka.lm.log
lin.mod.nobene$napaka.lmer
lin.mod.nobene$napaka.lmer2
lin.mod.nobene$napaka.glm
lin.mod.nobene$napaka.glm.log
lin.mod.nobene$napaka.glm2
lin.mod.nobene$napaka.glm.log2
paste("Če ne vključimo nobene nove spremenljivke, je napaka manjša.")

#samo eno
lin.mod.eno1 <- napaka.cv(podatki.TTY[,-c(10,11,18,22)], podatki.TTY$ID, formula=TTY~., 10)
lin.mod.eno1$`napaka 1`
lin.mod.eno1$`napaka 2`
paste("Če samo st_poslov je napaka večja")

lin.mod.eno2 <- napaka.cv(podatki.TTY[,-c(9,11,18,22)], podatki.TTY$ID, formula=TTY~., 10)
lin.mod.eno2$`napaka 1`
lin.mod.eno2$`napaka 2`
paste("Če samo skupni znesek je napaka večja")

lin.mod.eno3 <- napaka.cv(podatki.TTY[,-c(9,10,18,22)], podatki.TTY$ID, formula=TTY~., 10)
lin.mod.eno3$napaka.lm
lin.mod.eno3$napaka.lm2
lin.mod.eno3$napaka.lm.log
lin.mod.eno3$napaka.lmer
lin.mod.eno3$napaka.lmer2
lin.mod.eno3$napaka.glm
lin.mod.eno3$napaka.glm.log
lin.mod.eno3$napaka.glm2
lin.mod.eno3$napaka.glm.log2
paste("Če samo povprečni znesek je napaka najmanjša do zdaj")

#po dve
lin.mod.dve1 <- napaka.cv(podatki.TTY[,-c(11,18,22)], podatki.TTY$ID, formula=TTY~., 10)
lin.mod.dve1$`napaka 1`
lin.mod.dve1$`napaka 2`

lin.mod.dve2 <- napaka.cv(podatki.TTY[,-c(10,18,22)], podatki.TTY$ID, formula=TTY~., 10)
lin.mod.dve2$`napaka 1`
lin.mod.dve2$`napaka 2`

lin.mod.dve3 <- napaka.cv(podatki.TTY[,-c(9,18,22)], podatki.TTY$ID, formula=TTY~., 10)
lin.mod.dve3$`napaka 1`
lin.mod.dve3$`napaka 2`

paste("Katerekoli dve vklkučimo je napaka večja.")
paste("Pri linearni regresiji z lm in modelom TTY glede na vse spremenljivke, je najbolje če od dodatnih vključimo samo povprečni znesek.")


paste("Izgleda, kot da je najboljši model za TTY mod.glm.log z vključeno dodatno spremenljivko povpr_znesek.")


# 1. PRISTOP - krediti znotraj istega id-ja so neodvisni
#=======================================================

k <- 10
r <- unique(sample(podatki.TTY$ID))
# razrežemo na k intervalov
razrez <- cut(seq_along(r), k, labels = FALSE)
# Razbijemo vektor na k seznamov na osnovi razreza intervalov
razbitje = split(r, razrez)
# zdaj imamo dane indekse za vsakega od k-tih delov

# prečno preverjanje
#FOR LOOP:  for (i in 1:length(razbitje)){
# učni podatki
data <- podatki.TTY %>% mutate(contains = ID %in% razbitje[[1]])
train.data <- data %>% filter(contains == FALSE) %>% select(-contains)
# testni podatki
test.data <- data %>% filter(contains == TRUE) %>% select(-contains)

test.data$ID %in% train.data$ID

TTY.RF <- randomForest(TTY ~ ., data = train.data, mtry = 3,
                       importance = TRUE, na.action = na.omit)
TTY.RF
plot(TTY.RF)

# Predicting the Test set results
y_pred = predict(TTY.RF, newdata = test.data[,-4])

pp.napovedi[ razbitje[[10]] ] <- y_pred



# 2. PRISTOP - znotraj enega train.seta max 1 posel na id
#========================================================

k <- 10
r2 <- unique(sample(podatki.TTY$ID))
# razrežemo na k intervalov
razrez <- cut(seq_along(r2), k, labels = FALSE)
# Razbijemo vektor na k seznamov na osnovi razreza intervalov
razbitje = split(r2, razrez)
# zdaj imamo dane indekse za vsakega od k-tih delov

# prečno preverjanje
#FOR LOOP:  for (i in 1:length(razbitje)){
# učni podatki
data2 <- podatki.TTY %>% mutate(contains = ID %in% razbitje[[1]])
train.data.2 <- data %>% group_by(ID) %>% sample_n(1) %>% 
              filter(contains == FALSE) %>% select(-contains)
# testni podatki
test.data.2 <- data %>% group_by(ID) %>% sample_n(1) %>%
              filter(contains == TRUE) %>% select(-contains)

test.data.2$ID %in% train.data.2$ID

length(unique(train.data.2$ID)) == 1038 - length(unique(test.data.2$ID))

#========================================================================

# Koliko oseb jemlje kredite v več različnih poslovalnicah
unique(podatki.TTY$ID)
podatki.TTY %>% filter(ID == 34)
id.posl <- podatki.TTY %>% select(ID, poslovalnica)
unikatni <- as.data.frame(table(unique(id.posl)$ID))
id.posl.2 <- unikatni %>% filter(Freq > 1)
id.posl.2$Var1 <- as.numeric(as.character(id.posl.2$Var1))
id.vec.posl.df <- podatki.TTY %>% group_by(ID) %>% 
  summarise(contains = ID %in% id.posl.2$Var1, poslovalnica) %>% filter(contains == TRUE) %>% 
  select(-contains)


podatki %>% filter(TTY == 0, tip == "Sprememba")
podatki %>% filter(TTY == 0, tip != "Sprememba")

podatki %>% filter(TTC == 0, tip == "Sprememba")
podatki %>% filter(TTC == 0, tip != "Sprememba")

podatki %>% filter(TTM == 0, tip == "Sprememba")
podatki %>% filter(TTM == 0, tip != "Sprememba")

podatki %>% filter(TTM == 0)

podatki %>% filter(TTC < TTY)
podatki %>% filter(TTM < TTY)



# korelacijska matrika
cor_TTY <- round(cor(podatki.TTY),2)
cor_TTY2 <- rcorr(as.matrix(podatki.TTY))
#print(cor_TTY2)
symnum(cor_TTY)
cor.TTY.plt <- corrplot(cor_TTY, type = "upper", order = "hclust", 
                        tl.col = "black", tl.srt = 45)
print(cor.TTY.plt)











# ŠE ENKRAT ISTO ZA TTC IN TTM
#=============================


# TTC
#====================================

podatki.TTC <- podatki.ml %>% select(-c(TTY,TTM))
# korelacijska matrika
cor_TTC <- round(cor(podatki.TTC),2)
cor_TTC2 <- rcorr(as.matrix(podatki.TTC))
#print(cor_TTC2)
symnum(cor_TTC)
corrplot(cor_TTC, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


mod.L.ttc <- lm(data = podatki.TTC[,-c(1,2,18,22)], formula = TTC~.)
summary(mod.L.ttc)
alias(mod.L.ttc)
# ugotovimo, da sta stolpca produkt_študentski in tip_Spremembra popolnoma korelirana - ju odstranimo iz modela


mod.lmer.ttc <- lmer(data = podatki.TTC[,-c(1,2)], 
                  formula = TTC~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                    produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                    produkt_izobraževalni + produkt_osebni + produkt_startup + 
                    tip_Novo + tip_Obnova + tip_Podaljšanje + (regija | znesek))
summary(mod.lmer.ttc)

mod.glm.log.ttc <- glm(data = podatki.TTC[,-c(1,2)], 
                   formula = TTC+1~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                     produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                     produkt_izobraževalni + produkt_osebni + produkt_startup + povpr_znesek + 
                     tip_Novo + tip_Obnova + tip_Podaljšanje, family = "gaussian"(link= "log"))
summary(mod.glm.log.ttc)


napaka.cv.ttc <- function(podatki_vsi, podatki_id, formula, k){
  set.seed(42)
  # za k-kratno prečno preverjanje najprej podatke razdelimo na k enako velikih delov
  
  # najprej naključno premešamo id-je
  r <- unique(sample(podatki_id))
  # razrežemo na k intervalov
  razrez <- cut(seq_along(r), k, labels = FALSE)
  # Razbijemo vektor na k seznamov na osnovi razreza intervalov
  razbitje <- split(r, razrez)
  # zdaj imamo dane indekse za vsakega od k-tih delov
  
  pp.napovedi <- rep(0, nrow(podatki_vsi))
  pp.napovedi2 <- rep(0, nrow(podatki_vsi))
  pp.napovedi3 <- rep(0, nrow(podatki_vsi))
  pp.napovedi4 <- rep(0, nrow(podatki_vsi))
  pp.napovedi5 <- rep(0, nrow(podatki_vsi))
  pp.napovedi6 <- rep(0, nrow(podatki_vsi))
  pp.napovedi7 <- rep(0, nrow(podatki_vsi))
  pp.napovedi8 <- rep(0, nrow(podatki_vsi))
  pp.napovedi9 <- rep(0, nrow(podatki_vsi))
  
  # prečno preverjanje
  for (i in 1:length(razbitje)){
    # učni podatki krediti neodvisni
    data <- podatki_vsi %>% mutate(contains = ID %in% razbitje[[i]])
    train.data <- data %>% filter(contains == FALSE) %>% select(-contains)
    # testni podatki krediti neodvisni
    test.data <- data %>% filter(contains == TRUE) %>% select(-contains)
    
    # učni podatki 1 id v 1 trainu
    train.data.2 <- data %>% group_by(ID) %>% sample_n(1) %>% 
      filter(contains == FALSE) %>% select(-contains)
    # testni podatki 1 id v 1 testu
    test.data.2 <- data %>% group_by(ID) %>% sample_n(1) %>%
      filter(contains == TRUE) %>% select(-contains)
    
    # naučimo model
    #mod.RF <- randomForest(TTY ~ ., data = train.data, mtry = 3, importance = TRUE, na.action = na.omit)
    mod.L <- lm(data = train.data[,-c(1,2)], formula = formula)
    mod.L2 <- lm(data = train.data.2[,-c(1,2)], formula = formula)
    mod.L3 <- lm(data = train.data[,-c(1,2)], 
                 formula = log(TTC+1)~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                   produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                   produkt_izobraževalni + produkt_osebni + produkt_startup +  povpr_znesek +
                   tip_Novo + tip_Obnova + tip_Podaljšanje)
    mod.lmer <- lmer(data = train.data[,-c(1,2)], 
                     formula = TTC~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                       produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                       produkt_izobraževalni + produkt_osebni + produkt_startup +  povpr_znesek +
                       tip_Novo + tip_Obnova + tip_Podaljšanje + (znesek | regija))
    mod.lmer2 <- lmer(data = train.data.2[,-c(1,2)],
                      formula = TTC~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                        produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                        produkt_izobraževalni + produkt_osebni + produkt_startup +  povpr_znesek +
                        tip_Novo + tip_Obnova + tip_Podaljšanje + (znesek | regija))
    mod.glm <- glm(data = train.data[,-c(1,2)], 
                   formula = TTC~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                     produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                     produkt_izobraževalni + produkt_osebni + produkt_startup + povpr_znesek +
                     tip_Novo + tip_Obnova + tip_Podaljšanje + znesek:regija)
    mod.glm.log <- glm(data = train.data[,-c(1,2)], 
                       formula = TTC+1~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                         produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                         produkt_izobraževalni + produkt_osebni + produkt_startup + povpr_znesek + 
                         tip_Novo + tip_Obnova + tip_Podaljšanje, family = "gaussian"(link= "log"))
    mod.glm2 <- glm(data = train.data.2[,-c(1,2)],
                    formula = TTC~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                      produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                      produkt_izobraževalni + produkt_osebni + produkt_startup +  povpr_znesek +
                      tip_Novo + tip_Obnova + tip_Podaljšanje + znesek:regija)
    mod.glm.log2 <- glm(data = train.data.2[,-c(1,2)], 
                        formula = TTC+1~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                          produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                          produkt_izobraževalni + produkt_osebni + produkt_startup + povpr_znesek +
                          tip_Novo + tip_Obnova + tip_Podaljšanje, family = "gaussian"(link= "log"))
    
    # napovemo za testne podatke
    napovedi <- predict(mod.L, newdata = test.data[,-c(1,2)])
    napovedi2 <- predict(mod.L2, newdata = test.data.2[,-c(1,2)])
    napovedi3 <- predict(mod.L3, newdata = test.data[,-c(1,2)])
    napovedi4 <- predict(mod.lmer, newdata = test.data[,-c(1,2)])
    napovedi5 <- predict(mod.lmer2, newdata = test.data.2[,-c(1,2)])
    napovedi6 <- predict(mod.glm, newdata = test.data[,-c(1,2)])
    napovedi7 <- predict(mod.glm.log, newdata = test.data[,-c(1,2)])
    napovedi8 <- predict(mod.glm2, newdata = test.data.2[,-c(1,2)])
    napovedi9 <- predict(mod.glm.log2, newdata = test.data.2[,-c(1,2)])
    
    pp.napovedi[ test.data$id ] <- napovedi
    pp.napovedi2[ test.data.2$id ] <- napovedi2
    pp.napovedi3[test.data$id] <- exp(napovedi3)-1
    pp.napovedi4[ test.data$id ] <- napovedi4
    pp.napovedi5[ test.data.2$id ] <- napovedi5
    pp.napovedi6[ test.data$id ] <- napovedi6
    pp.napovedi7[ test.data$id ] <- exp(napovedi7)-1
    pp.napovedi8[ test.data.2$id ] <- napovedi8
    pp.napovedi9[ test.data.2$id ] <- exp(napovedi9)-1
    
  }
  # izračunamo MSE
  nenule1 <- which(pp.napovedi2 != 0)
  pp.napovedi2 <- pp.napovedi2[nenule1]
  
  nenule2 <- which(pp.napovedi5 != 0)
  pp.napovedi5 <- pp.napovedi5[nenule2]
  
  nenule3 <- which(pp.napovedi8 != 0)
  pp.napovedi8 <- pp.napovedi8[nenule3]
  
  nenule4 <- which(pp.napovedi9 != 0)
  pp.napovedi9 <- pp.napovedi9[nenule4]
  
  napaka = mean((pp.napovedi - podatki_vsi$TTC) ^ 2)
  napaka2 = mean((pp.napovedi2 - podatki.TTC[id %in% nenule1]$TTC) ^ 2)
  napaka3 = mean((pp.napovedi3 - podatki_vsi$TTC) ^ 2)
  napaka4 = mean((pp.napovedi4 - podatki_vsi$TTC) ^ 2)
  napaka5 = mean((pp.napovedi5 - podatki.TTC[id %in% nenule2]$TTC) ^ 2)
  napaka6 = mean((pp.napovedi6 - podatki_vsi$TTC) ^ 2)
  napaka7 = mean((pp.napovedi7 - podatki_vsi$TTC) ^ 2)
  napaka8 = mean((pp.napovedi8 - podatki.TTC[id %in% nenule3]$TTC) ^ 2)
  napaka9 = mean((pp.napovedi9 - podatki.TTC[id %in% nenule4]$TTC) ^ 2)
  
  izvoz <- list("napaka.lm" = napaka, "napaka.lm2" = napaka2, "napaka.lm.log" = napaka3, 
                "napaka.lmer" = napaka4, "napaka.lmer2" = napaka5, "napaka.glm" = napaka6,
                "napaka.glm.log" = napaka7, "napaka.glm2" = napaka8, "napaka.glm.log2" = napaka9)
  
  return(izvoz)
}





# iskanje najboljšega modela po obeh pristopih
# najprej vključimo vse generirane spremenljivke
ttc.lin.mod.vsi <- napaka.cv.ttc(podatki.TTC[,-c(18,22)], podatki.TTC$ID, formula=TTC~., 10)
ttc.lin.mod.vsi$napaka.lm
ttc.lin.mod.vsi$napaka.lm2
ttc.lin.mod.vsi$napaka.lm.log
ttc.lin.mod.vsi$napaka.lmer
ttc.lin.mod.vsi$napaka.lmer2
ttc.lin.mod.vsi$napaka.glm
ttc.lin.mod.vsi$napaka.glm.log
ttc.lin.mod.vsi$napaka.glm2
ttc.lin.mod.vsi$napaka.glm.log2


#nobene od novo generiranih spremenljivk
ttc.lin.mod.nobene <- napaka.cv.ttc(podatki.TTC[,-c(9,10,11,18,22)], podatki.TTC$ID, formula=TTC~., 10)
ttc.lin.mod.nobene$napaka.lm
ttc.lin.mod.nobene$napaka.lm2
ttc.lin.mod.nobene$napaka.lm.log
ttc.lin.mod.nobene$napaka.lmer
ttc.lin.mod.nobene$napaka.lmer2
ttc.lin.mod.nobene$napaka.glm
ttc.lin.mod.nobene$napaka.glm.log
ttc.lin.mod.nobene$napaka.glm2
ttc.lin.mod.nobene$napaka.glm.log2
paste("Če ne vključimo nobene nove spremenljivke, je napaka manjša.")

#samo eno
ttc.lin.mod.eno1 <- napaka.cv.ttc(podatki.TTC[,-c(10,11,18,22)], podatki.TTC$ID, formula=TTC~., 10)
ttc.lin.mod.eno1$`napaka 1`
ttc.lin.mod.eno1$`napaka 2`
paste("Če samo st_poslov je napaka večja")

ttc.lin.mod.eno2 <- napaka.cv.ttc(podatki.TTC[,-c(9,11,18,22)], podatki.TTC$ID, formula=TTC~., 10)
ttc.lin.mod.eno2$`napaka 1`
ttc.lin.mod.eno2$`napaka 2`
paste("Če samo skupni znesek je napaka večja")

ttc.lin.mod.eno3 <- napaka.cv.ttc(podatki.TTC[,-c(9,10,18,22)], podatki.TTC$ID, formula=TTC~., 10)
ttc.lin.mod.eno3$napaka.lm
ttc.lin.mod.eno3$napaka.lm2
ttc.lin.mod.eno3$napaka.lm.log
ttc.lin.mod.eno3$napaka.lmer
ttc.lin.mod.eno3$napaka.lmer2
ttc.lin.mod.eno3$napaka.glm
ttc.lin.mod.eno3$napaka.glm.log
ttc.lin.mod.eno3$napaka.glm2
ttc.lin.mod.eno3$napaka.glm.log2
paste("Če samo povprečni znesek je napaka večja")

#po dve
ttc.lin.mod.dve1 <- napaka.cv.ttc(podatki.TTC[,-c(11,18,22)], podatki.TTC$ID, formula=TTC~., 10)
ttc.lin.mod.dve1$`napaka 1`
ttc.lin.mod.dve1$`napaka 2`

ttc.lin.mod.dve2 <- napaka.cv.ttc(podatki.TTC[,-c(10,18,22)], podatki.TTC$ID, formula=TTC~., 10)
ttc.lin.mod.dve2$`napaka 1`
ttc.lin.mod.dve2$`napaka 2`

ttc.lin.mod.dve3 <- napaka.cv.ttc(podatki.TTC[,-c(9,18,22)], podatki.TTC$ID, formula=TTC~., 10)
ttc.lin.mod.dve3$`napaka 1`
ttc.lin.mod.dve3$`napaka 2`

paste("Katerekoli dve vklkučimo je napaka večja.")
paste("Pri linearni regresiji z lm in modelom TTY glede na vse spremenljivke, je najbolje ne vključimo nobene dodatne spremenljivke.")


paste("Izgleda, kot da je najboljši model za TTY mod.glm.log brez vključenih dodatnih spremenljivk.")






# TTM
#====================================

podatki.TTM <- podatki.ml %>% select(-c(TTY,TTC))
# korelacijska matrika
cor_TTM <- round(cor(podatki.TTM),2)
cor_TTM2 <- rcorr(as.matrix(podatki.TTM))
#print(cor_TTM2)
symnum(cor_TTM)
corrplot(cor_TTM, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


mod.L.ttm <- lm(data = podatki.TTM[,-c(1,2,18,22)], formula = TTM~.)
summary(mod.L.ttm)
alias(mod.L.ttm)
# ugotovimo, da sta stolpca produkt_študentski in tip_Spremembra popolnoma korelirana - ju odstranimo iz modela


mod.lmer.ttm <- lmer(data = podatki.TTM[,-c(1,2)], 
                     formula = TTM~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                       produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                       produkt_izobraževalni + produkt_osebni + produkt_startup + 
                       tip_Novo + tip_Obnova + tip_Podaljšanje + (regija | znesek))
summary(mod.lmer.ttm)

mod.glm.log.ttm <- glm(data = podatki.TTM[,-c(1,2)], 
                       formula = TTM+1~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                         produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                         produkt_izobraževalni + produkt_osebni + produkt_startup + povpr_znesek + 
                         tip_Novo + tip_Obnova + tip_Podaljšanje, family = "gaussian"(link= "log"))
summary(mod.glm.log.ttm)


napaka.cv.ttm <- function(podatki_vsi, podatki_id, formula, k){
  set.seed(42)
  # za k-kratno prečno preverjanje najprej podatke razdelimo na k enako velikih delov
  
  # najprej naključno premešamo id-je
  r <- unique(sample(podatki_id))
  # razrežemo na k intervalov
  razrez <- cut(seq_along(r), k, labels = FALSE)
  # Razbijemo vektor na k seznamov na osnovi razreza intervalov
  razbitje <- split(r, razrez)
  # zdaj imamo dane indekse za vsakega od k-tih delov
  
  pp.napovedi <- rep(0, nrow(podatki_vsi))
  pp.napovedi2 <- rep(0, nrow(podatki_vsi))
  pp.napovedi3 <- rep(0, nrow(podatki_vsi))
  pp.napovedi4 <- rep(0, nrow(podatki_vsi))
  pp.napovedi5 <- rep(0, nrow(podatki_vsi))
  pp.napovedi6 <- rep(0, nrow(podatki_vsi))
  pp.napovedi7 <- rep(0, nrow(podatki_vsi))
  pp.napovedi8 <- rep(0, nrow(podatki_vsi))
  pp.napovedi9 <- rep(0, nrow(podatki_vsi))
  
  # prečno preverjanje
  for (i in 1:length(razbitje)){
    # učni podatki krediti neodvisni
    data <- podatki_vsi %>% mutate(contains = ID %in% razbitje[[i]])
    train.data <- data %>% filter(contains == FALSE) %>% select(-contains)
    # testni podatki krediti neodvisni
    test.data <- data %>% filter(contains == TRUE) %>% select(-contains)
    
    # učni podatki 1 id v 1 trainu
    train.data.2 <- data %>% group_by(ID) %>% sample_n(1) %>% 
      filter(contains == FALSE) %>% select(-contains)
    # testni podatki 1 id v 1 testu
    test.data.2 <- data %>% group_by(ID) %>% sample_n(1) %>%
      filter(contains == TRUE) %>% select(-contains)
    
    # naučimo model
    #mod.RF <- randomForest(TTY ~ ., data = train.data, mtry = 3, importance = TRUE, na.action = na.omit)
    mod.L <- lm(data = train.data[,-c(1,2)], formula = formula)
    mod.L2 <- lm(data = train.data.2[,-c(1,2)], formula = formula)
    mod.L3 <- lm(data = train.data[,-c(1,2)], 
                 formula = log(TTM)~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                   produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                   produkt_izobraževalni + produkt_osebni + produkt_startup +  
                   tip_Novo + tip_Obnova + tip_Podaljšanje)
    mod.lmer <- lmer(data = train.data[,-c(1,2)], 
                     formula = TTM~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                       produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                       produkt_izobraževalni + produkt_osebni + produkt_startup +  
                       tip_Novo + tip_Obnova + tip_Podaljšanje + (znesek | regija))
    mod.lmer2 <- lmer(data = train.data.2[,-c(1,2)],
                      formula = TTM~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                        produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                        produkt_izobraževalni + produkt_osebni + produkt_startup + 
                        tip_Novo + tip_Obnova + tip_Podaljšanje + (znesek | regija))
    mod.glm <- glm(data = train.data[,-c(1,2)], 
                   formula = TTM~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                     produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                     produkt_izobraževalni + produkt_osebni + produkt_startup + 
                     tip_Novo + tip_Obnova + tip_Podaljšanje + znesek:regija)
    mod.glm.log <- glm(data = train.data[,-c(1,2)], 
                       formula = TTM~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                         produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                         produkt_izobraževalni + produkt_osebni + produkt_startup + 
                         tip_Novo + tip_Obnova + tip_Podaljšanje, family = "gaussian"(link= "log"))
    mod.glm2 <- glm(data = train.data.2[,-c(1,2)],
                    formula = TTM~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                      produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                      produkt_izobraževalni + produkt_osebni + produkt_startup + 
                      tip_Novo + tip_Obnova + tip_Podaljšanje + znesek:regija)
    mod.glm.log2 <- glm(data = train.data.2[,-c(1,2)], 
                        formula = TTM~znesek + regija + poslovalnica + mesec_sin + mesec_cos + 
                          produkt_avtomobilski + produkt_hipotekarni + produkt_investicijski + 
                          produkt_izobraževalni + produkt_osebni + produkt_startup +
                          tip_Novo + tip_Obnova + tip_Podaljšanje, family = "gaussian"(link= "log"))
    
    # napovemo za testne podatke
    napovedi <- predict(mod.L, newdata = test.data[,-c(1,2)])
    napovedi2 <- predict(mod.L2, newdata = test.data.2[,-c(1,2)])
    napovedi3 <- predict(mod.L3, newdata = test.data[,-c(1,2)])
    napovedi4 <- predict(mod.lmer, newdata = test.data[,-c(1,2)])
    napovedi5 <- predict(mod.lmer2, newdata = test.data.2[,-c(1,2)])
    napovedi6 <- predict(mod.glm, newdata = test.data[,-c(1,2)])
    napovedi7 <- predict(mod.glm.log, newdata = test.data[,-c(1,2)])
    napovedi8 <- predict(mod.glm2, newdata = test.data.2[,-c(1,2)])
    napovedi9 <- predict(mod.glm.log2, newdata = test.data.2[,-c(1,2)])
    
    pp.napovedi[ test.data$id ] <- napovedi
    pp.napovedi2[ test.data.2$id ] <- napovedi2
    pp.napovedi3[test.data$id] <- exp(napovedi3)
    pp.napovedi4[ test.data$id ] <- napovedi4
    pp.napovedi5[ test.data.2$id ] <- napovedi5
    pp.napovedi6[ test.data$id ] <- napovedi6
    pp.napovedi7[ test.data$id ] <- exp(napovedi7)
    pp.napovedi8[ test.data.2$id ] <- napovedi8
    pp.napovedi9[ test.data.2$id ] <- exp(napovedi9)
    
  }
  # izračunamo MSE
  nenule1 <- which(pp.napovedi2 != 0)
  pp.napovedi2 <- pp.napovedi2[nenule1]
  
  nenule2 <- which(pp.napovedi5 != 0)
  pp.napovedi5 <- pp.napovedi5[nenule2]
  
  nenule3 <- which(pp.napovedi8 != 0)
  pp.napovedi8 <- pp.napovedi8[nenule3]
  
  nenule4 <- which(pp.napovedi9 != 0)
  pp.napovedi9 <- pp.napovedi9[nenule4]
  
  napaka = mean((pp.napovedi - podatki_vsi$TTM) ^ 2)
  napaka2 = mean((pp.napovedi2 - podatki.TTM[id %in% nenule1]$TTM) ^ 2)
  napaka3 = mean((pp.napovedi3 - podatki_vsi$TTM) ^ 2)
  napaka4 = mean((pp.napovedi4 - podatki_vsi$TTM) ^ 2)
  napaka5 = mean((pp.napovedi5 - podatki.TTM[id %in% nenule2]$TTM) ^ 2)
  napaka6 = mean((pp.napovedi6 - podatki_vsi$TTM) ^ 2)
  napaka7 = mean((pp.napovedi7 - podatki_vsi$TTM) ^ 2)
  napaka8 = mean((pp.napovedi8 - podatki.TTM[id %in% nenule3]$TTM) ^ 2)
  napaka9 = mean((pp.napovedi9 - podatki.TTM[id %in% nenule4]$TTM) ^ 2)
  
  izvoz <- list("napaka.lm" = napaka, "napaka.lm2" = napaka2, "napaka.lm.log" = napaka3, 
                "napaka.lmer" = napaka4, "napaka.lmer2" = napaka5, "napaka.glm" = napaka6,
                "napaka.glm.log" = napaka7, "napaka.glm2" = napaka8, "napaka.glm.log2" = napaka9)
  
  return(izvoz)
}





# iskanje najboljšega modela po obeh pristopih
# najprej vključimo vse generirane spremenljivke
ttm.lin.mod.vsi <- napaka.cv.ttm(podatki.TTM[,-c(18,22)], podatki.TTM$ID, formula=TTM~., 10)
ttm.lin.mod.vsi$napaka.lm
ttm.lin.mod.vsi$napaka.lm2
ttm.lin.mod.vsi$napaka.lm.log
ttm.lin.mod.vsi$napaka.lmer
ttm.lin.mod.vsi$napaka.lmer2
ttm.lin.mod.vsi$napaka.glm
ttm.lin.mod.vsi$napaka.glm.log
ttm.lin.mod.vsi$napaka.glm2
ttm.lin.mod.vsi$napaka.glm.log2


#nobene od novo generiranih spremenljivk
ttm.lin.mod.nobene <- napaka.cv.ttm(podatki.TTM[,-c(9,10,11,18,22)], podatki.TTM$ID, formula=TTM~., 10)
ttm.lin.mod.nobene$napaka.lm
ttm.lin.mod.nobene$napaka.lm2
ttm.lin.mod.nobene$napaka.lm.log
ttm.lin.mod.nobene$napaka.lmer
ttm.lin.mod.nobene$napaka.lmer2
ttm.lin.mod.nobene$napaka.glm
ttm.lin.mod.nobene$napaka.glm.log
ttm.lin.mod.nobene$napaka.glm2
ttm.lin.mod.nobene$napaka.glm.log2
paste("Če ne vključimo nobene nove spremenljivke, je napaka manjša.")

#samo eno
ttm.lin.mod.eno1 <- napaka.cv.ttm(podatki.TTM[,-c(10,11,18,22)], podatki.TTM$ID, formula=TTM~., 10)
ttm.lin.mod.eno1$`napaka 1`
ttm.lin.mod.eno1$`napaka 2`
paste("Če samo st_poslov je napaka večja")

ttm.lin.mod.eno2 <- napaka.cv.ttm(podatki.TTM[,-c(9,11,18,22)], podatki.TTM$ID, formula=TTM~., 10)
ttm.lin.mod.eno2$`napaka 1`
ttm.lin.mod.eno2$`napaka 2`
paste("Če samo skupni znesek je napaka večja")

ttm.lin.mod.eno3 <- napaka.cv.ttm(podatki.TTM[,-c(9,10,18,22)], podatki.TTM$ID, formula=TTM~., 10)
ttm.lin.mod.eno3$napaka.lm
ttm.lin.mod.eno3$napaka.lm2
ttm.lin.mod.eno3$napaka.lm.log
ttm.lin.mod.eno3$napaka.lmer
ttm.lin.mod.eno3$napaka.lmer2
ttm.lin.mod.eno3$napaka.glm
ttm.lin.mod.eno3$napaka.glm.log
ttm.lin.mod.eno3$napaka.glm2
ttm.lin.mod.eno3$napaka.glm.log2
paste("Če samo povprečni znesek je napaka večja")

#po dve
ttm.lin.mod.dve1 <- napaka.cv.ttm(podatki.TTM[,-c(11,18,22)], podatki.TTM$ID, formula=TTM~., 10)
ttm.lin.mod.dve1$`napaka 1`
ttm.lin.mod.dve1$`napaka 2`

ttm.lin.mod.dve2 <- napaka.cv.ttm(podatki.TTM[,-c(10,18,22)], podatki.TTM$ID, formula=TTM~., 10)
ttm.lin.mod.dve2$`napaka 1`
ttm.lin.mod.dve2$`napaka 2`

ttm.lin.mod.dve3 <- napaka.cv.ttm(podatki.TTM[,-c(9,18,22)], podatki.TTM$ID, formula=TTM~., 10)
ttm.lin.mod.dve3$`napaka 1`
ttm.lin.mod.dve3$`napaka 2`

paste("Katerekoli dve vklkučimo je napaka večja.")
paste("Pri linearni regresiji z lm in modelom TTY glede na vse spremenljivke, je najbolje ne vključimo nobene dodatne spremenljivke.")


paste("Izgleda, kot da je najboljši model za TTY mod.glm.log brez vključenih dodatnih spremenljivk.")

#mogoče bi bilo smiselno gledati modele na TTM-TTC in TTC-TTY ?

