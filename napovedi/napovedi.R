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

napaka.cv <- function(podatki_vsi, podatki_id, formula, k){
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
    #mod.lmer <- lmer(data = train.data[,-c(1,2)], formula = formula)
    #mod.lmer2 <- lmer(data = train.data.2[,-c(1,2)], formula = formula)
    #mod.glm <- glm(data = train.data[,-c(1,2)], formula = formula)
    #mod.glm2 <- glm(data = train.data.2[,-c(1,2)], formula = formula)
    # napovemo za testne podatke
    napovedi <- predict(mod.L, newdata = test.data[,-c(1,2)])
    napovedi2 <- predict(mod.L2, newdata = test.data.2[,-c(1,2)])
    #napovedi3 <- predict(mod.lmer, newdata = test.data[,-c(1,2)])
    #napovedi4 <- predict(mod.lmer2, newdata = test.data.2[,-c(1,2)])
    #napovedi5 <- predict(mod.glm, newdata = test.data[,-c(1,2)])
    #napovedi6 <- predict(mod.glm2, newdata = test.data.2[,-c(1,2)])
    
    pp.napovedi[ test.data$id ] <- napovedi
    pp.napovedi2[ test.data.2$id ] <- napovedi2
    #pp.napovedi3[ test.data$id ] <- napovedi3
    #pp.napovedi4[ test.data.2$id ] <- napovedi4
    #pp.napovedi5[ test.data$id ] <- napovedi5
    #pp.napovedi6[ test.data.2$id ] <- napovedi6
    
  }
# izračunamo MSE
nenule1 <- which(pp.napovedi2 != 0)
pp.napovedi2 <- pp.napovedi2[nenule1]

#nenule2 <- which(pp.napovedi4 != 0)
#pp.napovedi4 <- pp.napovedi4[nenule2]
#
#nenule3 <- which(pp.napovedi6 != 0)
#pp.napovedi6 <- pp.napovedi6[nenule3]
  
napaka = mean((pp.napovedi - podatki_vsi$TTY) ^ 2)
napaka2 = mean((pp.napovedi2 - podatki.TTY[id %in% nenule1]$TTY) ^ 2)
#napaka3 = mean((pp.napovedi3 - podatki_vsi$TTY) ^ 2)
#napaka4 = mean((pp.napovedi4 - podatki.TTY[id %in% nenule2]$TTY) ^ 2)
#napaka5 = mean((pp.napovedi5 - podatki_vsi$TTY) ^ 2)
#napaka6 = mean((pp.napovedi6 - podatki.TTY[id %in% nenule3]$TTY) ^ 2)

izvoz <- list("napaka 1" = napaka, "napaka 2" = napaka2)
#, "napaka 3" = napaka3,
#              "napaka 4" = napaka4, "napaka 5" = napaka5, "napaka 6" = napaka6)
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



# iskanje najboljšega modela po obeh pristopih
# najprej vključimo vse generirane spremenljivke
lin.mod.vsi <- napaka.cv(podatki.TTY[,-c(18,22)], podatki.TTY$ID, formula=TTY~., 10)
lin.mod.vsi$`napaka 1`
lin.mod.vsi$`napaka 2`

#nobene od novo generiranih spremenljivk
lin.mod.nobene <- napaka.cv(podatki.TTY[,-c(9,10,11,18,22)], podatki.TTY$ID, formula=TTY~., 10)
lin.mod.nobene$`napaka 1`
lin.mod.nobene$`napaka 2`
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
lin.mod.eno3$`napaka 1`
lin.mod.eno3$`napaka 2`
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

