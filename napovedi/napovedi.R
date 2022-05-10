## Napoved trendov s stronjim učenjem

# glavne ideje:
# - ustvarjanje novih atributov
# - PAZI NA ID! - vsi z istim ID so 1 podatek - dodaj nove stolpce kot recimo število kreditov (in koliko katerih), 0 in 1
# - učno in testno množico je treba ustrezno razdeliti - VSI z istim ID v eno ali drugo!!
# - KNN, random forests, lin regression, decision trees, lmer? Probaj vse


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


# korelacijska matrika
cor_TTY <- round(cor(podatki.TTY),2)
cor_TTY2 <- rcorr(as.matrix(podatki.TTY))
#print(cor_TTY2)
symnum(cor_TTY)
corrplot(cor_TTY, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)



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



napaka.cv <- function(podatki_vis, podatki_id, formula, k){
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
  # prečno preverjanje
  for (i in 1:length(razbitje)){
    # učni podatki
    data <- podatki %>% mutate(contains = ID %in% razbitje[[i]])
    train.data <- data %>% filter(contains == FALSE) %>% select(-contains)
    # testni podatki
    test.data <- data %>% filter(contains == TRUE) %>% select(-contains)
    
    # naučimo model
    mod.L <- lm(data = train.data, formula = formula)
    mod.lmer <- lmer(data = trian.data, formula = formula)
    # napovemo za testne podatke
    napovedi <- predict(model, newdata = test.data)
    pp.napovedi[ razbitje[[i]] ] <- napovedi
  }
# izračunamo MSE
napaka = mean((pp.napovedi - podatki$Prob) ^ 2)
return(napaka)
}

for (i in 1:length(razbitje)){
  # učni podatki
  train.data = podatki.TTY[ -razbitje[[i]]]
  # testni podatki
  test.data = podatki.TTY[ razbitje[[i]], ]
}


#funkcija deli točne napovedi z vsoto vseh napovedi = točnost napovedi
accuracy <- function(x)
{sum(diag(x)/(sum(rowSums(x)))) * 100
}


# RANDOM FOREST
library(randomForest)

TTY.RF <- randomForest(TTY ~ ., data = train.data, mtry = 3,
                         importance = TRUE, na.action = na.omit)
TTY.RF
plot(TTY.RF)

# Predicting the Test set results
y_pred = predict(TTY.RF, newdata = test.data[,-4])


confusion_mtx = table(test.data$TTY, y_pred)

accuracy(confusion_mtx2)

napaka = mean((y_pred2 - test.data$TTY) ^ 2)
napaka

df.napak <- data.frame(cbind(test.data$TTY, round(y_pred2,4)))
df.napak <- rename(df.napak, c("podatek" = "X1", "napoved"="X2"))
df.napak <- df.napak %>% summarise(podatek, napoved,
                                   abs_napaka = as.numeric(format(round(abs(y_pred2 - test.data$TTY),4), scientific=FALSE)),
                                   kv_napaka = as.numeric(format(round((y_pred2 - test.data$TTY) ^ 2,4), scientific=FALSE)))
colMeans(df.napak)






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

