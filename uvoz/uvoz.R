# uvoz in obdelava podatkov

source("~/Faks/mag 1 letnik/MzR/Time-to-yes-contract-and-money/lib/libraries.R")

uvoz <- read_excel("podatki/IZMISLJENI_PODATKI.xlsx")


# May -> Maj
uvoz$MESEC <- gsub("May", "Maj", uvoz$MESEC)


# naredimo vrednosti produktov bolj atraktivne - izmislimo si tipe kreditov in 
# preimenujemo regiji v vzhodno in zahodno
podatki <- uvoz %>% mutate(PRODUKT = case_when(
  uvoz$PRODUKT == 1 ~ "osebni",
  uvoz$PRODUKT == 2 ~ "študentski",
  uvoz$PRODUKT == 3 ~ "avtomobilski",
  uvoz$PRODUKT == 4 ~ "startup",
  uvoz$PRODUKT == 5 ~ "hipotekarni",
  uvoz$PRODUKT == 6 ~ "investicijski",
  uvoz$PRODUKT == 7 ~ "izobraževalni"
  ), REGIJA = case_when(
    uvoz$REGIJA == 1 ~ "vzhodna",
    uvoz$REGIJA == 2 ~ "zahodna")
)

podatki <- rename(podatki, c("produkt" = "PRODUKT", "mesec" = "MESEC", "tip" = "TIP",
                             "znesek" = "ZNESEK_POSLA_V_DONARJIH", "regija" = "REGIJA",
                             "poslovalnica" = "POSLOVALNICA"))

# tidydata končana




