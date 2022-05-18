#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        output$podatki<- DT::renderDataTable({
            
            podatki
        })
        
        # PRODUKT
        
        output$produkti <- renderPlot({
            
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
            
            mes.del.prod$mesec <- factor(mes.del.prod$mesec, levels=one.year)
            mesec.produkti <- mes.del.prod %>% group_by(mesec) %>% count(produkt) %>% 
                mutate(delez = round(n/sum(n) * 100,2))
            mesec.produkti <- rename(mesec.produkti, "stevilo" = "n")
            
            mesec.povp <- mean(mesec.produkti.skupaj$vseh)
            mesec.med <- median(mesec.produkti.skupaj$vseh)
            
            
            if(input$lastnost_produkt != "Skupno število po mesecih"){
                produkti <- switch(input$lastnost_produkt,
                                   "Skupno število v letu" = ggplot(podatki, aes(y=produkt, fill =produkt)) +
                                       geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
                                       ylab("produkt") + xlab("število") +
                                       ggtitle("Število posameznih produktov v celem letu") + 
                                       theme(axis.text.x=element_text(angle=45, hjust=1)),
                                   "Število posameznih po mesecih" = ggplot(mesec.produkti, aes(x=mesec, y=stevilo, group=produkt, colour=produkt)) + 
                                       geom_line() + geom_point() + ggtitle("Število posameznih produktov po mesecih") +
                                       ylab("število") + scale_colour_discrete(labels = c("avtomobilski", "hipotekarni",
                                                                                          "investicijski", "izobraževalni", "osebni", "startup", "študentski")),
                                   "Delež v letu" = ggplot(delez.produktov2, aes(x="", y=delez, fill =produkt)) +
                                       geom_col(width=0.7, position = position_dodge(width = 1)) + 
                                       geom_label(aes(x="", y = delez + 2, label = delez), 
                                                  position = position_dodge(width = 1), show.legend = FALSE) + xlab("produkt") + ylab("delež (v %)")+
                                       ggtitle("Delež posameznih produktov (v odstotkih) v celem letu"),
                                   "Delež po mesecih" = ggplot(delez.produktov2, aes(x=mesec, y=mes_delez, group=produkt, colour=produkt)) + 
                                       geom_line() + geom_point() + ggtitle("Delež posameznih produktov (v odstotkih) po mesecih") +
                                       ylab("delež (v %)") + scale_colour_discrete(labels = c("avtomobilski", "hipotekarni",
                                                                                              "investicijski", "izobraževalni", "osebni", "startup", "študentski"))
                )
                print(produkti)
            }
            else{
                st_meseci <- switch(input$prod.lastnost.med.povp,
                                    "brez" = ggplot(mesec.produkti.skupaj, aes(x=mesec, y=vseh, group=1)) + 
                                        geom_smooth() + geom_point() + ggtitle("Število vseh produktov po mesecih") +
                                        ylab("število"),
                                    "mediana" = ggplot(mesec.produkti.skupaj, aes(x=mesec, y=vseh, group=1)) + 
                                        geom_smooth() + geom_point() + ggtitle("Število vseh produktov po mesecih") +
                                        ylab("število")+ geom_line(aes(y=mesec.med, colour = "Mediana"), lwd=1.3) + 
                                        scale_colour_manual("", breaks= "Mediana",
                                                            values = c("Mediana"="green")),
                                    "povprečje" = ggplot(mesec.produkti.skupaj, aes(x=mesec, y=vseh, group=1)) + 
                                        geom_smooth() + geom_point() + ggtitle("Število vseh produktov po mesecih") +
                                        ylab("število") + geom_line(aes(y=mesec.povp, colour="Povprečje"), lwd=1.3) + 
                                        scale_colour_manual("", breaks= "Povprečje",
                                                            values = c("Povprečje"="red")),
                                    "vse" = ggplot(mesec.produkti.skupaj, aes(x=mesec, y=vseh, group=1)) + 
                                        geom_smooth() + geom_point() + ggtitle("Število vseh produktov po mesecih") +
                                        ylab("število") + geom_line(aes(y=mesec.povp, colour="Povprečje"), lwd=1.2) + 
                                        geom_line(aes(y=mesec.med, colour = "Mediana"), lwd=1.2) +
                                        scale_colour_manual("", breaks = c("Povprečje", "Mediana"),
                                                            values = c("Povprečje"="red", "Mediana"="green")))
                print(st_meseci)
            }
            
            
            
            
            
        })
        
        #TIP
        
        output$tipi <- renderPlot({
            
            del.tipov <- as.data.frame(prop.table(table(podatki$tip))*100)
            delez.tipov <- rename(del.tipov, c("tip" = "Var1", "delez" = "Freq"))
            
            mes.del.tip <- subset(podatki, select = c(mesec, tip))
            mes.delez.tipov <- mes.del.tip %>% group_by(mesec) %>% count(tip) %>% 
                summarise(delez = n/sum(n) * 100, tip)
            mes.delez.tipov$mesec <- factor(mes.delez.tipov$mesec, levels=one.year)
            
            prod.tip <- subset(podatki, select = c(produkt, tip))
            prod.del.tip <- prod.tip %>% group_by(produkt) %>% count(tip) %>% 
                summarise(delez = n/sum(n) * 100, tip)
            
            tipi <- switch(input$lastnost_tip,
                           "Skupno število" = ggplot(podatki, aes(y=tip, fill = tip)) +
                               geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
                               ylab("produkt") + xlab("število") +
                               ggtitle("Število posameznih tipov v celem letu") + 
                               theme(axis.text.x=element_text(angle=45, hjust=1)),
                           "Delež v letu" = ggplot(delez.tipov, aes(x="", y=delez, fill =tip)) +
                               geom_col(width=0.7, position = position_dodge(width = 1)) + 
                               geom_label(aes(x="", y = delez + 3, label = delez), 
                                          position = position_dodge(width = 1), show.legend = FALSE) + xlab("tip") + 
                               ggtitle("Delež posameznih tipov (v odstotkih) v celem letu") + ylab("delež (v %)"),
                           "Delež po mesecih" = ggplot(mes.delez.tipov, aes(x=mesec, y=delez, group=tip, colour=tip)) + 
                               geom_line() + geom_point() + ggtitle("Delež posameznih tipov (v odstotkih) po mesecih") +
                               ylab("delež (v %)"),
                           "Delež znotraj produktov" = ggplot(prod.del.tip, aes(x=produkt, y=delez, fill=tip)) + 
                               geom_col(position = position_dodge(width = 0.9)) +
                               ggtitle("Delež posameznih tipov znotraj posameznega produkta") + xlab("produkt") + 
                               ylab("delež (v %") + theme(axis.text.x=element_text(angle=45, hjust=1))
            )
            print(tipi)
            
        })   
        
        #REGIJA
        
        output$regije <- renderPlot({ 
            
            regija <- subset(podatki, select = c(produkt, mesec, tip, regija, poslovalnica))
            
            prod.regija <- regija %>% group_by(regija) %>% count(produkt, mesec)
            prod.regija <- rename(prod.regija, "st_prod" = "n")
            
            tip.regija <- regija %>% group_by(regija) %>% count(tip, mesec)
            tip.regija <- rename(tip.regija, "st_tip" = "n")
            
            regije.skupaj <- prod.regija %>% summarise(vseh = sum(st_prod))
            vzhodna.prod <- prod.regija %>% filter(regija == "vzhodna")
            vzhodna.prod$mesec <- factor(vzhodna.prod$mesec, labels = one.year)
            
            zahodna.prod <- prod.regija %>% filter(regija == "zahodna")
            zahodna.prod$mesec <- factor(zahodna.prod$mesec, labels = one.year)
            
            vzhodna.tip <- tip.regija %>% filter(regija == "vzhodna")
            vzhodna.tip$mesec <- factor(vzhodna.tip$mesec, labels = one.year)
            
            zahodna.tip <- tip.regija %>% filter(regija == "zahodna")
            zahodna.tip$mesec <- factor(zahodna.tip$mesec, labels = one.year)
            
            
            if(input$lastnost_regija == "Število produktov po mesecih"){
                
                reg.produkt <- switch (input$reg.lastnost.produkt,
                                   "vzhodna" = ggplot(vzhodna.prod, aes(x=mesec, y=st_prod, group=produkt, colour=produkt)) + 
                                       geom_line() + geom_point() + ggtitle("Število posameznih produktov v vzhodni regiji po mesecih") +
                                       ylab("število") + scale_colour_discrete(labels = c("avtomobilski", "hipotekarni",
                                                                                          "investicijski", "izobraževalni", "osebni", "startup", "študentski")),
                                   "zahodna" = ggplot(zahodna.prod, aes(x=mesec, y=st_prod, group=produkt, colour=produkt)) + 
                                       geom_line() + geom_point() + ggtitle("Število posameznih produktov v zahodni regiji po mesecih") +
                                       ylab("število") + scale_colour_discrete(labels = c("avtomobilski", "hipotekarni",
                                                                                          "investicijski", "izobraževalni", "osebni", "startup", "študentski"))
                )
                
                print(reg.produkt)
                
                
            }
            
            
            
            else if(input$lastnost_regija == "Število tipov po mesecih"){
                
                reg.produkt <- switch (input$reg.lastnost.tip,
                                       "vzhodna" = ggplot(vzhodna.prod, aes(x=mesec, y=st_prod, group=produkt, colour=produkt)) + 
                                           geom_line() + geom_point() + ggtitle("Število posameznih produktov v vzhodni regiji po mesecih") +
                                           ylab("število") + scale_colour_discrete(labels = c("avtomobilski", "hipotekarni",
                                                                                              "investicijski", "izobraževalni", "osebni", "startup", "študentski")),
                                       "zahodna" = ggplot(zahodna.prod, aes(x=mesec, y=st_prod, group=produkt, colour=produkt)) + 
                                           geom_line() + geom_point() + ggtitle("Število posameznih produktov v zahodni regiji po mesecih") +
                                           ylab("število") + scale_colour_discrete(labels = c("avtomobilski", "hipotekarni",
                                                                                              "investicijski", "izobraževalni", "osebni", "startup", "študentski"))
                )
                
                print(reg.produkt)
                
                
            }
            
            
            else{
                ggplot(regija, aes(y=produkt, fill = regija)) +
                    geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
                    ggtitle("Število posameznih produktov v posamezni regiji") + 
                    xlab("število") + theme(axis.text.x=element_text(angle=45, hjust=1))
            }
              
        })
        
        
        output$poslovalnice <- renderPlot({
            
            posl.regija <- regija %>% group_by(regija) %>% count(poslovalnica)
            posl.regija <- rename(posl.regija, "st_poslov" = "n")
            
            posl.mesec <- regija %>% group_by(mesec) %>% count(poslovalnica)
            posl.mesec <- rename(posl.mesec, "st_poslov" = "n")
            posl.mesec$mesec <- factor(posl.mesec$mesec, levels = one.year)
            
            poslovalnica <- switch(input$lastnost_poslovalnica,
                                   "Skupno število vlog" = ggplot(regija, aes(y=poslovalnica, fill = regija)) +
                                       geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
                                       ggtitle("Število obdelanih vlog v posamezni poslovalnici") + 
                                       xlab("število"),
                                   "Število vlog po mesecih" = ggplot(posl.mesec, aes(x=mesec, y=st_poslov, group=poslovalnica, colour=poslovalnica)) + 
                                       geom_line() + geom_point() + ggtitle("Število vlog v posamezni posovalnici po mesecih") +
                                       ylab("število") ,
                                   "Število produktov v poslovalnici" = ggplot(regija, aes(y=poslovalnica, fill = produkt)) +
                                       geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
                                       ggtitle("Število posameznih produktov v posamezni poslovalnici") + 
                                       xlab("število"),
                                   "Število tipov v poslovalnici" = ggplot(regija, aes(y=poslovalnica, fill = tip)) +
                                       geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
                                       ggtitle("Število posameznih tipov v posamezni poslovalnici") + 
                                       xlab("število"))
            
            print(poslovalnica)
            
        })
        
        
        output$zneski <- renderPlot({
            
            znesek <- subset(podatki, select = c(produkt, mesec, tip, znesek, regija, poslovalnica))
            znesek$mesec <- factor(znesek$mesec, levels = one.year)
            
            znesek.prod.povp <- znesek %>% group_by(produkt) %>% summarise(povpr_prod = round(mean(znesek),2))
            znesek.tip.povp <- znesek %>% group_by(tip) %>% summarise(povpr_prod = round(mean(znesek),2))
            znesek.regija.povp <- znesek %>% group_by(regija) %>% summarise(povpr_prod = round(mean(znesek),2))
            znesek.posl.povp <- znesek %>% group_by(poslovalnica) %>% summarise(povpr_prod = round(mean(znesek),2))
            znesek.mesec.povp <- znesek %>% group_by(mesec) %>% summarise(povpr_prod = mean(znesek))
            
            
            if(input$lastnost_zneski == "Histogram zneskov"){
                
                hist.znesek <- switch(input$hist.lastnost.zneski,
                                      "brez" = ggplot(podatki, aes(x=znesek)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=5) + 
                                          ggtitle("Histogram zneskov") + xlab("znesek") + ylab("število"),
                                      "gostota" = ggplot(podatki, aes(x=znesek)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=5) + 
                                          geom_density(alpha=0.2, color = "red", size = 1.3, fill = "red") +
                                          ggtitle("Histogram in gostota zneskov") + xlab("znesek") + ylab("število")
                                      )
                
                print(hist.znesek)
            }
            
            else if(input$lastnost_zneski == "Povprečni znesek po mesecih"){
                
                mesec.znesek <- switch(input$mesec.lastnost.zneski,
                                       "brez" = ggplot(znesek.mesec.povp, aes(x=mesec, y=povpr_prod, group=1)) + 
                                              geom_smooth() + geom_point() + geom_line() + 
                                              ggtitle("Povprečen znesek po mesecih") +  ylab("povprečen znesek"),
                                       "mediana" = ggplot(znesek.mesec.povp, aes(x=mesec, y=povpr_prod, group=1)) + 
                                           geom_smooth() + geom_point() + geom_line() + 
                                           ggtitle("Povprečen znesek po mesecih") +  ylab("povprečen znesek") + 
                                           geom_line(aes(y=median(povpr_prod), colour="Mediana"), lwd=1.3) + 
                                           scale_colour_manual("", breaks = "Mediana",
                                                               values = c("Mediana"="green")),
                                       "povprečje" = ggplot(znesek.mesec.povp, aes(x=mesec, y=povpr_prod, group=1)) + 
                                           geom_smooth() + geom_point() + geom_line() + 
                                           ggtitle("Povprečen znesek po mesecih") +  ylab("povprečen znesek") + 
                                           geom_line(aes(y=mean(povpr_prod), colour="Povprečje"), lwd=1.3) + 
                                           scale_colour_manual("", breaks = "Povprečje",
                                                               values = c("Povprečje"="red")),
                                       "vse" = ggplot(znesek.mesec.povp, aes(x=mesec, y=povpr_prod, group=1)) + 
                                           geom_smooth() + geom_point() + geom_line() + 
                                           ggtitle("Povprečen znesek po mesecih") +  ylab("povprečen znesek") + 
                                           geom_line(aes(y=mean(povpr_prod), colour="Povprečje"), lwd=1.2) + 
                                           geom_line(aes(y=median(povpr_prod), colour = "Mediana"), lwd=1.2) +
                                           scale_colour_manual("", breaks = c("Povprečje", "Mediana"),
                                                               values = c("Povprečje"="red", "Mediana"="green"))
                )
                
                print(mesec.znesek)
            }
            
            else if(input$lastnost_zneski == "Povprečni znesek po lastnostih"){
                
                lastnost.znesek <- switch(input$povp.lastnost.zneski,
                                       "produkt" = ggplot(znesek.prod.povp, aes(x = produkt, y=povpr_prod, fill = produkt)) + geom_col() + 
                                           geom_label(aes(x=produkt, y = povpr_prod + 4, label = povpr_prod), 
                                                      position = position_dodge(width = 1), show.legend = FALSE) + 
                                           ylab("Povprečen znesek") + ggtitle("Povprečni zneski glede na produkt"),
                                       "tip" = ggplot(znesek.tip.povp, aes(x = tip, y=povpr_prod, fill = tip)) + geom_col() + 
                                           geom_label(aes(x=tip, y = povpr_prod + 4, label = povpr_prod), 
                                                      position = position_dodge(width = 1), show.legend = FALSE) +
                                           ylab("Povprečen znesek") + ggtitle("Povprečni zneski glede na tip"),
                                       "regija" = ggplot(znesek.regija.povp, aes(x = regija, y=povpr_prod, fill = regija)) + geom_col() + 
                                           geom_label(aes(x=regija, y = povpr_prod + 4, label = povpr_prod), 
                                                      position = position_dodge(width = 1), show.legend = FALSE) +
                                           ylab("Povprečen znesek") + ggtitle("Povprečni zneski glede na regijo"),
                                       "poslovalnica" = ggplot(znesek.posl.povp, aes(x = poslovalnica, y=povpr_prod, fill = poslovalnica)) + geom_col() + 
                                           geom_label(aes(x=poslovalnica, y = povpr_prod + 5, label = povpr_prod), 
                                                      position = position_dodge(width = 1), show.legend = FALSE) +
                                           ylab("Povprečen znesek") + ggtitle("Povprečni zneski glede na poslovalnico")
                )
                
                print(lastnost.znesek)
            }
            
            else{
                
                kvantili.znesek <- switch(input$kvantil.lastnost.zneski,
                                          "produkt" = ggplot(znesek, aes(x= produkt, y=znesek, fill = produkt)) + 
                                              geom_boxplot(outlier.color = "blue") + scale_fill_brewer(palette = "Dark2") +
                                              stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
                                              ggtitle("Graf kvantilov zneskov po produktih") +
                                              theme(axis.text.x=element_text(angle=45, hjust=1)),
                                          "tip" = ggplot(znesek, aes(x= tip, y=znesek, fill = tip)) + 
                                              geom_boxplot(outlier.color = "blue") + scale_fill_brewer(palette = "Dark2") +
                                              stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
                                              ggtitle("Graf kvantilov zneskov po tipih"),
                                          "regija" = ggplot(znesek, aes(x= regija, y=znesek, fill = regija)) + 
                                              geom_boxplot(outlier.color = "blue") + scale_fill_brewer(palette = "Dark2") +
                                              stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
                                              ggtitle("Graf kvantilov zneskov po regijah"),
                                          "poslovalnica" = ggplot(znesek, aes(x= poslovalnica, y=znesek, fill = poslovalnica)) + 
                                              geom_boxplot(outlier.color = "blue") + scale_fill_brewer(palette = "Dark2") +
                                              stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
                                              ggtitle("Graf kvantilov zneskov po poslovalnicah"),
                                          "mesec" = ggplot(znesek, aes(x= mesec, y=znesek, fill = mesec)) + 
                                              geom_boxplot(outlier.color = "blue") +
                                              stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
                                              ggtitle("Graf kvantilov zneskov po mesecih")
                                          )
                
                print(kvantili.znesek)
            
                }
            
        })
        
        output$hist.casi <- renderPlot({
            
            if(input$hist_casi == "Čas do odobritve"){
                
                tty.hist <- switch(input$hist.lastnost.casi,
                    "brez" = ggplot(podatki, aes(x=TTY)) + geom_histogram(color = "black", fill="white", binwidth=1) +
                        ggtitle("Histogram časov do odobritve") + xlab("čas") + ylab("število"),
                    "gostota" = ggplot(podatki, aes(x=TTY)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=1) + 
                        geom_density(alpha=0.2, color = "red", size = 1.3, fill = "red") + 
                        ggtitle("Histogram in gostota časov do odobritve") + xlab("čas") + ylab("število")
                )
                print(tty.hist)
            
            }
            
            
            else if(input$hist_casi == "Čas do podpisa pogodbe"){
                
                ttc.hist <- switch(input$hist.lastnost.casi,
                                   "brez" = ggplot(podatki, aes(x=TTC)) + geom_histogram(color = "black", fill="white", binwidth=1) +
                                       ggtitle("Histogram časov do podpisa pogodbe") + xlab("čas") + ylab("število"),
                                   "gostota" = ggplot(podatki, aes(x=TTC)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=1) + 
                                       geom_density(alpha=0.2, color = "red", size = 1.3, fill = "red") + 
                                       ggtitle("Histogram in gostota časov do podpisa pogodbe") + xlab("čas") + ylab("število")
                )
                print(ttc.hist)
                
            }
            
            else{
                
                ttm.hist <- switch(input$hist.lastnost.casi,
                                   "brez" = ggplot(podatki, aes(x=TTM)) + geom_histogram(color = "black", fill="white", binwidth=1) +
                                       ggtitle("Histogram časov do prejema sredstev") + xlab("čas") + ylab("število"),
                                   "gostota" = ggplot(podatki, aes(x=TTM)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=1) + 
                                       geom_density(alpha=0.2, color = "red", size = 1.3, fill = "red") + 
                                       ggtitle("Histogram in gostota časov do prejema sredstev") + xlab("čas") + ylab("število")
                )
                print(ttm.hist)
                
            }
            
        })
        
        
        output$prod.casi <- renderPlot({
            
            casi.produkt <- podatki %>% select(TTY,TTC,TTM,produkt)
            casi.produkt <- casi.produkt %>%  pivot_longer(!produkt, names_to="TTi", values_to = "cas")
            casi.produkt <- within(casi.produkt, TTi <- factor(TTi, levels=c("TTY", "TTC", "TTM")) )
            casi.produkt <- casi.produkt %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))
            
            casi.produkt.rac <- podatki %>% summarise(produkt, TTY, TTC-TTY, TTM-TTC)
            casi.produkt.rac <- casi.produkt.rac %>% pivot_longer(!produkt, names_to = "TTi", values_to = "cas")
            casi.produkt.rac <- within(casi.produkt.rac, TTi <- factor(TTi, levels = c("TTY", "TTC - TTY", "TTM - TTC")))
            casi.produkt.rac <- casi.produkt.rac %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))
            
          
            if(input$prod_casi == "Absolutni časi"){
                
                prod.cas.abs <- switch(input$prod.lastnost.casi,
                                       "brez" = ggplot(casi.produkt, aes(x=produkt, y=cas, fill=produkt)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                           scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                                                             values = palette("Dark2")),
                                       "povprečje" = ggplot(casi.produkt, aes(x=produkt, y=cas, fill=produkt)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.3) +
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                           scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                                                             values = palette("Dark2")) + 
                                           scale_colour_manual("", breaks = "Povprečje vseh",
                                                               values = c("Povprečje vseh"="dark green")),
                                       "mediana" = ggplot(casi.produkt, aes(x=produkt, y=cas, fill=produkt)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.3) + 
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                           scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                                                             values = palette("Dark2")) + 
                                           scale_colour_manual("", breaks = "Mediana vseh",
                                                               values = c("Mediana vseh"="dark blue")), 
                                       "vse" = ggplot(casi.produkt, aes(x=produkt, y=cas, fill=produkt)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.2) + 
                                           geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.2) +
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                           scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                                                             values = palette("Dark2")) + 
                                           scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                                                               values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
                                       )
                print(prod.cas.abs)
            }
            
            else{
                
                prod.cas.rel <- switch(input$prod.lastnost.casi,
                                        "brez" = ggplot(casi.produkt.rac, aes(x=produkt, y=cas, fill=produkt)) + 
                                            geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                            stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                            theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                            ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                            scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                                                              values = palette("Dark2")),
                                        "povprečje" = ggplot(casi.produkt.rac, aes(x=produkt, y=cas, fill=produkt)) + 
                                            geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                            geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.3) +
                                            stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                            theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                            ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                            scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                                                              values = palette("Dark2")) + 
                                            scale_colour_manual("", breaks = "Povprečje vseh",
                                                                values = c("Povprečje vseh"="dark green")),
                                        "mediana" = ggplot(casi.produkt.rac, aes(x=produkt, y=cas, fill=produkt)) + 
                                            geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                            geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.3) + 
                                            stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                            theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                            ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                            scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                                                              values = palette("Dark2")) + 
                                            scale_colour_manual("", breaks = "Mediana vseh",
                                                                values = c("Mediana vseh"="dark blue")), 
                                        "vse" = ggplot(casi.produkt.rac, aes(x=produkt, y=cas, fill=produkt)) + 
                                            geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                            geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.2) + 
                                            geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.2) +
                                            stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                            theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                            ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                            scale_fill_manual(labels = c("avtomobilski", "hipotekarni", "investicijski", "izobraževalni", "osebni", "startup", "študentski"),
                                                              values = palette("Dark2")) + 
                                            scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                                                                values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
            )
            print(prod.cas.rel)
                
            }
              
        })
        
        
        
        output$mesec.casi <- renderPlot({
            
            casi.mesec <- podatki %>% select(TTY,TTC,TTM,mesec)
            casi.mesec <- casi.mesec %>%  pivot_longer(!mesec, names_to="TTi", values_to = "cas")
            casi.mesec <- within(casi.mesec, TTi <- factor(TTi, levels=c("TTY", "TTC", "TTM")) )
            casi.mesec <- casi.mesec %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))
            
            casi.mesec.rac <- podatki %>% summarise(mesec, TTY, TTC-TTY, TTM-TTC)
            casi.mesec.rac <- casi.mesec.rac %>% pivot_longer(!mesec, names_to = "TTi", values_to = "cas")
            casi.mesec.rac <- within(casi.mesec.rac, TTi <- factor(TTi, levels = c("TTY", "TTC - TTY", "TTM - TTC")))
            casi.mesec.rac <- casi.mesec.rac %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))
            
            
            if(input$mesec_casi == "Absolutni časi"){
               mesec.cas.abs <- switch(input$mesec.lastnost.casi,
                                       "brez" = ggplot(casi.mesec, aes(x=mesec, y=cas, fill=mesec)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)"),
                                       "povprečje" = ggplot(casi.mesec, aes(x=mesec, y=cas, fill=mesec)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.3) +
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                           scale_colour_manual("", breaks = "Povprečje vseh",
                                                               values = c("Povprečje vseh"="dark green")),
                                       "mediana" = ggplot(casi.mesec, aes(x=mesec, y=cas, fill=mesec)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.3) + 
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") + 
                                           scale_colour_manual("", breaks = "Mediana vseh",
                                                               values = c("Mediana vseh"="dark blue")), 
                                       "vse" = ggplot(casi.mesec, aes(x=mesec, y=cas, fill=mesec)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.2) + 
                                           geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.2) +
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") + 
                                           scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                                                               values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
                )
                print(mesec.cas.abs)
            }
            
            else{
                
                mesec.cas.rel <- switch(input$mesec.lastnost.casi,
                                       "brez" = ggplot(casi.mesec.rac, aes(x=mesec, y=cas, fill=mesec)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)"),
                                       "povprečje" = ggplot(casi.mesec.rac, aes(x=mesec, y=cas, fill=mesec)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.3) +
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)")+ 
                                           scale_colour_manual("", breaks = "Povprečje vseh",
                                                               values = c("Povprečje vseh"="dark green")),
                                       "mediana" = ggplot(casi.mesec.rac, aes(x=mesec, y=cas, fill=mesec)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.3) + 
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)")+ 
                                           scale_colour_manual("", breaks = "Mediana vseh",
                                                               values = c("Mediana vseh"="dark blue")), 
                                       "vse" = ggplot(casi.mesec.rac, aes(x=mesec, y=cas, fill=mesec)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.2) + 
                                           geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.2) +
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)")+ 
                                           scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                                                               values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
                )
                print(mesec.cas.rel)
                
            }
            
        })
        
        
        output$tip.casi <- renderPlot({
            
            casi.tip <- podatki %>% select(TTY,TTC,TTM,tip)
            casi.tip <- casi.tip %>%  pivot_longer(!tip, names_to="TTi", values_to = "cas")
            casi.tip <- within(casi.tip, TTi <- factor(TTi, levels=c("TTY", "TTC", "TTM")) )
            casi.tip <- casi.tip %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))
            
            casi.tip.rac <- podatki %>% summarise(tip, TTY, TTC-TTY, TTM-TTC)
            casi.tip.rac <- casi.tip.rac %>% pivot_longer(!tip, names_to = "TTi", values_to = "cas")
            casi.tip.rac <- within(casi.tip.rac, TTi <- factor(TTi, levels = c("TTY", "TTC - TTY", "TTM - TTC")))
            casi.tip.rac <- casi.tip.rac %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))
            
            
            if(input$tip_casi == "Absolutni časi"){
                
                tip.cas.abs <- switch(input$tip.lastnost.casi,
                                       "brez" = ggplot(casi.tip, aes(x=tip, y=cas, fill=tip)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                          scale_fill_manual(values = palette("Dark2")),
                                       "povprečje" = ggplot(casi.tip, aes(x=tip, y=cas, fill=tip)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.3) +
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                          scale_fill_manual(values = palette("Dark2")) +
                                           scale_colour_manual("", breaks = "Povprečje vseh",
                                                               values = c("Povprečje vseh"="dark green")),
                                       "mediana" = ggplot(casi.tip, aes(x=tip, y=cas, fill=tip)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.3) + 
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                          scale_fill_manual(values = palette("Dark2")) + 
                                           scale_colour_manual("", breaks = "Mediana vseh",
                                                               values = c("Mediana vseh"="dark blue")), 
                                       "vse" = ggplot(casi.tip, aes(x=tip, y=cas, fill=tip)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.2) + 
                                           geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.2) +
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                          scale_fill_manual(values = palette("Dark2")) + 
                                           scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                                                               values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
                )
                print(tip.cas.abs)
            }
            
            else{
                
                tip.cas.rel <- switch(input$tip.lastnost.casi,
                                       "brez" = ggplot(casi.tip.rac, aes(x=tip, y=cas, fill=tip)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                          scale_fill_manual(values = palette("Dark2")),
                                       "povprečje" = ggplot(casi.tip.rac, aes(x=tip, y=cas, fill=tip)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.3) +
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                          scale_fill_manual(values = palette("Dark2")) + 
                                           scale_colour_manual("", breaks = "Povprečje vseh",
                                                               values = c("Povprečje vseh"="dark green")),
                                       "mediana" = ggplot(casi.tip.rac, aes(x=tip, y=cas, fill=tip)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.3) + 
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                          scale_fill_manual(values = palette("Dark2")) + 
                                           scale_colour_manual("", breaks = "Mediana vseh",
                                                               values = c("Mediana vseh"="dark blue")), 
                                       "vse" = ggplot(casi.tip.rac, aes(x=tip, y=cas, fill=tip)) + 
                                           geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                           geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.2) + 
                                           geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.2) +
                                           stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                           theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                           ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                          scale_fill_manual(values = palette("Dark2")) + 
                                           scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                                                               values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
                )
                print(tip.cas.rel)
                
            }
            
        })
        
        
        output$regija.casi <- renderPlot({
            
            casi.regija <- podatki %>% select(TTY,TTC,TTM,regija)
            casi.regija <- casi.regija %>%  pivot_longer(!regija, names_to="TTi", values_to = "cas")
            casi.regija <- within(casi.regija, TTi <- factor(TTi, levels=c("TTY", "TTC", "TTM")) )
            casi.regija <- casi.regija %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))
            
            casi.regija.rac <- podatki %>% summarise(regija, TTY, TTC-TTY, TTM-TTC)
            casi.regija.rac <- casi.regija.rac %>% pivot_longer(!regija, names_to = "TTi", values_to = "cas")
            casi.regija.rac <- within(casi.regija.rac, TTi <- factor(TTi, levels = c("TTY", "TTC - TTY", "TTM - TTC")))
            casi.regija.rac <- casi.regija.rac %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))
            
            
            if(input$regija_casi == "Absolutni časi"){
                
                regija.cas.abs <- switch(input$regija.lastnost.casi,
                                      "brez" = ggplot(casi.regija, aes(x=regija, y=cas, fill=regija)) + 
                                          geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                          stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                          theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                          ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                          scale_fill_manual(values = palette("Dark2")),
                                      "povprečje" = ggplot(casi.regija, aes(x=regija, y=cas, fill=regija)) + 
                                          geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                          geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.3) +
                                          stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                          theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                          ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                          scale_fill_manual(values = palette("Dark2")) +
                                          scale_colour_manual("", breaks = "Povprečje vseh",
                                                              values = c("Povprečje vseh"="dark green")),
                                      "mediana" = ggplot(casi.regija, aes(x=regija, y=cas, fill=regija)) + 
                                          geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                          geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.3) + 
                                          stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                          theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                          ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                          scale_fill_manual(values = palette("Dark2")) + 
                                          scale_colour_manual("", breaks = "Mediana vseh",
                                                              values = c("Mediana vseh"="dark blue")), 
                                      "vse" = ggplot(casi.regija, aes(x=regija, y=cas, fill=regija)) + 
                                          geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                          geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.2) + 
                                          geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.2) +
                                          stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                          theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                          ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                          scale_fill_manual(values = palette("Dark2")) + 
                                          scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                                                              values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
                )
                print(regija.cas.abs)
            }
            
            else{
                
                regija.cas.rel <- switch(input$regija.lastnost.casi,
                                      "brez" = ggplot(casi.regija.rac, aes(x=regija, y=cas, fill=regija)) + 
                                          geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                          stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                          theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                          ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                          scale_fill_manual(values = palette("Dark2")),
                                      "povprečje" = ggplot(casi.regija.rac, aes(x=regija, y=cas, fill=regija)) + 
                                          geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                          geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.3) +
                                          stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                          theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                          ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                          scale_fill_manual(values = palette("Dark2")) + 
                                          scale_colour_manual("", breaks = "Povprečje vseh",
                                                              values = c("Povprečje vseh"="dark green")),
                                      "mediana" = ggplot(casi.regija.rac, aes(x=regija, y=cas, fill=regija)) + 
                                          geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                          geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.3) + 
                                          stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                          theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                          ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                          scale_fill_manual(values = palette("Dark2")) +
                                          scale_colour_manual("", breaks = "Mediana vseh",
                                                              values = c("Mediana vseh"="dark blue")), 
                                      "vse" = ggplot(casi.regija.rac, aes(x=regija, y=cas, fill=regija)) + 
                                          geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                          geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.2) + 
                                          geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.2) +
                                          stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                          theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                          ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                          scale_fill_manual(values = palette("Dark2")) +
                                          scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                                                              values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
                )
                print(regija.cas.rel)
                
            }
            
        })  
        
        
        output$poslovalnica.casi <- renderPlot({
            
            casi.poslovalnica <- podatki %>% select(TTY,TTC,TTM,poslovalnica)
            casi.poslovalnica <- casi.poslovalnica %>%  pivot_longer(!poslovalnica, names_to="TTi", values_to = "cas")
            casi.poslovalnica <- within(casi.poslovalnica, TTi <- factor(TTi, levels=c("TTY", "TTC", "TTM")) )
            casi.poslovalnica <- casi.poslovalnica %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))
            
            casi.poslovalnica.rac <- podatki %>% summarise(poslovalnica, TTY, TTC-TTY, TTM-TTC)
            casi.poslovalnica.rac <- casi.poslovalnica.rac %>% pivot_longer(!poslovalnica, names_to = "TTi", values_to = "cas")
            casi.poslovalnica.rac <- within(casi.poslovalnica.rac, TTi <- factor(TTi, levels = c("TTY", "TTC - TTY", "TTM - TTC")))
            casi.poslovalnica.rac <- casi.poslovalnica.rac %>% group_by(TTi) %>% mutate(mediana = median(cas), povprecje = round(mean(cas),2))
            
            
            if(input$poslovalnica_casi == "Absolutni časi"){
                
                poslovalnica.cas.abs <- switch(input$poslovalnica.lastnost.casi,
                                         "brez" = ggplot(casi.poslovalnica, aes(x=poslovalnica, y=cas, fill=poslovalnica)) + 
                                             geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                             stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                             theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                             ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                             scale_fill_manual(values = palette("Dark2")),
                                         "povprečje" = ggplot(casi.poslovalnica, aes(x=poslovalnica, y=cas, fill=poslovalnica)) + 
                                             geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                             geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.3) +
                                             stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                             theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                             ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                             scale_fill_manual(values = palette("Dark2")) +
                                             scale_colour_manual("", breaks = "Povprečje vseh",
                                                                 values = c("Povprečje vseh"="dark green")),
                                         "mediana" = ggplot(casi.poslovalnica, aes(x=poslovalnica, y=cas, fill=poslovalnica)) + 
                                             geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                             geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.3) + 
                                             stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                             theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                             ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                             scale_fill_manual(values = palette("Dark2")) + 
                                             scale_colour_manual("", breaks = "Mediana vseh",
                                                                 values = c("Mediana vseh"="dark blue")), 
                                         "vse" = ggplot(casi.poslovalnica, aes(x=poslovalnica, y=cas, fill=poslovalnica)) + 
                                             geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                             geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.2) + 
                                             geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.2) +
                                             stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                             theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                             ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                             scale_fill_manual(values = palette("Dark2")) +
                                             scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                                                                 values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
                )
                print(poslovalnica.cas.abs)
            }
            
            else{
                
                poslovalnica.cas.rel <- switch(input$poslovalnica.lastnost.casi,
                                         "brez" = ggplot(casi.poslovalnica.rac, aes(x=poslovalnica, y=cas, fill=poslovalnica)) + 
                                             geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                             stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                             theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                             ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                             scale_fill_manual(values = palette("Dark2")),
                                         "povprečje" = ggplot(casi.poslovalnica.rac, aes(x=poslovalnica, y=cas, fill=poslovalnica)) + 
                                             geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                             geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.3) +
                                             stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                             theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                             ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                             scale_fill_manual(values = palette("Dark2")) +
                                             scale_colour_manual("", breaks = "Povprečje vseh",
                                                                 values = c("Povprečje vseh"="dark green")),
                                         "mediana" = ggplot(casi.poslovalnica.rac, aes(x=poslovalnica, y=cas, fill=poslovalnica)) + 
                                             geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                             geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.3) + 
                                             stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                             theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                             ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                             scale_fill_manual(values = palette("Dark2")) +
                                             scale_colour_manual("", breaks = "Mediana vseh",
                                                                 values = c("Mediana vseh"="dark blue")), 
                                         "vse" = ggplot(casi.poslovalnica.rac, aes(x=poslovalnica, y=cas, fill=poslovalnica)) + 
                                             geom_boxplot(outlier.colour = "blue") + facet_wrap(vars(TTi)) + 
                                             geom_hline(aes(yintercept = mediana, group = TTi, colour = 'Mediana vseh'), lwd=1.2) + 
                                             geom_hline(aes(yintercept = povprecje, group = TTi, colour = 'Povprečje vseh'), lwd=1.2) +
                                             stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") + 
                                             theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
                                             ggtitle("Grafi kvantilov časov po produktih") + ylab("Čas (v dnevih)") +
                                             scale_fill_manual(values = palette("Dark2")) +
                                             scale_colour_manual("", breaks = c("Povprečje vseh", "Mediana vseh"),
                                                                 values = c("Povprečje vseh"="dark green", "Mediana vseh"="dark blue"))
                )
                print(poslovalnica.cas.rel)
                
            }
            
        }) 
        
})


