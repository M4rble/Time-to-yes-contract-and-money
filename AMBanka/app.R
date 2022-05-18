#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Dobrodošli v AMBanko!"),
    
    navbarPage("AMBanka",
               navbarMenu("Tabela podatkov",
                         
                              tabPanel("Tabela osnovnih podatkov o strankah",
                                       DT::dataTableOutput("podatki"))),
               
               navbarMenu("Analiza osnovnih podatkov",
                          tabPanel("Produkti", h2("Produkti"),
                                   
                                   sidebarPanel(
                                       
                                       selectInput("lastnost_produkt", "Lastnost:", 
                                                   label = "Izberite želeni graf",
                                                   choices = c("Skupno število v letu",
                                                               "Število posameznih po mesecih",
                                                               "Skupno število po mesecih",
                                                               "Delež v letu",
                                                               "Delež po mesecih"))
                                   ),
                                   #=============================================================
                                   conditionalPanel(condition="input.lastnost_produkt == 'Skupno število po mesecih' ",
                                                    
                                                    fluidRow(column(3,
                                                           
                                                   radioButtons("prod.lastnost.med.povp", "Dodatna lastnost:",
                                                                      choices = list("navaden", "mediana",
                                                                                     "povprečje", "vse"),
                                                                      selected = "navaden"),
                                                    ))
                                                   ),
                                   #=============================================================
                                   
                                   column(10, plotOutput("produkti"))),
                                   
                          tabPanel("Tipi", h2("Tipi"),
                                   
                                   sidebarPanel(
                                       
                                       selectInput("lastnost_tip", "Lastnost:", 
                                                   label = "Izberite želeni graf",
                                                   choices = c("Skupno število",
                                                               "Delež v letu",
                                                               "Delež po mesecih",
                                                               "Delež znotraj produktov")),
                                   ),
                                   
                                   column(10, plotOutput("tipi"))),
                                   
                          
                          tabPanel("Regije", h2("Regije"),
                          
                                    sidebarPanel(
                                        
                                        selectInput("lastnost_regija", "Lastnost:", 
                                                    label = "Izberite želeni graf",
                                                    choices = c("Število produktov po regijah",
                                                                "Število produktov po mesecih",
                                                                "Število tipov po mesecih")),
                                    ),
                                   
                                   conditionalPanel(condition="input.lastnost_regija == 'Število produktov po mesecih' ",
                                                    
                                                    fluidRow(column(3,
                                                                    
                                                                    radioButtons("reg.lastnost.produkt", "Izberite regijo:",
                                                                                 choices = list("vzhodna", "zahodna"),
                                                                                 selected = "vzhodna"),
                                                    ))
                                   ),
                                   
                                   conditionalPanel(condition="input.lastnost_regija == 'Število tipov po mesecih' ",
                                                    
                                                    fluidRow(column(3,
                                                                    
                                                                    radioButtons("reg.lastnost.tip", "Izberite regijo:",
                                                                                 choices = list("vzhodna", "zahodna"),
                                                                                 selected = "vzhodna"),
                                                    ))
                                   ),
                                    
                                    column(10, plotOutput("regije"))),
                          
                          # treba dodat še delitev na vzhodno in zahodno 
               
               
                          tabPanel("Poslovalnice", h2("Poslovalnice"),
                                   
                                   sidebarPanel(
                                       
                                       selectInput("lastnost_poslovalnica", "Lastnost:", 
                                                   label = "Izberite želeni graf",
                                                   choices = c("Skupno število vlog",
                                                               "Število vlog po mesecih",
                                                               "Število produktov v poslovalnici",
                                                               "Število tipov v poslovalnici")),
                                   ),
                                   
                                   mainPanel(plotOutput("poslovalnice"))),
                          
                          tabPanel("Zneski", h2("Zneski"),
                                   
                                   sidebarPanel(
                                       
                                       selectInput("lastnost_zneski", "Lastnost:", 
                                                   label = "Izberite želeni graf",
                                                   choices = c("Histogram zneskov",
                                                               "Povprečni znesek po mesecih",
                                                               "Povprečni zneski po lastnostih",
                                                               "Grafi kvantilov po lastnostih")),
                                    # treba dodat odvisnosti glede na lastnosti
                                   ),
                                   
                                   mainPanel(plotOutput("zneski"))
                                   ),
                          ),
               
               navbarMenu("Analiza časov",
                          tabPanel("Histogrami vseh časov", h2("Histogrami vseh časov")),
                          tabPanel("Časi glede na produkt", h2("Časi glede na produkt")),
                          tabPanel("Časi glede na mesec", h2("Časi glede na mesec")),
                          tabPanel("Časi glede na tip", h2("Časi glede na tip")),
                          tabPanel("Časi glede na regijo", h2("Časi glede na regijo")),
                          tabPanel("Časi glede na poslovalnico", h2("Časi glede na poslovalnico"))
                          
               ),
               
               navbarMenu("Izračunaj svoje čase",
                          tabPanel("Čas do odobritve", h2("Čas do odobritve")),
                          tabPanel("Čas do podpisa pogodbe", h2("Čas do podpisa pogodbe")),
                          tabPanel("Čas do prejema sredstev", h2("Čas do prejema sredstev"))
               )
               
               
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
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
                              "navaden" = ggplot(mesec.produkti.skupaj, aes(x=mesec, y=vseh, group=1)) + 
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
          
          produkt <- switch (input$reg.lastnost.prod,
            case = action
          )
          
          
        }
        
        
        regije <- switch(input$lastnost_regija,
                         "Število produktov po regijah" = ggplot(regija, aes(y=produkt, fill = regija)) +
                             geom_bar(position = position_dodge(width = 0.9)) + coord_flip() +
                             ggtitle("Število posameznih produktov v posamezni regiji") + 
                             xlab("število") + theme(axis.text.x=element_text(angle=45, hjust=1)),
                         "Število produktov po mesecih" = ggplot(vzhodna.prod, aes(x=mesec, y=st_prod, group=produkt, colour=produkt)) + 
                             geom_line() + geom_point() + ggtitle("Število posameznih produktov v vzhodni regiji po mesecih") +
                             ylab("število") + scale_colour_discrete(labels = c("avtomobilski", "hipotekarni",
                                                                                "investicijski", "izobraževalni", "osebni", "startup", "študentski")),
                         "Število tipov po mesecih" = ggplot(vzhodna.tip, aes(x=mesec, y=st_tip, group=tip, colour=tip)) + 
                             geom_line() + geom_point() + ggtitle("Število posameznih tipov v vzhodni regiji po mesecih") +
                             ylab("število"))
        
        print(regije)
                         
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
        
        znesek.prod.povp <- znesek %>% group_by(produkt) %>% summarise(povpr_prod = mean(znesek))
        znesek.tip.povp <- znesek %>% group_by(tip) %>% summarise(povpr_prod = round(mean(znesek),2))
        znesek.regija.povp <- znesek %>% group_by(regija) %>% summarise(povpr_prod = round(mean(znesek),2))
        znesek.posl.povp <- znesek %>% group_by(poslovalnica) %>% summarise(povpr_prod = round(mean(znesek),2))
        znesek.mesec.povp <- znesek %>% group_by(mesec) %>% summarise(povpr_prod = mean(znesek))
        
        zneski <- switch(input$lastnost_zneski,
                         "Histogram zneskov" = ggplot(podatki, aes(x=znesek)) + geom_histogram(aes(y=..density..), color = "black", fill="white", binwidth=5) + 
                             geom_density(alpha=0.2, color = "red", size = 1.3, fill = "red") +
                             ggtitle("Histogram in gostota zneskov") + xlab("znesek") + ylab("število"),
                         "Povprečni znesek po mesecih" = ggplot(znesek.mesec.povp, aes(x=mesec, y=povpr_prod, group=1)) + 
                             geom_smooth() + geom_point() + geom_line() + 
                             ggtitle("Povprečen znesek po mesecih") +  ylab("povprečen znesek"),
                         "Povprečni zneski po lastnostih" = ggplot(znesek.prod.povp, aes(x = produkt, y=povpr_prod, fill = produkt)) + geom_col() + 
                             ylab("Povprečen znesek") + ggtitle("Povprečni zneski glede na produkt") + scale_fill_brewer("Accent"),
                         "Grafi kvantilov po lastnostih" = ggplot(znesek, aes(x= produkt, y=znesek, fill = produkt)) + 
                             geom_boxplot(outlier.color = "blue") + scale_fill_brewer(palette = "Dark2") +
                             stat_summary(fun =mean, geom="point", shape=20, size=4, color="green", fill="green") +
                             ggtitle("Graf kvantilov zneskov po produktih") +
                             theme(axis.text.x=element_text(angle=45, hjust=1)))
        
        print(zneski)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



# Sidebar with a slider input for number of bins 
#sidebarLayout(
#    sidebarPanel(
##        sliderInput("bins",
#                   "Number of bins:",
#                    min = 1,
#                    max = 50,
#                    value = 30)
#    ),

# Show a plot of the generated distribution