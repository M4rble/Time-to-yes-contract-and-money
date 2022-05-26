#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("united"),
                  
        #tags$head(
              tags$style(".shiny-output-error-validation {color: red;font-size: 20px;}
                              .col-sm-8{color: green;font-size: 20px;font-style: italic;}"),
              
            #),
        
        # Application title
        titlePanel("Dobrodošli v AMBanko!"),
        
        navbarPage("AMBanka",
                   
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
                                                                                     choices = list("brez", "mediana",
                                                                                                    "povprečje", "vse"),
                                                                                     selected = "brez"),
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
                                       
                                       column(10, plotOutput("poslovalnice"))),
                              
                              tabPanel("Zneski", h2("Zneski"),
                                       
                                       sidebarPanel(
                                           
                                           selectInput("lastnost_zneski", "Lastnost:", 
                                                       label = "Izberite želeni graf",
                                                       choices = c("Histogram zneskov",
                                                                   "Povprečni znesek po mesecih",
                                                                   "Povprečni znesek po lastnostih",
                                                                   "Grafi kvantilov po lastnostih")),
                                           # treba dodat odvisnosti glede na lastnosti
                                       ),
                                       
                                       
                                       conditionalPanel(condition="input.lastnost_zneski == 'Histogram zneskov' ",
                                                        
                                                        fluidRow(column(3,
                                                                        
                                                                        radioButtons("hist.lastnost.zneski", "Dodatna lastnost:",
                                                                                     choices = list("brez", "gostota"),
                                                                                     selected = "brez"),
                                                        ))
                                       ),
                                       
                                       
                                       conditionalPanel(condition="input.lastnost_zneski == 'Povprečni znesek po mesecih' ",
                                                        
                                                        fluidRow(column(3,
                                                                        
                                                                        radioButtons("mesec.lastnost.zneski", "Dodatna lastnost:",
                                                                                     choices = list("brez", "mediana",
                                                                                                    "povprečje", "vse"),
                                                                                     selected = "brez"),
                                                        ))
                                       ),
                                       
                                       
                                       conditionalPanel(condition="input.lastnost_zneski == 'Povprečni znesek po lastnostih' ",
                                                        
                                                        fluidRow(column(3,
                                                                        
                                                                        radioButtons("povp.lastnost.zneski", "Lastnost:",
                                                                                     choices = list("produkt", "tip",
                                                                                                    "regija", "poslovalnica"),
                                                                                     selected = "produkt"),
                                                        ))
                                       ),
                                       
                                       conditionalPanel(condition="input.lastnost_zneski == 'Grafi kvantilov po lastnostih' ",
                                                        
                                                        fluidRow(column(3,
                                                                        
                                                                        radioButtons("kvantil.lastnost.zneski", "Lastnost:",
                                                                                     choices = list("produkt", "tip",
                                                                                                    "regija", "poslovalnica",
                                                                                                    "mesec"),
                                                                                     selected = "produkt"),
                                                        ))
                                       ),
                                       
                                       
                                       column(10, plotOutput("zneski"))
                              ),
                   ),
                   
                   navbarMenu("Analiza časov",
                              tabPanel("Histogrami vseh časov", h2("Histogrami vseh časov"),
                                       
                                       sidebarPanel(
                                           
                                           selectInput("hist_casi", "Čas:", 
                                                       label = "Izberite želeni graf",
                                                       choices = c("Čas do odobritve",
                                                                   "Čas do podpisa pogodbe",
                                                                   "Čas do prejema sredstev")),

                                       
                                       ),
                                       
                                       conditionalPanel(condition="input.hist.casi == 'Čas do odobritve' ",
                                                        
                                                        fluidRow(column(3,
                                                                        
                                                                        radioButtons("hist.lastnost.casi", "Dodatna lastnost:",
                                                                                     choices = list("brez", "gostota"),
                                                                                     selected = "brez"),
                                                        ))
                                       ),
                                       
                                       column(10, plotOutput("hist.casi"))
                                       ),
                                       
                                       
                                       
                              tabPanel("Časi glede na produkt", h2("Časi glede na produkt"),
                                       
                                       sidebarPanel(
                                           
                                           selectInput("prod_casi", "Čas:", 
                                                       label = "Izberite želeni graf",
                                                       choices = c("Absolutni časi",
                                                                   "Relativni časi")),
                                           
                                           
                                       ),
                                       
                                       
                                       conditionalPanel(condition="input.prod.casi == 'Absolutni časi' ",
                                                        
                                                        fluidRow(column(3,
                                                                        
                                                                        radioButtons("prod.lastnost.casi", "Dodatna lastnost:",
                                                                                     choices = list("brez", "povprečje",
                                                                                                    "mediana", "vse"),
                                                                                     selected = "brez"),
                                                        ))
                                       ),
                                       
                                       column(10, plotOutput("prod.casi")),
                              ),

                                       
                                       
                              tabPanel("Časi glede na mesec", h2("Časi glede na mesec"),
                                       
                                       sidebarPanel(
                                           
                                           selectInput("mesec_casi", "Čas:", 
                                                       label = "Izberite želeni graf",
                                                       choices = c("Absolutni časi",
                                                                   "Relativni časi")),
                                           
                                           
                                       ),
                                       
                                       
                                       conditionalPanel(condition="input.mesec.casi == 'Absolutni časi' ",
                                                        
                                                        fluidRow(column(3,
                                                                        
                                                                        radioButtons("mesec.lastnost.casi", "Dodatna lastnost:",
                                                                                     choices = list("brez", "povprečje",
                                                                                                    "mediana", "vse"),
                                                                                     selected = "brez"),
                                                        ))
                                       ),
                                       
                                       column(10, plotOutput("mesec.casi")),
                              ),
                                       

                              tabPanel("Časi glede na tip", h2("Časi glede na tip"),
                                       
                                       sidebarPanel(
                                           
                                           selectInput("tip_casi", "Čas:", 
                                                       label = "Izberite želeni graf",
                                                       choices = c("Absolutni časi",
                                                                   "Relativni časi")),
                                           
                                           
                                       ),
                                       
                                       
                                       conditionalPanel(condition="input.tip.casi == 'Absolutni časi' ",
                                                        
                                                        fluidRow(column(3,
                                                                        
                                                                        radioButtons("tip.lastnost.casi", "Dodatna lastnost:",
                                                                                     choices = list("brez", "povprečje",
                                                                                                    "mediana", "vse"),
                                                                                     selected = "brez"),
                                                        ))
                                       ),
                                       
                                       column(10, plotOutput("tip.casi")),
                                       
                                       ),
                              
                              
                              
                              tabPanel("Časi glede na regijo", h2("Časi glede na regijo"),
                                       
                                       sidebarPanel(
                                           
                                           selectInput("regija_casi", "Čas:", 
                                                       label = "Izberite želeni graf",
                                                       choices = c("Absolutni časi",
                                                                   "Relativni časi")),
                                           
                                           
                                       ),
                                       
                                       
                                       conditionalPanel(condition="input.regija.casi == 'Absolutni časi' ",
                                                        
                                                        fluidRow(column(3,
                                                                        
                                                                        radioButtons("regija.lastnost.casi", "Dodatna lastnost:",
                                                                                     choices = list("brez", "povprečje",
                                                                                                    "mediana", "vse"),
                                                                                     selected = "brez"),
                                                        ))
                                       ),
                                       
                                       column(10, plotOutput("regija.casi")),
                                       
                                       ),
                              
                              
                              tabPanel("Časi glede na poslovalnico", h2("Časi glede na poslovalnico"),
                                       
                                       sidebarPanel(
                                           
                                           selectInput("poslovalnica_casi", "Čas:", 
                                                       label = "Izberite želeni graf",
                                                       choices = c("Absolutni časi",
                                                                   "Relativni časi")),
                                           
                                           
                                       ),
                                       
                                       
                                       conditionalPanel(condition="input.poslovalnica.casi == 'Absolutni časi' ",
                                                        
                                                        fluidRow(column(3,
                                                                        
                                                                        radioButtons("poslovalnica.lastnost.casi", "Dodatna lastnost:",
                                                                                     choices = list("brez", "povprečje",
                                                                                                    "mediana", "vse"),
                                                                                     selected = "brez"),
                                                        ))
                                       ),
                                       
                                       column(10, plotOutput("poslovalnica.casi")),
                                       
                                       ),
                              
                   ),
                   
                   navbarMenu("Tabela podatkov",
                              
                              tabPanel("Tabela osnovnih podatkov o strankah",
                                       DT::dataTableOutput("podatki"))),
                   
                   
                   navbarMenu("Izračunaj svoje čase!",
                              tabPanel("Izračun časov za svoj kredit", h2("Izračunajte predvidene čase za svoj kredit"),
                                       sidebarPanel(
                                           numericInput("izbira_zneska",
                                                        label = "Vnesite želeni znesek kredita (med 1 in 870)",
                                                        "", min=1, max=870),
                                            selectInput("izbira_produkta",
                                                        label = "Izberite želeni produkt",
                                                        choices = c("avtomobilski", "hipotekarni", "investicijski",
                                                                    "izobraževalni", "osebni", "startup", "študentski")),
                                            selectInput("izbira_meseca",
                                                        label = "Izberite mesec, ko boste oddali vlogo",
                                                        choices = c("Jan","Feb","Mar","Apr","Maj","Jun","Jul","Aug","Sep","Oct","Nov","Dec")),
                                            selectInput("izbira_tipa",
                                                        label = "Izberite tip kredita",
                                                        choices = c("Novo", "Obnova", "Podaljšanje", "Sprememba")),
                                            selectInput("izbira_regije",
                                                        label = "Izberite regijo v kateri boste oddali vlogo",
                                                        choices = c("vzhodna", "zahodna")),
                                            selectInput("izbira_poslovalnice",
                                                        label = "Izberite poslovalnico v kateri boste oddali vlogo",
                                                        choices = c(1,2,3,4,5,6,7))
                                       ),
                   
                                 mainPanel(textOutput("izracunani.casi.tty"), 
                                           textOutput("izracunani.casi.ttc"),
                                           textOutput("izracunani.casi.ttm"),
                                          )
                                 ))
                   
                   
                   
        )
        
    ) 
)
