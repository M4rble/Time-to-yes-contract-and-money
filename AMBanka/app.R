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
                                       
                                       selectInput("lastnost", "Lastnost:", 
                                                   label = "Izberite želeni graf",
                                                   choices = c("Delež po letu",
                                                     "Delež po mesecih")),
                                   ),
                                   
                                   mainPanel(plotOutput("delez.produktov"))),
                                   
                          tabPanel("Tipi", h2("Tipi")),
                          tabPanel("Meseci", h2("Meseci")),
                          tabPanel("Regije", h2("Regije")),
                          tabPanel("Poslovalnice", h2("Poslovalnice")),
                          tabPanel("Zneski", h2("Zneski")),
                          ),
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$podatki<- DT::renderDataTable({
        
        podatki
    })

    output$delez.produktov <- renderPlot({
        
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
        
        delez <- switch(input$lastnost,
                        "Delež po letu" = ggplot(delez.produktov2, aes(x="", y=delez, fill =produkt)) +
                            geom_col(width=0.7, position = position_dodge(width = 1)) + 
                            geom_label(aes(x="", y = delez + 2, label = delez), 
                                       position = position_dodge(width = 1), show.legend = FALSE) + xlab("") + 
                            ggtitle("Delež posameznih produktov (v odstotkih) v celem letu"),
                        "Delež po mesecih" = ggplot(delez.produktov2, aes(x=mesec, y=mes_delez, group=produkt, colour=produkt)) + 
                            geom_line() + geom_point() + ggtitle("Delež posameznih produktov (v odstotkih) po mesecih") +
                            ylab("delež (v %)") + scale_colour_discrete(labels = c("avtomobilski", "hipotekarni",
                                                                                   "investicijski", "izobraževalni", "osebni", "startup", "študentski"))
        )
        print(delez)
        
        
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