library(shiny)
library(lubridate)
library(colorspace)
library(rsconnect)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(readxl)
library(DT)

covid <- read_excel("Boletim Artur Nogueira.xlsx")

ui <- dashboardPage(
dashboardHeader(title = "Artur Nogueira"),  

dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Base de dados", tabName = "tabela", icon = icon("dashboard")),
        menuItem("Saiba quem somos", icon = icon("send",lib='glyphicon'), 
                 href = "https://www.linkedin.com/in/matheusduzziribeiro/")
    )
),

dashboardBody(
    tabItems(
        tabItem("dashboard",
                fluidRow(
                    valueBoxOutput("value1")
                    ,valueBoxOutput("value2")
                    ,valueBoxOutput("value3")
                    ,valueBoxOutput("value4")
                    ,valueBoxOutput("value5")
                    ,valueBoxOutput("value6")
                ),
                fluidRow(
                    
                    box(
                        title = "Acompanhamento dos casos monitorados"
                        ,status = "primary"
                        ,solidHeader = TRUE 
                        ,collapsible = TRUE 
                        ,plotOutput("revenuebyPrd", height = "300px")
                    ),
                    box(
                        title = "Acompanhamento dos casos descartados"
                        ,status = "primary"
                        ,solidHeader = TRUE 
                        ,collapsible = TRUE 
                        ,plotOutput("revenuebyPrd2", height = "300px")
                    )
                )
        ),
        tabItem("tabela",
                fluidRow( h2('Dados do boletim diário de Artur Nogueira'),
                          DT::dataTableOutput("mytable")
                )
                )
    )
)
)

server <- function(input, output) { 
    
    #manipulacao
    n <- length(covid$monitorados)
    primeirobox <- covid$monitorados[n]
    segundobox <- covid$aguardando[n]
    terceirobox <- sum(covid$descartados)
    quartobox <- covid$positivos[n]
    quintobox <- covid$negativos[n]
    sextobox <- covid$positivos[n]
    
    #creating the valueBoxOutput content
    output$value1 <- renderValueBox({
        valueBox(
            formatC(primeirobox, format="d")
            ,'Número de casos monitorados'
            ,color = "purple")
        
    })
    
    
    output$value2 <- renderValueBox({
        
        valueBox(
            formatC(segundobox, format="d")
            ,'Aguardando resultado'
            ,color = "green")
        
    })
    
    
    
    output$value3 <- renderValueBox({
        
        valueBox(
            formatC(terceirobox, format="d")
            ,'Total de casos já descartados'
            ,color = "yellow")
    })
    
    
    
    output$value4 <- renderValueBox({
        valueBox(
            formatC(quartobox, format="d")
            ,'Casos de exame com resultado positivo'
            ,color = "purple")
        
    })
    
    output$value5 <- renderValueBox({
        valueBox(
            formatC(quintobox, format="d")
            ,'Casos de exame com resultado negativo'
            ,color = "green")
        
    })
    
    output$value6 <- renderValueBox({
        valueBox(
            formatC(sextobox, format="d")
            ,'Casos de exame com resultado positivo'
            ,color = "yellow")
        
    })
    
    #creating the plotOutput content
    
    output$revenuebyPrd <- renderPlot({
        ggplot(data = covid, 
               aes(x=dias, y=monitorados)) + 
            geom_bar(position = "dodge", stat = "identity") + ylab("Casos monitorados") + 
            xlab("Dias") + theme(legend.position="bottom" 
                                 ,plot.title = element_text(size=15, face="bold")) + scale_x_continuous(breaks=seq(1,n,1))
    })
    
    output$revenuebyPrd2 <- renderPlot({
        ggplot(data = covid, 
               aes(x=dias, y=descartados)) + 
            geom_bar(position = "dodge", stat = "identity") + ylab("Casos descartados") + 
            xlab("Dias") + theme(legend.position="bottom" 
                                 ,plot.title = element_text(size=15, face="bold")) + scale_x_continuous(breaks=seq(1,n,1))
        
    })
    output$mytable = DT::renderDataTable({
        covid
    })
    
}


shinyApp(ui, server)

