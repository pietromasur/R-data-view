library(quantmod)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(lubridate)

library(readr)
rioc <- read_csv("riocovid.csv")
View(rioc)


rioc$Date <- strptime(rioc$Date, format='%Y-%m-%d')

ne_list <- c('ABOLICAO', 'ACARI', 'AGUA SANTA', 'ALTO DA BOA VISTA', 'ANCHIETA', 'ANDARAI', 'ANIL', 'BANCARIOS', 'BANGU', 'BARRA DA TIJUCA', 'BARROS FILHO', 'BENFICA', 'BENTO RIBEIRO', 'BONSUCESSO', 'BOTAFOGO', 'BRAS DE PINA', 'CACHAMBI', 'CACUIA', 'CAJU', 'CAMORIM', 'CAMPINHO', 'CAMPO DOS AFONSOS', 'CAMPO GRANDE', 'CASCADURA', 'CATETE', 'CATUMBI', 'CAVALCANTE', 'CENTRO', 'CIDADE DE DEUS', 'CIDADE NOVA', 'CIDADE UNIVERSITARIA', 'COCOTA', 'COELHO NETO', 'COLEGIO', 'COMPLEXO DO ALEMAO', 'COPACABANA', 'CORDOVIL', 'COSME VELHO', 'COSMOS', 'COSTA BARROS', 'CURICICA', 'DEL CASTILHO', 'DEODORO', 'ENCANTADO', 'ENGENHO DA RAINHA', 'ENGENHO DE DENTRO', 'ENGENHO NOVO', 'ESTACIO', 'FLAMENGO','FORA DO MUNICIPIO', 'FREGUESIA-ILHA', 'FREGUESIA-JPA', 'GALEAO', 'GAMBOA', 'GARDENIA AZUL', 'GAVEA', 'GLORIA', 'GRAJAU', 'GUADALUPE', 'GUARATIBA', 'HIGIENOPOLIS', 'HONORIO GURGEL', 'HUMAITA', 'INDEFINIDO', 'INHAUMA', 'INHOAIBA', 'IPANEMA', 'IRAJA', 'ITANHANGA', 'JACARE', 'JACAREPAGUA', 'JARDIM AMERICA', 'JARDIM BOTANICO', 'JARDIM CARIOCA', 'JARDIM GUANABARA', 'JARDIM SULACAP', 'JOA', 'LAGOA', 'LARANJEIRAS', 'LEBLON', 'LEME', 'LINS DE VASCONCELOS', 'MADUREIRA', 'MAGALHAES BASTOS', 'MANGUEIRA', 'MANGUINHOS', 'MARACANA', 'MARE', 'MARECHAL HERMES', 'MEIER', 'MONERO', 'OLARIA', 'OSWALDO CRUZ', 'PACIENCIA', 'PADRE MIGUEL', 'PAQUETA', 'PARADA DE LUCAS', 'PARQUE ANCHIETA', 'PAVUNA', 'PECHINCHA', 'PEDRA DA GUARATIBA', 'PENHA', 'PENHA CIRCULAR', 'PIEDADE', 'PILARES', 'PITANGUEIRAS', 'PORTUGUESA', 'PRACA DA BANDEIRA', 'PRACA SECA', 'PRAIA DA BANDEIRA', 'QUINTINO BOCAIUVA', 'RAMOS', 'REALENGO', 'RECREIO DOS BANDEIRANTES', 'RIACHUELO', 'RIBEIRA', 'RICARDO ALBUQUERQUE', 'RIO COMPRIDO', 'ROCHA', 'ROCHA MIRANDA', 'ROCINHA', 'SAMPAIO', 'SANTA CRUZ', 'SANTA TERESA', 'SANTISSIMO', 'SANTO CRISTO', 'SAO CONRADO', 'SAO CRISTOVAO', 'SAO FRANCISCO XAVIER', 'SAUDE', 'SENADOR CAMARA', 'SENADOR VASCONCELOS', 'SEPETIBA', 'TANQUE', 'TAQUARA', 'TAUA', 'TIJUCA', 'TODOS OS SANTOS', 'TOMAS COELHO', 'TURIACU', 'URCA', 'VARGEM GRANDE', 'VARGEM PEQUENA', 'VAZ LOBO', 'VICENTE DE CARVALHO', 'VIDIGAL', 'VIGARIO GERAL', 'VILA DA PENHA', 'VILA ISABEL', 'VILA KENNEDY', 'VILA KOSMOS', 'VILA VALQUEIRE', 'VISTA ALEGRE')

header <- dashboardHeader(
  
  
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Métricas", tabName = "table_1", icon = icon("chart-line")),
    menuItem('Comparando bairros', tabName = 'table_2', icon = icon('chart-bar'))
  )
  
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'table_1',
            fluidRow(
              box(title = 'Selecione Bairro, Classe de dados e Período de análise', width=12, solidHeader = TRUE, status='warning',
                  selectInput(inputId = 'in_table_1', 'Bairros', ne_list, multiple=FALSE),
                  selectInput(inputId = 'dados', "Tipo de dados", c("Casos" = "Cases", "Mortes" = "Deaths", "Recuperados" = "Recovered"), multiple=FALSE),
                  uiOutput("timedate"),
                  actionButton('b_table_1', 'Submeter')
              )
            ),
            fluidRow(
              box(title = "Informações sobre o bairro", width = 12, solidHeader = TRUE,
                  DTOutput('info_table1')
              )
            ),
            fluidRow(
              box(title = "Gráfico de linha", width = 12, solidHeader = TRUE,
                  plotOutput('sh')
              )
            ),
            fluidRow(
              box(title = "Histogramas", width = 12, solidHeader = TRUE,
                  plotOutput('histog_1')
              )
            ),
            fluidRow(
              box(title = "Boxplot", width = 12, solidHeader = TRUE,
                  plotOutput('boxplot_1')
              )
            )
    ),
    tabItem(tabName = 'table_2',
            fluidRow(
              box(title = 'Selecione Bairro, Classe de dados e Período de análise', width=12, solidHeader = TRUE, status='warning',
                  selectInput(inputId = 'in_table_2', 'Bairros', ne_list, multiple=TRUE),
                  selectInput(inputId = 'dados2', "Tipo de dados", c("Casos" = 'Cases', "Mortes" = 'Deaths', "Recuperados" = 'Recovered'), multiple=FALSE),
                  uiOutput("timedate_2"),
                  actionButton('b_table_2', 'Submeter')
              )
            ),
            fluidRow(
              box(title = "Gráfico de linha - Bairro 1", width = 12, solidHeader = TRUE,
                  plotOutput('sh2')
              )
            ),
            fluidRow(
              box(title = "Gráfico de barra", width = 12, solidHeader = TRUE,
                  plotOutput('barplot')
              )
            ),
            fluidRow(
              box(title = "Tabela de Correlação", width = 12, solidHeader = TRUE,
                  DTOutput('info_table2')
              )
            )
    )
  )
  
)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



server <- function(input, output) {
  
  select_ne <- eventReactive(input$b_table_1, {
    ne_name <- input$in_table_1
    twin <- input$true_date
    tipo <- input$dados
    df_rioc <- rioc %>% 
      filter( Neighborhood == ne_name)
    
    df_rioc2 <- df_rioc %>% filter(Date >= twin[1], Date <= twin[2])
    
    
    return(df_rioc2)
  })
  
  select_nenovo <- eventReactive(input$b_table_2, {
    
    ne_name <- input$in_table_2
    twin <- input$true_dated
    tipo2 <- input$dados2
    
    df_rioc <- rioc %>% 
      filter(Neighborhood == ne_name)
    
    df_rioc2 <- df_rioc %>% filter(Date >= twin[1], Date <= twin[2])
    return(df_rioc2)
  })
  
  output$info_table2 <- renderDT({
    Info_DataTable2() %>%
      as.data.frame() %>% 
      DT::datatable(options=list(
        language=list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
        )
      ))
  })
  
  Info_DataTable2 <- eventReactive(input$b_table_2,{
    df2 <- select_nenovo()
    ne_name <- input$in_table_2
    tipo2 <- input$dados2
    df3 <- df2 %>% filter(Neighborhood %in% ne_name[1])
    df4 <- df2 %>% filter(Neighborhood %in% ne_name[2])
    if (tipo2 == "Cases") {
      vec_1 <- df3$Cases
      vec_2 <- df4$Cases
      if (length(vec_1) < length(vec_2)) {
        lv <- length(vec_1)
      } else {
        lv <- length(vec_2)
      }
      cor(x = vec_1[1:lv], y = vec_2[1:lv], use="complete.obs", method="kendall")
    } else {
      if (tipo2 == "Deaths") {
        vec_1 <- df3$Deaths
        vec_2 <- df4$Deaths
        if (length(vec_1) < length(vec_2)) {
          lv <- length(vec_1)
        } else {
          lv <- length(vec_2)
        }
        cor(x = vec_1[1:lv], y = vec_2[1:lv], use="complete.obs", method="kendall")
      } else {
        vec_1 <- df3$Recovered
        vec_2 <- df4$Recovered
        if (length(vec_1) < length(vec_2)) {
          lv <- length(vec_1)
        } else {
          lv <- length(vec_2)
        }
        cor(x = vec_1[1:lv], y = vec_2[1:lv], use="complete.obs", method="kendall")
      }
    }
  })
  
  output$sh <- renderPlot({
    df <- select_ne()
    tipo <- input$dados
    
    aux <- df[, tipo] %>% na.omit()
    aux <- lapply(aux, as.numeric)
    
    aux1 <- min(aux[[1]])
    aux2 <- max(aux[[1]])
    
    df$Date <- ymd(df$Date)
    a <- df %>%
      ggplot(aes(Date, df[, tipo][[1]], group=1)) +
      geom_path() +
      ylab('Valores') +
      coord_cartesian(ylim = c(aux1, aux2)) +
      theme_bw() +
      scale_x_date(date_labels = "%Y-%m-%d")
    
    a
  })
  
  output$timedate_2 <- renderUI({
    ne_name <- input$in_table_2
    df <- rioc %>% 
      filter(Neighborhood %in% ne_name)
    min_time <- min(df$Date)
    max_time <- max(df$Date)
    dateRangeInput("true_dated", "Período de análise",
                   end = max_time,
                   start = min_time,
                   min  = min_time,
                   max  = max_time,
                   format = "yyyy/mm/dd",
                   separator = " - ",
                   language='pt-BR')
  })
  
  output$timedate <- renderUI({
    ne_name <- input$in_table_1
    df <- rioc %>% 
      filter(Neighborhood == ne_name)##
    min_time <- min(df$Date)
    max_time <- max(df$Date)
    dateRangeInput("true_date", "Período de análise",
                   end = max_time,
                   start = min_time,
                   min  = min_time,
                   max  = max_time,
                   format = "yyyy/mm/dd",
                   separator = " - ",
                   language='pt-BR')
  })
  
  Info_DataTable <- eventReactive(input$b_table_1,{
    df <- select_ne()
    tipo <- input$dados
    
    mean <- df %>% select(tipo) %>% colMeans()
    Media <- mean[[1]]
    Mediana <- median(df[[tipo]])
    DesvioPadrao <- sd(df[[tipo]])
    Maior_Valor <- max(df[[tipo]])
    Menor_Valor <- min(df[[tipo]])
    Moda <- getmode(df[[tipo]])
    Bairro <- input$in_table_1
    df_tb <-  data.frame(Bairro, Media, Mediana, DesvioPadrao, Maior_Valor, Menor_Valor, Moda)
    df_tb <- as.data.frame(t(df_tb))
    return(df_tb)
  })
  
  output$info_table1 <- renderDT({
    Info_DataTable() %>%
      as.data.frame() %>% 
      DT::datatable(options=list(
        language=list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
        )
      ))
  })
  
  output$sh2 <- renderPlot({
    df2 <- select_nenovo()
    ne_name <- input$in_table_2
    tipo2 <- input$dados2
    df3 <- df2 %>% filter(Neighborhood %in% ne_name[1])
    df4 <- df2 %>% filter(Neighborhood %in% ne_name[2])
    df3$Date <- ymd(df3$Date)
    df4$Date <- ymd(df4$Date)
    if (tipo2 == "Cases") {
      p = ggplot() + 
        geom_line(data = df3, aes(x = df3$Date, y = as.numeric(df3$Cases)), color = "blue") +
        geom_line(data = df4, aes(x = df4$Date, y = as.numeric(df4$Cases)), color = "red") +
        xlab('Dates') +
        ylab('Cases')
      p
    } else {
      if (tipo2 == "Deaths") {
        q = ggplot() + 
          geom_line(data = df3, aes(x = df3$Date, y = as.numeric(df3$Deaths)), color = "blue") +
          geom_line(data = df4, aes(x = df4$Date, y = as.numeric(df4$Deaths)), color = "red") +
          xlab('Dates') +
          ylab('Deaths')
        q
      } else {
        r = ggplot() + 
          geom_line(data = df3, aes(x = df3$Date, y = as.numeric(df3$Recovered)), color = "blue") +
          geom_line(data = df4, aes(x = df4$Date, y = as.numeric(df4$Recovered)), color = "red") +
          xlab('Dates') +
          ylab('Recovered')
        r
      }
    }
  })
  
  output$histog_1 <- renderPlot({ 
    df <- select_ne()
    tipo <- input$dados
    aux <- df[[tipo]] %>% na.omit() %>% as.numeric()
    aux1 <- min(aux)
    aux2 <- max(aux)
    df$Date <- ymd(df$Date)
    Datas <- df$Date
    Valores <- df[[tipo]]
    a <- ggplot(data = df, aes(x = Datas, y= Valores)) +
      geom_histogram(stat='identity')
    a
    
  })
  
  output$barplot <- renderPlot({
    df2 <- select_nenovo()
    tipo2 <- input$dados2
    ne_name <- input$in_table_2
    df3 <- df2 %>% filter(Neighborhood == ne_name[1])
    df4 <- df2 %>% filter(Neighborhood == ne_name[2])
    df3$Date <- ymd(df3$Date)
    df4$Date <- ymd(df4$Date)
    if (tipo2 == "Cases") {
      media1 <- round(mean(df3$Cases))
      media2 <- round(mean(df4$Cases))
    } else {
      if (tipo2 == "Deaths") {
        media1 <- round(mean(df3$Deaths))
        media2 <- round(mean(df4$Deaths))
      } else {
        media1 <- round(mean(df3$Recovered))
        media2 <- round(mean(df4$Recovered))
      }
    }
    dat <- data.frame(
      Bairros = factor(c(as.character(ne_name[1]),as.character(ne_name[2])), levels=c(as.character(ne_name[1]),as.character(ne_name[2]))),
      Medias = c(media1, media2)
    )
    p <- ggplot(data=dat, aes(x=Bairros, y=Medias)) +
      geom_bar(stat="identity")
    p
  })
  
  output$boxplot_1 <- renderPlot({
    df <- select_ne()
    tipo <- input$dados
    
    mean <- mean(df[,tipo][[1]])
    mediana <- median((df[,tipo][[1]]))
    desvioPadrao <- sd((df[,tipo][[1]]))
    
    maior_Valor <- max(df[,tipo][[1]])
    menor_Valor <- min(df[,tipo][[1]])
    g <- ggplot(df, aes(x= "", y=as.numeric(df[,tipo][[1]]))) +
      geom_boxplot() + geom_jitter(position=position_jitter(width=NULL, height=NULL), colour = "red") +
      ylab('Valores')
    g
  })
  
}


ui <- dashboardPage(header, sidebar, body)

shinyApp(
  ui = ui,
  server = server
)