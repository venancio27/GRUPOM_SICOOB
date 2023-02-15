library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(plotly)

dados = read_excel("SICOOB_DADOS.xlsx") 
dados$ANO = as.factor(dados$ANO)

#### Itens do UI

# Cabeçalho
Header = dashboardHeader(
  title = img(src = "logo_sicoob.png", height = 40))

# Side bar
Barra_Lateral = dashboardSidebar(
  sidebarMenu(
    # Aba Metodologia
    menuItem(text = "Metodologia",tabName = "metodologia",icon = icon("book"),
             badgeLabel = "Em construção", badgeColor = "yellow"),
    
    # Aba Dashboard Único
    menuItem(text = "Dashboard",tabName = "dashboard",icon = icon("chart-simple")),
    
    # Qualitativo
    menuItem(text = "Qualitativo",tabName = "quali",icon = icon("comment"),
             badgeLabel = "Em construção", badgeColor = "yellow")
    
  
  ),
  img(src = "grupom.png", height = 30)
  
  )

# Corpo do dash

Corpo = dashboardBody(
  tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #09393C;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #499588;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #499588;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #499588;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #A3C01C;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #ADD2C6;
                              color: #000000;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #496D6C;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #496D6C;
                              }
                              '))),
  
  
  
  tabItems(
    
    ##### Metodologia
    tabItem(tabName = "metodologia"), # Fim da Metodologia
    
    ##### Dash Único
    tabItem(tabName = "dashboard",
            
            ### Box de Filtro
            
            box(width = 12,height = 230,status = "success",
                
                
                # Filtro Estado
                
                box(width = 6,
                    
                    radioButtons(inputId = "estado",
                                 label = h3("Estado"),
                                 choices = list("Todos",
                                                "Goiás",
                                                "Minas Gerais"),
                                 selected = c("Todos"))
                    
                ),
                
                # Filtro Tipo de Conta
                
                box(width = 6, 
                    
                    radioButtons(inputId = "tipo_conta",
                                       label = h3("Tipo de conta"),
                                       choices = list("Todas"  ,
                                                      "Large"  ,
                                                      "PF" ,
                                                      "PJ" ),
                                       selected = c("Todas"))
                    
                ),
                
            ), # Fim do BOX de filtro
            
            fluidRow( # Primeira linha
              
            ### Utilização
            
            box(width = 6,collapsible = T,
                
                plotOutput(outputId = "utiliza_geral",  width = "100%")
                ),
            
            ### Nota média
            
            column(width = 6,
                    
                   box(width = 12,collapsible = T,
                   box(width = 12,height = 250,
                         
                   plotOutput(outputId = "satisfacao_media",  width = "100%")),
                
                   valueBoxOutput(outputId = "variacao_satisfacao",width = 12)
                
                   ))
            
            ), # Fim da primeira linha
            
            ### Segunda Linha
            
            fluidRow(
              
              box(width = 12,height = 600,
                  h1("NET PROMOTER SCORE"),
                  
                  box(width = 12,height = 280,
                      
              # Gráficos
              column(width = 6,
                     
                  
                     plotOutput(outputId = "nps_coluna",  width = "100%")),
              
              
              column(width = 6,
                     
                     plotOutput(outputId = "nps_linha",  width = "100%"))),
                     
              
              # Value Box

                     
                     valueBoxOutput(outputId = "nps_variacao",width = 12)
                     
              ),
    
          
              
              
            )
            
            
            
    ), # Fim do dash
    
    # Qualitativo
    tabItem(tabName = "quali") # Fim do Quali
  )
)



# Define UI for application that draws a histogram
ui <- dashboardPage(
  header = Header,
  sidebar = Barra_Lateral,
  body = Corpo
)









#
#
#
#
#        SERVER
#
#
#
#
#









# Define server logic required to draw a histogram
server <- function(input, output) {
  
  options(OutDec = ",")
  
  
  ####### FILTRAGEM DE DADOS
  
  
  ### Utilização
  
  dados_reactive <- reactive({
    
    
    estado_filter = switch(input$estado,
                           "Todos" = c("GO","MG"),
                           "Goiás" = "GO",
                           "Minas Gerais" = "MG")
    
    
    tipoconta_filter =  switch(input$tipo_conta,
                               "Todas" = c("PF","PJ","LARGE"),
                               "PF" = "PF",
                               "PJ" = "PJ",
                               "Large" = "LARGE"
    )
    
    filtro = dados %>% filter(ESTADO %in% estado_filter & TIPO_CONTA %in% tipoconta_filter)
    
    
    return(filtro)
  })
  
  
  ### Utiliza Geral
  
  df_utiliza <- reactive({
    
    estado1_filter = switch(input$estado,
                           "Todos" = "GERAL",
                           "Goiás" = "GO",
                           "Minas Gerais" = "MG")
    
    
    UTILIZA = summarise(group_by(dados_reactive(), UTILIZACAO,ANO),
                        n_ponderado = sum(PONDERACAO))
    
    ANO_UTILIZA = summarise(group_by(dados_reactive(), ANO),
                            amostra_tamanho = sum(PONDERACAO))
    
    df_utiliza_filtrado = left_join(UTILIZA,ANO_UTILIZA,"ANO") %>% mutate(prop=n_ponderado/amostra_tamanho)
    
    return(df_utiliza_filtrado)
  })
  
  ### Nota média
  
  dados_satisf <- reactive({
    
    estado1_filter = switch(input$estado,
                            "Todos" = "GERAL",
                            "Goiás" = "GO",
                            "Minas Gerais" = "MG")
    
    
    tipoconta_filter =  switch(input$tipo_conta,
                               "Todas" = c("PF","PJ","LARGE"),
                               "PF" = "PF",
                               "PJ" = "PJ",
                               "Large" = "LARGE"
    )
    
    satisf_nao_nula = dados %>% filter(is.na(NOTA_ATENDIMENTO) == F & TIPO_CONTA %in% tipoconta_filter)
    dados_satisf_2023 = satisf_nao_nula %>% filter(ANO == "2023") %>% mutate(NOTA_ATENDIMENTO_POND = NOTA_ATENDIMENTO*PONDERACAO)
    
    SATISFACAO_GERAL = summarise(group_by(satisf_nao_nula, ANO),
                                 n_ponderado = sum(PONDERACAO),
                                 media = mean(NOTA_ATENDIMENTO),
                                 ESTADO = "GERAL")
    
    nota_2023 = sum(dados_satisf_2023$NOTA_ATENDIMENTO_POND)/sum(dados_satisf_2023$PONDERACAO)
    SATISFACAO_GERAL$media[2] = nota_2023 
    
    SATISFACAO_ESTADO = summarise(group_by(satisf_nao_nula ,ESTADO,ANO),
                                  n_ponderado = sum(PONDERACAO),
                                  media = mean(NOTA_ATENDIMENTO))
    
    dadosgoias = dados_satisf_2023  %>% filter(ESTADO == "GO")
    NOTA_GO_2023 = sum(dadosgoias$NOTA_ATENDIMENTO_POND)/sum(dadosgoias$PONDERACAO)
    
    dadosmg = dados_satisf_2023  %>% filter(ESTADO == "MG")
    NOTA_MG_2023 = sum(dadosmg$NOTA_ATENDIMENTO_POND)/sum(dadosmg$PONDERACAO)
    
    SATISFACAO_ESTADO$media[2] = NOTA_GO_2023
    SATISFACAO_ESTADO$media[4] = NOTA_MG_2023
    
    SATISFACAO_JUNCAO = rbind(SATISFACAO_ESTADO,SATISFACAO_GERAL) %>% filter(ESTADO == estado1_filter)
    
    SATISFACAO_JUNCAO$ESTADO = as.factor(SATISFACAO_JUNCAO$ESTADO)
    SATISFACAO_JUNCAO = as.data.frame(SATISFACAO_JUNCAO)
    return(SATISFACAO_JUNCAO)
  })
  
  
  ### NPS
  
  df_nps <- reactive({
    
    estado_filter = switch(input$estado,
                           "Todos" = c("GO","MG"),
                           "Goiás" = "GO",
                           "Minas Gerais" = "MG")
    
    tipoconta_filter =  switch(input$tipo_conta,
                               "Todas" = c("PF","PJ","LARGE"),
                               "PF" = "PF",
                               "PJ" = "PJ",
                               "Large" = "LARGE"
    )
    
    dados_nps = dados %>% filter(is.na(NPS1) == F & TIPO_CONTA %in% tipoconta_filter & ESTADO %in% estado_filter) 
    
    NPS_DATA1 = summarise(group_by(dados_nps, NPS1,ANO),
                          n_ponderado = sum(PONDERACAO))
    
    NPS_DATA2 = summarise(group_by(dados_nps, ANO),
                          amostra_tamanho = sum(PONDERACAO))
    
    NPS_DATA3 = left_join(NPS_DATA1,NPS_DATA2,"ANO") %>% mutate(prop=n_ponderado/amostra_tamanho)
    
    NPS_DATA3 = as.data.frame(NPS_DATA3)
    
    return(NPS_DATA3)
  })
  
  
  nps_final  <- reactive({
    
    prom2023 = as.numeric(df_nps() %>% select(c(NPS1,ANO,prop)) %>% filter(ANO == 2023 & NPS1 == "Promotores") %>% select(c(prop)))
    det2023 = as.numeric(df_nps() %>% select(c(NPS1,ANO,prop)) %>% filter(ANO == 2023 & NPS1 == "Detratores") %>% select(c(prop)))
    
    prom2022 = as.numeric(df_nps() %>% select(c(NPS1,ANO,prop)) %>% filter(ANO == 2022 & NPS1 == "Promotores") %>% select(c(prop)))
    det2022 = as.numeric(df_nps() %>% select(c(NPS1,ANO,prop)) %>% filter(ANO == 2022 & NPS1 == "Detratores") %>% select(c(prop)))
    
    prom2023 = ifelse(is.na(prom2023) == T,0,prom2023)
    prom2022 = ifelse(is.na(prom2022) == T,0,prom2022)
    det2023 = ifelse(is.na(det2023) == T,0,det2023)
    det2022 = ifelse(is.na(det2022) == T,0,det2022)
    
    
    NPS2023 = round(100*(prom2023 - det2023),0)
    NPS2022 = round(100*(prom2022 - det2022),0)

    dados_nps_final = data.frame(ANO = as.factor(c(2022,2023)),
                                 NPS = c(NPS2022,NPS2023))
    
    return(dados_nps_final)
  })
  
  
  
  
  ######################## GRÁFICOS
  
  ### UTILIZA GERAL
  
  output$utiliza_geral <- renderPlot({
    
    ggplot(df_utiliza()) +
      aes(x = UTILIZACAO, fill = ANO, weight = prop) +
      geom_bar(position = "dodge") +
      scale_fill_manual(
        values = c(`2022` = "#06823E",
                   `2023` = "#0C208E")
      ) +
      theme_light()+
      labs(
        x = "Utilização",
        y = "Porcentagem",
        title = "O Sicoob Engrecred é a instituição financeira que você:",
        fill = "Ano"
      ) +
      theme(
        plot.title = element_text(size = 16L,
                                  hjust = 0.5),
        axis.title.y = element_text(size = 13L),
        axis.title.x = element_text(size = 13L)
      )+
      scale_y_continuous(limits = c(0,1),breaks = seq(0,1,0.2), labels = c("0%","20%","40%","60%","80%","100%"))+
      geom_text(aes( y=prop, label=paste(round(100*prop,1),"%",sep="") ), position=position_dodge(0.9), vjust=-0.5)
    
    
  }, width = 520,height = 370)
  
  ### SATISFAÇÃO
  
  
  output$satisfacao_media <- renderPlot({
    
    cor_estado = switch(input$estado,
                        "Todos" = "#4B3C8F",
                        "Goiás" = "#019D92",
                        "Minas Gerais" = "#c1d101")

    ggplot(data = dados_satisf(), aes(x = as.factor(ANO), y = media,group = ESTADO))+
      geom_line(linetype = "dashed", color = cor_estado,linewidth = 1.5)+
      geom_point(size = 3.5, color = "#00353D")+
      scale_y_continuous(limits = c(7.25,10),breaks = seq(7.25,10,0.5))+
      theme_minimal()+
      theme(
        plot.title = element_text(size = 15L,
                                  hjust = 0.5),
        axis.title.y = element_text(size = 13L),
        axis.title.x = element_text(size = 13L)
      )+
      geom_text(aes(label = round(media,1), hjust = -0.3, vjust = 1.5),size = 6, fontface = "bold")+
      labs(y = "Nota média",x = "Ano",
           title = paste("Nota média do Atendimento: Estado -",input$estado,"e Tipo de conta -",input$tipo_conta))
    
    
  }, height = 250)
  
  ### VALUE BOX SATISFACAO
  
  output$variacao_satisfacao <- renderValueBox({
    
  
    cor_vari <- reactive({  
      
      ifelse(dados_satisf()$media[2] - dados_satisf()$media[1] < 0, "red",
             ifelse(dados_satisf()$media[2] - dados_satisf()$media[1] == 0,"yellow","green"))
    })

    subtitulo = switch (cor_vari(),
                        "red" = "Diminuiu",
                        "yellow" = "Manteve",
                        "green" = "Aumentou"
    )
    
    img_icon = switch (cor_vari(),
                        "red" = "thumbs-down",
                        "yellow" = "equals",
                        "green" = "thumbs-up"
    )
    
    valueBox(
      value = round(dados_satisf()$media[2] - dados_satisf()$media[1],1), subtitulo, icon = icon(img_icon, lib = "glyphicon"),
      color = cor_vari()
    )
    
  })
  
  
  ### NPS COLUNA
  
  output$nps_coluna <- renderPlot({

    
    ggplot(df_nps()) +
      aes(x = as.factor(ANO), y = prop, fill = NPS1) +
      geom_col() +
      labs(title = paste("NPS: Estado -",input$estado,"e Tipo de conta -",input$tipo_conta))+
      scale_fill_manual(
        values = c(Detratores = "#DD2316",
                   Neutros = "#BFD609",
                   Promotores = "#038E07")
      ) +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
            plot.title = element_text(size = 15L,
                                      hjust = 0.5),
            axis.title.y = element_text(size = 13L),
            axis.title.x = element_text(size = 13L))+
      geom_text(aes(label=paste(round(100*prop,1),"%",sep="")),
                position=position_fill(vjust=0.5),size = 5,fontface = "bold")+
      labs(x = "Ano", y = NULL, fill = NULL)
    
  }, height = 270)
  
  ### NPS LINHA
  
  output$nps_linha <- renderPlot({
    
    cor_estado = switch(input$estado,
                        "Todos" = "#4B3C8F",
                        "Goiás" = "#019D92",
                        "Minas Gerais" = "#c1d101")
    
    ggplot(nps_final()) +
      aes(x= ANO, group = 1 , y = NPS) +
      geom_line(colour = cor_estado,linewidth = 1.5)+
      geom_point(size = 3.5, color = "#00353D")+
      scale_y_continuous(limits = c(0,100),breaks = seq(0,100,5))+
      theme_minimal()+
      theme(
        plot.title = element_text(size = 15L,
                                  hjust = 0.5),
        axis.title.y = element_text(size = 13L),
        axis.title.x = element_text(size = 13L)
      )+
      geom_text(aes(label = round(NPS,1), hjust = -0.3, vjust = 1.5),size = 6, fontface = "bold")+
      labs(y = "NPS (Promotores - Detratores)",x = "Ano",
           title = "NPS: Evolução")
    
    
  }, height = 250)
  
  output$nps_variacao <- renderValueBox({
    
    
    cor_vari_nps <- reactive({  
      
      ifelse(nps_final()$NPS[2] - nps_final()$NPS[1] < -0.5, "red",
             ifelse(nps_final()$NPS[2] - nps_final()$NPS[1] > 0.5,"green","yellow"))
    })
    
    subtitulo = switch (cor_vari_nps(),
                        "red" = "Diminuiu",
                        "yellow" = "Manteve",
                        "green" = "Aumentou"
    )
    
    img_icon = switch (cor_vari_nps(),
                       "red" = "thumbs-down",
                       "yellow" = "equals",
                       "green" = "thumbs-up"
    )
    
    valueBox(
      value = round(nps_final()$NPS[2] - nps_final()$NPS[1],1), subtitulo, icon = icon(img_icon),
      color = cor_vari_nps()
    )
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
