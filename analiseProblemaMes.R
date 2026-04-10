> library(shiny)
> library(dplyr)
> library(ggplot2)
> library(tidytext)
> 
> # preparar dados
> dados <- Abertura_de_Fichas_Ouvidoria_Solicitação_Manutenção |>
+     mutate(
+         data = as.Date(data, format = "%d/%m/%Y"),
+         mes = format(data, "%Y-%m")
+     )
> 
> ui <- fluidPage(
+     titlePanel("Problemas por mês"),
+     
+     sidebarLayout(
+         sidebarPanel(
+             selectInput(
+                 "mes_escolhido",
+                 "Escolha o mês:",
+                 choices = unique(dados$mes)
+             )
+         ),
+         
+         mainPanel(
+             plotOutput("grafico")
+         )
+     )
+ )
> 
> server <- function(input, output) {
+     
+     output$grafico <- renderPlot({
+         
+         dados |>
+             filter(mes == input$mes_escolhido) |>
+             count(tipoProblema, sort = TRUE) |>
+             slice_max(n, n = 5) |>
+             ggplot(aes(x = reorder(tipoProblema, n), y = n)) +
+             geom_bar(stat = "identity") +
+             coord_flip() +
+             labs(
+                 title = paste("Top 5 problemas -", input$mes_escolhido),
+                 x = "Tipo de problema",
+                 y = "Quantidade"
+             )
+     })
+ }
> 
> shinyApp(ui, server)
