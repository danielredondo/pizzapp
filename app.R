library(dplyr)
library(shiny)
library(shinythemes)
library(png)

ui <- fluidPage(theme = shinytheme ("readable"),
                titlePanel(tags$h2(tags$b("Pizzapp")), windowTitle = "Pizzapp"),
                #themeSelector(),
                tags$h4("Creada por ", tags$a("Daniel Redondo.", href = "http://www.danielredondo.com")),
                tags$p("Con tantas marcas, precios y tamaños, a veces es difícil decidir qué pizza comprar.
                       Esta aplicación para amantes de la pizza te ayudará a tomar la mejor decisión
                       mostrándote el precio por centímetro cuadrado de pizza."),
                tags$i(tags$b("Disclaimer"), ": El creador de Pizzapp apoya fuertemente la pizza con piña."),

fluidRow(
  column(8, ""),
  column(4, checkboxInput("trespizzas", "Comparar 3 pizzas", FALSE, width = '400px'))
),

fluidRow(
  column(4, h2("Pizza 1:"),
         fluidRow(column(6, numericInput(inputId = "d1", 
                                      label = "Diámetro en cm:",
                                      value = "25",
                                      width = "100%"
                         )),
                  column(6,numericInput(inputId = "p1", 
                                      label = "Precio :",
                                      value = "12"
                         ))),
                         
                         hr(),
                         
                         uiOutput("img1")
                         
                  ),
                  
                  column(4,
                         h2("Pizza 2:"),
                         fluidRow(column(6, numericInput(inputId = "d2", 
                                                         label = "Diámetro en cm:",
                                                         value = "35",
                                                         width = "100%"
                         )),
                         column(6,numericInput(inputId = "p2", 
                                               label = "Precio :",
                                               value = "17"
                         ))),
                         
                         hr(),
                         
                         uiOutput("img2")
                  ),
                  column(4, 
                         conditionalPanel(condition = "input.trespizzas == true", 
                         h2("Pizza 3:"),
                         fluidRow(column(6, numericInput(inputId = "d3", 
                                                         label = "Diámetro en cm:",
                                                         value = "45",
                                                         width = "100%"
                         )),
                         column(6,numericInput(inputId = "p3", 
                                               label = "Precio :",
                                               value = "25"
                         ))),
                         
                         hr(),
                         
                         uiOutput("img3")
                           
                  )
                )
  
  )

)

server <- function(input, output) {


  output$img1 <- renderUI(tags$div(tags$img(src="pizza.png", width = paste0(100 * input$d1 / max(input$d1, input$d2, input$d3), "%")),
                                   tags$h3(if(input$d1 > 0 & input$p1 > 0) {
                                     # area
                                     HTML(paste0(round(pi * (input$d1 / 2) ^ 2, 2), " cm", tags$sup(2), "<br><br>",
                                                 # Price/cm2
                                                 round(100 * input$p1 / (pi * (input$d1 / 2) ^ 2), 2), " céntimos/cm", tags$sup(2)))
                                   }, style = "position: absolute; top: 40%; left: 50%; transform: translate(-50%, -50%);"),
                                   style = "position: absolute; text-align:center; color: black; font-weight: bold; text-shadow: -1px 0 white, 0 1px white, 1px 0 white, 0 -1px white;"))
  
  output$img2 <- renderUI(tags$div(tags$img(src="pizza.png", width = paste0(100 * input$d2 / max(input$d1, input$d2, input$d3), "%")),
                                     tags$h3(if(input$d2 > 0 & input$p2 > 0) {
                                       # area
                                       HTML(paste0(round(pi * (input$d2 / 2) ^ 2, 2), " cm", tags$sup(2), "<br><br>",
                                                   # Price/cm2
                                                   round(100 * input$p2 / (pi * (input$d2 / 2) ^ 2), 2), " céntimos/cm", tags$sup(2)))
                                     }, style = "position: absolute; top: 40%; left: 50%; transform: translate(-50%, -50%);"),
                                     style = "position: absolute; text-align:center; color: black; font-weight: bold; text-shadow: -1px 0 white, 0 1px white, 1px 0 white, 0 -1px white;"))
  
  output$img3 <- renderUI(tags$div(tags$img(src="pizza.png", width = paste0(100 * input$d3 / max(input$d1, input$d2, input$d3), "%")),
                                     tags$h3(if(input$d3 > 0 & input$p3 > 0) {
                                       # area
                                       HTML(paste0(round(pi * (input$d3 / 2) ^ 2, 2), " cm", tags$sup(2), "<br><br>",
                                                   # Price/cm2
                                                   round(100 * input$p3 / (pi * (input$d3 / 2) ^ 2), 2), " céntimos/cm", tags$sup(2)))
                                     }, style = "position: absolute; top: 40%; left: 50%; transform: translate(-50%, -50%);"),
                                     style = "position: absolute; text-align:center; color: black; font-weight: bold; text-shadow: -1px 0 white, 0 1px white, 1px 0 white, 0 -1px white;"))
  
  }

shinyApp(ui = ui, server = server)