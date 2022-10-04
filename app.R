#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny, data.table, tidyverse, bslib, here)
m <- as.matrix(fread(here("data", "dw_tactics_matrix.txt")))
m <- round(m, 3)
colnames(m) <- NULL
m <- 
  m %>% 
  reshape2::melt() %>%
  rename(You = Var1, Opponent = Var2) 

### Navbarpage
ui <- navbarPage(
  "Dicewars tactical ledger",
  theme = bs_theme(bootswatch = "sandstone"),
  tabPanel(
    "Ledger",
    sidebarLayout(
      sidebarPanel(width = 2,
                   selectInput(inputId = "you_dice", 
                               label = "#Dice (you)", 
                               choices = 1:8, 
                               selected = 1, 
                               multiple = F),
                   selectInput(inputId = "opponent_dice", 
                               label = "#Dice (opponent)", 
                               choices = 1:8, 
                               selected = 1, 
                               multiple = F)
                   ),
      mainPanel(plotOutput("dicewars_ledger")),
    )
  ),
  tabPanel(
    "About",
    sidebarLayout(
      sidebarPanel(
        HTML(
          paste(
            h4("A successful attack is defined as the event where the sum of your dice throw is strictly greater than the sum of your opponent's dice throw."),
            "<br/>",
            h4("The cells of the heatmap correspond to the probability of a successful attack for a given pair of number of dice."),
            "<br/>",
            h4("The probabilities are based off 1,000,000 simulated games."),
            "<br/>",
            a(h4("Play Dicewars at GameDesign"), href = "https://www.gamedesign.jp/games/dicewars/"),
            "<br/>",
            a(h4("Source code"), href = "https://github.com/thorj/dicewars_ledger")
          )
        )),
      mainPanel()
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$dicewars_ledger <- renderPlot({ 
      m %>%
        ggplot(aes(x = You, y = Opponent, fill = value)) + 
        geom_tile(color = "white") +
        geom_tile(data = m %>% 
                    filter(You == input$you_dice, Opponent == input$opponent_dice),
                  color = "red",
                  fill = "red") +
        geom_text(aes(label = value), color = "white", fontface = "bold") +
        scale_x_continuous(breaks = 1:8) +
        scale_y_continuous(breaks = 1:8) +
        coord_cartesian(expand = F) +
        labs(x = "#Dice (you)",
             y = "#Dice (opponent)") +
        theme(legend.position = "none",
              text = element_text(size = 20))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
