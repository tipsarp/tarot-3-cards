library(shiny)

tarot_deck <- read.csv("tarot_df.csv", row.names = "X")

ui <- fluidPage(
  titlePanel("Simple Tarot Reading Platform"),
  
  sidebarLayout(
    position = "right",
    sidebarPanel(
      checkboxInput("allow_reversals", "Include Card Reversals", value = TRUE),
      actionButton("draw_btn", "Draw Three Cards"),
      hr(),
      tags$div(
        id = "copy_section",
        textOutput("card_string_display"),
        tags$br(),
        tags$button("Copy Reading Text", onclick = "navigator.clipboard.writeText(document.getElementById('hidden_text').innerText); alert('Copied to clipboard!');"),
        tags$div(id = "hidden_text", style = "display:none;", textOutput("raw_card_string", container = span))
      )
    ),
    
    mainPanel(
      h3("Your Cards"),
      fluidRow(
        column(4, uiOutput("card1_ui")),
        column(4, uiOutput("card2_ui")),
        column(4, uiOutput("card3_ui"))
      ),
      hr(),
      h3("Details"),
      uiOutput("details_ui")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values to store drawn card indices and orientation (TRUE = upright, FALSE = reversed)
  current_draw <- reactiveVal(c(1, 2, 3))
  current_orient <- reactiveVal(c(TRUE, TRUE, TRUE))
  
  observeEvent(input$draw_btn, {
    sampled <- sample(1:nrow(tarot_deck), 3)
    current_draw(sampled)
    
    if (input$allow_reversals) {
      orientations <- sample(c(TRUE, FALSE), 3, replace = TRUE)
    } else {
      orientations <- c(TRUE, TRUE, TRUE)
    }
    current_orient(orientations)
  })
  
  # Helper to construct card labels with orientation
  get_card_label <- function(index, upright) {
    name <- tarot_deck$name[index]
    if (upright) name else paste0(name, " (Reversed)")
  }
  
  card_string_val <- reactive({
    drawn_indices <- current_draw()
    orientations <- current_orient()
    
    c1 <- get_card_label(drawn_indices[1], orientations[1])
    c2 <- get_card_label(drawn_indices[2], orientations[2])
    c3 <- get_card_label(drawn_indices[3], orientations[3])
    
    paste0("1. ", c1, " | 2. ", c2, " | 3. ", c3)
  })
  
  output$card_string_display <- renderText({
    card_string_val()
  })
  
  output$raw_card_string <- renderText({
    card_string_val()
  })
  
  # Helper function to render an individual card UI with optional flip rotation
  render_single_card <- function(card_idx, is_upright) {
    card <- tarot_deck[card_idx, ]
    card_name_display <- if (is_upright) card$name else paste0(card$name, " (Reversed)")
    img_style <- if (is_upright) "width: 160px; height: auto;" else "width: 160px; height: auto; transform: rotate(180deg);"
    
    tagList(
      tags$img(src = paste0("cards/", card$file), style = img_style),
      h4(card_name_display)
    )
  }
  
  output$card1_ui <- renderUI({
    render_single_card(current_draw()[1], current_orient()[1])
  })
  
  output$card2_ui <- renderUI({
    render_single_card(current_draw()[2], current_orient()[2])
  })
  
  output$card3_ui <- renderUI({
    render_single_card(current_draw()[3], current_orient()[3])
  })
  
  # Render details at the bottom reflecting upright/reversed status
  output$details_ui <- renderUI({
    drawn_indices <- current_draw()
    orientations <- current_orient()
    drawn <- tarot_deck[drawn_indices, ]
    
    tagList(
      lapply(1:3, function(i) {
        is_upright <- orientations[i]
        card_title <- if (is_upright) drawn$name[i] else paste0(drawn$name[i], " (Reversed)")
        card_meaning <- if (is_upright) drawn$upright[i] else drawn$reversed[i]
        
        tags$div(
          h4(paste0("Card ", i, ": ", card_title)),
          tags$p(strong("Numerology: "), drawn$number[i]),
          tags$p(strong("Element: "), drawn$element[i]),
          tags$p(strong("Astrological Sign / Planet: "), drawn$astrology[i]),
          tags$p(strong("Orientation: "), if (is_upright) "Upright" else "Reversed"),
          tags$p(strong("Meaning & Interpretation: "), if (is_upright) drawn$upright[i] else drawn$reversed[i]),
          tags$hr()
        )
      })
    )
  })
}

shinyApp(ui = ui, server = server)
