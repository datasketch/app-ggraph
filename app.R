library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(V8)
library(dsmodules)
library(hotr)
library(tidygraph)
library(tidyverse)
library(ggraph)
library(stringr)
library(shinycustomloader)



ui <- panelsPage(useShi18ny(),
                 tags$head(tags$style(HTML("
                 #tab {
                 margin-bottom: 27px;
                 }
                 #tab div.shiny-options-group {
                 display: flex;
                 }
                 #tab div.radio label input + span {
                 border-radius: 0.35rem;
                 cursor: pointer;
                 margin: 6px 2px 6px 0;
                 padding: 10px;
                 }
                 #tab div.radio label input:checked + span {
                 background-color: #B70F7F;
                 color: #ffffff;
                 font-size: 13px;
                 font-weight: 700;
                 letter-spacing: 0.7px;
                 }
                 #tab input[type='radio'] {
                 display: none;
                 }"))),
                 panel(title = ui_("upload_data"),
                       width = 200,
                       body = uiOutput("table_input")),
                 panel(title = ui_("dataset"),
                       width = 300,
                       body = div(radioButtons("tab", "", c("connections", "nodes")),
                                  uiOutput("data_preview"))),
                 panel(title = ui_("options"),
                       width = 250,
                       color = "chardonnay",
                       body = uiOutput("controls")),
                 panel(title = ui_("viz"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(langSelectorInput("lang", position = "fixed"),
                                  uiOutput("download"),
                                  br(),
                                  withLoader(plotOutput("result", height = "80vh"), type = "image", loader = "loading_gris.gif"))))
                       #            plotOutput("result", height = "80vh",)
                       #            shinypanels::modal(id = "download",
                       #                               title = ui_("download_net"),
                       #                               uiOutput("modal"))),
                       # footer = shinypanels::modalButton(label = "Download network", modal_id = "download")))



server <- function(input, output, session) {
  
  i18n <- list(defaultLang = "en", availableLangs = c("es", "en", "pt_BR"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = FALSE)
  observeEvent(lang(), {uiLangUpdate(input$shi18ny_ui_classes, lang())})  
  
  data_input <- reactiveValues(up = NULL,
                               cn = NULL,
                               nd = NULL)
  
  output$table_input <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload", "google"), lang = lang())
    tableInputUI("initial_data",
                 choices = choices,
                 selected = ifelse(is.null(input$`initial_data-tableInput`), "sampleData", input$`initial_data-tableInput`))
  })
  
  labels <- reactive({
    req(input$tab)
    sm_f <- i_(c(#"sample_ch_cn_0", 
      "sample_ch_cn_1"), lang())
    names(sm_f) <- i_(c(#"sample_ch_nm_0",
      "sample_ch_nm_1"), lang())
    
    list(sampleLabel = i_("sample_lb", lang()), 
         sampleFile = sm_f,
         sampleSelected = input$`initial_data-inputDataSample`, 
         
         pasteLabel = i_("paste", lang()),
         pasteValue = "", 
         pastePlaceholder = i_("paste_pl", lang()), 
         pasteRows = 5, 
         
         uploadLabel = i_("upload_lb", lang()), 
         uploadButtonLabel = i_("upload_bt_lb", lang()),
         uploadPlaceholder = i_("upload_pl", lang()),
         
         googleSheetLabel = i_("google_sh_lb", lang()), 
         googleSheetValue = "",
         googleSheetPlaceholder = i_("google_sh_pl", lang()),
         googleSheetPageLabel = i_("google_sh_pg_lb", lang()))
  })
  
  observeEvent(list(labels(), input$`initial_data-tableInput`), {
    data_input$up <- do.call(callModule, c(tableInput, "initial_data", labels()))
  })
  
  output$data_preview <- renderUI({
    if (input$tab == "connections") {
      uiOutput("connections_preview")
    } else {
      uiOutput("nodes_preview")
    }
  })
  
  observe({
    req(input$`initial_data-tableInput`, data_input$up)
    d0 <- data_input$up()
    if (!is.null(d0)) {
      data_input$cn <- d0
      if (all(c("to", "from") %in% names(d0))) {
        data_input$nd <- data.frame(id = unique(c(d0$to, d0$from)))
      }
    }
  })
  
  output$connections_preview <- renderUI({
    req(data_input$cn)
    suppressWarnings(hotr("hotr_cn_input", data = data_input$cn, order = NULL, options = list(height = "80vh"), enableCTypes = FALSE))
  })
  
  output$nodes_preview <- renderUI({
    req(data_input$nd)
    suppressWarnings(hotr("hotr_nd_input", data = data_input$nd, order = NULL, options = list(height = "80vh"), enableCTypes = FALSE))
  })
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "choices", "text"))})
  output_parmesan("controls", 
                  parmesan = parmesan_lang,
                  input = input,
                  output = output,
                  env = environment())
  
  observeEvent(lang(), {
    ch0 <- as.character(parmesan$nodes$inputs[[1]]$input_params$choices)
    names(ch0) <- i_(ch0, lang())
    ch1 <- as.character(parmesan$network$inputs[[2]]$input_params$choices)
    names(ch1) <- i_(ch1, lang())
    # ch2 <- as.character(parmesan$edges$inputs[[3]]$input_params$choices)
    # names(ch2) <- i_(ch2, lang())
    ch3 <- c("connections", "nodes")
    names(ch3) <- toupper(i_(ch3, lang()))
    updateSelectizeInput(session, "nd_shape", choices = ch0, selected = input$nd_shape)
    updateSelectizeInput(session, "layout", choices = ch1, selected = input$layout)
    # updateRadioButtons(session, "ed_arrows", choices = ch2, selected = input$ed_arrows)
    updateRadioButtons(session, "tab", choices = ch3, selected = input$tab)
  })
  
  cols_cn <- reactive({
    c0 <- c("no", names(data_input$cn))
    names(c0) <- c(i_("no_lb", lang()), names(data_input$cn))
    c0
  })
  
  cols_nd <- reactive({
    c0 <- c("no", names(data_input$nd))
    names(c0) <- c(i_("no_lb", lang()), names(data_input$nd))
    c0
  })
  
  cn <- reactive({
    hotr_table(input$hotr_cn_input)
  })
  
  nd <- reactive({
    hotr_table(input$hotr_nd_input)
  })
  
  ntwrk <- reactive({
    req(data_input$cn, data_input$nd)
    nd <- data_input$nd
    cn <- data_input$cn
    # nd <- nd()
    # cn <- cn()
    nd_lb <- cn_lb <- ""
    if (input$nd_lb != "no") {
      nd_lb <- str_wrap(nd[[input$nd_lb]], input$nd_lb_wrap)
    }
    if (input$ed_lb != "no") {
      cn_lb <- str_wrap(cn[[input$ed_lb]], input$ed_lb_wrap)
    }
    t0 <- tbl_graph(nd, cn)
    ggraph(t0, layout = input$layout) +
      labs(title = input$title) +
      # geom_edge_link(aes_string(label = ifelse(input$ed_lb == "no", NA, input$ed_lb)),
      geom_edge_link(aes(label = cn_lb),
                     angle_calc = "along",
                     colour = input$ed_color,
                     width = input$ed_size,
                     label_size = input$ed_lb_size,
                     label_colour = input$ed_lb_color,
                     label_dodge = unit(2.5, "mm"),
                     arrow = arrow(length = unit(ifelse(input$ed_arrows, 2, 0), "mm")), 
                     end_cap = circle(1, "mm")) + 
      geom_node_point(size = ifelse(input$nd_size == 0, NA, input$nd_size),
                      shape = input$nd_shape,
                      colour = input$nd_color,
                      # aes(colour = "from"),
                      show.legend = FALSE) +
      geom_node_text(label = nd_lb, size = input$nd_lb_size,# colour = input$nd_lb_color, 
                     repel = TRUE) +
      theme(panel.background = element_rect(fill = input$background_color),
            plot.title = element_text(debug = FALSE, margin = margin(0, 0, 6.6, 0), size = rel(1.8), hjust = 0.5, vjust = 0.5, face = "bold"))
  })
  
  output$download <- renderUI({
    lb <- i_("download_net", lang())
    dw <- i_("download", lang())
    downloadImageUI("download_data_button", label = lb, text = dw, formats = c("jpeg", "png", "svg", "pdf"), display = "dropdown")
  })
  
  # renderizando reactable
  output$result <- renderPlot({
    req(ntwrk())
  })
  
  # output$modal <- renderUI({
  #   dw <- i_("download_net", lang())
  #   downloadImageUI("download_data_button", dw, formats = c("jpeg", "png", "svg", "pdf"))
  # })
  
  # descargas
  callModule(downloadImage, "download_data_button", graph = reactive(ntwrk()), lib = "ggplot", formats = c("jpeg", "png", "svg", "pdf"))
  
}



shinyApp(ui, server)