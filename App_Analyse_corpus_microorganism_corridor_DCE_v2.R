library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(purrr)
library(forcats)
library(stringdist)
library(sf)
library(rnaturalearth)
library(leaflet)
library(mapview)    # Pour capture carte
library(webshot2)   # Pour capture carte

# Fonction de normalisation des noms
normaliser_noms <- function(nom) {
  if (is.na(nom)) return(nom)
  repl <- c('é'='e','è'='e','ê'='e','ë'='e','à'='a','â'='a','ä'='a',
            'ă'='a','ã'='a','å'='a','á'='a','î'='i','ï'='i','ì'='i',
            'í'='i','ô'='o','ö'='o','ò'='o','õ'='o','ø'='o','ó'='o',
            'ù'='u','û'='u','ü'='u','ú'='u','ç'='c','ñ'='n','ý'='y','ÿ'='y')
  for (ch in names(repl)) nom <- str_replace_all(nom, fixed(ch), repl[ch])
  nom
}

ui <- fluidPage(
  titlePanel("Analyse du discours sur les microorganismes"),
  sidebarLayout(
    sidebarPanel(
      selectInput("metrique", "Choisir la mesure de discours :",
                  choices = c("Occurrences de mots" = "discour_M",
                              "Nombre de pages" = "pages_M")),
      selectInput("annee", "Choisir l'année :", choices = NULL),
      downloadButton("download_plot_discours_violin", "Télécharger le graphique (Discourse)", class = "btn-primary"),
      downloadButton("download_plot_discours_density", "Télécharger la distribution non-paramétrique (Discourse)"),
      downloadButton("download_plot_heatmap", "Télécharger la heatmap Z-score"),
      br(), br(),
      downloadButton("download_map_png", "Télécharger la carte interactive (PNG)")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Graphique", plotOutput("plot_discours")),
        tabPanel("Distribution non paramétrique", plotOutput("plot_density")),
        tabPanel("Heatmap Z-score", plotOutput("plot_heatmap")),
        tabPanel("Villes filtrées", tableOutput("filtered_villes_table")),
        tabPanel("Carte proportionnelle", plotOutput("plot_carte_interactive", height = "600px"))
      )
    )
  )
)
server <- function(input, output, session) {
  
  # Palette couleurs adaptée à 2 états
  couleurs_etat <- c("Good" = "#2CA02C", "Poor" = "#D62728")
  couleurs_zscore <- c("< -1" = "#4575b4", "-1 à 0" = "#91bfdb", "0 à 1" = "#fee090", "> 1" = "#d73027")
  
  data_full <- reactive({
    
    corpus <- read_csv("F:/Bureau PC Bureau/Stage M2 IWS_TU5/Analyse corpus_liolia/pollution/corpus_microorganismes.csv") %>%
      mutate(urban_aggl = sapply(urban_aggl, normaliser_noms),
             text_en = str_to_lower(text_en)) %>%
      filter(!is.na(text_en))
    
    attribution <- list(
      discour_M = c("micro-organism", "escherichia coli", "E.coli", "E.Coli", "E. coli", "coliform")
    )
    
    corpus_textes <- corpus %>%
      group_by(urban_aggl) %>%
      summarise(all_text = paste(text_en, collapse = " "), .groups = "drop")
    
    compter_occ <- function(txt, att) {
      sapply(att, \(mots) sum(str_count(txt, paste0("\\b(", paste(mots, collapse="|"), ")\\b"))))
    }
    
    result <- corpus_textes %>%
      mutate(counts = lapply(all_text, compter_occ, attribution)) %>%
      unnest_wider(counts)
    
    pattern_M <- paste0("\\b(", paste(attribution$discour_M, collapse = "|"), ")\\b")
    
    corpus_pages <- corpus %>%
      mutate(contains_M = str_detect(text_en, regex(pattern_M, ignore_case = TRUE))) %>%
      group_by(urban_aggl) %>%
      summarise(pages_M = sum(contains_M, na.rm = TRUE), .groups = "drop")
    
    result <- left_join(result, corpus_pages, by = "urban_aggl")
    
    donnees_etats <- read_delim("F:\\Bureau PC Bureau\\Stage M2 IWS_TU5\\New_analysis\\appreciation_seq_avec_coordonnees.csv", delim = ";", show_col_types = FALSE) %>%
      mutate(urbanaggl = sapply(urbanaggl, normaliser_noms))
    
    villes_corpus <- unique(corpus_textes$urban_aggl)
    villes_etats <- unique(donnees_etats$urbanaggl)
    
    manuelles <- c("bruxelles"="bruxelles-brussel", "dallas-fort worth"="dallas")
    
    corr_auto <- tibble(ville_etats = setdiff(villes_etats, villes_corpus)) %>%
      mutate(ville_corpus = vapply(ville_etats, function(v) {
        idx <- amatch(v, villes_corpus, maxDist = 5, method = "lv", nomatch = NA_integer_)
        if (is.na(idx)) NA_character_ else villes_corpus[idx]
      }, character(1)))
    
    corr <- corr_auto %>%
      mutate(ville_corpus = ifelse(tolower(ville_etats) %in% names(manuelles),
                                   manuelles[tolower(ville_etats)], ville_corpus))
    
    donnees_etats <- donnees_etats %>%
      left_join(corr, by = c("urbanaggl" = "ville_etats")) %>%
      mutate(urbanaggl = coalesce(ville_corpus, urbanaggl)) %>%
      rename(urban_aggl = urbanaggl) %>%
      select(-ville_corpus)
    
    left_join(result, donnees_etats, by = "urban_aggl")
  })
  
  observe({
    updateSelectInput(session, "annee", choices = sort(unique(data_full()$year)))
  })
  
  filtered_data <- reactive({
    data_full() %>%
      filter(reach == "city", year == input$annee) %>%
      mutate(
        etat = str_to_title(etat_m),
        etat = factor(etat, levels = c("Good", "Poor"))
      )
  })
  
  # 1. Violin plot
  output$plot_discours <- renderPlot({
    ggplot(filtered_data(), aes(x = etat, y = .data[[input$metrique]], fill = etat)) +
      geom_violin(trim = FALSE, alpha = .6) +
      geom_boxplot(width = .1, outlier.shape = NA, alpha = .9, color = "black") +
      scale_fill_manual(values = couleurs_etat) +
      theme_minimal(base_size = 14) +
      labs(x = "Biological status", y = input$metrique,
           title = paste("Discourse analysis:", input$metrique, "-", input$annee))
  })
  
  # 2. Density plot
  output$plot_density <- renderPlot({
    ggplot(filtered_data(), aes(x = .data[[input$metrique]], fill = etat, color = etat)) +
      geom_density(alpha = 0.3, size = 1.2) +
      scale_fill_manual(values = couleurs_etat, name = "State") +
      scale_color_manual(values = couleurs_etat, name = "State") +
      theme_minimal(base_size = 14) +
      labs(title = paste("Density by state -", input$metrique, input$annee),
           x = input$metrique, y = "Density")
  })
  
  # 3. Heatmap z-score
  output$plot_heatmap <- renderPlot({
    df <- filtered_data() %>%
      filter(!is.na(longitude), !is.na(latitude)) %>%
      mutate(
        z_score = scale(.data[[input$metrique]])[,1],
        z_class = cut(z_score, breaks = c(-Inf, -1, 0, 1, Inf),
                      labels = names(couleurs_zscore))
      )
    
    pts <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
    
    ggplot() +
      geom_sf(data = ne_countries(scale = "medium", returnclass = "sf"),
              fill = "grey95", color = "grey80") +
      geom_sf(data = pts, aes(color = z_class), size = 5, alpha = .8) +
      scale_color_manual(values = couleurs_zscore, name = "Z‑score") +
      theme_minimal(base_size = 13) +
      labs(title = paste("Z-score for discourse:", input$metrique))
  })
  
  # 4. Carte proportionnelle
  output$plot_carte_interactive <- renderPlot({
    df <- filtered_data() %>%
      filter(!is.na(latitude), !is.na(longitude)) %>%
      mutate(
        etat = case_when(
          is.na(etat_m) ~ "Unknown",
          str_to_title(etat_m) %in% c("Good", "Poor") ~ str_to_title(etat_m),
          TRUE ~ "Unknown"
        ),
        etat = factor(etat, levels = c("Good", "Poor", "Unknown")),
        metrique_val = .data[[input$metrique]],
        metrique_class = cut(
          metrique_val,
          breaks = c(-Inf, 5, 10, 20, Inf),
          labels = c("0-5", "6-10", "11-20", ">20"),
          include.lowest = TRUE
        )
      )
    
    couleurs_etat <- c("Good" = "#2CA02C", "Poor" = "#D62728", "Unknown" = "grey60")
    tailles_cercles <- c("0-5" = 3, "6-10" = 5, "11-20" = 7, ">20" = 9)
    
    pts <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    ggplot() +
      geom_sf(data = world, fill = "grey95", color = "grey80") +
      geom_sf(
        data = pts,
        aes(fill = etat, size = metrique_class),
        shape = 21, color = "black", alpha = 0.8
      ) +
      scale_fill_manual(values = couleurs_etat, name = "Biological status") +
      scale_size_manual(values = tailles_cercles, name = paste("Occurrences\n(", input$metrique, ")", sep = "")) +
      coord_sf(expand = FALSE) +
      theme_minimal(base_size = 13) +
      labs(
        title = paste("Occurrences and biological status -", input$annee),
        subtitle = "Diameter = discourse frequency / Color = biological status",
        x = NULL, y = NULL
      )
  })
  
  
  # 5. Table des villes filtrées
  filtered_villes <- reactive({
    data_full() %>%
      filter(str_to_title(etat_m) == "Poor", .data[[input$metrique]] < 10) %>%
      select(urban_aggl, etat_m, .data[[input$metrique]])
  })
  
  output$filtered_villes_table <- renderTable({
    filtered_villes()
  })
  
  # Téléchargements
  output$download_plot_discours_violin <- downloadHandler(
    filename = function() paste0("violin_", input$annee, ".png"),
    content = function(file) {
      ggsave(file, plot = output$plot_discours(), device = "png", dpi = 300, width = 10, height = 8)
    }
  )
  
  output$download_plot_discours_density <- downloadHandler(
    filename = function() paste0("density_", input$annee, ".png"),
    content = function(file) {
      ggsave(file, plot = output$plot_density(), device = "png", dpi = 300, width = 10, height = 8)
    }
  )
  
  output$download_plot_heatmap <- downloadHandler(
    filename = function() paste0("heatmap_", input$annee, ".png"),
    content = function(file) {
      ggsave(file, plot = output$plot_heatmap(), device = "png", dpi = 300, width = 10, height = 8)
    }
  )
  
  output$download_plot_carte <- downloadHandler(
    filename = function() paste0("map_", input$annee, ".png"),
    content = function(file) {
      ggsave(file, plot = output$plot_carte(), device = "png", dpi = 300, width = 10, height = 8)
    }
  )
  output$download_map_png <- downloadHandler(
    filename = function() {
      paste0("map_interactive_", input$annee, ".png")
    },
    content = function(file) {
      df <- filtered_data() %>%
        filter(!is.na(latitude), !is.na(longitude)) %>%
        mutate(
          etat = case_when(
            is.na(etat_m) ~ "Unknown",
            str_to_title(etat_m) %in% c("Good", "Poor") ~ str_to_title(etat_m),
            TRUE ~ "Unknown"
          ),
          etat = factor(etat, levels = c("Good", "Poor", "Unknown")),
          metrique_val = .data[[input$metrique]],
          metrique_class = cut(
            metrique_val,
            breaks = c(-Inf, 5, 10, 20, Inf),
            labels = c("0-5", "6-10", "11-20", ">20"),
            include.lowest = TRUE
          )
        )
      
      couleurs_etat <- c("Good" = "#2CA02C", "Poor" = "#D62728", "Unknown" = "grey60")
      tailles_metrique <- c("0-5" = 6, "6-10" = 10, "11-20" = 14, ">20" = 18)
      
      m <- mapview(
        df,
        xcol = "longitude", ycol = "latitude",
        zcol = "etat",
        cex = tailles_metrique[as.character(df$metrique_class)],
        col.regions = couleurs_etat,
        legend = TRUE,
        popup = paste0(
          "<strong>", df$urban_aggl, "</strong><br/>",
          "Etat: ", df$etat, "<br/>",
          input$metrique, ": ", df$metrique_val
        )
      )
      
      mapshot(m, file = file)
    }
  )
  
  
}

shinyApp(ui, server)