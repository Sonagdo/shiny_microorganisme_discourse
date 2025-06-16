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

# --- FONCTION DE NORMALISATION ---
normaliser_noms <- function(nom) {
  if (is.na(nom)) return(nom)
  repl <- c('é'='e','è'='e','ê'='e','ë'='e','à'='a','â'='a','ä'='a',
            'ă'='a','ã'='a','å'='a','á'='a','î'='i','ï'='i','ì'='i',
            'í'='i','ô'='o','ö'='o','ò'='o','õ'='o','ø'='o','ó'='o',
            'ù'='u','û'='u','ü'='u','ú'='u','ç'='c','ñ'='n','ý'='y','ÿ'='y')
  for (ch in names(repl)) nom <- str_replace_all(nom, fixed(ch), repl[ch])
  nom
}

# --- UI ---
ui <- fluidPage(
  titlePanel("Analyse du discours sur les microorganismes"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("metrique", "Choisir la mesure de discours :",
                  choices = c("Occurrences de mots" = "discour_M",
                              "Nombre de pages" = "pages_M")),
      selectInput("annee", "Choisir l'année :", choices = NULL), # sera mis à jour dynamiquement
      downloadButton("download_plot_discours_violin", "Télécharger le graphique (Discourse)", class = "btn-primary"),
      downloadButton("download_plot_discours_density", "Télécharger la distribution non-paramétrique (Discourse)"),
      downloadButton("download_plot_heatmap", "Télécharger la heatmap (Z-score)"),
      downloadButton("download_plot_carte", "Télécharger la carte (Occurrences)")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Graphique", plotOutput("plot_discours")),
        tabPanel("Distribution non paramétrique", plotOutput("plot_density")),
        tabPanel("Heatmap z-score", plotOutput("plot_heatmap")),
        tabPanel("Carte proportionnelle", plotOutput("plot_carte")),
        tabPanel("Villes filtrées", tableOutput("filtered_villes_table"))  # Affichage de la table
      )
    )
  )
)




# --- SERVER ---
server <- function(input, output, session) {
  
  data_full <- reactive({
    
    # --- IMPORT DONNÉES ---
    corpus <- read_csv("F:/Bureau PC Bureau/Stage M2 IWS_TU5/Analyse corpus_liolia/pollution/corpus_microorganismes.csv") %>%
      mutate(urban_aggl = sapply(urban_aggl, normaliser_noms),
             text_en = str_to_lower(text_en)) %>%
      filter(!is.na(text_en))
    
    # --- ATTRIBUTION MOTS-CLÉS ---
    attribution <- list(
      discour_M = c("micro-organism", "escherichia coli", "E.coli", "E.Coli","E. Coli","E. coli","Escherichia coli",
                    "Escherichia coli", "coliform")
    )
    
    # --- TEXTE PAR VILLE ---
    corpus_textes <- corpus %>%
      group_by(urban_aggl) %>%
      summarise(all_text = paste(text_en, collapse = " "), .groups = "drop")
    
    compter_occ <- function(txt, att) {
      sapply(att, \(mots) sum(str_count(txt, paste0("\\b(", paste(mots, collapse="|"), ")\\b"))))
    }
    
    result <- corpus_textes %>%
      mutate(counts = lapply(all_text, compter_occ, attribution)) %>%
      unnest_wider(counts)
    
    # --- PAGES PAR VILLE ---
    pattern_M <- paste0("\\b(", paste(attribution$discour_M, collapse = "|"), ")\\b")
    
    corpus_pages <- corpus %>%
      mutate(contains_M = str_detect(text_en, regex(pattern_M, ignore_case = TRUE))) %>%
      group_by(urban_aggl) %>%
      summarise(pages_M = sum(contains_M, na.rm = TRUE), .groups = "drop")
    
    result <- left_join(result, corpus_pages, by = "urban_aggl")
    
    # --- DONNÉES ÉTATS ---
    donnees_etats <- read_excel(
      "F:/Bureau PC Bureau/Stage M2 IWS_TU5/Nouvel analyse Gemstats/All samples/Aggregation_q90_ans_reach_all/fichier_final_avec_coordonnees_V2_050525.xlsx"
    ) %>%
      mutate(urbanaggl = sapply(urbanaggl, normaliser_noms))
    
    villes_corpus <- unique(corpus_textes$urban_aggl)
    villes_etats <- unique(donnees_etats$urbanaggl)
    
    manuelles <- c("bruxelles"="bruxelles-brussel","dallas-fort worth"="dallas",
                   "denver-aurora"="denver","koln (cologne)"="koln",
                   "neuquen-plottier-cipolletti"="neuquen")
    
    corr_auto <- tibble(ville_etats = setdiff(villes_etats, villes_corpus)) %>%
      mutate(ville_corpus = vapply(ville_etats, function(v) {
        idx <- amatch(v, villes_corpus, maxDist = 5, method = "lv", nomatch = NA_integer_)
        if (is.na(idx)) NA_character_ else villes_corpus[idx]
      }, character(1)))
    
    corr <- corr_auto %>%
      mutate(ville_corpus = ifelse(tolower(ville_etats)%in%names(manuelles),
                                   manuelles[tolower(ville_etats)], ville_corpus))
    
    donnees_etats <- donnees_etats %>%
      left_join(corr, by=c("urbanaggl"="ville_etats")) %>%
      mutate(urbanaggl = coalesce(ville_corpus, urbanaggl)) %>%
      select(-ville_corpus) %>%
      rename(urban_aggl = urbanaggl)
    
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
        etat = factor(etat, levels = c("Excellent", "Good", "Medium", "Poor", "Bad"))
      )
  })
  output$plot_discours <- renderPlot({
    ggplot(filtered_data(), aes_string(x = "etat", y = input$metrique, fill = "etat")) +
      geom_violin(trim = FALSE, alpha = .6) +
      geom_boxplot(width = .1, outlier.shape = NA, alpha = .9, color = "black") +
      scale_fill_manual(values = c("Excellent"="#1F77B4", "Good"="#2CA02C", "Medium"="#FFD700",
                                   "Poor"="#FF7F0E", "Bad"="#D62728")) +
      theme_minimal(base_size = 14) +
      labs(x = "Biological status", y = input$metrique, 
           title = paste("Discourse analysis:", input$metrique, "-", input$annee)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$download_plot_discours_violin <- downloadHandler(
  filename = function() {
    paste("graphique_discours_violin_", input$annee, ".png", sep = "")
  },
  content = function(file) {
    plot <- ggplot(filtered_data(), aes_string(x = "etat", y = input$metrique, fill = "etat")) +
      geom_violin(trim = FALSE, alpha = .6) +
      geom_boxplot(width = .1, outlier.shape = NA, alpha = .9, color = "black") +
      scale_fill_manual(values = c("Excellent"="#1F77B4", "Good"="#2CA02C", "Medium"="#FFD700",
                                   "Poor"="#FF7F0E", "Bad"="#D62728")) +
      theme_minimal(base_size = 14) +
      labs(x = "Biological status", y = input$metrique, 
           title = paste("Discourse analysis:", input$metrique, "-", input$annee)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(file, plot = plot, device = "png", dpi = 300, width = 10, height = 8)
  }
)

  output$download_plot_discours_density <- downloadHandler(
    filename = function() {
      paste("distribution_non_parametrique_", input$annee, ".png", sep = "")
    },
    content = function(file) {
      plot <- ggplot(filtered_data(), aes_string(x = input$metrique, fill = "etat", color = "etat")) +
        geom_density(alpha = 0.3, size = 1.2) +
        scale_fill_manual(values = c("Excellent" = "#1F77B4", "Good" = "#2CA02C", "Medium" = "#FFD700",
                                     "Poor" = "#FF7F0E", "Bad" = "#D62728")) +
        scale_color_manual(values = c("Excellent" = "#1F77B4", "Good" = "#2CA02C", "Medium" = "#FFD700",
                                      "Poor" = "#FF7F0E", "Bad" = "#D62728")) +
        theme_minimal(base_size = 14) +
        labs(title = paste("Distribution non-paramétrique -", input$metrique, input$annee),
             x = input$metrique, y = "Densité") +
        theme(legend.position = "right")
      
      ggsave(file, plot = plot, device = "png", dpi = 300, width = 10, height = 8)
    }
  )
  

    output$plot_density <- renderPlot({
      ggplot(filtered_data(), aes_string(x = input$metrique, fill = "etat", color = "etat")) +
        geom_density(alpha = 0.3, size = 1.2) +
        scale_fill_manual(
          values = c("Excellent" = "#1F77B4", "Good" = "#2CA02C", "Medium" = "#FFD700",
                     "Poor" = "#FF7F0E", "Bad" = "#D62728"),
          name = "State caption"
        ) +
        scale_color_manual(
          values = c("Excellent" = "#1F77B4", "Good" = "#2CA02C", "Medium" = "#FFD700",
                     "Poor" = "#FF7F0E", "Bad" = "#D62728"),
          name = "State caption"
        ) +
        labs(
          title = paste("Non-parametric distribution by state -", input$metrique, input$annee),
          x = input$metrique,
          y = "Density"
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "right")
    })
    

    output$download_plot_discours_density <- downloadHandler(
      filename = function() {
        paste("distribution_non_parametrique_", input$annee, ".png", sep = "")
      },
      content = function(file) {
        plot <- ggplot(filtered_data(), aes_string(x = input$metrique, fill = "etat", color = "etat")) +
          geom_density(alpha = 0.3, size = 1.2) +
          scale_fill_manual(
            name = "Water Quality State",  # <- légende de la couleur de remplissage
            values = c("Excellent" = "#1F77B4", "Good" = "#2CA02C", "Medium" = "#FFD700",
                       "Poor" = "#FF7F0E", "Bad" = "#D62728")
          ) +
          scale_color_manual(
            name = "Water Quality State",  # <- légende du contour de couleur
            values = c("Excellent" = "#1F77B4", "Good" = "#2CA02C", "Medium" = "#FFD700",
                       "Poor" = "#FF7F0E", "Bad" = "#D62728")
          ) +
          theme_minimal(base_size = 14) +
          labs(
            title = paste("Distribution non-paramétrique -", input$metrique, input$annee),
            x = input$metrique, y = "Density"
          ) +
          theme(legend.position = "right")
        
        ggsave(file, plot = plot, device = "png", dpi = 300, width = 10, height = 8)
      }
    )
    
    
  
  output$plot_heatmap <- renderPlot({
    df <- filtered_data() %>%
      filter(!is.na(longitude), !is.na(latitude)) %>%
      mutate(
        z_score = scale(.data[[input$metrique]])[,1],
        z_class = cut(z_score, breaks = c(-Inf, -1, 0, 1, Inf),
                      labels = c("< -1", "-1 à 0", "0 à 1", "> 1"))
      )
    
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    pts <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
    
    ggplot() +
      geom_sf(data = world, fill = "grey95", color = "grey80") +
      geom_sf(data = pts, aes(color = z_class), size = 5, alpha = .8) +
      scale_color_manual(
        values = c("< -1" = "#4575b4", "-1 à 0" = "#91bfdb", "0 à 1" = "#fee090", "> 1" = "#d73027"),
        name = "Z‑score\n(Discourse measure)"
      ) +
      theme_minimal(base_size = 13) +
      labs(title = paste("Z-score for discourse:", input$metrique),
           x = NULL, y = NULL)
  })
  output$download_plot_heatmap <- downloadHandler(
    filename = function() {
      paste("heatmap_zscore_", input$annee, ".png", sep = "")  # Choisir le nom du fichier
    },
    content = function(file) {
      df <- filtered_data() %>%
        filter(!is.na(longitude), !is.na(latitude)) %>%
        mutate(
          z_score = scale(.data[[input$metrique]])[,1],
          z_class = cut(z_score, breaks = c(-Inf, -1, 0, 1, Inf),
                        labels = c("< -1", "-1 à 0", "0 à 1", "> 1"))
        )
      
      world <- ne_countries(scale = "medium", returnclass = "sf")
      pts <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
      
      plot <- ggplot() +
        geom_sf(data = world, fill = "grey95", color = "grey80") +
        geom_sf(data = pts, aes(color = z_class), size = 5, alpha = .8) +
        scale_color_manual(
          values = c("< -1" = "#4575b4", "-1 à 0" = "#91bfdb", "0 à 1" = "#fee090", "> 1" = "#d73027"),
          name = "Z‑score\n(Discourse measure)"
        ) +
        theme_minimal(base_size = 13) +
        labs(title = paste("Z-score for discourse:", input$metrique),
             x = NULL, y = NULL)
      
      # Sauvegarder en PNG
      ggsave(file, plot = plot, device = "png", dpi = 300, width = 10, height = 8)
    }
  )
  
  output$plot_carte <- renderPlot({
    ordre_etats <- c("Excellent", "Good", "Medium", "Poor", "Bad")
    couleurs_etat <- c("Excellent"="#1F77B4", "Good"="#2CA02C", "Medium"="#FFD700",
                       "Poor"="#FF7F0E", "Bad"="#D62728")
    
    metrique <- input$metrique
    
    map_data <- filtered_data() %>%
      filter(!is.na(latitude), !is.na(longitude)) %>%
      mutate(
        etat = str_to_title(etat_m),
        etat = factor(etat, levels = ordre_etats),
        metrique_val = .data[[metrique]],
        metrique_class = cut(
          metrique_val,
          breaks = c(-Inf, 5, 10, 20, Inf),
          labels = c("0‑5", "6‑10", "11‑20", ">20")
        )
      )
    
    world <- ne_countries(scale = "medium", returnclass = "sf")
    pts <- st_as_sf(map_data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
    
    ggplot() +
      geom_sf(data = world, fill = "grey95", colour = "grey80") +
      geom_sf(
        data = pts,
        aes(fill = etat, size = metrique_class),
        shape = 21, colour = "black", alpha = .8
      ) +
      scale_fill_manual(values = couleurs_etat[levels(pts$etat)], name = "Biological status") +
      scale_size_manual(
        values = c("0‑5" = 3, "6‑10" = 5, "11‑20" = 7, ">20" = 9),
        name = paste("Occurrences\n(", metrique, ")", sep = "")
      ) +
      coord_sf(expand = FALSE) +
      theme_minimal(base_size = 13) +
      labs(
        title = paste("Occurrence of terms and biological states (", input$annee, ")", sep = ""),
        subtitle = "Diameter: occurrence class - Color: biological status",
        x = NULL, y = NULL
      ) +
      theme(
        legend.box = "vertical",
        legend.spacing.y = unit(0.4, "cm")
      )
  })
  output$download_plot_carte <- downloadHandler(
    filename = function() {
      paste("carte_occurence_", input$annee, ".png", sep = "")  # Choisir le nom du fichier
    },
    content = function(file) {
      ordre_etats <- c("Excellent", "Good", "Medium", "Bad", "Poor")
      couleurs_etat <- c("Excellent"="#1F77B4", "Good"="#2CA02C", "Medium"="#FFD700",
                         "Poor"="#FF7F0E", "Bad"="#D62728")
      
      metrique <- input$metrique
      
      map_data <- filtered_data() %>%
        filter(!is.na(latitude), !is.na(longitude)) %>%
        mutate(
          etat = str_to_title(etat_m),
          etat = factor(etat, levels = ordre_etats),
          metrique_val = .data[[metrique]],
          metrique_class = cut(
            metrique_val,
            breaks = c(-Inf, 5, 10, 20, Inf),
            labels = c("0‑5", "6‑10", "11‑20", ">20")
          )
        )
      
      world <- ne_countries(scale = "medium", returnclass = "sf")
      pts <- st_as_sf(map_data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
      
      plot <- ggplot() +
        geom_sf(data = world, fill = "grey95", colour = "grey80") +
        geom_sf(
          data = pts,
          aes(fill = etat, size = metrique_class),
          shape = 21, colour = "black", alpha = .8
        ) +
        scale_fill_manual(values = couleurs_etat[levels(pts$etat)], name = "Biological status") +
        scale_size_manual(
          values = c("0‑5" = 3, "6‑10" = 5, "11‑20" = 7, ">20" = 9),
          name = paste("Occurrences\n(", metrique, ")", sep = "")
        ) +
        coord_sf(expand = FALSE) +
        theme_minimal(base_size = 13) +
        labs(
          title = paste("Occurrence of terms and biological states (", input$annee, ")", sep = ""),
          subtitle = "Diameter: occurrence class - Color: biological status",
          x = NULL, y = NULL
        ) +
        theme(
          legend.box = "vertical",
          legend.spacing.y = unit(0.4, "cm")
        )
      
      # Sauvegarder en PNG
      ggsave(file, plot = plot, device = "png", dpi = 300, width = 10, height = 8)
    }
  )
  filtered_villes <- reactive({
    data_full() %>%
      filter(etat_m == "poor", .data[[input$metrique]] < 10) %>%
      select(urban_aggl, etat_m, .data[[input$metrique]])
  })
  
  output$filtered_villes_table <- renderTable({
    filtered_villes()
  })
}
# --- Lancer l'app ---
shinyApp(ui, server)
