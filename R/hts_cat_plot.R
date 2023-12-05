

hts_cat_plot = function(summary_ls,
                        graph_type,
                        weighted = TRUE,
                        plotly = TRUE,
                        stacked_palette = c(
                          "#F68B1F",
                          "#e6645e",
                          "#ba1222",
                          "#9b2b70",
                          "#3e3a62",
                          "#84b5d8",
                          "#006fa1",
                          "#17384e",
                          "#63af5e",
                          "#a9bf35",
                          "#ffc20e"
                        ),
                        single_bar_color = "#006fa1",
                        errorbar_color = "#17384e",
                        text_color = "#17384e",
                        missing_color = "#DCDDDE",
                        missing_value = "Missing Response") {
  
  if (!graph_type %in% c('stacked', 'facet', 'barchart')) {
    

    stop('graph_type must be stacked, facet, or barchart')
    
  }
  
  # Select Data ----------------------------------------------------------------
  if (weighted) { 
    dat = copy(summary_ls$cat_summary$wtd) 

  } else {
    dat = copy(summary_ls$cat_summary$unwtd)
  }
  
  # Get variables --------------------------------------------------------------
  summarize_var = summary_ls$summarize_var
  summarize_by = summary_ls$summarize_by
  units = summary_ls$n_ls$units
  
  # Create tooltip labels ------------------------------------------------------
  dat[, text :=
        paste0(
          '<b>', str_to_title(summarize_var), 
          ': ', '</b>',
          dat[[summarize_var]],

          ifelse(
            !is.null(summarize_by),
            paste0('<br><b>', str_to_title(summarize_by), '</b>',
                   ': ', 
                   dat[[summarize_by]]),
            ''),
          
          # Count
          ifelse(
            'count' %in% names(dat),
            paste0('<br><b>N = ', format(count, big.mark = ','), '</b>', ' surveyed ', units),
            ''
          ),
          
          # Estimate
          ifelse(
            'est' %in% names(dat),
            paste0(
              '<br><b>Estimated ', format(est, big.mark = ','),
              ifelse('est_se' %in% names(dat),
                     paste0(' +/- ', format(est_se, big.mark = ',')),
                     ''),
              '</b> ',
              ifelse(
                weighted,
                paste0(units, '*'),
                paste0('survyed ', units)
              )
            ),
            ''
          ),
          
          # Proportion
          ifelse(
            'prop' %in% names(dat),
            paste0(
              '<br><b>', as.character(round(prop * 100, 1)),
              
              ifelse('prop_se' %in% names(dat),
                     paste0('% +/- ', as.character(
                       round(prop_se * 100, 1)
                     )),
                     ''),
              
              '%</b> of ',
              
              ifelse(
                weighted,
                paste0(units, '*'),
                paste0('surveyed ', units)
              ),
              
              ''
            ),
            ''
          ),
          ifelse(
            weighted,
            paste0('<br><i>* Weighted to total population</em>'),
            ''
          )
        )]
  
  # Summarize variables should be factor ---------------------------------------
  if(!is.factor(dat[[summarize_var]])){
    dat[[summarize_var]] = factor(dat[[summarize_var]], levels = unique(dat[[summarize_var]]))
  }
  

  if(!is.null(summarize_by)){
    if(summarize_by != 'count' & !is.factor(dat[[summarize_by]])){
    dat[[summarize_by]] = factor(dat[[summarize_by]], levels = unique(dat[[summarize_by]]))
    }
  }
    
  # Stacked bar plot -----------------------------------------------------------
  if (graph_type == 'stacked'){
    
    plot = ggplot2::ggplot(
      dat,
      ggplot2::aes(
        y = prop,
        x = dat[[summarize_by]],
        fill = dat[[summarize_var]],
        text = text)
    ) +
      ggplot2::geom_bar(
        stat = 'identity'
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = str_to_title(summarize_var),
        y = 'Percent',
        fill = summarize_by,
        title = str_to_title(paste0(summarize_by,
                                    ' by ',
                                    summarize_var)),
        fill = summarize_by
      ) +
      ggplot2::scale_y_continuous(limits = c(0, 1.1),
                                  labels = scales::percent_format(accuracy = 1)) +
      ggplot2::scale_x_discrete(limits = rev)

    
    ## Apply stacked color palette ---------------------------------------------
    categories = levels(dat[[summarize_var]])
    missing_category = categories[categories %in% missing_value]
    non_missing_categories = categories[!categories %in% missing_value]
    n_colors_needed = length(non_missing_categories)
    
    if (n_colors_needed <= length(stacked_palette)) {
      
      pal = stacked_palette[1:n_colors_needed]
      names(pal) = non_missing_categories
      
    } else if (n_colors_needed > length(stacked_palette)) {
      
      pal = colorRampPalette(stacked_palette)
      pal = pal(n_colors_needed)

    }
    
    if(!is.null(missing_value)){
      
      pal = c(pal, missing_color)
      names(pal) = c(non_missing_categories, missing_category)
      
    } else if (is.null(missing_value)){
      
      names(pal) = non_missing_categories

    }
    
    # Add colors and legend: 
    plot = plot + 
      ggplot2::scale_fill_manual(values = pal)
    
  } else if (graph_type == 'facet') {
    
    # Faceted bar plot ---------------------------------------------------------
    ymax = max(dat$prop) + 0.35
    
    plot = ggplot2::ggplot(
      dat,
      ggplot2::aes(
        x = dat[[summarize_var]],
        y = prop,
        text = text)
    ) +
      ggplot2::coord_flip()+
      ggplot2::geom_bar(
        stat = "identity",
        fill = single_bar_color,
        position = 'dodge'
      ) +
      ggplot2::facet_wrap(~dat[[summarize_by]]) +
      ggplot2::labs(
        x = summarize_by,
        y = 'Percent',
        fill = summarize_var,
        title = str_to_title(paste0(summarize_by,
                                    ' by ',
                                    summarize_var)) +
          ggplot2::scale_x_discrete(labels = ~ str_wrap(.x, width = 60), drop = FALSE)
      ) +
      ggplot2::scale_y_continuous(limits = c(0, ymax),
                                  labels = scales::percent_format(accuracy = 1))
    

    if ('prop_se' %in% colnames(dat)){
      plot = plot + 
        ggplot2::geom_errorbar(
        aes(ymin = prop - prop_se,
            ymax = prop + prop_se),
        position = 'dodge',
        width = 0,
        color = errorbar_color
      )
    }

  } else if (graph_type == 'barchart') {
    ymax = max(dat$count) + 0.1 * max(dat$count)
    
    plot = ggplot2::ggplot(data = dat, aes(x = dat[[summarize_var]],
                                           y = count,
                                           text = text)) +
      ggplot2::coord_flip() +
      ggplot2::geom_bar(stat = "identity",
                        fill = single_bar_color) +
      ggplot2::labs(
        x = summarize_var,
        fill =  summarize_var,
        y = 'Count',
        title = str_to_title(paste0('Distribution', ' of ', summarize_var))
      ) +
      ggplot2::scale_x_discrete(
        labels = function(x)
          str_wrap(x, width = 60),
        limits = rev
      ) +

      ggplot2::scale_y_continuous(limits = c(0, ymax))
    
    
    if ('count_se' %in% colnames(dat)) {
      plot =
        plot +
        ggplot2::geom_errorbar(
          aes(ymin = count - count_se,
              ymax = count + count_se),
          position = 'dodge',
          width = 0,
          color = errorbar_color
        )
    }
  }
  
  # Add subtitle ---------------------------------------------------------------
  subtitle_lab = ifelse(weighted, 'Weighted', 'Unweighted')
  
  plot = plot +
    ggplot2::labs(subtitle = subtitle_lab)
  
  # Apply ggplot theme ---------------------------------------------------------
  plot = plot +
    hts_plot_theme

  
  # Generate plotly ------------------------------------------------------------
  if (plotly) {
    plot = ggplotly(plot, tooltip = 'text') %>% 
      config(displayModeBar = FALSE) %>%
      layout(
        hoverlabel = list(
          font = list(
            family = c('Oswald', 'Arial', 'sans-serif'),
            size = 16
          )
        )
      ) %>%
      {
        .$x$data <- lapply(.$x$data, function(data_element) {
          if (!is.null(data_element$name)) {
            data_element$name <-
              str_remove_all(data_element$name, "^\\(|\\,1\\)$")
          }
          return(data_element)
        })
        .
      }
    
  }
  
  return(plot)
}
