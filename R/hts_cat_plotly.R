


hts_cat_plotly = function(summary_ls,
                          stacked = FALSE,
                          weighted = TRUE,
                          proportion = TRUE,
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
  
  # Select Data ----------------------------------------------------------------
  
  # if weighted and there is no weighted summary, set weighted to FALSE
  if(weighted & is.null(summary_ls$cat_summary$wtd)){
    weighted = FALSE
  }
  
  if (weighted) {
    dat = copy(summary_ls$cat_summary$wtd)
  } else {
    dat = copy(summary_ls$cat_summary$unwtd)
  }
  
  # Choose stat column ---------------------------------------------------------
  if (proportion) {
    stat = 'prop'
  } else if (!proportion & weighted) {
    stat = 'est'
  } else if (!proportion & !weighted) {
    stat = 'count'
  }
  
  # Get variables --------------------------------------------------------------
  summarize_var = summary_ls$summarize_var
  summarize_by = summary_ls$summarize_by
  
  units = summary_ls$n_ls$units
  
  setnames(dat,
           old = summarize_var,
           new = "summarize_var")
  
  setnames(dat,
           old = stat,
           new = "stat")
  
  if(stat == 'count'){
    dat[, count := stat]
  }
  
  # Are standard errors present?
  se = paste0(stat, "_se") %in% names (dat)
  
  if (se) {
    setnames(dat,
             old = paste0(stat, "_se"),
             new = "stat_se")
  }
  
  # Add total by-variable
  if (!is.null(summarize_by)) {
    setnames(dat,
             old = summarize_by,
             new = "summarize_by",
             skip_absent = TRUE)
    dat = dat[, by_total := sum(count), by = summarize_by]
  }
  
  
  # if by-variable is NULL, cannot do stacked
  if(is.null(summarize_by)){
    stacked = FALSE
  }
  
  # Create tooltip labels ------------------------------------------------------
  summarize_var_desc = stringr::str_to_title(summary_ls$summarize_var_desc)
  
  if(!is.null(summarize_by)){
    summarize_by_desc = stringr::str_to_title(summary_ls$summarize_by_desc)
  }
  
  
  labels = list()
  
  for (row in 1:nrow(dat)) {
    
    d = dat[row, ]
    
    main_style = ifelse(
      !is.null(summarize_by),
      ifelse(d$by_total < 50,
             "<i>",
             ""),
      ifelse(d$count < 50,
             "<i>",
             "")
    )
    
    by_line = ifelse(
      !is.null(summarize_by),
      str_glue(
        "<b><span style = 'font-size:1.25rem'>{summarize_by_desc}: {str_wrap(d$summarize_by, 30)}</b></span>"
      ),
      ""
    )
    
    var_line = str_glue(
      "<br><b><span style = 'font-size:1.15rem'>{summarize_var_desc}: {d$summarize_var}</b></span>"
    )
    
    count_line =
      str_glue("<br>N = {count} {str_to_title(units)} Surveyed",
               count = prettyNum(d$count, big.mark = ","))
    
    insuff_line = ifelse(
      !is.null(summarize_by),
      ifelse(
        d$by_total < 50,
        "<br>Low sample size - estimates may be unreliable",
        ""
      ),
      ifelse(
        d$count < 50,
        "<br>Low sample size - estimates may be unreliable",
        ""
      )
    )
    
    stat_line =
      ifelse(
        weighted,
        ifelse(
          se,
          str_glue(
            "<br><b>{stat_i} ± {stat_se_i}{mark}</b> {str_to_title(units)}*",
            stat_i = ifelse(
              stat == "prop",
              round(100 * d$stat, 1),
              prettyNum(d$stat, big.mark = ",")
            ),
            stat_se_i = ifelse(
              stat == "prop",
              round(100 * d$stat_se, 1),
              prettyNum(d$stat_se, big.mark = ",")
            ),
            mark = ifelse(stat == "prop", "% of", "")
          ),
          
          str_glue(
            "<br><b>{stat_i}{mark}</b> {str_to_title(units)}*",
            stat_i = ifelse(
              stat == "prop",
              round(100 * d$stat, 1),
              prettyNum(d$stat, big.mark = ",")
            ),
            mark = ifelse(stat == "prop", "% of", "")
          )
        ),
        str_glue(
          "<br><b>{stat_i}{mark}</b> Surveyed {str_to_title(units)}",
          stat_i = ifelse(
            stat == "prop",
            round(100 * d$stat, 1),
            prettyNum(d$stat, big.mark = ",")
          ),
          mark = ifelse(stat == "prop", "% of", "")
        )
      )
    
    
    weight_line = ifelse(weighted,
                         "<br>*Weighted to total population",
                         "")
    
    end_style = ifelse(
      !is.null(summarize_by),
      ifelse(d$by_total < 50, "</i>", ""),
      ifelse(d$count < 50, "</i>", "")
    )
    
    row_label = paste0(
      main_style,
      by_line,
      var_line,
      count_line,
      insuff_line,
      stat_line,
      weight_line,
      end_style
    )
    
    row_label = htmltools::HTML(row_label)
    
    labels[[row]] = row_label
    
  }
  
  labels = unlist(labels)
  
  dat[, label := labels]
  
  # Summarize variables should be factor ---------------------------------------
  
  dat[, order(summarize_var)]
  dat[, summarize_var := stringr::str_wrap(summarize_var, width = 30)]
  dat[, summarize_var := factor(summarize_var, levels = rev(unique(summarize_var)))]
  
  if (!is.null(summarize_by)) {
    dat$summarize_by %>% levels()
    dat[, order(summarize_var, summarize_by)]
    dat[, summarize_by := stringr::str_wrap(summarize_by, width = 30)]
    if (stacked){
      dat[, summarize_by := factor(summarize_by, levels = rev(unique(summarize_by)))]
    } else {
      dat[, summarize_by := factor(summarize_by, levels = unique(summarize_by))]
    }
  }
  
  
  # Plot globals ---------------------------------------------------------------
  x_title = stringr::str_to_title(paste0(
    fcase(
      stat == "prop",
      "Percent of ",
      stat == "est",
      "Estimated Number of ",
      stat == "count",
      "Number of "
    ),
    fcase(
      stat %in% c("prop", "est") & weighted == TRUE, paste0(str_to_title(units), ' (Weighted)'), 
      stat %in% c("prop", "est") & weighted == FALSE, paste0('Surveyed ', str_to_title(units)), 
      stat == "count", paste0('Surveyed ', str_to_title(units))
    )
  ))
  
  
  plot_tickformat = ifelse(stat == "prop",
                           ".0%",
                           "")
  
  # Stacked bar plot -----------------------------------------------------------
  if (stacked) {
    ## Apply stacked color palette ---------------------------------------------
    categories = levels(dat$summarize_var)
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
    
    if (!is.null(missing_value)) {
      pal = c(pal, missing_color)
      names(pal) = c(non_missing_categories, missing_category)
      
    } else if (is.null(missing_value)) {
      names(pal) = non_missing_categories
      
    }
    
    p = plotly::plot_ly(
      data = dat,
      x = ~ stat,
      y = ~ summarize_by,
      type = 'bar',
      orientation = 'h',
      color = ~ summarize_var,
      colors = pal,
      text = ~ label,
      textposition = "none",
      hoverinfo = 'text',
      hoverlabel = list(font = list(family = "Oswald, Arial, sans-serif",
                                    size = 0.8))
    ) %>%
      layout(
        barmode = 'stack',
        bargap = 0.1,
        font = list(family = "Oswald, Arial, sans-serif",
                    size = 16),
        xaxis = list(tickformat = plot_tickformat,
                     title = x_title),
        yaxis = list(title = "",
                     # add some space between vertical axis and labels
                     ticksuffix = "  ")
      )
    
    plot_title = str_glue(
      "Percent of {str_to_title(units)} By {summarize_var_desc}, For Every Category of {summarize_by_desc}"
    )
    
    plot_subtitle = paste0("Unweighted N = ",
                           knitr::combine_words(sapply(names(summary_ls$n_ls$unwtd),
                                                       function(x)
                                                         paste0(format(summary_ls$n_ls$unwtd[[x]], big.mark = ","),
                                                                " ", x))),
                           " Surveyed")
    
    if (weighted) {
      plot_subtitle = paste0(plot_subtitle,
                                 "<br>",
                                 "Weighted N = ",
                                 paste0(format(summary_ls$n_ls$wtd[[1]], big.mark = ","), " ", str_to_title(units)))
    }
    
    
    plot_title_subtitle = 
      htmltools::HTML(str_glue("<h4>{plot_title}<br><span style = 'font-size: 1.1rem'>{plot_subtitle}</span></h4>"))
    
    stacked_card = bslib::card(card_title(plot_title_subtitle),
                               card_body(p, full_screen = TRUE))
    
    return(stacked_card)
    
  } else if (!stacked & !is.null(summarize_by)) {
    # Faceted bar plot ---------------------------------------------------------
    
    facet_card_ls = list()
    
    # Max y value should be constant across all plots:
    stat_max = max(dat$stat)
    
    if(stat == "prop"){
      stat_max = min(max(dat$stat) + 0.1, 1)
    }
    
    for (level in levels(dat$summarize_by)) {
      # level = "85 or older"
      
      dat_by = dat[summarize_by == level]
      
      # TODO: fill in zeros
      
      if (se) {
        facet_plot_base_i = plotly::plot_ly(
          data = dat_by,
          x = ~ stat,
          y = ~ summarize_var,
          type = 'bar',
          marker = list(color = single_bar_color),
          text = ~ label,
          error_x = list(array = ~ stat_se,
                         color = '#000'),
          textposition = "none",
          hoverinfo = 'text',
          hoverlabel = list(font = list(
            family = "Oswald, Arial, sans-serif",
            size = 0.8
          ))
        )
      } else if (!se) {
        facet_plot_base_i = plotly::plot_ly(
          data = dat_by,
          x = ~ stat,
          y = ~ summarize_var,
          type = 'bar',
          marker = list(color = single_bar_color),
          text = ~ label,
          textposition = "none",
          hoverinfo = 'text',
          hoverlabel = list(font = list(
            family = "Oswald, Arial, sans-serif",
            size = 0.8
          ))
        )
      }
      
      facet_plot_i = facet_plot_base_i %>%
        layout(
          bargap = 0.1,
          font = list(family = "Oswald, Arial, sans-serif",
                      size = 16),
          xaxis = list(
            tickformat = plot_tickformat,
            title = x_title,
            range = list(0, stat_max)
          ),
          yaxis = list(title = "",
                       # add some space between vertical axis and labels
                       ticksuffix = "  ")
        )
      
      facet_title_i = str_glue("{summarize_by_desc}: {stringr::str_to_title(level)}")
      
      facet_subtitle_i = str_glue(
        "N = {facet_count_i} {str_to_title(units)} Surveyed",
        facet_count_i = prettyNum(sum(dat_by$count), big.mark = ",")
      )
      
      facet_title_subtitle = htmltools::HTML(str_glue(
        "<h4>{facet_title_i}<br><span style = 'font-size: 1.1rem'>{facet_subtitle_i}</span></h4>"
      ))
      
      facet_card_i = bslib::card(
        card_title(facet_title_subtitle),
        card_body(facet_plot_i, full_screen = TRUE)
      )
      
      facet_card_ls[[level]] = facet_card_i
      
    }
    
    facet_plot_title = str_glue(
      "Percent of {str_to_title(units)} By {summarize_var_desc}, For Every Category Of {summarize_by_desc}"
    )
    
    facet_plot_subtitle = paste0("N = ",
                                 knitr::combine_words(sapply(names(summary_ls$n_ls$unwtd),
                                                             function(x)
                                                               paste0(format(summary_ls$n_ls$unwtd[[x]], big.mark = ","),
                                                                      " ", x))),
                                 " Surveyed")
    
    if (weighted) {
      facet_plot_subtitle = paste0(facet_plot_subtitle,
                                   "<br>",
                                   "Weighted N = ",
                                   paste0(format(summary_ls$n_ls$wtd[[1]], big.mark = ","), " ", str_to_title(units)))
    }
    
    facet_plot_title_subtitle = htmltools::HTML(str_glue(
      "<h4>{facet_plot_title}<br><span style = 'font-size: 1.1rem'>{facet_plot_subtitle}</span></h4>"
    ))
    
    facet_card = hts_make_facet_card(facet_card_ls,
                                     title = facet_plot_title_subtitle)
    
    return(facet_card)
  } else if (!stacked & is.null(summarize_by)) {
    # One-way bar plot ---------------------------------------------------------
    if (se) {
      barchart_base = plotly::plot_ly(
        data = dat,
        x = ~ stat,
        y = ~ summarize_var,
        type = 'bar',
        marker = list(color = single_bar_color),
        text = ~ label,
        error_x = list(array = ~ stat_se,
                       color = '#000'),
        textposition = "none",
        hoverinfo = 'text',
        hoverlabel = list(font = list(
          family = "Oswald, Arial, sans-serif",
          size = 0.8
        ))
      )
    } else if (!se) {
      barchart_base = plotly::plot_ly(
        data = dat,
        x = ~ stat,
        y = ~ summarize_var,
        type = 'bar',
        marker = list(color = single_bar_color),
        text = ~ label,
        textposition = "none",
        hoverinfo = 'text',
        hoverlabel = list(font = list(
          family = "Oswald, Arial, sans-serif",
          size = 0.8
        ))
      )
    }
    
    barchart_plot = barchart_base %>%
      layout(
        bargap = 0.1,
        font = list(family = "Oswald, Arial, sans-serif",
                    size = 16),
        xaxis = list(tickformat = plot_tickformat,
                     title = x_title),
        yaxis = list(title = "",
                     # add some space between vertical axis and labels
                     ticksuffix = "  ")
      )
    
    barchart_title = str_glue("{str_to_title(units)} By {summarize_var_desc}")
    
    barchart_subtitle = paste0("N = ",
                               knitr::combine_words(sapply(names(summary_ls$n_ls$unwtd),
                                                           function(x)
                                                             paste0(format(summary_ls$n_ls$unwtd[[x]], big.mark = ","),
                                                                    " ", x))),
                               " Surveyed")
    
    if (weighted) {
      barchart_subtitle = paste0(barchart_subtitle,
                                   "<br>",
                                   "Weighted N = ",
                                   paste0(format(summary_ls$n_ls$wtd[[1]], big.mark = ","), " ", str_to_title(units)))
    }
    
    barchart_title_subtitle = htmltools::HTML(str_glue(
      "<h4>{barchart_title}<br><span style = 'font-size: 1.1rem'>{barchart_subtitle}</span></h4>"
    ))
    
    barchart_card = bslib::card(
      card_title(barchart_title_subtitle),
      card_body(barchart_plot, full_screen = TRUE)
    )
    
    return(barchart_card)
    
  }
  
}
