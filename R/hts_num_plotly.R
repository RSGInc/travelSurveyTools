
hts_num_plotly = function(summary_ls,
                          stat = "mean", # or median
                          weighted = TRUE,
                          single_bar_color = "#006fa1",
                          errorbar_color = "#17384e",
                          text_color = "#17384e") {
  # Select Data ----------------------------------------------------------------
  
  # if weighted and there is no weighted summary, set weighted to FALSE
  if(weighted & is.null(summary_ls$num_summary$wtd)){
    weighted = FALSE
  }
  
  if (weighted) {
    dat = copy(summary_ls$num_summary$wtd)
  } else {
    dat = copy(summary_ls$num_summary$unwtd)
  }
  
  # Get variables --------------------------------------------------------------
  summarize_var = summary_ls$summarize_var
  summarize_by = summary_ls$summarize_by
  
  if (is.null(summarize_by)) {
    stop("hts_num_plotly needs a summarize_by variable: use hts_cat_plotly instead to make a histogram")
  }
  
  units = summary_ls$n_ls$units
  
  setnames(dat,
           old = stat,
           new = "stat")
  
  # Are standard errors present?
  se = paste0(stat, "_se") %in% names (dat)
  
  if (se) {
    setnames(dat,
             old = paste0(stat, "_se"),
             new = "stat_se")
  }
  
  # Add total by-variable
  
  setnames(dat,
           old = summarize_by,
           new = "summarize_by",
           skip_absent = TRUE)
  dat = dat[, by_total := sum(count), by = summarize_by]
  
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
    
    count_line =
      str_glue("<br>N = {count} {str_to_title(units)} Surveyed",
               count = prettyNum(d$count, big.mark = ","))
    
    insuff_line =
      ifelse(d$by_total < 50,
             "<br>Low sample size - estimates may be unreliable",
             "")
    
    stat_line = ifelse(
      se,
      str_glue(
        "<br><b>{stat_name} {summarize_var_desc}: {stat_i} ± {stat_se_i}</b>*",
        stat_name = fcase(stat == "mean", "Average",
                          stat == "median", "Median"),
        stat_i = round(d$stat, 1),
        stat_se_i = round(d$stat_se, 1)
      ),
      
      str_glue(
        "<br><b>{stat_name} {summarize_var_desc}: {stat_i}</b>*",
        stat_name = fcase(stat == "mean", "Average",
                          stat == "median", "Median"),
        stat_i = round(d$stat, 1)
      )
    )
    
    weight_line = ifelse(weighted,
                         "<br>*Weighted",
                         "<br>*Unweighted")
    
    end_style =
      ifelse(d$by_total < 50, "</i>", "")
    
    row_label = paste0(
      main_style,
      by_line,
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
  dat[, summarize_var := factor(summarize_var, levels = unique(summarize_var))]
  
  
  dat$summarize_by %>% levels()
  dat[, order(summarize_var, summarize_by)]
  dat[, summarize_by := stringr::str_wrap(summarize_by, width = 30)]
  dat[, summarize_by := factor(summarize_by, levels = rev(unique(summarize_by)))]
  
  
  # Plot globals ---------------------------------------------------------------
  x_title = stringr::str_to_title(
    str_glue(
      "{stat_name} {summarize_var_desc} ({weight_type})",
      stat_name = fcase(stat == "mean", "Average",
                        stat == "median", "Median"),
      weight_type = fcase(weighted, 'Weighted',
                          !weighted, 'Unweighted')
    )
  )
  
  
  
  if (se) {
    barchart_base = plotly::plot_ly(
      data = dat,
      x = ~ stat,
      y = ~ summarize_by,
      type = 'bar',
      marker = list(color = single_bar_color),
      text = ~ label,
      error_x = list(array = ~ stat_se,
                     color = '#000'),
      textposition = "none",
      hoverinfo = 'text',
      hoverlabel = list(font = list(family = "Oswald, Arial, sans-serif",
                                    size = 0.8))
    )
  } else if (!se) {
    barchart_base = plotly::plot_ly(
      data = dat,
      x = ~ stat,
      y = ~ summarize_by,
      type = 'bar',
      marker = list(color = single_bar_color),
      text = ~ label,
      textposition = "none",
      hoverinfo = 'text',
      hoverlabel = list(font = list(family = "Oswald, Arial, sans-serif",
                                    size = 0.8))
    )
  }
  
  barchart_plot = barchart_base %>%
    layout(
      bargap = 0.1,
      font = list(family = "Oswald, Arial, sans-serif",
                  size = 16),
      xaxis = list(title = x_title),
      yaxis = list(title = "",
                   # add some space between vertical axis and labels
                   ticksuffix = "  ")
    )
  
  barchart_title = str_glue("{x_title} By {summarize_by_desc}")
  
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
