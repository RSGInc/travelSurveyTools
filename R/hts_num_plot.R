hts_num_plot = function (summary_ls,
                         measure,
                         weighted = TRUE,
                         plotly = TRUE,
                         single_bar_color = "#006fa1",
                         errorbar_color = "#17384e",
                         text_color = "#17384e") {
  if (weighted) {
    
    dat = summary_ls$num_summary$wtd
    
  } else {
    
    dat = summary_ls$num_summary$unwtd
    
  }
  
  summarize_var = names(dat)[1]
  summarize_by = names(summary_ls$cat_summary$unwtd)[2]
  
  if (measure == 'mean') {
    
    plot = ggplot2::ggplot(data = dat,
                           ggplot2::aes(
                             x = mean,
                             y = dat[[1]],
                             text = str_wrap(
                               paste0(
                                 str_to_title(names(dat)[1]),
                                 ': ',
                                 dat[[1]],
                                 '<br>Mean: ',
                                 round(mean, 1)
                               ),
                               width = 60
                             )
                           )) +
      ggplot2::geom_bar(stat = "identity",
                        fill = single_bar_color) +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = mean - mean_se,
                                           xmax = mean + mean_se),
                              height = 0.2,
                              color = errorbar_color) + 
      ggplot2::scale_y_discrete(limits = rev)
    
  } else
    if (measure == 'median') {
      plot = ggplot2::ggplot(data = dat,
                             ggplot2::aes(
                               x = median,
                               y = dat[[summarize_var]],
                               text = str_wrap(
                                 paste0(
                                   str_to_title(names(dat)[1]),
                                   ': ',
                                   dat[[1]],
                                   '<br>Median: ',
                                   round(median, 1)
                                 ),
                                 width = 60
                               )
                             )) +
        ggplot2::geom_bar(stat = "identity",
                          fill = single_bar_color) + 
        ggplot2::scale_y_discrete(limits = rev)
    }
  
  plot = plot +
    ggplot2::labs(
      x =
        stringr::str_to_sentence(stringr::str_replace_all(summarize_by,
                                                          '_',
                                                          ' ')),
      y = stringr::str_replace_all(summarize_var,
                                   '_',
                                   ' '),
      fill = stringr::str_to_sentence(stringr::str_replace_all(summarize_var,
                                                               '_',
                                                               ' ')),
      title = paste0(
        str_to_title(measure),
        ' ',
        str_to_title(stringr::str_replace_all(
          paste0(summarize_by, ' by ', summarize_var),
          '_',
          ' '
        )),
        subtitle = if (weighted) {
          "Weighted"
        } else {
          "Unweighted"
        }
      )
    ) +
    hts_plot_theme
  
  plot
}
