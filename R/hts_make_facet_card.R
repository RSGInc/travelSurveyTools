
hts_make_facet_card = function(cards_list, title){

  #bslib total width 
  total_wdith = 12
  
  #plot 3 plots per rows max
  cards_per_row = 2
  
  #initialize
  cards_used = 0
  i = 1
  row_list = list()
  
  num_cards = length(cards_list)
  
  rows_to_make = ceiling(num_cards / cards_per_row)

  for (row in 1:rows_to_make){
    
    row_name = paste0('row_', row)
    
    num_plots = min(c(cards_per_row, num_cards - cards_used))
    
    width_per_plot = total_wdith / num_plots
    
    if (num_plots == 3){
    
    layout = layout_columns(
      col_widths = rep(width_per_plot, num_plots),
      cards_list[i], cards_list[i+1], cards_list[i+2]
    )
    } else if (num_plots == 2) {
      
      layout = layout_columns(
        col_widths = rep(width_per_plot, num_plots),
        cards_list[i], cards_list[i+1]
      )
    } else {
      
      layout = layout_columns(
        col_widths = rep(width_per_plot, num_plots),
        cards_list[i]
      )
    }
    
    #advance i to next plot that hasn't been used
    i = i + num_plots
    
    row_list[[row]] = layout
    
    # assign(row_name, layout)
    
    
  }
  
  card = card(
    card_title(title),
    row_list
  )
  
  return(card)

}

