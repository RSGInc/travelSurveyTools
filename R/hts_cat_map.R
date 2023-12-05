## function needs to return a leaflet based on the type of geog input and variable to summarize
## should return counts/ percent for selected values

# make them match closely to hts_summary
hts_cat_map = function(sf_object,
                           summary_ls,
                           palette = c(
                            "#F68b1f",
                            "#e6645e",
                            "#b05a7a",
                            "#705672",
                            "#48484a"
                           ),
                           weighted = TRUE){
  
  summarize_var = summary_ls$summarize_var
  geog_var = summary_ls$summarize_by
  units = summary_ls$n_ls$units
  
  if (weighted){
    
    df = as.data.frame(summary_ls$cat_summary$wtd)
    
  } else {
    
    df = as.data.frame(summary_ls$cat_summary$unwtd)
    
  }
  
  se = 'est_se' %in% names (df) & 'prop_se' %in% names (df)
  
  # change prop to percent
  df$prop = 100 * df$prop
  
  if (se){
    
    df$prop_se = 100 * df$prop_se
    
  } else {
    
    df = df
    
  }
  
  setDT(df)
  
  geogs = unique(sf_object$name)
  
  layers = unique(df[, get(summarize_var)])
  
  for(geog in geogs){
    
    for(layer in layers){
      
      if(df[get(geog_var) == geog & get(summarize_var) == layer, .N] == 0){
        
        new_row = setDT(data.frame(geog_var = geog, summarize_var = layer))
        setnames(new_row, 'geog_var', geog_var)
        setnames(new_row, 'summarize_var', summarize_var)
        
        df = rbind(df, new_row, fill = TRUE)
        
      }
    
    }
  
  }
  
  
  
  sf_merged = merge(sf_object, df, by.x = 'name', by.y = geog_var, all.x = TRUE)
  
  
  if (se){
  
  sf_merged = sf_merged %>% mutate_at(vars(prop, prop_se, count, est, est_se),
                                      ~replace_na(., 0))
  } else if(weighted) {
    
    sf_merged = sf_merged %>% mutate_at(vars(prop, count, est),
                                        ~replace_na(., 0))
  } else {
    
    sf_merged = sf_merged %>% mutate_at(vars(prop, count),
                                        ~replace_na(., 0))
  }
  
  # Add total county by county
  sf_merged = sf_merged %>%
    group_by(name) %>%
    mutate(geog_total = sum(count)) %>%
    ungroup()
  
  # Add note about insufficient data
  sf_merged = sf_merged %>%
    mutate(insuff_data = 
             ifelse(geog_total <50, 1, 0))
  
  setnames(sf_merged, summarize_var, 'summarize_var')
  
  map_ls = list()
  
  for(layer in layers){
    
    sf_layer_data = sf_merged %>% filter(summarize_var == layer)
    sf_layer = list()
    
    # sufficient data:
    sf_layer$suff = sf_layer_data %>% filter(insuff_data == 0)
    
    #insufficient data:
    sf_layer$insuff = sf_layer_data %>% filter(insuff_data == 1)
    
    # Make labels
    labels = list()
    
    if (weighted){
      
      if (se){
        
        labels$suff = str_glue(
          "<p style = 'font-family: Oswald, Arial, sans-serif;
          font-size: 1rem;
          color: #0b2444'>
          <b><span style = 'font-size:1.5rem'>{sf_layer$suff$name}</b></span>
          <br>{sf_layer$suff$summarize_var}</span>
          <br>N = <b>{prettyNum(sf_layer$suff$count, big.mark = ',')}</b> of <b>{prettyNum(sf_layer$suff$geog_total, big.mark = ',')}</b> {units} surveyed
          <br><b>{prettyNum(sf_layer$suff$est, big.mark = ',')} ± {prettyNum(sf_layer$suff$est_se, big.mark = ',')} </b>{units} total*
          <br><b>{round(sf_layer$suff$prop , 1)}% ± {round(sf_layer$suff$prop_se , 1)}%</b> of {units}*
          <br>*Weighted to total population
          </p>"
        ) %>%
          lapply(htmltools::HTML)
        
        
        labels$insuff = str_glue(
          "<i style = 'font-family: Oswald, Arial, sans-serif;
          font-size: 1rem;
          color: #0b2444'>
          <b><span style = 'font-size:1.5rem'>{sf_layer$insuff$name}</b></span>
          <br>{sf_layer$insuff$summarize_var}</span>
          <br>N = <b>{prettyNum(sf_layer$insuff$count, big.mark = ',')}</b> of <b>{prettyNum(sf_layer$insuff$geog_total, big.mark = ',')}</b> {units} surveyed
          <br><span style = 'color: #e86924'>Low sample size - estimates may be unreliable</span>
          <br><b>{prettyNum(sf_layer$insuff$est, big.mark = ',')} ± {prettyNum(sf_layer$insuff$est_se, big.mark = ',')} </b>{units} total*
          <br><b>{round(sf_layer$insuff$prop , 1)}% ± {round(sf_layer$insuff$prop_se , 1)}%</b> of {units}*
          <br>*Weighted to total population
          </i>"
        ) %>%
          lapply(htmltools::HTML)
        
      } else {
        
        labels$suff = str_glue(
          "<p style = 'font-family: Oswald, Arial, sans-serif;
          font-size: 1rem;
          color: #0b2444'>
          <b><span style = 'font-size:1.5rem'>{sf_layer$suff$name}</b></span>
          <br>{sf_layer$suff$summarize_var}</span>
          <br>N = <b>{prettyNum(sf_layer$suff$count, big.mark = ',')}</b> of <b>{prettyNum(sf_layer$suff$geog_total, big.mark = ',')}</b> {units} surveyed
          <br><b>{prettyNum(sf_layer$suff$est, big.mark = ',')}</b> {units} total*
          <br><b>{round(sf_layer$suff$prop , 1)}%</b> of {units}*
          <br>*Weighted to total population</p>
          </p>"
        ) %>%
          lapply(htmltools::HTML)
        
        
        labels$insuff = str_glue(
          "<i style = 'font-family: Oswald, Arial, sans-serif;
          font-size: 1rem;
          color: #0b2444'>
          <b><span style = 'font-size:1.5rem'>{sf_layer$insuff$name}</b></span>
          <br>{sf_layer$insuff$summarize_var}</span>
          <br>N = <b>{prettyNum(sf_layer$insuff$count, big.mark = ',')}</b> of <b>{prettyNum(sf_layer$insuff$geog_total, big.mark = ',')}</b> {units} surveyed
          <br><span style = 'color: #e86924'>Low sample size - estimates may be unreliable</span>
          <br><b>{prettyNum(sf_layer$insuff$est, big.mark = ',')}</b> {units} total*
          <br><b>{round(sf_layer$insuff$prop , 1)}%</b> of {units}*
          <br>*Weighted to total population
          </i>"
        ) %>%
          lapply(htmltools::HTML)
        
      }
      
      
    } else {
      
      labels$suff = str_glue(
        "<p style = 'font-family: Oswald, Arial, sans-serif;
          font-size: 1rem;
          color: #0b2444'>
          <b><span style = 'font-size:1.5rem'>{sf_layer$suff$name}</b></span>
          <br>{sf_layer$suff$summarize_var}</span>
          <br>N = <b>{prettyNum(sf_layer$suff$count, big.mark = ',')}</b> of <b>{prettyNum(sf_layer$suff$geog_total, big.mark = ',')}</b> {units} surveyed
          <br><b>{round(sf_layer$suff$prop , 1)}%</b> of surveyed {units}*
          </p>"
      ) %>%
        lapply(htmltools::HTML)
      
      labels$insuff = str_glue(
        "<i style = 'font-family: Oswald, Arial, sans-serif;
          font-size: 1rem;
          color: #0b2444'>
          <b><span style = 'font-size:1.5rem'>{sf_layer$insuff$name}</b></span>
          <br>{sf_layer$insuff$summarize_var}</span>
          <br>N = <b>{prettyNum(sf_layer$insuff$count, big.mark = ',')}</b> of <b>{prettyNum(sf_layer$insuff$geog_total, big.mark = ',')}</b> {units} surveyed
          <br><b>{round(sf_layer$insuff$prop , 1)}%</b> of surveyed {units}*
          </i>"
      ) %>%
        lapply(htmltools::HTML)
    }
    
    map_pal <- colorNumeric(palette = palette, domain = sf_layer_data$prop)
    
    map_ls[[layer]] =  
      leaflet(options = leafletOptions(
        minZoom = 5,
        maxZoom = 13,
        zoomControl = FALSE,
        attributionControl = FALSE
      )) %>% 
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = sf_layer$suff,
        opacity = 1,
        fillOpacity = .8,
        fillColor = ~ map_pal(sf_layer$suff$prop),
        color = '#eeeeee',
        label = ~ labels$suff,
        labelOptions = labelOptions(offset = c(10,0)),
        group = layer,
        weight = 1,
        highlightOptions = highlightOptions(weight = 3, color = "#fefefe")
      ) %>%
      addPolygons(
        data = sf_layer$insuff,
        opacity = 1,
        fillOpacity = .2,
        fillColor = ~ map_pal(sf_layer$insuff$prop),
        color = '#eeeeee',
        label = ~ labels$insuff,
        labelOptions = labelOptions(offset = c(10,0)),
        group = layer,
        weight = 1,
        highlightOptions = highlightOptions(weight = 3, color = "#fefefe")
      ) %>%
      addLegend(title = paste0("Percent of ", units),
                pal = map_pal,
                group = layer,
                position = "topleft",
                values = sf_layer_data$prop,
                labFormat = labelFormat(suffix = "%"))
      
  }
 
  return(map_ls)
  
}
