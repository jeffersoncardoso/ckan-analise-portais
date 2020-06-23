
grafico_formatos <- function(datasetsFormatos) {
  tabular <- c('CSV', 'ODS', 'XLSX', 'XLS')
  textual <- c('PDF', 'DOC', 'DOCX', 'TXT', 'RDF')
  zip <- c('ZIP')
  imagens <- c('PNG', 'JPG', 'JPEG', 'SVG')
  
  graph <- datasetsFormatos %>%
    mutate(format = sub("\\.", "", toupper(format))) %>%
    mutate(format = ifelse(format %in% c(tabular, textual, zip, imagens), format, 'Outros')) %>%
    mutate(format = ifelse(format %in% tabular, 'Tabular', format)) %>%
    mutate(format = ifelse(format %in% textual, 'Textual', format)) %>%
    mutate(format = ifelse(grepl(zip, format), 'ZIP', format)) %>%
    mutate(format = ifelse(format %in% imagens, 'Imagens', format)) %>%
    group_by(format) %>% 
    summarise(count = n())
  
  graph$Formato <- graph$format
  graph$Percentual <- round((graph$count / sum(graph$count)) * 100)
  graph <- graph %>% arrange(desc(count))
  
  print(graph)
  
  ggplot(data = graph) %+%
    geom_bar(aes(x = "", y = Percentual, fill = Formato), stat="identity", width = 1) %+%
    coord_polar("y", start = 0) %+%
    theme_void() %+%
    geom_text(aes(x=1, y = cumsum(Percentual) - Percentual/2, label = Percentual))
  
}


grafico_proporcao_tabular <- function(arquivos) {
  tabular <- c('CSV', 'ODS', 'XLSX', 'XLS')
  
  graph <- arquivos %>%
    mutate(format = sub("\\.", "", toupper(format))) %>%
    mutate(format = ifelse(format %in% tabular, 'Tabular', format)) %>%
    mutate(format = ifelse(format != 'Tabular', 'Outros', format)) %>%
    group_by(site, format) %>% 
    summarise(count = n()) %>%
    mutate(freq = round(count / sum(count)* 100)) %>%
    ungroup(site) %>%
    filter(format != 'Outros') %>%
    mutate(site = fct_reorder(site, desc(freq)))
  
  print(graph)
  
  ggplot(graph, aes(x=site, y=freq)) %+% 
    geom_bar(stat = "identity", fill = "#6495ED") %+%
    coord_flip() %+%
    xlab("Portais") %+% 
    ylab("Dados tabulares")
}
