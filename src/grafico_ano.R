library("ggplot2")

grafico_ano <- function(datasetsPopulacao) {
  graph <- datasetsPopulacao %>% 
    group_by(mesano, site) %>% 
    summarise(total = n())
  print(graph)
  
  ggplot(graph, aes(x = mesano, y = total, group = site, color = site)) %+% geom_line()
}

