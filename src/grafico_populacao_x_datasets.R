library('dplyr')
library('ggplot2')
library('stringr')

grafico_populacao_x_datasets <- function(datasets) {
  grupos <- datasets %>% group_by(site) %>%
    summarise(total = n(), populacao = buscar_populacao(site))
  
  grupos$populacao <- round(grupos$populacao / 1000000)
  ggplot(grupos, aes(x = populacao, y = total)) %+%
    geom_point() %+%
    xlab("População (em milhões)") %+%
    ylab("Datasets") %+%
    geom_text(
      label = word(str_replace(grupos$site, "-", " ")), 
      nudge_x = 0.25, nudge_y = 0.25,
      show.legend = T
    )
  
}

buscar_populacao <- function (site) {
  if(site[1] == "Alagoas")
    return(3337357)
  if(site[1] == "Distrito Federal")
    return(3015268)
  if(site[1] == "Minas Gerais")
    return(21168791)
  if(site[1] == "Rio Grande do Sul")
    return(11377239)
  if(site[1] == "Santa Catarina")
    return(7164788)
  if(site[1] == "Sao Paulo")
    return(45919049)
  if(site[1] == "Pernambuco")
    return(9557071)
  if(site[1] == "Fortaleza-CE")
    return(2669342)
  if(site[1] == "Belo Horizonte - MG")
    return(2512070)
  if(site[1] == "Porto Alegre - RS")
    return(1483771)
  if(site[1] == "Sao Paulo - SP")
    return(12252023)
  if(site[1] == "Natal - RN")
    return(884122)
}
