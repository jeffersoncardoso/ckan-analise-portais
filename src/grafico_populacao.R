library("ggplot2")
library(forcats)

grafico_populacao <- function(datasetsPopulacao) {
  graph <- datasetsPopulacao %>% 
                  group_by(site) %>% 
                  summarise(total = n()) %>%
                  mutate(site = fct_reorder(site, desc(total)))
  
  ggplot(graph, aes(x=site, y=total)) %+% 
    geom_bar(stat = "identity") %+%
    coord_flip() %+%
    xlab("Portais") %+% 
    ylab("Datasets")
  
}

