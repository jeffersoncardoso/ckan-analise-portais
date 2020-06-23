library("dplyr")
library("wordcloud2")

gerar_nuvem_palavras <- function(datasets, tags_datasets, grupos_datasets, tamanho = 0.5) {
  
  total_grupos <- grupos_datasets %>% 
    mutate(name = tolower(title)) %>%
    group_by(site, name) %>%
    summarise(total = n()) %>%
    arrange(desc(total))
  
  total_tags <- tags_datasets %>% 
    mutate(name = tolower(display_name)) %>%
    group_by(site, name) %>%
    summarise(total = n()) %>%
    arrange(desc(total))
  
  grupos_tags <- total_grupos %>% union(total_tags) %>% 
    arrange(desc(total)) %>% 
    filter(name != tolower(site), name != 'sim') %>%
    group_by(name) %>% 
    summarise(total = sum(total)) %>%
    arrange(desc(total))

  print(grupos_tags)
  
  wordcloud2(grupos_tags, size=tamanho)
}