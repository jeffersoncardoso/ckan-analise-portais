library("dplyr")
library("ckanr")
source('src/minerar_dados.R')
source('src/tratar_dados.R')

##Graficos, tabelas e nuvens de palavras
source('src/grafico_populacao.R')
source('src/grafico_formatos.R')
source('src/grafico_ano.R')
source('src/grafico_tamanho.R')
source('src/nuvem_palavras.R')


result <- package_search(as = 'table', url = 'http://dados.al.gov.br', limit = 500)
track <- result$results
track[1, ]$tracking_summary
################################################


#Minerar os sites
minerar_sites()

estados <- c(
  "Alagoas", "Distrito Federal", "Minas Gerais", "Rio Grande do Sul", "Santa Catarina", "Sao Paulo", "Pernambuco"
)

municipios <- c(
  "Fortaleza-CE", "Belo Horizonte - MG", "Porto Alegre - RS", "Sao Paulo - SP", "Natal - RN"
)

sites <- c(estados, municipios)
length((buscar_todos_datasets(estados))$id)
length((buscar_todos_datasets(municipios))$id)
length((buscar_todos_datasets(sites))$id)

datasets <- buscar_todos_datasets(sites)

datasets <- datasets %>% filter(format(as.Date(metadata_created), '%Y') == 2020)
arquivos <- buscar_todos_arquivos(pastas) %>% filter(package_id %in% datasets$id)
tags <- buscar_todas_tags(pastas)# %>% filter(package_id %in% datasets$id)
grupos <- buscar_todos_grupos(pastas)# %>% filter(package_id %in% datasets$id)
arquivos <- arquivos %>% filter(package_id %in% datasets$id)


datasets <- buscar_todos_datasets(sites)
grafico_idade_criacao(datasets)

datasets
datasets %>% filter(
  !(id %in% tags$package_id)
) %>% filter(
  !(id %in% grupos$package_id)
)

datasets %>% filter(
  
)



#517 + 1171

gerar_nuvem_palavras(datasets, tags, grupos)


##Tabela datasets x site
tabela <- datasets %>% group_by(site) %>% summarise(Datasets = n(), Arquivos = sum(num_resources))
write.csv2(tabela, './arquivos_artigo/tabela-sites-x-datasets.csv')

tabela_tamanho_arquivos(arquivos)


grafico_populacao(datasets)
grafico_formatos(arquivos)
tabela <- tabela_tamanho_arquivos(arquivos)





ggplot(graphData, aes(x=mesano, y=Total, fill = site)) %+% 
    geom_bar(stat='identity', position='dodge') %+%
    ylim(0, 300)

ggplot(graphData, aes(x=mesano, y=Total, fill = site)) %+% 
  geom_bar(stat='identity') %+%
  coord_cartesian(ylim = c(0,750))

###############################################

datasets <- buscar_todos_datasets(sites)
grafico_idade_criacao(datasets)


###############################################


#datasets$format

ggplot(graphData, aes(x=mesano, y=Total, color = site)) %+% 
  geom_point() %+%
  ylim(0, 10)




formatos <- datasets %>% group_by(format, site) %>% summarise(Total = n())
formatos <- formatos %>% group_by(format) %>% summarise(Total = sum(Total))

ggplot(data = formatos) %+% geom_bar()






