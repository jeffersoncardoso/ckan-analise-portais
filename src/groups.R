library('ckanr')
library('httr')
library("wordcloud")
library("rjson")
library("ggplot2")
library("stringr")


sites <- list(
  #'br' = "http://dados.gov.br",
  'alagoas' = "http://dados.al.gov.br",
  'fortaleza' = "http://dados.fortaleza.ce.gov.br",
  'brasilia' = "http://dados.df.gov.br",
  'minas-gerais' = "http://www.transparencia.dadosabertos.mg.gov.br",
  'belo-horizonte' = "https://dados.pbh.gov.br",
  'recife' = "http://dados.recife.pe.gov.br",
  'pernambuco' = "http://web.transparencia.pe.gov.br/ckan",
  'natal' = "http://dados.natal.br",
  'rs' = "http://dados.rs.gov.br",
  'rs-tece' = "http://dados.tce.rs.gov.br",
  'sao-paulo' = "http://catalogo.governoaberto.sp.gov.br/",
  'poa' = "http://datapoa.com.br",
  'sp' = "http://dados.prefeitura.sp.gov.br/",
  'sc' = "http://dados.sc.gov.br/"
)

package_search(q = 'created:2020-05-20')


grupos <- listarGrupos("http://dados.gov.br")
for (site in sites) {
  print(site)
  grupos <- rbind(grupos, listarGrupos(site))
}


replace()
str_replace(string = sites[1], pattern = c(":", "//"), replacement = "")
sites[1]

unique(sp$id)
listarDatasets("http://www.transparencia.dadosabertos.mg.gov.br")

for (site in names(sites)) {
  datasets <- listarDatasets(sites[site])
  write.csv2(datasets, file = paste('recursos/', site, '.csv', sep = ''), sep = ";")
}


length(grupos[grupos$Site == "http://dados.tce.rs.gov.br", ]$Grupo)

length(package_list(as = 'table', url = "http://dados.gov.br", limit = 99999))

data <- resource_search(url = "http://dados.gov.br", q = 'format:json')
data <- data$results[[1]]$created
resource_search(url = "http://dados.gov.br", q = 'format:csv', as = table)


listarGrupos <- function(site) {
  grupos <- list(c(), c(), c())
  names(grupos) <- c("Site", "Grupo", "Datasets")
  
  groups <- group_list(url = site, limit = 1000, as = 'table', all_fields = TRUE)
  names(groups)[names(groups) == "packages"] <- 'package_count'
  
  grupos$Grupo = groups$title
  grupos$Datasets = groups$package_count
  grupos$Site[1:length(grupos$Grupo)] <- site
  
  grupos <- as.data.frame(grupos)
  
  return(grupos)
}

listarDatasets <- function(p_site, limit = 99999) {
  print(p_site)
  dados <- resource_search(q = 'name:', url = p_site, limit = limit, as = 'table')
  dados = dados$results
  #dados$site = p_site
  
  return(dados)
}


dadosRS <- listarDatasets('http://dados.rs.gov.br')

library("dplyr")

listarPacotes <- function(p_site, limit = 99999) {
  print(p_site)
  pacotes <- package_list_current(limit = 99999, as = "table", url = p_site)
  tabelaPacotes <- pacotes %>% select(id, name, type, license_id, license_title, num_resources)
  
  return(tabelaPacotes)
}

pacotes <- listarPacotes('http://dados.sc.gov.br', limit = 1000)
pacotes$


names(pacotes)

length(pacotes)
length(pacotes$resources)

pacotes$results$resources
length(pacotes$created)
dadosRS$
length(unique(pacotes$package_id))


teste <- resource_search(
  q = "name:agricultura", 
  url = 'http://dados.rs.gov.br', 
  as = 'table'
)

package_list(as = 'table', url = 'http://dados.sc.gov.br/')
package_list

teste <- package_list_current(as = 'table', limit = 1000, url = 'http://dados.al.gov.br')



unique(dadosRS$package_id)

pacote <- package_show(dadosRS$package_id[1], url = 'http://dados.rs.gov.br')
pacote$

listarPacotes <- function(p_site)  {
  
}





unique(trimws(dados$format))

#dados[which(!is.na(dados$resource_group_id)), ]



dados$results

packageVersion("ckanr")

licenses <- license_list(url = 'http://dados.gov.br', as = 'table')
unique(df$format)
unique(df$mimetype)

install.packages("dplyr")



topFive <- groups[order(groups$package_count, decreasing = TRUE), ]
topFive <- topFive[1:3,]
print(topFive)



ggplot(data = groups, aes(x=packages, fill=title)) + geom_bar()



datasets2$results$title
groups$title
length(datasets)

result <- resource_search(q = 'name:', limit = 10000, as = 'table')$results
length(result)

unique(result$format)


read.csv(result$url[1], sep = ";")
read.csv(result$url[1], sep = ";")


allGroups = c()

for(site in sites) {
  ckanr_setup(url = sites[1])
  groups <- group_list(as="table")
  allGroups <- c(allGroups, groups)
}

length(allGroups)


ckanr_setup(url = "http://dados.gov.br/")
tags <- tag_list()
tags$results

tags <- tag_list(as = 'table')
tags$display_name
tags[1:1000]$name

