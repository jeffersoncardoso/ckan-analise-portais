lerDataset <- function(p_dados, p_site) {
  return (
    p_dados %>% select(format, created, url, name) %>% 
      mutate(site = p_site) %>%
      transform(created = as.Date(created), 
                dia = format(as.Date(created),"%d"),
                mesano = as.Date(format(as.Date(created),"%Y-%m-01")),
                mes = format(as.Date(created),"%m"),
                ano = format(as.Date(created),"%Y")
      )
  )
}
portais <- read.csv2("./catalogos.csv")

datasetsBeloHorizonte <- lerDataset(read.csv2("./recursos/cidade-belo-horizonte.csv"), "Belo Horizonte")
datasetsFortaleza     <- lerDataset(read.csv2("./recursos/cidade-fortaleza.csv"), "Fortaleza")
datasetsNatal         <- lerDataset(read.csv2("./recursos/cidade-natal.csv"), "Natal")
datasetsPoa           <- lerDataset(read.csv2("./recursos/cidade-porto-alegre.csv"), "Porto Alegre")
datasetsRecife        <- lerDataset(read.csv2("./recursos/cidade-recife.csv"), "Recife")
datasetsSaoPaulo      <- lerDataset(read.csv2("./recursos/cidade-sao-paulo.csv"), "São Paulo")

datasetsAL <- lerDataset(read.csv2("./recursos/estado-al.csv"), "AL")
datasetsMG <- lerDataset(read.csv2("./recursos/estado-mg.csv"), "MG")
datasetsPE <- lerDataset(read.csv2("./recursos/estado-pe.csv"), "PE")
datasetsRS <- lerDataset(read.csv2("./recursos/estado-rs.csv"), "RS")
datasetsRS <- datasetsRS %>% filter(created != as.Date('2017-05-02'))
datasetsSC <- lerDataset(read.csv2("./recursos/estado-sc.csv"), "SC")
datasetsSP <- lerDataset(read.csv2("./recursos/estado-sp.csv"), "SP")
datasetsDF <- lerDataset(read.csv2("./recursos/estado-df.csv"), "DF")


datasetsEstadual <- datasetsAL %>%
  union(datasetsMG) %>% union(datasetsPE) %>%
  union(datasetsRS) %>% union(datasetsSC) %>%
  union(datasetsSP) %>% union(datasetsDF)
datasetsEstadual <- datasetsEstadual %>% filter(format != "", created >= as.Date('2015-01-01'))
datasetsMunicipal <- datasetsBeloHorizonte %>% union(datasetsFortaleza) %>%
  union(datasetsNatal) %>% union(datasetsPoa) %>% 
  union(datasetsRecife) %>% union(datasetsSaoPaulo)
datasetsMunicipal <- datasetsMunicipal %>% filter(format != "", created >= as.Date('2015-01-01'))
datasets <- datasetsEstadual %>% union(datasetsMunicipal)

datasets %>% group_by(site) %>% summarise(Total = n()) %>% arrange(desc(Total))

source('src/grafico_formatos.R')
grafico_formatos(datasetsMunicipal)# %>% filter(site %in% c('Fortaleza'))

source('src/grafico_ano.R')
grafico_ano(datasets)

source('src/grafico_populacao.R')
grafico_populacao(datasets %>% filter(!(site %in% c('SP', 'AL'))))

#%>% filter( toupper(format) %in% c('CSV', 'XLSX', 'XLS', 'HTML', 'XML', 'PDF', 'DOC', 'DOCX', 'TXT', 'RDF', 'ZIP') )

datas <- datasets %>% filter() %>% group_by(mesano, site) %>% summarise(Total = n())
ggplot(datas, aes(x=mesano, y=Total, fill = site)) %+% 
  geom_bar(stat='identity') %+%
  coord_cartesian(ylim = c(0,1000))
