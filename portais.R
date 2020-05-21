library('ckanr')

portais <- read.csv2("./catalogos.csv")

portaisCKAN <- portais[portais$Solução == 'CKAN', ]

regiaoNorte <- C('AM', 'RR', 'AP', 'PA', 'TO', 'RO', 'AC')

regiaoSul <- C('')


