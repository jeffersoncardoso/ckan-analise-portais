library('ckanr')
library('dplyr')

minerar_site <- function(site, diretorio, limite = 1000, inicio = 0) {
  print(paste("Minerando dados de", site))
  
  pacotes <- package_list_current(limit = limite, as = "table", url = site, offset = inicio)
  datasets <- pacotes %>% select(id, license_id, license_title, title, num_resources, num_tags, metadata_created, metadata_modified)
  
  for(i in 1:length(pacotes$id)) {
    if(length(as.data.frame(pacotes[i, ]$tags)) > 0)
      pacotes[i, ]$tags[[1]]$package_id <- pacotes[i, ]$id
    
    if(length(as.data.frame(pacotes[i, ]$groups)) > 0)
      pacotes[i, ]$groups[[1]]$package_id <- pacotes[i, ]$id
    
    if(length(as.data.frame(pacotes[i, ]$resources)) > 0)
      pacotes[i, ]$resources[[1]]$package_id <- pacotes[i, ]$id
  }
  
  arquivos <- do.call("rbind", pacotes$resources)
  tags_pacotes <- do.call("rbind", pacotes$tags)
  grupos_pacotes <- do.call("rbind", pacotes$groups)

  sufixo <- ifelse(inicio > 0, paste("_", inicio, sep = ""), "")
  
  write.csv2(datasets, file = paste(diretorio, '/datasets', sufixo, '.csv', sep = ''))
  write.csv2(arquivos, file = paste(diretorio, '/arquivos', sufixo, '.csv', sep = ''))
  
  if(!is.null(names(tags_pacotes)))
    write.csv2(tags_pacotes, file = paste(diretorio, '/tags_pacotes', sufixo, '.csv',  sep = ''))

  if(!is.null(names(grupos_pacotes)))
    write.csv2(grupos_pacotes, file = paste(diretorio, '/grupos_pacotes', sufixo, '.csv', sep = ''))
}

minerar_sites <- function() {
  minerar_site(site = 'http://dados.al.gov.br', diretorio = './mineracao/Alagoas')
  minerar_site(site = 'http://dados.fortaleza.ce.gov.br', diretorio = './mineracao/Fortaleza-CE')
  minerar_site(site = 'http://dados.df.gov.br', diretorio = './mineracao/Distrito Federal')
  minerar_site(site = 'http://www.transparencia.dadosabertos.mg.gov.br', diretorio = 'mineracao/Minas Gerais')
  minerar_site(site = 'https://dados.pbh.gov.br', diretorio = './mineracao/Belo Horizonte - MG')
  minerar_site(site = 'http://dados.rs.gov.br', diretorio = './mineracao/Rio Grande do Sul', inicio = 1)
  minerar_site(site = 'http://dados.rs.gov.br', diretorio = './mineracao/Rio Grande do Sul', inicio = 1001)
  minerar_site(site = 'http://datapoa.com.br/', diretorio = './mineracao/Porto Alegre - RS')
  minerar_site(site = 'http://dados.sc.gov.br', diretorio = './mineracao/Santa Catarina')
  minerar_site(site = 'http://catalogo.governoaberto.sp.gov.br/', diretorio = './mineracao/Sao Paulo')
  minerar_site(site = 'http://dados.prefeitura.sp.gov.br', diretorio = './mineracao/Sao Paulo - SP')
  minerar_site(site = 'http://web.transparencia.pe.gov.br/ckan', diretorio = './mineracao/Pernambuco')
  minerar_site(site = 'http://dados.natal.br', diretorio = './mineracao/Natal - RN')
}