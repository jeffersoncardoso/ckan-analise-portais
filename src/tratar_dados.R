library('dplyr')

carregar_dataset <- function(nomeSite)
{
  dataset <- read.csv2(paste("./mineracao", nomeSite, "datasets.csv", sep = "/"))  
  dataset <- dataset %>% mutate(site = nomeSite) %>%
    filter(license_title != 'Nao Fornecida. Consulte a Fonte dos Dados')
  
  return(dataset)
}

carregar_arquivos <- function(nomeSite)
{
  arquivos <- read.csv2(paste("./mineracao", nomeSite, "arquivos.csv", sep = "/"))  
  arquivos <- arquivos %>% 
    select(id, name, package_id, format, created, url, size) %>% 
    mutate(site = nomeSite, size = ifelse(is.na(size), 0, size)) %>%
    transform(created = as.Date(created))
  #arquivos$site <- nomeSite
  #arquivos$webstore_last_updated <- NULL
  #arquivos$hash <- NULL
  #arquivos$webstore_url <- NULL
  #arquivos$datastore_active <- NULL
  #arquivos$revision_timestamp <- NULL
  #arquivos$last_modified <- NULL
  #arquivos$size <- as.integer(arquivos$size)
  #arquivos$mimetype <- as.character(arquivos$mimetype)
  #arquivos$description <- as.character(arquivos$description)
  
  return(arquivos)
}


carregar_grupos <- function(nomeSite)
{
  dir <- paste("./mineracao", nomeSite, "grupos_pacotes.csv", sep = "/")
  
  if(!file.exists(dir))
    return(FALSE)
  
  grupos <- read.csv2(dir)
  grupos <- grupos %>% mutate(site = nomeSite) %>% select(id, title, package_id, site)
  
  return(grupos)
}

carregar_tags <- function(nomeSite)
{
  dir <- paste("./mineracao", nomeSite, "tags_pacotes.csv", sep = "/")
  
  if(!file.exists(dir))
    return(FALSE)
    
  print(paste("Carregando tags de", nomeSite))
  tags <- read.csv2(dir)
  tags <- tags %>% mutate(site = nomeSite) %>% select(id, display_name, package_id, site)
  
  return(tags)
}
  

buscar_todos_datasets <- function(sites)
{
  df <- carregar_dataset(sites[1])
  
  for(i in 2:length(sites)) {
    df <- df %>% union(carregar_dataset(sites[i]))
  }
  
  return(df)
}


buscar_todos_arquivos <- function(sites)
{
  df <- carregar_arquivos(sites[1])
  
  for(i in 2:length(sites)) {
    df <- df %>% union(carregar_arquivos(sites[i]))
  }
  
  return(df)
}

buscar_todos_grupos <- function(sites)
{
  df <- carregar_grupos(sites[1])
  
  for(i in 2:length(sites)) {
    newDf <- carregar_grupos(sites[i])
    if(newDf != FALSE)
      df <- df %>% union(newDf)
  }
  
  return(df)
}

buscar_todas_tags <- function(sites)
{
  df <- carregar_tags(sites[1])
  
  for(i in 2:length(sites)) {
    newDf <- carregar_tags(sites[i])
    if(newDf != FALSE)
      df <- df %>% union(newDf)
  }
  
  return(df)
}