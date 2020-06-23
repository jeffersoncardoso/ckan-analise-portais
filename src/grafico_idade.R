library("ggplot2")

tabela_ano_criacao <- function(datasets) {
  datasets$metadata_created <- as.Date(datasets$metadata_created)
  datasets$ano <- as.integer(format(datasets$metadata_created, "%Y"))
  
  return(datasets)
}

tabela_idade_cricao <- function(datasets) {
  datasets$metadata_created <- as.Date(datasets$metadata_created)
  datasets$criacao <- as.Date('2020-06-01') - datasets$metadata_created
  
  tabelaIdade <- datasets %>% 
    group_by( 
      site, 
      faixa = cut(as.numeric(criacao), breaks = seq(0, 2190, by = 365), dig.lab = 4) 
    ) %>%
    summarise(total= n())
  
  tabelaIdade$faixa <- as.character(tabelaIdade$faixa)
  tabelaIdade$faixa[tabelaIdade$faixa == "(0,365]"] <- "< 1 Ano"
  tabelaIdade$faixa[tabelaIdade$faixa == "(365,730]"] <- "1 a 2 anos"
  tabelaIdade$faixa[tabelaIdade$faixa == "(730,1095]"] <- "2 a 3 anos"
  tabelaIdade$faixa[tabelaIdade$faixa == "(1095,1460]"] <- "3 a 4 anos"
  tabelaIdade$faixa[tabelaIdade$faixa == "(1460,1825]"] <- "4 a 5 anos"
  tabelaIdade$faixa[tabelaIdade$faixa == "(1825,2190]"] <- "5 anos ou mais"
  
  
  return(tabelaIdade)
}

grafico_idade_criacao <- function(datasets) {
  tabelaIdade <- tabela_idade_cricao(datasets)
  
  print(tabelaIdade)
  
  ggplot(tabelaIdade, aes(x=faixa, y=total, fill = site)) %+% 
    geom_bar(stat='identity') %+% xlab("Idade") %+% ylab("Total")
}

tabela_idade_modificacao <- function(datasets) {
  
  datasets$metadata_modified <- as.Date(datasets$metadata_modified)
  datasets$modificacao <- as.Date('2020-06-01') - datasets$metadata_modified
  
  tabelaModificacao <- data.frame(
    Tamanho = c('< 3 meses', '3 a 6 meses', '6 a 1 ano', '1 a 3 anos', '3 anos ou mais'),
    Quantidade = c(
      length((datasets %>% filter(modificacao >= 0, modificacao < 3*30))$id),
      length((datasets %>% filter(modificacao >= 3*30, modificacao < 6*30))$id),
      length((datasets %>% filter(modificacao >= 6*30, modificacao < 365))$id),
      length((datasets %>% filter(modificacao >= 365, modificacao < 3*365))$id),
      length((datasets %>% filter(modificacao >= 3*365))$id)
    )
  )
  
  return(tabelaModificacao)
}

grafico_idade_modificacao <- function(datasets) {
  tabelaModificacao <- tabela_idade_modificacao(datasets)
  print(tabelaModificacao)
  
  ggplot(tabelaModificacao, aes(x=Tamanho, y=Quantidade)) %+% geom_bar(stat = "identity")
}