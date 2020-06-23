
tabela_tamanho_arquivos <- function(arquivos) {
  comTamanho <- arquivos %>% filter(size > 0) %>% select(size)
  
  tabelaTamanhos <- data.frame(
    Tamanho = c('0-1K', '1K-10K', '10K-100K', '100K-1M', '1M-10M', '>10M'),
    Quantidade = c(
      length((comTamanho %>% filter(size >= 0, size < 1000))$size),
      length((comTamanho %>% filter(size >= 1000, size < 10000))$size),
      length((comTamanho %>% filter(size >= 10000, size < 100000))$size),
      length((comTamanho %>% filter(size >= 100000, size < 1000000))$size),
      length((comTamanho %>% filter(size >= 1000000, size <= 10000000))$size),
      length((comTamanho %>% filter(size > 10000000))$size)  
    )
  )
  
  tabelaTamanhos$Porcentagem <- paste(
    round((tabelaTamanhos$Quantidade / sum(tabelaTamanhos$Quantidade)) * 100), 
    "%", sep=""
  )
  
  return(tabelaTamanhos)
}

