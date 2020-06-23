library("ggplot2")
library("dplyr")


################## Primeira estrela
primeira_estrela <- function(arquivos, datasets) {
  
  result <- arquivos %>% inner_join(datasets, by = c("package_id" = "id"))
  result <- result %>% filter(license_id %in% licencasAbertas)
  
  return(arquivos %>% filter(id %in% result$id))
}

calcular_estrelas <- function(datasets, arquivos) {
  unique(datasets[datasets$license_id == 'other-closed',])
  licencasAbertas <- c(
    "cc-by-sa", "odc-odbl", "other-pd", 
    "gfdl", "cc-nc", "cc-by",
    "other-open", "CC0-1.0", "CC-BY-4.0",
    "CC-BY-NC-4.0", "odc-pddl", "cc-zero"
  )
  zeroEstrela 	<- c("API", "APP", "EXE", "JS", "Perl", "OWL", "HTML", "XSLT", "RDFa", "URL", "XLM", "LOG", "CTR")
  umaEstrela 		<- c(
    "PPTX", "DOC", "ArcGIS Online Map", "ZIP", "GZ", "ODT", "RAR", "TXT", "DCR", "DOCX", "WORD","BIN", "PPT", "ODP", 
    "PDF", "ODC", "MXD", "TAR", "OPENDOCUMENT", "RTF",
    "ZIP CSV", "ZIP/CSV", "ZIP/DBF", "CSV/ZIP", "BPMN", "CVS", "APPLICATION/MSWORD", "PDF E HTML", "XLS / ODS", "XLSX / ODS", "CSV E PDF"
  )
  duasEstrela 	<- c("XLS", "MDB", "ArcGIS Map Service", "BMP", "TIFF", "XLSX", "GIF", "E00", "MrSID", "ArcGIS Map Preview", "MOP", "Esri REST", "DBASE", "SHP")
  tresEstrela 	<- c("KML", "WCS", "NetCDF", "TSV", "WFS", "KMZ", "QGIS", "ODS", "JSON", "ODB", "ODF", "ODG", "XML", "WMS", "WMTS", "SVG", "JPEG", "JPG","CSV", "Atom Feed", "XYZ", "PNG", "RSS", "GEOJSON", "TOPOJSON", "IATI", "ICS")
  cincoEstrela 	<- c("N3", "SPARQL", "RDF", "TTL", "KML")
  
  
  result <- arquivos %>% 
    mutate(estrela = "") %>%
    mutate(format = sub("\\.", "", toupper(format))) %>%
    transform(estrela = ifelse(format %in% umaEstrela, '✰', estrela)) %>%
    transform(estrela = ifelse(format %in% duasEstrela, '✰✰', estrela)) %>%
    transform(estrela = ifelse(format %in% tresEstrela, '✰✰✰', estrela)) %>%
    transform(estrela = ifelse(format %in% cincoEstrela, '✰✰✰✰', estrela)) 
  
  return(result)
}
