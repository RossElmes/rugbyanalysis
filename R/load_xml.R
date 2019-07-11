load_xml<- function(direc = rstudioapi::selectDirectory()){

  direc <-  rstudioapi::selectDirectory()
  files <-list.files(direc,pattern = ".xml")
  data <- future_map_dfr(paste0(direc,"/",files), ~ sportscoder::read_sportscode_xml(.))
  invisible(data)

}
