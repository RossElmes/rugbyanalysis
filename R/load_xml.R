load_xml<- function(direc = rstudioapi::selectDirectory()){

  direc <-  rstudioapi::selectDirectory()
  files <-list.files(direc,pattern = ".xml")
  data <- future_map_dfr(paste0(direc,"/",files), ~ sportscoder::read_sportscode_xml(.))

  code_data<- data%>%
    group_split(code)%>%
    set_names(., nm = map(.x = ., ~glue("{first(.x$code)}")))%>%
    map(.,function(x)remove_empty(x,"cols"))



  results <- list(raw_data =data,grouped_data =code_data )
  invisible(results)

}

