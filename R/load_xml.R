load_xml<- function(direc = rstudioapi::selectDirectory()){

  direc <-  rstudioapi::selectDirectory()
  files <-list.files(direc,pattern = ".xml")
  data <- future_map_dfr(paste0(direc,"/",files), ~ sportscoder::read_sportscode_xml(.))

  cols_names = c("start", "end", "code")

  data<- data%>%
    select(-one_of(cols_names))%>%
    map_df(~sapply(strsplit(as.character(.), "- "), tail, 1)) %>%
    bind_cols(data[, cols_names], .) %>%
    mutate(bip.time = as.numeric(end) - as.numeric(start),
           start = as.numeric(start), end = as.numeric(end))

  code_data <-data%>%
    group_split(code)%>%
    set_names(., nm = map(.x = ., ~glue("{first(.x$code)}")))%>%
    map(.,function(x)remove_empty(x,"cols"))

  results <- list(raw_data =data,grouped_data =code_data)

  invisible(results)

}

