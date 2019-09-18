load_xml<- function(){

  direc <-  getwd() ## hopefully this will be project folder
  files <-list.files(paste0(direc,"/XMLs"),pattern = ".xml",full.names = T) ## list all the xmls
  db <- list.files(paste0(direc,"/CSVs"),pattern = ".csv",full.names = T) ## pull back the csv

  if(length(db) == 0){

    df <- future_map_dfr(paste0(direc,"/",files), ~ sportscoder::read_sportscode_xml(.))

  }


  if(length(db)>0){

    dat <- read_csv(db) ## read in the csv
    ## Pull out the
    file_check <- dat%>%
      distinct(file_path)%>%pull()
    ## Pull out the sting
    match = str_extract(file_check,"[:alpha:]*_v_[:alpha:]*")

    ## Pull out the index of the file that hasn't been loaded in
    index = str_which(files,paste0(match,collapse = "|"),negate = T)

  }


  if(length(index) > 0){

    df <- future_map_dfr(paste0(direc,"/",files[index]), ~ sportscoder::read_sportscode_xml(.))

  }else {

    df <- vector()
  }


  if(length(df) >0 ) {

    cols_names = c("start", "end", "code")

    df <- df%>%
      select(-one_of(cols_names))%>%
      map_df(~sapply(strsplit(as.character(.), "- "), tail, 1)) %>%
      bind_cols(data[, cols_names], .) %>%
      mutate(bip.time = as.numeric(end) - as.numeric(start),
             start = as.numeric(start), end = as.numeric(end))

    code_data <-df%>%
      group_split(code)%>%
      set_names(., nm = map(.x = ., ~glue("{first(.x$code)}")))%>%
      map(.,function(x)remove_empty(x,"cols"))

    results <- list(raw_data =df,grouped_data =code_data)

    invisible(results)

  }

}
