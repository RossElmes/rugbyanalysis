overview_bip <-function(df){

  ### BIP Code ###
  nme = names(df)
  index = grep("Ball in Play", nme)
  df <- df[index] %>% bind_rows(.)

  ### Colnames to review ###
  cols_names = c("start", "end", "code")

  df <- df %>%
    select(-one_of(cols_names))%>%
    map_df(~sapply(strsplit(as.character(.), "- "), tail, 1)) %>%
    bind_cols(df[, cols_names], .) %>%
    mutate(bip.time = as.numeric(end) - as.numeric(start),
           start = as.numeric(start), end = as.numeric(end)) %>%
    arrange(end)

  ##Ball in Play##
  BIP1 <- df%>%
    summarise(`Total BIP (m)`= sum(bip.time),
              `Avg BIP (s)` = mean(bip.time),
              `Max BIP (s)` = max(bip.time))%>%
    mutate(`Total BIP (m)` =`Total BIP (m)`/60,
           H01 = "Total")

  BIP2 <- df%>%
    group_by(H01)%>%
    summarise(`Total BIP (m)`= sum(bip.time),
              `Avg BIP (s)` = mean(bip.time),
              `Max BIP (s)` = max(bip.time))%>%
    mutate(`Total BIP (m)` =`Total BIP (m)`/60)

  BIP_Table <- union_all(BIP1,BIP2)%>%
    gather(.,key = "Metric",value = "Value", -H01 )%>%
    spread(.,key = H01,value = Value)

  invisible(BIP_Table)
}
