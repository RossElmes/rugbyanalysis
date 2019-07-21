overview_set_piece <-function(df,...){

  df = df$raw_data

  overview = list()

  for(i in seq(1,max(as.numeric(df$GameNumber)))){

    set_piece = list()

    ## Score Table ##
    try({

      set_piece[["Score"]] <-
        df%>%
        filter(code %like% "Possession",
               GameNumber == i)%>%
        remove_empty("cols")%>%
        group_by(Opp02)%>%
        summarise(Value = sum(as.numeric(Score)))%>%
        mutate(Group = "Score")

    },silent = TRUE)
    ##Trys##
    try({
      set_piece[["Trys"]] <-
        df%>%
        filter(code %like% "Possession",
               P02 %like% "Try",
               GameNumber == i)%>%
        remove_empty("cols")%>%
        group_by(Opp02)%>%
        summarise(Value = n())%>%
        mutate(Group = "Try")


    },silent = TRUE)
    ##Conversions##
    try({

      set_piece[["Conversions"]] = df%>%
        filter(code %like% "Possession",
               P02 %like% "Con Scored",
               GameNumber == i)%>%
        remove_empty("cols")%>%
        group_by(Opp02)%>%
        summarise(Value = n())%>%
        mutate(Group = "Conversions")
    },silent = TRUE)
    ##Penalty Drop Goal##
    try({

      set_piece[["PDG"]] = df%>%
        filter(code %like% "Possession",
               P02 %like% "Penalty Scored",
               GameNumber == i)%>%
        remove_empty("cols")%>%
        group_by(Opp02)%>%
        summarise(Value = n())%>%
        mutate(Group = "Penalty Drop Kicks")

    },silent = TRUE)
    ## Lineout ##
    try({

      set_piece[["Lineout"]] = df%>%
        filter(code %like% "Lineout",
               !code %like% "Attack",
               GameNumber == i)%>%
        remove_empty("cols")%>%
        group_by(Opp02,L05)%>%
        summarise(Count = n())%>%
        mutate(Proportion = Count/sum(Count),
               Value = scales::percent(Proportion),
               Group = "Lineout")%>%
        filter(L05 == "Won")

    },silent = TRUE)
    ##Scrum##
    try({

      set_piece[["Scrum"]] = df%>%
        filter(code %like% "Scrum",
               !code %like% "Attack",
               GameNumber == i)%>%
        remove_empty("cols")%>%
        group_by(Opp02,S03)%>%
        summarise(Count = n())%>%
        mutate(Proportion = Count/sum(Count),
               Value = scales::percent(Proportion),
               Group = "Scrum")%>%
        filter(S03 == "Won")
    },silent = TRUE)
    ##Turnover##
    try({

      set_piece[["Turnover"]] =df%>%
        filter(code %like% "Turnover",
               GameNumber == i)%>%
        remove_empty("cols")%>%
        group_by(Opp02)%>%
        summarise(Value = n())%>%
        mutate(Group = "Turnovers")
    },silent = TRUE)
    ##Penalties##
    try({

      set_piece[["Penalties"]] = df%>%
        filter(code %like% "Penalties",
               GameNumber == i)%>%
        remove_empty("cols")%>%
        group_by(Opp02)%>%
        summarise(Value = n())%>%
        mutate(Group = "Penalties")


    },silent = TRUE)

    overview[[i]] <-
      set_piece%>%
      map(~ungroup(.))%>%
      map_df(~mutate_all(.,as.character))%>%
      select(Opp02,Value,Group)%>%
      spread(.,key = Opp02,value = Value)%>%
      select(2," " = 1,3)

  }
  invisible(overview)

}



