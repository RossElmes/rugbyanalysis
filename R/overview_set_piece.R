overview_set_piece <-function(df,...){

df = df$raw_data

overview = list()

for(i in seq(1,max(as.numeric(df$GameNumber)))){

set_piece = list()

  try({

    ## Score Table ##
    set_piece[["Score"]] <-
      df%>%
      filter(code %like% "Possession",
             GameNumber == i)%>%
      remove_empty("cols")%>%
      group_by(Opp02)%>%
      summarise(Points = sum(as.numeric(Score)))%>%
      mutate(Group = "Score")%>%
      spread(.,key = Opp02,value = Points)%>%
      select(3,1,2)%>%
      mutate_all(.,as.character)

  },silent = TRUE)

  try({
##Trys##
set_piece[["Trys"]] <-
  df%>%
  filter(code %like% "Possession",
           P02 %like% "Try",
           GameNumber == i)%>%
    remove_empty("cols")%>%
    group_by(Opp02)%>%
    summarise(Count = n())%>%
    mutate(Group = "Trys")%>%
    spread(.,key = Opp02,value = Count)%>%
    select(3,1,2)%>%
    mutate_all(.,as.character)
},silent = TRUE)

  try({
##Conversions##
set_piece[["Conversions"]] = df%>%
                            filter(code %like% "Possession",
                                   P02 %like% "Con Scored",
                                   GameNumber == i)%>%
                            remove_empty("cols")%>%
                            group_by(Opp02)%>%
                            summarise(Count = n())%>%
                            mutate(Group = "Conversions")%>%
                            spread(.,key = Opp02,value = Count)%>%
                            select(3,1,2)%>%
                            mutate_all(.,as.character)
},silent = TRUE)

  try({
##Penalty Drop Goal##
set_piece[["PDG"]] = df%>%
                    filter(code %like% "Possession",
                           P02 %like% "Penalty Scored",
                           GameNumber == i)%>%
                    remove_empty("cols")%>%
                    group_by(Opp02)%>%
                    summarise(Count = n())%>%
                    mutate(Group = "Penalty Drop Kicks")%>%
                    spread(.,key = Opp02,value = Count)%>%
                    select(3,1,2)%>%
                    mutate_all(.,as.character)
},silent = TRUE)

  try({
## Lineout ##
set_piece[["Lineout"]] = df%>%
                        filter(code %like% "Lineout",
                               !code %like% "Attack",
                               GameNumber == i)%>%
                        remove_empty("cols")%>%
                        group_by(Opp02,L05)%>%
                        summarise(Count = n())%>%
                        mutate(Proportion = Count/sum(Count),
                               Proportion = scales::percent(Proportion),
                               Group = "Lineout")%>%
                        filter(L05 == "Won")%>%
                        select(-c(L05,Count))%>%
                        spread(.,key = Opp02,value = Proportion)%>%
                        select(3,1,2)%>%
                        mutate_all(.,as.character)

},silent = TRUE)

  try({
##Scrum##
set_piece[["Scrum"]] = df%>%
                      filter(code %like% "Scrum",
                             !code %like% "Attack",
                             GameNumber == i)%>%
                      remove_empty("cols")%>%
                      group_by(Opp02,S03)%>%
                      summarise(Count = n())%>%
                      mutate(Proportion = Count/sum(Count),
                             Proportion = scales::percent(Proportion),
                             Group = "Scrum")%>%
                    filter(S03 == "Won")%>%
                    select(-c(S03,Count))%>%
                    spread(.,key = Opp02,value = Proportion)%>%
                    select(3,1,2)%>%
                    mutate_all(.,as.character)
},silent = TRUE)

  try({
##Turnover##
set_piece[["Turnover"]] =df%>%
                        filter(code %like% "Turnover",
                               GameNumber == i)%>%
                        remove_empty("cols")%>%
                        group_by(Opp02)%>%
                        summarise(Count = n())%>%
                        mutate(Group = "Turnovers")%>%
                        spread(.,key = Opp02,value = Count)%>%
                        select(3,1,2)%>%
                        mutate_all(.,as.character)
},silent = TRUE)

  try({
##Penalties##
set_piece[["Penalties"]] = df%>%
                          filter(code %like% "Penalties",
                                 GameNumber == i)%>%
                          remove_empty("cols")%>%
                          group_by(Opp02)%>%
                          summarise(Count = n())%>%
                          mutate(Group = "Penalties")%>%
                          spread(.,key = Opp02,value = Count)%>%
                          select(3,1,2)%>%
                          mutate_all(.,as.character)

},silent = TRUE)


overview[[i]] <-
  set_piece%>%bind_rows(.)%>%
  select(1," " = Group,3)

  }
invisible(overview)

}



