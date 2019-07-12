timeline <-function(df,set_piece = "Lineout",group_var){

  group_var = enquo(group_var)
  nme = names(df)
  index = grep(set_piece,nme)

  set_piece_data = df[index]%>%
    bind_rows(.)

  timeline_df<-set_piece_data%>%
    filter(!code %like% "Attack")%>%
    select(id,code,start,end,!!group_var)%>%
    mutate(dir = case_when(code %like% unique(.$code)[1] ~ 1,
                           TRUE ~ -1),
           pos = rep(c(0.5,1,3), length.out=nrow(.)),
           pos = pos*dir,
           yend = 0.035 *dir,
           start = round(start/60,0),
           end = round(end/60,0))%>%
    ungroup()%>%
    mutate_if(.,is.factor, as.character)

  t1 <- timeline_df%>%
    filter(code %like% unique(.$code)[1])%>%
    group_by(code,!! group_var)%>%
    summarise(cnt = n())%>%
    mutate(prop = cnt/sum(cnt),
           prop = percent(prop))%>%
    #filter(!!group_var %like% "Won")%>%
    ungroup()%>%
    mutate_if(.,is.factor, as.character)


  t2 <- timeline_df%>%
    filter(!code %like% unique(.$code)[1])%>%
    group_by(code,!! group_var)%>%
    summarise(cnt = n())%>%
    mutate(prop = cnt/sum(cnt),
           prop = percent(prop))%>%
    #filter(!!group_var %like% "Won")%>%
    ungroup()%>%
    mutate_if(.,is.factor, as.character)

  plot <- timeline_df%>%
    ggplot(.,aes(x=start,y=0))+
    geom_hline(yintercept=0,color = "black", size=0.3)+
    geom_segment(aes(y=pos,yend=yend,xend=start), color='black',size=0.3)+
    geom_point(aes_(x=quo(start),y=quo(pos),colour =group_var),size=5)+
    annotate("text", x = max(timeline_df$end)-10, y =4 , label = paste0(unique(t1$code)))+
    annotate("text", x = max(timeline_df$end)-10, y =-4 , label = paste0(unique(t2$code)))+
    #annotate("text", x = max(timeline_df$end)-10, y =4 , label = paste0(unique(t1$code)," ",unique(t1$prop)))+
    #annotate("text", x = max(timeline_df$end)-10, y =-4 , label = paste0(unique(t2$code)," ",unique(t2$prop)))+
    #annotate("rect", xmin=1, xmax=20, ymin=0.015, ymax=1, alpha=0.2, fill="green")+
    #annotate("rect", xmin=21, xmax=40, ymin=-0.015, ymax=-1, alpha=0.2, fill="red")+
    labs(title = paste0(set_piece," Timeline"))+
    theme(axis.line.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x =element_blank(),
          axis.ticks.x =element_blank(),
          axis.line.x =element_blank(),
          legend.position= "bottom",
          plot.title = element_text( size = 14)
    )

  plot



  return(list(t1,t2,plot))

}
