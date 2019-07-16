scoring_timeline <- function(df,set_piece = "Possession",...){

nme = names(df)
index = grep(set_piece,nme)

df <- df[index]%>%bind_rows(.)

cols_names = c("start","end","code")

df <- df%>%
  select(-one_of(cols_names))%>%
  map_df(~sapply(strsplit(as.character(.), "- "), tail, 1))%>%
  bind_cols(df[,cols_names],.)%>%
  mutate(bip.time = as.numeric(end) - as.numeric(start),
         start = as.numeric(start),
         end = as.numeric(end))%>%
  arrange(end)

df <- df%>%
  mutate(pos = rep(c(0.5,1,3), length.out=nrow(.)),
         dir = case_when(code %like% paste0(unique(.$code)[1]) ~ 1,
                         !code %like% paste0(unique(.$code)[1]) ~ -1),
         pos = pos*dir,
         yend = 0.035 *dir,
         end = round(end/60,2),
         Score = as.numeric(Score)*dir,
         cum_score = cumsum(Score))%>%
        mutate_if(.,is.factor, as.character)

theme_timeline <-  theme_classic()+
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),
        legend.position= "bottom",
        plot.title = element_text( size = 14))

timeline_plot <-df%>%
    ggplot(.,aes(x=end,y=cum_score))+
    geom_path(aes(x=end,y=cum_score),color = "Grey",alpha = 0.5)+
    #geom_text(aes(x=end,y=cum_score,label = cum_score))+
    geom_hline(yintercept=0,color = "black", size=0.3)+
    annotate("text", x = max(df$end)-10, y = max(df$cum_score)+5, label = paste0(unique(df$code)[1]))+
    annotate("text", x = max(df$end)-10, y = min(df$cum_score)-5 , label = paste0(unique(df$code)[2]))+
    scale_colour_manual(name = element_blank(),
                        ,values = alpha(c("green4", "red4"), .8),
                        labels = c("Team A","Team B"))+
    labs(title = "Scoring Timeline")

  timeline_plot <- timeline_plot+theme_timeline

  invisible(timeline_plot)

}
