dane %>%
  filter(Genre == "Comedy", Languages == "Polish")%>%
  mutate(Score_mean = (IMDb.Score + Hidden.Gem.Score)/2)%>%
  arrange(desc(Score_mean))%>%
  select(Title, Score_mean, IMDb.Score, Hidden.Gem.Score)%>%
  head(3)
  