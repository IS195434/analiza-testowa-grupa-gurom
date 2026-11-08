dane %>%
  filter(Director=="Woody Allen", Series.or.Movie=="Movie")%>%
  mutate(Release.Date2 = Release.Date %>% as.Date(format = "%d/%m/%y"))%>%
  slice_min(Release.Date2, n=1, with_ties = TRUE)%>%
  select(Title, Release.Date)
  