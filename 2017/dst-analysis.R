dst_data <-
  adp_data3 %>%
  left_join(pwd_total_wlag, by = c("year","player_code" = "player")) %>%
  filter(pos == "D/ST")

library(ggplot2)

p <- ggplot(data = dst_data,
            aes(x=position_adp,y=ppg))
p +
  geom_point() +
  geom_smooth()

training_data2017

dst_data %>%
  filter(pos == 'D/ST') %>%
  group_by(year) %>%
  summarise(min_ppg = min(ppg),
            max_ppg = max(ppg),
            mean_ppg = mean(ppg))


# Maybe you want two top 5 defenses?
# Rare chance of going < 7.5 ppg, should be around 8 or 9. 
