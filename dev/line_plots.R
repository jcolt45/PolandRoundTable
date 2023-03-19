library(tidyverse)
library(lubridate)



#### Normalize by month

betweenness_all_members <- betweenness_all_members %>%
  group_by(Start.Date) %>%
  mutate(
    mean_betweenness = mean(Betweenness),
    sd_betweenness = sd(Betweenness),
    normalized_betweenness = (Betweenness - mean_betweenness)/sd_betweenness
  )


thing2 <- betweenness_all_members %>%
  filter(Member.ID %in% c("MEM0209", "MEM0114"))

thing2 %>%
  left_join(member_meta_info,
            by = "Member.ID") %>%
  filter(Start.Date > ymd("1970-01-01")) %>%
  group_by(Member.ID) %>%
  mutate(
    rolling_avg = slider::slide_dbl(normalized_betweenness, ~mean(.x), .before = 3, .after = 3)
  )%>%
  ggplot(aes(y = rolling_avg,
             x = Start.Date,
             color = Full.Name)) +
  #geom_point() +
  geom_line()



#### Rolling avg plots by group

plenary <- c("MEM0019", "MEM0023")

all_mem <- betweenness_all_members %>%
  left_join(member_meta_info) %>%
  mutate(
    plenary = case_when(
      Member.ID %in% plenary ~ "plenary",
      TRUE ~ "not plenary"
    ),
    profession_simple = case_when(
      str_detect(Profession, "Engineer") ~ "Worker",
      str_detect(Profession, "Academic") ~ "Academic",
      str_detect(Profession, "Worker") ~ "Worker",
      str_detect(Profession, "Miner") ~ "Worker",
      TRUE ~ Profession
    )
  )


bet_by_prof <- all_mem %>%
  group_by(profession_simple, Start.Date) %>%
  summarize(
    avg_betweenness = mean(Betweenness),
    num_people = n()
  ) %>%
  group_by(profession_simple) %>%
  mutate(
    rolling_avg = slider::slide_dbl(avg_betweenness, ~mean(.x), .before = 6, .after = 6)
  )

bet_by_prof %>%
  filter(Start.Date > ymd("1970-01-01"),
         num_people > 5) %>%
  ggplot(aes(x = Start.Date,
             y = rolling_avg,
             color = profession_simple)) +
  geom_line()

all_mem %>%
  distinct(Member.ID, .keep_all = TRUE) %>%
  ggplot(aes(x = profession_simple)) +
  geom_bar()


#### Top 10 most

## overall
betweenness_all_members %>%
  group_by(Member.ID) %>%
  summarize(
    avg_overall_bet = mean(Betweenness)
  ) %>%
  slice_max(order_by = avg_overall_bet,
            n = 10)


## each year
betweenness_all_members %>%
  group_by(year(Start.Date), Member.ID) %>%
  summarize(
    avg_overall_bet = mean(Betweenness)
  ) %>%
  slice_max(order_by = avg_overall_bet,
            n = 5) %>%
  left_join(member_meta_info) %>%
  write_csv("./Dev/top_5_by_year.csv")


#### Testing function version


all_betweenness_opp %>%
  filter(Member.ID %in% c("MEM0019", "MEM0023")) %>%
  left_join(member_meta_info, by = "Member.ID") %>%
  plot_betweenness_mem(group_col = Full.Name)

