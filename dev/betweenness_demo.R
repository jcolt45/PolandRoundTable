#remotes::install_github("kbodwin/polarexpress")

library(tidyverse)
library(polarexpress)

##### betweenness demo with PZPR and NSZZS

## Calculating betweenness for limited dates

pzpr <- get_betweenness_orgs("ORG01113",
                             start = "1960-01-01",
                             end = "1985-01-01")

pzpr %>%
  ggplot(aes(y = Betweenness,
             x = Start.Date)) +
  geom_point() +
  geom_line()



## Using pre-created data frame
thing <- betweenness_all %>%
  filter(Org.ID %in% c("ORG01113", "ORG00907"))


## Plot
thing %>%
  left_join(organization_meta_info,
            by = "Org.ID") %>%
  ggplot(aes(y = Betweenness,
             x = Start.Date,
             color = Org.Group.1)) +
  geom_point() +
  geom_line()


##### betweenness demo with Lech Walesa and Jacek Kuron


## Recalculating betweenness

Lech <- get_betweenness_members("MEM0209",
                                start = "1975-01-01")

Lech %>%
  ggplot(aes(y = Betweenness,
             x = Start.Date)) +
  geom_point() +
  geom_line() +
  scale_x_date(date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle=-90))


## Using pre-created data frame
thing2 <- betweenness_all_members %>%
  filter(Member.ID %in% c("MEM0209", "MEM0114"))


## Plot
thing2 %>%
  left_join(member_meta_info,
            by = "Member.ID") %>%
  ggplot(aes(y = Betweenness,
             x = Start.Date,
             color = Full.Name)) +
  geom_point() +
  geom_line()
