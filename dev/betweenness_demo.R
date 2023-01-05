#remotes::install_github("kbodwin/polarexpress")


library(tidyverse)
library(polarexpress)

##### betweenness demo with PZPR and NSZZS

## Calculating betweenness for limited dates
### This takes about 30 sec to run

pzpr <- get_betweenness_orgs("ORG01113",
                             start = "1960-01-01",
                             end = "1985-01-01")

pzpr <- get_degree_orgs("ORG01113",
                        start = "1960-01-01",
                        end = "1985-01-01")

pzpr_all <- get_vertex_metrics_orgs("ORG01113",
                                    list(degree = igraph::degree,
                                         betweenness = igraph::betweenness),
                        start = "1980-01-01",
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
### This takes about 30 sec to run

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
