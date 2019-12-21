library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)


file <- haven::read_sav("CC.sav") %>%
  sjlabelled::remove_all_labels()
# Question 1 --------------------------------------------------------------

choices_c1 <- tibble(number = as.numeric(c(1:4)),
                  text = as_factor(c("Yes, regulary", "Yes, in specific cases", "No",
                                     "Do not know")))

Q1 <- file %>% 
  select(C1a:C1j) %>% 
  gather(C1a: C1j, key = "question", value = "answer") %>% 
  left_join(choices_c1, by = c("answer" = "number")) %>% 
  select(1, 3) %>% 
  filter(!is.na(text)) %>% 
  group_by(question, text) %>% 
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count))

ggplot(Q1,aes(x=question, y=pct, fill=text)) +
  geom_bar(stat='identity',  width = .7, colour="black", lwd=0.1) +
  geom_text(aes(label=ifelse(pct >= 0.1, paste0(sprintf("%.0f", pct*100),"%"),"")),
                position=position_stack(vjust=0.5), colour="white") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Engagment of companies in each activity", x = " Questions", y = " Percentage")

# Question 2 --------------------------------------------------------------

choices_a1 <- tibble(number = as.numeric(c(1:6)),
                     size = as_factor(c("1 - 9", "10 - 49", "50 - 249", "250 - 999",
                                        "1000 - 10,000", ">10,000")))

Q2 <- file %>% 
  select(A1, C1a:C1j) %>% 
  left_join(choices_a1, by = c("A1" = "number")) %>% 
  gather(C1a: C1j, key = "question", value = "answer") %>% 
  left_join(choices_c1, by = c("answer" = "number")) %>% 
  select(2, 3, 4, 6) %>%
  filter(!is.na(text) & !is.na(size)) %>% 
  group_by(question, text, size) %>% 
  summarise(count = n())

ggplot(Q2) +
  geom_col(aes(question, count, fill = text), position = "fill") +
  facet_grid(rows = vars(size)) +
  scale_y_continuous(labels = scales::percent_format()) + 
  coord_flip() +
  theme(axis.text.y = element_text(size = 5))

# Question 2 suggestion ---------------------------------------------------

Q2_s <- Q2 %>% 
  filter(text == "Yes, regulary") %>% 
  mutate(pct = count/sum(count))

ggplot(Q2_s,aes(x=question, y=count, fill=size)) +
  geom_col(width = .7, colour="black", lwd=0.1) +
  geom_text(aes(label=ifelse(pct >= 0.2, paste0(sprintf("%.0f", pct*100),"%"),"")),
            position=position_stack(vjust=0.5), colour="white", size = 3) +
  coord_flip() +
  labs(title = "Regular engagment in each activity by company size", x = " Questions",
       y = " Number of companies")

# Question 3 --------------------------------------------------------------

industry = as_factor(c("Construction", "Mining / Metal", "Chemical / Pharmaceutical",
                       "Service industry", "Electrical engineering", "Energy / Water",
                       "Food industry", "Education", "Finance /  Insurance",
                       "Hotel and catering industry / Retail",
                       "Healthcare / Social welfare", "Real estate / Housing", "Trade",
                       "Information / Communication", "Automotive industry / Supplier",
                       "Arts / Entertainment / Leisure", "Agriculture and Forestry",
                       "Mechanical Engineering", "Traffic / Logistics / Transportation",
                       "Other manufacturing industry", "Other"))

choices_a5 <- tibble(number = as.numeric(c(1:21)), industry)

Q3 <- file %>% 
  select(A5, C1a:C1j) %>% 
  left_join(choices_a5, by = c("A5" = "number")) %>%
  gather(C1a: C1j, key = "question", value = "answer") %>% 
  left_join(choices_c1, by = c("answer" = "number")) %>% 
  select(2, 3, 5) %>% 
  filter(!is.na(text) & !is.na(industry)) %>% 
  filter(text == "Yes, regulary")
  
ggplot(Q3) +
  geom_bar(aes(question, fill = industry), color = "black", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(axis.text.y = element_text(size = 5))

# Question 3 suggestion ---------------------------------------------------

Q3_s <- Q3  %>% 
  group_by(question, text, industry) %>% 
  summarise(count = n()) %>%  
  filter(text == "Yes, regulary") %>% 
  mutate(pct = count/sum(count)) %>% 
  arrange(question, count) %>% 
  top_n(5) %>% 
  mutate(rank = rank(-count, ties.method = "random"))

ggplot(Q3_s,aes(x = question, y = count, fill = industry, postion = "dodge")) +
  geom_col(width = .7, colour="black", lwd=0.1) +
  geom_text(aes(label=ifelse(rank >= 0.1, paste0(sprintf("%.0f", rank)),"")),
            position=position_stack(vjust=0.5), colour="white", size = 3) +
  coord_flip() +
  labs(title = "Top engaging industries per activity", x = " Questions",
       y = " Number of companies")
# Question 4 --------------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")

germany <- world %>% 
  filter(sov_a3 == "DEU")

ggplot(world) +
  geom_sf(color = "black", fill = "lightgreen") +
  geom_sf(data = germany, color = "black", fill = "lightblue") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Germany") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  coord_sf(xlim = c(6, 15), ylim = c(47, 55))
  
