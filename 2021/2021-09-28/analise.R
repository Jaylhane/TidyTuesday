library(tidytuesdayR)

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2021-09-28')
tuesdata <- tidytuesdayR::tt_load(2021, week = 40)

papers <- tuesdata$papers

# Or read in the data manually

papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

#Cleaning the Data

library(nberwp)
library(tidyverse)

papers %>% 
  write_csv("2021/2021-09-28/papers.csv")

authors %>% 
  write_csv("2021/2021-09-28/authors.csv")

programs %>% 
  write_csv("2021/2021-09-28/programs.csv")

paper_authors %>% 
  write_csv('2021/2021-09-28/paper_authors.csv')

paper_programs %>% 
  write_csv("2021/2021-09-28/paper_programs.csv")

joined_df <- left_join(papers, paper_authors) %>% 
  left_join(authors) %>% 
  left_join(paper_programs) %>% 
  left_join(programs)%>% 
  mutate(
    catalogue_group = str_sub(paper, 1, 1),
    catalogue_group = case_when(
      catalogue_group == "h" ~ "Historical",
      catalogue_group == "t" ~ "Technical",
      catalogue_group == "w" ~ "General"
    ),
    .after = paper
  ) 

joined_df%>% 
  write_csv("2021/2021-09-28/joined_df.csv")

joined_df <- read.csv("2021/2021-09-28/joined_df.csv")

# Analisando

summary(joined_df)

summary(as.factor(joined_df$paper))
summary(as.factor(joined_df$catalogue_group))
summary(as.factor(joined_df$year))
summary(as.factor(joined_df$month))
summary(as.factor(joined_df$title))
summary(as.factor(joined_df$author))
summary(as.factor(joined_df$name))
summary(as.factor(joined_df$user_nber))
summary(as.factor(joined_df$user_repec))
summary(as.factor(joined_df$program))
summary(as.factor(joined_df$program_desc))
summary(as.factor(joined_df$program_category))

library(tidyverse)
library(hrbrthemes)
library(ggExtra)
library(gganimate)
library(lubridate)
library(extrafont)
library(Rcpp)
library(gifski)

#ftable(joined_df$catalogue_group,joined_df$year,joined_df$program_category )

dados <- as.data.frame(ftable(joined_df$program_desc, joined_df$year, joined_df$program_category))
names(dados) <- c("program_desc","year","program_category","freq")
dados$year <- as.character(dados$year)
dados$year <- as.Date(dados$year, format = "%Y")
dados$year <- year(dados$year)
dados$year <- as.integer(dados$year)

#dados %>% 
#  ggplot(aes(x=year, y=program_desc, size=freq, color=program_category))+
#  geom_point(alpha=.7)+
#  scale_size(range = c(.1, 10), name="Frequência")

#(dados %>% 
#  filter(year=="1980") %>% 
#  arrange(desc(freq)))[1:15,] %>% 

loadfonts(device = "win")
font_import(paths = 'Your Path/Fonts/Installeds R')

evol_nber <- dados %>%
  ggplot(aes(x=freq, y=reorder(program_desc, freq), fill=program_category))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("firebrick","olivedrab4","royalblue4"))+
  transition_time(year)+
  ease_aes('linear')+
  theme_ipsum(base_family="Courier New")+
  theme(legend.position = c(.30, 1.05),
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.title = element_blank(),
        plot.background = element_rect(fill = "honeydew1", color = NA),
        plot.margin = margin(20, 30, 20, 30),
        plot.title = element_text(
          margin = margin(0,0,20,0),
          size=26,
          hjust = 0,
          family="JMH Typewriter"),
        plot.subtitle = element_text(
          margin = margin(-10,0,40,0)
        ),
        plot.caption = element_text(
          size=12,
          color = "honeydew4",
          face = "bold.italic"
        )
  )+
  labs(title = "Evolution from NBER papers by description and category",
       subtitle = "Year:{frame_time}",
       x="",
       y="",
       fill="Program Category", 
       caption = "Data: National Bureau of Economic Research (NBER) | Graphic: @jaylhane | GitHub: Jaylhane/TidyTuesday")+
  #shadow_mark(alpha = 0.2, size = 0.1)+
  removeGrid()

dados %>%
  filter(year=="2021") %>% 
  ggplot(aes(x=freq, y=reorder(program_desc, freq), fill=program_category))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("firebrick","olivedrab4","royalblue4"))+
  theme_ipsum(base_family="Courier New")+
  theme(legend.position = c(.30, 1.05),
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.title = element_blank(),
        plot.background = element_rect(fill = "honeydew1", color = NA),
        plot.margin = margin(20, 30, 20, 30),
        plot.title = element_text(
          margin = margin(0,0,20,0),
          size=26,
          hjust = 0,
          family="JMH Typewriter"),
        plot.subtitle = element_text(
          margin = margin(-10,0,40,0)
        ),
        plot.caption = element_text(
          size=12,
          color = "honeydew4",
          face = "bold.italic"
        )
  )+
  labs(title = "Evolution from NBER papers by description and category",
       subtitle = "Year:2021",
       x="",
       y="",
       fill="Program Category", 
       caption = "Data: National Bureau of Economic Research (NBER) | Graphic: @jaylhane | GitHub: Jaylhane/TidyTuesday")+
  #shadow_mark(alpha = 0.2, size = 0.1)+
  removeGrid()


#anim_save("evolution-NBER-with-gganimate.gif", height = 900, width = 1600, dpi=320)

anim <- animate(evol_nber, nframes = 300, fps = 8,renderer = gifski_renderer(), height = 900, width = 1600)
anim_save(path = "Your Path/R/TidyTuesday/2021/2021-09-28", filename = "evolution-NBER-with-gganimate.gif", animation = anim)
