library(dplyr)
library(ggplot2)
library(plotly)
library(viridis)

# read data====
atbats <- read.csv(file =  "C:/Users/info/OneDrive/Desktop/work/Laboratorio/atbats.csv"
)
df_p <- read.csv(file =  "C:/Users/info/OneDrive/Desktop/work/Laboratorio/pitches.csv"
)
team <- read.csv(file =  "C:/Users/info/OneDrive/Desktop/work/Laboratorio/games.csv"
)
ejections <- read.csv(file =  "C:/Users/info/OneDrive/Desktop/work/Laboratorio/ejections.csv")



team <- team %>%
  select(g_id,home_team, away_team, date)
## Strucure of frames====
glimpse(atbats)
glimpse(team)


# df_p <- df
options(scipen = 999)


## complete data =====

complete_frame <- df_p %>%
  full_join(atbats, by ='ab_id')

# apply(is.na(complete_frame), 2, sum)


complete_frame <- complete_frame %>%
  filter(!is.na(px), !is.na(pz), !is.na(start_speed), !is.na(end_speed),
         !is.na(spin_rate), !is.na(spin_dir), !is.na(break_length), !is.na(break_y),
         !is.na(pitch_type), !is.na(outs), !is.na(vx0),
         !is.na(code))


complete_frame <- complete_frame %>%
  mutate(Code = case_when(code == "*B" ~ "Ball in dirt",
                          code == "B" ~ "Ball",
                          code == "S" ~ "Swinging Strike",
                          code == "C" ~ "Called Strike",
                          code == "F" ~ "Foul",
                          code == "T" ~ "Foul Tip",
                          code == "L" ~ "Foul Bunt",
                          code == "I" ~ "Intentional Ball",
                          code == "W" ~ "Swinging Strike (Blocked)",
                          code == "P" ~ "Pitchout",
                          code == "Q" ~ "Swinging pitchout",
                          code == "R" ~ "Foul pitchout",
                          code == "X" ~ "In play, out(s)",
                          code == "D" ~ "In play, no out",
                          code == "E" ~ "In play, runs",
                          code == "H" ~ "Hit by pitch",
                          TRUE ~ "Others"
                          ),
         PitchType = case_when(pitch_type == "CH" ~ "Changeup",
                              pitch_type == "CU" ~ "Curveball",
                              pitch_type == "EP" ~ "Eephus",
                              pitch_type == "FC" ~ "Cutter",
                              pitch_type == "FF" ~ "Four-seam Fastball",
                              pitch_type == "FS" ~ "Splitter",
                              pitch_type == "FT" ~ "Two-seam Fastball",
                              pitch_type == "IN" ~ "Intentional ball",
                              pitch_type == "KC" ~ "Knuckle curve",
                              pitch_type == "KN" ~ "Knuckeball",
                              pitch_type == "SL" ~ "Slider",
                              pitch_type == "PO" ~ "Pitchout",
                              pitch_type == "SC" ~ "Screwball",
                              pitch_type == "SI" ~ "Sinker",
                              pitch_type == "FO" ~ "Pitchout",
                              TRUE ~ "Unknown"))
# ylim(c(1.5,3.4))
# xlim(c(-1, 1))


#Zone of strike=====

# by code
graph_code <- complete_frame %>%
  filter(g_id == 201500001) %>%
  ggplot(aes(px, pz, color = as.factor(Code), size = 1))+
  geom_point()+
  theme(panel.background = element_rect(fill = "white"))+
  guides(color = guide_legend(title = "Legend"),
         size = guide_legend(title = " "))+
  annotate("rect", xmin=-1, xmax=1, ymin=1.5, ymax=3.4, fill="red", alpha=0.1, color = "black")

ggplotly(graph_code)


# by type Pitch
graph_pitch <- complete_frame %>%
  filter(g_id == 201500001) %>%
  ggplot(aes(px, pz, color = as.factor(PitchType), size = 1))+
  geom_point()+
  theme(panel.background = element_rect(fill = "white"))+
  guides(color = guide_legend(title = "Legend"),
         size = guide_legend(title = " "))+
  annotate("rect", xmin=-1, xmax=1, ymin=1.5, ymax=3.4, fill="red", alpha=0.1, color = "black")

ggplotly(graph_pitch)


# By event
event_p <- complete_frame %>%
  filter(g_id == 201500001) %>%
  group_by(inning,pitcher_id, event) %>%
  summarise(Event = last(event),
            px = last(px),
            pz = last(pz))

graph_event <- event_p %>%
  ggplot(aes(px, pz, size = 1, color =  Event))+
  geom_point()+
  guides(color = guide_legend(title = "Legend"),
         size = guide_legend(title = " "))+
  theme(panel.background = element_rect(fill = "white"))+
  annotate("rect", xmin=-1, xmax=1, ymin=1.5, ymax=3.4, fill="red", alpha=0.2, color = "black")


# graph_event <- complete_frame %>%
#   filter(g_id == 201500001) %>%
#   ggplot(aes(px, pz, size = 1, color =  event))+
#   geom_point()+
#   guides(color = guide_legend(title = "Legend"),
#          size = guide_legend(title = " "))+
#   theme(panel.background = element_rect(fill = "white"))+
#   annotate("rect", xmin=-1, xmax=1, ymin=1.5, ymax=3.4, fill="red", alpha=0.2, color = "black")

ggplotly(graph_event)

# Velocimetro
graph_velostar <- complete_frame %>%
  filter(g_id == 201500001) %>%
  ggplot(aes(px, pz, color = start_speed, size = 1))+
  geom_point()+
  ggtitle("Launchs by Velocity") +
  scale_color_viridis(direction = -1, option = "D", "VeloBall")+
  annotate("rect", xmin=-1, xmax=1, ymin=1.5, ymax=3.4, fill="red", alpha=0.2, color = "black")



ggplotly(graph_velostar)





# Velocity game by type

  bar_pitching <- complete_frame %>%
    filter(g_id == 201500001) %>%
    group_by(PitchType) %>%
    summarise(count = n()) %>%
    ggplot(aes(reorder(PitchType, -count), count, fill = PitchType))+
    geom_col()+
    coord_flip()+
    scale_fill_brewer(palette="Set1")+
    labs(x= "Pitch Type", y = "Count", title = "Type of Game Pitching")+
    theme(panel.background = element_rect(fill = "white"))


  ggplotly(bar_pitching)

# Velocity boxplot by type pitch
  box_velocity <- complete_frame %>%
    filter(g_id == 201500001) %>%
    group_by(PitchType) %>%
    ggplot(aes(PitchType, start_speed, fill = PitchType))+
    geom_boxplot()+
    labs(y = "Start Pitch", x = "Start Speed", title = "Velo by Pitch")+
    theme(panel.background = element_rect(fill = "white"))+
    scale_fill_brewer(palette="Set1")

  ggplotly(box_velocity)


# pitcher con mas lanzamientos

  launches_game <- complete_frame %>%
    filter(g_id == 201500001) %>%
    group_by(inning, pitcher_id, g_id) %>%
    summarise(lanza = max(pitch_num)) %>%
    group_by(pitcher_id) %>%
    summarise(launchs = sum(lanza)) %>%
    arrange(desc(launchs))

  DT::datatable(launches_game,
                extensions = 'FixedColumns',
                options = list(
                  dom = 't',
                  scrollX = TRUE,
                  fixedColumns = TRUE
                ))

# comparation of velocity
  veloplayer1_f <- complete_frame %>%
    dplyr::filter(g_id == 201500001,
           pitcher_id == launches_game$pitcher_id[1])

  veloplayer2_f <- complete_frame %>%
    filter(g_id == 201500001,
           pitcher_id == launches_game$pitcher_id[2])


  glimpse(complete_frame)
  levels(as.factor(complete_frame$event))

  fig <- plot_ly()
  # Add traces
  fig <- fig %>% add_trace(x = 1:length(veloplayer1_f$start_speed), y = veloplayer1_f$start_speed, name = "Player 1", mode = "lines+markers", type = "scatter",
                           line = list(color = 'black'),
                           marker = list(color = "black",
                                         line = list(color = "black", width = 2)))

  ay <- list(
    tickfont = list(color = "red"),
    overlaying = "y",
    side = "right",
    title = "")

  fig <- fig %>% add_trace(x = 1:length(veloplayer2_f$start_speed), y = veloplayer2_f$start_speed, name = "Player 2", yaxis = "y2", mode = "lines+markers", type = "scatter",
                           line = list(color = 'darkred'),
                           marker = list(color = "darkred",
                                         line = list(color = "darkred", width = 2)))

  # Set figure title, x and y-axes titles
  fig <- fig %>% layout(
    title = "Perfomance Pitchers", yaxis2 = ay,
    xaxis = list(title="Pitch"),
    yaxis = list(title="Velocity")
  )%>%
    layout(plot_bgcolor='#f2f5fa',
           xaxis = list(
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff'),
           yaxis = list(
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff')
    )
  fig

# Comparation by event

  barchart_p <-  complete_frame %>%
    filter(g_id == 201500001,
           pitcher_id %in% c(launches_game$pitcher_id[2], launches_game$pitcher_id[1])) %>%
    group_by(pitcher_id) %>%
    count(event)%>%
    mutate(pitcher_id = as.character(pitcher_id)) %>%
    tidyr::gather(key =  "pitcher_id",value = "player", 1) %>%
    select(-pitcher_id)



    fig1<- ggplot(barchart_p, aes(event, n, fill = player))+
      geom_col(position = "fill")+
      coord_flip()+
      scale_fill_manual(values = c("#c92a0a", "#270ac9"))+
      theme_minimal()+
      geom_hline(yintercept = 0.5, color = "white", size = 1, linetype = 2)+
      theme(legend.position = "top", axis.text.x=element_blank())+
      labs(title = "Player 1 VS Player 2",
           caption = "MLB",
           fill = "Players",x = "Event", y = "Count")

    ggplotly(fig1)







    # batters game
    batters_betters_game <- complete_frame %>%
      filter(g_id == 201500001 ) %>%
      group_by(inning,batter_id, event) %>%
      summarise(event = last(event)) %>%
      group_by(batter_id)%>%
      count(event) %>%
      filter(event %in% c("Home Run","Single", "Triple", "Double" ))

    View(batters_betters_game)

# Event by batters
library(GDAdata)

heat_map <- ggplot(complete_frame %>%
             filter(g_id == 201500001), aes(x=px, y=pz, z = end_speed))+
      theme(panel.background = element_rect(fill = "white"))+
      geom_bin2d()+
      scale_fill_gradient(low = "#fabe64", high = "#de5104")

ggplotly(heat_map)




df %>%
  filter(PlayerId == 11421) %>%
  ggplot(mapping = aes(StrikeZoneSide, StrikeZoneHeight))+
  theme(panel.background = element_rect(fill = "white"))+
  geom_hdr(
    aes(fill = after_stat(probs)),
    alpha = 1, xlim = c(-100, 100), ylim = c(-50, 200),
    show.legend = FALSE)

df %>%
  filter(PlayerId == 11421) %>%
  ggplot(mapping = aes(StrikeZoneSide, StrikeZoneHeight))+
  theme(panel.background = element_rect(fill = "white"))+
  geom_density_2d()+
  scale_fill_distiller(palette = "Blues", direction = 1)
  xlim(c(-100, 100))+
  ylim(c(-50, 200))


df %>%
    filter(PlayerId == 11421) %>%
    ggplot(mapping = aes(StrikeZoneSide, StrikeZoneHeight))+
    theme(panel.background = element_rect(fill = "white"))+
    geom_density_2d_filled(contour_var = "ndensity", bins = 5, show.legend = F)+
    theme_classic()+
  ylim(c(-50, 100 ))+
  xlim(c(-10, 10))



library(ggdensity)

if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("jamesotto852/ggdensity")
