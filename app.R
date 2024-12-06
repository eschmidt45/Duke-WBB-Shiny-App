#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(bslib)
library(gtools)
library(plotly)
library(htmlwidgets)

# Load Data
data <- readxl::read_xlsx("Duke WBB 22-23 Off Pos Data.xlsx") %>%
  mutate(
    ct_zone = as.character(ct_zone)
  )

# Load in court sf object 
court <- readRDS("court.rds")

# Data Pre-Processing: Change variable names and filter out players with less than 40 attempts 
data_p <- data %>%
  select(-notes, - `...24`) %>%
  filter(fg_id != "ft") %>%
  mutate(player_id = case_when(player_id == 0 ~ "#0 Celeste Taylor",
                               player_id == 2 ~ "#2 Vanessa de Jesus",
                               player_id == 3 ~ "#3 Ashlon Jackson",
                               player_id == 4 ~ "#4 Elizabeth Balogun",
                               player_id == 5 ~ "#5 Taya Corosdale",
                               player_id == 11 ~ "#11 Jordyn Oliver",
                               player_id == 13 ~ "#13 Lee Volker",
                               player_id == 15 ~ "#15 Emma Koable",
                               player_id == 21 ~ "#21 Kennedy Brown",
                               player_id == 22 ~ "#22 Shay Bollin",
                               player_id == 23 ~ "#23 Imani Lewis",
                               player_id == 24 ~ "#24 Reigan Richardson",
                               player_id == 30 ~ "#30 Shy Day-Wilson",
                               player_id == 32 ~ "#32 Bo Shaffer",
                               player_id == 33 ~ "#33 Jiselle Havas",
                               player_id == 42 ~ "#42 Mia Heide",
                               player_id == 45 ~ "#45 Emma Schmidt"),
         qtr_id = case_when(qtr_id == 1 ~ "1st Quarter",
                            qtr_id == 2 ~ "2nd Quarter",
                            qtr_id == 3 ~ "3rd Quarter",
                            qtr_id == 4 ~ "4th Quarter"),
         pos_type = case_when(pos_type == "hc" ~ "Half Court",
                              pos_type == "trans" ~ "Transition",
                              TRUE ~ pos_type),
         shot_id = case_when(shot_id == "dribble" ~ "Off the Dribble",
                             shot_id == "pass" ~ "Catch and Shoot",
                             TRUE ~ shot_id),
         game_id = case_when(game_id == "non-con" ~ "Non-Con",
                             game_id == "acc" ~ "ACC",
                             TRUE ~ game_id)) %>%
  rename(Player = player_id) %>%
  drop_na()

# Data Pre-Processing: Data for calculating team averages (no players filtered out)
data_t <- data %>%
  select(-notes, - `...24`) %>%
  filter(fg_id != "ft") %>%
  mutate(player_id = case_when(player_id == 0 ~ "#0 Celeste Taylor",
                               player_id == 2 ~ "#2 Vanessa de Jesus",
                               player_id == 3 ~ "#3 Ashlon Jackson",
                               player_id == 4 ~ "#4 Elizabeth Balogun",
                               player_id == 5 ~ "#5 Taya Corosdale",
                               player_id == 11 ~ "#11 Jordyn Oliver",
                               player_id == 13 ~ "#13 Lee Volker",
                               player_id == 15 ~ "#15 Emma Koable",
                               player_id == 21 ~ "#21 Kennedy Brown",
                               player_id == 22 ~ "#22 Shay Bollin",
                               player_id == 23 ~ "#23 Imani Lewis",
                               player_id == 24 ~ "#24 Reigan Richardson",
                               player_id == 30 ~ "#30 Shy Day-Wilson",
                               player_id == 32 ~ "#32 Bo Shaffer",
                               player_id == 33 ~ "#33 Jiselle Havas",
                               player_id == 42 ~ "#42 Mia Heide",
                               player_id == 45 ~ "#45 Emma Schmidt",
                               TRUE ~ "test"),
         qtr_id = case_when(qtr_id == 1 ~ "1st Quarter",
                            qtr_id == 2 ~ "2nd Quarter",
                            qtr_id == 3 ~ "3rd Quarter",
                            qtr_id == 4 ~ "4th Quarter"),
         pos_type = case_when(pos_type == "hc" ~ "Half Court",
                              pos_type == "trans" ~ "Transition",
                              TRUE ~ pos_type),
         shot_id = case_when(shot_id == "dribble" ~ "Off the Dribble",
                             shot_id == "pass" ~ "Catch and Shoot",
                             TRUE ~ shot_id),
         game_id = case_when(game_id == "non-con" ~ "Non-Con",
                             game_id == "acc" ~ "ACC",
                             TRUE ~ game_id)) %>%
  rename(Player = player_id) %>%
  drop_na()

# Function for alpha-numeric ordering
mixedrank = function(x) order(mixedorder(x))

# Table structuring for player season FG percentages and attempts
fg <- data_p %>% 
  group_by(Player) %>%
  summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01)))

fga <- data_p %>% 
  group_by(Player) %>%
  summarize(fg = paste(sum(fg_01), "/", n()))

fg3 <- data_p %>%
  filter(ct_zone %in% c(1,8,18,19,20,14,7)) %>%
  group_by(Player) %>%
  summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01)))

fg3a <- data_p %>%
  filter(ct_zone %in% c(1,8,18,19,20,14,7)) %>%
  group_by(Player) %>%
  summarize(fg = paste(sum(fg_01), "/", n()))

shot_type <-  data_p %>%
  filter(shot_id %in% c("Off the Dribble", "Catch and Shoot")) %>%
  group_by(Player, shot_id) %>%
  summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01)))

shot_typea <-  data_p %>%
  filter(shot_id %in% c("Off the Dribble", "Catch and Shoot")) %>%
  group_by(Player, shot_id) %>%
  summarize(fg = paste(sum(fg_01), "/", n()))

qtr <- data_p %>%
  group_by(Player, qtr_id) %>%
  summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01)))

qtra <- data_p %>%
  group_by(Player, qtr_id) %>%
  summarize(fg = paste(sum(fg_01), "/", n()))

pos <- data_p %>%
  filter(pos_type %in% c("Half Court", "Transition")) %>%
  group_by(Player, pos_type) %>%
  summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01)))

posa <- data_p %>%
  filter(pos_type %in% c("Half Court", "Transition")) %>%
  group_by(Player, pos_type) %>%
  summarize(fg = paste(sum(fg_01), "/", n()))

table1 <- fg %>% left_join(fg3, by =c("Player" = "Player")) %>%
  left_join(shot_type, by= c("Player" = "Player")) %>%
  left_join(pos, by= c("Player" = "Player")) %>%
  left_join(qtr, by= c("Player" = "Player")) %>%
  pivot_wider(names_from = shot_id, values_from = fg.x.x) %>%
  pivot_wider(names_from = pos_type, values_from = fg.y.y) %>%
  pivot_wider(names_from = qtr_id, values_from = fg) %>%
  rename('FG' = fg.x, '3PT FG' = fg.y) %>%
  arrange(mixedrank(Player))

table1a <- fga %>% left_join(fg3a, by =c("Player" = "Player")) %>%
  left_join(shot_typea, by= c("Player" = "Player")) %>%
  left_join(posa, by= c("Player" = "Player")) %>%
  left_join(qtra, by= c("Player" = "Player")) %>%
  pivot_wider(names_from = shot_id, values_from = fg.x.x) %>%
  pivot_wider(names_from = pos_type, values_from = fg.y.y) %>%
  pivot_wider(names_from = qtr_id, values_from = fg) %>%
  rename('FG' = fg.x, '3PT FG' = fg.y) %>%
  arrange(mixedrank(Player))

# Table structuring for team FG percentages and attempts
fg_t <- data_t %>%
  summarize('FG' = sprintf("%1.2f%%", 100*mean(fg_01)))

fg_ta <- data_t %>%
  summarize('FG' = paste(sum(fg_01), "/", n()))

fg3_t <- data_t %>%
  filter(ct_zone %in% c(1,8,18,19,20,14,7)) %>%
  summarize('3PT FG' = sprintf("%1.2f%%", 100*mean(fg_01)))

fg3_ta <- data_t %>%
  filter(ct_zone %in% c(1,8,18,19,20,14,7)) %>%
  summarize('3PT FG' = paste(sum(fg_01), "/", n()))

shot_type_t <- data_t %>%
  filter(shot_id %in% c("Off the Dribble", "Catch and Shoot")) %>%
  group_by(shot_id) %>%
  summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01))) %>%
  pivot_wider(names_from = shot_id, values_from = fg)

shot_type_ta <- data_t %>%
  filter(shot_id %in% c("Off the Dribble", "Catch and Shoot")) %>%
  group_by(shot_id) %>%
  summarize(fg = paste(sum(fg_01), "/", n())) %>%
  pivot_wider(names_from = shot_id, values_from = fg)

pos_t <- data_t %>%
  filter(pos_type %in% c("Half Court", "Transition")) %>%
  group_by(pos_type) %>%
  summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01))) %>%
  pivot_wider(names_from = pos_type, values_from = fg)

pos_ta <- data_t %>%
  filter(pos_type %in% c("Half Court", "Transition")) %>%
  group_by(pos_type) %>%
  summarize(fg = paste(sum(fg_01), "/", n())) %>%
  pivot_wider(names_from = pos_type, values_from = fg)

qtr_t <- data_t %>%
  group_by(qtr_id) %>%
  summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01))) %>%
  pivot_wider(names_from = qtr_id, values_from = fg)

qtr_ta <- data_t %>%
  group_by(qtr_id) %>%
  summarize(fg = paste(sum(fg_01), "/", n())) %>%
  pivot_wider(names_from = qtr_id, values_from = fg)

table_team <- cbind(Player = "Team", fg_t, fg3_t, shot_type_t, pos_t, qtr_t) %>%
  replace(is.na(.), "-")

table_teama <- cbind(Player = "Team", fg_ta, fg3_ta, shot_type_ta, pos_ta, qtr_ta) %>%
  replace(is.na(.), "-")

table_home <- rbind(table_team, table1) %>%
  replace(is.na(.), "-")

table_homea <- rbind(table_teama, table1a) %>%
  replace(is.na(.), "-")

# Table structuring for game by game team FG percentages and attempts
fg2 <- data_t %>%
  group_by(opp_code, game_id) %>%
  summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01)))

fg2a <- data_t %>%
  group_by(opp_code, game_id) %>%
  summarize(fg = paste(sum(fg_01), "/", n()))

fg32 <- data_t %>%
  filter(ct_zone %in% c(1,8,18,19,20,14,7)) %>%
  group_by(opp_code, game_id) %>%
  summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01)))

fg32a <- data_t %>%
  filter(ct_zone %in% c(1,8,18,19,20,14,7)) %>%
  group_by(opp_code, game_id) %>%
  summarize(fg = paste(sum(fg_01), "/", n()))

shot_type2 <-  data_t %>%
  filter(shot_id %in% c("Off the Dribble", "Catch and Shoot")) %>%
  group_by(opp_code, game_id, shot_id) %>%
  summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01)))

shot_type2a <-  data_t %>%
  filter(shot_id %in% c("Off the Dribble", "Catch and Shoot")) %>%
  group_by(opp_code, game_id, shot_id) %>%
  summarize(fg = paste(sum(fg_01), "/", n()))

qtr2 <- data_t %>%
  group_by(opp_code, game_id, qtr_id) %>%
  summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01)))

qtr2a <- data_t %>%
  group_by(opp_code, game_id, qtr_id) %>%
  summarize(fg = paste(sum(fg_01), "/", n()))


pos2 <- data_t %>%
  filter(pos_type %in% c("Half Court", "Transition")) %>%
  group_by(opp_code, game_id, pos_type) %>%
  summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01)))

pos2a <- data_t %>%
  filter(pos_type %in% c("Half Court", "Transition")) %>%
  group_by(opp_code, game_id, pos_type) %>%
  summarize(fg = paste(sum(fg_01), "/", n()))

table2 <- fg2 %>% left_join(fg32, by =c("opp_code" = "opp_code", "game_id" = "game_id")) %>%
  left_join(shot_type2, by= c("opp_code" = "opp_code", "game_id" = "game_id")) %>%
  left_join(pos2, by= c("opp_code" = "opp_code", "game_id" = "game_id")) %>%
  left_join(qtr2, by= c("opp_code" = "opp_code", "game_id" = "game_id")) %>%
  pivot_wider(names_from = shot_id, values_from = fg.x.x) %>%
  pivot_wider(names_from = pos_type, values_from = fg.y.y) %>%
  pivot_wider(names_from = qtr_id, values_from = fg) %>%
  arrange(mixedrank(opp_code))  %>%
  rename('FG%' = fg.x, '3P FG%' = fg.y, 'Opponent' = opp_code, 'Game Type' = game_id)  %>%
  replace(is.na(.), "-") 

table2a <- fg2a %>% left_join(fg32a, by =c("opp_code" = "opp_code", "game_id" = "game_id")) %>%
  left_join(shot_type2a, by= c("opp_code" = "opp_code", "game_id" = "game_id")) %>%
  left_join(pos2a, by= c("opp_code" = "opp_code", "game_id" = "game_id")) %>%
  left_join(qtr2a, by= c("opp_code" = "opp_code", "game_id" = "game_id")) %>%
  pivot_wider(names_from = shot_id, values_from = fg.x.x) %>%
  pivot_wider(names_from = pos_type, values_from = fg.y.y) %>%
  pivot_wider(names_from = qtr_id, values_from = fg) %>%
  arrange(mixedrank(opp_code))  %>%
  rename('FG%' = fg.x, '3P FG%' = fg.y, 'Opponent' = opp_code, 'Game Type' = game_id)  %>%
  replace(is.na(.), "-") 

# Table structuring for individual game analysis
table3 <- data_t %>%
  summarize('Opponent' = "Season Averages",
            'Makes' = round(sum(fg_01)/max(game_nbr), 2),
            'Attempts' = round(max(pos_id)/max(game_nbr), 2),
            'Paint Touches' = round(sum(paint_tchs)/max(game_nbr), 2),
            'Paint Touches/Pos' = round(sum(paint_tchs)/max(pos_id), 2),
            'Passes' = round(sum(passes)/max(game_nbr), 2),
            'Passes/Pos' = round(sum(passes)/max(pos_id), 2),
            'Sides' = round(sum(hc_sides)/max(game_nbr), 2),
            'Sides/Pos' = round(sum(hc_sides)/max(pos_id), 2))

table3_2 <- data_t %>%
  group_by(opp_code) %>%
  summarize('Makes' = sum(fg_01),
            'Attempts' = max(game_pos),
            'Paint Touches' = sum(paint_tchs),
            'Paint Touches/Pos' = round(sum(paint_tchs)/max(game_pos), 2),
            'Passes' = sum(passes),
            'Passes/Pos' = round(sum(passes)/max(game_pos), 2),
            'Sides' = sum(hc_sides),
            'Sides/Pos' = round(sum(hc_sides)/max(game_pos), 2)) %>%
  rename('Opponent' = opp_code)


table3_f <- rbind(table3, table3_2)


# Structure court zones
court <- court %>%
      mutate(Zone = c("court", "three", "low box", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
                         12, 13, 14, 15, 17, 18, 19, 20, 16))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = bs_theme(
    bg = "#0b3d91", fg = "white",
    primary = "white",
    base_font = font_google("Almarai"),
    code_font = font_google("Almarai")
  ), 
  
  
  
    # Application title
    titlePanel(h1("2022-23 Duke Women's Basketball Offensive Efficiency", align = "center")),
    
    tabsetPanel(
      # Home tab with heat map, team/player season averages and attempts
      tabPanel("Home", 
               fluidRow(textOutput(outputId = "blank1")),
               fluidRow(
                 column(3, align="center",
                        selectInput(inputId = "select_box", label = "Player Selection",
                                    choices = list("Team", "#0 Celeste Taylor", "#2 Vanessa de Jesus", "#3 Ashlon Jackson",
                                                   "#4 Elizabeth Balogun", "#5 Taya Corosdale", "#11 Jordyn Oliver", 
                                                   "#13 Lee Volker", "#15 Emma Koable", "#21 Kennedy Brown", "#22 Shay Bollin", "#23 Imani Lewis", "#24 Reigan Richardson", 
                                                   "#30 Shy Day-Wilson", "#32 Bo Shaffer", "#33 Jiselle Havas", "#42 Mia Heide", "#45 Emma Schmidt")),
                        imageOutput("roster")
                        
                 ),
                 column(9, align="center",
                        plotlyOutput("courtPlot")
                 )
               ),
               
               fluidRow(
                 column(3, align = "center",
                        selectInput(inputId = "select_box4", label = "Stat Selection",
                                    choices = list("FG Percentages", "FG Attempts"))
                 )
               ),
               
               fluidRow(
                 column(12, align = "center",
                        tableOutput('table')
                 )
               ),
               
               
      ),
      # Customization tab with heat map "build your own possession"
      tabPanel("Possession Breakdown",
               fluidRow(textOutput(outputId = "blank")
                        ),
               
               fluidRow(
                 column(4, align = "center",
                        textOutput(outputId = "Poss"))
                 ),
               
               
               sidebarLayout(
                 
                 sidebarPanel(
                   # Input: Shot Clock
                   sliderInput("shot_clock", "Shot Clock",
                               min = min(data$shot_clock), max = max(data$shot_clock),
                               value = c(min(data$shot_clock), max(data$shot_clock))),
                   
                   # Input: Paint Touches
                   sliderInput("paint_tchs", "Paint Touches",
                               min = min(data$paint_tchs), max = max(data$paint_tchs),
                               value = c(min(data$paint_tchs), max(data$paint_tchs))),
                   
                   # Input: Side Touches
                   sliderInput("side_tchs", "Side Touches",
                               min = min(data$hc_sides), max = max(data$hc_sides),
                               value = c(min(data$hc_sides), max(data$hc_sides))),
                   
                   # Input: Passes
                   sliderInput("passes", "Passes",
                               min = min(data$passes), max = max(data$passes),
                               value = c(min(data$passes), max(data$passes)))
                 ),
                 
                 mainPanel(
                   plotlyOutput("possplot")
                 )
               )
               
               
               ),
               
      # Game by game trend tab with team/player game by game performance with FG percentages and attempts
      tabPanel("Season Trends",
               fluidRow(
                 textOutput(outputId = "blank2")
               ),
               fluidRow(
                 column(3, align="center",
                        selectInput(inputId = "select_box2", label = "Player Selection",
                                    choices = list("Team", "#0 Celeste Taylor", "#2 Vanessa de Jesus", "#3 Ashlon Jackson",
                                                   "#4 Elizabeth Balogun", "#5 Taya Corosdale", "#11 Jordyn Oliver", 
                                                   "#13 Lee Volker", "#15 Emma Koable", "#21 Kennedy Brown", "#22 Shay Bollin", "#23 Imani Lewis", "#24 Reigan Richardson", 
                                                   "#30 Shy Day-Wilson", "#32 Bo Shaffer", "#33 Jiselle Havas", "#42 Mia Heide", "#45 Emma Schmidt")),
                        imageOutput("roster2")
                        
                 ),
                 column(9, align="center",
                        plotlyOutput("trend1")
                 )
                 
                 ),
               fluidRow(
                 column(3, align = "center",
                        selectInput(inputId = "select_box5", label = "Stat Selection",
                                    choices = list("FG Percentages", "FG Attempts"))
                 )
               ),
               fluidRow(
                 column(12, align = "center",
                        tableOutput('table2')
                 )
               ),
      ),
      
      # Single game breakdown tab with summary possession statistics 
      tabPanel("Game Trends",
               fluidRow(textOutput(outputId = "blank3")
               ),
               fluidRow(
                 column(3, align="center",
                        selectInput(inputId = "select_box3", label = "Opponent Selection",
                                    choices = list("1-NC A&T", "2-Charleston So", "3-Davidson", "4-Texas A&M",
                                                   "5-Toledo", "6-UConn", "7-Oregon St", 
                                                   "8-Northwestern", "9-Richmond", "10-Austin Peay", 
                                                   "11-FGCU", "12-Virginia", "13-NC State", "14-Louisville", "15-Wake Forest")),
                        imageOutput("opps")
                        
                 ),
                 column(9, align="center",
                        plotlyOutput("trend2")
                 )
                 
               ),
               fluidRow(
                 column(12, align = "center",
                        tableOutput('table3')
                 )
               ),
               )


)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$blank <- renderText({ 
    paste("---")
  })
  
  output$blank1 <- renderText({ 
    paste("---")
  })
  
  output$blank2 <- renderText({ 
    paste("---")
  })
  
  output$blank3 <- renderText({ 
    paste("---")
  })
  
  output$Poss <- renderText({ 
    paste("Customize Possession")
  })
    
    output$roster <- renderImage({
      
      list(src = paste0("www/", input$select_box, ".jpg"),
      height = 250
      )
    }, deleteFile = F)
    
    
    output$roster2 <- renderImage({
      
      list(src = paste0("www/", input$select_box2, ".jpg"),
           height = 250
      )
    }, deleteFile = F)
    
    output$opps <- renderImage({
      
      list(src = paste0("www/", input$select_box3, ".png"),
           height = 200
      )
    }, deleteFile = F)
  
    output$courtPlot <- renderPlotly({
      # Subset Data by Selected Player
      if(input$select_box == "Team") {
        
        total = nrow(data_t)
        
        zone_pct <- data_t %>%
          group_by(ct_zone) %>%
          summarize(zone_avg = mean(fg_01),
                    zone_make = sum(fg_01),
                    zone_att = n(),
                    volume = zone_att/total)
        
        process <- data_t %>%
          mutate(zone_make = sum(fg_01)) %>%
          count(ct_zone, .drop = FALSE)
        
        final <- process %>%
          left_join(zone_pct, by = c("ct_zone" = "ct_zone")) %>%
          mutate(zone_avg = coalesce(zone_avg, 0),
                 zone_make = coalesce(zone_make, 0),
                 zone_att = coalesce(zone_att, 0),
                 volume = coalesce(volume, 0))
        
      } else {
        
        total = nrow(data_p %>%
                       filter(Player == input$select_box))
        
        zone_pct <- data_p %>%
          filter(Player == input$select_box) %>%
          group_by(ct_zone) %>%
          summarize(zone_avg = mean(fg_01),
                    zone_make = sum(fg_01),
                    zone_att = n(),
                    volume = zone_att/total)
        
        process <- data_p %>%
          filter(Player == input$select_box) %>%
          mutate(zone_make = sum(fg_01)) %>%
          count(ct_zone, .drop = FALSE)
        
        final <- process %>%
          left_join(zone_pct, by = c("ct_zone" = "ct_zone")) %>%
          mutate(zone_avg = coalesce(zone_avg, 0),
                 zone_make = coalesce(zone_make, 0),
                 zone_att = coalesce(zone_att, 0),
                 volume = coalesce(volume, 0))
        
      }
      
      shot_chart <- court %>% 
        left_join(final, by = c("Zone" = "ct_zone")) %>%
        filter(Zone != "court", Zone != "three", Zone != "low box") %>%
        mutate(FG = case_when(n != 0 & zone_avg <= .10 ~ "0-10%",
                              n != 0 & zone_avg > .10 & zone_avg <= .20 ~ "10-20%",
                              n != 0 & zone_avg > .20 & zone_avg <= .30 ~ "20-30%",
                              n != 0 & zone_avg > .30  & zone_avg <= .40 ~ "30-40%",
                              n != 0 & zone_avg > .40  & zone_avg <= .50 ~ "40-50%",
                              n != 0 & zone_avg > .50   ~ "Above 50%",
                              n == 0 ~ "No Shot Attempts"),
               text = paste("FG%: ", sprintf("%1.2f%%", 100*zone_avg), "\n",
                            "FGM: ", zone_make, "\n",
                            "FGA: ", n, "\n",
                            "Volume: ", sprintf("%1.2f%%", 100*volume), sep = "" ))
    
      # Create Plot
      heatmap <-  ggplot(data = shot_chart) +
        geom_sf(aes(fill = FG, text = text, color = zone_avg+zone_make+n+volume)) +
        scale_fill_manual(values=c('Above 50%' = "#7D1D2E", '40-50%' = "#CC8D94", '30-40%' ="#DEC1C1",
                                   '20-30%' = "#A4B3DE", '10-20%' = "#6274B3", '0-10%' = "#01418F",
                                   'No Shot Attempts' = "#E0E0E0"), na.value = "#E0E0E0") +
        scale_color_gradient(low="black", high="black") +
        labs(title = paste0("Field Goal Percentage: ", input$select_box)) +
        theme(plot.title = element_text(hjust = .5, face = "bold", size = 16, family = "Helvetica Neue",
                                        color = "white"),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y=element_blank(),  
              axis.ticks.y=element_blank(),
              panel.grid.major = element_blank(),
              panel.background = element_rect(fill = "#E0E0E0"),
              plot.background = element_rect(fill = "#083c94", color = "#083c94"),
              legend.background = element_rect(fill = "black"),
              legend.title = element_text(color = "white"),
              legend.text = element_text(color = "white"),
              legend.box.background = element_rect(color = "white"),
              legend.key = element_rect(color = "white")) +
        guides(color = "none")        
      
      ggplotly(heatmap, tooltip = "text") %>% 
        style(hoverlabel = list(bgcolor = "black"), hoveron = "fill", hoverinfo = "text")
      
    })
    
    
    output$possplot <- renderPlotly({
      
      zone_pct_poss <- data_t %>%
        filter(paint_tchs >= input$paint_tchs[1] & paint_tchs <= input$paint_tchs[2],
               shot_clock >= input$shot_clock[1] & shot_clock <= input$shot_clock[2],
               passes >= input$passes[1] & passes <= input$passes[2], 
               hc_sides >= input$side_tchs[1] & hc_sides <= input$side_tchs[2]) %>%
        group_by(ct_zone) %>%
        summarize(zone_avg = mean(fg_01),
                  zone_make = sum(fg_01),
                  zone_att = n())
      
      process_poss <- data_t %>%
        filter(paint_tchs >= input$paint_tchs[1] & paint_tchs <= input$paint_tchs[2],
               shot_clock >= input$shot_clock[1] & shot_clock <= input$shot_clock[2],
               passes >= input$passes[1] & passes <= input$passes[2], 
               hc_sides >= input$side_tchs[1] & hc_sides <= input$side_tchs[2]) %>%
        mutate(zone_make = sum(fg_01)) %>%
        count(ct_zone, .drop = FALSE)
      
      final_poss <- process_poss %>%
        left_join(zone_pct_poss, by = c("ct_zone" = "ct_zone"))
      
      shot_chart_poss <- court %>% 
        left_join(final_poss, by = c("Zone" = "ct_zone")) %>%
        filter(Zone != "court", Zone != "three", Zone != "low box") %>%
        mutate(FG = case_when(zone_avg <= .10 ~ "0-10%",
                              zone_avg > .10 & zone_avg <= .20 ~ "10-20%",
                              zone_avg > .20 & zone_avg <= .30 ~ "20-30%",
                              zone_avg > .30  & zone_avg <= .40 ~ "30-40%",
                              zone_avg > .40  & zone_avg <= .50 ~ "40-50%",
                              zone_avg > .50   ~ "Above 50%"),
               text = paste("FG%: ", sprintf("%1.2f%%", 100*zone_avg), "\n",
                            "FGM: ", zone_make, "\n",
                            "FGA: ", n, 
                            sep = "" ))
      
      poss_plotly <- ggplot(data = shot_chart_poss) +
        geom_sf(aes(fill = FG, text = text, color = zone_avg+zone_make+n)) +
        scale_fill_manual(values=c('Above 50%' = "#7D1D2E", '40-50%' = "#CC8D94", '30-40%' ="#DEC1C1",
                                   '20-30%' = "#A4B3DE", '10-20%' = "#6274B3", '0-10%' = "#01418F",
                                   'No Shot Attempts' = "#E0E0E0"), na.value = "#E0E0E0") +
        scale_color_gradient(low="black", high="black") +
        labs(title = "Field Goal Percentage: Team") +
        theme(plot.title = element_text(hjust = .5, face = "bold", size = 16, family = "Helvetica Neue",
                                        color = "white"),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y=element_blank(),  
              axis.ticks.y=element_blank(),
              panel.grid.major = element_blank(),
              panel.background = element_rect(fill = "#E0E0E0"),
              plot.background = element_rect(fill = "#083c94", color = "#083c94"),
              legend.background = element_rect(fill = "black"),
              legend.title = element_text(color = "white"),
              legend.text = element_text(color = "white"),
              legend.box.background = element_rect(color = "white"),
              legend.key = element_rect(color = "white")) +
        guides(color = "none")
      
      ggplotly(poss_plotly, tooltip = "text") %>% 
        style(hoverlabel = list(bgcolor = "black"), hoveron = "fill", hoverinfo = "text")
      
    })

    output$table <- renderTable({
      
      
      if(input$select_box4 == "FG Percentages") {
        if(input$select_box == "Team") {
          table_home
        } else {
          table_home %>% 
            filter(Player %in% c("Team", paste0(input$select_box)))
        }
        
      } else {
        if(input$select_box == "Team") {
          table_homea
        } else {
          table_homea %>% 
            filter(Player %in% c("Team", paste0(input$select_box)))
        }

      }
      
    }, hover = T, caption = "Summary Stats:", align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"))
    
    output$trend1 <- renderPlotly({
      if(input$select_box2 == "Team") {
        game <- data_t %>%
          group_by(game_nbr, opp_id, outcome, score) %>%
          summarize(game_avg = round(100*mean(fg_01), 2),
                    game_make = sum(fg_01),
                    game_att = n())
      } else{
        game <- data_p %>%
          filter(Player == input$select_box2) %>%
          group_by(game_nbr, opp_id, outcome, score) %>%
          summarize(game_avg = round(100*mean(fg_01), 2),
                    game_make = sum(fg_01),
                    game_att = n())
        
      }
      
      
      trend <- ggplot(data = game, aes(x = game_nbr, y = game_avg, group = 1,
                                       text = paste0(
                                         "Opponent: ", opp_id, "\n",
                                         "Outcome: ", outcome, "\n",
                                         "Score: ", score, "\n",
                                         "FG%: ", game_avg, "%", "\n",
                                         "Attempts: ", game_make, "/", game_att, "\n",
                                         sep = ""))) +
        geom_line(color = "#0b3d91", lwd = 1.5) +
        geom_point(color = "black") +
        geom_hline(yintercept = 100*mean(data$fg_01), color = "red", show.legend = T) +
        annotate("text", x=max(game$game_nbr)-1.5, y=97, label="Team Average FG%", color = "red") +
        ylim(0,100) +
        scale_x_continuous(breaks = round(seq(min(game$game_nbr), max(game$game_nbr), by = 1),1)) +
        xlab("Game Number") +
        ylab("Field Goal Percentage") +
        labs(title = paste0("Field Goal Efficiency Over Season: ", input$select_box2)) +
        theme(panel.background = element_rect(fill = "#E0E0E0"),
              plot.title = element_text(hjust = .5, face = "bold", size = 16, family = "Helvetica Neue",
                                        color = "white"),
              panel.grid = element_line(color= "white"),
              plot.background = element_rect(fill = "#083c94", color = "#083c94"),
              axis.text = element_text(color = "white"),
              axis.ticks = element_line(color = "white"),
              axis.title = element_text(color = "white"))
      
      ggplotly(trend, tooltip = "text")
    })
    
    output$table2 <- renderTable({
      if(input$select_box5 == "FG Percentages") {
        if(input$select_box2 == "Team") {
          table2
        } else {
          fg2p <- data_p %>%
            filter(Player == paste0(input$select_box2)) %>%
            group_by(opp_code, game_id) %>%
            summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01)))
          
          fg32p <- data_p %>%
            filter(Player == paste0(input$select_box2),
                   ct_zone %in% c(1,8,18,19,20,14,7)) %>%
            group_by(opp_code, game_id) %>%
            summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01)))
          
          shot_type2p <-  data_p %>%
            filter(Player == paste0(input$select_box2),
                   shot_id %in% c("Off the Dribble", "Catch and Shoot")) %>%
            group_by(opp_code, game_id, shot_id) %>%
            summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01)))
          
          qtr2p <- data_p %>%
            filter(Player == paste0(input$select_box2)) %>%
            group_by(opp_code, game_id, qtr_id) %>%
            summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01)))
          
          pos2p <- data_p %>%
            filter(Player == paste0(input$select_box2),
                   pos_type %in% c("Half Court", "Transition")) %>%
            group_by(opp_code, game_id, pos_type) %>%
            summarize(fg = sprintf("%1.2f%%", 100*mean(fg_01)))
          
          table2p <- fg2p %>% left_join(fg32p, by =c("opp_code" = "opp_code", "game_id" = "game_id")) %>%
            left_join(shot_type2p, by= c("opp_code" = "opp_code", "game_id" = "game_id")) %>%
            left_join(pos2p, by= c("opp_code" = "opp_code", "game_id" = "game_id")) %>%
            left_join(qtr2p, by= c("opp_code" = "opp_code", "game_id" = "game_id")) %>%
            pivot_wider(names_from = shot_id, values_from = fg.x.x) %>%
            pivot_wider(names_from = pos_type, values_from = fg.y.y) %>%
            pivot_wider(names_from = qtr_id, values_from = fg) %>%
            arrange(mixedrank(opp_code))  %>%
            rename('FG%' = fg.x, '3P FG%' = fg.y, 'Opponent' = opp_code, 'Game Type' = game_id)  %>%
            replace(is.na(.), "-") 
          
          table2p
        }
        
      } else {
        if(input$select_box2 == "Team") {
          table2a
        } else {
          fg2pa <- data_p %>%
            filter(Player == paste0(input$select_box2)) %>%
            group_by(opp_code, game_id) %>%
            summarize(fg = paste(sum(fg_01), "/", n()))
          
          fg32pa <- data_p %>%
            filter(Player == paste0(input$select_box2),
                   ct_zone %in% c(1,8,18,19,20,14,7)) %>%
            group_by(opp_code, game_id) %>%
            summarize(fg = paste(sum(fg_01), "/", n()))
          
          shot_type2pa <-  data_p %>%
            filter(Player == paste0(input$select_box2),
                   shot_id %in% c("Off the Dribble", "Catch and Shoot")) %>%
            group_by(opp_code, game_id, shot_id) %>%
            summarize(fg = paste(sum(fg_01), "/", n()))
          
          
          qtr2pa <- data_p %>%
            filter(Player == paste0(input$select_box2)) %>%
            group_by(opp_code, game_id, qtr_id) %>%
            summarize(fg = paste(sum(fg_01), "/", n()))
          
          
          pos2pa <- data_p %>%
            filter(Player == paste0(input$select_box2),
                   pos_type %in% c("Half Court", "Transition")) %>%
            group_by(opp_code, game_id, pos_type) %>%
            summarize(fg = paste(sum(fg_01), "/", n()))
          
          
          table2pa <- fg2pa %>% left_join(fg32pa, by =c("opp_code" = "opp_code", "game_id" = "game_id")) %>%
            left_join(shot_type2pa, by= c("opp_code" = "opp_code", "game_id" = "game_id")) %>%
            left_join(pos2pa, by= c("opp_code" = "opp_code", "game_id" = "game_id")) %>%
            left_join(qtr2pa, by= c("opp_code" = "opp_code", "game_id" = "game_id")) %>%
            pivot_wider(names_from = shot_id, values_from = fg.x.x) %>%
            pivot_wider(names_from = pos_type, values_from = fg.y.y) %>%
            pivot_wider(names_from = qtr_id, values_from = fg) %>%
            arrange(mixedrank(opp_code))  %>%
            rename('FG%' = fg.x, '3P FG%' = fg.y, 'Opponent' = opp_code, 'Game Type' = game_id)  %>%
            replace(is.na(.), "-") 
          
          table2pa
        }
      }
    }, hover = T, caption = "Summary Stats:", align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"))
    

    
    output$trend2 <- renderPlotly({
      trend2 <- data_t %>%
        filter(opp_code == input$select_box3)
      
      trend2_plot <- ggplot(data = trend2, aes(x = game_pos, y = round(100*trend_pct,2), group = 1,
                                               text = paste0("Possession: ", game_pos, "\n",
                                                             "Attempts: ", trend_pct*game_pos, "/", game_pos, "\n",
                                                             "FG%: ", sprintf("%1.2f%%", 100*trend_pct)))) +
        geom_line(color = "#0b3d91", lwd = 1.5) +
        xlab("Possession Number") +
        ylab("Field Goal Percentage") +
        labs(title = paste0("Duke Field Goal Efficiency Game: ", input$select_box3 )) +
        geom_hline(yintercept = 100*mean(data$fg_01), color = "red", show.legend = T) +
        annotate("text", x=max(trend2$game_pos)-6, y=97, label="Team Average FG%", color = "red") +
        ylim(0,100) +
        theme(panel.background = element_rect(fill = "#E0E0E0"),
              plot.title = element_text(hjust = .5, face = "bold", size = 16, family = "Helvetica Neue",
                                        color = "white"),
              plot.background = element_rect(fill = "#083c94", color = "#083c94"),
              axis.text = element_text(color = "white"),
              axis.ticks = element_line(color = "white"),
              axis.title = element_text(color = "white"))
      
      ggplotly(trend2_plot, tooltip = "text") %>%
        style(hoverlabel = list(bgcolor = "black"))
    })
    
    output$table3 <- renderTable({
      table3_f %>% 
        filter(Opponent %in% c("Season Averages", paste0(input$select_box3)))
    }, hover = T, caption = "Summary Stats:", align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"))

    
}

# Run the application 
shinyApp(ui = ui, server = server)
