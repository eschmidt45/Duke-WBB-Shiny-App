# Shiny App: 2022-23 Duke Women's Basketball Offensive Efficiency

## Overview
This project seeks to explore and analyze how field goal efficiency varies from game to game, from player to player, and across different types of posessions from the 2022-23 Duke Women's Basketball season. The hope is that the Duke coaching staff will be able to utilize this feedback to enhance player development and offensive strategy. 

## Data
Data was collected on every offensive possession from the 2022-23 season that resulted in a shot attempt (possessions that resulted in a turnover or free throws were dropped). Key features of the possession data include: game_id (non-conference, conference, post-season), game_nbr (chronological game order), opp_code (opponent name), outcome (win/loss), score (final score), qtr_id (quarter when the shot occurred), ct_zone (location of the shot attempt), player_id (who took the shot), passes (number of passes that occurred in the half court), pos_type (transition, half court, sideline out of bounds, baseline out of bounds), adv_id (whether or not the offense had a numbers advantage on the defence, ex: 3 on 2, 4 on 3, etc), shot_clock (when in the possession the shot occurred), oreb_id (whether or not the possession was created by an offensive rebound), shot_id (off of the dribble, catch and shoot, putback), paint_tchs (number of paint touches), hc_sides (number of sides the ball touched sideline to sideline in the half court), fg_01 (0-miss, 1-make). 

## App Construction
The app consists of four seperate tab panels, each offering a different insight into the offensive efficiency of the team. The four panels include a home tab, possession breakdown tab, season trends tab, and game trends tab.  

### Home Tab
The home tab is designed to provide a general overview of the season as a whole. The first component allows the user to select either the team, or an individual player and displayes a heat map of their respective shot attempts over the course of the season. Below the heat map a table offers a summary of season overview statistics. These statistics include: FG%, 3P FG%, Off the dribble FG%, Catch and Shoot FG%, Half Court FG%, Transition FG%, and finally FG% in each of the four quarters. The user has the option to convert these percentages to an attempts view (3/5, 7/10, etc).


<img width="1279" alt="Screen Shot 2023-05-03 at 11 38 04 PM" src="https://user-images.githubusercontent.com/88643459/236107599-ecc974b5-a0a8-4d79-a652-2a9813db630a.png">

<img width="1279" alt="Screen Shot 2023-05-03 at 11 39 15 PM" src="https://user-images.githubusercontent.com/88643459/236107973-94ed5281-f817-436d-bba9-73641423fdd3.png">


### Possession Breakdown Tab
The possession breakdown tab seeks to give the coaching staff a "build your own possession" experience. This tab allows the user to control slider inputs controlling the range for the shot clock, number of paint touches, number of sides in the half court, and number of passess in the half court. With each change of the sliders a new heat map populates, offering coaches insight on what components of a possession lead to more success than others. 


<img width="1280" alt="Screen Shot 2023-05-03 at 11 39 33 PM" src="https://user-images.githubusercontent.com/88643459/236108762-c67d77df-1861-4d4e-9509-6618d7a8934f.png">


### Season Trends Tab
The season trends tab looks to explore how both the team as a whole and individuals performed over the course of the season. Specifically, FG% is plotted in a time series plot on a game by game basis, with the team season FG% average plotted as a comparable hline. Below this plot a table offers the same summary statistics mentioned in the home tab, this time broken down to a game by game level. Again, the user has the option to switch this table to an attempts view. 


<img width="1280" alt="Screen Shot 2023-05-03 at 11 39 52 PM" src="https://user-images.githubusercontent.com/88643459/236109166-6ec6afd8-fc79-4947-be9d-73da647caf9b.png">


<img width="1280" alt="Screen Shot 2023-05-03 at 11 40 14 PM" src="https://user-images.githubusercontent.com/88643459/236109177-1510b892-6eb5-45f5-a718-dc1684b66e6d.png">


### Game Trends Tab
The game trends tab breaks down the offensive performance of the team over the course of individual games. The user has the option to select individual games and this time team FG% is plotted in a time series plot on a possession by possession basis, again with the team season FG% average plotted as a comparable hline. Below the plot a table compares possession statistic season averages with the statistics from the selected game. The statistics of interest include: Makes, Atttempts, Paint Touches, Paint Touches per Possession, Passes, Passes per Possession, Sides, and Sides per Possession.


<img width="1278" alt="Screen Shot 2023-05-03 at 11 40 44 PM" src="https://user-images.githubusercontent.com/88643459/236110083-0df9a143-10b5-4c6e-8268-e66cc7b4f1f8.png">
