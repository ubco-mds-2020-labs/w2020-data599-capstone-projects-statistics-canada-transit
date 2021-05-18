import dash
import dash_bootstrap_components as dbc
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output, State
import altair as alt
import plotly as plt
from vega_datasets import data
import numpy as np
import pandas as pd

alt.renderers.set_embed_options(actions=False)

# DATA

# ALl the player data for NBA
columns_to_skip = [
    "birth_year",
    "birth_month",
    "height",
    "height_cm",
    "weight",
    "weight_kg",
    "nationality",
    "high_school",
    "draft_round",
    "draft_pick",
    "draft_team",
]
player_data = pd.read_csv(
    "https://github.com/ubco-mds-2020-labs/nba_analytics/raw/main/data/players_stats_by_season_full_details.csv",
    usecols=lambda x: x not in columns_to_skip,
)
player_data = player_data[player_data["League"] == "NBA"].drop("League", axis=1)
player_data["birth_date"] = pd.to_datetime(
    player_data["birth_date"], format="%b %d, %Y"
)
player_data["Season"] = pd.to_numeric(player_data["Season"].str.split(expand=True)[0])

# Data for Metrics / Key Performance Indicators
df_metrics = player_data[
    ["Player", "Stage", "GP", "MIN", "FGM", "FGA", "FTM", "FTA", "3PM", "3PA"]
].copy()
df_metrics = df_metrics.groupby(["Player", "Stage"]).mean().reset_index().copy()
df_metrics["career_FG_%"] = round(df_metrics["FGM"] / df_metrics["FGA"] * 100, 2)
df_metrics["career_FT_%"] = round(df_metrics["FTM"] / df_metrics["FTA"] * 100, 2)
df_metrics["career_3PT_%"] = round(df_metrics["3PM"] / df_metrics["3PA"] * 100, 2)
df_metrics["Minutes_per_game"] = round(df_metrics["MIN"] / df_metrics["GP"], 2)
df_metrics.drop(
    ["GP", "MIN", "FGM", "FGA", "FTM", "FTA", "3PM", "3PA"], axis=1, inplace=True
)

#  Data for Chart 1
df_chart_1 = player_data[
    ["Player", "Season", "Stage", "GP", "PTS", "FGM", "FTM", "3PM"]
].copy()
df_chart_1["PTS"] = df_chart_1["PTS"] / df_chart_1["GP"]
df_chart_1["3 Point"] = (df_chart_1["3PM"] * 3) / df_chart_1["GP"]
df_chart_1["FTM"] = df_chart_1["FTM"] / df_chart_1["GP"]
df_chart_1.rename(columns={"FTM": "Free throws"}, inplace=True)
df_chart_1["2 Point"] = df_chart_1["PTS"] - (
    df_chart_1["3 Point"] + df_chart_1["Free throws"]
)
df_chart_1.drop(["PTS", "GP", "FGM", "3PM"], axis=1, inplace=True)
df_chart_1 = df_chart_1.melt(
    id_vars=["Player", "Season", "Stage"],
    var_name="Points_type",
    value_name="Points_per_game",
).copy()
df_chart_1["Points_per_game"] = round(df_chart_1["Points_per_game"], 2)

# Data for Chart 2
df_chart_2 = player_data[["Player", "Season", "Stage", "GP", "AST"]].copy()
df_chart_2["Assists_per_game"] = round(df_chart_2["AST"] / df_chart_2["GP"], 2)
df_chart_2.drop(["GP", "AST"], axis=1, inplace=True)

# Data for Chart 3
df_chart_3 = player_data[["Player", "Season", "Stage", "GP", "ORB", "DRB"]].copy()
df_chart_3["Offensive Rebounds"] = round(df_chart_3["ORB"] / df_chart_3["GP"], 2)
df_chart_3["Defensive Rebounds"] = round(df_chart_3["DRB"] / df_chart_3["GP"], 2)
df_chart_3.drop(["GP", "ORB", "DRB"], axis=1, inplace=True)
df_chart_3 = df_chart_3.melt(
    id_vars=["Player", "Season", "Stage"],
    var_name="Rebound_type",
    value_name="Rebounds_per_game",
).copy()

# Data for Chart 4
df_chart_4 = player_data[["Player", "Season", "Stage", "GP", "BLK", "STL"]].copy()
df_chart_4["Blocks"] = round(df_chart_4["BLK"] / df_chart_4["GP"], 2)
df_chart_4["Steals"] = round(df_chart_4["STL"] / df_chart_4["GP"], 2)
df_chart_4.drop(["GP", "BLK", "STL"], axis=1, inplace=True)
df_chart_4 = df_chart_4.melt(
    id_vars=["Player", "Season", "Stage"],
    var_name="Blocks/Steals",
    value_name="per_game",
).copy()

# Data for Chart 5
df_chart_5 = player_data[["Player", "Season", "Stage", "GP", "TOV", "PF"]].copy()
df_chart_5["Turnovers"] = round(df_chart_5["TOV"] / df_chart_5["GP"], 2)
df_chart_5["Fouls"] = round(df_chart_5["PF"] / df_chart_5["GP"], 2)
df_chart_5.drop(["GP", "TOV", "PF"], axis=1, inplace=True)
df_chart_5 = df_chart_5.melt(
    id_vars=["Player", "Season", "Stage"],
    var_name="Turnovers/Fouls",
    value_name="per_game",
).copy()

# Data for Chart 11 (chart 1 in second tab)
df_chart_11 = player_data[
    ["Player", "Season", "Stage", "FGM", "FGA", "3PM", "3PA"]
].copy()
df_chart_11["2PA"] = df_chart_11["FGA"] - df_chart_11["3PA"]
df_chart_11["2PM"] = df_chart_11["FGM"] - df_chart_11["3PM"]
df_chart_11 = (
    df_chart_11.groupby(["Player", "Season", "Stage"]).mean().reset_index().copy()
)
df_chart_11["2PT_%"] = round(df_chart_11["2PM"] / df_chart_11["2PA"] * 100, 2)
df_chart_11["3PT_%"] = round(df_chart_11["3PM"] / df_chart_11["3PA"] * 100, 2)
df_chart_11["eFG_%"] = round(
    ((df_chart_11["FGM"] + (0.5 * df_chart_11["3PM"])) / df_chart_11["FGA"]) * 100, 2
)
df_chart_11.drop(["FGM", "FGA", "3PM", "3PA", "2PM", "2PA"], axis=1, inplace=True)
df_chart_11 = df_chart_11.melt(
    id_vars=["Player", "Season", "Stage"], var_name="2PT_3PT_eFG", value_name="per_game"
).copy()

# Data for Chart 12
df_chart_12 = player_data[
    ["Player", "Season", "Stage", "GP", "PTS", "FGA", "FTA"]
].copy()
df_chart_12["PTS"] = df_chart_12["PTS"] / df_chart_12["GP"]
df_chart_12["FGA"] = df_chart_12["FGA"] / df_chart_12["GP"]
df_chart_12["FTA"] = df_chart_12["FTA"] / df_chart_12["GP"]
df_chart_12["True shooting attempts"] = df_chart_12["FGA"] + 0.44 * df_chart_12["FTA"]
df_chart_12["True shooting percentage"] = round(
    df_chart_12["PTS"] / (2 * df_chart_12["True shooting attempts"]) * 100, 2
)
df_chart_12.drop(
    ["GP", "PTS", "FGA", "FTA", "True shooting attempts"], axis=1, inplace=True
)

# Data for Chart 13 & 14
df_chart_13 = player_data[
    [
        "Player",
        "Season",
        "Stage",
        "GP",
        "PTS",
        "MIN",
        "FGM",
        "FGA",
        "FTM",
        "FTA",
        "ORB",
        "DRB",
        "STL",
        "AST",
        "BLK",
        "PF",
        "TOV",
    ]
].copy()
df_chart_13["PTS"] = df_chart_13["PTS"] / df_chart_13["GP"]
df_chart_13["Minutes Played"] = round(df_chart_13["MIN"] / df_chart_13["GP"], 2)
df_chart_13["FGM"] = df_chart_13["FGM"] / df_chart_13["GP"]
df_chart_13["FGA"] = df_chart_13["FGA"] / df_chart_13["GP"]
df_chart_13["FTM"] = df_chart_13["FTM"] / df_chart_13["GP"]
df_chart_13["FTA"] = df_chart_13["FTA"] / df_chart_13["GP"]
df_chart_13["ORB"] = df_chart_13["ORB"] / df_chart_13["GP"]
df_chart_13["DRB"] = df_chart_13["DRB"] / df_chart_13["GP"]
df_chart_13["STL"] = df_chart_13["STL"] / df_chart_13["GP"]
df_chart_13["AST"] = df_chart_13["AST"] / df_chart_13["GP"]
df_chart_13["BLK"] = df_chart_13["BLK"] / df_chart_13["GP"]
df_chart_13["PF"] = df_chart_13["PF"] / df_chart_13["GP"]
df_chart_13["TOV"] = df_chart_13["TOV"] / df_chart_13["GP"]
df_chart_13["Game Score"] = round(
    df_chart_13["PTS"]
    + 0.4 * df_chart_13["FGM"]
    - 0.7 * df_chart_13["FGA"]
    - 0.4 * (df_chart_13["FTA"] - df_chart_13["FTM"])
    + 0.7 * df_chart_13["ORB"]
    + 0.3 * df_chart_13["DRB"]
    + df_chart_13["STL"]
    + 0.7 * df_chart_13["AST"]
    + 0.7 * df_chart_13["BLK"]
    - 0.4 * df_chart_13["PF"]
    - df_chart_13["TOV"],
    2,
)
df_chart_13.drop(
    [
        "GP",
        "PTS",
        "MIN",
        "FGM",
        "FGA",
        "FTM",
        "FTA",
        "ORB",
        "DRB",
        "STL",
        "AST",
        "BLK",
        "PF",
        "TOV",
    ],
    axis=1,
    inplace=True,
)

# DATA for NBA Trends tab
NBA_data = pd.read_csv(
    "https://github.com/ubco-mds-2020-labs/nba_analytics/raw/main/data/players_stats_by_season_full_details.csv"
)
# filter data to only include NBA
NBA_data = NBA_data[NBA_data["League"] == "NBA"]
# Regular season data
NBA_reg = NBA_data[NBA_data["Stage"] == "Regular_Season"]
# Playoff data
NBA_playoff = NBA_data[NBA_data["Stage"] == "Playoffs"]
# group by the seasons and sum up numerical columns. This shows stats by season (this includes regular season and playoff games together)
NBA_seasons_full = NBA_data.groupby("Season").sum().reset_index()

# list for the drop down menu
longform_stat_list = [
    "Average Player Minutes Played per Game",
    "Field Goals Made per Game",
    "Field Goals Attempted per Game",
    "3-Pointers Made per Game",
    "3-Pointers Attempted per Game",
    "Free-throws Made per Game",
    "Free-throws Attempted per Game",
    "Turnovers per Game",
    "Personal Fouls per Game",
    "Offensive Rebounds per Game",
    "Defensive Rebounds per Game",
    "Total Rebounds per Game",
    "Assists per Game",
    "Steals per Game",
    "Blocks per Game",
    "Points per Game",
    "Average Player Weight (lbs)",
    "Average Player Height (cm)",
    "Average Player Body Mass Index",
    "Ratio of Field Goals That Are 3-pointers",
]

# dictionary linking dropdown list key to stat value column labels
stat_dict = {
    "Average Player Minutes Played per Game": "MIN/Game",
    "Field Goals Made per Game": "FGM/Game",
    "Field Goals Attempted per Game": "FGA/Game",
    "3-Pointers Made per Game": "3PM/Game",
    "3-Pointers Attempted per Game": "3PA/Game",
    "Free-throws Made per Game": "FTM/Game",
    "Free-throws Attempted per Game": "FTA/Game",
    "Turnovers per Game": "TOV/Game",
    "Personal Fouls per Game": "PF/Game",
    "Offensive Rebounds per Game": "ORB/Game",
    "Defensive Rebounds per Game": "DRB/Game",
    "Total Rebounds per Game": "REB/Game",
    "Assists per Game": "Ast/Game",
    "Steals per Game": "STL/Game",
    "Blocks per Game": "BLK/Game",
    "Points per Game": "Pts/Game",
    "Average Player Weight (lbs)": "avg_weight",
    "Average Player Height (cm)": "avg_height_cm",
    "Average Player Body Mass Index": "avg_BMI",
    "Ratio of Field Goals That Are 3-pointers": "3PM_ratio",
}

## Data wrangling for stats over time for players/teams:

# Create column for ratio of shots made that are 3-pointers
NBA_seasons_full["3PM_ratio"] = NBA_seasons_full["3PM"] / NBA_seasons_full["FGM"]
# Create a column for average player height (need to create a season average dataframe first)
NBA_seasons_full_avg = NBA_data.groupby("Season").mean().reset_index()
NBA_seasons_full["avg_height_cm"] = NBA_seasons_full_avg["height_cm"]
# Create a column for average player weight (lbs)
NBA_seasons_full["avg_weight"] = NBA_seasons_full_avg["weight"]
# Create a column for average player BMI
NBA_seasons_full["avg_BMI"] = NBA_seasons_full_avg["weight_kg"] / (
    (NBA_seasons_full_avg["height_cm"] / 100) ** 2
)
## All of the following numerical stats per game (eg. points, assists, etc.) are found by
## getting the stat/minute/player, then multiplying that by 5 for 5 players on the court at
## a time for each team and then multiplying by 48 for 48 minutes in a game.
# Create a points per game column
NBA_seasons_full["Pts/Game"] = (NBA_seasons_full["PTS"] / NBA_seasons_full["MIN"]) * (
    5 * 48
)
# Create an assists per game column
NBA_seasons_full["Ast/Game"] = (NBA_seasons_full["AST"] / NBA_seasons_full["MIN"]) * (
    5 * 48
)
# Create a field goals attempted per game column
NBA_seasons_full["FGA/Game"] = (NBA_seasons_full["FGA"] / NBA_seasons_full["MIN"]) * (
    5 * 48
)
# Create a field goals made per game column
NBA_seasons_full["FGM/Game"] = (NBA_seasons_full["FGM"] / NBA_seasons_full["MIN"]) * (
    5 * 48
)
# Create a total rebounds per game column
NBA_seasons_full["ORB/Game"] = (NBA_seasons_full["ORB"] / NBA_seasons_full["MIN"]) * (
    5 * 48
)
# Create a defensive rebounds per game column
NBA_seasons_full["DRB/Game"] = (NBA_seasons_full["DRB"] / NBA_seasons_full["MIN"]) * (
    5 * 48
)
# Create a total rebounds per game column
NBA_seasons_full["REB/Game"] = (NBA_seasons_full["REB"] / NBA_seasons_full["MIN"]) * (
    5 * 48
)
# Create a minutes per game column
NBA_seasons_full["MIN/Game"] = NBA_seasons_full["MIN"] / NBA_seasons_full["GP"]
# Create a 3 pointers made per game column
NBA_seasons_full["3PM/Game"] = (NBA_seasons_full["3PM"] / NBA_seasons_full["MIN"]) * (
    5 * 48
)
# Create a 3-pointers attempted per game column
NBA_seasons_full["3PA/Game"] = (NBA_seasons_full["3PA"] / NBA_seasons_full["MIN"]) * (
    5 * 48
)
# Create a free-throws attempted per game column
NBA_seasons_full["FTA/Game"] = (NBA_seasons_full["FTA"] / NBA_seasons_full["MIN"]) * (
    5 * 48
)
# Create a free-throws attempted per game column
NBA_seasons_full["FTM/Game"] = (NBA_seasons_full["FTM"] / NBA_seasons_full["MIN"]) * (
    5 * 48
)
# Create a turnovers per game column
NBA_seasons_full["TOV/Game"] = (NBA_seasons_full["TOV"] / NBA_seasons_full["MIN"]) * (
    5 * 48
)
# Create a personal fouls per game column
NBA_seasons_full["PF/Game"] = (NBA_seasons_full["PF"] / NBA_seasons_full["MIN"]) * (
    5 * 48
)
# Create a steals per game column
NBA_seasons_full["STL/Game"] = (NBA_seasons_full["STL"] / NBA_seasons_full["MIN"]) * (
    5 * 48
)
# Create a blocks per game column
NBA_seasons_full["BLK/Game"] = (NBA_seasons_full["BLK"] / NBA_seasons_full["MIN"]) * (
    5 * 48
)

## Data Wrangling for stats over time for age.

NBA_data_age = NBA_data.copy()
# Convert birth year to integer
NBA_data_age["birth_year"] = NBA_data_age["birth_year"].map(lambda x: int(x))
NBA_data_age.reset_index(drop=True, inplace=True)
# Convert Season to the year the season ended (instead of the two years the season spans)
# and convert it to an integer.
NBA_data_age["Season"] = NBA_data_age["Season"].map(lambda x: int(x[-4 : len(x)]))
# Create a column for age by subtracting birth year from the season year
# (use year of playoffs)
NBA_data_age["Age"] = NBA_data_age["Season"] - NBA_data_age["birth_year"]
# Drop ages > 39 years old because there are less than 30 players in these
# age groups so the average estimates are likely to be innacurate.
NBA_data_age = NBA_data_age[NBA_data_age["Age"] < 40]
# Group the NBA data by age and sum up the numerical categories
NBA_age_avg = NBA_data_age.groupby("Age").mean().reset_index()
NBA_age_sum = NBA_data_age.groupby("Age").sum().reset_index()
# Create column for ratio of shots made that are 3-pointers
NBA_age_sum["3PM_ratio"] = NBA_age_sum["3PM"] / NBA_age_sum["FGM"]
# Create a column for average player height (need to create a season average dataframe first)
NBA_age_sum["avg_height_cm"] = NBA_age_avg["height_cm"]
# Create a column for average player weight (lbs)
NBA_age_sum["avg_weight"] = NBA_age_avg["weight"]
# Create a column for average player BMI
NBA_age_sum["avg_BMI"] = NBA_age_avg["weight_kg"] / (
    (NBA_age_avg["height_cm"] / 100) ** 2
)
## All of the following numerical stats per game by age (eg. points, assists, etc.) are
##found by getting the stat/game played.
# Create a points per game column
NBA_age_sum["Pts/Game"] = NBA_age_sum["PTS"] / NBA_age_sum["GP"]
# Create an assists per game column
NBA_age_sum["Ast/Game"] = NBA_age_sum["AST"] / NBA_age_sum["GP"]
# Create a field goals attempted per game column
NBA_age_sum["FGA/Game"] = NBA_age_sum["FGA"] / NBA_age_sum["GP"]
# Create a field goals made per game column
NBA_age_sum["FGM/Game"] = NBA_age_sum["FGM"] / NBA_age_sum["GP"]
# Create a total rebounds per game column
NBA_age_sum["ORB/Game"] = NBA_age_sum["ORB"] / NBA_age_sum["GP"]
# Create a defensive rebounds per game column
NBA_age_sum["DRB/Game"] = NBA_age_sum["DRB"] / NBA_age_sum["GP"]
# Create a total rebounds per game column
NBA_age_sum["REB/Game"] = NBA_age_sum["REB"] / NBA_age_sum["GP"]
# Create a minutes per game column
NBA_age_sum["MIN/Game"] = NBA_age_sum["MIN"] / NBA_age_sum["GP"]
# Create a 3 pointers made per game column
NBA_age_sum["3PM/Game"] = NBA_age_sum["3PM"] / NBA_age_sum["GP"]
# Create a 3-pointers attempted per game column
NBA_age_sum["3PA/Game"] = NBA_age_sum["3PA"] / NBA_age_sum["GP"]
# Create a free-throws attempted per game column
NBA_age_sum["FTA/Game"] = NBA_age_sum["FTA"] / NBA_age_sum["GP"]
# Create a free-throws attempted per game column
NBA_age_sum["FTM/Game"] = NBA_age_sum["FTM"] / NBA_age_sum["GP"]
# Create a turnovers per game column
NBA_age_sum["TOV/Game"] = NBA_age_sum["TOV"] / NBA_age_sum["GP"]
# Create a personal fouls per game column
NBA_age_sum["PF/Game"] = NBA_age_sum["PF"] / NBA_age_sum["GP"]
# Create a steals per game column
NBA_age_sum["STL/Game"] = NBA_age_sum["STL"] / NBA_age_sum["GP"]
# Create a blocks per game column
NBA_age_sum["BLK/Game"] = NBA_age_sum["BLK"] / NBA_age_sum["GP"]

## Data Wrangling for playoff vs reg season stats chart

NBA_reg_seasons = NBA_reg.groupby("Season").sum().reset_index()
NBA_playoff_seasons = NBA_playoff.groupby("Season").sum().reset_index()

# Make regular season stats

# Create column for ratio of shots made that are 3-pointers
NBA_reg_seasons["3PM_ratio"] = NBA_reg_seasons["3PM"] / NBA_reg_seasons["FGM"]
# Create a column for average player height (need to create a season average dataframe first)
NBA_reg_seasons_avg = NBA_reg.groupby("Season").mean().reset_index()
NBA_reg_seasons["avg_height_cm"] = NBA_reg_seasons_avg["height_cm"]
# Create a column for average player weight (lbs)
NBA_reg_seasons["avg_weight"] = NBA_reg_seasons_avg["weight"]
# Create a column for average player BMI
NBA_reg_seasons["avg_BMI"] = NBA_reg_seasons_avg["weight_kg"] / (
    (NBA_reg_seasons_avg["height_cm"] / 100) ** 2
)
## All of the following numerical stats per game (eg. points, assists, etc.) are found
## by getting the stat/minute/player, then multiplying that by 5 for 5 players on the
## court at a time for each team and then multiplying by 48 for 48 minutes in a game.
# Create a points per game column
NBA_reg_seasons["Pts/Game"] = (NBA_reg_seasons["PTS"] / NBA_reg_seasons["MIN"]) * (
    5 * 48
)
# Create an assists per game column
NBA_reg_seasons["Ast/Game"] = (NBA_reg_seasons["AST"] / NBA_reg_seasons["MIN"]) * (
    5 * 48
)
# Create a field goals attempted per game column
NBA_reg_seasons["FGA/Game"] = (NBA_reg_seasons["FGA"] / NBA_reg_seasons["MIN"]) * (
    5 * 48
)
# Create a field goals made per game column
NBA_reg_seasons["FGM/Game"] = (NBA_reg_seasons["FGM"] / NBA_reg_seasons["MIN"]) * (
    5 * 48
)
# Create a total rebounds per game column
NBA_reg_seasons["ORB/Game"] = (NBA_reg_seasons["ORB"] / NBA_reg_seasons["MIN"]) * (
    5 * 48
)
# Create a defensive rebounds per game column
NBA_reg_seasons["DRB/Game"] = (NBA_reg_seasons["DRB"] / NBA_reg_seasons["MIN"]) * (
    5 * 48
)
# Create a total rebounds per game column
NBA_reg_seasons["REB/Game"] = (NBA_reg_seasons["REB"] / NBA_reg_seasons["MIN"]) * (
    5 * 48
)
# Create a minutes per game column
NBA_reg_seasons["MIN/Game"] = NBA_reg_seasons["MIN"] / NBA_reg_seasons["GP"]
# Create a 3 pointers made per game column
NBA_reg_seasons["3PM/Game"] = (NBA_reg_seasons["3PM"] / NBA_reg_seasons["MIN"]) * (
    5 * 48
)
# Create a 3-pointers attempted per game column
NBA_reg_seasons["3PA/Game"] = (NBA_reg_seasons["3PA"] / NBA_reg_seasons["MIN"]) * (
    5 * 48
)
# Create a free-throws attempted per game column
NBA_reg_seasons["FTA/Game"] = (NBA_reg_seasons["FTA"] / NBA_reg_seasons["MIN"]) * (
    5 * 48
)
# Create a free-throws attempted per game column
NBA_reg_seasons["FTM/Game"] = (NBA_reg_seasons["FTM"] / NBA_reg_seasons["MIN"]) * (
    5 * 48
)
# Create a turnovers per game column
NBA_reg_seasons["TOV/Game"] = (NBA_reg_seasons["TOV"] / NBA_reg_seasons["MIN"]) * (
    5 * 48
)
# Create a personal fouls per game column
NBA_reg_seasons["PF/Game"] = (NBA_reg_seasons["PF"] / NBA_reg_seasons["MIN"]) * (5 * 48)
# Create a steals per game column
NBA_reg_seasons["STL/Game"] = (NBA_reg_seasons["STL"] / NBA_reg_seasons["MIN"]) * (
    5 * 48
)
# Create a blocks per game column
NBA_reg_seasons["BLK/Game"] = (NBA_reg_seasons["BLK"] / NBA_reg_seasons["MIN"]) * (
    5 * 48
)

## Make playoff stats

# Create column for ratio of shots made that are 3-pointers
NBA_playoff_seasons["3PM_ratio"] = (
    NBA_playoff_seasons["3PM"] / NBA_playoff_seasons["FGM"]
)
# Create a column for average player height (need to create a season average dataframe first)
NBA_playoff_seasons_avg = NBA_playoff.groupby("Season").mean().reset_index()
NBA_playoff_seasons["avg_height_cm"] = NBA_playoff_seasons_avg["height_cm"]
# Create a column for average player weight (lbs)
NBA_playoff_seasons["avg_weight"] = NBA_playoff_seasons_avg["weight"]
# Create a column for average player BMI
NBA_playoff_seasons["avg_BMI"] = NBA_playoff_seasons_avg["weight_kg"] / (
    (NBA_playoff_seasons_avg["height_cm"] / 100) ** 2
)
## All of the following numerical stats per game (eg. points, assists, etc.) are found
## by getting the stat/minute/player, then multiplying that by 5 for 5 players on the
## court at a time for each team and then multiplying by 48 for 48 minutes in a game.
# Create a points per game column
NBA_playoff_seasons["Pts/Game"] = (
    NBA_playoff_seasons["PTS"] / NBA_playoff_seasons["MIN"]
) * (5 * 48)
# Create an assists per game column
NBA_playoff_seasons["Ast/Game"] = (
    NBA_playoff_seasons["AST"] / NBA_playoff_seasons["MIN"]
) * (5 * 48)
# Create a field goals attempted per game column
NBA_playoff_seasons["FGA/Game"] = (
    NBA_playoff_seasons["FGA"] / NBA_playoff_seasons["MIN"]
) * (5 * 48)
# Create a field goals made per game column
NBA_playoff_seasons["FGM/Game"] = (
    NBA_playoff_seasons["FGM"] / NBA_playoff_seasons["MIN"]
) * (5 * 48)
# Create a total rebounds per game column
NBA_playoff_seasons["ORB/Game"] = (
    NBA_playoff_seasons["ORB"] / NBA_playoff_seasons["MIN"]
) * (5 * 48)
# Create a defensive rebounds per game column
NBA_playoff_seasons["DRB/Game"] = (
    NBA_playoff_seasons["DRB"] / NBA_playoff_seasons["MIN"]
) * (5 * 48)
# Create a total rebounds per game column
NBA_playoff_seasons["REB/Game"] = (
    NBA_playoff_seasons["REB"] / NBA_playoff_seasons["MIN"]
) * (5 * 48)
# Create a minutes per game column
NBA_playoff_seasons["MIN/Game"] = NBA_playoff_seasons["MIN"] / NBA_playoff_seasons["GP"]
# Create a 3 pointers made per game column
NBA_playoff_seasons["3PM/Game"] = (
    NBA_playoff_seasons["3PM"] / NBA_playoff_seasons["MIN"]
) * (5 * 48)
# Create a 3-pointers attempted per game column
NBA_playoff_seasons["3PA/Game"] = (
    NBA_playoff_seasons["3PA"] / NBA_playoff_seasons["MIN"]
) * (5 * 48)
# Create a free-throws attempted per game column
NBA_playoff_seasons["FTA/Game"] = (
    NBA_playoff_seasons["FTA"] / NBA_playoff_seasons["MIN"]
) * (5 * 48)
# Create a free-throws attempted per game column
NBA_playoff_seasons["FTM/Game"] = (
    NBA_playoff_seasons["FTM"] / NBA_playoff_seasons["MIN"]
) * (5 * 48)
# Create a turnovers per game column
NBA_playoff_seasons["TOV/Game"] = (
    NBA_playoff_seasons["TOV"] / NBA_playoff_seasons["MIN"]
) * (5 * 48)
# Create a personal fouls per game column
NBA_playoff_seasons["PF/Game"] = (
    NBA_playoff_seasons["PF"] / NBA_playoff_seasons["MIN"]
) * (5 * 48)
# Create a steals per game column
NBA_playoff_seasons["STL/Game"] = (
    NBA_playoff_seasons["STL"] / NBA_playoff_seasons["MIN"]
) * (5 * 48)
# Create a blocks per game column
NBA_playoff_seasons["BLK/Game"] = (
    NBA_playoff_seasons["BLK"] / NBA_playoff_seasons["MIN"]
) * (5 * 48)

##Add a column of type of season to grouped dataframes
list = ["Regular Season"] * len(NBA_reg_seasons)
col = pd.Series(list)
NBA_reg_seasons["Type"] = col

plist = ["Playoffs"] * len(NBA_playoff_seasons)
pcol = pd.Series(plist)
NBA_playoff_seasons["Type"] = pcol

# Combine playoffs and regular season dataframes into one
type_seasons = NBA_reg_seasons.append(NBA_playoff_seasons)


# ------------------------------------------------------------#
# Setup app and layout/frontend
app = dash.Dash(external_stylesheets=[dbc.themes.BOOTSTRAP])
server = app.server

# cards

fifth_card = dbc.Card(
    dbc.CardBody(
        children=[html.P("Career FG%", className="card-title"), html.H5(id="card-05")]
    ),
    color="info",
    inverse=True,
    style={"text-align": "center"},
)

sixth_card = dbc.Card(
    dbc.CardBody(
        children=[html.P("Career FT%", className="card-title"), html.H5(id="card-06")]
    ),
    color="secondary",
    inverse=True,
    style={"text-align": "center"},
)

seventh_card = dbc.Card(
    dbc.CardBody(
        children=[
            html.P("Career 3-pt %", className="card-title"),
            html.H5(id="card-07"),
        ]
    ),
    color="info",
    inverse=True,
    style={"text-align": "center"},
)

eighth_card = dbc.Card(
    dbc.CardBody(
        children=[
            html.P("Avg Minutes per game", className="card-title"),
            html.H5(id="card-08"),
        ]
    ),
    color="secondary",
    inverse=True,
    style={"text-align": "center"},
)


cards_tab2 = dbc.Row(
    [
        dbc.Col(fifth_card, width=3),
        dbc.Col(sixth_card, width=3),
        dbc.Col(seventh_card, width=3),
        dbc.Col(eighth_card, width=3),
    ]
)
# dropdowns
first_dropdown = html.Div(
    [
        dcc.Dropdown(
            id="player-widget",
            value="Kobe Bryant",  # REQUIRED to show the plot on the first page load
            options=[
                {"label": player, "value": player}
                for player in player_data["Player"].unique()
            ],
        )
    ],
    style={"width": "100%"},
)

second_dropdown = html.Div(
    [
        dcc.Dropdown(
            id="stage-widget",
            value="Regular_Season",  # REQUIRED to show the plot on the first page load
            options=[
                {"label": Stage, "value": Stage}
                for Stage in player_data["Stage"].unique()
            ],
        )
    ],
    style={"width": "100%"},
)

dropdowns = dbc.Col(
    [
        dbc.Row(first_dropdown),
        dbc.Row(html.H5("")),
        dbc.Row(second_dropdown),
        dbc.Row(html.H5("")),
    ],
    width=3,
)

third_dropdown = html.Div(
    [
        dcc.Dropdown(
            id="player-widget-2",
            # style={'width': '250px'},
            value="Kobe Bryant",  # REQUIRED to show the plot on the first page load
            options=[
                {"label": player, "value": player}
                for player in player_data["Player"].unique()
            ],
        )
    ],
    style={"width": "100%"},
)

fourth_dropdown = html.Div(
    [
        dcc.Dropdown(
            id="stage-widget-2",
            # style={'width': '250px'},
            value="Regular_Season",  # REQUIRED to show the plot on the first page load
            options=[
                {"label": Stage, "value": Stage}
                for Stage in player_data["Stage"].unique()
            ],
        )
    ],
    style={"width": "100%"},
)

fifth_dropdown = html.Div(
    [
        html.Br(),
    ]
)

sith_dropdown = html.Div(
    [
        html.Br(),
    ]
)

dropdowns2_1 = dbc.Row(
    [
        dbc.Col(third_dropdown, width=3),
        # dbc.Col(fourth_dropdown, width=3),
        dbc.Col(html.H5(""), width=6),
    ]
)

dropdowns2_2 = dbc.Row(
    [
        dbc.Col(fourth_dropdown, width=3),
        dbc.Col(html.H5(""), width=6),
    ]
)

dropdowns2_3 = dbc.Row(
    [
        dbc.Col(fifth_dropdown, width=3),
        dbc.Col(html.H5(""), width=6),
    ]
)


dropdowns3 = dbc.Row([dbc.Col(sith_dropdown, width=5), dbc.Col(html.H5(""), width=7)])

tab1_content = html.Div(
    [
        dropdowns,
        html.Br(),
        # cards,
        html.Br(),
        dbc.Row(
            [
                dbc.Col(
                    html.Iframe(
                        id="chart-5",
                        style={
                            "border-width": "1",
                            "border-color": "#DCDCDC",
                            # "width": "1090px",
                            # "height": "300px",
                        },
                    ),
                    # width=12,
                )
            ]
        ),
    ]
)

tab2_content = html.Div(
    [
        dropdowns2_1,
        html.Br(),
        dropdowns2_2,
        html.Br(),
        dropdowns2_3,
        html.Br(),
        cards_tab2,
        html.Br(),
        dbc.Row(
            [
                dbc.Col(
                    html.Iframe(
                        id="chart-11",
                        style={
                            "border-width": "1",
                            "border-color": "#DCDCDC",
                            "width": "530px",
                            "height": "300px",
                        },
                    ),
                    width=6,
                ),
                dbc.Col(
                    html.Iframe(
                        id="chart-12",
                        style={
                            "border-width": "1",
                            "border-color": "#DCDCDC",
                            "width": "530px",
                            "height": "300px",
                        },
                    ),
                    width=6,
                ),
            ]
        ),
    ]
)

tab3_content = html.Div(
    [
        dropdowns3,
        # cards_tab2,
        html.Br(),
        dbc.Row(
            [
                dbc.Col(
                    html.Iframe(
                        id="chart-21",
                        style={
                            "border-width": "1",
                            "border-color": "#DCDCDC",
                            "width": "530px",
                            "height": "300px",
                        },
                    ),
                    width=6,
                ),
                dbc.Col(
                    html.Iframe(
                        id="chart-22",
                        style={
                            "border-width": "1",
                            "border-color": "#DCDCDC",
                            "width": "530px",
                            "height": "300px",
                        },
                    ),
                    width=6,
                ),
            ]
        ),
        dbc.Row(
            [
                dbc.Col(
                    html.Iframe(
                        id="chart-23",
                        style={
                            "border-width": "1",
                            "border-color": "#DCDCDC",
                            "width": "1090px",
                            "height": "300px",
                        },
                    ),
                    width=12,
                )
            ]
        ),
    ]
)

tab4_content = html.Div(
    [
        html.Br(),
    ]
)

tab5_content = html.Div(
    [
        html.Br(),
    ]
)

tabs = html.Div(
    [
        html.H2(
            "MEASURES OF PUBLIC TRANSIT ACCESSIBILITY TO CULTURAL AND ART FACILITIES IN VANCOUVER"
        ),
        html.H3("STATISTICS CANADA"),
        dbc.Tabs(
            [
                dbc.Tab(
                    children=[html.Br(), tab1_content],
                    label="Block assessibility map",
                    style={"padding": "10px"},
                    label_style={
                        "color": "#4682B4",
                        "font-weight": "bold",
                        "font-size": "larger",
                        "background-color": "#f4f6f6",
                    },
                    active_label_style={
                        "color": "#DC143C",
                        "font-weight": "bold",
                        "font-size": "larger",
                        "background-color": "#FFEFD5",
                    },
                ),
                dbc.Tab(
                    children=[html.Br(), tab2_content],
                    label="Transit efficiency map",
                    style={"padding": "10px"},
                    label_style={
                        "color": "#4682B4",
                        "font-weight": "bold",
                        "font-size": "larger",
                        "background-color": "#f4f6f6",
                    },
                    active_label_style={
                        "color": "#DC143C",
                        "font-weight": "bold",
                        "font-size": "larger",
                        "background-color": "#FFEFD5",
                    },
                ),
                dbc.Tab(
                    children=[html.Br(), tab3_content],
                    label="Accessibility equity map",
                    style={"padding": "10px"},
                    label_style={
                        "color": "#4682B4",
                        "font-weight": "bold",
                        "font-size": "larger",
                        "background-color": "#f4f6f6",
                    },
                    active_label_style={
                        "color": "#DC143C",
                        "font-weight": "bold",
                        "font-size": "larger",
                        "background-color": "#FFEFD5",
                    },
                ),
                dbc.Tab(
                    children=[html.Br(), tab4_content],
                    label="Accessibility heat map",
                    style={"padding": "10px"},
                    label_style={
                        "color": "#4682B4",
                        "font-weight": "bold",
                        "font-size": "larger",
                        "background-color": "#f4f6f6",
                    },
                    active_label_style={
                        "color": "#DC143C",
                        "font-weight": "bold",
                        "font-size": "larger",
                        "background-color": "#FFEFD5",
                    },
                ),
                dbc.Tab(
                    children=[html.Br(), tab5_content],
                    label="Network optimization",
                    style={"padding": "10px"},
                    label_style={
                        "color": "#4682B4",
                        "font-weight": "bold",
                        "font-size": "larger",
                        "background-color": "#f4f6f6",
                    },
                    active_label_style={
                        "color": "#DC143C",
                        "font-weight": "bold",
                        "font-size": "larger",
                        "background-color": "#FFEFD5",
                    },
                ),
            ]
        ),
    ]
)


app.layout = html.Div(
    [
        # dbc.Container([html.Br(), tabs]),
        tabs
    ]
)

# Set up callbacks/backend

# metrics
@app.callback(
    Output(component_id="card-01", component_property="children"),
    Input("player-widget", "value"),
    Input("stage-widget", "value"),
)
def metric_FG(player, stage):
    career_FG = df_metrics[
        (df_metrics["Player"] == player) & (df_metrics["Stage"] == stage)
    ]["career_FG_%"].iloc[0]
    return str(career_FG) + " %"


@app.callback(
    Output(component_id="card-02", component_property="children"),
    Input("player-widget", "value"),
    Input("stage-widget", "value"),
)
def metric_FT(player, stage):
    career_FT = df_metrics[
        (df_metrics["Player"] == player) & (df_metrics["Stage"] == stage)
    ]["career_FT_%"].iloc[0]
    return str(career_FT) + " %"


@app.callback(
    Output(component_id="card-03", component_property="children"),
    Input("player-widget", "value"),
    Input("stage-widget", "value"),
)
def metric_3PT(player, stage):
    career_3PT = df_metrics[
        (df_metrics["Player"] == player) & (df_metrics["Stage"] == stage)
    ]["career_3PT_%"].iloc[0]
    return str(career_3PT) + " %"


@app.callback(
    Output(component_id="card-04", component_property="children"),
    Input("player-widget", "value"),
    Input("stage-widget", "value"),
)
def metric_minutes(player, stage):
    avg_minutes = df_metrics[
        (df_metrics["Player"] == player) & (df_metrics["Stage"] == stage)
    ]["Minutes_per_game"].iloc[0]
    return str(avg_minutes) + " minutes"


@app.callback(
    Output(component_id="card-05", component_property="children"),
    Input("player-widget", "value"),
    Input("stage-widget", "value"),
)
def metric_FG(player, stage):
    career_FG = df_metrics[
        (df_metrics["Player"] == player) & (df_metrics["Stage"] == stage)
    ]["career_FG_%"].iloc[0]
    return str(career_FG) + " %"


@app.callback(
    Output(component_id="card-06", component_property="children"),
    Input("player-widget", "value"),
    Input("stage-widget", "value"),
)
def metric_FT(player, stage):
    career_FT = df_metrics[
        (df_metrics["Player"] == player) & (df_metrics["Stage"] == stage)
    ]["career_FT_%"].iloc[0]
    return str(career_FT) + " %"


@app.callback(
    Output(component_id="card-07", component_property="children"),
    Input("player-widget", "value"),
    Input("stage-widget", "value"),
)
def metric_3PT(player, stage):
    career_3PT = df_metrics[
        (df_metrics["Player"] == player) & (df_metrics["Stage"] == stage)
    ]["career_3PT_%"].iloc[0]
    return str(career_3PT) + " %"


@app.callback(
    Output(component_id="card-08", component_property="children"),
    Input("player-widget", "value"),
    Input("stage-widget", "value"),
)
def metric_minutes(player, stage):
    avg_minutes = df_metrics[
        (df_metrics["Player"] == player) & (df_metrics["Stage"] == stage)
    ]["Minutes_per_game"].iloc[0]
    return str(avg_minutes) + " minutes"


# tab 1
@app.callback(
    Output("chart-1", "srcDoc"),
    Input("player-widget", "value"),
    Input("stage-widget", "value"),
)
def plot_altair(xcol, ycol):
    chart = (
        alt.Chart(
            df_chart_1[(df_chart_1["Player"] == xcol) & (df_chart_1["Stage"] == ycol)]
        )
        .mark_bar()
        .encode(
            y=alt.Y("sum(Points_per_game)", title="Points"),
            x=alt.X("Season:O"),
            color=alt.Color(
                "Points_type", legend=alt.Legend(orient="bottom", title="")
            ),
            tooltip=["Player", "Stage", "Season", "Points_type", "Points_per_game"],
        )
        .properties(title="Average Points by Season", width=240, height=160)
    )
    return chart.to_html()


@app.callback(
    Output("chart-2", "srcDoc"),
    Input("player-widget", "value"),
    Input("stage-widget", "value"),
)
def plot_altair(xcol, ycol):
    chart = (
        alt.Chart(
            df_chart_2[(df_chart_2["Player"] == xcol) & (df_chart_2["Stage"] == ycol)]
        )
        .mark_line()
        .encode(
            y=alt.Y("Assists_per_game", title="Assists", scale=alt.Scale(zero=False)),
            x=alt.X("Season:O"),
            tooltip=["Player", "Stage", "Season", "Assists_per_game"],
        )
        .properties(title="Average Assists by Season", width=240, height=200)
    )
    return chart.to_html()


@app.callback(
    Output("chart-3", "srcDoc"),
    Input("player-widget", "value"),
    Input("stage-widget", "value"),
)
def plot_altair(xcol, ycol):
    chart = (
        alt.Chart(
            df_chart_3[(df_chart_3["Player"] == xcol) & (df_chart_3["Stage"] == ycol)]
        )
        .mark_bar()
        .encode(
            y=alt.Y("sum(Rebounds_per_game)", title="Rebounds"),
            x=alt.X("Season:O"),
            color=alt.Color(
                "Rebound_type", legend=alt.Legend(orient="bottom", title="")
            ),
            tooltip=["Player", "Stage", "Season", "Rebound_type", "Rebounds_per_game"],
        )
        .properties(title="Average Rebounds by Season", width=240, height=160)
    )
    return chart.to_html()


@app.callback(
    Output("chart-4", "srcDoc"),
    Input("player-widget", "value"),
    Input("stage-widget", "value"),
)
def plot_altair(xcol, ycol):
    chart = (
        alt.Chart(
            df_chart_4[(df_chart_4["Player"] == xcol) & (df_chart_4["Stage"] == ycol)]
        )
        .mark_line()
        .encode(
            y=alt.Y("per_game", title="Count"),
            x=alt.X("Season:O"),
            color=alt.Color(
                "Blocks/Steals", legend=alt.Legend(orient="bottom", title="")
            ),
            tooltip=["Player", "Stage", "Season", "Blocks/Steals", "per_game"],
        )
        .properties(title="Average Blocks & Steals by Season", width=430, height=160)
    )
    return chart.to_html()


@app.callback(
    Output("chart-5", "srcDoc"),
    Input("player-widget", "value"),
    Input("stage-widget", "value"),
)
def plot_altair(xcol, ycol):
    chart = (
        alt.Chart(
            df_chart_5[(df_chart_5["Player"] == xcol) & (df_chart_5["Stage"] == ycol)]
        )
        .mark_line()
        .encode(
            y=alt.Y("per_game", title="Count"),
            x=alt.X("Season:O"),
            color=alt.Color(
                "Turnovers/Fouls", legend=alt.Legend(orient="bottom", title="")
            ),
            tooltip=["Player", "Stage", "Season", "Turnovers/Fouls", "per_game"],
        )
        .properties(title="Average Turnovers & Fouls by Season")
    )
    return chart.to_html()


# tab 2
@app.callback(
    Output("chart-11", "srcDoc"),
    Input("player-widget-2", "value"),
    Input("stage-widget-2", "value"),
)
def plot_altair(xcol, ycol):
    chart = (
        alt.Chart(
            df_chart_11[
                (df_chart_11["Player"] == xcol) & (df_chart_11["Stage"] == ycol)
            ]
        )
        .mark_line()
        .encode(
            y=alt.Y(
                "per_game", title="Shooting Percentage", scale=alt.Scale(zero=False)
            ),
            x=alt.X("Season:O"),
            color=alt.Color(
                "2PT_3PT_eFG", legend=alt.Legend(orient="bottom", title="")
            ),
            tooltip=["Player", "Stage", "Season", "2PT_3PT_eFG", "per_game"],
        )
        .properties(
            title="Average Shooting Percentages by Season", width=430, height=160
        )
    )
    return chart.to_html()


@app.callback(
    Output("chart-12", "srcDoc"),
    Input("player-widget-2", "value"),
    Input("stage-widget-2", "value"),
)
def plot_altair(xcol, ycol):
    chart = (
        alt.Chart(
            df_chart_12[
                (df_chart_12["Player"] == xcol) & (df_chart_12["Stage"] == ycol)
            ]
        )
        .mark_bar()
        .encode(
            y=alt.Y(
                "True shooting percentage",
                title="True Shooting Percentage",
                scale=alt.Scale(zero=False),
            ),
            x=alt.X("Season:O"),
            tooltip=["Player", "Stage", "Season", "True shooting percentage"],
        )
        .properties(title="True Shooting Percentage by Season", width=430, height=190)
    )
    return chart.to_html()


# tab 3

## Callback Function for chart 1 (stats of teams/players over time)
@app.callback(Output("chart-21", "srcDoc"), Input("statistic-1", "value"))
def simple_stat(stat):
    # Use dictionary key to get the proper column
    leave_list = [
        "Average Player Minutes Played per Game",
        "Average Player Weight (lbs)",
        "Average Player Height (cm)",
        "Average Player Body Mass Index",
        "Ratio of Field Goals That Are 3-pointers",
    ]
    stat_label = stat_dict[stat]
    if stat not in leave_list:
        stat = stat + " per Team"
    line_chart = (
        alt.Chart(NBA_seasons_full, title=alt.TitleParams(text=stat))
        .mark_line(color="#f6573f", size=3)
        .encode(
            alt.Y(
                stat_label,
                scale=alt.Scale(zero=False),
                axis=alt.Axis(grid=False),
                title=None,
            ),
            alt.X("Season"),
        )
        .configure_view(strokeWidth=0)
        .properties(width=430, height=160)
    )
    return line_chart.to_html()


## Callback Function for chart 2 (stats of players by age)
@app.callback(Output("chart-22", "srcDoc"), Input("statistic-1", "value"))
def simple_stat_age(stat):
    # Use dictionary key to get the proper column
    stat_label = stat_dict[stat]
    line_chart = (
        alt.Chart(NBA_age_sum, title=alt.TitleParams(text=stat))
        .mark_line(color="#969696", size=3)
        .encode(
            alt.Y(
                stat_label,
                scale=alt.Scale(zero=False),
                axis=alt.Axis(grid=False),
                title=None,
            ),
            alt.X("Age", axis=alt.Axis(grid=False)),
        )
        .configure_view(strokeWidth=0)
        .properties(width=430, height=200)
    )
    return line_chart.to_html()


## Callback Function for chart 3 (stats in playoffs vs. regular season)
@app.callback(Output("chart-23", "srcDoc"), Input("statistic-1", "value"))
def type_stat(stat):
    # Use dictionary key to get the proper column
    leave_list = [
        "Average Player Minutes Played per Game",
        "Average Player Weight (lbs)",
        "Average Player Height (cm)",
        "Average Player Body Mass Index",
        "Ratio of Field Goals That Are 3-pointers",
    ]
    stat_label = stat_dict[stat]
    if stat not in leave_list:
        stat = stat + " per Team"
    stat = stat + " by Season"
    bar_chart = (
        alt.Chart(type_seasons)
        .mark_bar()
        .encode(
            alt.Y(
                stat_label,
                scale=alt.Scale(zero=False),
                axis=alt.Axis(grid=False),
                title=None,
                stack=None,
            ),
            alt.X("Type", axis=None),
            alt.Column(
                "Season",
                title=stat,
                header=alt.Header(labelOrient="bottom", labelAngle=90, labelPadding=60),
            ),
            alt.Color("Type", legend=alt.Legend(orient="bottom", title=None)),
        )
        .configure_facet(spacing=5)
        .configure_axis(grid=False)
        .configure_view(strokeWidth=0)
        .properties(width=42, height=145)
    )
    return bar_chart.to_html()


if __name__ == "__main__":
    app.run_server(debug=True)
