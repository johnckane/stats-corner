stats_corner
============

R code I use to do analysis and create graphics for my fantasy football league.

Here is a file description by week that I produced them.

### Week 5 ###
"simple stats.R" : this file arranges the league history summary post. League records such as most points in a single game, single season, playoff appearance by owner and owner winning percentages.

### Week 6 ###
"winning percentage by points scored.R" : determines percentage of times a team won when scoring a certain amount of points. Points are binned into groups so the analysis is done on discrete rather than continuous values. Also looks at how this changes if it is an NFL bye-week because on bye-weeks some high scoring players will not be playing.

"playoff rate by record.R" : generates two plots. The first is a grid/heat map that shows the proportion of teams after any given week with a given amount of wins that have made the playoffs. There are two versions, the first is overall record, which is messy due to some ties and our small sample size. The second is based just on wins and is much cleaners. The second plot is sample size for each grid in the wins and playoff rate heat map. 

### Week 7 ###
"projected vs actual over time.R" : I kept on a spreadsheet the values of acutal points, projected points, minutes remaining and time of day for each game. These are plots of that data.

### Week 8 ###
"PA analysis.R" : file to investigate historically bad "defenses". That is teams who have had the most points scored against them, which of course they can't control. 

"head to head records.R" : file to produce grid of individual owners' head-to-head records against each other. The final product is a matrix with null values down the diagnol displayed in a ggplot2 graph.

"strength of schedule.R": file that calculates both career strength of schedule and season-by-season ranks

"proportional wins.R" : generic file to pull and calculate proportional wins. Will also summarize by owner and by year.

### Week 9 ###
"high scoring weeks.R" : file that ranks weeks by average points leaguewide. Also, determines how a certain week's score ranks in each owner's individual career long body of work.

### Week 10 ###
"pull data.R" : file that pulls projection data from ESPN and actual scores from pro-football-reference.com. The end of the file writes an output dataset to directory specified.

"exploratory analysis.R" : file that explores the differences between actual and projected points. Graphs are more crude, not all calculations here made the final post, etc.

"final graphs and analysis.R" : the final graphs and calculations that made it into the post are more neatly organized here. 

### Week 11 ###
"power rankings.R" : no explicit Stats Corner post this week. I incorporated some things into my Power Rankings. Namely, owner specific graphs. All plots and stats are detailed here. 

"harrington plot.R" : file that calculates SOS as determined by opponent's proportional wins. Plots them against total wins (raw wins, not proportional wins).


### Week 12 ###
"pw and projections" : file that uses proportional wins for last five games as well as the whole season to project final standings. 

### Week 13 ###
"transactions" : file that analyzes 12 of 13 weeks of the 2014 season and the transaction counter to see relationship between performance and trades, FA signings and starting lineup substitutions. 

"transactions_league_history" : file that analyzes complete regular seasons 2009 - 2013 as well as through Week 12 2014 for relationship between performance and trades only. 

### Round 1 ###
"playoff_preview.R" : file that calculates bye-weeks by owner, creates plot of winner's bracket points scored in relation to regular season performance, an unused graph of playoff win/loss status by both regular season and last 6 game proportional wins, and line plot of each of the first round match-ups regular season point totals.

### Round 2 ###
"round2_preview.R" : file that creates plot of Round 1 Winner's Bracket projected and actual points by both calendar time and minutes remaining, overall playoff performance as determined by proportional wins and not championships, preview this week's matchups and calculate some head-to-head stats for winner's bracket games. Also bar charts for each winner's bracket matchup series history. 

### Round 3 ###
"round3_preview.R" : file that creates plot of Round 2 Winner's Bracket projected and actual points by both calendar time and minutes remaining, calculated Winner's Bracket W-L records by owner, and preview this week's matchups and some stats for the Championship matchup and SAT Bowl.


