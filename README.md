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

### Week 9 ###

