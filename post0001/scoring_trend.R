require(ggplot2)
w <- read.csv("../data/2013_weekly.csv")
totals <- data.frame(Points=w$QB+w$RB1+w$RB2+w$WR1+w$WR2+w$TE+w$TE+w$D+w$K+w$FLEX, Week=w$week, Team=w$team, Win=w$result)
g <- ggplot(totals, aes(x=Week, y=Points, group=Win, colour=Win)) + geom_point() + geom_smooth(method=lm,fullrange=TRUE) + scale_x_continuous(limits = c(2, 16)) + ggtitle( "2013 Scoring trend" )
png(filename="scoring_trend.png", width=600, height=300)
print(g)
dev.off()

