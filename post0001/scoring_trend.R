require(ggplot2)
require(colorspace)

w <- read.csv("../data/2013_weekly.csv")
totals <- data.frame(Points=w$QB+w$RB1+w$RB2+w$WR1+w$WR2+w$TE+w$D+w$K+w$FLEX, Week=w$week, Team=w$team, Win=w$result)
g <- ggplot(totals, aes(x=Week, y=Points, group=Win, colour=Win)) + geom_point() + geom_smooth(method=lm,fullrange=TRUE) + scale_x_continuous(limits = c(2, 16)) + ggtitle( "Route 18's Finest - 2013 Scoring trend (by W/L)" )
png(filename="scoring_trend.png", width=600, height=300)
print(g)
dev.off()

colours <- c("#2121D9", "#9999FF", "#D92121", "#21D921", "#FFFF4D", "#FF9326")
ltype <- c(rep(1,6),rep(2,6))

g <- ggplot(totals, aes(x=Week, y=Points, group=Team, colour=Team)) + geom_point() + geom_smooth(method=lm,fullrange=TRUE,se=FALSE) + scale_x_continuous(limits = c(2, 16)) + ggtitle( "Route 18's Finest - 2013 Scoring trend (by team)" ) + scale_fill_manual(values=colours)
png(filename="scoring_trend_team.png", width=600, height=300)
print(g)
dev.off()




print(summary(lm(totals$Points ~ 1 + totals$Week, subset=which(totals$Win=="W"))))
print(summary(lm(totals$Points ~ 1 + totals$Week, subset=which(totals$Win=="L"))))
