require(ggplot2)
require(colorspace)

#w <- read.csv("../data/2013_weekly.csv")
wfull <- read.csv("../data/2013_weekly.csv")
w <- wfull[ which(wfull$week < 14), ] 

ranks <- c("john","frank","brian","nate","owen","pete","anthony","dave","ryan","steve","fred","jeff")
w$team <- factor(w$team, levels=ranks)

totals <- data.frame(Points=w$QB+w$RB1+w$RB2+w$WR1+w$WR2+w$TE+w$D+w$K+w$FLEX, Week=w$week, Team=w$team, Result=w$result)
                  

g <- ggplot(totals, aes(x=Week, y=Points, group=Result, colour=Result)) + geom_point() + geom_smooth(method=lm,fullrange=TRUE) + scale_x_continuous(limits = c(1, 16)) + ggtitle( "Route 18's Finest - 2013 Scoring trend (by W/L)" )
png(filename="scoring_trend.png", width=600, height=300)
print(g)
dev.off()


cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7")
cbPalette <- c(cbPalette, cbPalette)


g2 <- ggplot(totals, aes(x=Week, y=Points, group=Team, colour=Team, linetype=Team, shape=Team )) + geom_point() + scale_shape_manual(values=c(rep(1,6),rep(2,6)) ) + scale_colour_manual(values=cbPalette) + geom_smooth(method="lm",fullrange=TRUE,se=FALSE,size=1) + scale_linetype_manual(values=c(rep("solid",6),rep("dashed",6)) )  + scale_x_continuous(limits = c(1, 16)) + ggtitle( "Route 18's Finest - 2013 Scoring trend (by team)" )
png(filename="scoring_trend_team.png", width=600, height=300)
print(g2)
dev.off()

print( "made figures" )
#print(summary(lm(totals$Points ~ 1 + totals$Week, subset=which(totals$Win=="W"))))
#print(summary(lm(totals$Points ~ 1 + totals$Week, subset=which(totals$Win=="L"))))

xw <- summary(lm(totals$Points ~ 1 + totals$Week, subset=which(totals$Result=="W")))
xl <- summary(lm(totals$Points ~ 1 + totals$Week, subset=which(totals$Result=="L")))

pw <- pf(xw$fstatistic[1], xw$fstatistic[2], xw$fstatistic[3], lower.tail=FALSE)
pl <- pf(xl$fstatistic[1], xl$fstatistic[2], xl$fstatistic[3], lower.tail=FALSE)

print( paste("win",pw))
print( paste("lose",pl))

for( team in ranks ) {
  tx <- which(totals$Team==team)
  pts <- totals$Points[tx]
  week <- totals$Week[tx]
  
  lx <- lm(pts ~ 1 + week)
  x <- summary(lx)
  pval <- pf(x$fstatistic[1], x$fstatistic[2], x$fstatistic[3], lower.tail=FALSE)
  #print( paste(team, pval) )

  ftr <- data.frame(week=c(14,15,16))
  preds <- predict(lx,ftr, se.fit=TRUE)

  print( paste( team, "14", preds$fit[1], "+/-", preds$se.fit[1] ) )
}

totals2 <- data.frame(Points=wfull$QB+wfull$RB1+wfull$RB2+wfull$WR1+wfull$WR2+wfull$TE+wfull$D+wfull$K+wfull$FLEX, Week=wfull$week, Team=wfull$team, Result=wfull$result)

playoffs1 <- c("john","frank","brian","nate") 

for( team in playoffs1 ) {
  tx <- which(totals2$Team==team)
  pts <- totals2$Points[tx]
  week <- totals2$Week[tx]
  
  lx <- lm(pts ~ 1 + week)
  x <- summary(lx)
  pval <- pf(x$fstatistic[1], x$fstatistic[2], x$fstatistic[3], lower.tail=FALSE)
  #print( paste(team, pval) )

  ftr <- data.frame(week=c(14,15,16))
  preds <- predict(lx,ftr, se.fit=TRUE)

  print( paste( team, "15", preds$fit[2], "+/-", preds$se.fit[2] ) )
                 
}


