## init
library(moments)
#library(e1071)
Sys.setlocale("LC_ALL", "Czech")

## load data
csv_data = read.csv2(file="ukol_15.csv")

## take Windows 10 data
win10 = csv_data[,1:3]
colnames(win10) = c("id", "2015", "2016")

## remove far measurements
bplot = boxplot(win10[["2016"]], plot=FALSE)
w2016_noout = win10[["2016"]][!(win10[["2016"]] %in% bplot$out)]

shapiro.test(w2016_noout)
qqnorm(win10[["2015"]],
    main="",
    xlab="Teoretické kvantily normálního rozdělení",
    ylab="Výběrové kvantily",
    ylim=c(0, 1300)
  )
  qqline(win10[["2015"]],
    col="red"
  )

## data
getSummary = function(data)
{
  c(summary(data), sd(data), sd(data) / mean(data), skewness(data), kurtosis(data) - 3)
}

plotDist = function(data)
{
  data = sort(data)
  s = sum(data)
  plot(data, cumsum(data) / s)
  lines(data, pnorm(data, mean=mean(data), sd=sd(data)))
}

## plot data
plotData = function(data, hist_lim_y=c(0, 0.002), hist_lim_x=c(0, 1200), round_digits=2, saveImage=FALSE)
{
  if (saveImage != FALSE)
  {
    png(filename=saveImage, width=1750, height=1850, antialias="default", res=330, pointsize=9)
  }

  lay = layout(mat = matrix(c(1, 2, 3),3,1, byrow=TRUE), heights=c(0.3,0.3,0.5))
  par.default = par(oma=rep(0, 4),mar=c(4, 5, 2, 0.5))
  
  fontsize.lab = 1.4
  fontsize.axis = 1.1
  fontsize.text = 1.1
  fontsize.legend = 1.2

  boxplot(data,
    horizontal=TRUE,
    xlab="Odehrané hodiny",
    ylim=hist_lim_x,
    names=c("asd"),
    cex.lab=fontsize.lab,
    cex.axis=fontsize.axis
  )
  stats = signif(fivenum(data), round_digits)
  text(x = (stats + 30), labels = stats, y = 1.25, cex=fontsize.text)
  
  mean_val = signif(mean(data), round_digits)
  abline(v = mean_val, col="red", lty=2)
  text(x = mean_val + 30, labels = c(mean_val), y = 0.6, col="red", cex=fontsize.text)
  
  legend("topleft", "průměr", col="red", bty="n", lty=2,
    pt.cex=1, cex=fontsize.legend
  )

  hist(data,
    main="",
    xlab="Odehrané hodiny",
    ylab="Hustota pravděpodobnosti",
    ylim=hist_lim_y,
    xlim=hist_lim_x,
    freq=FALSE,
    breaks=20,
    cex.lab=fontsize.lab,
    cex.axis=fontsize.axis
  )
  box(which = "plot", lty = "solid")
  lines(density(data),
    col="blue"
  )
  xData = min(data):max(data)
  lines(xData, dnorm(xData, mean=mean(data), sd=sd(data)),
    col="red"
  )
  legend("topleft", c("hustota pravd. normálního rozdělení", "hustota pravd. výběru"),
    col=c("red", "blue"), bty="n", lty=c(1, 1),
    pt.cex=1, cex=fontsize.legend
  )
  
  qqnorm(data,
    main="",
    xlab="Teoretické kvantily normovaného normálního rozdělení",
    ylab="Výběrové kvantily",
    xlim=c(-2, 2),
    cex.lab=fontsize.lab,
    cex.axis=fontsize.axis
  )
  legend("topleft", "dist. f-ce normovaného normálního rozdělení", col="red", bty="n", lty=1,
    pt.cex=1, cex=fontsize.legend
  )
  qqline(data,
    distribution=qnorm,
    col="red"
  )
  
  if (saveImage != FALSE)
  {
    #dev.cur()
    #dev.print(device = png, file=saveImage, width=20, height=15)
    dev.off()
  }
  
  par(par.default)
}

plotData(win10[["2015"]], c(0, 0.0025), c(200, 1200), 2, "2015.png")
plotData(w2016_noout, c(0, 0.0025), c(200, 1450), 2, "2016.png")