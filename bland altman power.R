####################################################################
#
# Bland Altman power - based on Martin Bland FAQs. (https://www-users.york.ac.uk/~mb55/meas/sizemeth.htm)
#
####################################################################

#created by Paul Thompson
#25-05-2017

#Adapted code from the 'BlandAltmanLeh' package, https://cran.r-project.org/web/packages/BlandAltmanLeh/index.html

### simulate data and estimate the SE of the 95% limit of agreeement, 
### then calc the proportion of runs than are smaller than the max error of the 95% limit of agreement.

pwr_blandAlt<-function() 
{
  N=40
  conf.int = 0.95
 
  group1 <- rnorm(N,1,1)
  group2 <- group1 + runif(N, -.3, .3)
  
  dfr <- data.frame(group1=group1,group2=group2) 

  called.with <- length(group1)
  
  based.on <- length(dfr[[1]])

  diffs <- dfr[[1]] - dfr[[2]]

  means <- (dfr[[1]] + dfr[[2]])/2
  
  critical.diff <- 1.96 * sd(diffs)
  
  mean.diffs <- mean(diffs)
  lower.limit <- mean.diffs - critical.diff
  upper.limit <- mean.diffs + critical.diff
  
  lines <- c(lower.limit = lower.limit, mean.diffs = mean.diffs, upper.limit = upper.limit)
  
  t1 <- qt((1 - conf.int)/2, df = based.on - 1)
  t2 <- qt((conf.int + 1)/2, df = based.on - 1)
  
  max.low <- lower.limit + t1 * sqrt(sd(diffs)^2 * 3/based.on)
  max.up <- upper.limit + t2 * sqrt(sd(diffs)^2 * 3/based.on)
  
  CI.lines <- c(lower.limit.ci.lower = lower.limit + t1 * sqrt(sd(diffs)^2 *3/based.on), 
                lower.limit.ci.upper = lower.limit + t2 * sqrt(sd(diffs)^2 * 3/based.on), 
                mean.diff.ci.lower = mean.diffs + t1 * sd(diffs)/sqrt(based.on), 
                mean.diff.ci.upper = mean.diffs + t2 * sd(diffs)/sqrt(based.on), 
                upper.limit.ci.lower = upper.limit + t1 * sqrt(sd(diffs)^2 * 3/based.on), 
                upper.limit.ci.upper = upper.limit + t2 * sqrt(sd(diffs)^2 * 3/based.on))
  
  ############################################################################################
  # values <- data.frame(m = means, d = diffs)
  #
  # p<- ggplot2::ggplot(values, ggplot2::aes(x = m, y = d)) + 
  #     ggplot2::geom_point() + 
  #     ggplot2::geom_hline(yintercept = lines, linetype = 2, size = 1) + 
  #     ggplot2::xlab("mean of measurements") + 
  #     ggplot2::ylab("difference") + 
  #     ggplot2::geom_hline(yintercept = CI.lines, linetype = 2, size = 0.7)
  # 
  ############################################################################################
  
return(sqrt(sd(diffs)^2 * 3/based.on))
}

res<-t(replicate(100,pwr_blandAlt()))

       
maxdiff=.05

pwr1<-mean(res<maxdiff)    
pwr1   
