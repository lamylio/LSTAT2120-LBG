# Mean, Sd, Skewness, Kurtosis
library(moments)
library(formattable) # Better print
library(ggplot2)
library(gridExtra)

compute.desc.quantitative = function(data){
  
  addStats = function(col, name){
    
    return(data.frame(
      Variable=name,
      Mean = round(mean(col, na.rm = T), opt.digits),
      Std.Deviation = round(sd(col), opt.digits),
      Skewness = round(skewness(col), opt.digits),
      Kurtosis = round(kurtosis(col), opt.digits)
    ))
  }
  
  for (c in colnames(data)){
    if (exists("results")){
      results = rbind(results, addStats(data[[c]], c))
    }else{
      results = addStats(data[[c]], c)
    }
  }
  
  results.order = order(as.character(results$Variable))
  results = results[results.order,]
  row.names(results) = NULL
  return (results)
  
}

# ---

draw.hist.boxplots = function(data, nrow=2, ncol=2, boxframe=T){
  
  
  layout(mat=matrix(seq(nrow*ncol*2), 2*nrow, 1*ncol, byrow=F), heights = rep(c(4,1.5), ncol))
  drawPlots = function(coname){
    
    par(mar=c(0, 2, 2, 2))
    hist(data[[coname]], axes=T, main=coname, col = opt.barcolor, xlab="", ylab="Count", xaxt="n")
    par(mar=c(3, 2.3, 0, 2.3))
    boxplot(data[[coname]], horizontal = T, frame=boxframe)
    
  }
  
  invisible(sapply(colnames(data), function (x) drawPlots(x)))
  
}