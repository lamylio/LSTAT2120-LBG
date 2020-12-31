models.comparison = data.frame()

addComparison = function(model, model_name){
  to_combine = data.frame(
    Variable = as.character(model_name),
    Count = length(names(coef(model)))-1,
    LogLik = ifelse(is.null(logLik(model)[1]), yes=NA, no=round(logLik(model)[1], opt.digits)),
    AIC = round(AIC(model), opt.digits),
    #BIC = round(BIC(model), opt.digits),
    R2 = round(summary(model)$r.squared, opt.digits),
    Adjusted.R2 = round(summary(model)$adj.r.squared, opt.digits),
    Deviance = ifelse(is.null(deviance(model)), yes=NA, no=round(deviance(model), opt.digits))
    #Deviance.custom = cost_deviance(base_train_freq$Frequency, model$fitted),
    #row.names = as.character(model_name)
  )
  rbind(
    models.comparison,
    to_combine
  )
}

changePlot = function(p){
  p$layers[[5]] = NULL
  p$layers[[4]] = NULL
  p$layers[[3]]$aes_params$colour = opt.barcolor
  p$layers[[1]]$aes_params$colour = opt.barcolor
  p$layers[[2]]$aes_params$colour = "black"
  p$layers[[3]]$aes_params$shape = 16
  return (p)
}