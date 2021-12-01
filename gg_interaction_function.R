gg_interaction <- function(x, y, random=NULL, data){
  # x is a vector of the column labels of categorical variables
  # y is the response column
  # random is a column name of a blocking factor
  # data is a data.frame or data.table
  dt <- data.table(data)
  fixed_part <- paste(y, "~", paste(x[1], x[2], sep="*"))
  if(is.null(random)){ # linear model
    lm_formula <- formula(fixed_part)
    fit <- lm(lm_formula, data=dt)
  }else{ ## linear mixed model
    random_part <- paste("(1|", random, ")", sep="")
    lmm_formula <- formula(paste(fixed_part, random_part, sep=" + "))
    fit <- lmer(lmm_formula, data=dt)
  }
  fit.emm <- data.table(summary(emmeans(fit, specs=x)))
  new_names <- c("f1", "f2")
  setnames(fit.emm, old=x, new=new_names)
  pd <- position_dodge(.3)
  gg <- ggplot(data=fit.emm, aes(x=f1, y=emmean, shape=f2, group=f2)) +
    #geom_jitter(position=pd, color='gray', size=2) +
    geom_point(color='black', size=4, position=pd) +
    geom_errorbar(aes(ymin=(emmean-SE), ymax=(emmean+SE)), 
                  color='black', width=.2, position=pd) +
    geom_line(position=pd) +
    xlab(x[1]) +
    ylab(y) +
    theme_bw() +
    guides(shape=guide_legend(title=x[2])) +
    theme(axis.title=element_text(size = rel(1.5)),
          axis.text=element_text(size = rel(1.5)),
          legend.title=element_text(size = rel(1.3)),
          legend.text=element_text(size = rel(1.3))) +
    NULL
  return(gg)
}
