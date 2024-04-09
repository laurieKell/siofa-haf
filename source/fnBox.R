#summary boxplot function
fnBox<-function(x) {
  r=quantile(x, probs=c(0.05, 0.33, 0.5, 0.66, 0.95))
  names(r)=c("ymin", "lower", "middle", "upper", "ymax")
  r}