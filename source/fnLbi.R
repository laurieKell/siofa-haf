lfm<-function(lc,linf) 0.75*lc+0.25*linf

lmax<-function(x){
  rtn=reshape2::melt(aaply(x,2:6, function(x) rev(names(x[x>0&!is.na(x)]))[1]))
  names(rtn)[dim(rtn)[2]]="data"
  rtn$data=an(ac(rtn$data))
  as.FLQuant(rtn)}

boot<-function(x,nits){
  
  rtn=adply(propagate(x,nits),c(2:6), function(x){  
    reshape2::melt(table(sample(names(x), size=sum(x), replace=TRUE, prob=x)),
                   value.name="data")})    
  rtn=as.FLQuant(rtn)
  names(dimnames(rtn))[1]=names(x)[1]
  rtn[is.na(rtn)]=0
  rtn}

l50<-function(model, sex, Type) {
  # Extract coefficients from the model
  coefs     =coef(model)
  intercept =coefs['(Intercept)']
  coefLen   =coefs['len']
  
  # Adjust coefficients based on sex
  sexEffect =ifelse(sex == "Male", coefs['sexMale'], 0)
  lenSex    =ifelse(sex == "Male", coefs['len:sexMale'], 0)
  
  # Adjust coefficients based on Type
  typeEffect=ifelse(Type == "Morph", coefs['len:TypeMorph'], 0)
  
  # Calculate the adjusted coefficient for len
  coefLen  =coefLen + lenSex + typeEffect
  
  # Calculate the length at maturity=0.5
  l50      =-(intercept + sexEffect) / coefLen
  
  return(l50)}

