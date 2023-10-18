### Marginal effects ###
marginsplotdf.cont <-function(model, xterm, zterm, zseq){
  coefs<-coef(model)
  cov<-vcov(model)
  intterm<-ifelse(is.na(coefs[paste(xterm,zterm,sep=":")]),paste(zterm,xterm,sep=":"),paste(xterm,zterm,sep=":"))
  dy.dx<-coefs[xterm]+coefs[intterm]*zseq
  se.dy.dx<-sqrt(cov[xterm,xterm]+zseq^2*cov[intterm,intterm]+zseq*2*cov[xterm,intterm])
  CI <- data.frame(lwr95 = dy.dx-(1.96*se.dy.dx),
                   lwr90 = dy.dx-(1.68*se.dy.dx),
                   upr90 = dy.dx+(1.68*se.dy.dx),
                   upr95 = dy.dx+(1.96*se.dy.dx))
  
  margins<-data.frame(z=zseq,dydx=dy.dx,se=se.dy.dx, CI)
  return(margins)
}

### Marginal effects with factor variable ###
marginsplotdf.factor <- function(mod, xvar, zvar, z_seq, data){
  df <- data.frame(z = z_seq,
                   dydx = NA, se = NA,
                   lwr95 = NA, lwr90 = NA,
                   upr90 = NA, upr95 = NA)
  for(i in z_seq){
    if(i == min(z_seq)){
      df[df$z == i,-1] <- marginsplotdf(model = mod, xterm = xvar, zterm = paste("factor(",zvar,")",z_seq[2], sep = ""), zseq = c(0,1))[1,-1]
    }else{
      df[df$z == i,-1] <- marginsplotdf(model = mod, xterm = xvar, zterm = paste("factor(",zvar,")",i, sep = ""), zseq = c(0,1))[2,-1]
    }
  }
  return(df)
}

