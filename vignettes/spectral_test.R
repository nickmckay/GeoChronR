library(ggplot2)
library(ggthemes)
library(geoChronR)


# test case on synthetic colored noise with Milankovitch frequencies.
time = seq(0,1000,by=2)
nt = length(time)
m = nt %/% 4
y = matrix(nrow = nt, ncol = m)
alpha = 0.7 # noise color
f0 = 1/nt
theta = 2*pi*runif(m)
for(j in 1:nt){
  for(k in 1:m){
    y[j,k] = (f0*k)^(-alpha/2)*sin(2*k*f0*time[j] + theta[k])
  }
}

ys = rowSums(y)

periods = c(100,41,23,19)
kp = nt %/% periods
amp = 30

for(i in 1:4){ 
  ys = ys + amp*sin(2*kp[i]*f0*time)
}

ys = scale(ys) 

p <- ggplot() + geom_line(aes(x=time,y=ys),colour="orange") + ggtitle("Colored Milankovitch noise") + ylab("d18O") + scale_x_continuous(breaks=seq(0,5)) + xlab("Age (ka)") + scale_y_reverse() + theme_hc(base_size = 12, base_family = "sans", style = "darkunica", bgcolor = NULL)
show(p)


spec.wwz = nuwavelet_psd(time,ys,sigma=0.05)

orb.freq = 
  dtm  = median(diff(time))
# plot this 
p.wwz <- ggplot() + geom_line(aes(x=spec.wwz$Frequency,y=spec.wwz$Power),colour="orange") + xlab("Frequency (1/kyr)") + ylab("Normalized Power") + scale_x_log10() +scale_y_log10() + ggtitle("Colored Milankovitch noise, WWZ") + theme_hc(base_size = 12, base_family = "sans", style = "darkunica", bgcolor = NULL)
p.wwz <- geoChronR::plotSpectraAnnotate(p.wwz,periods = periods)
show(p.wwz)



