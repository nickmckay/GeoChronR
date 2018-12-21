library(geoChronR)
library(ggplot2)
library(ggthemes)
library(astrochron)

devtools::install_github("nickmckay/nuspectral")
library(nuspectral)

# generate synthetic colored noise with Milankovitch frequencies.
# method: Kirchner, J. W. (2005), Aliasing in 1/fα noise spectra: Origins, consequences, and remedies, Physical Review E, 71(6), 066,110–, doi:10.1103/PhysRevE.71.066110.
dt = 2
time = seq(dt,3000,by=dt)
nt = as.numeric(length(time))


nf = nt
y = matrix(nrow = nt, ncol = nf)
alpha = 1 # noise color
fs = 1/dt
f0 = fs/nt
theta = 2*pi*runif(nf)

for(k in 1:nf){
  y[,k] = (f0*k)^(-alpha/2)*sin(2*k*f0*time + theta[k])
}
ys = scale(rowSums(y))  # add up and scale
# add Milankovitch harmonics
periods = c(100,41,23,19)
amp = 1
np = length(periods)
yp = y = matrix(nrow = nt, ncol = np)
for(i in 1:np){
  yp[,i] = amp*sin(2*pi/periods[i]*time)
}

yp = rowSums(yp)  # add up

yt = ys + yp

# plot
p <- ggplot() + geom_line(aes(x=time,y=yt),colour="orange") + ggtitle("Colored Milankovitch noise") + ylab("d18O") + scale_x_continuous(breaks=seq(0,5)) + xlab("Age (ka)") + theme_hc(base_size = 12, base_family = "sans", style = "darkunica", bgcolor = NULL)
show(p)

# MTM
df = data.frame(t = time, y=yt)
fl = 0.005; fh = 0.1
spec.mtmPL <- astrochron::mtmPL(df,tbw=3, padfac=2,genplot = F,output=1, verbose = F,flow=fl, fhigh=fh)
sig.freqPL <- astrochron::mtmPL(df,tbw=3, padfac=2,genplot = F,output=2, verbose = F,flow=fl, fhigh=fh)
f = spec.mtmPL$Frequency

period_ticks= c(10, 20, 50, 100, 200, 500, 1000)
# Plot as a function of period
p.mtm1 <- ggplot() + geom_line(aes(x=1/f,y=spec.mtmPL$Power)) + geom_line(aes(x=1/f,y=spec.mtmPL$PowerLaw_fit),colour="blue")
p.mtm1 <- p.mtm1 + scale_x_continuous(breaks=period_ticks, trans=reverselog_trans(10)) 
p.mtm1 <- p.mtm1 + xlab("Period (kyr)") + ylab("Normalized Power") + scale_y_log10() + ggtitle("Colored Milankovitch noise, MTM, power-law null")
p.mtm1 <- PeriodAnnotate(p.mtm1,periods = 1/sig.freqPL)
show(p.mtm1)
#ggsave('colored_milankovitch_mtm_period_style',plot = p.mtm1, device = 'png')

# Plot as a function of frequency
p.mtm2 <- ggplot() + geom_line(aes(x=f,y=spec.mtmPL$Power)) + xlab("Frequency (1/kyr)") + ylab("Normalized Power") + scale_x_log10() +scale_y_log10() + ggtitle("Colored Milankovitch noise, MTM, power-law null")
p.mtm2 <-FrequencyAnnotate(p.mtm2,periods = 1/sig.freqPL) +  geom_line(aes(x=f,y=spec.mtmPL$PowerLaw_fit),colour="blue")
show(p.mtm2)

#ggsave('colored_milankovitch_mtm_freq_style.png',plot = p.mtm2, device = 'png')


# WWZ method
tau = seq(min(time),max(time),length = max(nt %/% 2,5))
spec.wwz = nuspectral::nuwavelet_psd(time,yt,sigma=0.05,taus = )
period_range =  c(10, 1000) 
freq = spec.wwz$Frequency
f.low = 1/period_range[2]
f.high = 1/period_range[1]
freq_range = (freq>= f.low & freq<=f.high)


# add power-law fit
dof = 2
specIn = data.frame(cbind(spec.wwz$Frequency, spec.wwz$Power))
resFit = pwrLawFit(specIn, dof = dof, flow = f.low, fhigh = f.high, output = 1, genplot = F)
pwrLawFreq = resFit[, 1]
pwrLawLine = resFit[, 4]
# estimate range
m <- floor(log10(min(spec.wwz$Power[freq_range]))) 
M <- ceiling(log10(max(spec.wwz$Power[freq_range]))) 

p.wwz <- ggplot() + geom_line(aes(x=1/spec.wwz$Frequency,y=spec.wwz$Power)) #+ geom_line(aes(x=1/f,y=spec.mtmPL$PowerLaw_fit),colour="blue")
p.wwz <- p.wwz + scale_x_continuous(breaks=period_ticks, trans=reverselog_trans(10), limits = rev(period_range)) 
p.wwz <- p.wwz + xlab("Period (kyr)") + ylab("Normalized Power") + ggtitle("Colored Milankovitch noise, MTM, power-law null")
p.wwz <- PeriodAnnotate(p.wwz,periods = periods) + scale_y_log10() #+ scale_y_log10(limits = c(10^m,10^M)) 
p.wwz <- p.wwz +  geom_line(aes(x=1/pwrLawFreq,y=pwrLawLine),colour="blue")
show(p.wwz)
ggsave('colored_milankovitch_wwz_period_style',plot = p.wwz, device = 'png')





show(p.wwz)



