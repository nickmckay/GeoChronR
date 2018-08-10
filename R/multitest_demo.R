# generate example series with periods of 400 ka, 100 ka, 40 ka and 20 ka
ex = cycles(freqs=c(1/400,1/100,1/40,1/20),start=1,end=1000,dt=5)

# add AR1 noise
noise = ar1(npts=200,dt=5,sd=.5)
ex[2] = ex[2] + noise[2]

# first, let's look at mtm with conventional AR1 background
spec=mtm(ex,padfac=1,ar1=TRUE,output=1)

# when blindly prospecting for cycles, it is necessary to consider all of the 
#  observed frequencies in the test
multiTest(cb(spec,c(1,4)))

# if, a priori, you are only concerned with the Milankovitch frequency bands, 
#  restrict your analysis to those bands (as constrained by available sedimentation
#  rate estimates and the frequency resolution of the spectrum). in the example below, 
#  the mtm bandwidth resolution is employed to search frequencies nearby the 
#  Milankovitch-target periods.
flow=c((1/400)-0.003,(1/100)-0.003,(1/41)-0.003,(1/20)-0.003)
fhigh=c((1/400)+0.003,(1/100)+0.003,(1/41)+0.003,(1/20)+0.003)
multiTest(cb(spec,c(1,4)),flow=flow,fhigh=fhigh)

# now try with the lowspec method. this uses prewhitening, so it has one less data point.
spec=lowspec(ex,padfac=1,output=1)
flow=c((1/400)-0.003015075,(1/100)-0.003015075,(1/41)-0.003015075,(1/20)-0.003015075)
fhigh=c((1/400)+0.003015075,(1/100)+0.003015075,(1/41)+0.003015075,(1/20)+0.003015075)
multiTest(cb(spec,c(1,4)),flow=flow,fhigh=fhigh)

# compare with BFS
