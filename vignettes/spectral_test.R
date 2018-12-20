library(ggplot2)
library(ggthemes)
library(lipdR)
library(geoChronR)


# load LiPD file
I = readLipd("ODP846.Lawrence.2006.lpd")
# extract relevant data 

I <- mapAgeEnsembleToPaleoData(I,which.pmt = 1)

temp <- selectData(I,varName = "temp",which.mt = 1)
age <- selectData(I,varName = "age",which.mt = 1)
ageEns <- selectData(I,varName = "ageEnsemble",which.mt = 1)


age.upper <- selectData(I, varName = "upper95",where = "chronData",tableType = "summary")
age.median <- selectData(I, varName = "median",where = "chronData",tableType = "summary")
age.lower <- selectData(I, varName = "lower95",where = "chronData",tableType = "summary")

age.ens  <- selectData(I, varName = "age",where = "chronData",tableType = "ensemble")



# put into dataframe
df <- data.frame(t = age$values, temp = temp$values)
#df = data.frame(t = age.median$values, temp = temp$values, upper=age.upper$values, lower=age.lower$values)
# plot
#ggplot(df) + geom_line(aes(x=t/1000,y=temp),colour="orange") + ggtitle("IODP 846 aligment to ProbStack, central estimate") + ylab("Temperature (deg C)") + scale_x_continuous(breaks=seq(0,5)) + xlab("Age (Ma)") + scale_x_reverse() + theme_hc(base_size = 12, base_family = "sans", style = "darkunica", bgcolor = NULL)

plotLine(X = age, Y  = temp,color = "orange") + theme_hc(base_size = 12, base_family = "sans", style = "darkunica", bgcolor = NULL) + ggtitle("IODP 846 aligment to ProbStack, central estimate")

# hone in on past 1 Ma 
dfs = dplyr::filter(df,t<=1000)

# put into matrix
values = dfs$d18O
#time = iodp846$alignment[1:382,]  

# apply geoChronR method
spec.ens.mtm <- computeSpectraEns(ageEns,temp,method='mtm')


#OLD WAY: load Matlab file
# library(R.matlab)
# path <- "/Users/julieneg/Documents/Science/Research/GeoChronR/spectral/data"
# pathname <- file.path(path, "HMM_Alignment_8461_v6.mat")
# iodp846 <- readMat(pathname)
# df = data.frame(t = iodp846$median.ali, d18O=iodp846$d18O, upper=iodp846$upper.95, lower=iodp846$lower.95,tens = iodp846$alignment)
# 
