# 1.1
da=read.table("d-3stocks9908.txt",header=T) # Load the data.
axp = da[,2]*100
cat = da[,3]*100
sbux = da[,4]*100
basicStats(axp)
axp_log = log(1+axp/100)*100
cat_log = log(1+cat/100)*100
sbux_log = log(1+sbux/100)*100
basicStats(axp_log)
t.test(axp_log)
t.test(cat_log)
t.test(sbux_log)
# 1.2
da=read.table("m-gm3dx7508.txt",header=T) # Load the data.
gm = da[,2]*100
vw = da[,3]*100
ew = da[,4]*100
sp = da[,4]*100
basicStats(gm)
gm_log = log(1+gm/100)*100
vw_log = log(1+vw/100)*100
ew_log = log(1+ew/100)*100
sp_log = log(1+sp/100)*100
basicStats(gm_log)
t.test(gm_log)
t.test(vw_log)
t.test(ew_log)
t.test(sp_log)
# 1.3
annnual_log_return = mean(sp_log)*12 # in percentage
value_invest = exp(sum(sp_log)/100)

# 1.4
# skewness
s1=skewness(axp_log)
t1_s=s1/sqrt(6/408) # Compute test statistic
t1_s #-2.774312
pv_s=2*(1-pnorm(abs(t1_s))) # Compute p-value.
pv_s #0.005531866
# kurtosis
k1=kurtosis(sibm)
t1_k=s1/sqrt(24/408) # Compute test statistic
t1_k #-1.387156
pv_k=2*(1-pnorm(abs(t1_k))) # Compute p-value.
pv_k #0.1653943

# 1.5
# candadian dollar-dollar
da=read.table("d-caus.txt",header=T) # Load the data.
caus = da[,4]
log_return_caus = log(caus[2:length(caus)]/caus[1:length(caus)-1])*100 # log return in percentage
basicStats(log_return_caus)
# us-uk
da=read.table("d-usuk.txt",header=T) # Load the data.
usuk = da[,4]
log_return_usuk = log(usuk[2:length(usuk)]/usuk[1:length(usuk)-1])*100 # log return in percentage
basicStats(log_return_usuk)
# jpy-us
da=read.table("d-jpus.txt",header=T) # Load the data.
jpus = da[,4]
log_return_jpus = log(jpus[2:length(jpus)]/jpus[1:length(jpus)-1])*100 # log return in percentage
basicStats(log_return_jpus)
# useu
da=read.table("d-useu.txt",header=T) # Load the data.
useu = da[,4]
log_return_useu = log(useu[2:length(useu)]/useu[1:length(useu)-1])*100 # log return in percentage
basicStats(log_return_useu)
hist(log_return_useu)