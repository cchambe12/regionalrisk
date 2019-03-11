### 25 Feb 2019 - make some output files for plotting interactions!
## Libraries
options(stringsAsFactors = FALSE)

library(brms)
library(ggeffects)

load("/n/wolkovich_lab/Lab/Cat/orig_full.Rdata")

naosp<- ggpredict(orig.full, terms = c("nao.z", "species"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/naosp_predicted_90.csv", row.names = FALSE)

matsp<- ggpredict(orig.full, terms = c("mat.z", "species"), ci.lvl=0.9) 
write.csv(matsp, file="/n/wolkovich_lab/Lab/Cat/matsp_predicted_90.csv", row.names = FALSE)

distsp<- ggpredict(orig.full, terms = c("dist.z", "species"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/distsp_predicted_90.csv", row.names = FALSE)

elevsp<- ggpredict(orig.full, terms = c("elev.z", "species"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/elevsp_predicted_90.csv", row.names = FALSE)

ccsp<- ggpredict(orig.full, terms = c("cc.z", "species"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/ccsp_predicted_90.csv", row.names = FALSE)


##############################################################################################<
##############################################################################################<

load("/n/wolkovich_lab/Lab/Cat/dvr_full.Rdata")

naosp<- ggpredict(dvr.full, terms = c("nao.z", "species"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/naosp_predicted_90_dvr.csv", row.names = FALSE)

matsp<- ggpredict(dvr.full, terms = c("mat.z", "species"), ci.lvl=0.9) 
write.csv(matsp, file="/n/wolkovich_lab/Lab/Cat/matsp_predicted_90_dvr.csv", row.names = FALSE)

distsp<- ggpredict(dvr.full, terms = c("dist.z", "species"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/distsp_predicted_90_dvr.csv", row.names = FALSE)

elevsp<- ggpredict(dvr.full, terms = c("elev.z", "species"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/elevsp_predicted_90_dvr.csv", row.names = FALSE)

ccsp<- ggpredict(dvr.full, terms = c("cc.z", "species"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/ccsp_predicted_90_dvr.csv", row.names = FALSE)


##############################################################################################<
##############################################################################################<
  
load("/n/wolkovich_lab/Lab/Cat/five_full.Rdata")

naosp<- ggpredict(five.full, terms = c("nao.z", "species"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/naosp_predicted_90_five.csv", row.names = FALSE)

matsp<- ggpredict(five.full, terms = c("mat.z", "species"), ci.lvl=0.9) 
write.csv(matsp, file="/n/wolkovich_lab/Lab/Cat/matsp_predicted_90_five.csv", row.names = FALSE)

distsp<- ggpredict(five.full, terms = c("dist.z", "species"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/distsp_predicted_90_five.csv", row.names = FALSE)

elevsp<- ggpredict(five.full, terms = c("elev.z", "species"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/elevsp_predicted_90_five.csv", row.names = FALSE)

ccsp<- ggpredict(five.full, terms = c("cc.z", "species"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/ccsp_predicted_90_five.csv", row.names = FALSE)


##############################################################################################<
##############################################################################################<

naosp<- ggpredict(orig.full, terms = c("nao.z", "cc.z"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/naocc_predicted_90.csv", row.names = FALSE)

matsp<- ggpredict(orig.full, terms = c("mat.z", "cc.z"), ci.lvl=0.9) 
write.csv(matsp, file="/n/wolkovich_lab/Lab/Cat/matcc_predicted_90.csv", row.names = FALSE)

distsp<- ggpredict(orig.full, terms = c("dist.z", "cc.z"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/distcc_predicted_90.csv", row.names = FALSE)

elevsp<- ggpredict(orig.full, terms = c("elev.z", "cc.z"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/elevcc_predicted_90.csv", row.names = FALSE)

##############################################################################################<
##############################################################################################<
naosp<- ggpredict(dvr.full, terms = c("nao.z", "cc.z"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/naocc_predicted_90_dvr.csv", row.names = FALSE)

matsp<- ggpredict(dvr.full, terms = c("mat.z", "cc.z"), ci.lvl=0.9) 
write.csv(matsp, file="/n/wolkovich_lab/Lab/Cat/matcc_predicted_90_dvr.csv", row.names = FALSE)

distsp<- ggpredict(dvr.full, terms = c("dist.z", "cc.z"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/distcc_predicted_90_dvr.csv", row.names = FALSE)

elevsp<- ggpredict(dvr.full, terms = c("elev.z", "cc.z"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/elevcc_predicted_90_dvr.csv", row.names = FALSE)

##############################################################################################<
##############################################################################################<

naosp<- ggpredict(five.full, terms = c("nao.z", "cc.z"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/naocc_predicted_90_five.csv", row.names = FALSE)

matsp<- ggpredict(five.full, terms = c("mat.z", "cc.z"), ci.lvl=0.9) 
write.csv(matsp, file="/n/wolkovich_lab/Lab/Cat/matcc_predicted_90_five.csv", row.names = FALSE)

distsp<- ggpredict(five.full, terms = c("dist.z", "cc.z"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/distcc_predicted_90_five.csv", row.names = FALSE)

elevsp<- ggpredict(five.full, terms = c("elev.z", "cc.z"), ci.lvl=0.9) 
write.csv(naosp, file="/n/wolkovich_lab/Lab/Cat/elevcc_predicted_90_five.csv", row.names = FALSE)

