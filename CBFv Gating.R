Vel.Gate = function (x, y, z) {
  
withProgress(message = "Beat detection in progress, please wait...", {  
  
packages = c("tidyverse",
             "ggplot2",
             "lme4",
             "readxl",
             "spiro",
             "signal",
             "zoo",
             "writexl",
             "MESS",
             "dplyr",
             "gsignal",
             "clintools",
             "plotly",
             "pracma",
             "plotly")

package.check = lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


incProgress(amount = 0.25)
 

Fz = z
Nyq = z / 2
BW_Cutoff = 20 / Nyq
Wn = c(5, 15) / Nyq
Derive.Coef = sgolay(p = 3, n = 5, m = 1)
Band.Filt.Coef = butter(4, Wn, type = "pass")

if(y == 1){
  Dataset_Base = read_excel(x, 
                            sheet = "CondA_Baseline")
  attach(Dataset_Base)
  
  
  Dataset_Stim = read_excel(x, 
                            sheet = "CondA_Stim")
  attach(Dataset_Stim)
  
}

if(y == 2){
  Dataset_Base = read_excel(x, 
                            sheet = "CondB_Baseline")
  attach(Dataset_Base)
  
  
  Dataset_Stim = read_excel(x, 
                            sheet = "CondB_Stim")
  attach(Dataset_Stim)
  
}

incProgress(amount = 0.25)
  
Dataset_Base$Time = 1:length(Dataset_Base$CBFv)
Dataset_Stim$Time = 1:length(Dataset_Stim$CBFv)

#Dataset_Base$BW_Filt = bw_filter(Dataset_Base$CBFv, n = 8, W = BW_Cutoff, zero_lag = TRUE)
#Dataset_Stim$BW_Filt = bw_filter(Dataset_Stim$CBFv, n = 8, W = BW_Cutoff, zero_lag = TRUE)

Dataset_Base$InverseCBFv = -Dataset_Base$CBFv
Dataset_Stim$InverseCBFv = -Dataset_Stim$CBFv

B.Fil = filtfilt(Band.Filt.Coef$b, Band.Filt.Coef$a, Dataset_Base$InverseCBFv) 
S.Fil = filtfilt(Band.Filt.Coef$b, Band.Filt.Coef$a, Dataset_Stim$InverseCBFv) 
  
Dataset_Base$BW_Filt = signal::filter(Derive.Coef, B.Fil)
Dataset_Stim$BW_Filt = signal::filter(Derive.Coef, S.Fil)
  

print("Pre")

B.Peaks = data.frame(pracma::findpeaks(Dataset_Base$BW_Filt, npeaks = Inf, minpeakdistance = (0.5 * Fq)))
S.Peaks = data.frame(pracma::findpeaks(Dataset_Stim$BW_Filt, npeaks = Inf, minpeakdistance = (0.5 * Fq)))



Dataset_Base$Beat_Num[B.Peaks$X2] = "beat"
Dataset_Stim$Beat_Num[S.Peaks$X2] = "beat"
print("Post")


B.Peak_Plot = ggplot(Dataset_Base, aes(x = Time, y = CBFv)) + 
  geom_line() +
  geom_point(data = B.Peaks, aes(y = Dataset_Base$CBFv[X2], x = X2), color = "red") +
  theme(axis.title = element_text(face = "bold"), 
        panel.background = element_blank(), 
        axis.line = element_line(color = "black"),
        strip.background = element_blank(), 
        legend.position = "none")

incProgress(amount = 0.25)
  
S.Peak_Plot = ggplot(Dataset_Stim, aes(x = Time, y = CBFv)) + 
  geom_line() +
  geom_point(data = S.Peaks, aes(y = Dataset_Stim$CBFv[X2], x = X2), color = "red") +
  theme(axis.title = element_text(face = "bold"), 
        panel.background = element_blank(), 
        axis.line = element_line(color = "black"),
        strip.background = element_blank(), 
        legend.position = "none")
  
incProgress(amount = 0.24)


B.Peak_Plot1 = ggplotly(B.Peak_Plot) %>%
  rangeslider(start = Dataset_Base$Time[1], end = Dataset_Base$Time[50000])
  
S.Peak_Plot1 = ggplotly(S.Peak_Plot) %>%
  rangeslider(start = Dataset_Stim$Time[1], end = Dataset_Stim$Time[50000])
  
return(list(B.Peak_Plot = B.Peak_Plot1, 
            S.Peak_Plot = S.Peak_Plot1, 
            Baseline.Data = Dataset_Base, 
            Stim.Data = Dataset_Stim)
      )
  incProgress(amount = 0.01)
})
}    
