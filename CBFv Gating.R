Vel.Gate = function (x, y, z) {

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


Fz = z


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

Dataset_Base$Time = 1:length(Dataset_Base$CBFv)
Dataset_Stim$Time = 1:length(Dataset_Stim$CBFv)


Dataset_Base$InverseCBFv = -Dataset_Base$CBFv
Dataset_Stim$InverseCBFv = -Dataset_Stim$CBFv


B.Peaks = data.frame(findpeaks(Dataset_Base$InverseCBFv, npeaks = Inf, minpeakdistance = (0.5 * Fq)))
S.Peaks = data.frame(findpeaks(Dataset_Stim$InverseCBFv, npeaks = Inf, minpeakdistance = (0.5 * Fq)))



Dataset_Base$Beat_Num[B.Peaks$X2] = "beat"
Dataset_Stim$Beat_Num[S.Peaks$X2] = "beat"


B.Peak_Plot = ggplot(Dataset_Base, aes(x = Time, y = CBFv)) + 
  geom_line() +
  geom_point(data = B.Peaks, aes(y = Dataset_Base$CBFv[X2], x = X2), color = "red") +
  theme(axis.title = element_text(face = "bold"), 
        panel.background = element_blank(), 
        axis.line = element_line(color = "black"),
        strip.background = element_blank(), 
        legend.position = "none")


S.Peak_Plot = ggplot(Dataset_Stim, aes(x = Time, y = CBFv)) + 
  geom_line() +
  geom_point(data = S.Peaks, aes(y = Dataset_Stim$CBFv[X2], x = X2), color = "red") +
  theme(axis.title = element_text(face = "bold"), 
        panel.background = element_blank(), 
        axis.line = element_line(color = "black"),
        strip.background = element_blank(), 
        legend.position = "none")

return(list(B.Peak_Plot = B.Peak_Plot, 
            S.Peak_Plot = S.Peak_Plot, 
            Baseline.Data = Dataset_Base, 
            Stim.Data = Dataset_Stim)
)



}

