setupAnova<-function(dat_roi,meas) {

###Set up dataframe for ANOCOVA, where 'meas' is the column name
###of the dependent variable of interest

#Developing centered covariate data (change this based on covariates in model)

dat_roi$mean_Paness=mean(dat_roi$Paness)
dat_roi$cPaness=dat_roi$Paness-dat_roi$mean_Paness
dat_roi$mean_Age=mean(dat_roi$Age)
dat_roi$cAge=dat_roi$Age-dat_roi$mean_Age
dat_roi$mean_ICV=mean(dat_roi$ICV)
dat_roi$cICV=dat_roi$ICV-dat_roi$mean_ICV

#centering factors (change this based on factors in model)
contrasts(dat_roi$Diag)<-contr.sum
contrasts(dat_roi$Hemi)<-contr.sum

#setting dependent variable of interest
colnames(dat_roi)[colnames(dat_roi)==meas]="measure"
return(dat_roi)

}