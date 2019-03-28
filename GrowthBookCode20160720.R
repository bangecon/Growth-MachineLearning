# Load some of the package libraries we will need
# All packages are free from the R "CRAN" website
library(gdata) # for some data manipulation (like "subset")
library(foreign) # for importing from Stata format
library(rpart) # for CART trees
library(rpart.plot) # for pretty CART trees
library(randomForest) # for random forests (duh!)
library(nnet) # for ANNs
library(pROC) # for ROC curves
library(gbm) # for boosting regression tree predictor
library(adabag) # for boosting classification tree predictor
load(url("https://github.com/bangecon/Growth-MachineLearning/raw/master/GrowthDataImputed.RData"))
GrowthData.Full <- subset(GrowthData.rfImpute, Year > 1970)
GrowthData.Full <- subset(GrowthData.Full, !is.na(GDPpcGrowthMA))
GrowthData.Full <- subset(GrowthData.Full, !is.na(LagGDPpcGrowth))
GrowthData.Full <- subset(GrowthData.Full, !is.na(LagConflict))
GrowthData.Full$Recession <- factor(GrowthData.Full$Recession, levels = c(0,1), 
                              labels = c("NoRecession", "Recession"))
GrowthData.Full$Recession5 <- factor(GrowthData.Full$Recession5, levels = c(0,1), 
                               labels = c("NoRecession", "Recession"))

# Subset into learning ("in") sample and test ("out") sample
set.seed(8976) 
GrowthData.Full$rand <- runif(nrow(GrowthData.Full))
GrowthData.Full$randbar <- with(GrowthData.Full, ave(rand, wbcode, FUN = mean))
GrowthData.Full$randbarp <- rank(GrowthData.Full$randbar)/length(GrowthData.Full$randbar) 
GrowthData.LS <- subset(GrowthData.Full, randbarp <= 0.70)
GrowthData.TS <- subset(GrowthData.Full, randbarp >  0.70)
attach(GrowthData.LS)
Growth.lm <- lm(GDPpcGrowthMA ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP + 
            LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP + 
            LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP + 
            LagPrimCommodExports + LagTermsOfTrade + LagExportPrices + 
            LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation + 
            LagPopulationGrowth + LagLifeExpectancy + LagDependency + 
            LagLaborForceParticipation + LagFemaleLaborForce + 
            LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP + 
            LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI + 
            LagRealInterestRate + LagLendingInterestRate + LagInterestSpread + 
            LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict + 
            LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab + 
            LagRegimeInstab + LagTerror + LagProtest + LagSys_Parliamentary + 
            LagSys_AssemblyElectedPresident + LagSys_Presidential + LagExec_Military + 
            LagExec_Monarch + LagExec_Other + LagExec_Premier + LagExec_Premier + 
            LagExec_President + LagReg_Civilian + LagReg_Military + 
            LagReg_MilitaryCivilian + LagReg_Other)
set.seed(8976)
Growth.nnet <- nnet(GDPpcGrowthMA ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP + 
            LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP + 
            LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP + 
            LagPrimCommodExports + LagTermsOfTrade + LagExportPrices + 
            LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation + 
            LagPopulationGrowth + LagLifeExpectancy + LagDependency + 
            LagLaborForceParticipation + LagFemaleLaborForce + 
            LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP + 
            LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI + 
            LagRealInterestRate + LagLendingInterestRate + LagInterestSpread + 
            LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict + 
            LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab + 
            LagRegimeInstab + LagTerror + LagProtest + LagSys_Parliamentary + 
            LagSys_AssemblyElectedPresident + LagSys_Presidential + LagExec_Military + 
            LagExec_Monarch + LagExec_Other + LagExec_Premier + LagExec_Premier + 
            LagExec_President + LagReg_Civilian + LagReg_Military + LagReg_Other + 
            LagReg_MilitaryCivilian, size = 20, linout = T, MaxNWts = 1200)
Growth.tree <- rpart(GDPpcGrowthMA ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP + 
                 LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP + 
                 LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP + 
                 LagPrimCommodExports + LagTermsOfTrade + LagExportPrices + 
                 LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation + 
                 LagPopulationGrowth + LagLifeExpectancy + LagDependency + 
                 LagLaborForceParticipation + LagFemaleLaborForce + 
                 LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP + 
                 LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI + 
                 LagRealInterestRate + LagLendingInterestRate + LagInterestSpread + 
                 LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict + 
                 LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab + 
                 LagRegimeInstab + LagTerror + LagProtest + LagSystem + LagExecutive + 
                 LagRegime, na.action = na.roughfix, model = TRUE, x = TRUE, y = TRUE, 
               control = rpart.control(cp = 0.001, minsplit = 10))
set.seed(8976)
Growth.bag <- randomForest(GDPpcGrowthMA ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP + 
                       LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP + 
                       LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP + 
                       LagPrimCommodExports + LagTermsOfTrade + LagExportPrices + 
                       LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation + 
                       LagPopulationGrowth + LagLifeExpectancy + LagDependency + 
                       LagLaborForceParticipation + LagFemaleLaborForce + 
                       LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP + 
                       LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI + 
                       LagRealInterestRate + LagLendingInterestRate + LagInterestSpread + 
                       LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict + 
                       LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab + 
                       LagRegimeInstab + LagTerror + LagProtest + LagSystem + LagExecutive + 
                       LagRegime, na.action = na.roughfix, mtry = 39, proximity = TRUE,  
                     nodesize = 10, localImp = TRUE, keep.forest = TRUE, sampsize = 3325)
set.seed(8976)
Growth.boost <- gbm(GDPpcGrowthMA ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP + 
                LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP + 
                LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP + 
                LagPrimCommodExports + LagTermsOfTrade + LagExportPrices + 
                LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation + 
                LagPopulationGrowth + LagLifeExpectancy + LagDependency + 
                LagLaborForceParticipation + LagFemaleLaborForce + 
                LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP + 
                LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI + 
                LagRealInterestRate + LagLendingInterestRate + LagInterestSpread + 
                LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict + 
                LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab + 
                LagRegimeInstab + LagTerror + LagProtest + LagSystem + LagExecutive + 
                LagRegime, distribution = "gaussian", n.trees = 500, interaction.depth = 3)
set.seed(8976)
Growth.rf <- randomForest(GDPpcGrowthMA ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP + 
                      LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP + 
                      LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP + 
                      LagPrimCommodExports + LagTermsOfTrade + LagExportPrices + 
                      LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation + 
                      LagPopulationGrowth + LagLifeExpectancy + LagDependency + 
                      LagLaborForceParticipation + LagFemaleLaborForce + 
                      LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP + 
                      LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI + 
                      LagRealInterestRate + LagLendingInterestRate + LagInterestSpread + 
                      LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict + 
                      LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab + 
                      LagRegimeInstab + LagTerror + LagProtest + LagSystem + LagExecutive + 
                      LagRegime, na.action = na.roughfix, proximity = TRUE, nodesize = 10,  
                    localImp = TRUE, keep.forest = TRUE, mtry = 3)
# Predicted Values
GrowthData.LS$GDPpcGrowthMA.lm <- predict(Growth.lm)
GrowthData.TS$GDPpcGrowthMA.lm <- predict(Growth.lm, newdata = GrowthData.TS)
GrowthData.LS$GDPpcGrowthMA.nnet <- predict(Growth.nnet)
GrowthData.TS$GDPpcGrowthMA.nnet <- predict(Growth.nnet, newdata = GrowthData.TS)
GrowthData.LS$GDPpcGrowthMA.tree <- predict(Growth.tree)
GrowthData.TS$GDPpcGrowthMA.tree <- predict(Growth.tree, newdata = GrowthData.TS)
GrowthData.LS$GDPpcGrowthMA.bag <- predict(Growth.bag)
GrowthData.TS$GDPpcGrowthMA.bag <- predict(Growth.bag, newdata = GrowthData.TS)
GrowthData.LS$GDPpcGrowthMA.boost <- predict(Growth.boost, n.trees = 500)
GrowthData.TS$GDPpcGrowthMA.boost <- predict(Growth.boost, newdata = GrowthData.TS, n.trees = 500)
GrowthData.LS$GDPpcGrowthMA.rf <- predict(Growth.rf)
GrowthData.TS$GDPpcGrowthMA.rf <- predict(Growth.rf, newdata = GrowthData.TS)
GrowthData.LS$GDPpcGrowthMA.ave <- (GrowthData.LS$GDPpcGrowthMA.lm + 
                                GrowthData.LS$GDPpcGrowthMA.bag + 
                                GrowthData.LS$GDPpcGrowthMA.boost + 
                                GrowthData.LS$GDPpcGrowthMA.rf)/4 
GrowthData.TS$GDPpcGrowthMA.ave <- (GrowthData.TS$GDPpcGrowthMA.lm + 
                                GrowthData.TS$GDPpcGrowthMA.bag + 
                                GrowthData.TS$GDPpcGrowthMA.boost + 
                                GrowthData.TS$GDPpcGrowthMA.rf)/4 
# Mean Squared Errors
MSE.LS <- mean((GDPpcGrowthMA-mean(GDPpcGrowthMA))^2)
MSE.TS <- mean((GrowthData.TS$GDPpcGrowthMA-mean(GrowthData.TS$GDPpcGrowthMA))^2)
MSE.lm.LS <- mean((GDPpcGrowthMA-GrowthData.LS$GDPpcGrowthMA.lm )^2)
MSE.lm.TS <- mean((GrowthData.TS$GDPpcGrowthMA-GrowthData.TS$GDPpcGrowthMA.lm)^2)
MSE.nnet.LS <- mean((GDPpcGrowthMA-GrowthData.LS$GDPpcGrowthMA.nnet)^2)
MSE.nnet.TS <- mean((GrowthData.TS$GDPpcGrowthMA-GrowthData.TS$GDPpcGrowthMA.nnet)^2)
MSE.tree.LS <- mean((GDPpcGrowthMA-GrowthData.LS$GDPpcGrowthMA.tree)^2)
MSE.tree.TS <- mean((GrowthData.TS$GDPpcGrowthMA-GrowthData.TS$GDPpcGrowthMA.tree)^2)
MSE.bag.LS <- mean((GDPpcGrowthMA-GrowthData.LS$GDPpcGrowthMA.bag)^2)
MSE.bag.TS <- mean((GrowthData.TS$GDPpcGrowthMA-GrowthData.TS$GDPpcGrowthMA.bag)^2)
MSE.boost.LS <- mean((GDPpcGrowthMA-GrowthData.LS$GDPpcGrowthMA.boost)^2)
MSE.boost.TS <- mean((GrowthData.TS$GDPpcGrowthMA-GrowthData.TS$GDPpcGrowthMA.boost)^2)
MSE.rf.LS <- mean((GDPpcGrowthMA-GrowthData.LS$GDPpcGrowthMA.rf)^2)
MSE.rf.TS <- mean((GrowthData.TS$GDPpcGrowthMA-GrowthData.TS$GDPpcGrowthMA.rf)^2)
MSE.ave.LS <- mean((GrowthData.LS$GDPpcGrowthMA-GrowthData.LS$GDPpcGrowthMA.ave)^2)
MSE.ave.TS <- mean((GrowthData.TS$GDPpcGrowthMA-GrowthData.TS$GDPpcGrowthMA.ave)^2)
MSE.Table <- cbind(rbind(MSE.lm.LS, MSE.nnet.LS, MSE.tree.LS, MSE.bag.LS, 
                   MSE.boost.LS, MSE.rf.LS, MSE.ave.LS, MSE.LS),
             rbind(MSE.lm.TS, MSE.nnet.TS, MSE.tree.TS, MSE.bag.TS, 
                   MSE.boost.TS, MSE.rf.TS, MSE.ave.TS, MSE.TS))
colnames(MSE.Table) <- c("Learning Sample", "Test Sample")
# Importance
Importance.tree<-100*as.data.frame(Growth.tree$variable.importance)/
sum(Growth.tree$variable.importance)
Importance.rf<-100*as.data.frame(Growth.rf$importance[,1])/ sum(Growth.rf$importance[,1])
Importance.bag<-as.data.frame(Growth.bag$importance[,1])
Importance.boost<-as.data.frame(summary(Growth.boost)[,2])
Importance.tree$id<-rownames(Importance.tree)
Importance.rf$id<-rownames(Importance.rf)
Importance.bag$id<-rownames(Importance.bag)
Importance.boost$id<-rownames(summary(Growth.boost))
Importance<-merge(Importance.tree, Importance.rf, by = "id", all = T)
Importance<-merge(Importance, Importance.bag, by = "id", all = T)
Importance<-merge(Importance, Importance.boost, by = "id", all = T)
Importance[is.na(Importance)]<-0
colnames(Importance)<-c("id", "Tree", "Forest", "Bagging", "Boosting")
rownames(Importance)<-Importance$id
Importance$Average<-(Importance$Tree + Importance$Forest + Importance$Bagging + 
                 Importance$Boosting)/4
Importance<-Importance[order(-Importance$Average),]
# Partial Dependence Plots
Categoricals<-c("LagEthnicConflict", "LagNonethnicConflict", "System", "Executive", "Regime")
Numericals<-rownames(Importance)[!rownames(Importance) %in% Categoricals]
source("partialPlotGBM.R")
for (i in seq_along(Numericals)) {
pdp.bag<-partialPlot(Growth.bag, pred.data = GrowthData.LS, Numericals[i], xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), plot = T, col = "blue")
lim.bag<-par('usr')
pdp.boost<-partialPlotGBM(Growth.boost, pred.data = GrowthData.LS, Numericals[i], xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), plot = T, col = "green", n.trees = 500)
lim.boost<-par('usr')
pdp.rf<-partialPlot(Growth.rf, pred.data = GrowthData.LS, Numericals[i], xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), plot = T, col = "red")
lim.rf<-par('usr')
yLimMin<-min(lim.bag[3], lim.boost[3], lim.rf[3])
yLimMax<-max(lim.bag[4], lim.boost[4], lim.rf[4])
jpeg(filename = paste(Numericals[i], ".jpg"))
partialPlot(Growth.bag, pred.data = GrowthData.LS, Numericals[i], xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), add = F, col = "blue",  ylim = c(yLimMin, yLimMax))  
partialPlotGBM(Growth.boost, pred.data = GrowthData.LS, Numericals[i], xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), add = T, col = "green", n.trees = 500)
partialPlot(Growth.rf, pred.data = GrowthData.LS, Numericals[i], xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), add = T, col = "red") 
dev.off()
legend("bottomright", c("Forest", "Boosting", "Bagging"), lty = c(1,1), lwd = 1, col = c("red","green","blue"))}

# Recession Dummy Variable
Recession.lm <- glm(Recession5 ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP +
                LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP +
                LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP +
                LagPrimCommodExports + LagTermsOfTrade + LagExportPrices +
                LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation +
                LagPopulationGrowth + LagLifeExpectancy + LagDependency +
                LagLaborForceParticipation + LagFemaleLaborForce +
                LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP +
                LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI +
                LagRealInterestRate + LagLendingInterestRate + LagInterestSpread +
                LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict +
                LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab +
                LagRegimeInstab + LagTerror + LagProtest + LagSys_Parliamentary +
                LagSys_AssemblyElectedPresident + LagSys_Presidential + LagExec_Military + 
                LagExec_Monarch + LagExec_Other + LagExec_Premier + LagExec_Premier + 
                LagExec_President + LagReg_Civilian + LagReg_Military + LagReg_Other + 
                LagReg_MilitaryCivilian, family = "binomial")
Recession.nnet <- nnet(Recession5 ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP +
                LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP +
                LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP +
                LagPrimCommodExports + LagTermsOfTrade + LagExportPrices +
                LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation +
                LagPopulationGrowth + LagLifeExpectancy + LagDependency +
                LagLaborForceParticipation + LagFemaleLaborForce +
                LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP +
                LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI +
                LagRealInterestRate + LagLendingInterestRate + LagInterestSpread +
                LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict +
                LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab +
                LagRegimeInstab + LagTerror + LagProtest + LagSys_Parliamentary +
                LagSys_AssemblyElectedPresident + LagSys_Presidential + LagExec_Military + 
                LagExec_Monarch + LagExec_Other + LagExec_Premier + LagExec_Premier + 
                LagExec_President + LagReg_Civilian + LagReg_Military + LagReg_Other + 
                LagReg_MilitaryCivilian, size = 20, MaxNWts = 1200)
Recession.tree <- rpart(Recession5 ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP + 
                         LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP + 
                         LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP + 
                         LagPrimCommodExports + LagTermsOfTrade + LagExportPrices + 
                         LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation + 
                         LagPopulationGrowth + LagLifeExpectancy + LagDependency + 
                         LagLaborForceParticipation + LagFemaleLaborForce + 
                         LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP + 
                         LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI + 
                         LagRealInterestRate + LagLendingInterestRate + LagInterestSpread + 
                         LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict + 
                         LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab + 
                         LagRegimeInstab + LagTerror + LagProtest + LagSystem + LagExecutive + 
                         LagRegime, na.action = na.roughfix, model = TRUE, x = TRUE, y = TRUE, 
                  control = rpart.control(cp = 0.001, minsplit = 10))
Recession.bag <- randomForest(Recession5 ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP + 
                         LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP + 
                         LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP + 
                         LagPrimCommodExports + LagTermsOfTrade + LagExportPrices + 
                         LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation + 
                         LagPopulationGrowth + LagLifeExpectancy + LagDependency + 
                         LagLaborForceParticipation + LagFemaleLaborForce + 
                         LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP + 
                         LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI + 
                         LagRealInterestRate + LagLendingInterestRate + LagInterestSpread + 
                         LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict + 
                         LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab + 
                         LagRegimeInstab + LagTerror + LagProtest + LagSystem + LagExecutive + 
                         LagRegime, na.action = na.roughfix, mtry = 39, proximity = TRUE,  
                       nodesize = 10, localImp = TRUE, keep.forest = TRUE, sampsize = 3325)
Recession.boost <- boosting(Recession5 ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP + 
                         LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP + 
                         LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP + 
                         LagPrimCommodExports + LagTermsOfTrade + LagExportPrices + 
                         LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation + 
                         LagPopulationGrowth + LagLifeExpectancy + LagDependency + 
                         LagLaborForceParticipation + LagFemaleLaborForce + 
                         LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP + 
                         LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI + 
                         LagRealInterestRate + LagLendingInterestRate + LagInterestSpread + 
                         LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict + 
                         LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab + 
                         LagRegimeInstab + LagTerror + LagProtest + LagSystem + LagExecutive + 
                         LagRegime, control = rpart.control(cp = 0.001, maxdepth = 5, minsplit = 10), 
                      data = GrowthData.LS, mfinal = 25)
colnames(Recession.boost$votes) <- c("NoRecession", "Recession")
Recession.rf <- randomForest(Recession5 ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP + 
                        LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP + 
                        LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP + 
                        LagPrimCommodExports + LagTermsOfTrade + LagExportPrices + 
                        LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation + 
                        LagPopulationGrowth + LagLifeExpectancy + LagDependency + 
                        LagLaborForceParticipation + LagFemaleLaborForce + 
                        LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP + 
                        LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI + 
                        LagRealInterestRate + LagLendingInterestRate + LagInterestSpread + 
                        LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict + 
                        LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab + 
                        LagRegimeInstab + LagTerror + LagProtest + LagSystem + LagExecutive + 
                        LagRegime, na.action = na.roughfix, proximity = TRUE, nodesize = 10,  
                        localImp = TRUE, keep.forest = TRUE, mtry = 3)
# PREDICTIVE ACCURACY, IMPORTANCE, AND PARTIAL DEPENDENCE PLOTS
## MEASURES OF PREDICTIVE ACCURACY
## PREDICTED VALUES 
### Probability of Conflict
GrowthData.LS$treeProb<-predict(Recession.tree, type = "prob")[,"Recession"]
GrowthData.TS$treeProb<-predict(Recession.tree, type = "prob", newdata = GrowthData.TS)[,"Recession"]
GrowthData.LS$rfProb<-predict(Recession.rf, type = "prob")[,"Recession"]
GrowthData.TS$rfProb<-predict(Recession.rf, type = "prob", newdata = GrowthData.TS)[,"Recession"]
GrowthData.LS$bagProb<-predict(Recession.bag, type = "prob")[,"Recession"]
GrowthData.TS$bagProb<-predict(Recession.bag, type = "prob", newdata = GrowthData.TS)[,"Recession"]
GrowthData.LS$boostProb<-predict(Recession.boost, newdata = GrowthData.LS)$prob[,2]
GrowthData.TS$boostProb<-predict(Recession.boost, newdata = GrowthData.TS)$prob[,2]
GrowthData.LS$lmProb<-predict(Recession.lm, newdata = GrowthData.LS, type = "response")
GrowthData.TS$lmProb<-predict(Recession.lm, newdata = GrowthData.TS, type = "response")
GrowthData.LS$nnetProb<-as.vector(Recession.nnet$fitted.values)
GrowthData.TS$nnetProb<-as.vector(predict(Recession.nnet, newdata = GrowthData.TS, type = "raw"))
GrowthData.LS$averageProb<-(GrowthData.LS$treeProb + GrowthData.LS$rfProb + GrowthData.LS$bagProb + GrowthData.LS$boostProb + GrowthData.LS$lmProb)/5
GrowthData.TS$averageProb<-(GrowthData.TS$treeProb + GrowthData.TS$rfProb + GrowthData.TS$bagProb + GrowthData.TS$boostProb + GrowthData.TS$lmProb)/5
### Predicted Classes
GrowthData.LS$treeClass<-predict(Recession.tree, type = "class")
GrowthData.TS$treeClass<-predict(Recession.tree, type = "class", newdata = GrowthData.TS)
GrowthData.LS$rfClass<-predict(Recession.rf, type = "class")
GrowthData.TS$rfClass<-predict(Recession.rf, type = "class", newdata = GrowthData.TS)
GrowthData.LS$bagClass<-predict(Recession.bag, type = "class")
GrowthData.TS$bagClass<-predict(Recession.bag, type = "class", newdata = GrowthData.TS)
GrowthData.LS$boostClass<-as.factor(predict(Recession.boost, newdata = GrowthData.LS)$class)
GrowthData.TS$boostClass<-as.factor(predict(Recession.boost, newdata = GrowthData.TS)$class)
GrowthData.LS$lmClass<-as.factor(ifelse(GrowthData.LS$lmProb < 0.5, "NoRecession", "Recession"))
GrowthData.TS$lmClass<-as.factor(ifelse(GrowthData.TS$lmProb < 0.5, "NoRecession", "Recession"))
GrowthData.LS$nnetClass<-as.factor(predict(Recession.nnet, newdata = GrowthData.LS, type = "class"))
GrowthData.TS$nnetClass<-as.factor(predict(Recession.nnet, newdata = GrowthData.TS, type = "class"))
GrowthData.LS$averageClass<-ifelse(GrowthData.LS$averageProb < 0.5, "NoRecession", "Recession")
GrowthData.TS$averageClass<-ifelse(GrowthData.TS$averageProb < 0.5, "NoRecession", "Recession")
### Confusion Matrices
Confusion.tree.LS <-with(GrowthData.LS, table(Recession5, treeClass))
Confusion.tree.TS<-with(GrowthData.TS, table(Recession5, treeClass))
Confusion.rf.LS <-with(GrowthData.LS, table(Recession5, rfClass))
Confusion.rf.TS<-with(GrowthData.TS, table(Recession5, rfClass))
Confusion.bag.LS <-with(GrowthData.LS, table(Recession5, bagClass))
Confusion.bag.TS<-with(GrowthData.TS, table(Recession5, bagClass))
Confusion.bag.TS <- cbind(Confusion.bag.TS[,2], Confusion.bag.TS[,1])
Confusion.boost.LS <-with(GrowthData.LS, table(Recession5, boostClass))
Confusion.boost.TS<-with(GrowthData.TS, table(Recession5, boostClass))
Confusion.boost.TS <- cbind(Confusion.boost.TS[,2], Confusion.boost.TS[,1])
Confusion.lm.LS <-with(GrowthData.LS, table(Recession5, lmClass))
Confusion.lm.TS<-with(GrowthData.TS, table(Recession5, lmClass))
Confusion.nnet.LS <-with(GrowthData.LS, table(Recession5, nnetClass))
Confusion.nnet.TS<-with(GrowthData.TS, table(Recession5, nnetClass))
Confusion.ave.LS <-with(GrowthData.LS, table(Recession5, averageClass))
Confusion.ave.TS<-with(GrowthData.TS, table(Recession5, averageClass))
### Specificity
Specificity.tree.LS<-Confusion.tree.LS[1,1]/(Confusion.tree.LS[1,1] + Confusion.tree.LS[1,2])
Specificity.tree.TS<-Confusion.tree.TS[1,1]/(Confusion.tree.TS[1,1] + Confusion.tree.TS[1,2])
Specificity.rf.LS<-Confusion.rf.LS[1,1]/(Confusion.rf.LS[1,1] + Confusion.rf.LS[1,2])
Specificity.rf.TS<-Confusion.rf.TS[1,1]/(Confusion.rf.TS[1,1] + Confusion.rf.TS[1,2])
Specificity.bag.LS<-Confusion.bag.LS[1,1]/(Confusion.bag.LS[1,1] + Confusion.bag.LS[1,2])
Specificity.bag.TS<-Confusion.bag.TS[1,1]/(Confusion.bag.TS[1,1] + Confusion.bag.TS[1,2])
Specificity.boost.LS<-Confusion.boost.LS[1,1]/(Confusion.boost.LS[1,1] + Confusion.boost.LS[1,2])
Specificity.boost.TS<-Confusion.boost.TS[1,1]/(Confusion.boost.TS[1,1] + Confusion.boost.TS[1,2])
Specificity.lm.LS<-Confusion.lm.LS[1,1]/(Confusion.lm.LS[1,1] + Confusion.lm.LS[1,2])
Specificity.lm.TS<-Confusion.lm.TS[1,1]/(Confusion.lm.TS[1,1] + Confusion.lm.TS[1,2])
Specificity.nnet.LS<-Confusion.nnet.LS[1,1]/(Confusion.nnet.LS[1,1] + Confusion.nnet.LS[1,2])
Specificity.nnet.TS<-Confusion.nnet.TS[1,1]/(Confusion.nnet.TS[1,1] + Confusion.nnet.TS[1,2])
Specificity.ave.LS<-Confusion.ave.LS[1,1]/(Confusion.ave.LS[1,1] + Confusion.ave.LS[1,2])
Specificity.ave.TS<-Confusion.ave.TS[1,1]/(Confusion.ave.TS[1,1] + Confusion.ave.TS[1,2])
### Sensitivity
Sensitivity.tree.LS<-Confusion.tree.LS[2,2]/(Confusion.tree.LS[2,2] + Confusion.tree.LS[2,1])
Sensitivity.tree.TS<-Confusion.tree.TS[2,2]/(Confusion.tree.TS[2,2] + Confusion.tree.TS[2,1])
Sensitivity.rf.LS<-Confusion.rf.LS[2,2]/(Confusion.rf.LS[2,2] + Confusion.rf.LS[2,1])
Sensitivity.rf.TS<-Confusion.rf.TS[2,2]/(Confusion.rf.TS[2,2] + Confusion.rf.TS[2,1])
Sensitivity.bag.LS<-Confusion.bag.LS[2,2]/(Confusion.bag.LS[2,2] + Confusion.bag.LS[2,1])
Sensitivity.bag.TS<-Confusion.bag.TS[2,2]/(Confusion.bag.TS[2,2] + Confusion.bag.TS[2,1])
Sensitivity.boost.LS<-Confusion.boost.LS[2,2]/(Confusion.boost.LS[2,2] + Confusion.boost.LS[2,1])
Sensitivity.boost.TS<-Confusion.boost.TS[2,2]/(Confusion.boost.TS[2,2] + Confusion.boost.TS[2,1])
Sensitivity.lm.LS<-Confusion.lm.LS[2,2]/(Confusion.lm.LS[2,2] + Confusion.lm.LS[2,1])
Sensitivity.lm.TS<-Confusion.lm.TS[2,2]/(Confusion.lm.TS[2,2] + Confusion.lm.TS[2,1])
Sensitivity.nnet.LS<-Confusion.nnet.LS[2,2]/(Confusion.nnet.LS[2,2] + Confusion.nnet.LS[2,1])
Sensitivity.nnet.TS<-Confusion.nnet.TS[2,2]/(Confusion.nnet.TS[2,2] + Confusion.nnet.TS[2,1])
Sensitivity.ave.LS<-Confusion.ave.LS[2,2]/(Confusion.ave.LS[2,2] + Confusion.ave.LS[2,1])
Sensitivity.ave.TS<-Confusion.ave.TS[2,2]/(Confusion.ave.TS[2,2] + Confusion.ave.TS[2,1])
### PPV
PPV.tree.LS<-Confusion.tree.LS[2,2]/(Confusion.tree.LS[2,2] + Confusion.tree.LS[1,2])
PPV.tree.TS<-Confusion.tree.TS[2,2]/(Confusion.tree.TS[2,2] + Confusion.tree.TS[1,2])
PPV.rf.LS<-Confusion.rf.LS[2,2]/(Confusion.rf.LS[2,2] + Confusion.rf.LS[1,2])
PPV.rf.TS<-Confusion.rf.TS[2,2]/(Confusion.rf.TS[2,2] + Confusion.rf.TS[1,2])
PPV.bag.LS<-Confusion.bag.LS[2,2]/(Confusion.bag.LS[2,2] + Confusion.bag.LS[1,2])
PPV.bag.TS<-Confusion.bag.TS[2,2]/(Confusion.bag.TS[2,2] + Confusion.bag.TS[1,2])
PPV.boost.LS<-Confusion.boost.LS[2,2]/(Confusion.boost.LS[2,2] + Confusion.boost.LS[1,2])
PPV.boost.TS<-Confusion.boost.TS[2,2]/(Confusion.boost.TS[2,2] + Confusion.boost.TS[1,2])
PPV.lm.LS<-Confusion.lm.LS[2,2]/(Confusion.lm.LS[2,2] + Confusion.lm.LS[1,2])
PPV.lm.TS<-Confusion.lm.TS[2,2]/(Confusion.lm.TS[2,2] + Confusion.lm.TS[1,2])
PPV.nnet.LS<-Confusion.nnet.LS[2,2]/(Confusion.nnet.LS[2,2] + Confusion.nnet.LS[1,2])
PPV.nnet.TS<-Confusion.nnet.TS[2,2]/(Confusion.nnet.TS[2,2] + Confusion.nnet.TS[1,2])
PPV.ave.LS<-Confusion.ave.LS[2,2]/(Confusion.ave.LS[2,2] + Confusion.ave.LS[1,2])
PPV.ave.TS<-Confusion.ave.TS[2,2]/(Confusion.ave.TS[2,2] + Confusion.ave.TS[1,2])
### NPV
NPV.tree.LS<-Confusion.tree.LS[1,1]/(Confusion.tree.LS[1,1] + Confusion.tree.LS[2,1])
NPV.tree.TS<-Confusion.tree.TS[1,1]/(Confusion.tree.TS[1,1] + Confusion.tree.TS[2,1])
NPV.rf.LS<-Confusion.rf.LS[1,1]/(Confusion.rf.LS[1,1] + Confusion.rf.LS[2,1])
NPV.rf.TS<-Confusion.rf.TS[1,1]/(Confusion.rf.TS[1,1] + Confusion.rf.TS[2,1])
NPV.bag.LS<-Confusion.bag.LS[1,1]/(Confusion.bag.LS[1,1] + Confusion.bag.LS[2,1])
NPV.bag.TS<-Confusion.bag.TS[1,1]/(Confusion.bag.TS[1,1] + Confusion.bag.TS[2,1])
NPV.boost.LS<-Confusion.boost.LS[1,1]/(Confusion.boost.LS[1,1] + Confusion.boost.LS[2,1])
NPV.boost.TS<-Confusion.boost.TS[1,1]/(Confusion.boost.TS[1,1] + Confusion.boost.TS[2,1])
NPV.lm.LS<-Confusion.lm.LS[1,1]/(Confusion.lm.LS[1,1] + Confusion.lm.LS[2,1])
NPV.lm.TS<-Confusion.lm.TS[1,1]/(Confusion.lm.TS[1,1] + Confusion.lm.TS[2,1])
NPV.nnet.LS<-Confusion.nnet.LS[1,1]/(Confusion.nnet.LS[1,1] + Confusion.nnet.LS[2,1])
NPV.nnet.TS<-Confusion.nnet.TS[1,1]/(Confusion.nnet.TS[1,1] + Confusion.nnet.TS[2,1])
NPV.ave.LS<-Confusion.ave.LS[1,1]/(Confusion.ave.LS[1,1] + Confusion.ave.LS[2,1])
NPV.ave.TS<-Confusion.ave.TS[1,1]/(Confusion.ave.TS[1,1] + Confusion.ave.TS[2,1])
### Overall error rate
Error.tree.LS<-(Confusion.tree.LS[1,2] + Confusion.tree.LS[2,1])/ nrow(GrowthData.LS)
Error.tree.TS<-(Confusion.tree.TS[1,2] + Confusion.tree.TS[2,1])/ nrow(GrowthData.TS)
Error.rf.LS<-(Confusion.rf.LS[1,2] + Confusion.rf.LS[2,1])/ nrow(GrowthData.LS)
Error.rf.TS<-(Confusion.rf.TS[1,2] + Confusion.rf.TS[2,1])/ nrow(GrowthData.TS)
Error.bag.LS<-(Confusion.bag.LS[1,2] + Confusion.bag.LS[2,1])/ nrow(GrowthData.LS)
Error.bag.TS<-(Confusion.bag.TS[1,2] + Confusion.bag.TS[2,1])/ nrow(GrowthData.TS)
Error.boost.LS<-(Confusion.boost.LS[1,2] + Confusion.boost.LS[2,1])/ nrow(GrowthData.LS)
Error.boost.TS<-(Confusion.boost.TS[1,2] + Confusion.boost.TS[2,1])/ nrow(GrowthData.TS)
Error.lm.LS<-(Confusion.lm.LS[1,2] + Confusion.lm.LS[2,1])/ nrow(GrowthData.LS)
Error.lm.TS<-(Confusion.lm.TS[1,2] + Confusion.lm.TS[2,1])/ nrow(GrowthData.TS)
Error.nnet.LS<-(Confusion.nnet.LS[1,2] + Confusion.nnet.LS[2,1])/ nrow(GrowthData.LS)
Error.nnet.TS<-(Confusion.nnet.TS[1,2] + Confusion.nnet.TS[2,1])/ nrow(GrowthData.TS)
Error.ave.LS<-(Confusion.ave.LS[1,2] + Confusion.ave.LS[2,1])/ nrow(GrowthData.LS)
Error.ave.TS<-(Confusion.ave.TS[1,2] + Confusion.ave.TS[2,1])/ nrow(GrowthData.TS)
PredictiveQuality.LS <- cbind(rbind(Specificity.tree.LS, Sensitivity.tree.LS, PPV.tree.LS, NPV.tree.LS, Error.ave.LS), rbind(Specificity.rf.LS, Sensitivity.rf.LS, PPV.rf.LS, NPV.rf.LS, Error.rf.LS), rbind(Specificity.bag.LS, Sensitivity.bag.LS, PPV.bag.LS, NPV.bag.LS, Error.bag.LS), rbind(Specificity.boost.LS, Sensitivity.boost.LS, PPV.boost.LS, NPV.boost.LS, Error.boost.LS), rbind(Specificity.lm.LS, Sensitivity.lm.LS, PPV.lm.LS, NPV.lm.LS, Error.lm.LS), rbind(Specificity.nnet.LS, Sensitivity.nnet.LS, PPV.nnet.LS, NPV.nnet.LS, Error.nnet.LS), rbind(Specificity.ave.LS, Sensitivity.ave.LS, PPV.ave.LS, NPV.ave.LS, Error.ave.LS))
colnames(PredictiveQuality.LS)<- c("Tree", "Forest", "Bagging", "Boosting", "Logit", "Neural Network", "Average")
rownames(PredictiveQuality.LS)<- c("Specificity", "Sensitivity", "Positive Predictive Value", "Negative Predictive Value", "Overall Error Rate")
PredictiveQuality.TS <- cbind(rbind(Specificity.tree.TS, Sensitivity.tree.TS, PPV.tree.TS, NPV.tree.TS, Error.ave.TS), rbind(Specificity.rf.TS, Sensitivity.rf.TS, PPV.rf.TS, NPV.rf.TS, Error.rf.TS), rbind(Specificity.bag.TS, Sensitivity.bag.TS, PPV.bag.TS, NPV.bag.TS, Error.bag.TS), rbind(Specificity.boost.TS, Sensitivity.boost.TS, PPV.boost.TS, NPV.boost.TS, Error.boost.TS), rbind(Specificity.lm.TS, Sensitivity.lm.TS, PPV.lm.TS, NPV.lm.TS, Error.lm.TS), rbind(Specificity.nnet.TS, Sensitivity.nnet.TS, PPV.nnet.TS, NPV.nnet.TS, Error.nnet.TS), rbind(Specificity.ave.TS, Sensitivity.ave.TS, PPV.ave.TS, NPV.ave.TS, Error.ave.TS))
colnames(PredictiveQuality.TS)<- c("Tree", "Forest", "Bagging", "Boosting", "Logit", "Neural Network", "Average")
rownames(PredictiveQuality.TS)<- c("Specificity", "Sensitivity", "Positive Predictive Value", "Negative Predictive Value", "Overall Error Rate")
### RECEIVER OPERATING CHARACTERISTIC (ROC) CURVES
library(pROC) 
roc.tree.GrowthData.LS<-roc(GrowthData.LS$Recession5, GrowthData.LS$treeProb, smooth = FALSE, auc = TRUE, plot = T, add = F, col = "brown", lwd = 1)
roc.rf.GrowthData.LS<-roc(GrowthData.LS$Recession5, GrowthData.LS$rfProb, smooth = FALSE, auc = TRUE, plot = T, add = T, col = "red", lwd = 1)
roc.bag.GrowthData.LS<-roc(GrowthData.LS$Recession5, GrowthData.LS$bagProb, smooth = FALSE, auc = TRUE, plot = T, add = T, col = "green", lwd = 1)
roc.boost.GrowthData.LS<-roc(GrowthData.LS$Recession5, GrowthData.LS$boostProb, smooth = FALSE, auc = TRUE, plot = T, add = T, col = "blue", lwd = 1)
roc.lm.GrowthData.LS<-roc(GrowthData.LS$Recession5, GrowthData.LS$lmProb, smooth = FALSE, auc = TRUE, plot = T, add = T, col = "gold", lwd = 1)
roc.nnet.GrowthData.LS<-roc(GrowthData.LS$Recession5, GrowthData.LS$nnetProb, smooth = FALSE, auc = TRUE, plot = T, add = T, col = "gray", lwd = 1)
roc.ave.GrowthData.LS<-roc(GrowthData.LS$Recession5, GrowthData.LS$averageProb, smooth = FALSE, auc = TRUE, plot = T, add = T, col = "black", lwd = 1)
legend("bottomright", c("Tree", "Forest", "Bagging", "Boosting", "Logit", "ANN", "Average"), lty = c(1,1), lwd = 1, col = c("brown", "red", "green", "blue", "gold", "gray", "black"))
roc.tree.GrowthData.TS<-roc(GrowthData.TS$Recession5, GrowthData.TS$treeProb, smooth = FALSE, auc = TRUE, plot = T, add = F, col = "brown", lwd = 1)
roc.rf.GrowthData.TS<-roc(GrowthData.TS$Recession5, GrowthData.TS$rfProb, smooth = FALSE, auc = TRUE, plot = T, add = T, col = "red", lwd = 1)
roc.bag.GrowthData.TS<-roc(GrowthData.TS$Recession5, GrowthData.TS$bagProb, smooth = FALSE, auc = TRUE, plot = T, add = T, col = "green", lwd = 1)
roc.boost.GrowthData.TS<-roc(GrowthData.TS$Recession5, GrowthData.TS$boostProb, smooth = FALSE, auc = TRUE, plot = T, add = T, col = "blue", lwd = 1)
roc.lm.GrowthData.TS<-roc(GrowthData.TS$Recession5, GrowthData.TS$lmProb, smooth = FALSE, auc = TRUE, plot = T, add = T, col = "gold", lwd = 1)
roc.nnet.GrowthData.TS<-roc(GrowthData.TS$Recession5, GrowthData.TS$nnetProb, smooth = FALSE, auc = TRUE, plot = T, add = T, col = "gray", lwd = 1)
roc.ave.GrowthData.TS<-roc(GrowthData.TS$Recession5, GrowthData.TS$averageProb, smooth = FALSE, auc = TRUE, plot = T, add = T, col = "black", lwd = 1)
legend("bottomright", c("Tree", "Forest", "Bagging", "Boosting", "Logit", "ANN", "Average"), lty = c(1,1), lwd = 1, col = c("brown", "red", "green", "blue", "gold", "gray", "black"))
### Areas under the (ROC) Curve (AUCs or C-Stats)
auc.table<-cbind( rbind(roc.tree.GrowthData.LS$auc,roc.tree.GrowthData.TS$auc), rbind(roc.rf.GrowthData.LS$auc,roc.rf.GrowthData.TS$auc), rbind(roc.bag.GrowthData.LS$auc,roc.bag.GrowthData.TS$auc), rbind(roc.boost.GrowthData.LS$auc,roc.boost.GrowthData.TS$auc), rbind(roc.logit.GrowthData.LS$auc,roc.logit.GrowthData.TS$auc), rbind(roc.nnet.GrowthData.LS$auc,roc.nnet.GrowthData.TS$auc), rbind(roc.ave.GrowthData.LS$auc,roc.ave.GrowthData.TS$auc))
rownames(auc.table)<-c("Learning Sample", "Test Sample")
colnames(auc.table)<-c("Tree", "Forest", "Bagging", "Boosting", "Logit", "ANN", "Average")

## VARIABLE IMPORTANCE
Importance.treeR<-100*as.data.frame(Recession.tree$variable.importance)/sum(Recession.tree$variable.importance)
Importance.rfR<-100*as.data.frame(Recession.rf$importance[,4])/sum(Recession.rf$importance[,4])
Importance.bagR<-100*as.data.frame(Recession.bag$importance[,4])/sum(Recession.bag$importance[,4])
Importance.boostR<-as.data.frame(Recession.boost$importance)
Importance.treeR$id<-rownames(Importance.treeR)
Importance.rfR$id<-rownames(Importance.rfR)
Importance.bagR$id<-rownames(Importance.bagR)
Importance.boostR$id<-rownames(Importance.boostR)
ImportanceR<-merge(Importance.treeR, Importance.rfR, by = "id", all = T)
ImportanceR<-merge(ImportanceR, Importance.bagR, by = "id", all = T)
ImportanceR<-merge(ImportanceR, Importance.boostR, by = "id", all = T)
ImportanceR[is.na(ImportanceR)]<-0
colnames(ImportanceR)<-c("id", "Tree", "Forest", "Bagging", "Boosting")
rownames(ImportanceR)<-ImportanceR$id
ImportanceR$Average<-(ImportanceR$Tree + ImportanceR$Forest + ImportanceR$Bagging + 
                 ImportanceR$Boosting)/4
ImportanceR<-ImportanceR[order(-ImportanceR$Average),]
## PARTIAL DEPENDENCE PLOTS
source('partialPlotBagging.R')
source('partialPlotBoosting.R')
# Partial Plots of Top  Important Variables (Except Current Conflict)
Categoricals<-c("LagEthnicConflict", "LagNonethnicConflict", "System", "Executive", "Regime")
Numericals<-rownames(ImportanceR)[!rownames(ImportanceR) %in% Categoricals]
for (i in seq_along(Numericals[1:25])) {
pdp.bagR<-partialPlot(Recession.bag, pred.data = GrowthData.LS, Numericals[i], which.class = "Recession", xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), plot = T, col = "green")
lim.bagR<-par('usr')
pdp.rfR<-partialPlot(Recession.rf, pred.data = GrowthData.LS, Numericals[i], which.class = "Recession", xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), plot = T, col = "red")
lim.rfR<-par('usr')
pdp.boostR<-partialPlotBoosting(Recession.boost, pred.data = GrowthData.LS, Numericals[i], which.class = "Recession", xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), plot = T, col = "blue")
lim.boostR<-par('usr')
yLimMin<-min(lim.boostR[3], lim.bagR[3], lim.rfR[3])
yLimMax<-max(lim.boostR[4], lim.bagR[4], lim.rfR[4])
partialPlot(Recession.bag, pred.data = GrowthData.LS, Numericals[i], which.class = "Recession", xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), plot = T, col = "green",  ylim = c(yLimMin, yLimMax), add = F)  
partialPlot(Recession.rf, pred.data = GrowthData.LS, Numericals[i], which.class = "Recession", xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), plot = T, col = "red", add = T) 
partialPlotBoosting(Recession.boost, pred.data = GrowthData.LS, Numericals[i], which.class = "Recession", xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), plot = T, col = "blue", add = T) 
legend("bottomright", c("Forest", "Bagging", "Boosting"), lty = c(1,1), lwd = 1, col = c("red","green","blue"))}
# Optimal Thresholds
## Predicted Classes
GrowthData.LS$treeClassP <- ifelse(GrowthData.LS$treeProb < coords(roc.tree.GrowthData.LS, "best")[1], 
                             "NoRecession", "Recession")
GrowthData.TS$treeClassP <- ifelse(GrowthData.TS$treeProb < coords(roc.tree.GrowthData.LS, "best")[1], 
                               "NoRecession", "Recession")
GrowthData.LS$rfClassP <- ifelse(GrowthData.LS$rfProb < coords(roc.rf.GrowthData.LS, "best")[1], 
                             "NoRecession", "Recession")
GrowthData.TS$rfClassP <- ifelse(GrowthData.TS$rfProb < coords(roc.rf.GrowthData.LS, "best")[1], 
                             "NoRecession", "Recession")
GrowthData.LS$bagClassP <- ifelse(GrowthData.LS$bagProb < coords(roc.bag.GrowthData.LS, "best")[1], 
                              "NoRecession", "Recession")
GrowthData.TS$bagClassP <- ifelse(GrowthData.TS$bagProb < coords(roc.bag.GrowthData.LS, "best")[1], 
                              "NoRecession", "Recession")
GrowthData.LS$boostClassP <- ifelse(GrowthData.LS$boostProb < coords(roc.boost.GrowthData.LS, "best")[1], 
                                "NoRecession", "Recession")
GrowthData.TS$boostClassP <- ifelse(GrowthData.TS$boostProb < coords(roc.boost.GrowthData.LS, "best")[1], 
                                "NoRecession", "Recession")
GrowthData.LS$lmClassP <- ifelse(GrowthData.LS$lmProb < coords(roc.lm.GrowthData.LS, "best")[1], 
                                "NoRecession", "Recession")
GrowthData.TS$lmClassP <- ifelse(GrowthData.TS$lmProb < coords(roc.lm.GrowthData.LS, "best")[1], 
                                "NoRecession", "Recession")
GrowthData.LS$nnetClassP <- ifelse(GrowthData.LS$nnetProb < coords(roc.nnet.GrowthData.LS, "best")[1], 
                               "NoRecession", "Recession")
GrowthData.TS$nnetClassP <- ifelse(GrowthData.TS$nnetProb < coords(roc.nnet.GrowthData.LS, "best")[1], 
                               "NoRecession", "Recession")
GrowthData.LS$averageClassP <- ifelse(GrowthData.LS$averageProb < coords(roc.ave.GrowthData.LS, "best")[1], 
                                  "NoRecession", "Recession")
GrowthData.TS$averageClassP <- ifelse(GrowthData.TS$averageProb < coords(roc.ave.GrowthData.LS, "best")[1], 
                                  "NoRecession", "Recession")
## MEASURES OF PREDICTIVE ACCURACY WITH OPTIMAL THRESHOLDS
### Confusion matrices
Confusion.tree.LSp <- table(GrowthData.LS$Conflict, GrowthData.LS$treeClassP)
Confusion.tree.LSp <- cbind(Confusion.tree.LSp[,2], Confusion.tree.LSp[,1])
Confusion.tree.TSp <- table(GrowthData.TS$Conflict, GrowthData.TS$treeClassP)
Confusion.tree.TSp <- cbind(Confusion.tree.TSp[,2], Confusion.tree.TSp[,1])
Confusion.rf.LSp <- table(GrowthData.LS$Conflict, GrowthData.LS$rfClassP)
Confusion.rf.LSp <- cbind(Confusion.rf.LSp[,2], Confusion.rf.LSp[,1])
Confusion.rf.TSp <- table(GrowthData.TS$Conflict, GrowthData.TS$rfClassP)
Confusion.rf.TSp <- cbind(Confusion.rf.TSp[,2], Confusion.rf.TSp[,1])
Confusion.bag.LSp <- table(GrowthData.LS$Conflict, GrowthData.LS$bagClassP)
Confusion.bag.LSp <- cbind(Confusion.bag.LSp[,2], Confusion.bag.LSp[,1])
Confusion.bag.TSp <- table(GrowthData.TS$Conflict, GrowthData.TS$bagClassP)
Confusion.bag.TSp <- cbind(Confusion.bag.TSp[,2], Confusion.bag.TSp[,1])
Confusion.boost.LSp <- table(GrowthData.LS$Conflict, GrowthData.LS$boostClassP)
Confusion.boost.LSp <- cbind(Confusion.boost.LSp[,2], Confusion.boost.LSp[,1])
Confusion.boost.TSp <- table(GrowthData.TS$Conflict, GrowthData.TS$boostClassP)
Confusion.boost.TSp <- cbind(Confusion.boost.TSp[,2], Confusion.boost.TSp[,1])
Confusion.logit.LSp <- table(GrowthData.LS$Conflict, GrowthData.LS$logitClassP)
Confusion.logit.LSp <- cbind(Confusion.logit.LSp[,2], Confusion.logit.LSp[,1])
Confusion.logit.TSp <- table(GrowthData.TS$Conflict, GrowthData.TS$logitClassP)
Confusion.logit.TSp <- cbind(Confusion.logit.TSp[,2], Confusion.logit.TSp[,1])
Confusion.nnet.LSp <- table(GrowthData.LS$Conflict, GrowthData.LS$nnetClassP)
Confusion.nnet.TSp <- table(GrowthData.TS$Conflict, GrowthData.TS$nnetClassP)
Confusion.ave.LSp <- table(GrowthData.LS$Conflict, GrowthData.LS$averageClassP)
Confusion.ave.LSp <- cbind(Confusion.ave.LSp[,2], Confusion.ave.LSp[,1])
Confusion.ave.TSp <- table(GrowthData.TS$Conflict, GrowthData.TS$averageClassP)
Confusion.ave.TSp <- cbind(Confusion.ave.TSp[,2], Confusion.ave.TSp[,1])
### Specificity
Specificity.tree.LSp<-Confusion.tree.LSp[1,1]/(Confusion.tree.LSp[1,1] + Confusion.tree.LSp[1,2])
Specificity.tree.TSp<-Confusion.tree.TSp[1,1]/(Confusion.tree.TSp[1,1] + Confusion.tree.TSp[1,2])
Specificity.rf.LSp<-Confusion.rf.LSp[1,1]/(Confusion.rf.LSp[1,1] + Confusion.rf.LSp[1,2])
Specificity.rf.TSp<-Confusion.rf.TSp[1,1]/(Confusion.rf.TSp[1,1] + Confusion.rf.TSp[1,2])
Specificity.bag.LSp<-Confusion.bag.LSp[1,1]/(Confusion.bag.LSp[1,1] + Confusion.bag.LSp[1,2])
Specificity.bag.TSp<-Confusion.bag.TSp[1,1]/(Confusion.bag.TSp[1,1] + Confusion.bag.TSp[1,2])
Specificity.boost.LSp<-Confusion.boost.LSp[1,1]/(Confusion.boost.LSp[1,1] + Confusion.boost.LSp[1,2])
Specificity.boost.TSp<-Confusion.boost.TSp[1,1]/(Confusion.boost.TSp[1,1] + Confusion.boost.TSp[1,2])
Specificity.logit.LSp<-Confusion.logit.LSp[1,1]/(Confusion.logit.LSp[1,1] + Confusion.logit.LSp[1,2])
Specificity.logit.TSp<-Confusion.logit.TSp[1,1]/(Confusion.logit.TSp[1,1] + Confusion.logit.TSp[1,2])
Specificity.nnet.LSp<-Confusion.nnet.LSp[1,1]/(Confusion.nnet.LSp[1,1] + Confusion.nnet.LSp[1,2])
Specificity.nnet.TSp<-Confusion.nnet.TSp[1,1]/(Confusion.nnet.TSp[1,1] + Confusion.nnet.TSp[1,2])
Specificity.ave.LSp<-Confusion.ave.LSp[1,1]/(Confusion.ave.LSp[1,1] + Confusion.ave.LSp[1,2])
Specificity.ave.TSp<-Confusion.ave.TSp[1,1]/(Confusion.ave.TSp[1,1] + Confusion.ave.TSp[1,2])
### Sensitivity
Sensitivity.tree.LSp<-Confusion.tree.LSp[2,2]/(Confusion.tree.LSp[2,2] + Confusion.tree.LSp[2,1])
Sensitivity.tree.TSp<-Confusion.tree.TSp[2,2]/(Confusion.tree.TSp[2,2] + Confusion.tree.TSp[2,1])
Sensitivity.rf.LSp<-Confusion.rf.LSp[2,2]/(Confusion.rf.LSp[2,2] + Confusion.rf.LSp[2,1])
Sensitivity.rf.TSp<-Confusion.rf.TSp[2,2]/(Confusion.rf.TSp[2,2] + Confusion.rf.TSp[2,1])
Sensitivity.bag.LSp<-Confusion.bag.LSp[2,2]/(Confusion.bag.LSp[2,2] + Confusion.bag.LSp[2,1])
Sensitivity.bag.TSp<-Confusion.bag.TSp[2,2]/(Confusion.bag.TSp[2,2] + Confusion.bag.TSp[2,1])
Sensitivity.boost.LSp<-Confusion.boost.LSp[2,2]/(Confusion.boost.LSp[2,2] + Confusion.boost.LSp[2,1])
Sensitivity.boost.TSp<-Confusion.boost.TSp[2,2]/(Confusion.boost.TSp[2,2] + Confusion.boost.TSp[2,1])
Sensitivity.logit.LSp<-Confusion.logit.LSp[2,2]/(Confusion.logit.LSp[2,2] + Confusion.logit.LSp[2,1])
Sensitivity.logit.TSp<-Confusion.logit.TSp[2,2]/(Confusion.logit.TSp[2,2] + Confusion.logit.TSp[2,1])
Sensitivity.nnet.LSp<-Confusion.nnet.LSp[2,2]/(Confusion.nnet.LSp[2,2] + Confusion.nnet.LSp[2,1])
Sensitivity.nnet.TSp<-Confusion.nnet.TSp[2,2]/(Confusion.nnet.TSp[2,2] + Confusion.nnet.TSp[2,1])
Sensitivity.ave.LSp<-Confusion.ave.LSp[2,2]/(Confusion.ave.LSp[2,2] + Confusion.ave.LSp[2,1])
Sensitivity.ave.TSp<-Confusion.ave.TSp[2,2]/(Confusion.ave.TSp[2,2] + Confusion.ave.TSp[2,1])
### PPV
PPV.tree.LSp<-Confusion.tree.LSp[2,2]/(Confusion.tree.LSp[2,2] + Confusion.tree.LSp[1,2])
PPV.tree.TSp<-Confusion.tree.TSp[2,2]/(Confusion.tree.TSp[2,2] + Confusion.tree.TSp[1,2])
PPV.rf.LSp<-Confusion.rf.LSp[2,2]/(Confusion.rf.LSp[2,2] + Confusion.rf.LSp[1,2])
PPV.rf.TSp<-Confusion.rf.TSp[2,2]/(Confusion.rf.TSp[2,2] + Confusion.rf.TSp[1,2])
PPV.bag.LSp<-Confusion.bag.LSp[2,2]/(Confusion.bag.LSp[2,2] + Confusion.bag.LSp[1,2])
PPV.bag.TSp<-Confusion.bag.TSp[2,2]/(Confusion.bag.TSp[2,2] + Confusion.bag.TSp[1,2])
PPV.boost.LSp<-Confusion.boost.LSp[2,2]/(Confusion.boost.LSp[2,2] + Confusion.boost.LSp[1,2])
PPV.boost.TSp<-Confusion.boost.TSp[2,2]/(Confusion.boost.TSp[2,2] + Confusion.boost.TSp[1,2])
PPV.logit.LSp<-Confusion.logit.LSp[2,2]/(Confusion.logit.LSp[2,2] + Confusion.logit.LSp[1,2])
PPV.logit.TSp<-Confusion.logit.TSp[2,2]/(Confusion.logit.TSp[2,2] + Confusion.logit.TSp[1,2])
PPV.nnet.LSp<-Confusion.nnet.LSp[2,2]/(Confusion.nnet.LSp[2,2] + Confusion.nnet.LSp[1,2])
PPV.nnet.TSp<-Confusion.nnet.TSp[2,2]/(Confusion.nnet.TSp[2,2] + Confusion.nnet.TSp[1,2])
PPV.ave.LSp<-Confusion.ave.LSp[2,2]/(Confusion.ave.LSp[2,2] + Confusion.ave.LSp[1,2])
PPV.ave.TSp<-Confusion.ave.TSp[2,2]/(Confusion.ave.TSp[2,2] + Confusion.ave.TSp[1,2])
### NPV
NPV.tree.LSp<-Confusion.tree.LSp[1,1]/(Confusion.tree.LSp[1,1] + Confusion.tree.LSp[2,1])
NPV.tree.TSp<-Confusion.tree.TSp[1,1]/(Confusion.tree.TSp[1,1] + Confusion.tree.TSp[2,1])
NPV.rf.LSp<-Confusion.rf.LSp[1,1]/(Confusion.rf.LSp[1,1] + Confusion.rf.LSp[2,1])
NPV.rf.TSp<-Confusion.rf.TSp[1,1]/(Confusion.rf.TSp[1,1] + Confusion.rf.TSp[2,1])
NPV.bag.LSp<-Confusion.bag.LSp[1,1]/(Confusion.bag.LSp[1,1] + Confusion.bag.LSp[2,1])
NPV.bag.TSp<-Confusion.bag.TSp[1,1]/(Confusion.bag.TSp[1,1] + Confusion.bag.TSp[2,1])
NPV.boost.LSp<-Confusion.boost.LSp[1,1]/(Confusion.boost.LSp[1,1] + Confusion.boost.LSp[2,1])
NPV.boost.TSp<-Confusion.boost.TSp[1,1]/(Confusion.boost.TSp[1,1] + Confusion.boost.TSp[2,1])
NPV.logit.LSp<-Confusion.logit.LSp[1,1]/(Confusion.logit.LSp[1,1] + Confusion.logit.LSp[2,1])
NPV.logit.TSp<-Confusion.logit.TSp[1,1]/(Confusion.logit.TSp[1,1] + Confusion.logit.TSp[2,1])
NPV.nnet.LSp<-Confusion.nnet.LSp[1,1]/(Confusion.nnet.LSp[1,1] + Confusion.nnet.LSp[2,1])
NPV.nnet.TSp<-Confusion.nnet.TSp[1,1]/(Confusion.nnet.TSp[1,1] + Confusion.nnet.TSp[2,1])
NPV.ave.LSp<-Confusion.ave.LSp[1,1]/(Confusion.ave.LSp[1,1] + Confusion.ave.LSp[2,1])
NPV.ave.TSp<-Confusion.ave.TSp[1,1]/(Confusion.ave.TSp[1,1] + Confusion.ave.TSp[2,1])
### Overall error rate
Error.tree.LSp<-(Confusion.tree.LSp[1,2] + Confusion.tree.LSp[2,1])/ nrow(GrowthData.LS)
Error.tree.TSp<-(Confusion.tree.TSp[1,2] + Confusion.tree.TSp[2,1])/ nrow(GrowthData.TS)
Error.rf.LSp<-(Confusion.rf.LSp[1,2] + Confusion.rf.LSp[2,1])/ nrow(GrowthData.LS)
Error.rf.TSp<-(Confusion.rf.TSp[1,2] + Confusion.rf.TSp[2,1])/ nrow(GrowthData.TS)
Error.bag.LSp<-(Confusion.bag.LSp[1,2] + Confusion.bag.LSp[2,1])/ nrow(GrowthData.LS)
Error.bag.TSp<-(Confusion.bag.TSp[1,2] + Confusion.bag.TSp[2,1])/ nrow(GrowthData.TS)
Error.boost.LSp<-(Confusion.boost.LSp[1,2] + Confusion.boost.LSp[2,1])/ nrow(GrowthData.LS)
Error.boost.TSp<-(Confusion.boost.TSp[1,2] + Confusion.boost.TSp[2,1])/ nrow(GrowthData.TS)
Error.logit.LSp<-(Confusion.logit.LSp[1,2] + Confusion.logit.LSp[2,1])/ nrow(GrowthData.LS)
Error.logit.TSp<-(Confusion.logit.TSp[1,2] + Confusion.logit.TSp[2,1])/ nrow(GrowthData.TS)
Error.nnet.LSp<-(Confusion.nnet.LSp[1,2] + Confusion.nnet.LSp[2,1])/ nrow(GrowthData.LS)
Error.nnet.TSp<-(Confusion.nnet.TSp[1,2] + Confusion.nnet.TSp[2,1])/ nrow(GrowthData.TS)
Error.ave.LSp<-(Confusion.ave.LSp[1,2] + Confusion.ave.LSp[2,1])/ nrow(GrowthData.LS)
Error.ave.TSp<-(Confusion.ave.TSp[1,2] + Confusion.ave.TSp[2,1])/ nrow(GrowthData.TS)
PredictiveQuality.LSp <- cbind(rbind(Specificity.tree.LSp, Sensitivity.tree.LSp, PPV.tree.LSp, NPV.tree.LSp, Error.ave.LSp), rbind(Specificity.rf.LSp, Sensitivity.rf.LSp, PPV.rf.LSp, NPV.rf.LSp, Error.rf.LSp), rbind(Specificity.bag.LSp, Sensitivity.bag.LSp, PPV.bag.LSp, NPV.bag.LSp, Error.bag.LSp), rbind(Specificity.boost.LSp, Sensitivity.boost.LSp, PPV.boost.LSp, NPV.boost.LSp, Error.boost.LSp), rbind(Specificity.logit.LSp, Sensitivity.logit.LSp, PPV.logit.LSp, NPV.logit.LSp, Error.logit.LSp), rbind(Specificity.nnet.LSp, Sensitivity.nnet.LSp, PPV.nnet.LSp, NPV.nnet.LSp, Error.nnet.LSp), rbind(Specificity.ave.LSp, Sensitivity.ave.LSp, PPV.ave.LSp, NPV.ave.LSp, Error.ave.LSp))
colnames(PredictiveQuality.LSp)<- c("Tree", "Forest", "Bagging", "Boosting", "Logit", "Neural Network", "Average")
rownames(PredictiveQuality.LSp)<- c("Specificity", "Sensitivity", "Positive Predictive Value", "Negative Predictive Value", "Overall Error Rate")
PredictiveQuality.TSp <- cbind(rbind(Specificity.tree.TSp, Sensitivity.tree.TSp, PPV.tree.TSp, NPV.tree.TSp, Error.ave.TSp), rbind(Specificity.rf.TSp, Sensitivity.rf.TSp, PPV.rf.TSp, NPV.rf.TSp, Error.rf.TSp), rbind(Specificity.bag.TSp, Sensitivity.bag.TSp, PPV.bag.TSp, NPV.bag.TSp, Error.bag.TSp), rbind(Specificity.boost.TSp, Sensitivity.boost.TSp, PPV.boost.TSp, NPV.boost.TSp, Error.boost.TSp), rbind(Specificity.logit.TSp, Sensitivity.logit.TSp, PPV.logit.TSp, NPV.logit.TSp, Error.logit.TSp), rbind(Specificity.nnet.TSp, Sensitivity.nnet.TSp, PPV.nnet.TSp, NPV.nnet.TSp, Error.nnet.TSp), rbind(Specificity.ave.TSp, Sensitivity.ave.TSp, PPV.ave.TSp, NPV.ave.TSp, Error.ave.TSp))
colnames(PredictiveQuality.TSp)<- c("Tree", "Forest", "Bagging", "Boosting", "Logit", "Neural Network", "Average")
rownames(PredictiveQuality.TSp)<- c("Specificity", "Sensitivity", "Positive Predictive Value", "Negative Predictive Value", "Overall Error Rate")



# Predicting beyond the time sample
GrowthData.rfImpute$treeProb <- predict(Growth.rpart, newdata = GrowthData.rfImpute, 
                                    type = "prob")[,"Recession"]
GrowthData.rfImpute$rfProb <- predict(Growth.rF, newdata = GrowthData.rfImpute, 
                                  type = "prob")[,"Recession"]
GrowthData.rfImpute$bagProb <- predict(Growth.bag, newdata = GrowthData.rfImpute)$prob[,1]
GrowthData.rfImpute$boostProb <- predict(Growth.boost, newdata = GrowthData.rfImpute)$prob[,1]
GrowthData.rfImpute$logitProb <- 1-predict(Growth.logit, newdata = GrowthData.rfImpute, type = "response")
GrowthData.rfImpute$nnetProb <- 1 - as.vector(predict(Growth.nnet, newdata = GrowthData.rfImpute, type = "raw"))
GrowthData.rfImpute$averageProb <- (GrowthData.rfImpute$treeProb + GrowthData.rfImpute$rfProb + 
                                  GrowthData.rfImpute$bagProb + GrowthData.rfImpute$boostProb + GrowthData.rfImpute$logitProb + 
                                  GrowthData.rfImpute$nnetProb)/6

Data2014.rfImpute <- subset(GrowthData.rfImpute, Year == 2014)
