setwd("C:/Users/jb0616165/Google Drive/Research - Growth (except Conflict)/Security and Growth (Trees)")
library(foreign); library(randomForest); library(nnet)
GrowthData<-read.dta("GrowthData.dta")
GrowthData$PartyExclusion <-as.factor(GrowthData$PartyExclusion)
GrowthData$Executive<-as.factor(GrowthData$Executive)
GrowthData$Regime<-as.factor(GrowthData$Regime)
GrowthData$PartyCoalitions<-as.factor(GrowthData$PartyCoalitions)
GrowthData$ParliamentRespons<-as.factor(GrowthData$ParliamentRespons)
GrowthData$System<-as.factor(GrowthData$System) 
# COPING WITH MISSING VALUES: RANDOM FOREST IMPUTATION
set.seed(8976)
GrowthData.rfImpute<-rfImpute(Conflict ~ GDPpc + ConsumptionGDP + InvestmentGDP + 
                                NetGovernmentGDP + MilitaryGDP + ImportsGDP + 
                                IndustryGDP + AidAssistGDP + TradeGDP + ExportsGDP + 
                                PrimCommodExports + TermsOfTrade + ExportPrices + 
                                ImportPrices + FDI_In_GDP + FDI_Out_GDP + Population + 
                                PopulationGrowth + LifeExpectancy + Dependency + 
                                LaborForceParticipation + FemaleLaborForce + 
                                LifeExpectancy + SecEnrollRatePCT + SchoolExpendGDP + 
                                RuralPopulationPCT + MoneyGDP + MoneyGrowth + InflationCPI + 
                                RealInterestRate + LendingInterestRate + InterestSpread + 
                                PhonesPC + Gini + GovStability + InvestmentProf + 
                                InternalConflict + ExternalConflict + Corruption + 
                                EthnicTensions + DemAcct + BureaucraticQual + 
                                EthnicPolariz + ReligiousPolariz + LegFrac + 
                                GovPolarization + ExecYrsOffice + ChangeInVetoes + 
                                GovHerfindahl + Checks + LIEC + EIEC + Fraud + Polity2 + 
                                RegDurability + Assassinations + Strikes + GuerrillaWar + 
                                GovCrises + Purges + Riots + Revolutions + Demonstrations + 
                                Coups + CabinetChgs + ExecutiveChgs + LegElections + 
                                TerrorAttacks + Fatalities + Injuries + PropertyDamage + 
                                System + Executive + Regime + PartyExclusion + 
                                PartyCoalitions + ParliamentRespons + Year, 
                              data = GrowthData, ntree = 500, iter = 20, maxnodes = 128,  
                              nodesize = 10, localImp = TRUE) 
# Create and merge new data frames of dummy variables based on the categoricals
library(nnet)
i.System<-class.ind(GrowthData.rfImpute$System)
colnames(i.System)<-paste("Sys", colnames(i.System), sep = "_")
i.PartyExcl <-class.ind(GrowthData.rfImpute$PartyExclusion)
colnames(i.PartyExcl)<-paste("Excl",colnames(i.PartyExcl),sep = "_")
i.Executive <-class.ind(GrowthData.rfImpute$Executive)
colnames(i.Executive)<-paste("Exec",colnames(i.Executive),sep = "_")
i.Regime <-class.ind(GrowthData.rfImpute$Regime)
colnames(i.Regime)<-paste("Reg", colnames(i.Regime), sep = "_")
i.PartyCoal <-class.ind(GrowthData.rfImpute$PartyCoalitions)
colnames(i.PartyCoal)<-paste("Coal",colnames(i.PartyCoal),sep = "_")
i.ParlResp <-class.ind(GrowthData.rfImpute$ParliamentRespons)
colnames(i.ParlResp)<-paste("Resp", colnames(i.ParlResp),sep = "_")
GrowthData.rfImpute<-cbind(GrowthData.rfImpute, i.System, i.PartyExcl, 
                           i.Executive, i.Regime, i.PartyCoal, i.ParlResp)
GrowthData.rfImpute<-cbind(GrowthData$wbcode, GrowthData$GDPpcGrowth, 
                           GrowthData$EthnicConflict, 
                           GrowthData$NonethnicConflict)
colnames(GrowthData.rfImpute)[104:107] <- c("wbcode", "GDPpcGrowth", "EthnicConflict",
                                            "NonethnicConflict")
save.image(file = "GrowthData20160719.RData")
write.dta(GrowthData.rfImpute, file = "GrowthDataImputed.dta")
