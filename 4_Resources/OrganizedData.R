#OrganizedData

#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install(version = "3.18")
# BiocManager::install("bigmelon")
# BiocManager::install("gdsfmt", force = TRUE)
library(bigmelon)
library(gdsfmt)

# Read in data ####
# load in data for batch2017
batch2017 <- openfn.gds("1_RawData/OMIC-49-SUGDEN.gds")
pData2017 <- read.delim('1_RawData/OMIC-49-SUGDEN_epi_sendout.txt', sep='\t', header=TRUE, stringsAsFactors=FALSE)  

betas_2017 = as.matrix(betas(batch2017)[,])
save(betas_2017, file = "1_RawData/betas_2017.RData")

closefn.gds(batch2017)
# load in data for batch2020
batch2020 <- openfn.gds("1_RawData/OMIC-49-SUGDEN_2020.gds")
pData2020 <- read.delim("1_RawData/OMIC-49-SUGDEN_epi_2020_sendout.txt", sep="\t", header=TRUE, stringsAsFactors=FALSE)

betas_2020 = as.matrix(betas(batch2020)[,])
save(betas_2020, file = "1_RawData/betas_2020.RData")

closefn.gds(batch2020)

# Subset the matrix with overlapping probe ####

load("1_RawData/betas_2017.RData")
load("1_RawData/betas_2020.RData")

length(intersect(rownames(betas_2017), rownames(betas_2020)))
overlap = intersect(rownames(betas_2017), rownames(betas_2020))
# There are 854803 probes are overlapping. 

sub_2017 = betas_2017[overlap,]
setdiff(colnames(sub_2017), pData2017$id)
setdiff(pData2017$id, colnames(sub_2017))
sub_2017 = sub_2017[,match(pData2017[,1], colnames(sub_2017))]
save(sub_2017, file = "subset_betas_2017.RData")

sub_2020 = betas_2020[overlap,]
setdiff(pData2020$id, colnames(sub_2020))
setdiff(colnames(sub_2020), pData2020$id)
sub_2020 = sub_2020[,match(pData2020[,1], colnames(sub_2020))]
save(sub_2020, file = "subset_betas_2020.RData")

# Select necessary for pData for Plan1 ####
# for 2017
variables2017 = c('id', 'confage', 'nsex', 'rackbarcode', 'cd8t', 'cd4t', 'nk', 'bcell', 'mono',
                  'c_sf1','c_scsf1', 'c_scsf2a', 'c_scsf2b', 'c_scsf3a', 'c_scsf3b', 'c_scsf5', 'c_scsf7')

pData2017 = pData2017[,variables2017]
pDataSorted2017 <- pData2017[match(colnames(sub_2017), pData2017[,1]),]
colnames(pDataSorted2017) = c('id', 'Age_numeric', 'Sex_factor', 'Rackbarcode_factor', 
                              'cd8t', 'cd4t', 'nk', 'bcell', 'mono',
                              'sf1','scsf1', 'scsf2a', 'scsf2b', 'scsf3a', 'scsf3b', 'scsf5', 'scsf7')

pDataSorted2017$Age_numeric = as.numeric(pDataSorted2017$Age_numeric)
pDataSorted2017$Sex_factor = as.factor(pDataSorted2017$Sex_factor)
pDataSorted2017$Rackbarcode_factor = as.factor(pDataSorted2017$Rackbarcode_factor)

# for 2020
wave2 = pData2020[which(pData2020$wave == 2), c('id', 'b_sf1','b_scsf1', 'b_scsf2a', 'b_scsf2b', 
                                                'b_scsf3a', 'b_scsf3b', 'b_scsf5', 'b_scsf7')]
colnames(wave2) <- c('id', 'sf1','scsf1', 'scsf2a', 'scsf2b', 'scsf3a', 'scsf3b', 'scsf5', 'scsf7')
wave3 = pData2020[which(pData2020$wave == 3), c('id', 'c_sf1','c_scsf1', 'c_scsf2a', 'c_scsf2b', 
                                                'c_scsf3a', 'c_scsf3b', 'c_scsf5', 'c_scsf7')]
colnames(wave3) <- c('id', 'sf1','scsf1', 'scsf2a', 'scsf2b', 'scsf3a', 'scsf3b', 'scsf5', 'scsf7')

pDataHealth2020 = rbind(wave2, wave3)
pData2020 = pData2020[,c('id', 'confage', 'nsex', 'rackbarcode', 'cd8t', 'cd4t', 'nk', 'bcell', 'mono')]
pData2020 = merge(pData2020, pDataHealth2020, by.y = 'id', by.x = 'id')
pDataSorted2020 <- pData2020[match(colnames(sub_2020), pData2020[,1]),]
colnames(pDataSorted2020) = c('id', 'Age_numeric', 'Sex_factor', 'Rackbarcode_factor', 
                              'cd8t', 'cd4t', 'nk', 'bcell', 'mono',
                              'sf1','scsf1', 'scsf2a', 'scsf2b', 'scsf3a', 'scsf3b', 'scsf5', 'scsf7')

pDataSorted2020$Age_numeric = as.numeric(pDataSorted2020$Age_numeric)
pDataSorted2020$Sex_factor = as.factor(pDataSorted2020$Sex_factor)
pDataSorted2020$Rackbarcode_factor = as.factor(pDataSorted2020$Rackbarcode_factor)

save(pDataSorted2017, file = '1_RawData/pDataSorted2017.RData')
save(pDataSorted2020, file = '1_RawData/pDataSorted2020.RData')


load("1_RawData/subset_betas_2017.RData")
load('1_RawData/pDataSorted2017.RData')
betas = sub_2017
pData = pDataSorted2017
save(betas ,pData , file = '1_RawData/BetasPhen2017.RData')

load("1_RawData/subset_betas_2020.RData")
load('1_RawData/pDataSorted2020.RData')
betas = sub_2020
pData = pDataSorted2020
save(betas ,pData , file = '1_RawData/BetasPhen2020.RData')


# Combined batch2017 and batch2020 ####
setdiff(row.names(sub_2017), row.names(sub_2020))
betas = cbind(sub_2017, sub_2020)
pData = rbind(pDataSorted2017, pDataSorted2020)
setdiff(colnames(betas), pData$id)
save(pData, file = '1_RawData/pDataSortedCombined.RData')
save(betas, pData, file = '1_RawData/BetasPhenCombined.RData')

# only for wave2 in 2020 ####
pData2020 <- read.delim("1_RawData/OMIC-49-SUGDEN_epi_2020_sendout.txt", sep="\t", header=TRUE, stringsAsFactors=FALSE)
pDataWave2 = pData2020[which(pData2020$wave == 2), c('id', 'confage', 'nsex', 'rackbarcode', 'cd8t', 'cd4t', 'nk', 'bcell', 'mono',
                                                     'b_sf1','b_scsf1', 'b_scsf2a', 'b_scsf2b', 'b_scsf3a', 'b_scsf3b', 'b_scsf5', 
                                                     'b_scsf7', 'b_smcigs', 'b_smnow')]

colnames(pDataWave2) <- c('id', 'Age_numeric', 'Sex_factor', 'Rackbarcode_factor', 
                     'cd8t', 'cd4t', 'nk', 'bcell', 'mono',
                     'sf1','scsf1', 'scsf2a', 'scsf2b', 'scsf3a', 'scsf3b', 'scsf5', 'scsf7', 
                     'smcigs', 'smnow')


pDataWave2$Age_numeric = as.numeric(pDataWave2$Age_numeric)
pDataWave2$Sex_factor = as.factor(pDataWave2$Sex_factor)
pDataWave2$Rackbarcode_factor = as.factor(pDataWave2$Rackbarcode_factor)

pData = pDataWave2

setdiff(pData$id, colnames(sub_2020))
setdiff(colnames(sub_2020), pData$id)
betas = sub_2020[,match(pData[,1], colnames(sub_2020))]

setdiff(pData$id, colnames(betas))
setdiff(colnames(betas), pData$id)

save(pData, file = '1_RawData/pDataSortedWave2.RData')
save(betas, pData, file = '1_RawData/BetasPhenWave2.RData')




