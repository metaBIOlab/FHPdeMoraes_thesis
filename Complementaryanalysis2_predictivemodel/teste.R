# ROC CURVE ANALYSIS

trainIndex <- createDataPartition(dados_hemi_v1_CH$SUBJ, p = .8, 
                                  list = FALSE, 
                                  times = 2, groups = min(5, length(dados_hemi_v1_CH$SUBJ)))

head(trainIndex)
irisTrain <- dados_hemi_v1_CH[ trainIndex,]
irisTest  <- dados_hemi_v1_CH[-trainIndex,]

GLM_DIAG <- glm(K ~ Diagnostic, data = dados_hemi_v1)

summary(GLM_DIAG)
confusionMatrix()


folds <- groupKFold(dados_hemi_v1_CH$SUBJ, k = 2)
dados_hemi_v1_CHtraining <- dados_hemi_v1_CH[ folds$Fold1,]
dados_hemi_v1_CHtesting <- dados_hemi_v1_CH[ folds$Fold2,]

GLM_training <- glm(K ~ Diagnostic, data = dados_hemi_v1_CHtraining, family = binomial)
pdata <- predict(GLM_training, newdata = dados_hemi_v1_CHtraining, type = "response")
confusionMatrix(data = pdata, reference = dados_hemi_v1_CHtesting$K)
