str(cleaned_data)
summary(cleaned_data)
library(FactoMineR)
df=cleaned_data
colnames(df)
colnames(df)[colnames(df) == "Value(£)"] <- "value"
library(plyr)
summary_values=ddply(df, c("Club"), summarise,
      somme_valeurs=sum(value),
       moyenne_age=mean(Age)

df$value <- as.numeric(df$value)
df$Age <- as.numeric(df$Age)
df$Potential <- as.numeric(df$Potential)
df$`Contract Valid Until` <- as.numeric(df$`Contract Valid Until`)
df$`Release Clause(£)` <- as.numeric(df$`Release Clause(£)`)
df$`Height(cm.)` <- as.numeric(df$`Height(cm.)`)
variables_numeriques <- df[, c("Age", "Potential", "value", "Contract Valid Until", "Height(cm.)", "Release Clause(£)")]
df_normalise=scale(variables_numeriques)
X=cbind.data.frame(variables_numeriques, df$ID,df$Name,df$Nationality,df$Club,df$`Lieu du club`,df$`Preferred Foot`,df$`Work Rate`,df$`Body Type`,df$Position,df$Joined,df$`Weight(lbs.)`,df$Year_Joined)
head(X)
View(X)
colnames(df)

library(reshape2)
library(ggplot2)

# Charger les packages nécessaires
library(reshape2)
library(ggplot2)

# Supposons que votre jeu de données s'appelle df
# Nous allons convertir le jeu de données en format long avec melt
dfmelt <- melt(df, measure.vars = c("Age", "Potential", "value", "Contract Valid Until", "Height(cm.)", "Weight(lbs.)", "Release Clause(£)"))

# Créer les boxplots

gr <- ggplot(dfmelt, aes(y = value, x = Club, fill = Club)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1));

# Afficher les boxplots
print(gr)

library(corrgram)
# Calculer la matrice des corrélations
matrice_corr <- cor(df[, c("Age", "Potential", "value", "Contract Valid Until", "Height(cm.)", "Weight(lbs.)", "Release Clause(£)")])

# Afficher la matrice des corrélations
print(matrice_corr)
mean(matrice_corr)

# Tracer le corrélogramme
library(corrplot)
corrplot(matrice_corr, method = "circle")

# La matrice des corrélations nous permet d'examiner les relations linéaires entre les variables quantitatives.
# Les valeurs de la matrice représentent les coefficients de corrélation de Pearson, variant de -1 à 1.

# Commentaires sur les corrélations :
# - L'âge semble être négativement corrélé avec le potentiel (-0.38) et la validité du contrat (-0.39), indiquant que les joueurs plus âgés ont tendance à avoir un potentiel et des contrats moins favorables.
# - Le potentiel est fortement corrélé avec la valeur (0.81) et la clause de libération (0.80), suggérant que les joueurs avec un potentiel plus élevé ont tendance à avoir une valeur et une clause de libération plus élevées.
# - La valeur est également fortement corrélée avec la clause de libération (0.98), ce qui est attendu.
# - La taille (en cm) présente une corrélation modérée positive avec le poids (0.62), ce qui est logique.
# - Aucune corrélation évidente n'est observée entre le poids et d'autres variables, à l'exception de la taille.

# Ces résultats nous aident à comprendre les relations entre les variables quantitatives dans notre ensemble de données, ce qui peut orienter nos analyses ultérieures et nos décisions de modélisation.

# Nuage de points entre l'âge et la potentiel
plot(df$Age, df$Potential, 
     main = "Nuage de points entre l'âge et la valeur",
     xlab = "Âge", ylab = "potentiel")


cov_matrix <- cov(variables_numeriques)


# Normalisation des variables
library(FactoMineR)
# Utilisez votre propre ensemble de données FIFA
View(df)

Y <- as.matrix(df[, c("Age", "Potential", "value", "Contract Valid Until", "Height(cm.)", "Weight(lbs.)", "Release Clause(£)")])

g <- colMeans(Y)
Y <- sweep(Y, MARGIN = 2, STATS = g, FUN = "-")
Y <- scale(Y)

# Décomposition en valeurs singulières (SVD)
n <- nrow(Y)  # nombre d'individus
p <- ncol(Y)  # nombre de variables
D <- diag(rep(1/n, n))
V <- t(Y) %*% D %*% Y
vs <- eigen(V)
lambda <- vs$values  # valeurs propres
vp <- vs$vectors  # vecteurs propres
C <- Y %*% vp  # matrice des composantes principales
View(C)
# Analyse en composantes principales (ACP)
quantitative_vars <- c("Age", "Potential", "value", "Contract Valid Until", "Height(cm.)", "Weight(lbs.)", "Release Clause(£)")
cleaned_data_quantitative <- df[, quantitative_vars]
library(FactoMineR)
res.pca <- PCA(cleaned_data_quantitative)
# Étude de l'inertie
res.pca$eig

# Graphique des valeurs propres
barplot(res.pca$eig[, 1], main = "Éboulis des valeurs propres", names.arg = paste("lambda", 1:nrow(res.pca$eig)))

# Coordonnées, cos2, et contributions des individus et des variables
res.pca$ind$coord
res.pca$ind$cos2
res.pca$ind$contrib

res.pca$var$coord
res.pca$var$contrib
res.pca$var$cos2

# Visualisation des résultats de l'ACP
library(factoextra)
fviz_pca_ind(res.pca, geom.ind = c("text", "point"), repel = TRUE, col.ind = "contrib")
fviz_pca_var(res.pca, col.var = "cos2", repel = TRUE)

df_normalized <- scale(df[, c("Age", "Potential", "value", "Contract Valid Until", "Height(cm.)", "Weight(lbs.)", "Release Clause(£)")])

# Réaliser la CAH
hc_fifa <- hclust(dist(df_normalized), method = "ward.D")
# 2. Générer toutes les sorties de la fonction hclust :
library(cluster)
summary(hc_fifa)
plot(hc_fifa)
hc_fifa <- hclust(dist(df_normalized), method = "ward.D")
plot(hc_fifa, hang = -1)
classes <- cutree(hc_fifa, k = 4)
# Boxplot de la valeur des joueurs par club
ggplot(df, aes(x = Club, y = value)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Boxplot de la valeur des joueurs par club")

# Nuage de points entre l'âge et la valeur des joueurs
ggplot(df, aes(x = Age, y = value)) +
  geom_point() +
  labs(title = "Nuage de points entre l'âge et la valeur des joueurs", x = "Âge", y = "Valeur")

# Matrice de corrélation
correlation_matrix <- cor(df[, c("Age", "Potential", "value", "Contract Valid Until", "Height(cm.)", "Weight(lbs.)", "Release Clause(£)")])
corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black")

# Modélisation
# Choix du modèle - Exemple de régression linéaire
linear_model <- lm(value ~ Age + Potential + Position, data = df)

# Évaluation du modèle
summary(linear_model)

# Prédiction
predicted_values <- predict(linear_model, newdata = df)

# Évaluation de la performance du modèle
RMSE <- sqrt(mean((df$value - predicted_values)^2))
R_squared <- cor(df$value, predicted_values)^2
cat("RMSE:", RMSE, "\n")
cat("R-squared:", R_squared, "\n")


