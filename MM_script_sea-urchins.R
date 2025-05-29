# Load required libraries
library(readxl)
library(FactoMineR)
library(ggplot2)
library(dplyr)

# --- Load and preprocess data ---
data <- read_excel("C:/Users/mauri/Downloads/Literature review/Depressus2.xlsx") %>%
  filter(!is.na(TL), TL > 0) %>%
  mutate(
    angle_deg = as.numeric(gsub("\u00b0", "", degrees)),
    angle_rad = angle_deg * pi / 180,
    TW_rel = TW / TL,
    TH_rel = TH / TL,
    Mw_rel = Mw / TL,
    Aw_rel = Aw / TL,
    Ah_rel = Ah / TL
  )

# --- Subset Meerssen and Nekum ---
data_mn <- data %>%
  filter(Member %in% c("Meerssen", "Nekum")) %>%
  mutate(Group = Member)

# --- PCA for Meerssen and Nekum ---
pca_input_mn <- data_mn %>% select(TW_rel, TH_rel, Mw_rel, Aw_rel, Ah_rel, angle_rad)
pca_model_mn <- PCA(pca_input_mn, graph = FALSE, ncp = 5)
data_mn <- data_mn %>%
  mutate(
    PC1 = pca_model_mn$ind$coord[, 1],
    PC2 = pca_model_mn$ind$coord[, 2],
    PC3 = pca_model_mn$ind$coord[, 3],
    PC4 = pca_model_mn$ind$coord[, 4]
  )

# --- PCA Plot: PC1 vs PC2 ---

# --- Factor Loadings for PCs (Meerssen and Nekum only) ---
barplot(pca_model_mn$var$contrib[, 1], main = "Factor Loadings for PC1 (MN Only)",
        xlab = "Variables", ylab = "Contribution (%)",
        names.arg = colnames(pca_input_mn), las = 2)

barplot(pca_model_mn$var$contrib[, 2], main = "Factor Loadings for PC2 (MN Only)",
        xlab = "Variables", ylab = "Contribution (%)",
        names.arg = colnames(pca_input_mn), las = 2)
ggplot(data_mn, aes(x = PC1, y = PC2, color = Group, shape = Group)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Meerssen" = "blue", "Nekum" = "red")) +
  scale_shape_manual(values = c("Meerssen" = 16, "Nekum" = 17)) +
  labs(title = "PCA: Meerssen + Nekum", x = "PC1", y = "PC2") +
  theme_minimal()

# --- PCA Plot: PC3 vs PC4 for Meerssen + Nekum only ---
ggplot(data_mn, aes(x = PC3, y = PC4, color = Group, shape = Group)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Meerssen" = "blue", "Nekum" = "red")) +
  scale_shape_manual(values = c("Meerssen" = 16, "Nekum" = 17)) +
  labs(title = "PCA: PC3 vs PC4 (Meerssen + Nekum)", x = "PC3", y = "PC4") +
  theme_minimal()


# --- View percentage of variance explained for each PC ---
pca_var_percent <- pca_model_mn$eig[, 2]  # Column 2 contains % of variance
names(pca_var_percent) <- paste0("PC", 1:length(pca_var_percent))
print(pca_var_percent[1:4])  # Show PC1 to PC4

# Test for normality (optional)
shapiro.test(data$Ah_rel)
shapiro.test(data$angle_rad)
shapiro.test(data$TH_rel)

# Pearson correlation (use if all normal)
cor.test(data$Ah_rel, data$TH_rel, method = "pearson")
cor.test(data$angle_rad, data$TH_rel, method = "pearson")

# OR Spearman correlation (use if not normal)
cor.test(data$Ah_rel, data$TH_rel, method = "spearman")
cor.test(data$angle_rad, data$TH_rel, method = "spearman")

# --- Filter data: Include all three groups ---
data_pca <- data %>%
  filter(Member %in% c("Meerssen", "Nekum", "Echinocorys"), !is.na(TL), TL > 0) %>%
  mutate(
    angle_deg = as.numeric(gsub("°", "", degrees)),
    angle_rad = angle_deg * pi / 180,
    TH_rel = TH / TL,
    Mw_rel = Mw / TL,
    Aw_rel = Aw / TL,
    Ah_rel = Ah / TL,
    Group = Member
  )

# --- Select variables for PCA ---
pca_vars <- data_pca %>%
  select(TW_rel = TW / TL,
         TH_rel,
         Mw_rel,
         Aw_rel,
         Ah_rel,
         angle_rad)

# --- Run PCA ---

library(factoextra)

pca_model_all <- PCA(pca_vars, graph = FALSE, ncp = 5)

# --- Add PCA scores to dataset ---
data_pca$PC1 <- pca_model_all$ind$coord[, 1]
data_pca$PC2 <- pca_model_all$ind$coord[, 2]
data_pca$PC3 <- pca_model_all$ind$coord[, 3] 
data_pca$PC4 <- pca_model_all$ind$coord[, 4]
# --- Plot PCA with Echinocorys included ---


ggplot(data_pca, aes(x = PC1, y = PC2, color = Group, shape = Group)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "PCA: Meerssen, Nekum, and Echinocorys",
       x = "PC1", y = "PC2") +
  scale_color_manual(values = c("Meerssen" = "blue", "Nekum" = "red", "Echinocorys" = "darkgreen")) +
  scale_shape_manual(values = c("Meerssen" = 16, "Nekum" = 17, "Echinocorys" = 15)) +
  theme_minimal() 

ggplot(data_pca, aes(x = PC3, y = PC4, color = Group, shape = Group)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "PCA: Meerssen, Nekum, and Echinocorys",
       x = "PC3", y = "PC4") +
  scale_color_manual(values = c("Meerssen" = "blue", "Nekum" = "red", "Echinocorys" = "darkgreen")) +
  scale_shape_manual(values = c("Meerssen" = 16, "Nekum" = 17, "Echinocorys" = 15)) +
  theme_minimal() 



explained_var <- vis.pca$eig[, 2]
names(explained_var) <- paste0("PC", 1:length(explained_var))
print("Percentage of variance explained:")
print(explained_var)

# --- Normality and group comparisons (Meerssen and Nekum only) ---
shapiro.test(data_mn$Ah_rel)
shapiro.test(data_mn$angle_rad)
wilcox.test(Ah_rel ~ Group, data = data_mn)
t.test(angle_rad ~ Group, data = data_mn)

#check for PC greatest separation between groups and separation within groups
data_mn %>%
  group_by(Group) %>%
  summarise(
    PC1_sd = sd(PC1),
    PC2_sd = sd(PC2),
    PC1_var = var(PC1),
    PC2_var = var(PC2)
  )


# Compute group means for PC1 to PC4
group_means <- data_mn %>%
  group_by(Group) %>%
  summarise(
    mean_PC1 = mean(PC1),
    mean_PC2 = mean(PC2),
    mean_PC3 = mean(PC3),
    mean_PC4 = mean(PC4),
    .groups = "drop"
  )

# Transpose for easier comparison
pc_separation <- group_means %>%
  pivot_longer(cols = starts_with("mean"), names_to = "PC", values_to = "mean_value") %>%
  pivot_wider(names_from = Group, values_from = mean_value) %>%
  mutate(abs_diff = abs(Meerssen - Nekum))

# View the result
pc_separation

# --- Geometric morphometrics---

# Load libraries
library(tidyverse)
library(geomorph)
library(Morpho)

# Define path to your landmark CSV files
file_paths <- list.files("C:/Users/mauri/Downloads/Master documenten", pattern = "\\.csv$", full.names = TRUE)

group_assignments <- tibble(
  filename = c(
    "NA.csv", "MK860a4.csv", "MK823_54.csv", "MK823_43.csv", "MK823_41.csv", "MK823_39.csv",
    "MK823_35.csv", "MK823_34.csv", "MK653e.csv", "MK567d.csv", "MK4261.csv", "MK3552.csv",
    "MK3484.csv", "MK3476.csv", "MK30_correct.csv", "MK3031.csv", "MK2693.csv", "MK2408.csv",
    "MK2406.csv", "MK2319.csv", "MK1097.csv", "MK1677.csv", "MA140.csv", "BL308.csv"
  ),
  group = c(
    "Meerssen", "Nekum", "Nekum", "Nekum", "Nekum", "Nekum",
    "Nekum", "Nekum", "Nekum", "Nekum", "Meerssen", "Meerssen",
    "Meerssen", "Meerssen", "Meerssen", "Meerssen", "Meerssen", "Nekum",
    "Nekum", "Meerssen", "Meerssen", "Nekum", "Meerssen", "Meerssen"
  )
)


# Function to read X, Y, Z landmark coordinates
read_landmarks <- function(file) {
  lines <- readLines(file)
  lines <- lines[3:length(lines)]  # skip metadata/header lines
  df <- read_csv(paste(lines, collapse = "\n"),
                 col_names = c("Index", "Name", "Type", "X", "Y", "Z", "NX", "NY", "NZ"),
                 show_col_types = FALSE)
  coords <- df %>% select(X, Y, Z) %>% as.matrix()
  return(coords)
}

# Read all landmark files into list
landmark_list <- lapply(file_paths, read_landmarks)

# Get dimensions
num_landmarks <- nrow(landmark_list[[1]])
num_specimens <- length(landmark_list)

# Combine into 3D array (landmarks x 3 coords x specimens)
landmarks_array <- array(unlist(landmark_list), dim = c(num_landmarks, 3, num_specimens))

print(landmark_list)
# Run Generalized Procrustes Analysis
gpa <- gpagen(landmarks_array)
print(gpa)
# Run PCA on aligned coordinates
pca <- gm.prcomp(gpa$coords)
print(summary(pca))
# Prepare PCA dataframe
specimen_names <- basename(file_paths)
groups <- group_assignments$group[match(specimen_names, group_assignments$filename)]

pca_scores <- as_tibble(pca$x[, 1:2]) %>%
  rename(PC1 = 1, PC2 = 2) %>%
  mutate(Specimen = specimen_names, Group = groups)

#PCA showing sizes and Members
text(pca_scores$PC1,pca_scores$PC2,labels=groups) 

test2=CAC(gpa$coords,gpa$Csize)
plot(gpa$Csize,test2$CACscores) 


# Compute variance within each group


pca_scores %>%
  group_by(Group) %>%
  summarise(
    var_PC1 = var(PC1),
    var_PC2 = var(PC2)
  )

str(groups) 

pca_scores$PC3 <- pca$x[, 3]
pca_scores$PC4 <- pca$x[, 4]
text(pca_scores$PC3, pca_scores$PC4, labels = groups)
# Plot the points
plot(pca_scores$PC3, pca_scores$PC4,
     xlab = "PC3", ylab = "PC4",
     pch = 21, col = "black", bg = as.numeric(groups))

# Add the group labels on top of the points
text(pca_scores$PC3, pca_scores$PC4, labels = groups, pos = 3, cex = 0.7)

text(pca_scores$PC3, pca_scores$PC4, labels = groups, pos = 3, cex = 0.7)


# PCA scatter plot
ggplot(pca_scores, aes(x = PC1, y = PC2, color = Group, shape = Group, label = Specimen)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  scale_color_manual(values = c("Meerssen" = "darkblue", "Nekum" = "darkred")) +
  scale_shape_manual(values = c("Meerssen" = 16, "Nekum" = 17)) +
  theme_minimal() +
  labs(title = "PCA of Landmark Data",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Group", shape = "Group")

test=geomorph.data.frame(shape=gpa$coords,size=gpa$Csize,groups=groups)
fit <- procD.lm(shape ~ size*groups, data = test,iter = 999, RRPP = TRUE,SS.type = "I")
summary(fit)

# Perform Procrustes ANOVA to test for group differences
fit <- procD.lm(gpa$coords ~ gpa$Csize+groups, iter = 999, RRPP = TRUE )
summary(fit)

# Procrustes ANOVA: shape ~ size
fit_size <- procD.lm(gpa$coords ~ Csize, data = metadata, iter = 999)
summary(fit_size)

# Procrustes ANOVA: shape ~ group
fit_group <- procD.lm(gpa$coords ~ Group, data = metadata, iter = 999)
summary(fit_group)

# Procrustes ANOVA: shape ~ size + group (combined model)
fit_both <- procD.lm(gpa$coords ~ Csize + Group, data = metadata, iter = 999)
summary(fit_both)

summary(lm(pca_scores$PC1 ~ gpa$Csize))

# Plot CAC with labels
plot(gpa$Csize, test2$CACscores, pch = 21, bg = as.numeric(groups), cex = 1.5)
text(gpa$Csize, test2$CACscores, labels = specimen_names, pos = 3, cex = 0.7)



# --- Volume analysis ---

# raw data
plot(volumetric_measures_scanner$surfacearea, volumetric_measures_scanner$volume,
     xlab = "Oppervlakte (draagvlak)",
     ylab = "Volume",
     main = "Volume vs Oppervlakte",
     pch = 19, col = "steelblue")

# Linear model
lm_fit <- lm(volume ~ surfacearea, data = volumetric_measures_scanner)
abline(lm_fit, col = "red", lwd = 2)
summary(lm_fit)

# Log-log model
log_surfacearea <- log(volumetric_measures_scanner$surfacearea)
log_volume <- log(volumetric_measures_scanner$volume)

plot(log_surfacearea, log_volume,
     xlab = "Log(Oppervlakte)",
     ylab = "Log(Volume)",
     main = "Log-Log Plot: Volume vs Oppervlakte",
     pch = 19, col = "steelblue")

log_lm_fit <- lm(log_volume ~ log_surfacearea)
abline(log_lm_fit, col = "red", lwd = 2)
summary(log_lm_fit)
