# Installing packages and accessing the library to calculate MODA
install.packages("modeest")
library(modeest)

# Installing packages and accessing libraries to generate graphics
install.packages("ggplot2")
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

# Importando a base de dados
bd <- read.table("BD1.csv", header=TRUE, sep=";")

# Central tendency and dispersion measures

# Age
cat("AGE 
    Mean:", mean(bd$idade),
    "Median:", median(bd$idade),
    "Moda:", mlv(bd$idade),
    "Standard Deviation:", sd(bd$idade),
    "Variance:", sd(bd$idade) * sd(bd$idade))

# Last salary amount
cat("LAST SALARY VALUE 
    Mean:", mean(bd$valor.do.último.salário),
    "Median:", median(bd$valor.do.último.salário),
    "Moda:", mlv(bd$valor.do.último.salário),
    "Standard Deviation:", sd(bd$valor.do.último.salário),
    "Variance:", sd(
      bd$valor.do.último.salário) * sd(bd$valor.do.último.salário))

# Length of professional experience
cat("PROFESSIONAL EXPERIENCE LENGTH
    Mean:", mean(bd$tempo.de.experiência.profissional),
    "Median:", median(bd$tempo.de.experiência.profissional),
    "Moda:", mlv(bd$tempo.de.experiência.profissional),
    "Standard Deviation:", sd(bd$tempo.de.experiência.profissional),
    "Variance:", sd(
      bd$tempo.de.experiência.profissional) * sd(bd$tempo.de.experiência.profissional))

# Length of experience in the role
cat("TIME OF EXPERIENCE IN THE ROLE
    Mean:", mean(bd$tempo.de.experiência.na.função),
    "Median:", median(bd$tempo.de.experiência.na.função),
    "Moda:", mlv(bd$tempo.de.experiência.na.função),
    "Standard Deviation:", sd(bd$tempo.de.experiência.na.função),
    "Variance:", sd(
      bd$tempo.de.experiência.na.função) * sd(bd$tempo.de.experiência.na.função))

# Bar Chart of the Education variable
bd %>%
  group_by(escolaridade) %>%
  summarise(
    count_escolaridade = n()
  ) %>%
  
  # Bar Chart
  ggplot(aes(x = escolaridade, y = count_escolaridade, label = count_escolaridade)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_label(size = 5) +
  coord_flip()

