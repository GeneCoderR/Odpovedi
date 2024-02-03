# knihovna
library(dplyr)

# Nahrani data
data <- read.csv("/home/tareq/Repromeda/transfery.csv", stringsAsFactors = FALSE)

head(data)
#######################################################################################################################
# A) Dle věku matky “vek_mother”, ve věkových kategoriích viz tabulka, vytvořte tabulku úspěšnosti embryotransferu v procentech dle sloupce “clinical_gravidity”, 
# kde 1 = transfer byl úspěšný a 0 = neúspěšný. Prázdné hodnoty do statistik nepočítejte.
#######################################################################################################################
# Cisteni dat
data$vek_mother <- as.numeric(as.character(data$vek_mother))

filtered_data <- na.omit(data[c('vek_mother', 'clinical_gravidity')])

# Definovani veku, kategorizace
filtered_data$age_category <- cut(filtered_data$vek_mother, breaks = c(-Inf, 29, 34, 39, Inf),  labels = c('do 29', '30-34', '35-39', '40 a výše'), include.lowest = TRUE
)

# Spojeni age_category a sumarizace uspesnosti (success_rate)
grouped_data <- group_by(filtered_data, age_category)
cleaned_data <- summarise(grouped_data, success_rate = mean(clinical_gravidity, na.rm = TRUE) * 100)

# Pridani vsech kategorii
all_cat <- data.frame(age_category = 'všechny věkové kategorie', 
                      success_rate = mean(cleaned_data$success_rate, na.rm = TRUE))

# Spojeni s predchozi tabulkou
final_success_rates <- rbind(all_cat, cleaned_data)

print(final_success_rates)


#######################################################################################################################
#B) Urcete zda-li je věk matky statisticky významný na úspěch transferu.
#######################################################################################################################
model <- glm(clinical_gravidity ~ vek_mother, data=data, family="binomial")

# Summary of the model to see results
summary(model)


library(ggplot2)

# Vytvoření pravděpodobnost
filtered_data$predicted_prob <- predict(model, type = "response")

# Graf
ggplot(filtered_data, aes(x = vek_mother, y = predicted_prob)) +
  geom_point(aes(color = as.factor(clinical_gravidity)), alpha = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "Věk matky", y = "Predikovaná pravděpodobnost úspěšné gravidity",
       color = "Klinická gravidita") +
  theme_minimal()



#######################################################################################################################
# C)Taktéž A-B proveďte i pro věk embrya “vek_embryo”. Pokud bylo embryo darované ”f_donor” = 1, takový transfer do statistiky nepočítejte.
#######################################################################################################################
# Nahrani data
data <- read.csv("/home/tareq/Repromeda/transfery.csv", stringsAsFactors = FALSE)
data <- data[!data$f_donor == 1 | is.na(data$f_donor), ] # Vyfiltruje f_donor = 1 

# Převod na číselný typ
# Cisteni dat
data$vek_embryo <- as.numeric(as.character(data$vek_embryo))

e_filtered_data <- na.omit(data[c('vek_embryo', 'clinical_gravidity')])

# Provedeni logisticke regrese pro vek embrya
model_embryo <- glm(clinical_gravidity ~ vek_embryo, data=data, family=binomial)

# Zobrazeni vysledku
summary(model_embryo)

#Graf
# Vytvoření pravděpodobnost
e_filtered_data$predicted_prob <- predict(model_embryo, type = "response")

# Graf
ggplot(e_filtered_data, aes(x = vek_embryo, y = predicted_prob)) +
  geom_point(aes(color = as.factor(clinical_gravidity)), alpha = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "Věk embrya", y = "Predikovaná pravděpodobnost úspěšné gravidity",
       color = "Gravidita") +
  theme_minimal()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Definování mezníků pro věkové kategorie a jejich popisků
bins <- c(-Inf, 29, 34, 39, Inf)
labels <- c('do 29', '30-34', '35-39', '40 a výše')

# Použití funkce cut k rozdělení věku embrya do kategorií
e_filtered_data$age_category_embryo <- cut(e_filtered_data$vek_embryo, breaks=bins, labels=labels, include.lowest = TRUE)

grouped_data <- group_by(e_filtered_data, age_category_embryo)
cleaned_data <- summarise(grouped_data, success_rate = mean(clinical_gravidity, na.rm = TRUE) * 100)

overall <- data.frame(age_category_embryo = 'všechny věkové kategorie', 
                      success_rate = mean(cleaned_data$success_rate, na.rm = TRUE))

final_success_rates <- rbind(overall, cleaned_data)

print(final_success_rates)



#######################################################################################################################
# D) Vytvořte tabulku s počty transferů dle použité genetické metody "genetic_method” viz tabulka. #####################
#######################################################################################################################


data <- read.csv("/home/tareq/Repromeda/transfery.csv", stringsAsFactors = FALSE)

data$genetic_method[data$genetic_method == ""] <- NA
# Jenom prázdne hodnoty bez geneticke metody
data$genetic_method[is.na(data$genetic_method)] <- 'bez genetické metody'

# Specifické gen. metody + ostatni
specific_methods <- c('PGT-A', 'PGT-SR', 'Karyomapping', 'OneGene', 'bez genetické metody')
data$genetic_method[!data$genetic_method %in% specific_methods] <- 'ostatní'

# Tabulka s metodami
table_genetic_method <- table(data$genetic_method)

# Prepnuti na data.frame
df_genetic_method <- as.data.frame(table_genetic_method)

# Výpis výsledků
print(df_genetic_method)
sum(df_genetic_method$Freq)


#######################################################################################################################
#E) Určete statistickou významnost pohlaví embrya “sex” – XX/XY na úspěch klinické gravidity dle sloupce “clinical_gravidity”,
#kde 1 = transfer byl úspěšný a 0 = neúspěšný. Prázdné hodnoty do statistik nepočítejte.
#######################################################################################################################

data <- read.csv("/home/tareq/Repromeda/transfery.csv", stringsAsFactors = FALSE)
data <- na.omit(data[, c('clinical_gravidity', 'sex')])  # Odstranění řádků s chybějícími hodnotami

data$sex <- as.factor(data$sex)

model_sex <- glm(clinical_gravidity ~ sex, data=data, family=binomial)

summary(model_sex)



# F) Z výsledných tabulek z úkolu A a D vytvořte a uložte grafy ve formátu .png, kde na ose x bude první a na ose y druhý řádek tabulky.

# 1graf
# '

ggplot(final_success_rates, aes(x=age_category_embryo, y=success_rate)) +
  geom_bar(stat='identity', fill='skyblue') +
  labs(x='Věková kategorie matky', y='Úspěšnost embryotransferu (%)') +
  ggtitle('Úspěšnost embryotransferu podle věku matky') +
  theme_minimal()

ggsave('success_rate_by_age.png', plot = last_plot(), width = 8, height = 6, dpi = 300)

# 2graf
#

ggplot(df_genetic_method, aes(x=Var1, y=Freq)) +
  geom_bar(stat='identity', fill='coral') +
  labs(x='Genetická metoda', y='Počet transferů') +
  ggtitle('Počet transferů podle genetické metody') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotace popisků na ose X pro lepší čitelnost

ggsave('transfer_count_by_method.png', plot = last_plot(), width = 8, height = 6, dpi = 300)


#######################################################################################################################
########BONUS Vytvořte jednoduchou R shiny aplikaci, která udělá předchozí statistiky při spuštění a	vykreslí tabulky nebo hodnoty, tak aby je uživatel viděl.
#######################################################################################################################

library(shiny)

# Definice UI
ui <- fluidPage(
  titlePanel("Analýza úspěšnosti embryotransferu a genetických metod"),
  
  # Panel pro zobrazení grafu úspěšnosti embryotransferu podle věku matky
  h4("Úspěšnost embryotransferu podle věku matky"),
  plotOutput("plotAgeSuccess"),
  
  # Panel pro zobrazení grafu počtu transferů podle genetické metody
  h4("Počet transferů podle genetické metody"),
  plotOutput("plotMethodCount")
)

# Server logika
server <- function(input, output) {
  
  # Graf úspěšnosti embryotransferu podle věku matky
  output$plotAgeSuccess <- renderPlot({
    ggplot(final_success_rates, aes(x=age_category_embryo, y=success_rate)) +
      geom_bar(stat='identity', fill='skyblue') +
      labs(x='Věková kategorie matky', y='Úspěšnost embryotransferu (%)') +
      theme_minimal()
  })
  
  # Graf počtu transferů podle genetické metody
  output$plotMethodCount <- renderPlot({
    ggplot(df_genetic_method, aes(x=Var1, y=Freq)) +
      geom_bar(stat='identity', fill='coral') +
      labs(x='Genetická metoda', y='Počet transferů') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Spuštění aplikace
shinyApp(ui = ui, server = server)

#######################################################################################################################
##Vytvořte script, který bude vytvářet jednoduchý .docx dokument obsahující nadpis, tučným a zarovnaný na střed “Výsledný protokol genetického vyšetření”, 
##a dále pak tabulku, která se vyplní dle tří vstupních argumentů následovně:#############################################################################
#########################################################################################################################
#######################################################################################################################

create_document <- function(arg1, arg2, arg3) {
  # Vytvoření nového Word dokumentu
  doc <- read_docx() %>%
    # Přidání nadpisu
    body_add_par("Výsledný protokol genetického vyšetření", style = "heading 1") %>%
    # Přidání mezery
    body_add_par(" ") %>% body_add_par("") %>% body_add_par("") %>%
    # Přidání tabulky
    body_add_table(data.frame(
        ' ' = c('Jméno a příjmení:', 'Rodné číslo:', 'Datum odběru:'),
        ' ' = c(arg1, arg2, arg3)
      ),
      style = "table_template",
      no_hband = TRUE,
      no_vband = TRUE
    )
  
  # Uložení dokumentu
  print(doc, target = "Výsledný protokol genetického vyšetření.docx")
}

# Můžete nyní zavolat funkci s konkrétními argumenty
create_document("argument1", "argument2", "argument3")


















library(dplyr)

# Load the data
data <- read.csv("/home/tareq/Repromeda/transfery.csv", stringsAsFactors = FALSE)

# Convert 'vek_mother' to numeric and drop rows with NA values for 'vek_mother' and 'clinical_gravidity'
cleaned_data <- data %>%
  mutate(vek_mother = as.numeric(vek_mother)) %>%
  drop_na(vek_mother, clinical_gravidity) %>%
  mutate(
    age_category = cut(vek_mother, 
                       breaks = c(-Inf, 29, 34, 39, Inf), 
                       labels = c('do 29', '30-34', '35-39', '40 a výše'), 
                       include.lowest = TRUE)
  ) %>%
  group_by(age_category) %>%
  summarise(success_rate = mean(clinical_gravidity, na.rm = TRUE) * 100)

# Add overall success rate
overall <- data.frame(age_category = 'všechny věkové kategorie', 
                  success_rate = mean(cleaned_data$clinical_gravidity, na.rm = TRUE) * 100)
final_success_rates <- bind_rows(overall, cleaned_data)

print(final_success_rates)




data <- read.csv("/home/tareq/Repromeda/transfery.csv", stringsAsFactors = FALSE)
data <- na.omit(data)  # Odstranění chybějících hodnot
data$vek_mother <- as.numeric(data$vek_mother)  # Převod na číselný typ

bins <- c(0, 29, 34, 39, Inf)
labels <- c('do 29', '30-34', '35-39', '40 a výše')


