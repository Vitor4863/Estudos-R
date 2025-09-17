# -------------------------------
# 1. Carregar pacotes
# -------------------------------
library(readxl)   # Para ler Excel
library(dplyr)    # Para manipulação de dados

# -------------------------------
# 2. Ler a planilha
# -------------------------------
dados <- read_excel("C:/Users/Vitor/Downloads/Planilhas/Vendas.xlsx")

# -------------------------------
# 3. Verificar os dados iniciais
# -------------------------------
head(dados)      # Mostra as 6 primeiras linhas
str(dados)       # Estrutura do data frame
summary(dados)   # Resumo estatístico
colSums(is.na(dados))  # Ver quantos NA existem por coluna

# -------------------------------
# 4. Renomear colunas para facilitar
# -------------------------------
names(dados) <- c("Datavenda", "Produto", "Categoria", "PrecoUnitario", 
                  "Marca", "QtdVendida", "Nome", "Sobrenome", "Pais", "Continente")

# -------------------------------
# 5. Criar coluna ValorTotal
# -------------------------------
dados <- dados %>%
  mutate(ValorTotal = PrecoUnitario * QtdVendida)

dados <- dados %>% 
   mutate(precoComDesconto = PrecoUnitario * 0.9)

dados <- dados %>% 
  mutate(Classificacao = ifelse(ValorTotal > 1000, "Alta" ,"Baixa"))


dados <- dados %>% 
  mutate(Cliente = paste(Nome, Sobrenome))

total_por_cliente <- dados %>%
  group_by(Cliente)%>%
  summarise(totalGasto = sum(ValorTotal)) %>%
  arrange(desc(totalGasto))

dados <- dados %>%
  group_by(Cliente) %>%
  mutate(totalgastoCliente = sum(ValorTotal)) %>%
  ungroup()

# -------------------------------
# 6. Visualizar as primeiras linhas
# -------------------------------
head(dados)   # mostra as primeiras 6 linhas
View(dados)   # abre a tabela em uma aba tipo planilha
