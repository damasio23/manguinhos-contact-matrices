# 🦠 Análise de Matrizes de Contato Suavizadas no Complexo de Manguinhos (RJ)

Este repositório contém o código R para o trabalho de conclusão de curso (TCC) intitulado **Análise de Matrizes de Contato Suavizadas no Complexo de Manguinhos**.

O objetivo do estudo é construir e validar matrizes de contato específicas para este território vulnerável do Rio de Janeiro, utilizando dados do inquérito sorológico COMVIDA. Tais matrizes são essenciais para estimar parâmetros epidemiológicos precisos como o $R_{0}$.

---

## 🔬 Metodologia e Modelos Comparados

O código implementa todas as etapas metodológicas, desde a correção do sub-relato domiciliar baseado no mínimo esperado, até a suavização da matriz.

| Modelo | Técnica | Geometria | Distribuição | Finalidade |
| :--- | :--- | :--- | :--- | :--- |
| M1 & M2 | LOESS | Idade-Idade e Coorte | Gaussiana | Avaliação exploratória |
| M3 & M4 | GAM | Idade-Idade | Poisson & Quasi-Poisson | Identificar a sobredispersão severa $(\hat{\phi} \approx 16,06)$ |
| **M5** | **GAM** | **Idade-Idade** | **Binomial Negativa** | **Modelo Final Selecionado** |
| M6 | GAM | Coorte (resp $\times$ diff) | Binomial Negativa | Avaliação de sobreajuste (overfitting) |

### Resultado Principal

O modelo selecionado como o mais robusto e com melhor capacidade de generalização (menor erro de validação cruzada) foi o **GAM Binomial Negativa na geometria Idade-Idade ($k=20$)**.

* **Modelo Final:** GAM NegBinomial $k=20$ (M5)
* **Deviance Explicada:** 99,8%
* **CV-RMSE (Erro de Validação Cruzada):** 0,079

---

## 🔒 Dados e Reprodução (AVISO DE PRIVACIDADE)

Os dados originais (`bancocomvidafull.csv` e `2974.xls`) contêm informações sensíveis dos participantes do inquérito COMVIDA. Por questões de privacidade e ética, **estes arquivos não estão incluídos neste repositório público**.

### Estrutura do Repositório

| Pasta/Arquivo | Conteúdo |
| :--- | :--- |
| `main_analysis.R` | Script principal que orquestra a execução de todas as etapas, desde o pré-processamento até o diagnóstico. |
| `R/` | Contém todas as funções modulares (`01_data_cleaning.R`, `03_models.R`, etc.) que implementam a metodologia estatística. |
| `data/raw/` | Contém apenas o dado demográfico e o arquivo de dados sintéticos para testes. **Não contém o banco original.** |

### Como Rodar a Análise

Para reproduzir os resultados e gerar as matrizes e gráficos de diagnóstico:

1.  **Gere os Dados Sintéticos:** No RStudio, execute o script de criação de dados falsos:
    ```r
    source("R/00_create_synthetic_data.R")
    create_synthetic_data()
    ```
2.  **Execute a Análise Principal:** Rode o script que chama todo o pipeline:
    ```r
    source("main_analysis.R")
    ```

**Nota:** Os resultados numéricos obtidos com dados sintéticos serão diferentes daqueles apresentados na monografia.

---

## 👩‍💻 Sobre a Autora

**Anna Clara Damasio Monteiro**
* **Orientador:** Prof. Dr. Claudio José Struchiner
* **Instituição:** Fundação Getulio Vargas - Escola de Matemática Aplicada (FGV EMAp)
* **Ano:** 2025
