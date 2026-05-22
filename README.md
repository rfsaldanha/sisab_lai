# SISAB LAI CIAP/CID

Este projeto organiza arquivos CSV do SISAB recebidos via LAI (Lei de Acesso à Informação), resolve sobreposições entre pedidos, padroniza os dados de CID/CIAP e gera arquivos anuais em CSV e Parquet.

O fluxo foi pensado para ser reexecutado mensalmente: basta adicionar uma nova resposta LAI em `data/lai/` e rodar novamente o script de importação.

## Estrutura Esperada

Os CSVs devem estar descompactados em subpastas no formato:

```text
data/
  lai/
    pedido_*/
      csv/
        *.csv
```

Arquivos `.zip` são ignorados. O script procura apenas arquivos `.csv` sob `data/lai/`.

## Como Rodar

No diretório raiz do projeto:

```bash
Rscript data_import.R
```

O script faz uma reconstrução completa dos arquivos anuais a cada execução. Isso mantém o processo simples, auditável e permite que um pedido LAI mais novo substitua automaticamente arquivos antigos, incompletos ou corrompidos.

## Dependências R

O script usa os pacotes:

- `arrow`
- `cli`
- `DBI`
- `dplyr`
- `duckdb`
- `fs`
- `purrr`
- `readr`
- `stringr`
- `tibble`
- `R.utils`

## O Que o Script Faz

1. Localiza todos os CSVs em `data/lai/*/csv/`.
2. Infere a competência (`YYYYMM`) a partir do nome do arquivo.
3. Detecta o cabeçalho real após eventuais textos de preâmbulo do SQL*Plus.
4. Valida arquivos vazios, inválidos, sem cabeçalho reconhecido ou sem linhas de dados.
5. Resolve sobreposições mensais escolhendo, por competência, o arquivo válido com maior número de linhas.
6. Padroniza os esquemas antigos e novos para uma tabela tidy única.
7. Exporta arquivos anuais em CSV e Parquet.
8. Exporta relatórios de auditoria e diagnóstico.
9. Usa cache de inspeção para acelerar execuções futuras.

## Saídas

Os arquivos de dados são gravados em:

```text
data/export/data/sisab_saude_ciap_cid_YYYY.csv
data/export/data/sisab_saude_ciap_cid_YYYY.parquet
```

Os relatórios são gravados em:

```text
data/export/reports/sisab_lai_file_inventory.csv
data/export/reports/sisab_lai_selected_files.csv
data/export/reports/sisab_lai_invalid_files.csv
data/export/reports/sisab_lai_missing_months.csv
```

O cache de inspeção é gravado em:

```text
data/export/cache/sisab_lai_file_cache.rds
```

Uma tabela de referência CIAP-2 para junção com os dados é mantida em:

```text
reference/ciap2_codes.csv
```

## Formato dos Dados Exportados

Os arquivos anuais têm as seguintes variáveis:

| Variável | Descrição |
|---|---|
| `ano_competencia` | Ano da competência. |
| `competencia` | Competência no formato `YYYYMM`. |
| `competencia_date` | Primeiro dia do mês de competência. |
| `co_municipio_ibge` | Código IBGE do município. |
| `tp_codigo` | Tipo do código: `CID` ou `CIAP`. |
| `codigo` | Código CID-10 ou CIAP-2. |
| `qt_atendimentos` | Quantidade de atendimentos. |
| `source_request` | Pasta do pedido LAI selecionado. |
| `source_file` | Arquivo CSV selecionado. |

No Parquet, `qt_atendimentos` é exportado como inteiro e `competencia_date` como data.

Consulte [DATA_DICTIONARY.md](DATA_DICTIONARY.md) para o dicionário completo, incluindo a lista de rubricas CIAP-2.

Para anexar os nomes completos das rubricas CIAP-2:

```r
library(dplyr)
library(readr)

dados <- read_csv("data/export/data/sisab_saude_ciap_cid_2025.csv")
ciap <- read_csv("reference/ciap2_codes.csv")

dados_com_ciap <- dados |>
  left_join(ciap, by = c("tp_codigo", "codigo"))
```

## Relatórios

- `sisab_lai_file_inventory.csv`: inventário completo dos CSVs encontrados, incluindo validação, cache, esquema detectado e status de seleção.
- `sisab_lai_selected_files.csv`: arquivos mensais escolhidos após resolver sobreposições.
- `sisab_lai_invalid_files.csv`: arquivos rejeitados e motivo da rejeição.
- `sisab_lai_missing_months.csv`: competências sem arquivo válido dentro do intervalo observado.

## Regra de Sobreposição

Quando há mais de um arquivo válido para a mesma competência, o script seleciona:

1. O arquivo com maior número de linhas de dados.
2. Em caso de empate, o maior arquivo em bytes.
3. Em novo empate, a pasta `pedido_*` lexicograficamente mais recente.

Os demais arquivos válidos aparecem como `superseded` nos relatórios.

## Fluxo Mensal

1. Crie uma nova pasta para o pedido LAI em `data/lai/`.
2. Descompacte os CSVs dentro de `data/lai/pedido_*/csv/`.
3. Rode `Rscript data_import.R`.
4. Revise os relatórios em `data/export/reports/`.
5. Use os arquivos anuais em `data/export/data/`.

Como os CSVs LAI são tratados como estáticos, o cache evita recalcular cabeçalhos e contagens de linhas de arquivos já vistos. Se um arquivo for substituído, o caminho, tamanho ou data de modificação mudam e o cache é invalidado para aquele arquivo.

## Observações

- Os arquivos em `data/` não precisam ser versionados no Git.
- O script mantém colunas de proveniência (`source_request` e `source_file`) nos dados exportados.
- O campo `source_schema` fica nos relatórios, não nos dados anuais.
- Meses ausentes são calculados automaticamente entre a menor e a maior competência observada.
