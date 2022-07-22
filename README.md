rpcdas: An R PCDaS API wrapper

rpcdas is an R package with wrapper functions for the PCDaS API. 

## Instalation

Currently, this package is available only on Github.

```{r}
remotes::install_github("rfsaldanha/rpcdas")
library(rpcdas)
```

## Token

The PCDaS API requires an access token. After receiving your token, you can inform it at each package function or store the token at renviron for more convenient use. 

```{r}
Sys.setenv("pcdas_token" = "YOUR_PCDAS_TOKEN")
```

After running the line above, the package functions will access your PCDaS token internally. 


## Usage

This package provides some functions for more convenient use of the PCDaS API. 

### List available indexes

```{r}
list_pcdas_tables()
```

The function above will list the available indexes for your token.

### SIM

```{r}
# Total number of deaths by municipality of residence in 2010.
get_sim(agg = "mun_res", ano = 2010)

# Total number of deaths by state of occurrence in 2010.
get_sim(agg = "uf_ocor", ano = 2010)

# Total number of deaths by state of residence in 2010, filtering ages between 10 and 30 years old.
get_sim(agg = "uf_res", ano = 2010, idade_a = 10, idade_b = 30)

# Total number of deaths by state of residence in 2010 were the basic cause of death starting with the letter I.
get_sim(agg = "uf_res", ano = 2010, cid_like = "I")

# Total number of deaths by state of residence in 2010 were the basic cause of deaths are some of the vector.
get_sim(agg = "uf_res", ano = 2010, cid_in = c("I219", "B342", "R98"))
```

### Generic query

This function allows a generic query to the PCDaS indexes where you should write a SQL query.

```{r}
generic_pcdas_query(sql_query = "SELECT N_AIH, DT_INTER, PROC_REA FROM \"datasus-sih\" LIMIT 100")
```

The query may return 10,000 lines maximum.
