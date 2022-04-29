library(tidyverse)
library(purrr)

ds <- data.frame(
  a = c(1),
  b = c(123),
  c = c(12)
)


retornaTaxa <- function(
  data,
  prob,
  vars) {
  
  data |>
    group_by_at({{vars}}) |> 
    summarise(
     retorno = sum({{ prob }} * {{vars}})
    )
}

retornaTaxa(dados, prob, c(txRetornoA, txRetornoB))



retornos <- dados %>%
  mutate(esperadoA = prob * txRetornoA,
         esperadoB = prob * txRetornoB) %>%
  summarise(tresperadoA = sum(esperadoA),
            tresperadoB = sum(esperadoB),
            dsA = sqrt(sum( (txRetornoA - tresperadoA )^2 * prob ) ),
            dsB = sqrt(sum( (txRetornoB - tresperadoB )^2 * prob ) ),
            covAB = sum((txRetornoA - tresperadoA) * ( txRetornoB - tresperadoB) * prob),
            corrAB = covAB / (dsA * dsB))
```
