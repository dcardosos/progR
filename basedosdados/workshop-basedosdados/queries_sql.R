# contando numero de registros por ano
"
SELECT ano, count(*) numero_vinculos
FROM `basedosdados.br_me_rais.microdados_vinculos`
WHERE sigla_uf = 'SP'
GROUP BY ano
ORDER BY ano
"

## por ano e por sigla_uf
"
SELECT ano, sigla_uf, count(*) numero_vinculos
FROM `basedosdados.br_me_rais.microdados_vinculos`
GROUP BY ano, sigla_uf
ORDER BY ano
"

# viuva por ano
"
SELECT ano, count(*) numero_viuvas
FROM `basedosdados.br_me_pensionistas.microdados` 
WHERE tipo_beneficiario = 'VIUVA'
GROUP BY ano
ORDER BY ano
"

# excluindo valores null
"
SELECT *
FROM `basedosdados.br_inep_ideb.brasil`
WHERE taxa_aprovacao IS NOT NULL
LIMIT 1000
"

# inner join tabelas
"
WITH vinculos_acre AS (
  SELECT id_municipio, ano, count(*) numero_vinculos
  FROM `basedosdados.br_me_rais.microdados_vinculos`
  WHERE sigla_uf = 'AC'
  GROUP BY ano, id_municipio)
SELECT t1.id_municipio, t1.ano, t1.numero_vinculos, t2.populacao
FROM vinculos_acre t1
JOIN `basedosdados.br_ibge_populacao.municipio` t2
ON t1.id_municipio = t2.id_municipio
AND t1.ano = t2.ano
"

## groupby em cima disso: ano, pegando n vinculos e pop
"
WITH vinculos_acre AS(
  SELECT id_municipio, 
         ano, 
         count(*) numero_vinculos
  FROM `basedosdados.br_me_rais.microdados_vinculos`
  WHERE sigla_uf = 'AC'
  GROUP BY ano, id_municipio)
  
SELECT t1.ano, 
       SUM(t1.numero_vinculos) as numero_de_vinculos, 
       SUM(t2.populacao) as populacao
FROM vinculos_acre t1
JOIN `basedosdados.br_ibge_populacao.municipio` t2
ON t1.id_municipio = t2.id_municipio
AND t1.ano = t2.ano
GROUP BY t1.ano
"


