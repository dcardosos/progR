library(tidyverse)
library(magrittr)

# Get data ---------------------------------------------------------]
# https://rebrickable.com/downloads/

sets <- readr::read_csv('https://cdn.rebrickable.com/media/downloads/sets.csv.gz')

themes <- readr::read_csv('https://cdn.rebrickable.com/media/downloads/themes.csv.gz')

parts <- readr::read_csv('https://cdn.rebrickable.com/media/downloads/parts.csv.gz')

part_categories <- readr::read_csv('https://cdn.rebrickable.com/media/downloads/part_relationships.csv.gz')

inventories <- readr::read_csv('https://cdn.rebrickable.com/media/downloads/inventories.csv.gz')

inventory_parts <- readr::read_csv('https://cdn.rebrickable.com/media/downloads/inventory_parts.csv.gz')

colors <- readr::read_csv('https://cdn.rebrickable.com/media/downloads/colors.csv.gz')

# Inner join -------------------------------------------------------------------
# pares de linhas que satisfazem a condição de junção
## join by 'name'
sets %>% 
  inner_join(themes)

## join by id
sets %>% 
  inner_join(
    themes,
    by = c('theme_id' = 'id'),
    suffix = c('_sets', '_themes'))

## join by set_num
sets %>% 
  inner_join(inventories)

## join by part_num
inventory_parts %>% 
  inner_join(parts)

## sets, inventories, colors and inventory_parts
## count name color
sets %>% 
  inner_join(inventories) %>% 
  inner_join(inventory_parts, by = c('id' = 'inventory_id')) %>% 
  inner_join(colors, 
             by = c('color_id' = 'id'),
             suffix = c('_set', '_color')) %>% 
  count(name_color, sort = TRUE)
 
# Left join (keep first table) --------------------------------------------
# Right join (keep second table)
## keep all observations
sets %>% 
  inner_join(inventories) %>% 
  inner_join(inventory_parts, by = c('id' = 'inventory_id')) %>% 
  inner_join(colors, 
             by = c('color_id' = 'id'),
             suffix = c('_set', '_color')) %>% 
  left_join(themes, by = c('theme_id' = 'id'))


# Join tables to themselves
themes %>% 
  dplyr::inner_join(themes, 
                    by = c('id' = 'parent_id'), 
                    suffix = c('_parent', '_child')) %>% 
  dplyr::filter(name_parent == 'Space')

# Full join (keep all) --------------------------------------------
sets %>% 
  dplyr::full_join(inventories) %>% 
  tidyr::replace_na(list(num_parts = 'opa')) %>% 
  dplyr::filter(num_parts == 'opa')

sets %>% 
  dplyr::inner_join(inventories)