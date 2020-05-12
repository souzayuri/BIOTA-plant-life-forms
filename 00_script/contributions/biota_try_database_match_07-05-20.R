###
#title: Match entre tabelas (BIOTA- TRY)
#author: "Yuri Souza"
#data: "07/05/2020"
#content: Preenche a tabela do BIOTA com os traits do TRY para as espécies e generos compartilhados.

###

# limpar diretorio e carregar os pacotes ----------------------------------

rm(list = ls())


if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if(!require("textclean")) install.packages("textclean", dependencies = TRUE)
if(!require("data.table")) install.packages("datatable", dependencies = TRUE)
if(!require("beepr")) install.packages("beepr", dependencies = TRUE)

# carregar as tabelas -----------------------------------------------------


try <- fread('try_species_05-07-20.txt', header = TRUE, sep = "\t", dec = ".", quote = "", data.table = T)
try

beepr::beep(4)

biota.total.species <- read_csv2("Species_t0et96.csv")
biota.total.species

# ou
#biota.total.species <- read_csv("Species_t0et96.csv")
#biota.total.species

# limpar tabela do biota --------------------------------------------------


# para remover os cf. dos nomes das plantas que tenham "Genero cf. espécie". Também pode deletar Plantas que tenham
# "sp.", "cf.", "aff. no final do nome (quando não sabemos qual é a espécie, apenas o genero ou familia) e "indeterminate"


biota.confirmed.species <-biota.total.species %>% 
  dplyr::mutate(Specie = Specie %>% 
                  stringr::str_replace(" cf. ", " ")) %>% # remove o CF do meio do nome e mantem como se fosse uma espécies confirmada. Se quiser remover essas espécies é só comentar essa linha
  textclean::drop_row("Specie", c("Indeterminate", "sp.", "cf.", "aff.")) %>%  # deleta todas as linhas que tenham esses atributos
  rename(SpeciesName = Specie) # renomeia para o mesmo nome da coluna de espécies do TRY
biota.confirmed.species



# preencher a tabela do biota com os Matchs para as mesmas espécies do TRY------------------------


biota.confirmed.species.try.ft <-left_join(biota.confirmed.species, try, by = "SpeciesName") # preenche a tabela do biota com os dados do try para as mesmas espécies


write_csv2(biota.confirmed.species.try.ft, "biota.confirmed.species.try.ft_07-05-20.csv")


# para generos -------------------------------------------------------------------------


# separando coluna de especies do Try -----------------------------------

try_genus_sp <- try %>% 
  mutate(SpeciesNames = SpeciesName) %>% 
  separate(SpeciesName, c("Genus", "Species")) %>% 
  select(1:4, 30, 5:29) %>% 
  dplyr::select(-c(5,7)) %>% 
  rename(SpeciesName = Genus)
try_genus_sp


# deixando apenas os generos na tabela do BIOTA ---------------------------

biota.total.species <- biota.total.species[-c(34),] 
# Tem que remover essa linha porque o nome esta no formato errado (sp34). 
# Isso é um problema que vem lá da tabela da Valesca e não sei como resolver, ja aconteceu comigo. 
# Não interfere em nada remover ela aqui, porque elas tem Id até espécie.



biota.genus <-biota.total.species %>% 
  dplyr::mutate(Species = Specie %>%
                  stringr::str_replace(" sp. ", " ")) %>%  # substitui tudo que tenha sp. no nome por nada. Vai ficar só os numeros, que são substituidos nas linhas seguintes.
  dplyr::mutate(Species = Specie %>%
                  stringr::str_replace(" sp.", "")) %>%  # substitui tudo que tenha sp. no nome por nada. Vai ficar só os numeros, que são substituidos nas linhas seguintes.
  dplyr::mutate(Species = Species %>%
                  stringr::str_replace(" 1", "")) %>% 
  dplyr::mutate(Species = Species %>%
                  stringr::str_replace(" 2", "")) %>%
  dplyr::mutate(Species = Species %>%
                  stringr::str_replace(" 3", "")) %>%
  dplyr::mutate(Species = Species %>%
                  stringr::str_replace(" 4", "")) %>%
  dplyr::mutate(Species = Species %>%
                  stringr::str_replace(" cf. ", "_")) %>% # substitui cf. por _
  dplyr::mutate(Species = Species %>%
                  stringr::str_replace(" cf.", "")) %>% 
  textclean::drop_row("Species", c(" ", "_", "ceae", "Smilaxcata")) %>%  # se vc comentar desse pipe ( %>% ) para baixo, vai conseguir ver quais foram as linhas que eu removi os sufixos de cf. e sp.
  select(3,2) %>% 
  rename(SpeciesName = Species)

biota.genus




# preencher a tabela do biota com os Matchs para as mesmas espécies do TRY------------------------


biota.confirmed.genus.try.ft <-left_join(biota.genus, try_genus_sp, by = "SpeciesName") # preenche a tabela do biota com os dados do try para as mesmas espécies
biota.confirmed.genus.try.ft


# juntar a tabela de genero e especies ------------------------------------

biota.confirmed.species.genus.try.ft <- bind_rows(biota.confirmed.species.try.ft, biota.confirmed.genus.try.ft)


write_csv2(biota.confirmed.species.genus.try.ft, "biota.confirmed.species.genus.try.ft_07-05-20.csv")
