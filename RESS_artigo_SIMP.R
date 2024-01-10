################################## CARREGA BIBLIOTECAS E BANCOS ##################################

####____#### BIBLIOTECAS ####
library(lubridate)
library(tidyverse)
library(openxlsx)
library(epiDisplay)

####____#### BANCOS ####
## Redcap/SIM-P 
load("~/SIMP/ARTIGO/banco/simpbr_paper2020.RData")

## SIVEP/SRAG
dados_srag2021 <- read.csv(file = "~/SIMP/ARTIGO/dados/sivep/INFLUD21-04-02-2021.csv", header = TRUE, sep = ";")
dados_srag2020 <- read.csv(file = "~/SIMP/ARTIGO/dados/sivep/INFLUD-04-02-2021.csv", header = TRUE, sep = ";")

## eSUS/SG
dados_esus = read.csv2(file="~/SIMP/ARTIGO/dados/esus/OneDrive_1_04-02-2021/clean_esusve-open_simp_04fev21.csv", header = TRUE, sep = ",")

################################## _________ CURADORIA _________ ##################################  

####____#### SIVEP/SRAG  ####
## Estrutura geral do banco
glimpse(dados_srag)

# 01 - Seleciona variáveis que serão utilizadas e junta as notificações de 2020 e 2121 ####
dados_srag2021 <- dados_srag2021 %>% dplyr::select(DT_SIN_PRI, DT_NOTIFIC, NU_IDADE_N, CLASSI_FIN, SEM_PRI)
dados_srag2020 <- dados_srag2020 %>% dplyr::select(DT_SIN_PRI, DT_NOTIFIC, NU_IDADE_N, CLASSI_FIN,SEM_PRI)
dados_srag <- bind_rows(dados_srag2020, dados_srag2021)
rm(dados_srag2020, dados_srag2021)

# 02 - Configura data de início dos sintomas e de notificação#### 
dados_srag$DT_SIN_PRI=as.Date(dados_srag$DT_SIN_PRI, "%d/%m/%y")
dados_srag$DT_NOTIFIC=as.Date(dados_srag$DT_NOTIFIC, "%d/%m/%y")

# 03 - Filtra data de início dos sintomas para 2020, notificação até 15/01/2021, idade <20  ####
dados_srag <- dados_srag %>% 
  filter(DT_SIN_PRI>as.POSIXct("2020-02-25") & DT_SIN_PRI<as.POSIXct("2020-12-31")) %>% 
  filter(DT_NOTIFIC<as.POSIXct("2021-01-15")) %>% 
  filter(NU_IDADE_N>=0 & NU_IDADE_N <20) %>% 
  filter(CLASSI_FIN == 5)
  
table(dados_srag$SEM_PRI, useNA = "always")
table(dados_srag$CLASSI_FIN, useNA = "always")



####____#### eSUS/Síndrome gripal ####
## Estrutura geral do banco
glimpse(dados_esus)

# 00 _ Seleciona variáveis que serão utilizadas ####
dados_esus <- dados_esus %>% dplyr::select(dis, idade, dataNotificacao, dataInicioSintomas, classificacaoFinal)

# 01 - Filtra data de início dos sintomas para 2020. ####
  # Os demais filtros já foram solicitados na disponibilização do banco para a área técnica responsável (casos confirmados, data de notificação e idade<20)
dados_esus <- dados_esus %>% 
  mutate(dis=ymd(dataInicioSintomas))%>% 
  filter (dis>as.POSIXct("2020-02-25") & dis<as.POSIXct("2020-12-31"))  
 
table(dados_esus$dis, useNA = "always")
table(dados_esus$classificacaoFinal, useNA = "always")
table(dados_esus$idade, useNA = "always")
table(dados_esus$dataNotificacao, useNA = "always")



####____#### Redcap/SIM-P ####
## Estrutura geral do banco
glimpse(simpbr)

# 01 - Renomea variáveis ####
simpbr <- simpbr %>% 
  rename(febre = defcaso___1) %>% rename(conjuntivite_defcaso = defcaso___2) %>% rename(hipotensao_choque_defcaso = defcaso___3) %>% 
  rename(difuncao_cardio = defcaso___4) %>% rename(coagulopatia = defcaso___5) %>% rename(gastro = defcaso___6) %>% rename(inflamacao = defcaso___7) %>% 
  rename(outrasinfecciosas = defcaso___8) %>% rename(evid_covid = defcaso___9) %>% rename(alteracoes_pele = sintom___1) %>% 
  rename(cefaleia = sintom___2) %>% rename(confusao_mental = sintom___3) %>% rename(conjuntivite = sintom___4) %>% rename(coriza = sintom___5) %>% 
  rename(diarreia = sintom___6) %>% rename(dispneia = sintom___7) %>% rename(dor_degluticao = sintom___8) %>% rename(dor_garganta = sintom___9) %>% 
  rename(dor_peito = sintom___10) %>% rename(dor_abdm = sintom___11) %>% rename(edema_cerv = sintom___12) %>% rename(edema_maospes = sintom___13) %>% 
  rename(irritabilidade = sintom___14) %>% rename(letargia = sintom___15) %>% rename(linfadenopatia = sintom___16) %>% rename(nausea_vomito = sintom___17) %>% 
  rename(mialgia = sintom___18) %>% rename(manchas_vermelhas = sintom___19) %>% rename(oliguria = sintom___20) %>% rename(saturacao_o2 = sintom___21) %>% 
  rename(taquicardia = sintom___22) %>% rename(tosse = sintom___23) %>% rename(nenhumacomplicacao = quais_complicacoes___0) %>% 
  rename(convulsoes = quais_complicacoes___1) %>% rename(edema_agudo_pulmonar = quais_complicacoes___2) %>% 
  rename(evento_trombo = quais_complicacoes___3) %>% rename(falencia_outros_orgaos = quais_complicacoes___4) %>% 
  rename(hipertensao = quais_complicacoes___5) %>%  rename(hipotens_drogas = quais_complicacoes___6) %>% 
  rename(infarto = quais_complicacoes___7) %>%  rename(insuficienncai_renal = quais_complicacoes___8) %>% 
  rename(ventilacao_invasiva = quais_complicacoes___9) %>% rename(ventilacao_nao_invasiva = quais_complicacoes___10) %>% 
  rename(troca_plasmatica = quais_complicacoes___11) %>% rename(pneumonia = quais_complicacoes___12) %>% 
  rename(sepse = quais_complicacoes___13) %>% rename(outras_complicacacoes = quais_complicacoes___14) %>% 
  rename(nenhumacomorbidade = qual_comorbidade___0) %>% 
  rename(cardiopatia = qual_comorbidade___1) %>% rename(diabete = qual_comorbidade___2) %>% 
  rename(dislipidemia = qual_comorbidade___3) %>% rename(desnutricao = qual_comorbidade___4) %>% 
  rename(doencahematologica = qual_comorbidade___5) %>%  rename(doencaneurologica = qual_comorbidade___6) %>% 
  rename(doencareumato = qual_comorbidade___7) %>%  rename(doencaonco = qual_comorbidade___8) %>% 
  rename(imunossuprimido = qual_comorbidade___9) %>% rename(has = qual_comorbidade___10) %>% 
  rename(hippulmonar = qual_comorbidade___11) %>% rename(nefropatia = qual_comorbidade___12) %>% 
  rename(pneumoniapre = qual_comorbidade___13) %>% rename(sindgenetica = qual_comorbidade___14) %>% 
  rename(outrascomorbidades = qual_comorbidade___15) 


# 02 - Filtra data de início dos sintomas para 2020 e notificação até 15/01/2021 ####
simpbr <- simpbr %>% 
  filter (dt_inicio_ss <= as.Date("2020-12-31") & dt_not < as.Date("2021-01-15")) # exclui missing na data de início dos sintomas  

# 03 - Realiza limpeza de duplicidades determinística ####
## Exclui registros que têm todos os campos com preenchimento igual
simpbr <- distinct(simpbr) 
## Identifica os registros duplicados 
simpbr <- simpbr %>%        
  group_by(nome_pac, nome_mae, dt_nasc, uf_res) %>% #por agrupamento, identifica os registros que têm nome, nome da mãe, data de nascimento e uf de residencia exatamente iguais
  mutate(duplicidade = n()) %>%                     #cria uma variável com o número de registros iguais a esta linha
  ungroup()                                         #desagrupa

## Cria tabela com as duplicidades para revisão manual
tbDuplicatas <- simpbr %>% filter(duplicidade > 1) %>% dplyr::select(-c(nome_mae, dt_nasc, uf_res, duplicidade, prof_not, tel_not, mail_not, nome_resp, cpf, cns))

#manipula os registros que são duplicidade, manualmente
simpbr$dt_alt_uti[simpbr$record_id==560]=simpbr$dt_alt_uti[simpbr$record_id==618]  
simpbr$dor_abdm[simpbr$record_id==560]=simpbr$dor_abdm[simpbr$record_id==580]
simpbr$hipotens_drogas[simpbr$record_id==560]=simpbr$hipotens_drogas[simpbr$record_id==580]
simpbr$ventilacao_nao_invasiva[simpbr$record_id==560]=simpbr$ventilacao_nao_invasiva[simpbr$record_id==580]
simpbr$leucocitos[simpbr$record_id==560]=simpbr$leucocitos[simpbr$record_id==580]
simpbr$neutrof[simpbr$record_id==560]=simpbr$neutrof[simpbr$record_id==580]
simpbr$ttpta[simpbr$record_id==560]=simpbr$ttpta[simpbr$record_id==618]
simpbr$creat[simpbr$record_id==560]=simpbr$creat[simpbr$record_id==618]
simpbr$sodio[simpbr$record_id==560]=simpbr$sodio[simpbr$record_id==618]
simpbr$dt_evolucao[simpbr$record_id==560]=simpbr$dt_evolucao[simpbr$record_id==613]

simpbr$evolucao[simpbr$record_id==1075]=simpbr$evolucao[simpbr$record_id==1119]
simpbr$ventilacao_nao_invasiva[simpbr$record_id==1075]=simpbr$ventilacao_nao_invasiva[simpbr$record_id==1077]
simpbr$nenhumacomorbidade[simpbr$record_id==1075]=simpbr$nenhumacomorbidade[simpbr$record_id==1083]
simpbr$linfoct[simpbr$record_id==1075]=simpbr$linfoct[simpbr$record_id==1083]
simpbr$d_dimero[simpbr$record_id==1075]=simpbr$d_dimero[simpbr$record_id==1083]
simpbr$potass[simpbr$record_id==1075]=simpbr$potass[simpbr$record_id==1083]
simpbr$plaqt[simpbr$record_id==1075]=simpbr$plaqt[simpbr$record_id==1083]
simpbr$ttpta[simpbr$record_id==1075]=simpbr$ttpta[simpbr$record_id==1084]
simpbr$fibring[simpbr$record_id==1075]=simpbr$fibring[simpbr$record_id==1084]
simpbr$dhl[simpbr$record_id==1075]=simpbr$dhl[simpbr$record_id==1084]
simpbr$tropon[simpbr$record_id==1075]=simpbr$tropon[simpbr$record_id==1084]
simpbr$procalct[simpbr$record_id==1075]=simpbr$procalct[simpbr$record_id==1078]
simpbr$ckmb[simpbr$record_id==1075]=simpbr$ckmb[simpbr$record_id==1091]
simpbr$ecocardio___5[simpbr$record_id==1075]=simpbr$ecocardio___5[simpbr$record_id==1077]

simpbr$hemoglobina[simpbr$record_id==332]=simpbr$hemoglobina[simpbr$record_id==338]
simpbr$leucocitos[simpbr$record_id==332]=simpbr$leucocitos[simpbr$record_id==338]
simpbr$hematocrito[simpbr$record_id==332]=simpbr$hematocrito[simpbr$record_id==338]
simpbr$tp[simpbr$record_id==332]=simpbr$tp[simpbr$record_id==336]
simpbr$albumina[simpbr$record_id==332]=simpbr$albumina[simpbr$record_id==336]
simpbr$hemoglobina[simpbr$record_id==332]=simpbr$hemoglobina[simpbr$record_id==338]
simpbr$exame_imagem[simpbr$record_id==332]=simpbr$exame_imagem[simpbr$record_id==336]
simpbr$imagem___rt[simpbr$record_id==332]=simpbr$imagem___rt[simpbr$record_id==334]
simpbr$imagem___tt[simpbr$record_id==332]=simpbr$imagem___tt[simpbr$record_id==336]
simpbr$imagem_resultado___1[simpbr$record_id==332]=simpbr$imagem_resultado___1[simpbr$record_id==334]
simpbr$imagem_resultado___2[simpbr$record_id==332]=simpbr$imagem_resultado___2[simpbr$record_id==336]
simpbr$imagem_resultado___3[simpbr$record_id==332]=simpbr$imagem_resultado___3[simpbr$record_id==336]
simpbr$cortic[simpbr$record_id==332]=simpbr$cortic[simpbr$record_id==358]
simpbr$imunog[simpbr$record_id==332]=simpbr$imunog[simpbr$record_id==358]
simpbr$anticoag[simpbr$record_id==332]=simpbr$anticoag[simpbr$record_id==358]
simpbr$evolucao[simpbr$record_id==332]=simpbr$evolucao[simpbr$record_id==358]

#exclui registros de duplicidade
simpbr<-simpbr %>% 
  filter(record_id != 562 & record_id != 580 & record_id != 678 & record_id != 600 & record_id != 611  & record_id != 614 & record_id != 618
         & record_id != 626 & record_id != 633 & record_id != 650 & record_id != 673 & record_id != 612 & record_id != 613 & record_id != 1245
         & record_id != 1247 & record_id != 1246 & record_id != 1248 & record_id != 1250 & record_id != 1251 & record_id != 1252 & record_id != 1253
         & record_id != 1254 & record_id != 1255 & record_id != 281 & record_id != 721 & record_id != 1077 & record_id != 1078 & record_id != 1080
         & record_id != 1081 & record_id != 1083 & record_id != 1082 & record_id != 1084 & record_id != 1091 & record_id != 1102 & record_id != 1117 
         & record_id != 1118 & record_id != 1119 & record_id != 334 & record_id != 340 & record_id != 336 & record_id != 341 & record_id != 337
         & record_id != 358 & record_id != 338)
# 04 - Seta todos os missing em "NA" ####
simpbr <- sjmisc::set_na(simpbr, na = c("NA", "", "Ignorado", "Não declarado", "Não realizou", "Não sabe / Não respondeu", "Se Desconhece"))

# 05 - Cria nova variável de evolução para grupos de comparação #### 
simpbr <- simpbr %>% 
  mutate(evolucao2 = case_when(evolucao == "Óbito" ~ "1 - Óbito", 
                               TRUE ~ "2 - Vivo ou sem evolução"))
simpbr <- simpbr %>% 
  mutate(evolucao3 = case_when(evolucao == "Óbito" ~ "1 - Óbito", 
                               evolucao == "Alta hospitalar" ~ "2 - Alta"))
                               

#Freq
table(simpbr$evolucao, useNA = "always")
table(simpbr$evolucao2, useNA = "always")
table(simpbr$evolucao3, useNA = "always")

# 06 - Calcula idade e cria faixa etária ####

table(simpbr$dt_nasc, useNA = "always") #Freq: não há registro com dtnasc em branco 
simpbr$dt_nasc=as.Date(simpbr$dt_nasc)  #transforma variável de nascimento em variável do tipo data
simpbr$dt_not=as.Date(simpbr$dt_not)    #transforma variável de notificação em variável do tipo data
simpbr$idade=trunc(time_length(interval(simpbr$dt_nasc,simpbr$dt_not),"years")) #calcula idade que a pessoa tinha ao ser notificada
table(simpbr$idade, useNA = "always")   #Freq idade
simpbr <- simpbr %>% filter (idade < 20) # exclui registrs com idade > 19  
simpbr <- simpbr %>% mutate (faixet = case_when(idade < 1 ~ "Menor de 1", #calcula faixas etárias
                                                idade > 0 & idade <5 ~ "01 a 04",
                                                idade > 4 & idade < 10 ~ "05 a 09",
                                                idade > 9 & idade < 15 ~ "10 a 14",
                                                idade > 14 ~ "15 a 19"))
simpbr$faixet <- factor(simpbr$faixet, levels = c("Menor de 1", "01 a 04", "05 a 09", "10 a 14", "15 a 19"), #transfrma em fator, tendo os bebês como referência
                        labels = c("Menor de 1", "01 a 04", "05 a 09", "10 a 14","15 a 19"))
table(simpbr$faixet, useNA = "always") #Freq faixet

# 07 - Recategoriza raça/cor para branco/não branco ####
simpbr <- simpbr %>% 
  mutate(racacorbnb = case_when(racacor == "Branca" ~ "2 - Branca",
                                racacor == "Indígena" | racacor == "Parda" | 
                                  racacor == "Preta" ~ "1 - Não Branca"))

table(simpbr$racacorbnb, useNA = "always") #Freq

# 08 - Padroniza campos abertos utilizados ####
simpbr <- simpbr %>% 
  #retira todos os acentos
  mutate_at(c("nome_pac", "nome_mae", "outros_ss", "outra_compicacao", "comorb_otrs", "tto_quais", "imagem_resultado_outro", "ecocardio_outro", "ecocardio_outro_2", "eletro_result"), ~abjutils::rm_accent(.)) %>% 
  #retira todos os caracteres que não são letras e substitui por espaço. Não incluí aqui outros sinais e sintomas, complicações, tratamento e comorbidades, pois tudo bem aparecerem números, vírgulas e outros caracteres não alfabéticos nessas variáveis. 
  mutate_at(c("nome_pac", "nome_mae"), ~str_replace_all(., "[^[:alpha:]]", " ")) %>%
  #transforma tudo em caixa baixa (letra minúscula)
  mutate_at(c("nome_pac", "nome_mae", "outros_ss", "outra_compicacao", "comorb_otrs", "tto_quais", "imagem_resultado_outro", "ecocardio_outro", "ecocardio_outro_2", "eletro_result"), ~str_to_lower(.)) %>%               
  #retira espaçoo excessivo no início e no final da calcula
  mutate_at(c("nome_pac", "nome_mae", "outros_ss", "outra_compicacao", "comorb_otrs", "tto_quais", "imagem_resultado_outro", "ecocardio_outro", "ecocardio_outro_2", "eletro_result"), ~str_trim(., side = "both")) %>%
  #retira espaço excessivo entre as palavra
  mutate_at(c("nome_pac", "nome_mae", "outros_ss", "outra_compicacao", "comorb_otrs", "tto_quais", "imagem_resultado_outro", "ecocardio_outro", "ecocardio_outro_2", "eletro_result"), ~ str_squish(.))

# 09 - Reclassifica e cria variáveis de comrbidadess  ####
## Reclassifica e cria dicotômicas a partir do preenchimento da variável aberta "outros"
simpbr = simpbr %>% 
  mutate(doencaneurologica = case_when(str_detect(comorb_otrs, fixed("autismo")) | str_detect(comorb_otrs, fixed("epilepsia"))|
                                       str_detect(comorb_otrs, fixed("autista")) | str_detect(comorb_otrs, fixed("neuropatia"))|
                                       str_detect(comorb_otrs, fixed("convulsao")) | str_detect(comorb_otrs, fixed("encefalopatia"))|
                                       str_detect(comorb_otrs, fixed("paralisia cerebral"))| str_detect(comorb_otrs, fixed("sindrome de west"))|
                                       str_detect(comorb_otrs, fixed("transtorno do processamento"))
                                     ~ "Checked", TRUE ~ doencaneurologica)) %>% 
  mutate(cardiopatia = case_when(str_detect(comorb_otrs, fixed("pericardite")) | str_detect(comorb_otrs, fixed("cardiopatia"))
                                 | str_detect(comorb_otrs, fixed("deficiencia de g6pd")) | str_detect(comorb_otrs, fixed("hipertrofia miocardica"))
                                 ~ "Checked", TRUE ~ cardiopatia)) %>%
  mutate(asma = case_when(str_detect(comorb_otrs, fixed("asma")) ~ "Checked", TRUE ~ "Não Checked")) %>%
  mutate(respre = case_when(str_detect(comorb_otrs, fixed("rinite")) | str_detect(comorb_otrs, fixed("sinusite"))|
                              str_detect(comorb_otrs, fixed("bronquite")) | str_detect(comorb_otrs, fixed("bronquiolite")) |
                              str_detect(comorb_otrs, fixed("broncodisplasia")) | str_detect(comorb_otrs, fixed("rinossinusite")) |
                              asma == "Checked" | pneumoniapre == "Checked"
                            ~ "Checked", TRUE ~ "Não Checked")) %>%
  mutate(doencaonco = case_when(str_detect(comorb_otrs, fixed("leucemia"))~ "Checked", TRUE ~ doencaonco)) %>% 
  mutate(desnutricao = case_when(str_detect(comorb_otrs, fixed("desnutricao"))~ "Checked", TRUE ~ desnutricao)) %>% 
  mutate(sindgenetica = case_when(str_detect(comorb_otrs, fixed("sindrome de down")) | str_detect(comorb_otrs, fixed("agamaglobulinemia")) 
                                  | str_detect(comorb_otrs, fixed("leocodistrofia"))
                                  ~ "Checked", TRUE ~ sindgenetica)) %>% 
  mutate(endocrinopre = case_when(str_detect(comorb_otrs, fixed("obesidade"))|str_detect(comorb_otrs, fixed("diabetes"))|
                                    str_detect(comorb_otrs, fixed("hipotireoidismo"))| str_detect(comorb_otrs, fixed("glicogenose"))|
                                    diabete =="Checked"
                                  ~ "Checked", TRUE ~ "Não Checked")) %>% 
  mutate(geniturinariopre = case_when(str_detect(comorb_otrs, fixed("drc iv"))|str_detect(comorb_otrs, fixed("pielonefrite"))|
                                    str_detect(comorb_otrs, fixed("bexiga neurogenica"))| str_detect(comorb_otrs, fixed("cisto variano"))
                                  ~ "Checked", TRUE ~ "Não Checked")) %>% 
  mutate(gastrointestinalpre = case_when(str_detect(comorb_otrs, fixed("alergia a proteina"))|str_detect(comorb_otrs, fixed("doença de crohn"))|
                                        str_detect(comorb_otrs, fixed("doenca celiaca"))| str_detect(comorb_otrs, fixed("atresia de esofago"))
                                        | str_detect(comorb_otrs, fixed("estenose severa")) | str_detect(comorb_otrs, fixed("laringite"))
                                        | str_detect(comorb_otrs, fixed("megacolon")) | str_detect(comorb_otrs, fixed("incontinencia f"))
                                      ~ "Checked", TRUE ~ "Não Checked")) %>% 
  mutate(osteomuscularpre = case_when(str_detect(comorb_otrs, fixed("lesao litica"))|str_detect(comorb_otrs, fixed("paresia"))|
                                           str_detect(comorb_otrs, fixed("neuropatia"))
                                         ~ "Checked", TRUE ~ "Não Checked")) %>% 
  mutate(imunologicopre = case_when(str_detect(comorb_otrs, fixed("imunodeficiencia"))|str_detect(comorb_otrs, fixed("pfapa"))|
                                           str_detect(comorb_otrs, fixed("adenite cerv"))| str_detect(comorb_otrs, fixed("hiv"))
                                         | str_detect(comorb_otrs, fixed("hipogamaglobulinemia"))
                                         ~ "Checked", TRUE ~ "Não Checked")) 
  

## Reclassifica "Outras comorbidades"
simpbr <- simpbr %>% 
  mutate(outrascomorbidades = case_when(doencaonco== "Checked" | desnutricao == "Checked" | diabete == "Checked" | has  == "Checked" |
                                          nefropatia == "Checked"| doencareumato == "Checked" | dislipidemia == "Checked"| hippulmonar == "Checked"
                                        | doencahematologica == "Checked" | endocrinopre == "Checked" | sindgenetica == "Checked" 
                                        | imunossuprimido =="Checked" | geniturinariopre =="Checked" | gastrointestinalpre == "Checked"
                                        | osteomuscularpre =="Checked" | imunologicopre =="Checked"
                                        | doencareumato =="Checked"|  dislipidemia =="Checked"
                                        | has =="Checked"| hippulmonar =="Checked" | nefropatia =="Checked"
                                        ~ "Checked", TRUE ~ outrascomorbidades))
table(simpbr$outrascomorbidades)

## Cria comorbidade dictômica
simpbr <- simpbr %>% 
  mutate (comorb = case_when(cardiopatia =="Checked" | doencaneurologica =="Checked" | respre =="Checked"|
                               outrascomorbidades =="Checked" ~"Sim",
                             nenhumacomorbidade =="Checked"~"Não",
                             TRUE ~ NA_character_))
table(simpbr$comorb, useNA = "always") #Freq

# 10 - Reclassifica e cria variáveis de sinais e sintomas ####
## Reclassifica e cria variáveis dicotômicas a partir do preenchimento da variável aberta "outros"
simpbr = simpbr %>% 
  mutate(coriza = case_when( str_detect(outros_ss, fixed("congestao nasal")) | str_detect(outros_ss, fixed("obstrucao nasal")) ~ "Checked", TRUE ~ coriza)) %>% 
  mutate(dispneia = case_when(str_detect(outros_ss, fixed("dispneia")) |
                                str_detect(outros_ss, fixed("desconforto respirat")) ~ "Checked", TRUE ~ dispneia)) %>%   
  mutate(dor_degluticao = case_when(str_detect(outros_ss, fixed("odinofagia")) ~ "Checked", TRUE ~ dor_degluticao)) %>% 
  mutate(dor_peito = case_when(str_detect(outros_ss, fixed("dor ventilatorio")) ~ "Checked", TRUE ~ dor_peito)) %>% 
  mutate(saturacao_o2 = case_when(str_detect(outros_ss, fixed("Saturacao O2 < 95")) |  str_detect(outros_ss, fixed("24/08 - apresentou saturacao 91-96% em ar ambiente")) |
                                    str_detect(outros_ss, fixed("sat o2<95%")) ~"Checked", TRUE ~ saturacao_o2)) %>% 
  mutate(outros_respiratorios = case_when(str_detect(outros_ss, fixed("taquipnei")) | str_detect(outros_ss, fixed("tiragem intercostal")) |
                                            str_detect(outros_ss, fixed("atelectasia d")) | str_detect(outros_ss, fixed("anosmia")) |
                                            str_detect(outros_ss, fixed("insuficiencia respiratoria"))|
                                            str_detect(outros_ss, fixed("ausculta pulmonar com presenca de estertores"))|
                                            str_detect(outros_ss, fixed("disturbio olfativo"))|
                                            str_detect(outros_ss, fixed("aumento secrecao pulmonar"))~ "Checked", TRUE ~ "Não Checked")) %>% 
  
  mutate(diarreia = case_when(str_detect(simpbr$outros_ss, fixed("diarreia")) ~ "Checked", TRUE ~ diarreia)) %>% 
  mutate(nausea_vomito = case_when(str_detect(simpbr$outros_ss, fixed("vomito")) |  str_detect(simpbr$outros_ss, fixed("emese")) |
                                     str_detect(simpbr$outros_ss, fixed("nausea")) ~ "Checked", TRUE ~ nausea_vomito)) %>% 
  mutate(dor_abdm = case_when(str_detect(simpbr$outros_ss, fixed("dor abd")) |  str_detect(simpbr$outros_ss, fixed("epigastralgia")) |
                                str_detect(simpbr$outros_ss, fixed("dor abdominal")) ~ "Checked", TRUE ~ dor_abdm)) %>%  
  mutate(dist_abdm = case_when(str_detect(outros_ss, fixed("distensao abd"))|  str_detect(simpbr$outros_ss, fixed("abdome distendido"))
                               |str_detect(simpbr$outros_ss, fixed("abdomen distendido"))
                               |str_detect(simpbr$outros_ss, fixed("distensao abdominal")) ~ "Checked", TRUE ~ "Não Checked")) %>% 
  mutate(inapetencia = case_when(str_detect(simpbr$outros_ss, fixed("inapetencia")) | str_detect(simpbr$outros_ss, fixed("reducao do apetite")) |
                                   str_detect(simpbr$outros_ss, fixed("reducai do apetite")) | str_detect(simpbr$outros_ss, fixed("perda de apetite")) | 
                                   str_detect(simpbr$outros_ss, fixed("hiporexia")) ~ "Checked", TRUE ~ "Não Checked")) %>% 
  mutate(outros_gastro = case_when(str_detect(simpbr$outros_ss, fixed("ascite")) | str_detect(simpbr$outros_ss, fixed("hepatoesplenomegalia")) |
                                     str_detect(simpbr$outros_ss, fixed("hepatomegalia",  ignore_case = TRUE)) |
                                     str_detect(simpbr$outros_ss, fixed("hiporrexia")) |
                                     str_detect(outros_ss, fixed("sialorreia")) | str_detect(outros_ss, fixed("hematemese"))
                                   ~ "Checked", TRUE ~ "Não Checked")) %>% 
  
  mutate(irritabilidade = case_when(str_detect(outros_ss, fixed("irritabilidade")) ~ "Checked", TRUE ~ irritabilidade)) %>% 
  mutate(cervicalgia_rigidez = case_when(str_detect(outros_ss, fixed("dor cervical")) | str_detect(outros_ss, fixed("cervicalgia"))|
                                           str_detect(outros_ss, fixed("rigidez de nuca")) |
                                           str_detect(outros_ss, fixed("rigidez da nuca")) ~ "Checked", TRUE ~ "Não Checked")) %>% 
  mutate(letargia = case_when(str_detect(outros_ss, fixed("hipoativ")) ~ "Checked", TRUE ~ letargia)) %>% 
  mutate(outros_neurologicos = case_when(str_detect(outros_ss, fixed("sinais meningeos")) | str_detect(outros_ss, fixed("astenia")) |
                                           str_detect(outros_ss, fixed("perda de forca")) | str_detect(outros_ss, fixed("perda da forca")) |
                                           str_detect(outros_ss, fixed("ataxia")) | str_detect(outros_ss, fixed("CONVULS")) |
                                           str_detect(outros_ss, fixed("desvio de rima a esquerda")) |
                                           str_detect(outros_ss, fixed("estrabismo")) | str_detect(outros_ss, fixed("sonolencia")) |
                                           str_detect(outros_ss, fixed("agitacao psicomotora")) |
                                           str_detect(outros_ss, fixed("vertigem")) | str_detect(outros_ss, fixed("visao dupla"))|
                                           str_detect(outros_ss, fixed("mal convulsivo")) |
                                           str_detect(outros_ss, fixed("crise convulsiva")) | str_detect(outros_ss, fixed("movimentos tonico"))
                                         ~"Checked", TRUE ~ "Não Checked")) %>% 
  
  mutate(linfadenopatia = case_when(str_detect(outros_ss, fixed("adenomegalia"))|str_detect(outros_ss, fixed("linfadenomegalia"))|
                                      str_detect(outros_ss, fixed("linfodenomegalia"))~"Checked", TRUE ~ linfadenopatia)) %>% 
  mutate(edema_facial_labolho = case_when(str_detect(outros_ss, fixed("edema abdominal e facial")) | 
                                            str_detect(outros_ss, fixed("edema de face"))|str_detect(outros_ss, fixed("edema em face"))|
                                            str_detect(outros_ss, fixed("edema labial"))| str_detect(outros_ss, fixed("edema na face"))|
                                            str_detect(outros_ss, fixed("edema palpebral"))| str_detect(outros_ss, fixed("edema periorbital"))|
                                            str_detect(outros_ss, fixed("edema de palpebras"))| str_detect(outros_ss, fixed("edema bipalpebral"))
                                          ~"Checked", TRUE ~ "Não Checked")) %>% 
  mutate(outros_edema = case_when(str_detect(outros_ss, fixed("edema em face e abdominal"))| 
                                    str_detect(simpbr$outros_ss, fixed("edema abdominal"))|
                                    str_detect(outros_ss, fixed("edema,"))| str_detect(outros_ss, fixed("edema facial e abd")) |
                                    str_detect(outros_ss, fixed("edema de face, abdome"))| str_detect(outros_ss, fixed("edema de testiculos"))|
                                    str_detect(outros_ss, fixed("edema e eritema de saco escrotal"))|
                                    str_detect(outros_ss, fixed("edema em bolsa escrotal"))| str_detect(outros_ss, fixed("edema mmii"))|
                                    str_detect(outros_ss, fixed("sinais flogisticos"))| str_detect(outros_ss, fixed("anasarca"))|
                                    str_detect(outros_ss, fixed("hodeolo"))| str_detect(outros_ss, fixed("edema cervical"))
                                  ~ "Checked", TRUE ~ "Não Checked")) %>% 
  
  mutate(alteracoes_pele = case_when(str_detect(outros_ss, fixed("hiperemia")) | str_detect(outros_ss, fixed("hipocorad")) | 
                                       str_detect(outros_ss, fixed("equimose")) | str_detect(simpbr$outros_ss, fixed("ictericia"))
                                     ~"Checked", TRUE ~ alteracoes_pele)) %>%
  mutate(manchas_vermelhas = case_when(str_detect(outros_ss, fixed("exantema")) | 
                                         str_detect(outros_ss, fixed("rash")) | str_detect(outros_ss, fixed("mancha"))|
                                         str_detect(outros_ss, fixed("Eritema"))| str_detect(outros_ss, fixed("edema e eritema de saco escrotal"))|
                                         str_detect(outros_ss, fixed("lingua de morango"))|
                                         str_detect(outros_ss, fixed("lingua  cor de franboesa"))|str_detect(outros_ss, fixed("lingua em framboesa"))|
                                         str_detect(outros_ss, fixed("lesoes eritematosas"))| str_detect(outros_ss, fixed("lesoaes sangrantes"))|
                                         str_detect(outros_ss, fixed("lesoes de pele"))| str_detect(outros_ss, fixed("lesoes equimoticas"))|
                                         str_detect(outros_ss, fixed("lesoes cicatriciais de impetigo"))|
                                         str_detect(outros_ss, fixed("prurido"))|
                                         str_detect(outros_ss, fixed("petequia"))  ~"Checked", TRUE ~ manchas_vermelhas)) %>% 
  mutate(conjuntivite = case_when(str_detect(outros_ss, fixed("conjuntivite")) ~"Checked", TRUE ~ conjuntivite)) %>% 
  mutate(outros_dermatocutaneos = case_when(str_detect(outros_ss, fixed("descamacao")) | str_detect(outros_ss, fixed("fissura"))|
                                              str_detect(outros_ss, fixed("coceira"))|str_detect(outros_ss, fixed("prurisinos"))|
                                              str_detect(outros_ss, fixed("mucosa seca")) | str_detect(outros_ss, fixed("necrose tecidual"))|
                                              str_detect(outros_ss, fixed("ulceracoes")) | str_detect(outros_ss, fixed("lesoes mucosas")) | 
                                              str_detect(outros_ss, fixed("lesoesmucocutaneas")) | str_detect(outros_ss, fixed("lesoes aftosas"))
                                            ~"Checked", TRUE ~ "Não Checked")) %>% 
  mutate(hipotensao_choque = case_when(str_detect(outros_ss, fixed("choque"))|
                                         str_detect(outros_ss, fixed("hipotens")) ~"Checked", TRUE ~ "Não Checked")) %>% 
  mutate(outros_circulatorios = case_when(str_detect(outros_ss, fixed("epistaxe")) | str_detect(outros_ss, fixed("bradicardia"))|
                                            str_detect(outros_ss, fixed("disfuncao miocardica"))|str_detect(outros_ss, fixed("sopro"))|
                                            str_detect(outros_ss, fixed("hipervolemia")) | str_detect(outros_ss, fixed("ma perfusao periferica"))|
                                            str_detect(outros_ss, fixed("hematoquezia")) | str_detect(outros_ss, fixed("diminuicao dos pulsos")) |
                                            str_detect(outros_ss, fixed("diminuicao de pulsos")) | str_detect(outros_ss, fixed("cianose")) |
                                            str_detect(outros_ss, fixed("sangramento pulmonar")) | str_detect(outros_ss, fixed("hipervolemia"))
                                          ~ "Checked", TRUE ~ "Não Checked")) %>% 
  mutate(outros_muscolosque = case_when(str_detect(outros_ss, fixed("artralgia")) | str_detect(outros_ss, fixed("adnamia"))|
                                          str_detect(outros_ss, fixed("adinamia"))| str_detect(outros_ss, fixed("dismetria")) | 
                                          str_detect(outros_ss, fixed("prostracao")) | str_detect(outros_ss, fixed("dores articulares"))
                                        ~"Checked", TRUE ~ "Não Checked")) %>% 
  mutate(outros_geniturinario = case_when(str_detect(outros_ss, fixed("anuria"))| str_detect(outros_ss, fixed("diurese de cor escura"))|
                                            str_detect(outros_ss, fixed("diurese escura"))| str_detect(outros_ss, fixed("uremia"))|
                                            str_detect(outros_ss, fixed("infeccao urinaria")) | str_detect(outros_ss, fixed("hematuria"))|
                                            str_detect(outros_ss, fixed("uremia"))| str_detect(outros_ss, fixed("coluria")) ~"Checked", TRUE ~ "Não Checked"))

## Reclassifica e cria grupos de sinais/sintomas
simpbr = simpbr %>%
  mutate(respiratorios = case_when (tosse == "Checked" | dor_garganta == "Checked" | dor_degluticao == "Checked" | dispneia == "Checked"|
                                     coriza == "Checked" | saturacao_o2 == "Checked"| dor_peito == "Checked" | outros_respiratorios == "Checked" ~ "Checked", TRUE ~"Não Checked")) %>%
  mutate(gastrointestinais = case_when (gastro == "Checked" | nausea_vomito == "Checked" | diarreia == "Checked" | dor_abdm == "Checked" |
                                         dist_abdm == "Checked"| inapetencia == "Checked" | outros_gastro == "Checked" ~ "Checked", TRUE ~  "Não Checked")) %>%  
  mutate(dermatocutaneo = case_when (alteracoes_pele == "Checked" | manchas_vermelhas == "Checked" | conjuntivite == "Checked" |
                                      conjuntivite_defcaso == "Checked" | outros_dermatocutaneos == "Checked" ~ "Checked", TRUE ~ "Não Checked")) %>% 
  mutate(circulatorio = case_when(hipotensao_choque == "Checked" |  hipotensao_choque_defcaso == "Checked" | 
                                    taquicardia == "Checked" | outros_circulatorios == "Checked" ~ "Checked", TRUE~ "Não Checked")) %>% 
  mutate(neurologicos = case_when(cefaleia == "Checked" | confusao_mental == "Checked" | irritabilidade  == "Checked" | letargia == "Checked" |
                                    cervicalgia_rigidez ==  "Checked" | outros_neurologicos=="Checked" ~ "Checked", TRUE ~  "Não Checked")) %>% 
  mutate(muscolosqueleticos = case_when(mialgia =="Checked" |  outros_muscolosque =="Checked" ~ "Checked", TRUE~ "Não Checked")) %>% 
  mutate(edema = case_when(edema_cerv == "Checked" | edema_maospes == "Checked"| linfadenopatia == "Checked" | edema_facial_labolho =="Checked" | 
                             outros_edema == "Checked" ~ "Checked", TRUE~ "Não Checked")) %>% 
  mutate(geniturinario = case_when(oliguria == "Checked" | outros_geniturinario =="Checked" ~ "Checked", TRUE ~ "Não Checked")) %>%
  mutate(outros= case_when(str_detect(outros_ss, fixed("desidrat"))| str_detect(outros_ss, fixed("anosmia"))|
                             str_detect(outros_ss, fixed("gemente"))| str_detect(outros_ss, fixed("gemencia"))|
                             str_detect(outros_ss, fixed("ageusia"))| str_detect(outros_ss, fixed("hidrocele"))|
                             str_detect(outros_ss, fixed("queda no estado geral"))|str_detect(outros_ss, fixed("dor no dorso"))|
                             str_detect(outros_ss, fixed("dor testicular")) | str_detect(outros_ss, fixed("fadiga"))|
                             str_detect(outros_ss, fixed("calafrio")) | str_detect(outros_ss, fixed("otite")) | str_detect(outros_ss, fixed("serosite"))| 
                             str_detect(outros_ss, fixed("anorexia")) | str_detect(outros_ss, fixed("perda de peso"))|
                             str_detect(outros_ss, fixed("polidipsia")) | str_detect(outros_ss, fixed("sangramento pulmonar"))|
                             str_detect(outros_ss, fixed("polidipsia")) | str_detect(outros_ss, fixed("sangramento pulmonar"))|
                             str_detect(outros_ss, fixed("tontura")) ~ "Checked", TRUE~ "Não Checked")) 


# 11 - Reclassifica e cria variáveis de complicações ####
## Reclassifica e cria dicotômicas a partir do preenchimento da variável aberta "outros"
simpbr <- simpbr %>% 
  mutate(complic_ci = case_when(str_detect(outra_compicacao, fixed("valvulite"))
                                | str_detect(outra_compicacao, fixed("hipertens"))
                                | str_detect(outra_compicacao, fixed("cardiopatia"))| str_detect(outra_compicacao, fixed("pancardite"))
                                | str_detect(outra_compicacao, fixed("policerosite"))| str_detect(outra_compicacao, fixed("pericardite"))
                                | str_detect(outra_compicacao, fixed("dilatacao coronaria"))| str_detect(outra_compicacao, fixed("miocardite"))
                                | str_detect(outra_compicacao, fixed("disfuncao de arteria"))| str_detect(outra_compicacao, fixed("disfuncao cardiaca"))
                                | str_detect(outra_compicacao, fixed("disfuncao sistolica"))| str_detect(outra_compicacao, fixed("disfuncao do ventriculo"))
                                | str_detect(outra_compicacao, fixed("derrame pericardico"))| str_detect(outra_compicacao, fixed("sindrome hemofago"))
                                | str_detect(outra_compicacao, fixed("parada cardiorresp"))| str_detect(outra_compicacao, fixed("insuficiencia card"))
                                | str_detect(outra_compicacao, fixed("aneurisma de coro"))| str_detect(outra_compicacao, fixed("bradicardia"))
                                | str_detect(outra_compicacao, fixed("hipotensao"))| str_detect(outra_compicacao, fixed("coagulacao intravascular"))
                                | hipotens_drogas == "Checked" | hipertensao == "Checked" | evento_trombo == "Checked" | infarto == "Checked"
                                ~"Sim", TRUE ~ "Não")) %>% 
  mutate(complic_resp = case_when(str_detect(outra_compicacao, fixed("derrame pleural"))| str_detect(outra_compicacao, fixed ("hipertensao pulmonar"))
                                  |str_detect(outra_compicacao, fixed("atelectasia"))| str_detect(outra_compicacao, fixed ("broncoespasmo"))
                                  |str_detect(outra_compicacao, fixed("insuficiencia respirat"))| str_detect(outra_compicacao, fixed ("congestao pulmonar"))
                                  |str_detect(outra_compicacao, fixed("hemorragia de vias aereas"))| str_detect(outra_compicacao, fixed ("desconforto respirat"))
                                  |str_detect(outra_compicacao, fixed("sinusite"))| str_detect(outra_compicacao, fixed ("infiltrado peri"))
                                  |str_detect(outra_compicacao, fixed("srag"))| str_detect(outra_compicacao, fixed ("angustia respirat"))
                                  |str_detect(outra_compicacao, fixed("hemorragia pulmonar")) | pneumonia == "Checked"
                                  | ventilacao_invasiva == "Checked" | ventilacao_nao_invasiva == "Checked" | edema_agudo_pulmonar  == "Checked"
                                  ~"Sim", TRUE ~ "Não")) %>% 
  mutate(insuficienncai_renal = case_when(str_detect(outra_compicacao, fixed("insuficiencia renal"))
                                    ~"Checked", TRUE ~ insuficienncai_renal)) %>%
  mutate(complic_renal = case_when(str_detect(outra_compicacao, fixed("abcesso renal"))| str_detect(outra_compicacao, fixed ("sindrome nefrotica"))
                                 |str_detect(outra_compicacao, fixed("hematuria"))| str_detect(outra_compicacao, fixed ("dialitica"))
                                 | insuficienncai_renal == "Checked" | troca_plasmatica == "Checked"
                                 ~"Sim", TRUE ~ "Não")) %>% 

  mutate(sepse = case_when(str_detect(outra_compicacao, fixed("choque sep"))| str_detect(outra_compicacao, fixed ("choque distrib"))
                                   |str_detect(outra_compicacao, fixed("choque cardiog"))|str_detect(outra_compicacao, fixed("choque ana"))
                           ~"Checked", TRUE ~ sepse)) %>% 

  mutate(falencia_outros_orgaos = case_when(str_detect(outra_compicacao, fixed("encefalite"))| str_detect(outra_compicacao, fixed ("edema cerebral"))
                                            |str_detect(outra_compicacao, fixed("hematoquezia"))| str_detect(outra_compicacao, fixed ("sangue nas fezes"))
                                   |str_detect(outra_compicacao, fixed("adenite mesent"))| str_detect(outra_compicacao, fixed ("dinfuncao hep"))
                                   |str_detect(outra_compicacao, fixed("hemorragia digestiva"))| str_detect(outra_compicacao, fixed ("varizes esofagicas"))
                                   |str_detect(outra_compicacao, fixed("cetoacidose diabetica"))| str_detect(outra_compicacao, fixed ("colangite"))
                                   |str_detect(outra_compicacao, fixed("hepatite"))| str_detect(outra_compicacao, fixed ("pancreatite"))
                                   |str_detect(outra_compicacao, fixed("ativacao macrof"))
                                   |str_detect(outra_compicacao, fixed("anemia"))| str_detect(outra_compicacao, fixed ("guillain barre"))
                                   |str_detect(outra_compicacao, fixed ("desidratacao"))
                                   ~"Checked", TRUE ~ falencia_outros_orgaos))
  
    


## Cria complicações 
simpbr <- simpbr %>% 
  mutate (complicacoes = case_when(convulsoes =="Checked" | sepse =="Checked" | complic_resp =="Sim" | complic_renal =="Sim" |
                                     complic_ci == "Sim" | falencia_outros_orgaos =="Checked"
                                   ~ "Sim",TRUE ~"Não"))

# 12 - Reclassifica variáveis de tratamento ####
## Reclassifica dicotômicas a partir do preenchimento da variável aberta "outros"
simpbr = simpbr %>% 
  mutate(antiv = case_when(str_detect(tto_quais, fixed("aciclovir"))  ~ "Sim", TRUE ~ antiv)) %>% 
  
  mutate(cortic = case_when( str_detect(tto_quais, fixed("dexametasona")) | str_detect(tto_quais, fixed("prednisona")) ~ "Sim", TRUE ~ cortic)) %>%   
  
  mutate(imunog = case_when( str_detect(tto_quais, fixed("tocilizumabe")) | str_detect(tto_quais, fixed("tocilizumab")) ~ "Sim", TRUE ~ imunog)) %>%   
  
  mutate(anticoag = case_when( str_detect(tto_quais, fixed("aas")) | str_detect(tto_quais, fixed("acido acetilsalicilico"))
                               | str_detect(tto_quais, fixed("acido acetil salicilico")) | str_detect(tto_quais, fixed("aspirina")) 
                               | str_detect(tto_quais, fixed("heparina")) | str_detect(outros_ss, fixed("enoxaparina")) 
                               | str_detect(tto_quais, fixed("enoxeparina")) | str_detect(tto_quais, fixed("clexane")) 
                               ~ "Sim", TRUE ~ anticoag)) %>%   
  
  mutate(antib = case_when( str_detect(tto_quais, fixed("antibiotico")) | str_detect(tto_quais, fixed("atb")) 
                            |str_detect(tto_quais, fixed("azitromicina")) | str_detect(tto_quais, fixed("astro")) 
                            |str_detect(tto_quais, fixed("ceftriaxona")) | str_detect(tto_quais, fixed("ceftriaxone")) 
                            |str_detect(tto_quais, fixed("rocefin")) | str_detect(tto_quais, fixed("rocefim")) 
                            |str_detect(tto_quais, fixed("cefepime")) | str_detect(tto_quais, fixed("oxacilina")) 
                            |str_detect(tto_quais, fixed("oxanon")) | str_detect(tto_quais, fixed("vancomicina")) 
                            |str_detect(tto_quais, fixed("meropenem")) | str_detect(tto_quais, fixed("meronem")) 
                            |str_detect(tto_quais, fixed("meropenen")) | str_detect(tto_quais, fixed("amoxicilina")) 
                            |str_detect(tto_quais, fixed("amoxacilina")) | str_detect(tto_quais, fixed("amoxicilina")) 
                            |str_detect(tto_quais, fixed("clindamicina")) | str_detect(tto_quais, fixed("clinda")) 
                            |str_detect(tto_quais, fixed("metronidazol")) | str_detect(tto_quais, fixed("teicoplanina")) 
                            |str_detect(tto_quais, fixed("targocid")) | str_detect(tto_quais, fixed("cefuroxima")) 
                            |str_detect(tto_quais, fixed("zinnat")) | str_detect(tto_quais, fixed("cefuro")) 
                            |str_detect(tto_quais, fixed("tazocin")) | str_detect(tto_quais, fixed("amicacina")) 
                            |str_detect(tto_quais, fixed("cefazolina")) | str_detect(tto_quais, fixed("ampicilina")) 
                            |str_detect(tto_quais, fixed("claritromicina")) | str_detect(tto_quais, fixed("clarotormicina")) 
                            |str_detect(tto_quais, fixed("ciprofloxacino")) | str_detect(tto_quais, fixed("ciprofloxacina")) 
                            |str_detect(tto_quais, fixed("cipro")) | str_detect(tto_quais, fixed("garamicina")) 
                            |str_detect(tto_quais, fixed("gentamicina")) | str_detect(tto_quais, fixed("linezolida")) 
                            |str_detect(tto_quais, fixed("piperaciclina")) | str_detect(tto_quais, fixed("polimixina")) 
                            |str_detect(tto_quais, fixed("antibiotico")) | str_detect(tto_quais, fixed("atb")) 
                            |str_detect(tto_quais, fixed("antibiotico")) | str_detect(tto_quais, fixed("atb")) 
                            |str_detect(tto_quais, fixed("antibiotico")) | str_detect(tto_quais, fixed("atb")) 
                            |str_detect(tto_quais, fixed("antibiotico")) | str_detect(tto_quais, fixed("atb"))
                            ~ "Sim", TRUE ~ "Não")) %>%
  
  mutate(outros_tto = case_when(str_detect(tto_quais, fixed("ivermectina")) | str_detect(tto_quais, fixed("albendazol")) 
                                |str_detect(tto_quais, fixed("permetrina"))
                                |str_detect(tto_quais, fixed("vasoativ")) | str_detect(tto_quais, fixed("adrenalina")) 
                                 |str_detect(tto_quais, fixed("epinefrina")) |str_detect(tto_quais, fixed("noradrenalina"))
                                 |str_detect(tto_quais, fixed("dexclorfeniramina")) | str_detect(tto_quais, fixed("loratadina"))
                                 |str_detect(tto_quais, fixed("hidroxizina"))
                                 |str_detect(tto_quais, fixed("fluconazol")) | str_detect(tto_quais, fixed("anfotericina")) 
                                 |str_detect(tto_quais, fixed("zoltec")) |str_detect(tto_quais, fixed("zoltec"))
                                 |str_detect(tto_quais, fixed("caspofungina")) | str_detect(tto_quais, fixed("cancidas"))
                                 |str_detect(tto_quais, fixed("nistatina")) | str_detect(tto_quais, fixed("cetoconazol"))
                                 |str_detect(tto_quais, fixed("captopril")) | str_detect(tto_quais, fixed("hidralazina"))
                                 |str_detect(tto_quais, fixed("carvedilol"))
                                 |str_detect(tto_quais, fixed("omeprazol")) | str_detect(tto_quais, fixed("simeticona"))
                                 |str_detect(tto_quais, fixed("buscopam")) |str_detect(tto_quais, fixed("ondansetrona"))
                                 |str_detect(tto_quais, fixed("enterogermina"))
                                 |str_detect(tto_quais, fixed("espironolactona")) | str_detect(tto_quais, fixed("furosemida")) 
                            |str_detect(tto_quais, fixed("ambroxol")) | str_detect(tto_quais, fixed("albumina")) 
                            |str_detect(tto_quais, fixed("plaquetas")) | str_detect(tto_quais, fixed("concentrado de hemacias")) 
                            |str_detect(tto_quais, fixed("vitamina")) | str_detect(tto_quais, fixed("cristaloides")) 
                            |str_detect(tto_quais, fixed("soro")) | str_detect(tto_quais, fixed("antitermico")) 
                            |str_detect(tto_quais, fixed("nutricao parenteral")) | str_detect(tto_quais, fixed("npt")) 
                            |str_detect(tto_quais, fixed("hemodialise")) | str_detect(tto_quais, fixed("dipirona")) 
                            |str_detect(tto_quais, fixed("naproxeno")) | str_detect(tto_quais, fixed("tramadol")) 
                            |str_detect(tto_quais, fixed("milrinona")) | str_detect(tto_quais, fixed("milrinone")) 
                            |str_detect(tto_quais, fixed("dobutamina")) | str_detect(tto_quais, fixed("ciclosporina")) 
                            |str_detect(tto_quais, fixed("etoposido")) | str_detect(tto_quais, fixed("etoposide")) 
                            |str_detect(tto_quais, fixed("risperidona")) | str_detect(tto_quais, fixed("hidantal")) 
                            |str_detect(tto_quais, fixed("sedativos")) | str_detect(tto_quais, fixed("colchicina")) 
                            |str_detect(tto_quais, fixed("hidroxicloroquina")) | str_detect(tto_quais, fixed("insulina")) 
                            |str_detect(tto_quais, fixed("midazolam")) | str_detect(tto_quais, fixed("fentanil")) 
                            |str_detect(tto_quais, fixed("acido valproico")) | str_detect(tto_quais, fixed("fenobarbital")) 
                            |str_detect(tto_quais, fixed("cetamina"))
                            ~ "Sim", TRUE ~ "Não"))

table(simpbr$imunog, useNA = "always")
table(simpbr$cortic, useNA = "always")
table(simpbr$anticoag, useNA = "always")
table(simpbr$antiv, useNA = "always")
table(simpbr$antib, useNA = "always")
table(simpbr$outros_tto, useNA = "always")

# 13 - Reclassifica resultados de eco a partir do preenchimento do resultado "outros"####
simpbr = simpbr %>% 
  mutate(ecocardio___1 = case_when (str_detect(ecocardio_outro_2, fixed("miocardiopatia"))
                                    |str_detect(ecocardio_outro_2, fixed("miocardite"))
                                      ~ "Checked", TRUE ~ ecocardio___1)) %>% 
  mutate(ecocardio___2 = case_when (str_detect(ecocardio_outro_2, fixed("derrame pericard"))
                                    ~ "Checked", TRUE ~ ecocardio___2)) %>% 
  mutate(ecocardio___4 = case_when (str_detect(ecocardio_outro_2, fixed("dilat leve"))
                                    |str_detect(ecocardio_outro_2, fixed("dilatacao"))
                                    |str_detect(ecocardio_outro_2, fixed("discreto aumento de ve"))
                                    |str_detect(ecocardio_outro_2, fixed("disfuncao moderada"))
                                    |str_detect(ecocardio_outro_2, fixed("disfuncao sistolica"))
                                    |str_detect(ecocardio_outro_2, fixed("ectasia"))
                                    |str_detect(ecocardio_outro_2, fixed("regurgitacoes mitral"))
                                    |str_detect(ecocardio_outro_2, fixed("funcao sistolica"))
                                    |str_detect(ecocardio_outro_2, fixed("hiperrefringencia da parede"))
                                    |str_detect(ecocardio_outro_2, fixed("hipertrofia excentrica"))
                                    |str_detect(ecocardio_outro_2, fixed("insuficiencia de valvulas"))
                                    |str_detect(ecocardio_outro_2, fixed("insuficiencia mitral"))
                                    |str_detect(ecocardio_outro_2, fixed("insuficiencia tricuspide"))
                                    |str_detect(ecocardio_outro_2, fixed("leve espessamento"))
                                    |str_detect(ecocardio_outro_2, fixed("regurgitacao tricuspide"))
                                    |str_detect(ecocardio_outro_2, fixed("regurgitacao discreta"))
                                    |str_detect(ecocardio_outro_2, fixed("insulf. mitral"))
                                    ~ "Checked", TRUE ~ ecocardio___4))
  
table(simpbr$ecocardio___4)

# 14 - Reclassifica resultados de ultrasson a partir do preenchimento do resultado "outros"####
simpbr = simpbr %>% 
  mutate(ultrassonabd___2 = case_when (str_detect(ecocardio_outro, fixed("leve aumento das dim"))
                                    ~ "Checked", TRUE ~ ultrassonabd___2)) %>% 
  mutate(ultrassonabd___5 = case_when (str_detect(ecocardio_outro, fixed("linfonodos mesentericos"))
                                       |str_detect(ecocardio_outro, fixed("adenite mesent"))
                                       |str_detect(ecocardio_outro, fixed("adenopatia mesent"))
                                    ~ "Checked", TRUE ~ ultrassonabd___5)) 


# 15 - Reclassificar variáveis dicotômicas da definição de caso a partir de outras variáveis disponíveis no banco ####
simpbr = simpbr %>% 
  mutate(conjuntivite_defcaso = case_when (conjuntivite == "Checked" ~ "Checked", TRUE ~ conjuntivite_defcaso)) %>% 
  mutate(hipotensao_choque_defcaso = case_when(hipotensao_choque == "Checked" ~ "Checked", TRUE ~ hipotensao_choque_defcaso)) %>% 
  mutate(difuncao_cardio = case_when(ecocardio___1 == "Checked" | ecocardio___2 == "Checked"|
                                       ecocardio___3 == "Checked" | ecocardio___4 == "Checked" |  
                                       tropon == "Alterado" | ntprobnp == "Alterado" ~ "Checked", TRUE ~ difuncao_cardio)) %>% 
  mutate(coagulopatia = case_when(tp == "Alterado" | ttpta == "Alterado" | d_dimero == "Alterado" | evento_trombo == "Checked" |
                                    fibring == "Alterado" ~ "Checked", TRUE ~ coagulopatia )) %>% 
  mutate(gastro = case_when(diarreia == "Checked" | nausea_vomito == "Checked" | dor_abdm == "Checked" ~"Checked", TRUE ~ gastro)) %>% 
  mutate(inflamacao = case_when(vhs == "Alterado" | procalct == "Alterado" | protecr == "Alterado" | il6 == "Alterado" ~ "Checked", TRUE ~ inflamacao)) %>% 
  mutate(evid_covid = case_when(result_pcr == "Detectável para SARS-CoV-2 (Covid-19)" |
                                  result_tr == "Reagente para SARS-CoV-2 (Covid-19)"|
                                  result_sorol == "Reagente para SARS-CoV-2 (Covid-19)"|
                                  ctt_caso_suspeito == "Sim" ~ "Checked", TRUE ~ evid_covid)) 


# 16 - Cria variável caso de SIM-P ####
simpbr = simpbr %>% 
  mutate(ssdef2 = case_when(conjuntivite_defcaso == "Checked" ~1, TRUE~0)) %>% 
  mutate(ssdef3 = case_when(hipotensao_choque == "Checked" ~1, TRUE~0)) %>%
  mutate(ssdef4 = case_when(difuncao_cardio == "Checked" ~1, TRUE~0)) %>%
  mutate(ssdef5 = case_when(coagulopatia == "Checked" ~1, TRUE~0)) %>%
  mutate(ssdef6 = case_when(gastro == "Checked" ~1, TRUE~0)) %>%
  mutate(ssdefn = ssdef2 + ssdef3 + ssdef4 + ssdef5 + ssdef6) %>% 
  mutate(ssdef  = case_when(ssdefn>1 ~ 1, TRUE ~ 0)) %>% 
  mutate(caso = case_when(febre == "Checked" & ssdefn>1 & inflamacao == "Checked" &
                            evid_covid == "Checked" & idade<20 & idade>=0 & (class_fin != "Descartado (outro diagnóstico)" | is.na(class_fin)) ~1, TRUE~0)) #atencao, Não inclui o 8, efastadas outras hipóteses. 
table(simpbr$caso) #Freq

# 17 - Cria dicotômicas para testes laboratoriais de Covid-19 ####
simpbr = simpbr %>% 
  mutate(covidlab = case_when(result_pcr == "Detectável para SARS-CoV-2 (Covid-19)" |
                                result_tr == "Reagente para SARS-CoV-2 (Covid-19)" |
                                result_sorol == "Reagente para SARS-CoV-2 (Covid-19)" ~"Sim",
                              TRUE ~ "Não")) %>% 
  mutate(sorologico = case_when(result_tr == "Reagente para SARS-CoV-2 (Covid-19)" |
                                 result_sorol == "Reagente para SARS-CoV-2 (Covid-19)"
                              ~"Sim", TRUE ~ "Não")) %>% 
  mutate(sosorologico = case_when(covidlab == "Sim" & result_pcr != "Detectável para SARS-CoV-2 (Covid-19)"
                                  ~"Sim", TRUE ~ "Não")) %>% 
  mutate(molecular = case_when(result_pcr == "Detectável para SARS-CoV-2 (Covid-19)" ~"Sim",
                              TRUE ~ "Não"))
#Freq
table(simpbr$covidlab) 
table(simpbr$sosorologico)
table(simpbr$molecular)

# 18 - Calcula tempo de UTI ####
table(simpbr$intern_uti, useNA = "always") #qts crianças foram p UTI?
simpbr$tempo_uti=round(time_length(difftime(simpbr$dt_alt_uti,simpbr$dt_int_uti),"days"),0) #calcula o tempo na UTI em dias                                                 
table(simpbr$tempo_uti) #tabula tempo de UTI em dias para conferir se o comando ficou ok.
table(simpbr$intern_uti, simpbr$tempo_uti, useNA = "always") #tabula uti y/n segundo tempo de internação em dias para conferir NA

# 19 - Calcula tempo de Internação ####
table(simpbr$dt_int, useNA = "always") #quantos registros nao tem data de internação
simpbr$tempo_int=round(time_length(difftime(simpbr$dt_evolucao,simpbr$dt_int),"days"),0) #calcula o tempo de internacao em dias                                                 
table(simpbr$tempo_int) #tabula tempo de UTI em dias para conferir se o comando ficou ok

# 20 - Calcula semana epidemiológica de início dos sinais/sintomas ####
simpbr = simpbr %>% mutate(SE=epiweek(dt_inicio_ss))
table(simpbr$SE, useNA = "always") # Freq

# 21 - Transforma todas as variaveis Y/N em fator para o "Sim" aparecer primeiro e facilitar a tabela 2x2 ####
simpbr <- simpbr %>% mutate_at(c("complicacoes","imunog", "anticoag", "cortic", "antiv", "tto_outros", "comorb", "intern_uti", "covidlab", "sorologico", "ctt_caso_suspeito", "pre_kawasaki", "comorb"), ~forcats::fct_relevel(., "Sim", "Não"))

# 22 - Tabulações de conferência ####
  ## Cria tabelas para leitura das variáveis abertas cnsideradas no estudo
  outros_ss <- simpbr %>% group_by(outros_ss) %>% filter(caso==1) %>% count(caso) %>% arrange(desc(outros_ss)) #sinais e sintmas
  outra_complicacao <- simpbr %>% group_by(outra_compicacao) %>% filter(caso==1) %>% count(caso)  %>% arrange(desc(outra_compicacao)) #complicações
  outras_comorb <- simpbr %>% group_by(comorb_otrs) %>% filter(caso==1) %>% count(caso) %>% arrange(comorb_otrs) #comorbidades
  outros_ttos <- simpbr %>% group_by(tto_quais) %>% filter(caso==1) %>% count(caso) %>% arrange(tto_quais) #tratamentos
  outros_rt_tomo <- simpbr %>% group_by(imagem_resultado_outro) %>% filter(caso==1) %>% count(caso) %>% arrange(imagem_resultado_outro) #resultados de exames de imagem
  outros_ultrasonnabd <- simpbr %>% group_by(ecocardio_outro) %>% filter(caso==1) %>% count(caso) %>% arrange(ecocardio_outro) #ecocardiografia
  outros_eco <- simpbr %>% group_by(ecocardio_outro_2) %>% filter(caso==1) %>% count(caso) %>% arrange(ecocardio_outro_2) #
  outros_eletro <- simpbr %>% group_by(eletro_result) %>% filter(caso==1) %>% count(caso) %>% arrange(eletro_result) # eletro
  
# 23 - Seleciona apenas os casos ####
simpbr <- simpbr %>% filter(caso == 1)


################################## ____ TABULACAO DE DADOS _____ ################################## 

## Fluxograma inicial
table(simpbr$evolucao, useNA = "always")

####____#### TABELA 1 - CARACTERÍSTICAS DO SUJEITO ####
# 01 - Cria objeto tempprário com  tabulação da variável idade ####
tb1idade <- bind_cols(
  
  simpbr %>% filter(faixet=="corrigir") %>% 
    summarise(mediana=round(median(idade),1), q1=round(quantile(idade,0.25),1), q3=round(quantile(idade,0.75),1)) %>% 
    mutate(var=paste0("Idade [Mediana(Q1-Q3)]"))%>% dplyr::select(-mediana, -q1, -q3), 
  
  simpbr %>% filter(simpbr$idade>=0) %>% 
    summarise(mediana=round(median(idade),1), q1=round(quantile(idade,0.25),1), q3=round(quantile(idade,0.75),1)) %>% 
    mutate("Total" = paste0(mediana, " (", q1, " - ",q3,")")) %>% dplyr::select(-mediana, -q1, -q3),
  
  simpbr %>% filter(evolucao3=="1 - Óbito" & idade>=0) %>% 
    summarise(mediana=round(median(idade),1), q1=round(quantile(idade,0.25),1), q3=round(quantile(idade,0.75),1)) %>% 
    mutate("1 - Óbito" = paste0(mediana, " (", q1, " - ",q3,")")) %>% dplyr::select(-mediana, -q1, -q3),
  
  simpbr %>% filter(evolucao3=="2 - Alta" & idade>=0) %>% 
    summarise(mediana=round(median(idade),1), q1=round(quantile(idade,0.25),1), q3=round(quantile(idade,0.75),1)) %>% 
    mutate("2 - Alta" = paste0(mediana, " (", q1, " - ",q3,")")) %>% dplyr::select(-mediana, -q1, -q3),
) 


# 02 - Cria objeto temprorário com  tabulações das demais variáveis do sujeito segundo alta/cura (estratificada) ####
tb01 <- data.frame(var = "Faixa Etária", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA, check.names = F)  %>% 
  add_row(simpbr %>% count(var = faixet, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
          ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Sexo", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = sexo, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Raça/Cor", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(is.na(racacor) == FALSE) %>% count(var = racacor, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
          ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Presença de comorbidade", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>% 
  add_row(simpbr  %>% count(var = comorb, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Respiratórias", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>% 
  add_row(simpbr %>% count(var = respre, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Neurológicas", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = doencaneurologica, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Cardíacas", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>% 
  add_row(simpbr %>% count(var = cardiopatia, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Outras Comorbidades", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>% 
  add_row(simpbr %>% count(var = outrascomorbidades, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result"))

# 03 - Cria objeto temporário com  tabulação das variáveis do sujeito (total) ####
tb01tot <- data.frame(var = "Faixa Etária", `Total`=NA, `<NA>`=NA, check.names = F)  %>% 
  add_row(simpbr %>% count(var = faixet) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%  
  add_row(var = "Sexo", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = sexo) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Raça/Cor", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(is.na(racacor) == FALSE) %>% count(var = racacor) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Presença de comorbidade", `Total`=NA, `<NA>`=NA) %>%  
  add_row(simpbr %>% count(var = comorb) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Respiratórias", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = respre) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Neurológicas", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = doencaneurologica) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Cardiopatias", `Total`=NA, `<NA>`=NA) %>%  
  add_row(simpbr %>% count(var = cardiopatia) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%  
  add_row(var = "Outras comorbidades", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = outrascomorbidades) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop))

# 04 - Cria tabela final e exclui objetos temporários ####
  ##Junta tabela estratificada por desfecho fatal com tabela de total 
  tb1 <- bind_cols(tb01tot, tb01) %>% dplyr::select(-var1,-"<NA>",-"NA") 
  ## Junta tabela de idade
  tb1 <- bind_rows(tb1idade, tb1)
  ## Exclui objetos temporários
  rm(tb01, tb01tot, tbidade)

# 05 - Tabulações de apoio à análise de dados ####
table(simpbr$racacor, simpbr$evolucao3, useNA = "always")


####____#### TABELA 2 - CARACTERÍSTICAS CLÍNICAS, DA INTERNAÇÃO E EVOLUÇÃO ####
# 01 - Cria objeto temporário com  tabulação das variáveis clínicas segundo desfecho fatal (estratificada) ####
tb02 <- data.frame(var = "Sinais/sintomas", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA, check.names = F)  %>% 
  add_row(var = "Gastrointestinais", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = gastrointestinais, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Náusea ou vômito", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = nausea_vomito, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Diareia", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = diarreia, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Dor abdominal", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = dor_abdm, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Dermatocutâneos", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = dermatocutaneo, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Alterações na cor da pele (Ex.: palidez, cianose)", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = alteracoes_pele, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Manchas vermelhas pelo corpo (Ex.: exantema, rash etc.)", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = manchas_vermelhas, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Conjuntivite", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = conjuntivite_defcaso, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Respiratórios", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = respiratorios, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Dispneia", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = dispneia, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Saturação O2 < 95% ar ambiente", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = saturacao_o2, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Tosse", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = tosse, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Coriza", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = coriza, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Dor de garganta", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = dor_garganta, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Neurológicos", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = neurologicos, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Cefaleia", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = cefaleia, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Irritabilidade", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = irritabilidade, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Letargia", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = letargia, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Confusão mental", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = confusao_mental, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Circulatórios e hemodinânmicos", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = circulatorio, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Hipotensão/choque", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = hipotensao_choque_defcaso, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Taquicardia", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = taquicardia, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Edema", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = edema, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Edema de mãos ou pés", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = edema_maospes, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Linfadenopatia", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = linfadenopatia, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Geniturinários", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = geniturinario, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Oligúria", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = oliguria, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Muscolosqueléticos", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = muscolosqueleticos, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Mialgia", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = mialgia, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Outros", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = outros, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Tratamento", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(var = "Imunoglobulina", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>% 
  add_row(simpbr %>%  filter (is.na(imunog) == FALSE) %>% count(var = imunog, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Anticoagulante", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>%  filter (is.na(anticoag) == FALSE) %>% count(var = anticoag, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Antibiótico", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>%  filter (is.na(antib) == FALSE) %>% count(var = antib, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Antiviral", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>% 
  add_row(simpbr %>%  filter (is.na(antiv) == FALSE) %>% count(var = antiv, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Corticóide", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>% 
  add_row(simpbr %>%  filter (is.na(cortic) == FALSE) %>% count(var = cortic, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Antibiótico", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>% 
  add_row(simpbr %>%  filter (is.na(antib) == FALSE) %>% count(var = antib, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Outros", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>%  filter (is.na(outros_tto) == FALSE) %>% count(var = outros_tto, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Complicações", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>%  filter (is.na(complicacoes) == FALSE) %>% count(var = complicacoes, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Cardíacas e circulatórias", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>%  filter (is.na(complic_ci) == FALSE) %>% count(var = complic_ci, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Hipotensão (necessidade de drogas vasoativas)", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>%  filter (is.na(hipotens_drogas) == FALSE) %>% count(var = hipotens_drogas, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Respiratórias", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>%  filter (is.na(complic_resp) == FALSE) %>% count(var = complic_resp, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Necessidade de ventilação invasiva", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>%  filter (is.na(ventilacao_invasiva) == FALSE) %>% count(var = ventilacao_invasiva, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Pneumonia", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>%  filter (is.na(pneumonia) == FALSE) %>% count(var = pneumonia, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Necessidade de ventilação não invasiva", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>%  filter (is.na(ventilacao_nao_invasiva) == FALSE) %>% count(var = ventilacao_nao_invasiva, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Geniturinárias", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>%  filter (is.na(complic_renal) == FALSE) %>% count(var = complic_renal, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Insuficiência renal", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>%  filter (is.na(insuficienncai_renal) == FALSE) %>% count(var = insuficienncai_renal, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Convulsões", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>%  filter (is.na(convulsoes) == FALSE) %>% count(var = convulsoes, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Outras", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>%  filter (is.na(falencia_outros_orgaos) == FALSE) %>% count(var = falencia_outros_orgaos, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Internação em UTI", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>%  filter (is.na(intern_uti) == FALSE) %>% count(var = intern_uti, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Diagnóstico médico final", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>%  filter (is.na(diag_med_final) == FALSE) %>% count(var = diag_med_final, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result"))

# 02 - Cria objeto temprorário com  tabulação das variáveis clínicas (total) ####
tb02tot <- data.frame(var = "Sinais/Sintomas", `Total`=NA, `<NA>`=NA, check.names = F)  %>% 
  add_row(var = "Gastrointestinais", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = gastrointestinais) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Náusea ou vômito", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = nausea_vomito) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Diareia", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = diarreia) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Dor abdominal", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = dor_abdm) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Dermatocutâneos", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = dermatocutaneo) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Alterações na cor da pele (Ex.: palidez, cianose)", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = alteracoes_pele) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Manchas vermelhas pelo corpo (Ex.: exantema, rash etc.)", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = manchas_vermelhas) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Conjuntivite", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = conjuntivite_defcaso) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Respiratórios", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = respiratorios) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Dispneia", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = dispneia) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Saturação O2 < 95% ar ambiente", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = saturacao_o2) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Tosse", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = tosse) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Coriza", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = coriza) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Dor de garganta", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = dor_garganta) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Neurológicos", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = neurologicos) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Cefaleia", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = cefaleia) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Irritabilidade", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = irritabilidade) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Letargia", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = letargia) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Confusão mental", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = confusao_mental) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Circulatórios e hemodinânmicos", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = circulatorio) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Hipotensão/choque", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = hipotensao_choque_defcaso) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Taquicardia", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = taquicardia) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Edema", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = edema) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Edema de mãos ou pés", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = edema_maospes) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Linfadenopatia", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = linfadenopatia) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Geniturinários", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = geniturinario) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Oligúria", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = oliguria) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>%  ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Muscolosqueléticos", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = muscolosqueleticos) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Mialgia", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = mialgia) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Outros", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = outros) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Tratamento", `Total`=NA, `<NA>`=NA) %>%
  add_row(var = "Imunoglobulina", `Total`=NA, `<NA>`=NA) %>%  
  add_row(simpbr %>%  filter (is.na(imunog) == FALSE) %>% count(var = imunog) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Anticoagulante", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>%  filter (is.na(anticoag) == FALSE) %>% count(var = anticoag) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Antibiótico", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>%  filter (is.na(antib) == FALSE) %>% count(var = antib) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Antiviral", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>%  filter (is.na(antiv) == FALSE) %>% count(var = antiv) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%  
  add_row(var = "Corticóide", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>%  filter (is.na(cortic) == FALSE) %>% count(var = cortic) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Antibióticos", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>%  filter (is.na(antib) == FALSE) %>% count(var = antib) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Outros", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>%  filter (is.na(outros_tto) == FALSE) %>% count(var = outros_tto) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Complicações", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = complicacoes) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Cardíacas e circulatórias", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = complic_ci) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Hipotensão (necessidade de drogas vasoativas)", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = hipotens_drogas) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Respiratórias", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = complic_resp) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Necessidade de ventilação invasiva", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = ventilacao_invasiva) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Pneumonia", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = pneumonia) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Necessidade de ventilação não invasiva", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = ventilacao_nao_invasiva) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Geniturinárias", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = complic_renal) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Insuficiência renal", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = insuficienncai_renal) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Convulsões", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = convulsoes) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Outras", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = falencia_outros_orgaos) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Internação em UTI", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>%  filter (is.na(intern_uti) == FALSE) %>% count(var = intern_uti) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Diagnóstico médico final", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>%  filter (is.na(diag_med_final) == FALSE) %>% count(var = diag_med_final) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop))

# 03 - Cria objeto temporário com  tabulação da variável tempo de internação ####
tb02tempoint <- bind_cols(
  
  simpbr %>% filter(evolucao3=="corrigir") %>% 
    summarise(mediana=round(median(tempo_int),1), q1=round(quantile(tempo_int,0.25),1), q3=round(quantile(tempo_int,0.75),1)) %>% 
    mutate(var=paste0("Tempo de internação"))%>% dplyr::select(-mediana, -q1, -q3), 

  simpbr %>% filter(simpbr$tempo_int>=0) %>% 
    summarise(mediana=round(median(tempo_int),1), q1=round(quantile(tempo_int,0.25),1), q3=round(quantile(tempo_int,0.75),1)) %>% 
    mutate(Total = paste0(mediana, " (", q1, " - ",q3,")")) %>% dplyr::select(-mediana, -q1, -q3),
  
  simpbr %>% filter(evolucao3 == "1 - Óbito" & simpbr$tempo_int>=0) %>% 
    summarise(mediana=round(median(tempo_int),1), q1=round(quantile(tempo_int,0.25),1), q3=round(quantile(tempo_int,0.75),1)) %>% 
    mutate("1 - Óbito" = paste0(mediana, " (", q1, " - ",q3,")")) %>% dplyr::select(-mediana, -q1, -q3),
  
  simpbr %>% filter(evolucao3=="2 - Alta" & simpbr$tempo_int>=0) %>% 
    summarise(mediana=round(median(tempo_int),1), q1=round(quantile(tempo_int,0.25),1), q3=round(quantile(tempo_int,0.75),1)) %>% 
    mutate("2 - Alta" = paste0(mediana, " (", q1, " - ",q3,")")) %>% dplyr::select(-mediana, -q1, -q3)
) 

# 04 - Cria objeto temporário com  tabulação do tempo de UTI ####
tb02tempouti <- bind_cols(
  
  simpbr %>% filter(evolucao3=="corrigir") %>% 
    summarise(mediana=round(median(tempo_uti),1), q1=round(quantile(tempo_uti,0.25),1), q3=round(quantile(tempo_uti,0.75),1)) %>% 
    mutate(var=paste0("Tempo de UTI"))%>% dplyr::select(-mediana, -q1, -q3), 
  
  simpbr %>% filter(simpbr$tempo_uti>=0) %>% 
    summarise(mediana=round(median(tempo_uti),1), q1=round(quantile(tempo_uti,0.25),1), q3=round(quantile(tempo_uti,0.75),1)) %>% 
    mutate(Total = paste0(mediana, " (", q1, " - ",q3,")")) %>% dplyr::select(-mediana, -q1, -q3),
  
  simpbr %>% filter(evolucao3 == "1 - Óbito" & simpbr$tempo_uti>=0) %>% 
    summarise(mediana=round(median(tempo_uti),1), q1=round(quantile(tempo_uti,0.25),1), q3=round(quantile(tempo_uti,0.75),1)) %>% 
    mutate("1 - Óbito" = paste0(mediana, " (", q1, " - ",q3,")")) %>% dplyr::select(-mediana, -q1, -q3),
  
  simpbr %>% filter(evolucao3=="2 - Alta" & simpbr$tempo_uti>=0) %>% 
    summarise(mediana=round(median(tempo_uti),1), q1=round(quantile(tempo_uti,0.25),1), q3=round(quantile(tempo_uti,0.75),1)) %>% 
    mutate("2 - Alta" = paste0(mediana, " (", q1, " - ",q3,")")) %>% dplyr::select(-mediana, -q1, -q3),
) 

# 05 - Cria tabela final e exclui objetos temporários ####
  ## Junta tabela estratificada por desfecho fatal com tabela de total
  tb20 <- bind_cols(tb02tot, tb02) %>% dplyr::select(-var1, -"<NA>", -"NA")
  ##Junta tabelas de tempo de internação e UTI
  tb2  <- bind_rows(tb20,tb02tempoint, tb02tempouti)
  ## Exclui objetos temporários 
  rm(tb02, tb02tot, tb20, tb02tempouti, tb02tempoint)  
# 06 - Tabulações de apoio à análise de dados ####
  ## Calcula o total de registros com informação disponível para algumas variáveis em que foram excluídos os ignorados 
  tb02_tot_imunog <- simpbr %>% filter(is.na(imunog) == FALSE) %>% count(evolucao3) %>% spread(key ="evolucao3", value="n") #Imunglobulina
  table(is.na(simpbr$imunog))
  tb02_tot_imunog2 <- simpbr %>% filter(is.na(imunog) == FALSE) %>% count(imunog) #Imunglobulina
  tb02_tot_cortc <- simpbr %>% filter(is.na(cortic) == FALSE) %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")  #Corticóides
  tb02_tot_antic <- simpbr %>% filter(is.na(anticoag) == FALSE) %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")#Anticoagulantes
  tb02_tot_antiv <- simpbr %>% filter(is.na(antiv) == FALSE) %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")   #Antivirais
  tb02_tot_tto_outros <- simpbr %>% filter(is.na(tto_outros) == FALSE) %>% count(evolucao3) %>% spread(key ="evolucao3", value="n") #Outrs tratamentos
  tb02_tot_uti <- simpbr %>% filter(is.na(intern_uti) == FALSE) %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")        #Internação em UTI
  table(is.na(simpbr$intern_uti))
  
  tb02_tot_diag <- simpbr %>% filter(is.na(diag_med_final) == FALSE) %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")   #Diagnóstic Médico final
  table(is.na(simpbr$diag_med_final))
  
  tb02_tot_tempoint <- simpbr %>% filter(is.na(tempo_int) == FALSE) %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")    #Tempo de Internação 
  table(is.na(simpbr$tempo_int))
  
  tb02_tot_tempouti <- simpbr %>% filter(is.na(tempo_uti) == FALSE) %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")    #Tempo de internação em UTI
  table(is.na(simpbr$tempo_uti))
  
  tb02_tot_diag <- simpbr %>% filter(is.na(diag_med_final) == FALSE) %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")   #Diagnóstico Médico final
  table(is.na(simpbr$diag_med_final))
  tb02_tot_parcial <- bind_rows(tb02_tot_imunog, tb02_tot_cortc, tb02_tot_antic, tb02_tot_antiv, tb02_tot_tto_outros, tb02_tot_uti, tb02_tot_diag, tb02_tot_tempoint, tb02_tot_tempouti)
  rm(tb02_tot_imunog, tb02_tot_cortc, tb02_tot_antic, tb02_tot_antiv, tb02_tot_tto_outros, tb02_tot_uti, tb02_tot_diag, tb02_tot_tempoint, tb02_tot_tempouti)

####____#### TABELA 3 - RESULTADOS DE EXAMES ####
# 01 - Cria objeto temporário com  tabulação das variáveis de exames segundo desfecho fatal (estratificada) ####
tb03 <- data.frame(var = "Critério de confirmação para SARS-Cov2", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA, check.names = F)  %>% 
  add_row(var = "Laboratorial", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr  %>% count(var = covidlab, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Molecular", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr  %>% filter(covidlab == "Sim") %>% count(var = result_pcr, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Sorológico", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr  %>% filter(covidlab == "Sim") %>% count(var = sorologico, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Clínico-epidemiológico", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% count(var = ctt_caso_suspeito, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  
  add_row(var = "Ecocardiograma", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(var = "Disfunção miocárdica", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(imagem___ec == "Checked") %>% count(var = ecocardio___1, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Pericardite", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(imagem___ec == "Checked") %>% count(var = ecocardio___2, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Valvulite", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(imagem___ec == "Checked") %>% count(var = ecocardio___3, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Anormalidades coronárias", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(imagem___ec == "Checked") %>% count(var = ecocardio___4, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  
  add_row(var = "Ultrasson de abdome", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(var = "Hepatomegalia", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(imagem___ua == "Checked") %>% count(var = ultrassonabd___1, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Eplenomegalia", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(imagem___ua == "Checked") %>% count(var = ultrassonabd___2, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Colite", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(imagem___ua == "Checked") %>% count(var = ultrassonabd___3, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Ileíte", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(imagem___ua == "Checked") %>% count(var = ultrassonabd___4, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Linfadenite", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(imagem___ua == "Checked") %>% count(var = ultrassonabd___5, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Ascite", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(imagem___ua == "Checked") %>% count(var = ultrassonabd___6, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  
  add_row(var = "RX de torax ou tomografia", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(var = "Infiltrado", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(imagem___rt == "Checked" | imagem___tt == "Checked") %>% count(var = imagem_resultado___1, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Derrame Pleural", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(imagem___rt == "Checked" | imagem___tt == "Checked") %>% count(var = imagem_resultado___2, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Imagem de vidro fosco", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(imagem___rt == "Checked" | imagem___tt == "Checked") %>% count(var = imagem_resultado___3, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Condensação", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(imagem___rt == "Checked" | imagem___tt == "Checked") %>% count(var = imagem_resultado___4, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  
  add_row(var = "Eletrocardiograma", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  
  add_row(var = "Marcadores de inflamação, coagulopatia ou disfunção orgânica", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(var = "Proteína C reativa", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(protecr=="Alterado" | protecr=="Normal") %>% count(var = protecr, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Dímero-D", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(d_dimero=="Alterado" | d_dimero=="Normal") %>% count(var = d_dimero, evolucao3) %>%  group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>% 
  add_row(var = "Hemoglobina", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>% 
  add_row(simpbr %>% filter(hemoglobina=="Alterado" | hemoglobina=="Normal") %>% count(var = hemoglobina, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Leucócitos totais", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(leucocitos=="Alterado" | leucocitos=="Normal") %>% count(var = leucocitos, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "VHS", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(vhs=="Alterado" | vhs=="Normal") %>% count(var = vhs, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Plaquetas", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(plaqt=="Alterado" | plaqt=="Normal") %>% count(var = plaqt, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Hematócritos", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(hematocrito=="Alterado" | hematocrito=="Normal") %>% count(var = hematocrito, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Neutrófilos", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(neutrof=="Alterado" | neutrof=="Normal") %>% count(var = neutrof, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Linfócitos", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(linfoct=="Alterado" | linfoct=="Normal") %>% count(var = linfoct, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Ferritina", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(ferritina=="Alterado" | ferritina=="Normal") %>% count(var = ferritina, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "TGO", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(tgo=="Alterado" | tgo=="Normal") %>% count(var = tgo, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "TGP", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(tgp=="Alterado" | tgp=="Normal") %>% count(var = tgp, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Albumina", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(albumina=="Alterado" | albumina=="Normal") %>% count(var = albumina, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "DHL", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(dhl=="Alterado" | dhl=="Normal") %>% count(var = dhl, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "TP", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(tp=="Alterado" | tp=="Normal") %>% count(var = tp, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "TTPTa", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(ttpta=="Alterado" | ttpta=="Normal") %>% count(var = ttpta, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Troponina", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(tropon=="Alterado" | tropon=="Normal") %>% count(var = tropon, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Triglicérides", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(triglic=="Alterado" | triglic=="Normal") %>% count(var = triglic, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Fibrinogênio", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(fibring=="Alterado" | fibring=="Normal") %>% count(var = fibring, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Ureia", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(ureia=="Alterado" | ureia=="Normal") %>% count(var = ureia, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Creatinina", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(creat=="Alterado" | creat=="Normal") %>% count(var = creat, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "NT Pro-BNP", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(ntprobnp=="Alterado" | ntprobnp=="Normal") %>% count(var = ntprobnp, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Sódio", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(sodio=="Alterado" | sodio=="Normal") %>% count(var = sodio, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Lactato", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(lactato=="Alterado" | lactato=="Normal") %>% count(var = lactato, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Potássio", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(potass=="Alterado" | potass=="Normal") %>% count(var = potass, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "CKMB", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(ckmb=="Alterado" | ckmb=="Normal") %>% count(var = ckmb, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "BNP", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(bnp =="Alterado" | bnp =="Normal") %>% count(var = bnp, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "Procalcitonina", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(procalct=="Alterado" | procalct=="Normal") %>% count(var = procalct, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result")) %>%
  add_row(var = "IL-6", `1 - Óbito` = NA, `2 - Alta` = NA, `NA`=NA) %>%
  add_row(simpbr %>% filter(il6=="Alterado" | il6=="Normal") %>% count(var = il6, evolucao3) %>% group_by(evolucao3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(result = paste0(n, " (", prop, ")")) %>% 
            ungroup() %>% dplyr::select(-n, -prop) %>% pivot_wider(names_from ="evolucao3", values_from ="result"))

# 02 - Cria objeto temporário com  tabulação das variáveis clínicas (total) ####
tb03tot <- data.frame(var = "Critério de confirmação para SARS-Cov2", `Total`=NA, `<NA>`=NA, check.names = F)  %>% 
  add_row(var = "Laboratorial", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = covidlab) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%  
  add_row(var = "Molecular", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(covidlab == "Sim") %>% count(var = result_pcr) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%  
  add_row(var = "Sorológico", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(covidlab == "Sim") %>% count(var = sorologico) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%  
  add_row(var = "Clínico-epidemiológico", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% count(var = ctt_caso_suspeito) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%  
  
  add_row(var = "Ecocardiograma", `Total`=NA, `<NA>`=NA) %>%
  add_row(var = "Disfunção miocárdica", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(imagem___ec=="Checked") %>% count(var = ecocardio___1) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%  
  add_row(var = "Pericardite", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(imagem___ec=="Checked") %>% count(var = ecocardio___2) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%
  add_row(var = "Valvulite", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(imagem___ec=="Checked") %>% count(var = ecocardio___3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%
  add_row(var = "Anormalidades coronarias", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(imagem___ec=="Checked") %>% count(var = ecocardio___4) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%
  
  add_row(var = "Ultrasson de abdome", `Total`=NA, `<NA>`=NA) %>%
  add_row(var = "Hepatomegalia", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(imagem___ua=="Checked") %>% count(var = ultrassonabd___1) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%
  add_row(var = "Esplenomegalia", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(imagem___ua=="Checked") %>% count(var = ultrassonabd___2) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%
  add_row(var = "Colite", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(imagem___ua=="Checked") %>% count(var = ultrassonabd___3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%
  add_row(var = "Ileíte", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(imagem___ua=="Checked") %>% count(var = ultrassonabd___4) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%
  add_row(var = "Linfadenite", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(imagem___ua=="Checked") %>% count(var = ultrassonabd___5) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%
  add_row(var = "Ascite", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(imagem___ua=="Checked") %>% count(var = ultrassonabd___6) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%
  
  add_row(var = "RX de torax ou tomografia", `Total`=NA, `<NA>`=NA) %>%
  add_row(var = "Infiltrado", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(imagem___rt=="Checked" | imagem___tt=="Checked" ) %>% count(var = imagem_resultado___1) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%  
  add_row(var = "Derrame Pleural", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(imagem___rt=="Checked" | imagem___tt=="Checked" ) %>% count(var = imagem_resultado___2) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%  
  add_row(var = "Imagem de vidro fosco", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(imagem___rt=="Checked" | imagem___tt=="Checked" ) %>% count(var = imagem_resultado___3) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%  
  add_row(var = "Condensação", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(imagem___rt=="Checked" | imagem___tt=="Checked" ) %>% count(var = imagem_resultado___4) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%  
  
  add_row(var = "Eletrocardiograma", `Total`=NA, `<NA>`=NA) %>%
  add_row(var = "Marcadores de inflamação, coagulopatia ou disfunção orgânica", `Total`=NA, `<NA>`=NA) %>%
  add_row(var = "Proteína C reativa", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(protecr=="Alterado" | protecr=="Normal") %>% count(var = protecr) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>%  
  add_row(var = "Dímero-D", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(d_dimero=="Alterado" | d_dimero=="Normal") %>% count(var =  d_dimero) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Hemoglobina", `Total`=NA, `<NA>`=NA) %>%  
  add_row(simpbr %>% filter(hemoglobina=="Alterado" | hemoglobina=="Normal") %>% count(var = hemoglobina) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Leucócitos totais", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(leucocitos=="Alterado" | leucocitos=="Normal") %>% count(var = leucocitos) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "VHS", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(vhs=="Alterado" | vhs=="Normal") %>% count(var = vhs) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Plaquetas", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(plaqt=="Alterado" | plaqt=="Normal") %>% count(var = plaqt) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Hematócritos", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(hematocrito=="Alterado" | hematocrito=="Normal") %>% count(var = hematocrito) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Neutrófilos", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(neutrof=="Alterado" | neutrof=="Normal") %>% count(var = neutrof) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Linfócitos", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(linfoct=="Alterado" | linfoct=="Normal") %>% count(var = linfoct) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Ferritina", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(ferritina=="Alterado" | ferritina=="Normal") %>% count(var = ferritina) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "TGO", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(tgo=="Alterado" | tgo=="Normal") %>% count(var = tgo ) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "TGP", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(tgp=="Alterado" | tgp=="Normal") %>% count(var = tgp) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Albumina", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(albumina=="Alterado" | albumina=="Normal") %>% count(var = albumina) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "DHL", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(dhl=="Alterado" | dhl=="Normal") %>% count(var = dhl) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "TP", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(tp=="Alterado" | tp=="Normal") %>% count(var = tp) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "TTPTa", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(ttpta=="Alterado" | ttpta=="Normal") %>% count(var = ttpta) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Troponina", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(tropon=="Alterado" | tropon=="Normal") %>% count(var = tropon) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Triglicérides", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(triglic=="Alterado" | triglic=="Normal") %>% count(var = triglic) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Fibrinogênio", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(fibring=="Alterado" | fibring=="Normal") %>% count(var = fibring) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Ureia", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(ureia=="Alterado" | ureia=="Normal") %>% count(var = ureia) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Creatinina", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(creat=="Alterado" | creat=="Normal") %>% count(var = creat) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "NT Pro-BNP", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(ntprobnp=="Alterado" | ntprobnp=="Normal") %>% count(var = ntprobnp ) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Sódio", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(sodio=="Alterado" | sodio=="Normal") %>% count(var = sodio) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Lactato", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(lactato=="Alterado" | lactato=="Normal") %>% count(var = lactato) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Potássio", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(potass=="Alterado" | potass=="Normal") %>% count(var = potass) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>%  ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "CKMB", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(ckmb=="Alterado" | ckmb =="Normal") %>% count(var = ckmb ) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "BNP", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(bnp=="Alterado" | bnp=="Normal") %>% count(var = bnp) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "Procalcitonina", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(procalct=="Alterado" | procalct=="Normal") %>% count(var = procalct) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) %>% 
  add_row(var = "IL-6", `Total`=NA, `<NA>`=NA) %>%
  add_row(simpbr %>% filter(il6=="Alterado" | il6=="Normal") %>% count(var = il6) %>% mutate(prop = round((n / sum(n))*100, 1)) %>% mutate(Total = paste0(n, " (", prop, ")")) %>% ungroup() %>% dplyr::select(-n, -prop)) 


# 03 - Cria tabela final e exclui objetos temporários ####
  ## Junta tabela estratificada por desfecho fatal com tabela de total
  tb3 <- bind_cols(tb03tot, tb03) %>% dplyr::select(-var1,-"<NA>",-"NA")
  ## Exclui objetos temporários 
  rm(tb03, tb03tot)
# 04 - Tabulações de apoio à análise de dados ####
  ## Calcula o total de registros com informação disponível para algumas variáveis em que foram excluídos os ignorados 
  tb03_tot_eco <- simpbr %>% filter(imagem___ec=="Checked") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_eco2 <- simpbr %>% filter(imagem___ec=="Checked") %>% count(imagem___ec=="Checked")
  tb03_tot_ultrassnabd <- simpbr %>% filter(imagem___ua=="Checked") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_ultrassnabd2 <- simpbr %>% filter(imagem___ua=="Checked") %>% count(imagem___ua=="Checked") 
  tb03_tot_rttt <- simpbr %>% filter(imagem___tt=="Checked" | imagem___rt=="Checked") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_rttt2 <- simpbr %>% filter(imagem___tt=="Checked" | imagem___rt=="Checked") %>% count(imagem___tt=="Checked" | imagem___rt=="Checked")
  tb03_tot_pcr <- simpbr %>% filter(protecr=="Alterado" | protecr=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_pcr2 <- simpbr %>% filter(protecr=="Alterado" | protecr=="Normal") %>% count(protecr=="Alterado" | protecr=="Normal")
  tb03_tot_d_dimero <- simpbr %>% filter(d_dimero=="Alterado" | d_dimero=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_d_dimero2 <- simpbr %>% filter(d_dimero=="Alterado" | d_dimero=="Normal") %>% count(d_dimero=="Alterado" | d_dimero=="Normal")
  tb03_tot_hemoglobina <- simpbr %>% filter(hemoglobina=="Alterado" | hemoglobina=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_hemoglobina2 <- simpbr %>% filter(hemoglobina=="Alterado" | hemoglobina=="Normal") %>% count(hemoglobina=="Alterado" | hemoglobina=="Normal")
  tb03_tot_leucocitos <- simpbr %>% filter(leucocitos=="Alterado" | leucocitos=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_leucocitos2 <- simpbr %>% filter(leucocitos=="Alterado" | leucocitos=="Normal") %>% count(leucocitos=="Alterado" | leucocitos=="Normal") 
  tb03_tot_vhs <- simpbr %>% filter(vhs=="Alterado" | vhs=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_vhs2 <- simpbr %>% filter(vhs=="Alterado" | vhs=="Normal") %>% count(vhs=="Alterado" | vhs=="Normal")
  tb03_tot_plaqt <- simpbr %>% filter(plaqt=="Alterado" | plaqt=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_plaqt2 <- simpbr %>% filter(plaqt=="Alterado" | plaqt=="Normal") %>% count(plaqt=="Alterado" | plaqt=="Normal") 
  tb03_tot_hematocrito <- simpbr %>% filter(hematocrito=="Alterado" | hematocrito=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_hematocrito2 <- simpbr %>% filter(hematocrito=="Alterado" | hematocrito=="Normal") %>% count(hematocrito=="Alterado" | hematocrito=="Normal")
  tb03_tot_neutrof <- simpbr %>% filter(neutrof=="Alterado" | neutrof=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_neutrof2 <- simpbr %>% filter(neutrof=="Alterado" | neutrof=="Normal") %>% count(neutrof=="Alterado" | neutrof=="Normal") 
  tb03_tot_linfoct <- simpbr %>% filter(linfoct=="Alterado" | linfoct=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_linfoct2 <- simpbr %>% filter(linfoct=="Alterado" | linfoct=="Normal") %>% count(linfoct=="Alterado" | linfoct=="Normal")
  tb03_tot_ferritina <- simpbr %>% filter(ferritina=="Alterado" | ferritina=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_ferritina2 <- simpbr %>% filter(ferritina=="Alterado" | ferritina=="Normal") %>% count(ferritina=="Alterado" | ferritina=="Normal")
  tb03_tot_tgo <- simpbr %>% filter(tgo=="Alterado" | tgo=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_tgo2 <- simpbr %>% filter(tgo=="Alterado" | tgo=="Normal") %>% count(tgo=="Alterado" | tgo=="Normal") 
  tb03_tot_tgp <- simpbr %>% filter(tgp=="Alterado" | tgp=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_tgp2 <- simpbr %>% filter(tgp=="Alterado" | tgp=="Normal") %>% count(tgp=="Alterado" | tgp=="Normal")
  tb03_tot_albumina <- simpbr %>% filter(albumina=="Alterado" | albumina=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_albumina2 <- simpbr %>% filter(albumina=="Alterado" | albumina=="Normal") %>% count(albumina=="Alterado" | albumina=="Normal")
  tb03_tot_dhl <- simpbr %>% filter(dhl=="Alterado" | dhl=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_dhl2 <- simpbr %>% filter(dhl=="Alterado" | dhl=="Normal") %>% count(dhl=="Alterado" | dhl=="Normal")
  tb03_tot_tp <- simpbr %>% filter(tp=="Alterado" | tp=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_tp2 <- simpbr %>% filter(tp=="Alterado" | tp=="Normal") %>% count(tp=="Alterado" | tp=="Normal") 
  tb03_tot_ttpta <- simpbr %>% filter(ttpta=="Alterado" | ttpta=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_ttpta2 <- simpbr %>% filter(ttpta=="Alterado" | ttpta=="Normal") %>% count(ttpta=="Alterado" | ttpta=="Normal")
  tb03_tot_tropon <- simpbr %>% filter(tropon=="Alterado" | tropon=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_tropon2 <- simpbr %>% filter(tropon=="Alterado" | tropon=="Normal") %>% count(tropon=="Alterado" | tropon=="Normal")
  tb03_tot_triglic <- simpbr %>% filter(triglic=="Alterado" | triglic=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_triglic2 <- simpbr %>% filter(triglic=="Alterado" | triglic=="Normal") %>% count(triglic=="Alterado" | triglic=="Normal")
  tb03_tot_fibring <- simpbr %>% filter(fibring=="Alterado" | fibring=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_fibring2 <- simpbr %>% filter(fibring=="Alterado" | fibring=="Normal") %>% count(fibring=="Alterado" | fibring=="Normal")
  tb03_tot_ureia <- simpbr %>% filter(ureia=="Alterado" | ureia=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_ureia2 <- simpbr %>% filter(ureia=="Alterado" | ureia=="Normal") %>% count(ureia=="Alterado" | ureia=="Normal")
  tb03_tot_creat <- simpbr %>% filter(creat=="Alterado" | creat=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_creat2 <- simpbr %>% filter(creat=="Alterado" | creat=="Normal") %>% count(creat=="Alterado" | creat=="Normal")
  tb03_tot_ntprobnp <- simpbr %>% filter(ntprobnp=="Alterado" | ntprobnp=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_ntprobnp2 <- simpbr %>% filter(ntprobnp=="Alterado" | ntprobnp=="Normal") %>% count(ntprobnp=="Alterado" | ntprobnp=="Normal")
  tb03_tot_sodio <- simpbr %>% filter(sodio=="Alterado" | sodio=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_sodio2 <- simpbr %>% filter(sodio=="Alterado" | sodio=="Normal") %>% count(sodio=="Alterado" | sodio=="Normal") 
  tb03_tot_lactato <- simpbr %>% filter(lactato=="Alterado" | lactato=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_lactato2 <- simpbr %>% filter(lactato=="Alterado" | lactato=="Normal") %>% count(lactato=="Alterado" | lactato=="Normal")
  tb03_tot_potass <- simpbr %>% filter(potass=="Alterado" | potass=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_potass2 <- simpbr %>% filter(potass=="Alterado" | potass=="Normal") %>% count(potass=="Alterado" | potass=="Normal")
  tb03_tot_ckmb <- simpbr %>% filter(ckmb=="Alterado" | ckmb=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_ckmb2 <- simpbr %>% filter(ckmb=="Alterado" | ckmb=="Normal") %>% count(ckmb=="Alterado" | ckmb=="Normal")
  tb03_tot_bnp <- simpbr %>% filter(bnp=="Alterado" | bnp=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_bnp2 <- simpbr %>% filter(bnp=="Alterado" | bnp=="Normal") %>% count(bnp=="Alterado" | bnp=="Normal") 
  tb03_tot_procalct <- simpbr %>% filter(procalct=="Alterado" | procalct=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_procalct2 <- simpbr %>% filter(procalct=="Alterado" | procalct=="Normal") %>% count(procalct=="Alterado" | procalct=="Normal")
  tb03_tot_il6 <- simpbr %>% filter(il6=="Alterado" | il6=="Normal") %>% count(evolucao3) %>% spread(key ="evolucao3", value="n")
  tb03_tot_il62 <- simpbr %>% filter(il6=="Alterado" | il6=="Normal") %>% count(il6=="Alterado" | il6=="Normal")
  
  ## Junta 
  tb03_tot_parcial <- bind_rows(tb03_tot_eco2, tb03_tot_eco, tb03_tot_ultrassnabd2, tb03_tot_ultrassnabd, tb03_tot_rttt2, tb03_tot_rttt,
                                tb03_tot_pcr2, tb03_tot_pcr, tb03_tot_d_dimero2, tb03_tot_d_dimero, tb03_tot_hemoglobina2, tb03_tot_hemoglobina,
                                tb03_tot_leucocitos2, tb03_tot_leucocitos, tb03_tot_vhs2, tb03_tot_vhs, tb03_tot_plaqt2, tb03_tot_plaqt,
                                tb03_tot_hematocrito2, tb03_tot_hematocrito, tb03_tot_neutrof2, tb03_tot_neutrof, tb03_tot_linfoct2, tb03_tot_linfoct,
                                tb03_tot_ferritina2, tb03_tot_ferritina, tb03_tot_tgo2, tb03_tot_tgo, tb03_tot_tgp2, tb03_tot_tgp,
                                tb03_tot_albumina2, tb03_tot_albumina, tb03_tot_dhl2, tb03_tot_dhl, tb03_tot_tp2, tb03_tot_tp, tb03_tot_ttpta2, 
                                tb03_tot_ttpta, tb03_tot_tropon2, tb03_tot_tropon, tb03_tot_triglic2, tb03_tot_triglic, 
                                tb03_tot_ureia2, tb03_tot_ureia, tb03_tot_creat2, tb03_tot_creat, tb03_tot_ntprobnp2, tb03_tot_ntprobnp, 
                                tb03_tot_sodio2, tb03_tot_sodio, tb03_tot_lactato2, tb03_tot_lactato, tb03_tot_potass2, tb03_tot_potass,
                                tb03_tot_ckmb2, tb03_tot_ckmb, tb03_tot_bnp2, tb03_tot_bnp, tb03_tot_procalct2, tb03_tot_procalct, tb03_tot_il62, tb03_tot_il6)
  ## Remove
  rm(tb03_tot_eco, tb03_tot_ultrassnabd, tb03_tot_rttt, tb03_tot_pcr, tb03_tot_d_dimero, tb03_tot_hemoglobina, tb03_tot_leucocitos, 
     tb03_tot_vhs, tb03_tot_plaqt, tb03_tot_hematocrito, tb03_tot_neutrof, tb03_tot_linfoct, tb03_tot_ferritina, tb03_tot_tgo,
     tb03_tot_tgp, tb03_tot_albumina, tb03_tot_dhl, tb03_tot_tp, tb03_tot_ttpta, tb03_tot_tropon, tb03_tot_triglic,
     tb03_tot_ureia, tb03_tot_creat, tb03_tot_ntprobnp, tb03_tot_sodio, tb03_tot_lactato, tb03_tot_potass,
     tb03_tot_ckmb, tb03_tot_bnp, tb03_tot_procalct, tb03_tot_il6)


####____##### HISTOGRAMA ####
  ## Estrutura geral dos bancos
  glimpse (dados_esus)
  glimpse (dados_srag)
  
# 01 - Cria tabela com casos de Covid-19 em <20 segundo data de início dos sintomas, somando casos de SRAG(SIVEP) + SG(eSUS)  ####

dados_esus <- dados_esus %>% mutate(SE=epiweek(dis))
dados_srag <- dados_srag %>% 
    filter(DT_SIN_PRI>as.POSIXct("2020-02-25") & DT_SIN_PRI<as.POSIXct("2020-12-06")) %>% 
    filter(NU_IDADE_N>=0 & NU_IDADE_N <20) 
  
tb_covid_ped <- bind_cols(
    dados_srag %>% count(var=SEM_PRI),  
    dados_esus  %>% count(var=SE))  %>% 
  mutate(covidped=n+n1) %>% 
    dplyr::select (-var1, -n, -n1)
  
tb_covid_ped_dia <- left_join(
  dados_esus  %>% count(dis),  
  dados_srag %>% count(DT_SIN_PRI), 
  by = c("dis" = "DT_SIN_PRI")) %>%
    rename("sg"="n.x", "srag"="n.y") %>% 
    mutate(covidped = rowSums(.[2:3], na.rm = TRUE))
  
  ## Exclui bases de SRAG(SIVEP) + SG(eSUS)
  rm(dados_esus, dados_srag)
  
# 02 - Cria tabelas com casos de SIM-P segundo data de início dos sintomas (total e estratificada segundo desfecho fatal) ####
tbhsimp_dia <- simpbr %>% count(dt_inicio_ss)  # total
tbhobsimp_dia <- simpbr %>% filter (evolucao=="Óbito") %>% count(dt_inicio_ss) #estratificada 

tbhsimp_SE <- simpbr %>% count(SE) # total
tbhobsimp_SE <- simpbr %>% filter (evolucao=="Óbito") %>% count(SE) #estratificada 

# 03 -  Junta tabelas casos de SIM-P, Óbitos,  descartados sintomáticos e Covid-19 em <20 segundo data de início dos sintomas ####
  
tbh_dia <- tb_covid_ped_dia %>% 
  left_join(tbhsimp_dia, by = c("dis" = "dt_inicio_ss")) %>% 
  left_join(tbhobsimp_dia, by = c("dis" = "dt_inicio_ss")) %>% 
  rename(simp=n.x, obitos_simp=n.y)
  
  ## Exclui objetos temporários
  rm(tbh_desc, tbhsimp_dia, tb_covid_ped_dia, tbhobsimp_dia)

tbh_SE <- tb_covid_ped %>% 
    left_join(tbhsimp_SE, by = c("var" = "SE")) %>% 
    left_join(tbhobsimp_SE, by = c("var" = "SE")) %>% 
    rename(simp=n.x, obitos_simp=n.y)
  
  ## Exclui objetos temporários
  rm(tbhsimp_SE, tb_covid_ped, tbhobsimp_SE)

####____##### MAPAS ####
# 01 - Cria tabelas com casos de SIM-P  e Óbitos segundo UF de notificação ####
tbm <- simpbr %>% count(ufnot)
tbmob <- simpbr %>% filter (evolucao=="Óbito") %>% count(ufnot)


################################## ________ EXPORTAÇÃO _________ ################################## 
# 01 - Tabelas de conferência ####
  ## Cria espaço de trabalho com uma aba para cada tabela
  wb = createWorkbook()
  addWorksheet(wb, sheetName = "ss")
  addWorksheet(wb, sheetName = "complicacoes")
  addWorksheet(wb, sheetName = "comorbidades")
  addWorksheet(wb, sheetName = "ids confirmados")
  addWorksheet(wb, sheetName = "duplicidades")
  addWorksheet(wb, sheetName = "tratamentos")
  addWorksheet(wb, sheetName = "ultrasson abd")
  addWorksheet(wb, sheetName = "radio torax e tomografia")
  addWorksheet(wb, sheetName = "eco")
  addWorksheet(wb, sheetName = "eletro")
  
  ## Indica qual objeto (tabela) será incluído em cada aba 
  writeData(wb, sheet = "ss", x = outros_ss) 
  writeData(wb, sheet = "complicacoes", x = outra_complicacao) 
  writeData(wb, sheet = "comorbidades", x = outras_comorb) 
  writeData(wb, sheet = "ids confirmados", x = tbid) 
  writeData(wb, sheet = "duplicidades", x = tbDuplicatas) 
  writeData(wb, sheet = "tratamentos", x = outros_ttos) 
  writeData(wb, sheet = "ultrasson abd", x = outros_ultrasonnabd) 
  writeData(wb, sheet = "radio torax e tomografia", x = outros_rt_tomo) 
  writeData(wb, sheet = "eco", x = outros_eco) 
  writeData(wb, sheet = "eletro", x = outros_eletro) 

  ## Salva arquivo em Excell
  saveWorkbook(wb, "C:/Users/laisr/OneDrive/Documentos/SIMP/ARTIGO/conferencias.xlsx", overwrite = T)
  
  ## Remove 
  rm(outros_ss, outra_complicacao, outras_comorb, outros_ttos, outros_eco, outros_eletro, outros_ultrasonnabd, outros_rt_tomo, tbid, tbDuplicatas, wb)

# 02 - Tabelas do artigo ####
  ## Cria espaço de trabalho com uma aba para cada tabela
  wb = createWorkbook()
  addWorksheet(wb, sheetName = "tb1")
  addWorksheet(wb, sheetName = "tb2")
  addWorksheet(wb, sheetName = "tb3")
  addWorksheet(wb, sheetName = "tbh_dia")
  addWorksheet(wb, sheetName = "tbh_SE")
  addWorksheet(wb, sheetName = "tbm")
  addWorksheet(wb, sheetName = "tbmob")
  
  ## Indica qual objeto (tabela) será incluído em cada aba 
  writeData(wb, sheet = "tb1", x = tb1) 
  writeData(wb, sheet = "tb2", x = tb2) 
  writeData(wb, sheet = "tb3", x = tb3) 
  writeData(wb, sheet = "tbh_dia", x = tbh_dia)
  writeData(wb, sheet = "tbh_SE", x = tbh_SE)
  writeData(wb, sheet = "tbm", x = tbm) 
  writeData(wb, sheet = "tbmob", x = tbmob) 
  
  ## Salva arquivo em Excell
  saveWorkbook(wb, "C:/Users/laisr/OneDrive/Documentos/SIMP/ARTIGO/simpbr_paper2020_tabelas.xlsx", overwrite = T)
  
  ## Remove 
  rm(tb1, tb2, tb3, tbh_dia, tbh_SE, tbm, tbmob, wb)



################################## ____ REGRESSÃO LOGÍSTICA _____#### 
  #### Desfecho analisado
  table(simpbr$evolucao3)
  
####____##### DEFINIR CATEGORIAS DE REFERÊNCIA  #### 
  simpbr <- simpbr %>% mutate_at(c("protecr", "d_dimero", "bnp", "ckmb", "comorb", "creat", "fibring", "albumina", "ferritina", 
                                   "hematocrito", "hemoglobina", "il6", "imunog", "lactato", "leucocitos", "linfoct", "neutrof", "ntprobnp", 
                                   "plaqt", "potass", "procalct", "sodio", "tgo", "tgp", "tp", "triglic", "tropon", "ttpta", "ureia", "vhs"), 
                                 ~forcats::fct_relevel(., "Normal", "Alterado"))
  
  simpbr <- simpbr %>% mutate_at(c("ecocardio___1", "ecocardio___2", "ecocardio___3", "ecocardio___4", "imagem_resultado___1", "imagem_resultado___2",
                                   "imagem_resultado___3", "imagem_resultado___4", "ultrassonabd___1", "ultrassonabd___2", "ultrassonabd___3", 
                                   "ultrassonabd___4", "ultrassonabd___5", "ultrassonabd___6", "troca_plasmatica", "ventilacao_nao_invasiva", 
                                   "ventilacao_invasiva", "insuficienncai_renal", "infarto", "falencia_outros_orgaos", "evento_trombo", 
                                   "edema_agudo_pulmonar", "convulsoes", "sepse", "pneumonia", "hipotens_drogas", "hipertensao", "mialgia", "oliguria", 
                                   "linfadenopatia", "edema_maospes", "taquicardia", "hipotensao_choque_defcaso", "confusao_mental", "letargia", 
                                   "irritabilidade", "cefaleia", "dor_garganta", "coriza", "tosse", "saturacao_o2", "dispneia", "conjuntivite_defcaso", 
                                   "manchas_vermelhas", "alteracoes_pele", "dor_abdm", "diarreia", "nausea_vomito", "gastrointestinais", 
                                   "outrascomorbidades", "imunossuprimido", "sindgenetica", "doencahematologica", "cardiopatia", "respre", 
                                   "doencaneurologica", "edema_maospes","linfadenopatia"), 
                                 ~forcats::fct_relevel(., "Unchecked", "Checked"))
  
  simpbr <- simpbr %>% mutate_at(c("neurologicos", "endocrinopre", "dermatocutaneo", "respiratorios", "neurologicos", "circulatorio", "edema", 
                                   "geniturinario", "muscolosqueleticos", "outros", "respre", "gastrointestinais"), 
                                 ~forcats::fct_relevel(., "Não Checked", "Checked"))
  
  simpbr <- simpbr %>% mutate_at(c("imunog", "cortic", "anticoag", "antiv", "tto_outros", "complicacoes", "antib", "complic_renal", "complic_resp", 
                                   "complic_ci", "comorb", "intern_uti", "covidlab", "sorologico", "molecular"), 
                                 ~forcats::fct_relevel(., "Não", "Sim"))
  class(simpbr$evolucao3)
  simpbr$evolucao3 <- as.factor(simpbr$evolucao3)
  simpbr$evolucao3 <- relevel(simpbr$evolucao3, ref="2 - Alta")
  
  simpbr$racacorbnb <- as.factor(simpbr$racacorbnb)
  simpbr$racacorbnb <- relevel(simpbr$racacorbnb, ref="2 - Branca")
  
  simpbr$sexo <- as.factor(simpbr$sexo)
  
####____##### REGRESSÕES SIMPLES  #### 
  logistica_sexo <- glm (evolucao3 ~ sexo, data=simpbr, family=binomial(link='logit'))
  logistica_raca <- glm (evolucao3 ~ racacorbnb, data=simpbr, family=binomial(link='logit')) 
  logistica_comorb <- glm(evolucao3 ~ comorb, data=simpbr, family=binomial(link="logit"))
  logistica_doencaneurologica <- glm(evolucao3 ~ doencaneurologica, data=simpbr, family=binomial(link="logit"))
  logistica_respre <- glm(evolucao3 ~ respre, data=simpbr, family=binomial(link="logit"))
  logistica_cardiopatia <- glm(evolucao3 ~ cardiopatia, data=simpbr, family=binomial(link="logit"))
  logistica_outrascomorbidades <- glm(evolucao3 ~ outrascomorbidades, data=simpbr, family=binomial(link="logit"))
  logistica_faixet <- glm(evolucao3 ~ faixet, data=simpbr, family=binomial(link="logit"))
  logistica_gastrointestinais <- glm(evolucao3 ~ gastrointestinais, data=simpbr, family=binomial(link="logit"))
  logistica_nausea_vomito <- glm(evolucao3 ~ nausea_vomito, data=simpbr, family=binomial(link="logit"))
  logistica_diarreia <- glm(evolucao3 ~ diarreia, data=simpbr, family=binomial(link="logit"))
  logistica_dor_abdm <- glm(evolucao3 ~ dor_abdm, data=simpbr, family=binomial(link="logit"))
  logistica_dermatocutaneo <- glm(evolucao3 ~ dermatocutaneo, data=simpbr, family=binomial(link="logit"))
  logistica_alteracoes_pele <- glm(evolucao3 ~ alteracoes_pele, data=simpbr, family=binomial(link="logit"))
  logistica_manchas_vermelhas <- glm(evolucao3 ~ manchas_vermelhas, data=simpbr, family=binomial(link="logit"))
  logistica_conjuntivite_defcaso <- glm(evolucao3 ~ conjuntivite_defcaso, data=simpbr, family=binomial(link="logit"))
  logistica_respiratorios <- glm(evolucao3 ~ respiratorios, data=simpbr, family=binomial(link="logit"))
  logistica_dispneia <- glm(evolucao3 ~ dispneia, data=simpbr, family=binomial(link="logit"))
  logistica_saturacao_o2 <- glm(evolucao3 ~ saturacao_o2, data=simpbr, family=binomial(link="logit"))
  logistica_tosse <- glm(evolucao3 ~ tosse, data=simpbr, family=binomial(link="logit"))
  logistica_coriza <- glm(evolucao3 ~ coriza, data=simpbr, family=binomial(link="logit"))
  logistica_dor_garganta <- glm(evolucao3 ~ dor_garganta, data=simpbr, family=binomial(link="logit"))
  logistica_neurologicos <- glm(evolucao3 ~ neurologicos, data=simpbr, family=binomial(link="logit"))
  logistica_cefaleia <- glm(evolucao3 ~ cefaleia, data=simpbr, family=binomial(link="logit"))
  logistica_irritabilidade <- glm(evolucao3 ~ irritabilidade, data=simpbr, family=binomial(link="logit"))
  logistica_letargia <- glm(evolucao3 ~ letargia, data=simpbr, family=binomial(link="logit"))
  logistica_confusao_mental <- glm(evolucao3 ~ confusao_mental, data=simpbr, family=binomial(link="logit"))
  logistica_circulatorio <- glm(evolucao3 ~ circulatorio, data=simpbr, family=binomial(link="logit"))
  logistica_hipotensao_choque_defcaso <- glm(evolucao3 ~ hipotensao_choque_defcaso, data=simpbr, family=binomial(link="logit"))
  logistica_taquicardia <- glm(evolucao3 ~ taquicardia, data=simpbr, family=binomial(link="logit"))
  logistica_edema <- glm(evolucao3 ~ edema, data=simpbr, family=binomial(link="logit"))
  logistica_edema_maospes <- glm(evolucao3 ~ edema_maospes, data=simpbr, family=binomial(link="logit"))
  logistica_linfadenopatia <- glm(evolucao3 ~ linfadenopatia, data=simpbr, family=binomial(link="logit"))
  logistica_oliguria <- glm(evolucao3 ~ oliguria, data=simpbr, family=binomial(link="logit"))
  logistica_mialgia <- glm(evolucao3 ~ mialgia, data=simpbr, family=binomial(link="logit"))
  logistica_imunog <- glm(evolucao3 ~ imunog, data=simpbr, family=binomial(link="logit"))
  logistica_cortic <- glm(evolucao3 ~ cortic, data=simpbr, family=binomial(link="logit"))
  logistica_anticoag <- glm(evolucao3 ~ anticoag, data=simpbr, family=binomial(link="logit"))
  logistica_antib <- glm(evolucao3 ~ antib, data=simpbr, family=binomial(link="logit"))
  logistica_antiv <- glm(evolucao3 ~ antiv, data=simpbr, family=binomial(link="logit"))
  logistica_tto_outros <- glm(evolucao3 ~ tto_outros, data=simpbr, family=binomial(link="logit"))
  logistica_complicacoes <- glm(evolucao3 ~ complicacoes, data=simpbr, family=binomial(link="logit"))
  logistica_complic_ci <- glm(evolucao3 ~ complic_ci, data=simpbr, family=binomial(link="logit")) 
  logistica_hipotens_drogas <- glm(evolucao3 ~ hipotens_drogas, data=simpbr, family=binomial(link="logit"))
  logistica_complic_resp <- glm(evolucao3 ~ complic_resp, data=simpbr, family=binomial(link="logit"))
  logistica_pneumonia <- glm(evolucao3 ~ pneumonia, data=simpbr, family=binomial(link="logit"))
  logistica_ventilacao_invasiva <- glm(evolucao3 ~ ventilacao_invasiva, data=simpbr, family=binomial(link="logit"))
  logistica_ventilacao_nao_invasiva <- glm(evolucao3 ~ ventilacao_nao_invasiva, data=simpbr, family=binomial(link="logit"))
  logistica_sepse <- glm(evolucao3 ~ sepse, data=simpbr, family=binomial(link="logit"))
  logistica_complic_renal <- glm(evolucao3 ~ complic_renal, data=simpbr, family=binomial(link="logit"))
  logistica_insuficienncai_renal <- glm(evolucao3 ~ insuficienncai_renal, data=simpbr, family=binomial(link="logit"))
  logistica_convulsoes <- glm(evolucao3 ~ convulsoes, data=simpbr, family=binomial(link="logit"))
  logistica_falencia_outros_orgaos <- glm(evolucao3 ~ falencia_outros_orgaos, data=simpbr, family=binomial(link="logit"))
  logistica_intern_uti <- glm(evolucao3 ~ intern_uti, data=simpbr, family=binomial(link="logit"))

  logistica_covidlab <- glm(evolucao3 ~ covidlab, data=simpbr, family=binomial(link="logit"))
  logistica_molecular <- glm(evolucao3 ~ molecular, data=simpbr, family=binomial(link="logit"))
  logistica_sorologico <- glm(evolucao3 ~ sorologico, data=simpbr, family=binomial(link="logit"))
  
  logistica_imagem_resultado___1 <- glm(evolucao3 ~ imagem_resultado___1, data=simpbr, family=binomial(link="logit"))
  logistica_imagem_resultado___2 <- glm(evolucao3 ~ imagem_resultado___2, data=simpbr, family=binomial(link="logit"))
  logistica_imagem_resultado___3 <- glm(evolucao3 ~ imagem_resultado___3, data=simpbr, family=binomial(link="logit"))
  logistica_imagem_resultado___4 <- glm(evolucao3 ~ imagem_resultado___4, data=simpbr, family=binomial(link="logit"))
  logistica_ecocardio___1 <- glm(evolucao3 ~ ecocardio___1, data=simpbr, family=binomial(link="logit"))
  logistica_ecocardio___2 <- glm(evolucao3 ~ ecocardio___2, data=simpbr, family=binomial(link="logit"))
  logistica_ecocardio___3 <- glm(evolucao3 ~ ecocardio___3, data=simpbr, family=binomial(link="logit"))
  logistica_ecocardio___4 <- glm(evolucao3 ~ ecocardio___4, data=simpbr, family=binomial(link="logit"))
  logistica_ultrassonabd___1 <- glm(evolucao3 ~ ultrassonabd___1, data=simpbr, family=binomial(link="logit"))
  logistica_ultrassonabd___2 <- glm(evolucao3 ~ ultrassonabd___2, data=simpbr, family=binomial(link="logit"))
  logistica_ultrassonabd___3 <- glm(evolucao3 ~ ultrassonabd___3, data=simpbr, family=binomial(link="logit"))
  logistica_ultrassonabd___4 <- glm(evolucao3 ~ ultrassonabd___4, data=simpbr, family=binomial(link="logit"))
  logistica_ultrassonabd___5 <- glm(evolucao3 ~ ultrassonabd___5, data=simpbr, family=binomial(link="logit"))
  logistica_ultrassonabd___6 <- glm(evolucao3 ~ ultrassonabd___6, data=simpbr, family=binomial(link="logit"))
  logistica_protecr <- glm(evolucao3 ~ protecr, data=simpbr, family=binomial(link="logit"))
  logistica_d_dimero <- glm(evolucao3 ~ d_dimero, data=simpbr, family=binomial(link="logit"))
  logistica_hemoglobina <- glm(evolucao3 ~ hemoglobina, data=simpbr, family=binomial(link="logit"))
  logistica_leucocitos <- glm(evolucao3 ~ leucocitos, data=simpbr, family=binomial(link="logit"))
  logistica_vhs <- glm(evolucao3 ~ vhs, data=simpbr, family=binomial(link="logit"))
  logistica_plaqt <- glm(evolucao3 ~ plaqt, data=simpbr, family=binomial(link="logit"))
  logistica_hematocrito <- glm(evolucao3 ~ hematocrito, data=simpbr, family=binomial(link="logit"))
  logistica_neutrof <- glm(evolucao3 ~ neutrof, data=simpbr, family=binomial(link="logit"))
  logistica_linfoct <- glm(evolucao3 ~ linfoct, data=simpbr, family=binomial(link="logit"))
  logistica_ferritina <- glm(evolucao3 ~ ferritina, data=simpbr, family=binomial(link="logit"))
  logistica_tgo <- glm(evolucao3 ~ tgo, data=simpbr, family=binomial(link="logit"))
  logistica_tgp <- glm(evolucao3 ~ tgp, data=simpbr, family=binomial(link="logit"))
  logistica_albumina <- glm(evolucao3 ~ albumina, data=simpbr, family=binomial(link="logit"))
  logistica_dhl <- glm(evolucao3 ~ dhl, data=simpbr, family=binomial(link="logit"))
  logistica_tp <- glm(evolucao3 ~ tp, data=simpbr, family=binomial(link="logit"))
  logistica_ttpta <- glm(evolucao3 ~ ttpta, data=simpbr, family=binomial(link="logit"))
  logistica_tropon <- glm(evolucao3 ~ tropon, data=simpbr, family=binomial(link="logit"))
  logistica_triglic <- glm(evolucao3 ~ triglic, data=simpbr, family=binomial(link="logit"))
  logistica_fibring <- glm(evolucao3 ~ fibring, data=simpbr, family=binomial(link="logit"))
  logistica_ureia <- glm(evolucao3 ~ ureia, data=simpbr, family=binomial(link="logit"))
  logistica_creat <- glm(evolucao3 ~ creat, data=simpbr, family=binomial(link="logit"))
  logistica_ntprobnp <- glm(evolucao3 ~ ntprobnp, data=simpbr, family=binomial(link="logit"))
  logistica_sodio <- glm(evolucao3 ~ sodio, data=simpbr, family=binomial(link="logit"))
  logistica_lactato <- glm(evolucao3 ~ lactato, data=simpbr, family=binomial(link="logit"))
  logistica_potass <- glm(evolucao3 ~ potass, data=simpbr, family=binomial(link="logit"))
  logistica_ckmb <- glm(evolucao3 ~ ckmb, data=simpbr, family=binomial(link="logit"))
  logistica_bnp <- glm(evolucao3 ~ bnp, data=simpbr, family=binomial(link="logit"))
  logistica_procalct <- glm(evolucao3 ~ procalct, data=simpbr, family=binomial(link="logit"))
  logistica_il6 <- glm(evolucao3 ~ il6, data=simpbr, family=binomial(link="logit"))

####____##### RESULTADOS DAS REGRESSÕES SIMPLES  ####   
  logistic.display(logistica_idade)
  logistic.display(logistica_faixet)
  logistic.display(logistica_sexo)
  logistic.display(logistica_raca)
  logistic.display(logistica_comorb)
  logistic.display(logistica_doencaneurologica)
  logistic.display(logistica_respre)
  logistic.display(logistica_cardiopatia)
  logistic.display(logistica_gastrointestinais)
  logistic.display(logistica_nausea_vomito)
  logistic.display(logistica_diarreia)
  logistic.display(logistica_dor_abdm)
  logistic.display(logistica_dermatocutaneo)
  logistic.display(logistica_alteracoes_pele)
  logistic.display(logistica_manchas_vermelhas)
  logistic.display(logistica_conjuntivite_defcaso)
  logistic.display(logistica_respiratorios)
  logistic.display(logistica_dispneia)
  logistic.display(logistica_saturacao_o2)
  logistic.display(logistica_tosse)
  logistic.display(logistica_coriza)
  logistic.display(logistica_dor_garganta)
  logistic.display(logistica_neurologicos)
  logistic.display(logistica_letargia)
  logistic.display(logistica_cefaleia)
  logistic.display(logistica_irritabilidade)
  logistic.display(logistica_confusao_mental)
  logistic.display(logistica_circulatorio) 
  logistic.display(logistica_taquicardia)
  logistic.display(logistica_hipotensao_choque_defcaso)
  logistic.display(logistica_edema)
  logistic.display(logistica_edema_maospes)
  logistic.display(logistica_linfadenopatia)
  logistic.display(logistica_oliguria)
  logistic.display(logistica_mialgia)
  logistic.display(logistica_imunog)
  logistic.display(logistica_cortic)
  logistic.display(logistica_anticoag)
  logistic.display(logistica_antib)
  logistic.display(logistica_antiv)
  logistic.display(logistica_tto_outros)
  logistic.display(logistica_complic_ci)
  logistic.display(logistica_hipotens_drogas)
  logistic.display(logistica_complic_resp)
  logistic.display(logistica_ventilacao_invasiva)
  logistic.display(logistica_pneumonia)
  logistic.display(logistica_sepse)
  logistic.display(logistica_ventilacao_nao_invasiva)
  logistic.display(logistica_insuficienncai_renal)
  logistic.display(logistica_convulsoes)
  logistic.display(logistica_intern_uti)
  
  logistic.display(logistica_covidlab)
  logistic.display(logistica_molecular)
  logistic.display(logistica_sorologico)
  logistic.display(logistica_ecocardio___1)
  logistic.display(logistica_ecocardio___4)
  logistic.display(logistica_ecocardio___2)
  logistic.display(logistica_ultrassonabd___1)
  logistic.display(logistica_imagem_resultado___1)
  logistic.display(logistica_imagem_resultado___2)
  logistic.display(logistica_imagem_resultado___3)
  logistic.display(logistica_imagem_resultado___4)
  logistic.display(logistica_protecr)
  logistic.display(logistica_vhs)
  logistic.display(logistica_ntprobnp)
  logistic.display(logistica_ferritina)
  logistic.display(logistica_hemoglobina)
  logistic.display(logistica_bnp)
  logistic.display(logistica_triglic)
  logistic.display(logistica_dhl)
  logistic.display(logistica_hematocrito)
  logistic.display(logistica_neutrof)
  logistic.display(logistica_leucocitos)
  logistic.display(logistica_albumina)
  logistic.display(logistica_fibring)
  logistic.display(logistica_plaqt)
  logistic.display(logistica_linfoct)
  logistic.display(logistica_tgo)
  logistic.display(logistica_tgp)
  logistic.display(logistica_lactato)
  logistic.display(logistica_tp)
  logistic.display(logistica_tropon)
  logistic.display(logistica_ttpta)
  logistic.display(logistica_ckmb)
  logistic.display(logistica_ureia)
  logistic.display(logistica_creat)
  logistic.display(logistica_sodio)
  logistic.display(logistica_potass)

####____##### MODELO SINAIS E SINTOMAS  ESPECÍFICOS ####
  logistica_sinais_sintomas_parcial1 <-glm (evolucao3 ~
                                              conjuntivite_defcaso +
                                              manchas_vermelhas +
                                              alteracoes_pele +
                                              dispneia +
                                              saturacao_o2 +
                                              tosse +
                                              hipotensao_choque_defcaso + 
                                              taquicardia + 
                                              edema_maospes + 
                                              oliguria +
                                              linfadenopatia+
                                              confusao_mental+
                                              letargia,
                                            data =simpbr_SS,
                                            family = binomial(link='logit'), maxit=100)
  
  summary(logistica_sinais_sintomas_parcial1)
  logistic.display(logistica_sinais_sintomas_parcial1)
  
  logistica_sinais_sintomas_final <-glm (evolucao3 ~
                                           conjuntivite_defcaso +
                                           dispneia +
                                           saturacao_o2 +
                                           hipotensao_choque_defcaso + 
                                           linfadenopatia,
                                         data =simpbr,
                                         family = binomial(link='logit'), maxit=100)
  
  summary(logistica_sinais_sintomas_final)
  logistic.display(logistica_sinais_sintomas_final)
  
  
####____##### MODELO EXAMES - RESULTADOS DE MARCADORES LABORATORIAIS  E RX ####
  logistica_exames_parcial1 <-glm (evolucao3 ~
                                     hematocrito +
                                     neutrof + 
                                     leucocitos + 
                                     albumina + 
                                     plaqt + 
                                     linfoct +
                                     tgo +
                                     tgp + 
                                     tp + 
                                     ttpta +
                                     ureia + 
                                     creat + 
                                     sodio +
                                     potass +
                                     imagem_resultado___1 +
                                     imagem_resultado___3 +
                                     imagem_resultado___4,
                                   data =simpbr,
                                   family = binomial(link='logit'), maxit=100)
  
  logistic.display(logistica_exames_parcial1)
  
  
  logistica_exames_parcial2 <-glm (evolucao3 ~
                                     plaqt + 
                                     tgo +
                                     tp + 
                                     creat + 
                                     potass +
                                     imagem_resultado___1 +
                                     imagem_resultado___3,
                                   data =simpbr,
                                   family = binomial(link='logit'), maxit=100)
  
  logistic.display(logistica_exames_parcial2)
  
  logistica_exames_final <-glm (evolucao3 ~
                                  tgo +
                                  tp + 
                                  creat + 
                                  potass +
                                  imagem_resultado___1 +
                                  imagem_resultado___3,
                                data =simpbr,
                                family = binomial(link='logit'), maxit=100)
  
  logistic.display(logistica_exames_final)
  
####____##### MODELO TRATAMENTO ####
  logistica_tto_parcial1 <-glm (evolucao3 ~
                                  antib + 
                                  antiv + 
                                  cortic + 
                                  imunog + 
                                  anticoag,
                                data =simpbr,
                                family = binomial(link='logit'), maxit=100)
  
  summary(logistica_tto_parcial1)
  logistic.display(logistica_tto_parcial1)
  
  logistica_tto_final <-glm (evolucao3 ~
                               antiv + 
                               cortic + 
                               imunog + 
                               anticoag,
                             data =simpbr,
                             family = binomial(link='logit'), maxit=100)
  
  summary(logistica_tto_final)
  logistic.display(logistica_tto_final)
  
  
  
  
####____##### MODELO 1 - SOCIODEMOGRAFICO ####
  logistica_modelo1_parcial1 <- glm (evolucao3 ~
                                       faixet +
                                       sexo +
                                       racacorbnb,
                                     data=simpbr,
                                     family=binomial(link='logit'), maxit=100)
  
  logistic.display(logistica_modelo1_parcial1)
  
  logistica_modelo1_parcial2 <- glm (evolucao3 ~
                                       faixet,
                                     data=simpbr,
                                     family=binomial(link='logit'), maxit=100)
  
  logistic.display(logistica_modelo1_parcial2)
  
####____##### MODELO 2 - SOCIODEMOGRÁFICO + COMORBIDADES ####
  logistica_modelo2_parcial1 <-glm (evolucao3 ~
                                      faixet +
                                      comorb ,
                                    data =simpbr,
                                    family = binomial(link='logit'), maxit=100)
  
  summary(logistica_modelo2_parcial1)
  logistic.display(logistica_modelo2_parcial1)
  
  
####____##### MODELO 3 - SOCIODEMOGRAFICO + COMORBIDADES + SINAIS E SINTOMAS ESPECÍFICOS ####
  logistica_modelo3_parcial1 <-glm (evolucao3 ~
                                      faixet +
                                      comorb +
                                      conjuntivite_defcaso +
                                      dispneia +
                                      saturacao_o2 +
                                      hipotensao_choque_defcaso + 
                                      linfadenopatia,
                                    data =simpbr,
                                    family = binomial(link='logit'), maxit=100)
  
  summary(logistica_modelo3_parcial1)
  logistic.display(logistica_modelo3_parcial1)
  
  logistica_modelo3_parcial2 <-glm (evolucao3 ~
                                      faixet +
                                      comorb +
                                      conjuntivite_defcaso +
                                      dispneia +
                                      hipotensao_choque_defcaso + 
                                      linfadenopatia,
                                    data =simpbr,
                                    family = binomial(link='logit'), maxit=100)
  
  summary(logistica_modelo3_parcial2)
  logistic.display(logistica_modelo3_parcial2)
  
  logistica_modelo3_final <-glm (evolucao3 ~
                                   comorb +
                                   conjuntivite_defcaso +
                                   dispneia +
                                   hipotensao_choque_defcaso + 
                                   linfadenopatia,
                                 data =simpbr,
                                 family = binomial(link='logit'), maxit=100)
  
  summary(logistica_modelo3_final)
  logistic.display(logistica_modelo3_final)
  
####____##### MODELO 4 - SOCIODEMOGRAFICO + COMORBIDADES + SINAIS E SINTOMAS + EXAMES ####
  logistica_modelo4_parcial1 <-glm (evolucao3 ~
                                      comorb +
                                      conjuntivite_defcaso +
                                      dispneia +
                                      hipotensao_choque_defcaso + 
                                      linfadenopatia +
                                      tgo +
                                      tp + 
                                      creat + 
                                      potass +
                                      imagem_resultado___1 +
                                      imagem_resultado___3,
                                    data =simpbr,
                                    family = binomial(link='logit'), maxit=100)
  
  summary(logistica_modelo4_parcial1)
  logistic.display(logistica_modelo4_parcial1)
  
  logistica_modelo4_final <-glm (evolucao3 ~
                                   conjuntivite_defcaso +
                                   dispneia +
                                   hipotensao_choque_defcaso + 
                                   tp + 
                                   potass,
                                 data =simpbr,
                                 family = binomial(link='logit'), maxit=100)
  
  summary(logistica_modelo4_final)
  logistic.display(logistica_modelo4_final)
  
  
####____##### MODELO 5- SOCIODEMOGRAFICO + COMORBIDADES + SINAIS E SINTOMAS ESPECIFICOS + EXAMES + TRATAMENTO ####
  logistica_modelo5_parcial1 <-glm (evolucao3 ~
                                      conjuntivite_defcaso +
                                      dispneia +
                                      hipotensao_choque_defcaso + 
                                      tp + 
                                      potass +
                                      antiv + 
                                      anticoag + 
                                      cortic + 
                                      imunog,
                                    data =simpbr,
                                    family = binomial(link='logit'), maxit=100)
  
  summary(logistica_modelo5_parcial1)
  logistic.display(logistica_modelo5_parcial1)
  
  logistica_modelo5_parcial2 <-glm (evolucao3 ~
                                      conjuntivite_defcaso +
                                      dispneia +
                                      hipotensao_choque_defcaso + 
                                      tp + 
                                      potass +
                                      antiv + 
                                      anticoag,
                                    data =simpbr,
                                    family = binomial(link='logit'), maxit=100)
  
  summary(logistica_modelo5_parcial2)
  logistic.display(logistica_modelo5_parcial2)
  
  logistica_modelo5_parcial3 <-glm (evolucao3 ~
                                      conjuntivite_defcaso +
                                      dispneia +
                                      hipotensao_choque_defcaso + 
                                      tp + 
                                      potass +
                                      anticoag,
                                    data =simpbr,
                                    family = binomial(link='logit'), maxit=100)
  
  summary(logistica_modelo5_parcial3)
  logistic.display(logistica_modelo5_parcial3)
  
  logistica_modelo5_final <-glm (evolucao3 ~
                                   conjuntivite_defcaso +
                                   dispneia +
                                   hipotensao_choque_defcaso + 
                                   tp + 
                                   potass,
                                 data =simpbr,
                                 family = binomial(link='logit'), maxit=100)
  
  summary(logistica_modelo5_final)
  logistic.display(logistica_modelo5_final)  
