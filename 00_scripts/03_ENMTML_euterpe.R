install_github("andrefaa/ENMTML",force=T)  
library(ENMTML)  

ENMTML(pred_dir ='C:/Users/anaf_/Desktop/Modelagem/Euterpe/var_cortadas',
       occ_file= "C:/Users/anaf_/Desktop/Modelagem/Euterpe/precatoria.txt",
       result_dir="C:/Users/anaf_/Desktop/Modelagem/Euterpe/teste_precatoria_tss",
       proj_dir= NULL,
       sp="species",
       x="longitude",
       y="latitude",
       min_occ=5, #numero minimo de ocorrencias aceitas para fazer a modelagem
       
       thin_occ=c(method='CELLSIZE'), # definir o metodo de rarefacao dos dados
       colin_var = c(method='PCA'), # metodo de controle de colinearidade das variaveis, os resultados ficam na pasta das variaveis
       imp_var = TRUE, # calculo de importancia de variavel e curvas de resposta
       eval_occ = NULL, #colocar caminho de ocorrencia para especie invasivas
       sp_accessible_area = c(method='MASK', filepath='C:/Users/anaf_/Desktop/Modelagem/Euterpe/mascara/Lowenberg_Neto_2014.shp'), # area acessivas
       pres_abs_ratio = 1, # quantidade de pseudo-ausencia para cada presenca comprovada
       pseudoabs_method = c(method='ENV_CONST'), #metodo de pseudo ausencias
       
       part=c(method='BOOT', replicates='10', proportion='0.7'), # particao dos dados para ajuste e avaliacao dos modelos
       save_part=FALSE,save_final=TRUE, #salvar modelos 
       algorithm = c("MXS","RDF","BIO","SVM"), # algoritmos
       thr=c(type='MAX_TSS'), #Threshold, metricas de cortes
       msdm=c(method='PRES'),
       ensemble=c(method=c('MEAN', 'PCA')), #nao eh type, eh method
       extrapolation=FALSE,
       cores=6) #numero de cores para processar






      
