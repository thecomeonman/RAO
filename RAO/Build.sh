# rm ./Pikachu/NAMESPACE
# touch ./Pikachu/NAMESPACE
sudo R CMD build  RAO
sudo R CMD document RAO
sudo R CMD INSTALL RAO
#sudo R -e 'library(devtools);document();build();install()'
