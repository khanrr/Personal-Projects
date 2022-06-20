TeamsElo <- read.csv("NCAAElo.csv")

if (TeamsElo$Gonzaga > TeamsElo$Norfolk.St.){
  TeamsElo$Gonzaga <- TeamsElo$Gonzaga + elo.update(c(1, 0), TeamsElo$Gonzaga, TeamsElo$Norfolk.St., k=20)[1]
  TeamsElo$Norfolk.St. <- TeamsElo$Norfolk.St. + elo.update(c(1, 0), TeamsElo$Gonzaga, TeamsElo$Norfolk.St., k=20)[2]
  print('Gonzaga beats Norfolk State')
  TeamsElo = subset(TeamsElo, select = -c(Norfolk.St.))
}else {
  TeamsElo$Gonzaga <- TeamsElo$Gonzaga + elo.update(c(0, 1), TeamsElo$Gonzaga, TeamsElo$Norfolk.St., k=20)[1]
  TeamsElo$Norfolk.St.<- TeamsElo$Norfolk.St. + elo.update(c(0, 1), TeamsElo$Gonzaga, TeamsElo$Norfolk.St., k=20)[2]
  print('Norfolk State beats Gonzaga')
  TeamsElo = subset(TeamsElo, select = -c(Gonzaga))
}

if (TeamsElo$Oklahoma > TeamsElo$Missouri){
  TeamsElo$Oklahoma <- TeamsElo$Oklahoma + elo.update(c(1, 0), TeamsElo$Oklahoma, TeamsElo$Missouri, k=20)[1]
  TeamsElo$Missouri <- TeamsElo$Missouri + elo.update(c(1, 0), TeamsElo$Oklahoma, TeamsElo$Missouri, k=20)[2]
  print('Oklahoma beats Missouri')
  TeamsElo = subset(TeamsElo, select = -c(Missouri))
}else {
  TeamsElo$Oklahoma <- TeamsElo$Oklahoma + elo.update(c(0, 1), TeamsElo$Oklahoma, TeamsElo$Missouri, k=20)[1]
  TeamsElo$Missouri <- TeamsElo$Missouri + elo.update(c(0, 1), TeamsElo$Oklahoma, TeamsElo$Missouri, k=20)[2]
  print('Missouri beats Oklahoma')
  TeamsElo = subset(TeamsElo, select = -c(Oklahoma))
}

if (TeamsElo$Creighton > TeamsElo$UCSB){
  TeamsElo$Creighton <- TeamsElo$Creighton + elo.update(c(1, 0), TeamsElo$Creighton, TeamsElo$UCSB, k=20)[1]
  TeamsElo$UCSB <- TeamsElo$UCSB + elo.update(c(1, 0), TeamsElo$Creighton, TeamsElo$UCSB, k=20)[2]
  print('Creighton beats UCSB')
  TeamsElo = subset(TeamsElo, select = -c(UCSB))
}else {
  TeamsElo$Creighton <- TeamsElo$Creighton + elo.update(c(0, 1), TeamsElo$Creighton, TeamsElo$UCSB, k=20)[1]
  TeamsElo$UCSB <- TeamsElo$UCSB + elo.update(c(0, 1), TeamsElo$Creighton, TeamsElo$UCSB, k=20)[2]
  print('UCSB beats Creighton')
  TeamsElo = subset(TeamsElo, select = -c(Creighton))
}

if (TeamsElo$Virginia > TeamsElo$Ohio){
  TeamsElo$Virginia <- TeamsElo$Virginia + elo.update(c(1, 0), TeamsElo$Virginia, TeamsElo$Ohio, k=20)[1]
  TeamsElo$Ohio <- TeamsElo$Ohio + elo.update(c(1, 0), TeamsElo$Virginia, TeamsElo$Ohio, k=20)[2]
  print('Virginia beats Ohio')
  TeamsElo = subset(TeamsElo, select = -c(Ohio))
}else {
  TeamsElo$Virginia <- TeamsElo$Virginia + elo.update(c(0, 1), TeamsElo$Virginia, TeamsElo$Ohio, k=20)[1]
  TeamsElo$Ohio <- TeamsElo$Ohio + elo.update(c(0, 1), TeamsElo$Virginia, TeamsElo$Ohio, k=20)[2]
  print('Ohio beats Virginia')
  TeamsElo = subset(TeamsElo, select = -c(Virginia))
}

if (TeamsElo$USC > TeamsElo$Drake){
  TeamsElo$USC <- TeamsElo$USC + elo.update(c(1, 0), TeamsElo$USC, TeamsElo$Drake, k=20)[1]
  TeamsElo$Drake <- TeamsElo$Drake + elo.update(c(1, 0), TeamsElo$USC, TeamsElo$Drake, k=20)[2]
  print('USC beats Drake')
  TeamsElo = subset(TeamsElo, select = -c(Drake))
}else {
  TeamsElo$USC <- TeamsElo$USC + elo.update(c(0, 1), TeamsElo$USC, TeamsElo$Drake, k=20)[1]
  TeamsElo$Drake <- TeamsElo$Drake + elo.update(c(0, 1), TeamsElo$USC, TeamsElo$Drake, k=20)[2]
  print('Drake beats USC')
  TeamsElo = subset(TeamsElo, select = -c(USC))
}

if (TeamsElo$Kansas > TeamsElo$Eastern.Wash.){
  TeamsElo$Kansas <- TeamsElo$Kansas + elo.update(c(1, 0), TeamsElo$Kansas, TeamsElo$Eastern.Wash., k=20)[1]
  TeamsElo$Eastern.Wash. <- TeamsElo$Eastern.Wash. + elo.update(c(1, 0), TeamsElo$Kansas, TeamsElo$Eastern.Wash., k=20)[2]
  print('Kansas beats Eastern.Wash.')
  TeamsElo = subset(TeamsElo, select = -c(Eastern.Wash.))
}else {
  TeamsElo$Kansas <- TeamsElo$Kansas + elo.update(c(0, 1), TeamsElo$Kansas, TeamsElo$Eastern.Wash., k=20)[1]
  TeamsElo$Eastern.Wash. <- TeamsElo$Eastern.Wash. + elo.update(c(0, 1), TeamsElo$Kansas, TeamsElo$Eastern.Wash., k=20)[2]
  print('Eastern.Wash. beats Kansas')
  TeamsElo = subset(TeamsElo, select = -c(Kansas))
}

if (TeamsElo$Oregon > TeamsElo$VCU){
  TeamsElo$Oregon <- TeamsElo$Oregon + elo.update(c(1, 0), TeamsElo$Oregon, TeamsElo$VCU, k=20)[1]
  TeamsElo$VCU <- TeamsElo$VCU + elo.update(c(1, 0), TeamsElo$Oregon, TeamsElo$VCU, k=20)[2]
  print('Oregon beats VCU')
  TeamsElo = subset(TeamsElo, select = -c(VCU))
}else {
  TeamsElo$Oregon <- TeamsElo$Oregon + elo.update(c(0, 1), TeamsElo$Oregon, TeamsElo$VCU, k=20)[1]
  TeamsElo$VCU <- TeamsElo$VCU + elo.update(c(0, 1), TeamsElo$Oregon, TeamsElo$VCU, k=20)[2]
  print('VCU beats Oregon')
  TeamsElo = subset(TeamsElo, select = -c(Oregon))
}

if (TeamsElo$Iowa > TeamsElo$Grand.Canyon){
  TeamsElo$Iowa <- TeamsElo$Iowa + elo.update(c(1, 0), TeamsElo$Iowa, TeamsElo$Grand.Canyon, k=20)[1]
  TeamsElo$Grand.Canyon <- TeamsElo$Grand.Canyon + elo.update(c(1, 0), TeamsElo$Iowa, TeamsElo$Grand.Canyon, k=20)[2]
  print('Iowa beats Grand.Canyon')
  TeamsElo = subset(TeamsElo, select = -c(Grand.Canyon))
}else {
  TeamsElo$Iowa <- TeamsElo$Iowa + elo.update(c(0, 1), TeamsElo$Iowa, TeamsElo$Grand.Canyon, k=20)[1]
  TeamsElo$Grand.Canyon <- TeamsElo$Grand.Canyon + elo.update(c(0, 1), TeamsElo$Iowa, TeamsElo$Grand.Canyon, k=20)[2]
  print('Grand.Canyon beats Iowa')
  TeamsElo = subset(TeamsElo, select = -c(Iowa))
}

if (TeamsElo$Michigan > TeamsElo$Texas.So.){
  TeamsElo$Michigan <- TeamsElo$Michigan + elo.update(c(1, 0), TeamsElo$Michigan, TeamsElo$Texas.So., k=20)[1]
  TeamsElo$Texas.So. <- TeamsElo$Texas.So. + elo.update(c(1, 0), TeamsElo$Michigan, TeamsElo$Texas.So., k=20)[2]
  print('Michigan beats Texas.So.')
  TeamsElo = subset(TeamsElo, select = -c(Texas.So.))
}else {
  TeamsElo$Michigan <- TeamsElo$Michigan + elo.update(c(0, 1), TeamsElo$Michigan, TeamsElo$Texas.So., k=20)[1]
  TeamsElo$Texas.So. <- TeamsElo$Texas.So. + elo.update(c(0, 1), TeamsElo$Michigan, TeamsElo$Texas.So., k=20)[2]
  print('Texas.So. beats Michigan')
  TeamsElo = subset(TeamsElo, select = -c(Michigan))
}

if (TeamsElo$LSU > TeamsElo$St..Bonaventure){
  TeamsElo$LSU <- TeamsElo$LSU + elo.update(c(1, 0), TeamsElo$LSU, TeamsElo$St..Bonaventure, k=20)[1]
  TeamsElo$St..Bonaventure <- TeamsElo$St..Bonaventure + elo.update(c(1, 0), TeamsElo$LSU, TeamsElo$St..Bonaventure, k=20)[2]
  print('LSU beats St..Bonaventure')
  TeamsElo = subset(TeamsElo, select = -c(St..Bonaventure))
}else {
  TeamsElo$LSU <- TeamsElo$LSU + elo.update(c(0, 1), TeamsElo$LSU, TeamsElo$St..Bonaventure, k=20)[1]
  TeamsElo$St..Bonaventure <- TeamsElo$St..Bonaventure + elo.update(c(0, 1), TeamsElo$LSU, TeamsElo$St..Bonaventure, k=20)[2]
  print('St..Bonaventure beats LSU')
  TeamsElo = subset(TeamsElo, select = -c(LSU))
}

if (TeamsElo$Colorado > TeamsElo$Georgetown){
  TeamsElo$Colorado <- TeamsElo$Colorado + elo.update(c(1, 0), TeamsElo$Colorado, TeamsElo$Georgetown, k=20)[1]
  TeamsElo$Georgetown <- TeamsElo$Georgetown + elo.update(c(1, 0), TeamsElo$Colorado, TeamsElo$Georgetown, k=20)[2]
  print('Colorado beats Georgetown')
  TeamsElo = subset(TeamsElo, select = -c(Georgetown))
}else {
  TeamsElo$Colorado <- TeamsElo$Colorado + elo.update(c(0, 1), TeamsElo$Colorado, TeamsElo$Georgetown, k=20)[1]
  TeamsElo$Georgetown <- TeamsElo$Georgetown + elo.update(c(0, 1), TeamsElo$Colorado, TeamsElo$Georgetown, k=20)[2]
  print('Georgetown beats Colorado')
  TeamsElo = subset(TeamsElo, select = -c(Colorado))
}

if (TeamsElo$Florida.St. > TeamsElo$UNC.Greensboro){
  TeamsElo$Florida.St. <- TeamsElo$Florida.St. + elo.update(c(1, 0), TeamsElo$Florida.St., TeamsElo$UNC.Greensboro, k=20)[1]
  TeamsElo$UNC.Greensboro <- TeamsElo$UNC.Greensboro + elo.update(c(1, 0), TeamsElo$Florida.St., TeamsElo$UNC.Greensboro, k=20)[2]
  print('Florida.St. beats UNC.Greensboro')
  TeamsElo = subset(TeamsElo, select = -c(UNC.Greensboro))
}else {
  TeamsElo$Florida.St. <- TeamsElo$Florida.St. + elo.update(c(0, 1), TeamsElo$Florida.St., TeamsElo$UNC.Greensboro, k=20)[1]
  TeamsElo$UNC.Greensboro <- TeamsElo$UNC.Greensboro + elo.update(c(0, 1), TeamsElo$Florida.St., TeamsElo$UNC.Greensboro, k=20)[2]
  print('UNC.Greensboro beats Florida.St.')
  TeamsElo = subset(TeamsElo, select = -c(Florida.St.))
}

if (TeamsElo$BYU > TeamsElo$UCLA){
  TeamsElo$BYU <- TeamsElo$BYU + elo.update(c(1, 0), TeamsElo$BYU, TeamsElo$UCLA, k=20)[1]
  TeamsElo$UCLA <- TeamsElo$UCLA + elo.update(c(1, 0), TeamsElo$BYU, TeamsElo$UCLA, k=20)[2]
  print('BYU beats UCLA')
  TeamsElo = subset(TeamsElo, select = -c(UCLA))
}else {
  TeamsElo$BYU <- TeamsElo$BYU + elo.update(c(0, 1), TeamsElo$BYU, TeamsElo$UCLA, k=20)[1]
  TeamsElo$UCLA <- TeamsElo$UCLA + elo.update(c(0, 1), TeamsElo$BYU, TeamsElo$UCLA, k=20)[2]
  print('UCLA beats BYU')
  TeamsElo = subset(TeamsElo, select = -c(BYU))
}

if (TeamsElo$Texas > TeamsElo$Abilene.Christian){
  TeamsElo$Texas <- TeamsElo$Texas + elo.update(c(1, 0), TeamsElo$Texas, TeamsElo$Abilene.Christian, k=20)[1]
  TeamsElo$Abilene.Christian <- TeamsElo$Abilene.Christian + elo.update(c(1, 0), TeamsElo$Texas, TeamsElo$Abilene.Christian, k=20)[2]
  print('Texas beats Abilene.Christian')
  TeamsElo = subset(TeamsElo, select = -c(Abilene.Christian))
}else {
  TeamsElo$Texas <- TeamsElo$Texas + elo.update(c(0, 1), TeamsElo$Texas, TeamsElo$Abilene.Christian, k=20)[1]
  TeamsElo$Abilene.Christian <- TeamsElo$Abilene.Christian + elo.update(c(0, 1), TeamsElo$Texas, TeamsElo$Abilene.Christian, k=20)[2]
  print('Abilene.Christian beats Texas')
  TeamsElo = subset(TeamsElo, select = -c(Texas))
}

if (TeamsElo$UConn > TeamsElo$Maryland){
  TeamsElo$UConn <- TeamsElo$UConn + elo.update(c(1, 0), TeamsElo$UConn, TeamsElo$Maryland, k=20)[1]
  TeamsElo$Maryland <- TeamsElo$Maryland + elo.update(c(1, 0), TeamsElo$UConn, TeamsElo$Maryland, k=20)[2]
  print('UConn beats Maryland')
  TeamsElo = subset(TeamsElo, select = -c(Maryland))
}else {
  TeamsElo$UConn <- TeamsElo$UConn + elo.update(c(0, 1), TeamsElo$UConn, TeamsElo$Maryland, k=20)[1]
  TeamsElo$Maryland <- TeamsElo$Maryland + elo.update(c(0, 1), TeamsElo$UConn, TeamsElo$Maryland, k=20)[2]
  print('Maryland beats UConn')
  TeamsElo = subset(TeamsElo, select = -c(UConn))
}

if (TeamsElo$Alabama > TeamsElo$Iona){
  TeamsElo$Alabama <- TeamsElo$Alabama + elo.update(c(1, 0), TeamsElo$Alabama, TeamsElo$Iona, k=20)[1]
  TeamsElo$Iona <- TeamsElo$Iona + elo.update(c(1, 0), TeamsElo$Alabama, TeamsElo$Iona, k=20)[2]
  print('Alabama beats Iona')
  TeamsElo = subset(TeamsElo, select = -c(Iona))
}else {
  TeamsElo$Alabama <- TeamsElo$Alabama + elo.update(c(0, 1), TeamsElo$Alabama, TeamsElo$Iona, k=20)[1]
  TeamsElo$Iona <- TeamsElo$Iona + elo.update(c(0, 1), TeamsElo$Alabama, TeamsElo$Iona, k=20)[2]
  print('Iona beats Alabama')
  TeamsElo = subset(TeamsElo, select = -c(Alabama))
}

if (TeamsElo$Baylor > TeamsElo$Hartford){
  TeamsElo$Baylor <- TeamsElo$Baylor + elo.update(c(1, 0), TeamsElo$Baylor, TeamsElo$Hartford, k=20)[1]
  TeamsElo$Hartford <- TeamsElo$Hartford + elo.update(c(1, 0), TeamsElo$Baylor, TeamsElo$Hartford, k=20)[2]
  print('Baylor beats Hartford')
  TeamsElo = subset(TeamsElo, select = -c(Hartford))
}else {
  TeamsElo$Baylor <- TeamsElo$Baylor + elo.update(c(0, 1), TeamsElo$Baylor, TeamsElo$Hartford, k=20)[1]
  TeamsElo$Hartford <- TeamsElo$Hartford + elo.update(c(0, 1), TeamsElo$Baylor, TeamsElo$Hartford, k=20)[2]
  print('Hartford beats Baylor')
  TeamsElo = subset(TeamsElo, select = -c(Baylor))
}

if (TeamsElo$North.Carolina > TeamsElo$Wisconsin){
  TeamsElo$North.Carolina <- TeamsElo$North.Carolina + elo.update(c(1, 0), TeamsElo$North.Carolina, TeamsElo$Wisconsin, k=20)[1]
  TeamsElo$Wisconsin <- TeamsElo$Wisconsin + elo.update(c(1, 0), TeamsElo$North.Carolina, TeamsElo$Wisconsin, k=20)[2]
  print('North.Carolina beats Wisconsin')
  TeamsElo = subset(TeamsElo, select = -c(Wisconsin))
}else {
  TeamsElo$North.Carolina <- TeamsElo$North.Carolina + elo.update(c(0, 1), TeamsElo$North.Carolina, TeamsElo$Wisconsin, k=20)[1]
  TeamsElo$Wisconsin <- TeamsElo$Wisconsin + elo.update(c(0, 1), TeamsElo$North.Carolina, TeamsElo$Wisconsin, k=20)[2]
  print('Wisconsin beats North.Carolina')
  TeamsElo = subset(TeamsElo, select = -c(North.Carolina))
}

if (TeamsElo$Villanova > TeamsElo$Winthrop){
  TeamsElo$Villanova <- TeamsElo$Villanova + elo.update(c(1, 0), TeamsElo$Villanova, TeamsElo$Winthrop, k=20)[1]
  TeamsElo$Winthrop <- TeamsElo$Winthrop + elo.update(c(1, 0), TeamsElo$Villanova, TeamsElo$Winthrop, k=20)[2]
  print('Villanova beats Winthrop')
  TeamsElo = subset(TeamsElo, select = -c(Winthrop))
}else {
  TeamsElo$Villanova <- TeamsElo$Villanova + elo.update(c(0, 1), TeamsElo$Villanova, TeamsElo$Winthrop, k=20)[1]
  TeamsElo$Winthrop <- TeamsElo$Winthrop + elo.update(c(0, 1), TeamsElo$Villanova, TeamsElo$Winthrop, k=20)[2]
  print('Winthrop beats Villanova')
  TeamsElo = subset(TeamsElo, select = -c(Villanova))
}

if (TeamsElo$Purdue > TeamsElo$North.Texas){
  TeamsElo$Purdue <- TeamsElo$Purdue + elo.update(c(1, 0), TeamsElo$Purdue, TeamsElo$North.Texas, k=20)[1]
  TeamsElo$North.Texas <- TeamsElo$North.Texas + elo.update(c(1, 0), TeamsElo$Purdue, TeamsElo$North.Texas, k=20)[2]
  print('Purdue beats North.Texas')
  TeamsElo = subset(TeamsElo, select = -c(North.Texas))
}else {
  TeamsElo$Purdue <- TeamsElo$Purdue + elo.update(c(0, 1), TeamsElo$Purdue, TeamsElo$North.Texas, k=20)[1]
  TeamsElo$North.Texas <- TeamsElo$North.Texas + elo.update(c(0, 1), TeamsElo$Purdue, TeamsElo$North.Texas, k=20)[2]
  print('North.Texas beats Purdue')
  TeamsElo = subset(TeamsElo, select = -c(Purdue))
}

if (TeamsElo$Texas.Tech > TeamsElo$Utah.St.){
  TeamsElo$Texas.Tech <- TeamsElo$Texas.Tech + elo.update(c(1, 0), TeamsElo$Texas.Tech, TeamsElo$Utah.St., k=20)[1]
  TeamsElo$Utah.St. <- TeamsElo$Utah.St. + elo.update(c(1, 0), TeamsElo$Texas.Tech, TeamsElo$Utah.St., k=20)[2]
  print('Texas.Tech beats Utah.St.')
  TeamsElo = subset(TeamsElo, select = -c(Utah.St.))
}else {
  TeamsElo$Texas.Tech <- TeamsElo$Texas.Tech + elo.update(c(0, 1), TeamsElo$Texas.Tech, TeamsElo$Utah.St., k=20)[1]
  TeamsElo$Utah.St. <- TeamsElo$Utah.St. + elo.update(c(0, 1), TeamsElo$Texas.Tech, TeamsElo$Utah.St., k=20)[2]
  print('Utah.St. beats Texas.Tech')
  TeamsElo = subset(TeamsElo, select = -c(Texas.Tech))
}

if (TeamsElo$Arkansas > TeamsElo$Colgate){
  TeamsElo$Arkansas <- TeamsElo$Arkansas + elo.update(c(1, 0), TeamsElo$Arkansas, TeamsElo$Colgate, k=20)[1]
  TeamsElo$Colgate <- TeamsElo$Colgate + elo.update(c(1, 0), TeamsElo$Arkansas, TeamsElo$Colgate, k=20)[2]
  print('Arkansas beats Colgate')
  TeamsElo = subset(TeamsElo, select = -c(Colgate))
}else {
  TeamsElo$Arkansas <- TeamsElo$Arkansas + elo.update(c(0, 1), TeamsElo$Arkansas, TeamsElo$Colgate, k=20)[1]
  TeamsElo$Colgate <- TeamsElo$Colgate + elo.update(c(0, 1), TeamsElo$Arkansas, TeamsElo$Colgate, k=20)[2]
  print('Colgate beats Arkansas')
  TeamsElo = subset(TeamsElo, select = -c(Arkansas))
}

if (TeamsElo$Florida > TeamsElo$Virginia.Tech){
  TeamsElo$Florida <- TeamsElo$Florida + elo.update(c(1, 0), TeamsElo$Florida, TeamsElo$Virginia.Tech, k=20)[1]
  TeamsElo$Virginia.Tech <- TeamsElo$Virginia.Tech + elo.update(c(1, 0), TeamsElo$Florida, TeamsElo$Virginia.Tech, k=20)[2]
  print('Florida beats Virginia.Tech')
  TeamsElo = subset(TeamsElo, select = -c(Virginia.Tech))
}else {
  TeamsElo$Florida <- TeamsElo$Florida + elo.update(c(0, 1), TeamsElo$Florida, TeamsElo$Virginia.Tech, k=20)[1]
  TeamsElo$Virginia.Tech <- TeamsElo$Virginia.Tech + elo.update(c(0, 1), TeamsElo$Florida, TeamsElo$Virginia.Tech, k=20)[2]
  print('Virginia.Tech beats Florida')
  TeamsElo = subset(TeamsElo, select = -c(Florida))
}

if (TeamsElo$Ohio.St. > TeamsElo$Oral.Roberts){
  TeamsElo$Ohio.St. <- TeamsElo$Ohio.St. + elo.update(c(1, 0), TeamsElo$Ohio.St., TeamsElo$Oral.Roberts, k=20)[1]
  TeamsElo$Oral.Roberts <- TeamsElo$Oral.Roberts + elo.update(c(1, 0), TeamsElo$Ohio.St., TeamsElo$Oral.Roberts, k=20)[2]
  print('Ohio.St. beats Oral.Roberts')
  TeamsElo = subset(TeamsElo, select = -c(Oral.Roberts))
}else {
  TeamsElo$Ohio.St. <- TeamsElo$Ohio.St. + elo.update(c(0, 1), TeamsElo$Ohio.St., TeamsElo$Oral.Roberts, k=20)[1]
  TeamsElo$Oral.Roberts <- TeamsElo$Oral.Roberts + elo.update(c(0, 1), TeamsElo$Ohio.St., TeamsElo$Oral.Roberts, k=20)[2]
  print('Oral.Roberts beats Ohio.St.')
  TeamsElo = subset(TeamsElo, select = -c(Ohio.St.))
}

if (TeamsElo$Illinois > TeamsElo$Drexel){
  TeamsElo$Illinois <- TeamsElo$Illinois + elo.update(c(1, 0), TeamsElo$Illinois, TeamsElo$Drexel, k=20)[1]
  TeamsElo$Drexel <- TeamsElo$Drexel + elo.update(c(1, 0), TeamsElo$Illinois, TeamsElo$Drexel, k=20)[2]
  print('Illinois beats Drexel')
  TeamsElo = subset(TeamsElo, select = -c(Drexel))
}else {
  TeamsElo$Illinois <- TeamsElo$Illinois + elo.update(c(0, 1), TeamsElo$Illinois, TeamsElo$Drexel, k=20)[1]
  TeamsElo$Drexel <- TeamsElo$Drexel + elo.update(c(0, 1), TeamsElo$Illinois, TeamsElo$Drexel, k=20)[2]
  print('Drexel beats Illinois')
  TeamsElo = subset(TeamsElo, select = -c(Illinois))
}

if (TeamsElo$Loyola.Chicago > TeamsElo$Georgia.Tech){
  TeamsElo$Loyola.Chicago <- TeamsElo$Loyola.Chicago + elo.update(c(1, 0), TeamsElo$Loyola.Chicago, TeamsElo$Georgia.Tech, k=20)[1]
  TeamsElo$Georgia.Tech <- TeamsElo$Georgia.Tech + elo.update(c(1, 0), TeamsElo$Loyola.Chicago, TeamsElo$Georgia.Tech, k=20)[2]
  print('Loyola.Chicago beats Georgia.Tech')
  TeamsElo = subset(TeamsElo, select = -c(Georgia.Tech))
}else {
  TeamsElo$Loyola.Chicago <- TeamsElo$Loyola.Chicago + elo.update(c(0, 1), TeamsElo$Loyola.Chicago, TeamsElo$Georgia.Tech, k=20)[1]
  TeamsElo$Georgia.Tech <- TeamsElo$Georgia.Tech + elo.update(c(0, 1), TeamsElo$Loyola.Chicago, TeamsElo$Georgia.Tech, k=20)[2]
  print('Georgia.Tech beats Loyola.Chicago')
  TeamsElo = subset(TeamsElo, select = -c(Loyola.Chicago))
}

if (TeamsElo$Tennessee > TeamsElo$Oregon.St.){
  TeamsElo$Tennessee <- TeamsElo$Tennessee + elo.update(c(1, 0), TeamsElo$Tennessee, TeamsElo$Oregon.St., k=20)[1]
  TeamsElo$Oregon.St. <- TeamsElo$Oregon.St. + elo.update(c(1, 0), TeamsElo$Tennessee, TeamsElo$Oregon.St., k=20)[2]
  print('Tennessee beats Oregon.St.')
  TeamsElo = subset(TeamsElo, select = -c(Oregon.St.))
}else {
  TeamsElo$Tennessee <- TeamsElo$Tennessee + elo.update(c(0, 1), TeamsElo$Tennessee, TeamsElo$Oregon.St., k=20)[1]
  TeamsElo$Oregon.St. <- TeamsElo$Oregon.St. + elo.update(c(0, 1), TeamsElo$Tennessee, TeamsElo$Oregon.St., k=20)[2]
  print('Oregon.St. beats Tennessee')
  TeamsElo = subset(TeamsElo, select = -c(Tennessee))
}

if (TeamsElo$Oklahoma.St. > TeamsElo$Liberty){
  TeamsElo$Oklahoma.St. <- TeamsElo$Oklahoma.St. + elo.update(c(1, 0), TeamsElo$Oklahoma.St., TeamsElo$Liberty, k=20)[1]
  TeamsElo$Liberty <- TeamsElo$Liberty + elo.update(c(1, 0), TeamsElo$Oklahoma.St., TeamsElo$Liberty, k=20)[2]
  print('Oklahoma.St. beats Liberty')
  TeamsElo = subset(TeamsElo, select = -c(Liberty))
}else {
  TeamsElo$Oklahoma.St. <- TeamsElo$Oklahoma.St. + elo.update(c(0, 1), TeamsElo$Oklahoma.St., TeamsElo$Liberty, k=20)[1]
  TeamsElo$Liberty <- TeamsElo$Liberty + elo.update(c(0, 1), TeamsElo$Oklahoma.St., TeamsElo$Liberty, k=20)[2]
  print('Liberty beats Oklahoma.St.')
  TeamsElo = subset(TeamsElo, select = -c(Oklahoma.St.))
}

if (TeamsElo$San.Diego.St. > TeamsElo$Syracuse){
  TeamsElo$San.Diego.St. <- TeamsElo$San.Diego.St. + elo.update(c(1, 0), TeamsElo$San.Diego.St., TeamsElo$Syracuse, k=20)[1]
  TeamsElo$Syracuse <- TeamsElo$Syracuse + elo.update(c(1, 0), TeamsElo$San.Diego.St., TeamsElo$Syracuse, k=20)[2]
  print('San.Diego.St. beats Syracuse')
  TeamsElo = subset(TeamsElo, select = -c(Syracuse))
}else {
  TeamsElo$San.Diego.St. <- TeamsElo$San.Diego.St. + elo.update(c(0, 1), TeamsElo$San.Diego.St., TeamsElo$Syracuse, k=20)[1]
  TeamsElo$Syracuse <- TeamsElo$Syracuse + elo.update(c(0, 1), TeamsElo$San.Diego.St., TeamsElo$Syracuse, k=20)[2]
  print('Syracuse beats San.Diego.St.')
  TeamsElo = subset(TeamsElo, select = -c(San.Diego.St.))
}

if (TeamsElo$West.Virginia > TeamsElo$Morehead.St.){
  TeamsElo$West.Virginia <- TeamsElo$West.Virginia + elo.update(c(1, 0), TeamsElo$West.Virginia, TeamsElo$Morehead.St., k=20)[1]
  TeamsElo$Morehead.St. <- TeamsElo$Morehead.St. + elo.update(c(1, 0), TeamsElo$West.Virginia, TeamsElo$Morehead.St., k=20)[2]
  print('West.Virginia beats Morehead.St.')
  TeamsElo = subset(TeamsElo, select = -c(Morehead.St.))
}else {
  TeamsElo$West.Virginia <- TeamsElo$West.Virginia + elo.update(c(0, 1), TeamsElo$West.Virginia, TeamsElo$Morehead.St., k=20)[1]
  TeamsElo$Morehead.St. <- TeamsElo$Morehead.St. + elo.update(c(0, 1), TeamsElo$West.Virginia, TeamsElo$Morehead.St., k=20)[2]
  print('Morehead.St. beats West.Virginia')
  TeamsElo = subset(TeamsElo, select = -c(West.Virginia))
}

if (TeamsElo$Clemson > TeamsElo$Rutgers){
  TeamsElo$Clemson <- TeamsElo$Clemson + elo.update(c(1, 0), TeamsElo$Clemson, TeamsElo$Rutgers, k=20)[1]
  TeamsElo$Rutgers <- TeamsElo$Rutgers + elo.update(c(1, 0), TeamsElo$Clemson, TeamsElo$Rutgers, k=20)[2]
  print('Clemson beats Rutgers')
  TeamsElo = subset(TeamsElo, select = -c(Rutgers))
}else {
  TeamsElo$Clemson <- TeamsElo$Clemson + elo.update(c(0, 1), TeamsElo$Clemson, TeamsElo$Rutgers, k=20)[1]
  TeamsElo$Rutgers <- TeamsElo$Rutgers + elo.update(c(0, 1), TeamsElo$Clemson, TeamsElo$Rutgers, k=20)[2]
  print('Rutgers beats Clemson')
  TeamsElo = subset(TeamsElo, select = -c(Clemson))
}

if (TeamsElo$Houston > TeamsElo$Cleveland.St.){
  TeamsElo$Houston <- TeamsElo$Houston + elo.update(c(1, 0), TeamsElo$Houston, TeamsElo$Cleveland.St., k=20)[1]
  TeamsElo$Cleveland.St. <- TeamsElo$Cleveland.St. + elo.update(c(1, 0), TeamsElo$Houston, TeamsElo$Cleveland.St., k=20)[2]
  print('Houston beats Cleveland.St.')
  TeamsElo = subset(TeamsElo, select = -c(Cleveland.St.))
}else {
  TeamsElo$Houston <- TeamsElo$Houston + elo.update(c(0, 1), TeamsElo$Houston, TeamsElo$Cleveland.St., k=20)[1]
  TeamsElo$Cleveland.St. <- TeamsElo$Cleveland.St. + elo.update(c(0, 1), TeamsElo$Houston, TeamsElo$Cleveland.St., k=20)[2]
  print('Cleveland.St. beats Houston')
  TeamsElo = subset(TeamsElo, select = -c(Houston))
}

if (TeamsElo$T1 > TeamsElo$T2){
  TeamsElo$T1 <- TeamsElo$T1 + elo.update(c(1, 0), TeamsElo$T1, TeamsElo$T2, k=20)[1]
  TeamsElo$T2 <- TeamsElo$T2 + elo.update(c(1, 0), TeamsElo$T1, TeamsElo$T2, k=20)[2]
  print('T1 beats T2')
  TeamsElo = subset(TeamsElo, select = -c(T2))
}else {
  TeamsElo$T1 <- TeamsElo$T1 + elo.update(c(0, 1), TeamsElo$T1, TeamsElo$T2, k=20)[1]
  TeamsElo$T2 <- TeamsElo$T2 + elo.update(c(0, 1), TeamsElo$T1, TeamsElo$T2, k=20)[2]
  print('T2 beats T1')
  TeamsElo = subset(TeamsElo, select = -c(T1))
}