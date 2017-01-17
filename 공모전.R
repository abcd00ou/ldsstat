

library(gmodels)
library(sqldf)

# Data Reading
setwd("C:\\Users\\Dongsung\\Desktop\\공모전\\challenge_data\\챌린지리그_데이터")
claim<-read.csv("BGCON_CLAIM_DATA.csv",header=T,sep=",")
cntt<-read.csv("BGCON_CNTT_DATA.csv",header=T,sep=",")
cust<-read.csv("BGCON_CUST_DATA.csv",header=T,sep=",")
family<-read.csv("BGCON_FMLY_DATA.csv",header=T,sep=",")
fpinfo<-read.csv("BGCON_FPINFO_DATA.csv",header=T,sep=",")



##data reconstruction(데이터 재구성 )
custcntt<-merge(cust,cntt,"CUST_ID")
custcntt$DIF_INCM<-custcntt$CUST_INCM-custcntt$MNTH_INCM_AMT
(custcntt$DIF_INCM[custcntt$SIU_CUST_YN=="Y"])
(custcntt$DIF_INCM[custcntt$SIU_CUST_YN=="N"])

CrossTable(custcntt$DIF_INCM,custcntt$SIU_CUST_YN,prop.t=F,expected=T,chisq=T)

custcnttfp<-merge(cust,cnttfp,"CUST_ID")
CrossTable(custcnttfp$EDGB,custcnttfp$SIU_CUST_YN,prop.t=F,expected=T,chisq=T)

#CAUSE_DATA(사고 원인코드 재구성) 
claim$CAUS_CODE1<-claim$CAUS_CODE
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='A','A',as.character(claim$CAUS_CODE1))
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='B','B',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='C','C',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='D','D',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='E','E',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='F','F',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='G','G',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='H','H',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='I','I',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='J','J',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='K','K',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='L','L',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='M','M',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='N','N',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='O','O',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='P','P',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='Q','Q',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='R','R',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='S','S',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='T','T',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='U','U',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='V','V',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='W','W',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='X','X',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='Y','Y',as.character(claim$CAUS_CODE1))                         
claim$CAUS_CODE1<-ifelse(substr(claim$CAUS_CODE,1,1)=='Z','Z',as.character(claim$CAUS_CODE1))




##CLAIM_COUNT(고객당 클레임 횟수)
countCl<-sqldf("select cust_id,count(cust_id) as CLAIM_SCORE from claim group by cust_id")
claim<-merge(claim,countCl,"CUST_ID")
  

##LATE_SCORE(사고일자 이후에 보험에 가입한 횟수)
cnttclaim<-merge(cntt,claim,"CUST_ID")
cnttclaim$ORIG_RESN_DATE<-substr(cnttclaim$ORIG_RESN_DATE,1,6)
cnttclaim$ORIG_RESN_DATE<-as.character(cnttclaim$ORIG_RESN_DATE)
cnttclaim$CNTT_YM<-as.character(cnttclaim$CNTT_YM)
cnttclaim$POLY_COUNT<-ifelse(cnttclaim$ORIG_RESN_DATE<=cnttclaim$CNTT_YM,1,0)
names(cnttclaim)[names(cnttclaim)=="POLY_NO.x"]<-"POLY_NO"
temp<-sqldf("select CUST_ID, POLY_NO,sum(POLY_COUNT) SUM from cnttclaim  group by POLY_NO order by cust_id")
temp1<-merge(cust,temp,"CUST_ID")
temp2<-sqldf("select cust_id, avg(SUM) as SUM from temp1 group by cust_id")
temp3<-merge(cust,temp2,"CUST_ID",all.x=T)
cust$LATE_SCORE<-temp3$SUM
cust$LATE_SCORE<-ifelse(cust$LATE_SCORE==0,NA,cust$LATE_SCORE)



##트레이닝을 위한 데이터와 실제 테스트를 위한 데이터 분리
cust0<-cust[!cust$SIU_CUST_YN=="",]
xTest<-cust[cust$SIU_CUST_YN=="",]
cust0$FRAUD<-ifelse(cust0$SIU_CUST_YN=="Y",1,0)
cust0$FRAUD<-as.factor(cust0$FRAUD)

##cust 에 대한 점수화 
score1<-function(dsn) {
  
  # 주거형태 점수화
  dsn$RESI_TYPE_SCORE<-0
  dsn$RESI_TYPE_SCORE<-ifelse(dsn$RESI_TYPE_CODE==11, -2.5,dsn$RESI_TYPE_SCORE)
  dsn$RESI_TYPE_SCORE<-ifelse(dsn$RESI_TYPE_CODE==12,  5.4,dsn$RESI_TYPE_SCORE)
  dsn$RESI_TYPE_SCORE<-ifelse(dsn$RESI_TYPE_CODE==13,  2.8,dsn$RESI_TYPE_SCORE)
  dsn$RESI_TYPE_SCORE<-ifelse(dsn$RESI_TYPE_CODE==20,  -13,dsn$RESI_TYPE_SCORE)
  dsn$RESI_TYPE_SCORE<-ifelse(dsn$RESI_TYPE_CODE==30,  8.6,dsn$RESI_TYPE_SCORE)
  dsn$RESI_TYPE_SCORE<-ifelse(dsn$RESI_TYPE_CODE==40,  7.8,dsn$RESI_TYPE_SCORE)
  dsn$RESI_TYPE_SCORE<-ifelse(dsn$RESI_TYPE_CODE==50,  2.4,dsn$RESI_TYPE_SCORE)
  
  dsn$FP_CAREER<-ifelse(dsn$FP_CAREER=="Y",1,0)
  # 직업 점수화
  dsn$OCCP_GRP_SCORE<-0
  dsn$OCCP_GRP_SCORE<-ifelse(dsn$OCCP_GRP_2=="1차산업 종사자", -6.6,dsn$OCCP_GRP_SCORE)
  dsn$OCCP_GRP_SCORE<-ifelse(dsn$OCCP_GRP_2=="2차산업 종사자", -38.7,dsn$OCCP_GRP_SCORE)
  dsn$OCCP_GRP_SCORE<-ifelse(dsn$OCCP_GRP_2=="3차산업 종사자",  48.7,dsn$OCCP_GRP_SCORE)
  dsn$OCCP_GRP_SCORE<-ifelse(dsn$OCCP_GRP_2=="공무원", -5.2,dsn$OCCP_GRP_SCORE)
  dsn$OCCP_GRP_SCORE<-ifelse(dsn$OCCP_GRP_2=="교사", -11,dsn$OCCP_GRP_SCORE)
  dsn$OCCP_GRP_SCORE<-ifelse(dsn$OCCP_GRP_2=="교육관련직", -2,dsn$OCCP_GRP_SCORE)
  dsn$OCCP_GRP_SCORE<-ifelse(dsn$OCCP_GRP_2=="기타", -2.9,dsn$OCCP_GRP_SCORE)
  dsn$OCCP_GRP_SCORE<-ifelse(dsn$OCCP_GRP_2=="자영업", 46,dsn$OCCP_GRP_SCORE)
  dsn$OCCP_GRP_SCORE<-ifelse(dsn$OCCP_GRP_2=="전문직", -4.2,dsn$OCCP_GRP_SCORE)
  dsn$OCCP_GRP_SCORE<-ifelse(dsn$OCCP_GRP_2=="종교인/역술인", 3,dsn$OCCP_GRP_SCORE)
  dsn$OCCP_GRP_SCORE<-ifelse(dsn$OCCP_GRP_2=="주부", 33,dsn$OCCP_GRP_SCORE)
  dsn$OCCP_GRP_SCORE<-ifelse(dsn$OCCP_GRP_2=="학생", -32,dsn$OCCP_GRP_SCORE)
  dsn$OCCP_GRP_SCORE<-ifelse(dsn$OCCP_GRP_2=="학자/연구직", -1.7,dsn$OCCP_GRP_SCORE)
  #지역 점수화
  dsn$CTPR_SCORE<-0
  dsn$CTPR_SCORE<-ifelse(dsn$CTPR=="강원",  17,dsn$CTPR_SCORE)
  dsn$CTPR_SCORE<-ifelse(dsn$CTPR=="경기", -13,dsn$CTPR_SCORE)
  dsn$CTPR_SCORE<-ifelse(dsn$CTPR=="경남",  -9,dsn$CTPR_SCORE)
  dsn$CTPR_SCORE<-ifelse(dsn$CTPR=="경북", -14,dsn$CTPR_SCORE)
  dsn$CTPR_SCORE<-ifelse(dsn$CTPR=="광주", 106,dsn$CTPR_SCORE)
  dsn$CTPR_SCORE<-ifelse(dsn$CTPR=="대구",  -7,dsn$CTPR_SCORE)
  dsn$CTPR_SCORE<-ifelse(dsn$CTPR=="부산",  48,dsn$CTPR_SCORE)
  dsn$CTPR_SCORE<-ifelse(dsn$CTPR=="울산", -25,dsn$CTPR_SCORE)
  dsn$CTPR_SCORE<-ifelse(dsn$CTPR=="인천",  35,dsn$CTPR_SCORE)
  dsn$CTPR_SCORE<-ifelse(dsn$CTPR=="전남", 1.4,dsn$CTPR_SCORE)
  dsn$CTPR_SCORE<-ifelse(dsn$CTPR=="전북", 7.3,dsn$CTPR_SCORE)
  dsn$CTPR_SCORE<-ifelse(dsn$CTPR=="충남",-7.7,dsn$CTPR_SCORE)
  dsn$CTPR_SCORE<-ifelse(dsn$CTPR=="충북",-24.5,dsn$CTPR_SCORE)
  
  #자식수 점수화 
  dsn$CHLD_SCORE<-0
  dsn$CHLD_SCORE<-ifelse(dsn$CHLD_CNT==0,-123,dsn$CHLD_SCORE)
  dsn$CHLD_SCORE<-ifelse(dsn$CHLD_CNT==1,211,dsn$CHLD_SCORE)
  dsn$CHLD_SCORE<-ifelse(dsn$CHLD_CNT==2,-10,dsn$CHLD_SCORE)
  dsn$CHLD_SCORE<-ifelse(dsn$CHLD_CNT==3,150,dsn$CHLD_SCORE)
  dsn$CHLD_SCORE<-ifelse(dsn$CHLD_CNT==4,107,dsn$CHLD_SCORE)
  dsn$CHLD_SCORE<-ifelse(dsn$CHLD_CNT==6,59,dsn$CHLD_SCORE)
  
  dsn$LATE_SCORE<-dsn$LATE_SCORE
  
  return(dsn)
}

cust0<-score1(cust0)
xTest<-score1(xTest)

# cntt 점수화
# noInsu: 인당 보험증권수 
temp<-sqldf("select CUST_ID, count(*) as noInsu, AVG(MAIN_INSR_AMT) as MAIN_INSR_AMT,
            AVG(SUM_ORIG_PREM) as SUM_ORIG_PREM 
            from cntt group by CUST_ID order by CUST_ID")
cust0<-merge(cust0,temp, by="CUST_ID")
xTest<-merge(xTest,temp, by="CUST_ID")
cntt1<-merge(cust0,cntt, by="CUST_ID")

# 계약에서 고객역할 점수
cntt$CUST_ROLE_SCORE<-0
cntt$CUST_ROLE_SCORE<-ifelse(cntt$CUST_ROLE==0, 78,cntt$CUST_ROLE_SCORE)
cntt$CUST_ROLE_SCORE<-ifelse(cntt$CUST_ROLE==1, 20,cntt$CUST_ROLE_SCORE)
cntt$CUST_ROLE_SCORE<-ifelse(cntt$CUST_ROLE==2,-74,cntt$CUST_ROLE_SCORE)
cntt$CUST_ROLE_SCORE<-ifelse(cntt$CUST_ROLE==3, 17,cntt$CUST_ROLE_SCORE)
cntt$CUST_ROLE_SCORE<-ifelse(cntt$CUST_ROLE==4, 44,cntt$CUST_ROLE_SCORE)
cntt$CUST_ROLE_SCORE<-ifelse(cntt$CUST_ROLE==5,-50,cntt$CUST_ROLE_SCORE)
cntt$CUST_ROLE_SCORE<-ifelse(cntt$CUST_ROLE==21,-73,cntt$CUST_ROLE_SCORE)
# 상품점수
cntt$CUST_GOOD_SCORE<-0
cntt$CUST_GOOD_SCORE<-ifelse(cntt$GOOD_CLSF_CDNM=='정기', 42,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE<-ifelse(cntt$GOOD_CLSF_CDNM=='일반저축', 19,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE<-ifelse(cntt$GOOD_CLSF_CDNM=='보장', 7,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE<-ifelse(cntt$GOOD_CLSF_CDNM=='변액연금',-11,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE<-ifelse(cntt$GOOD_CLSF_CDNM=='실손',-13,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE<-ifelse(cntt$GOOD_CLSF_CDNM=='어린이',-9,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE<-ifelse(cntt$GOOD_CLSF_CDNM=='암',-6,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE<-ifelse(cntt$GOOD_CLSF_CDNM=='변액CI',-5,cntt$CUST_GOOD_SCORE)
cntt$CUST_GOOD_SCORE<-ifelse(cntt$GOOD_CLSF_CDNM=='어린이연금',-5,cntt$CUST_GOOD_SCORE)
# 가입채널점수
cntt$CUST_CHNL_SCORE<-0
cntt$CUST_CHNL_SCORE<-ifelse(cntt$SALE_CHNL_CODE==2,-175,cntt$CUST_CHNL_SCORE)
cntt$CUST_CHNL_SCORE<-ifelse(cntt$SALE_CHNL_CODE==3, 30,cntt$CUST_CHNL_SCORE)
cntt$CUST_CHNL_SCORE<-ifelse(cntt$SALE_CHNL_CODE==4, 93,cntt$CUST_CHNL_SCORE)
cntt$CUST_CHNL_SCORE<-ifelse(cntt$SALE_CHNL_CODE==5,-11,cntt$CUST_CHNL_SCORE)
cntt$CUST_CHNL_SCORE<-ifelse(cntt$SALE_CHNL_CODE==6,-5,cntt$CUST_CHNL_SCORE)
cntt$CUST_CHNL_SCORE<-ifelse(cntt$SALE_CHNL_CODE==7, 1,cntt$CUST_CHNL_SCORE)
#보험금 납입주기 점수
cntt$CUST_CYCL_SCORE<-0
cntt$CUST_CYCL_SCORE<-ifelse(cntt$PAYM_CYCL_CODE==0,-15,cntt$CUST_CYCL_SCORE)
cntt$CUST_CYCL_SCORE<-ifelse(cntt$PAYM_CYCL_CODE==1, 15,cntt$CUST_CYCL_SCORE)
cntt$CUST_CYCL_SCORE<-ifelse(cntt$PAYM_CYCL_CODE==3, 85,cntt$CUST_CYCL_SCORE)
cntt$CUST_CYCL_SCORE<-ifelse(cntt$PAYM_CYCL_CODE==12,-287,cntt$CUST_CYCL_SCORE)



temp<-sqldf("select CUST_ID, avg(CUST_ROLE_SCORE) as CUST_ROLE_SCORE, avg(CUST_GOOD_SCORE) as CUST_GOOD_SCORE,
            avg(CUST_CHNL_SCORE) as CUST_CHNL_SCORE, avg(CUST_CYCL_SCORE) as CUST_CYCL_SCORE
            from cntt group by CUST_ID order by CUST_ID")
cust0<-merge(cust0, temp, by="CUST_ID")
xTest<-merge(xTest, temp, by="CUST_ID")



##fpinfo 에 대한 점수화
##fp의 학력에 대한 점수화
cnttfp<-merge(cntt,fpinfo,"CLLT_FP_PRNO")
cnttfp$EDGB_SCORE<-0
cnttfp$EDGB_SCORE<-ifelse(cnttfp$EDGB=="고졸",125,cnttfp$EDGB_SCORE)

temp<-sqldf("select CUST_ID, AVG(EDGB_SCORE) as EDGB_SCORE
            from cnttfp group by CUST_ID order by CUST_ID")
cust0<-merge(cust0,temp,"CUST_ID",all.x=T)
xTest<-merge(xTest,temp,"CUST_ID",all.x=T)



# claim 에 대한 점수화
claim1<-merge(cust0,claim, by="CUST_ID")  #사기자를 판별하기 위한 합서


# 사고구분점수 Scoring
claim$CUST_ACCI_SCORE<-0
claim$CUST_ACCI_SCORE<-ifelse(claim$ACCI_DVSN==1,-85,claim$CUST_ACCI_SCORE)
claim$CUST_ACCI_SCORE<-ifelse(claim$ACCI_DVSN==2,18,claim$CUST_ACCI_SCORE)
claim$CUST_ACCI_SCORE<-ifelse(claim$ACCI_DVSN==3,21,claim$CUST_ACCI_SCORE)
# 보험금청구사유 점수
claim$CUST_RESN_SCORE<-0
claim$CUST_RESN_SCORE<-ifelse(claim$DMND_RESN_CODE==1,-61,claim$CUST_RESN_SCORE)
claim$CUST_RESN_SCORE<-ifelse(claim$DMND_RESN_CODE==2,3010,claim$CUST_RESN_SCORE)
claim$CUST_RESN_SCORE<-ifelse(claim$DMND_RESN_CODE==3,-2045,claim$CUST_RESN_SCORE)
claim$CUST_RESN_SCORE<-ifelse(claim$DMND_RESN_CODE==4,45,claim$CUST_RESN_SCORE)
claim$CUST_RESN_SCORE<-ifelse(claim$DMND_RESN_CODE==5,-751,claim$CUST_RESN_SCORE)
claim$CUST_RESN_SCORE<-ifelse(claim$DMND_RESN_CODE==6,-672,claim$CUST_RESN_SCORE)
claim$CUST_RESN_SCORE<-ifelse(claim$DMND_RESN_CODE==7,-27,claim$CUST_RESN_SCORE)
# 입원/통원 병원 점수
claim$CUST_HSPT_SCORE<-0
claim$CUST_HSPT_SCORE<-ifelse(claim$HOSP_SPEC_DVSN==10,-1823,claim$CUST_HSPT_SCORE)
claim$CUST_HSPT_SCORE<-ifelse(claim$HOSP_SPEC_DVSN==20,18,claim$CUST_HSPT_SCORE)
claim$CUST_HSPT_SCORE<-ifelse(claim$HOSP_SPEC_DVSN==30,25,claim$CUST_HSPT_SCORE)
claim$CUST_HSPT_SCORE<-ifelse(claim$HOSP_SPEC_DVSN==40,-30,claim$CUST_HSPT_SCORE)
claim$CUST_HSPT_SCORE<-ifelse(claim$HOSP_SPEC_DVSN==45,-81,claim$CUST_HSPT_SCORE)
claim$CUST_HSPT_SCORE<-ifelse(claim$HOSP_SPEC_DVSN==80,2402,claim$CUST_HSPT_SCORE)
claim$CUST_HSPT_SCORE<-ifelse(claim$HOSP_SPEC_DVSN==85,210,claim$CUST_HSPT_SCORE)
claim$CUST_HSPT_SCORE<-ifelse(claim$HOSP_SPEC_DVSN==95,1027,claim$CUST_HSPT_SCORE)
# 실손처리여부 점수
claim$CUST_PMMI_SCORE<-ifelse(claim$PMMI_DLNG_YN=='N',1,0)
# 금감원유의병원 점수
claim$CUST_HEED_SCORE<-ifelse(claim$HEED_HOSP_YN=='Y',61,0)
#사고 원인 점수
claim$CAUSE_SCORE<-0
claim$CAUSE_SCORE<-ifelse(claim$CAUS_CODE1=='A',20,claim$CAUSE_SCORE)
claim$CAUSE_SCORE<-ifelse(claim$CAUS_CODE1=='C',-296,claim$CAUSE_SCORE)
claim$CAUSE_SCORE<-ifelse(claim$CAUS_CODE1=='D',-379,claim$CAUSE_SCORE)
claim$CAUSE_SCORE<-ifelse(claim$CAUS_CODE1=='E',44,claim$CAUSE_SCORE)
claim$CAUSE_SCORE<-ifelse(claim$CAUS_CODE1=='H',-124,claim$CAUSE_SCORE)
claim$CAUSE_SCORE<-ifelse(claim$CAUS_CODE1=='I',-28,claim$CAUSE_SCORE)
claim$CAUSE_SCORE<-ifelse(claim$CAUS_CODE1=='K',-31,claim$CAUSE_SCORE)
claim$CAUSE_SCORE<-ifelse(claim$CAUS_CODE1=='L',-18,claim$CAUSE_SCORE)
claim$CAUSE_SCORE<-ifelse(claim$CAUS_CODE1=='M',3405,claim$CAUSE_SCORE)
claim$CAUSE_SCORE<-ifelse(claim$CAUS_CODE1=='N',-238,claim$CAUSE_SCORE)
claim$CAUSE_SCORE<-ifelse(claim$CAUS_CODE1=='O',-287,claim$CAUSE_SCORE)
claim$CAUSE_SCORE<-ifelse(claim$CAUS_CODE1=='R',-39,claim$CAUSE_SCORE)
claim$CAUSE_SCORE<-ifelse(claim$CAUS_CODE1=='V',18,claim$CAUSE_SCORE)
claim$CAUSE_SCORE<-ifelse(claim$CAUS_CODE1=='W',-78,claim$CAUSE_SCORE)







temp<-sqldf("select CUST_ID, avg(CUST_ACCI_SCORE) as CUST_ACCI_SCORE,AVG(CUST_RESN_SCORE) as CUST_RESN_SCORE,
            AVG(CUST_HSPT_SCORE) as CUST_HSPT_SCORE, AVG(CUST_PMMI_SCORE) as CUST_PMMI_SCORE,
            AVG(VLID_HOSP_OTDA) as VLID_HOSP_OTDA,AVG(CAUSE_SCORE) as CAUSE_SCORE, 
            AVG(CUST_HEED_SCORE) as CUST_HEED_SCORE,AVG(CLAIM_SCORE) as CLAIM_SCORE
            from claim group by CUST_ID order by CUST_ID")
cust0<-merge(cust0, temp, by="CUST_ID")
xTest<-merge(xTest, temp, by="CUST_ID")





#청구사유코드일련번호
temp<-sqldf("select cust_id, max(DMND_RSCD_SQNO) as MAX_DMND from claim group by cust_id order by DMND_RSCD_SQNO desc")
cust0<-merge(cust0, temp, "CUST_ID")
xTest<-merge(xTest, temp, "CUST_ID")

#병원종별
hospec<-sqldf("select HOSP_SPEC_DVSN, count(distinct CUST_ID) as x from claim1 where SIU_CUST_YN = 'Y' group by HOSP_SPEC_DVSN")
hospec2<-sqldf("select HOSP_SPEC_DVSN, count(distinct CUST_ID) as y from claim1 group by HOSP_SPEC_DVSN")
hospec3<-merge(hospec,hospec2,"HOSP_SPEC_DVSN")
hospec3$HOSPEC_SCORE<-hospec3$x/hospec3$y
hospec<-hospec3[,c(1,4)]
rm(hospec2,hospec3)
claim2<-merge(claim,hospec,"HOSP_SPEC_DVSN")
claim$HOSPEC_SCORE <- ifelse(is.na(claim2$HOSP_SPEC_DVSN)==T,0,claim2$HOSPEC_SCORE)
temp<-sqldf("select cust_id, avg(HOSPEC_SCORE) as HOSPEC_SCORE from claim group by cust_id")

cust0<-merge(cust0, temp, by="CUST_ID")
xTest<-merge(xTest, temp, by="CUST_ID")




## 트레이닝을 위한 샘플링
cust1 <- cust0[order(cust0$SIU_CUST_YN),] 

samp <- c(sample(1:18801,3600), sample(18802:20607, 1800))
samp0 <- append(samp[1:1800],samp[3601:4500])
samp1 <- append(samp[1801:3600],samp[4501:5400])



##트레이닝셋과 연습용 테스트셋 구성
fdsTr<-cust1[samp0,]
fdsTe<-cust1[samp1,]

x <- subset(fdsTe, select=-FRAUD)
y <- fdsTe$FRAUD


# 사기자인 FP 찾기
# FPFraud: FP Fraud Rate for fdsTr
FpList<-sqldf("select CUST_ID, CLLT_FP_PRNO from cntt order by CUST_ID")
CustList<-sqldf("select CUST_ID, FRAUD from fdsTr order by CUST_ID")
temp1<-merge(x=CustList,y=FpList, by="CUST_ID", all.x=TRUE)
FpScore<-sqldf("select CLLT_FP_PRNO, avg(Fraud) as FpFrScore from temp1 group by CLLT_FP_PRNO ")
temp2<-sqldf("select a.CUST_ID,a.CLLT_FP_PRNO, b.FpFrScore
             from temp1 a left outer join FpScore b 
             on a.CLLT_FP_PRNO=b.CLLT_FP_PRNO order by a.CUST_ID")
temp3<-sqldf("select CUST_ID, avg(FpFrScore) as FpFrScore from temp2 group by CUST_ID")
fdsTr<-merge(x=fdsTr,y=temp3, by="CUST_ID", all.x=TRUE)


# FPFraud: FP Fraud Rate for fdsTe
CustList<-sqldf("select CUST_ID from fdsTe order by CUST_ID")
temp1<-merge(x=CustList,y=FpList, by="CUST_ID", all.x=TRUE)
temp2<-sqldf("select a.CUST_ID,a.CLLT_FP_PRNO, b.FpFrScore
             from temp1 a left outer join FpScore b 
             on a.CLLT_FP_PRNO=b.CLLT_FP_PRNO order by a.CUST_ID")
FpScore<-sqldf("select CUST_ID, avg(FpFrScore) as FpFrScore from temp2 group by CUST_ID")
fdsTe<-merge(x=fdsTe,y=FpScore, by="CUST_ID", all.x=TRUE)

# 사기자인 의사 찾기
#DOCFraud : DOC Fraud Rate for fdsTr
DocList<-sqldf("select CUST_ID, CHME_LICE_NO from claim order by CUST_ID")
CustList<-sqldf("select CUST_ID, FRAUD from fdsTr order by CUST_ID")
temp1<-merge(x=CustList,y=DocList, by="CUST_ID", all.x=TRUE)
DocScore<-sqldf("select CHME_LICE_NO, avg(Fraud) as DocFrScore from temp1 group by CHME_LICE_NO")
temp2<-sqldf("select a.CUST_ID,a.CHME_LICE_NO, b.DocFrScore
             from temp1 a left outer join DocScore b 
             on a.CHME_LICE_NO=b.CHME_LICE_NO order by a.CUST_ID")
temp3<-sqldf("select CUST_ID, avg(DocFrScore) as DocFrScore from temp2 group by CUST_ID")
fdsTr<-merge(x=fdsTr,y=temp3, by="CUST_ID", all.x=TRUE)


#DOCFraud : DOC Fraud Rate for fdsTe
CustList<-sqldf("select CUST_ID from fdsTe order by CUST_ID")
temp1<-merge(x=CustList,y=DocList, by="CUST_ID", all.x=TRUE)
temp2<-sqldf("select a.CUST_ID,a.CHME_LICE_NO, b.DocFrScore
             from temp1 a left outer join DocScore b 
             on a.CHME_LICE_NO=b.CHME_LICE_NO order by a.CUST_ID")
DocScore<-sqldf("select CUST_ID, avg(DocFrScore) as DocFrScore from temp2 group by CUST_ID")
fdsTe<-merge(x=fdsTe,y=DocScore, by="CUST_ID", all.x=TRUE)


##FP 와 DOC 점수화에 결측치인 경우 회귀 직선의 평균을 적용하여 결측치 처리
fdsTe$FpFrScore<-ifelse(is.na(fdsTe$FpFrScore)==TRUE,-0.03886+0.98653*fdsTe$DocFrScore,fdsTe$FpFrScore)
fdsTe$DocFrScore<-ifelse(is.na(fdsTe$DocFrScore)==TRUE,0.1309+0.836*fdsTe$FpFrScore,fdsTe$DocFrScore)

fdsTr[,50]<-ifelse(is.na(fdsTr[,50])==TRUE,mean(fdsTr[,50],na.rm=TRUE),fdsTr[,50])


x <- subset(fdsTe, select=-FRAUD)
y <- fdsTe$FRAUD


# FP와 의사에 관한 실제 테스트셋 구성 
# FPFraud: FP Fraud Rate for fdsTr
FpList1<-sqldf("select CUST_ID, CLLT_FP_PRNO from cntt order by CUST_ID")
CustList1<-sqldf("select CUST_ID, FRAUD from cust0 order by CUST_ID")
temp1<-merge(x=CustList1,y=FpList1, by="CUST_ID", all.x=TRUE)
FpScore1<-sqldf("select CLLT_FP_PRNO, avg(Fraud) as FpFrScore from temp1 group by CLLT_FP_PRNO ")
temp2<-sqldf("select a.CUST_ID,a.CLLT_FP_PRNO, b.FpFrScore
             from temp1 a left outer join FpScore1 b 
             on a.CLLT_FP_PRNO=b.CLLT_FP_PRNO order by a.CUST_ID")
temp3<-sqldf("select CUST_ID, avg(FpFrScore) as FpFrScore from temp2 group by CUST_ID")
cust0<-merge(x=cust0,y=temp3, by="CUST_ID", all.x=TRUE)


# FPFraud: FP Fraud Rate for fdsTe
CustList1<-sqldf("select CUST_ID from xTest order by CUST_ID")
temp1<-merge(x=CustList1,y=FpList1, by="CUST_ID", all.x=TRUE)
temp2<-sqldf("select a.CUST_ID,a.CLLT_FP_PRNO, b.FpFrScore
             from temp1 a left outer join FpScore1 b 
             on a.CLLT_FP_PRNO=b.CLLT_FP_PRNO order by a.CUST_ID")
FpScore1<-sqldf("select CUST_ID, avg(FpFrScore) as FpFrScore from temp2 group by CUST_ID")
xTest<-merge(x=xTest,y=FpScore1, by="CUST_ID", all.x=TRUE)

# 사기자인 의사 찾기
#DOCFraud : DOC Fraud Rate for fdsTr
DocList1<-sqldf("select CUST_ID, CHME_LICE_NO from claim order by CUST_ID")
CustList1<-sqldf("select CUST_ID, FRAUD from cust0 order by CUST_ID")
temp1<-merge(x=CustList1,y=DocList1, by="CUST_ID", all.x=TRUE)
DocScore1<-sqldf("select CHME_LICE_NO, avg(Fraud) as DocFrScore from temp1 group by CHME_LICE_NO")
temp2<-sqldf("select a.CUST_ID,a.CHME_LICE_NO, b.DocFrScore
             from temp1 a left outer join DocScore1 b 
             on a.CHME_LICE_NO=b.CHME_LICE_NO order by a.CUST_ID")
temp3<-sqldf("select CUST_ID, avg(DocFrScore) as DocFrScore from temp2 group by CUST_ID")
cust0<-merge(x=cust0,y=temp3, by="CUST_ID", all.x=TRUE)


#DOCFraud : DOC Fraud Rate for fdsTe
CustList1<-sqldf("select CUST_ID from xTest order by CUST_ID")
temp1<-merge(x=CustList1,y=DocList1, by="CUST_ID", all.x=TRUE)
temp2<-sqldf("select a.CUST_ID,a.CHME_LICE_NO, b.DocFrScore
             from temp1 a left outer join DocScore1 b 
             on a.CHME_LICE_NO=b.CHME_LICE_NO order by a.CUST_ID")
DocScore1<-sqldf("select CUST_ID, avg(DocFrScore) as DocFrScore from temp2 group by CUST_ID")
xTest<-merge(x=xTest,y=DocScore1, by="CUST_ID", all.x=TRUE)



##FP 와 DOC 점수화에 결측치인 경우 회귀 직선의 평균을 적용하여 결측치 처리
xTest$FpFrScore<-ifelse(is.na(xTest$FpFrScore)==TRUE,-0.009617+0.733957*xTest$DocFrScore,xTest$FpFrScore)
xTest$DocFrScore<-ifelse(is.na(xTest$DocFrScore)==TRUE,0.07591+0.66526*xTest$FpFrScore,xTest$DocFrScore)

cust0[,49]<-ifelse(is.na(cust0[,49])==TRUE,mean(cust0[,49],na.rm=TRUE),cust0[,49])



# Random Forest Model
#결측치 처리
library(missForest)
fds.imp <- missForest(fdsTr)
x.imp<-missForest(x)
xTest.imp<-missForest(xTest)
fds.tr0<-as.data.frame(fds.imp[1])
ximp<-as.data.frame(x.imp[1])
xTestimp<-as.data.frame(xTest.imp[1])

library(randomForest)



#모델 1
rf1<-randomForest(FRAUD~ FpFrScore+DocFrScore,data=fdsTr, ntree=400, proximity=TRUE,keep.forest=TRUE)
pred1<-predict(rf1,x,type="prob")
RFResult1<-predict(rf1, xTest,type="prob")
#table(pred1,y)

#모델 2
rf2 <- randomForest(ximp.FRAUD ~ ximp.SEX+ximp.AGE+ximp.RESI_COST+ximp.RESI_TYPE_SCORE
                    +ximp.FP_CAREER+ximp.CTPR_SCORE+ximp.OCCP_GRP_SCORE+ximp.CHLD_SCORE
                    +ximp.MAIN_INSR_AMT+ximp.SUM_ORIG_PREM
                    +ximp.CUST_ROLE_SCORE+ximp.CUST_GOOD_SCORE+ximp.CUST_CHNL_SCORE
                    +ximp.CUST_CYCL_SCORE+ximp.CUST_RESN_SCORE+ximp.VLID_HOSP_OTDA
                    +ximp.CUST_ACCI_SCORE+ximp.CUST_HSPT_SCORE+ximp.CUST_PMMI_SCORE
                    +ximp.CUST_HEED_SCORE+ximp.CAUSE_SCORE+ximp.HOSPEC_SCORE+ximp.CLAIM_SCORE+ximp.LATE_SCORE, data=fds.tr0, ntree=400, proximity=TRUE,keep.forest=TRUE)
pred2<-predict(rf2,ximp,type="prob")
RFResult2<-predict(rf2, xTestimp,type="prob")
#table(pred2,y)



## 계층모델 
rfOut<-as.data.frame(cbind(pred1,pred2,y))
names(rfOut)<-paste(c("pred10","pred11","pred20","pred21","y"))
rfOut$y<-rfOut$y-1
rfOut$pred30<-ifelse(is.na(rfOut$pred10)==FALSE,(rfOut$pred10+rfOut$pred20)/2,rfOut$pred20)
rfOut$pred31<-ifelse(is.na(rfOut$pred11)==FALSE,(rfOut$pred11+rfOut$pred21)/2,rfOut$pred21)
rfOut$pred<-ifelse(rfOut$pred31>0.55,1,0)
rfOut$predTest<-ifelse(rfOut$pred21>0.5,1,0)
table(rfOut$pred,rfOut$y)
table(rfOut$predTest,rfOut$y)
rfOut$predf<-ifelse(((is.na(rfOut$pred10)==FALSE)&(rfOut$pred31>0.89)|(is.na(rfOut$pred10)==FALSE&(rfOut$pred31>0.60))),1,0)
table(rfOut$pred,rfOut$y)
table(rfOut$predf,rfOut$y)

rfOut$pred<-ifelse(rfOut$pred31>=rfOut$pred30,rfOut$pred31,-rfOut$pred30)
rfOut$predTest<-ifelse(rfOut$pred21>=rfOut$pred20,rfOut$pred21,-rfOut$pred20)
rfOut$pred3<-ifelse(rfOut$pred+rfOut$predTest>=0,1,0)
table(rfOut$pred3,rfOut$y)


rfOut1<-as.data.frame(cbind(RFResult1,RFResult2))
names(rfOut1)<-paste(c("pred10","pred11","pred20","pred21"))
rfOut1$pred30<-ifelse(is.na(rfOut1$pred10)==FALSE,(rfOut1$pred10+rfOut1$pred20)/2,rfOut1$pred20)
rfOut1$pred31<-ifelse(is.na(rfOut1$pred11)==FALSE,(rfOut1$pred11+rfOut1$pred21)/2,rfOut1$pred21)
rfOut1$pred<-ifelse(rfOut1$pred31>0.55,1,0)
rfOut1$predTest<-ifelse(rfOut1$pred21>0.5,1,0)
rfOut1$pred<-ifelse(rfOut1$pred31>=rfOut1$pred30,rfOut1$pred31,-rfOut1$pred30)
rfOut1$predTest<-ifelse(rfOut1$pred21>=rfOut1$pred20,rfOut1$pred21,-rfOut1$pred20)
rfOut1$pred3<-ifelse(rfOut1$pred+rfOut1$predTest>=0,1,0)
xTest$SIU_CUST_YN<-rfOut1$pred3

write.csv(rfOut1,"C:\\Users\\Dongsung\\Desktop\\공모전\\challenge_data\\result.csv")
