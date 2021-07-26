 !  PSSBaseline.f90 
!
!  FUNCTIONS:
!  PSSBaseline - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: PSSBaseline
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

 MODULE Variables

      IMPLICIT NONE

!     PARAMETERS OF THE MODEL:

      REAL(8), PARAMETER    :: Toler = 1.0e-8   ! Tolerance
      INTEGER, PARAMETER    :: MaxIter = 100000   ! Tolerance

      REAL(8), PARAMETER    :: Freq = 12.0   ! How Many Meetings per year?
      REAL(8), PARAMETER    :: VisitMintProb   = 1.00          ! Prob of visiting mints
      REAL(8), PARAMETER    :: MeetingProb   = 1.00  
      INTEGER, PARAMETER    :: AgStock   = 70           ! Total Stock of Silver
      INTEGER, PARAMETER    :: NumDeno   = 4            ! # of types of coins
      INTEGER, PARAMETER    :: NumSpec   = 2            ! # of specializations
      INTEGER, PARAMETER    :: Deno(NumDeno)=(/1,2,4,12 /)    ! The donomination structure
      INTEGER, PARAMETER    :: JeweSize  = 60     ! The silver content in each piece of jewelry
      REAL(8), PARAMETER    :: SmallUnit  = 1.0   !  Actuall size of the smallest coin (<=1)
      !INTEGER, PARAMETER    :: SilvCoinUB  = 200 
      INTEGER, PARAMETER    :: SilvCoinUBBuyer  = 60
      INTEGER, PARAMETER    :: SilvCoinUBSeller = 60
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !INTEGER, PARAMETER    :: NumPortfo  = 1               ! Total Number of possible portfolios
      INTEGER, PARAMETER    :: NumPortfoB  = 595!297!306              ! Total Number of possible portfolios
      INTEGER, PARAMETER    :: NumPortfoS  = 211!106!106              ! Total Number of possible portfolios
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
      REAL(8), PARAMETER    :: Beta          = 0.9**(1.0/Freq)        ! Discount Rate
      REAL(8), PARAMETER    :: MultiCoef     = 2.0        !  
      REAL(8), PARAMETER    :: HoProCoef     = 1.0        ! Alternative utility function u(x)=MultiCoef*LOG(x+HoProCoef)
      REAL(8), PARAMETER    :: TechoCoef     = 1.0 
      REAL(8), PARAMETER    :: Sigma     = 0.5       ! CRRA coefficient on consumption utility
      REAL(8), PARAMETER    :: UtilEps   = 0.0001
      REAL(8), PARAMETER    :: Epsilon   = 0.01/Freq     ! Normalization coefficient on jewelry utility
      REAL(8), PARAMETER    :: JeweCoef  = REAL(JeweSize)*SmallUnit!10.0
      REAL(8), PARAMETER    :: SigmaJewe = 0.5!Sigma        ! CRRA coefficient on jewelry utility
      REAL(8)               :: CarryCost(NumDeno)       ! Per unit mingting cost and carrying cost of coins
       
      INTEGER, PARAMETER    :: UpBound = 3 *AgStock      ! Maximum Silver Holdings (in any form)      
      !INTEGER, PARAMETER    :: UBVec(NumDeno+1)= (/2,2,2,2,2,5,2/)
      INTEGER, PARAMETER    :: UBVecB(NumDeno+1)= (/ 2,2,3,4,3/)!(/ 2,2,3,4,2/)
      INTEGER, PARAMETER    :: UBVecS(NumDeno+1)= (/ 1,1,2,4,3/)!(/ 1,1,2,4,2/)
      INTEGER, PARAMETER    :: MaxPay = 15
      
      REAL(8), PARAMETER    :: LucasTax = -0.00 
      INTEGER, PARAMETER    :: StoreResults = 0 ! 1=yes, 0=no 
      
      REAL(8), PARAMETER    :: PriceGridSize = 0.05
      INTEGER, PARAMETER    :: PriceGridNumber = 20
      ! Variables to be used
 
      INTEGER               :: BuyerID, SellerID, BuyerIDNew, SellerIDNew,MintPofoBuyer,MintPofoSeller,BuyerWlth,SellerWlth
      INTEGER               :: BuyerCoin(NumDeno), SellerCoin(NumDeno), TradeCoin(NumDeno)  , BPostWealth, SPostWealth
      REAL(8)               :: TradeGood,TradeGoodSec, TradeObj,TradeObjSec,MintObj,MintObjBuyer,MintObjSeller,MintGain,MintGainBuyer,MintGainSeller, DV_B, DV_S, BuyerGain 
      INTEGER               :: ZerosVec(1:NumDeno)
      INTEGER               :: IndxM2VB(0:UBVecB(1),0:UBVecB(2),0:UBVecB(3),0:UBVecB(4),0:UBVecB(5) ) ! Index
      INTEGER               :: IndxM2VS(0:UBVecS(1),0:UBVecS(2),0:UBVecS(3),0:UBVecS(4),0:UBVecS(5) )
      
      INTEGER               :: NumPortfoCheckB,NumPortfoCheckS                ! Total Number of possible portfolios
      
      REAL(8), DIMENSION(1:NumPortfoB)   ::   BuyerDist, BuyerDistLocal !, MidDistNew         ! Distributions
      REAL(8), DIMENSION(1:NumPortfoS)   ::   SellerDist, SellerDistLocal
      
      REAL(8), DIMENSION(0:UpBound)   :: BeginDist, BeginDistNew,BeginDistLocal
      
      REAL(8), DIMENSION(1:NumPortfoB)   ::   BuyerValueLocal , BuyerValue,  BuyerValueNew 
      REAL(8), DIMENSION(1:NumPortfoS)   ::   SellerValueLocal, SellerValue, SellerValueNew
 
      REAL(8), DIMENSION(0:UpBound)   :: BeginValue
      
      REAL(8), DIMENSION(NumDeno)  :: CoinCirculation,CoinCirculationLocal
 
      REAL(8)   :: MintVol(1:NumDeno), MintVolLocal(1:NumDeno),MintVolIndivB(1:NumDeno),MintVolIndivS(1:NumDeno),AvgPaymt,&
          &AvgPaymtLocal,TotOutput,TotOutputLocal,Mass ,MassLocal,AvgPrice,AvgPriceLocal,Welfare,WelfareLocal,Util,Disutil!, TotOutput, , AvgPrice
      
      INTEGER, DIMENSION(0:UpBound)       :: MintPofoChoiceB,MintPofoChoiceS
      
      !REAL(8), DIMENSION(1:NumPortfo,1:NumDeno)   :: MintVolMat,MeltVolMat
      
      INTEGER, DIMENSION(1:NumPortfoB,1:(NumDeno+1))   :: PortfoB                            
      INTEGER, DIMENSION(1:NumPortfoS,1:(NumDeno+1))   :: PortfoS 
      
      INTEGER, DIMENSION(1:NumPortfoB)       :: PortfoSilvB
      INTEGER, DIMENSION(1:NumPortfoS)       :: PortfoSilvS
      
      REAL(8), DIMENSION(1:NumPortfoB)       ::  JeweUtilB!,CarryCostVecB
      REAL(8), DIMENSION(1:NumPortfoS)       ::  JeweUtilS!,CarryCostVecS! , DMGain, MintCostVec  
       
      REAL(8)               :: DiffBD, DiffMD, DiffBV, DiffBuyV,DiffSelV,   TotMint, TotMelt  
      REAL(8)               :: DistaBD, DistaMD, DistaBV, DistaMV 
      !REAL(8)               :: LOGMV(MaxIter+1), LOGBV(MaxIter+1)
      
      INTEGER, DIMENSION(0:UpBound)       :: IsoWlthPofoNumB,IsoWlthPofoNumS
      INTEGER, DIMENSION(0:UpBound,1:UpBound)       :: IsoWlthPofoListB,IsoWlthPofoListS
      LOGICAL, DIMENSION(1:NumPortfoB,1:NumPortfoS,0:MaxPay)     :: PWPaymentSpace
      INTEGER, DIMENSION(1:(MaxPay+1))       :: PWPaymentRank
      REAL(8), DIMENSION(1:(MaxPay+1))       :: PWBuyerGainRank,PWOutputRank
      INTEGER, DIMENSION(1:NumPortfoB,1:NumPortfoS,0:MaxPay,1:NumDeno)     :: PWPaymentDetail
      
      REAL(8), DIMENSION(0:MaxPay)       :: PWPaymentDist,PWPaymentDistLocal
      
      REAL(8), DIMENSION(1:PriceGridNumber)       :: PWPriceDist,PWPriceDistLocal
      
    END MODULE Variables
    
SUBROUTINE kickstart

USE Variables

IMPLICIT NONE

INTEGER                  :: i, ipny,itwp,igrt,itst,k,bid,sid,pmtqt(0:MaxPay),buypqt(0:MaxPay),selpqt(0:MaxPay),sprice
 
!IsoWlthPofoNumB=0; IsoWlthPofoListB=0
!PortfoB = 0
NumPortfoCheckB = 0            !  First we count how many possible portfolios there are.
DO k=0,UBVecB(5) 
 DO itst=0,UBVecB(4)
  DO igrt=0,UBVecB(3)   
   DO itwp=0,UBVecB(2) 
    DO ipny=0,UBVecB(1) 
            
         IF ((DOT_PRODUCT((/Deno,JeweSize/),(/ipny,itwp,igrt,itst,k/))<=UpBound).AND.(DOT_PRODUCT((/ipny,itwp,igrt,itst/),Deno)<=SilvCoinUBBuyer)) THEN
            NumPortfoCheckB = NumPortfoCheckB + 1
            IndxM2VB(ipny,itwp,igrt,itst,k) = NumPortfoCheckB
            
            !PortfoB(IndxM2VB(ibfar,ifar,ihpn,ipny,itwp,igrt,k),:) = (/ibfar,ifar,ihpn,ipny,itwp,igrt,k/)
            !IsoWlthPofoNumB(DOT_PRODUCT((/ibfar,ifar,ihpn,ipny,itwp,igrt,k/),(/Deno,JeweSize/))) = IsoWlthPofoNumB(DOT_PRODUCT((/ibfar,ifar,ihpn,ipny,itwp,igrt,k/),(/Deno,JeweSize/))) + 1
            !IsoWlthPofoListB(DOT_PRODUCT((/ibfar,ifar,ihpn,ipny,itwp,igrt,k/),(/Deno,JeweSize/)),IsoWlthPofoNumB(DOT_PRODUCT((/ibfar,ifar,ihpn,ipny,itwp,igrt,k/),(/Deno,JeweSize/)))) = IndxM2VB(ibfar,ifar,ihpn,ipny,itwp,igrt,k)
         END IF
      
    END DO
   END DO
  END DO
 END DO
END DO

!IsoWlthPofoNumS=0; IsoWlthPofoListS=0
!PortfoS = 0
NumPortfoCheckS = 0            !  First we count how many possible portfolios there are.
DO k=0,UBVecS(5) 
 DO itst=0,UBVecS(4)
  DO igrt=0,UBVecS(3)   
   DO itwp=0,UBVecS(2) 
    DO ipny=0,UBVecS(1) 
            
         IF ((DOT_PRODUCT((/Deno,JeweSize/),(/ipny,itwp,igrt,itst,k/))<=UpBound).AND.(DOT_PRODUCT((/ipny,itwp,igrt,itst /),Deno)<=SilvCoinUBSeller)) THEN
            NumPortfoCheckS = NumPortfoCheckS + 1
            IndxM2VS(ipny,itwp,igrt,itst,k) = NumPortfoCheckS
            
            !PortfoS(IndxM2VS(ibfar,ifar,ihpn,ipny,itwp,igrt,k),:) = (/ibfar,ifar,ihpn,ipny,itwp,igrt,k/)
            !IsoWlthPofoNumS(DOT_PRODUCT((/ibfar,ifar,ihpn,ipny,itwp,igrt,k/),(/Deno,JeweSize/))) = IsoWlthPofoNumS(DOT_PRODUCT((/ibfar,ifar,ihpn,ipny,itwp,igrt,k/),(/Deno,JeweSize/))) + 1
            !IsoWlthPofoListS(DOT_PRODUCT((/ibfar,ifar,ihpn,ipny,itwp,igrt,k/),(/Deno,JeweSize/)),IsoWlthPofoNumS(DOT_PRODUCT((/ibfar,ifar,ihpn,ipny,itwp,igrt,k/),(/Deno,JeweSize/)))) = IndxM2VS(ibfar,ifar,ihpn,ipny,itwp,igrt,k)
         END IF
       
    END DO
   END DO
  END DO
 END DO
END DO
 

IF ((NumPortfoCheckB/=NumPortfoB).OR.(NumPortfoCheckS/=NumPortfoS)) THEN
    WRITE(*,*) 'NumPortfo should be:',NumPortfoCheckB,NumPortfoCheckS
    STOP 'Please correct'
END IF
 
IsoWlthPofoNumB=0; IsoWlthPofoListB=0
PortfoB = 0
!NumPortfoCheckB = 0;NumPortfoCheckB = 0           !  First we count how many possible portfolios there are.
DO k=0,UBVecB(5) 
 DO itst=0,UBVecB(4)
  DO igrt=0,UBVecB(3)   
   DO itwp=0,UBVecB(2) 
    DO ipny=0,UBVecB(1) 
            
         IF ((DOT_PRODUCT((/Deno,JeweSize/),(/ipny,itwp,igrt,itst,k/))<=UpBound).AND.(DOT_PRODUCT((/ipny,itwp,igrt,itst/),Deno)<=SilvCoinUBBuyer)) THEN
           ! NumPortfoCheckB = NumPortfoCheckB + 1
           ! IndxM2VB(ibfar,ifar,ihpn,ipny,itwp,igrt,k) = NumPortfoCheckB
            
            PortfoB(IndxM2VB(ipny,itwp,igrt,itst,k),:) = (/ipny,itwp,igrt,itst,k/)
            IsoWlthPofoNumB(DOT_PRODUCT((/ipny,itwp,igrt,itst,k/),(/Deno,JeweSize/))) = IsoWlthPofoNumB(DOT_PRODUCT((/ipny,itwp,igrt,itst,k/),(/Deno,JeweSize/))) + 1
            IsoWlthPofoListB(DOT_PRODUCT((/ipny,itwp,igrt,itst,k/),(/Deno,JeweSize/)),IsoWlthPofoNumB(DOT_PRODUCT((/ipny,itwp,igrt,itst,k/),(/Deno,JeweSize/)))) = IndxM2VB(ipny,itwp,igrt,itst,k)
         END IF
       
    END DO
   END DO
  END DO
 END DO
END DO

IsoWlthPofoNumS=0; IsoWlthPofoListS=0
PortfoS = 0
!NumPortfoCheckS = 0;NumPortfoCheckS = 0           !  First we count how many possible portfolios there are.
DO k=0,UBVecS(5) 
 DO itst=0,UBVecS(4)
  DO igrt=0,UBVecS(3)   
   DO itwp=0,UBVecS(2) 
    DO ipny=0,UBVecS(1) 
            
         IF ((DOT_PRODUCT((/Deno,JeweSize/),(/ipny,itwp,igrt,itst,k/))<=UpBound).AND.(DOT_PRODUCT((/ipny,itwp,igrt,itst /),Deno)<=SilvCoinUBSeller)) THEN
            !NumPortfoCheckS = NumPortfoCheckS + 1
            !IndxM2VS(ibfar,ifar,ihpn,ipny,itwp,igrt,k) = NumPortfoCheckS
            
            PortfoS(IndxM2VS(ipny,itwp,igrt,itst,k),:) = (/ipny,itwp,igrt,itst,k/)
            IsoWlthPofoNumS(DOT_PRODUCT((/ipny,itwp,igrt,itst,k/),(/Deno,JeweSize/))) = IsoWlthPofoNumS(DOT_PRODUCT((/ipny,itwp,igrt,itst,k/),(/Deno,JeweSize/))) + 1
            IsoWlthPofoListS(DOT_PRODUCT((/ipny,itwp,igrt,itst,k/),(/Deno,JeweSize/)),IsoWlthPofoNumS(DOT_PRODUCT((/ipny,itwp,igrt,itst,k/),(/Deno,JeweSize/)))) = IndxM2VS(ipny,itwp,igrt,itst,k)
         END IF
  
    END DO
   END DO
  END DO
 END DO
END DO 


IF ((SUM(IsoWlthPofoNumB)/=NumPortfoB).OR.(SUM(IsoWlthPofoNumS)/=NumPortfoS)) THEN
    STOP 'sth is wrong'
END IF
 
PortfoSilvB = MATMUL(PortfoB,(/Deno,JeweSize/))
!JeweUtilB   = Epsilon*(((REAL(PortfoB(:,NumDeno+1))*REAL(JeweSize))*SmallUnit+UtilEps)**(1.0-SigmaJewe)-(UtilEps)**(1.0-SigmaJewe))/(1.0-SigmaJewe)
 JeweUtilB   = Epsilon *JeweCoef* REAL(PortfoB(:,NumDeno+1)) 

PortfoSilvS = MATMUL(PortfoS,(/Deno,JeweSize/))
!JeweUtilS   = Epsilon*(((REAL(PortfoS(:,NumDeno+1))*REAL(JeweSize))*SmallUnit+UtilEps)**(1.0-SigmaJewe)-(UtilEps)**(1.0-SigmaJewe))/(1.0-SigmaJewe)
 JeweUtilS   = Epsilon *JeweCoef* REAL(PortfoS(:,NumDeno+1)) 

DO k=0,UBVecB(5) 
 DO itst=0,UBVecB(4)
  DO igrt=0,UBVecB(3)   
   DO itwp=0,UBVecB(2) 
    DO ipny=0,UBVecB(1) 
            
            IF ((DOT_PRODUCT((/Deno,JeweSize/),(/ipny,itwp,igrt,itst,k/))<=UpBound).AND.(DOT_PRODUCT((/ipny,itwp,igrt,itst/),Deno)<=SilvCoinUBBuyer)) THEN
                
                BuyerValueNew(IndxM2VB(ipny,itwp,igrt,itst,k)) =2.0*LOG(PortfoSilvB(IndxM2VB(ipny,itwp,igrt,itst,k))*SmallUnit+1)  &
                    & + JeweUtilB(IndxM2VB(ipny,itwp,igrt,itst,k)) - 1.0* DOT_PRODUCT(REAL((/ipny,itwp,igrt,itst/)),CarryCost)
                
            END IF
  
    END DO
   END DO
  END DO
 END DO
END DO
  BuyerValueNew(1)=0.0
DO k=0,UBVecS(5) 
 DO itst=0,UBVecS(4)
  DO igrt=0,UBVecS(3)   
   DO itwp=0,UBVecS(2) 
    DO ipny=0,UBVecS(1) 
            
            IF ((DOT_PRODUCT((/Deno,JeweSize/),(/ipny,itwp,igrt,itst,k/))<=UpBound).AND.(DOT_PRODUCT((/ipny,itwp,igrt,itst/),Deno)<=SilvCoinUBSeller)) THEN
                
                SellerValueNew(IndxM2VS(ipny,itwp,igrt,itst,k)) =2.0*LOG(PortfoSilvS(IndxM2VS(ipny,itwp,igrt,itst,k))*SmallUnit+1)  &
                    & + JeweUtilS(IndxM2VS(ipny,itwp,igrt,itst,k)) - 1.0* DOT_PRODUCT(REAL((/ipny,itwp,igrt,itst/)),CarryCost)
                
            END IF
  
    END DO
   END DO
  END DO
 END DO
END DO

 SellerValueNew(1)=0.0
  
DO i=1,UpBound

    BeginValue(i) = 2.0*LOG(REAL(DOT_PRODUCT((/ipny,itwp,igrt,itst/),Deno))*SmallUnit+1)

END DO
     
BeginDist = 0.0
! BeginDist(IndxM2V(FLOOR(AgStock/Deno(1)),0,0)) = 1.0-AgStock/Deno(1)+FLOOR(AgStock/Deno(1))
! BeginDist(IndxM2V(FLOOR(AgStock/Deno(1))+1,0,0)) =  AgStock/Deno(1)-FLOOR(AgStock/Deno(1))
BeginDist(AgStock) = 1.0
!BeginDist(IndxM2V(0,FLOOR(AgStock/Deno(2)),0)) = 1.0-AgStock/Deno(2)+FLOOR(AgStock/Deno(2))
!BeginDist(IndxM2V(0,FLOOR(AgStock/Deno(2))+1,0)) =  AgStock/Deno(2)-FLOOR(AgStock/Deno(2))
BeginDistNew = BeginDist!; !MidDist = BeginDist!; MidDistNew = BeginDist
PWPaymentSpace = .FALSE.
PWPaymentSpace(:,:,0)=.TRUE.

DO bid=1,NumPortfoB
  IF (MAXVAL(PortfoB(bid,1:NumDeno))>0) THEN 
    DO sid=1,NumPortfoS
      pmtqt = 1000;buypqt=1000;selpqt=1000;sprice=-100 ! number of coins involved in the payment  
      DO ipny = ((-1)*PortfoS(sid,1)),PortfoB(bid,1)
       DO itwp = ((-1)*PortfoS(sid,2)),PortfoB(bid,2)
        DO igrt = ((-1)*PortfoS(sid,3)),PortfoB(bid,3)
         DO itst = ((-1)*PortfoS(sid,4)),PortfoB(bid,4) 
            IF ((DOT_PRODUCT((/ipny,itwp,igrt,itst/),Deno)>=0).AND.(DOT_PRODUCT((/ipny,itwp,igrt,itst/),Deno)<=MaxPay).AND.&
                &(DOT_PRODUCT((PortfoB(bid,1:NumDeno) - (/ipny,itwp,igrt,itst/)),Deno) + JeweSize*PortfoB(bid,NumDeno+1)<=UpBound).AND.&
                &(DOT_PRODUCT((PortfoS(sid,1:NumDeno) + (/ipny,itwp,igrt,itst/)),Deno) + JeweSize*PortfoS(sid,NumDeno+1)<=UpBound) ) THEN
                PWPaymentSpace(bid,sid,DOT_PRODUCT((/ipny,itwp,igrt,itst/),Deno))  = .TRUE.  ! it's a valid transfer
                
           
                IF (SUM(ABS((/ipny,itwp,igrt,itst/)))<pmtqt(DOT_PRODUCT((/ipny,itwp,igrt,itst/),Deno))) THEN
                    pmtqt(DOT_PRODUCT((/ipny,itwp,igrt,itst/),Deno)) = SUM(ABS((/ipny,itwp,igrt,itst/)))
                    PWPaymentDetail(bid,sid,DOT_PRODUCT((/ipny,itwp,igrt,itst/),Deno),:)=(/ipny,itwp,igrt,itst/)
                END IF
                    
              
                
            END IF
           
         END DO
        END DO
       END DO
      END DO
    END DO
  END IF    
END DO
 
 END SUBROUTINE kickstart
    
MODULE  DMMappings
    
    use Variables
    IMPLICIT  NONE
    
    CONTAINS
   
   SUBROUTINE pairoutput(V, SW, BW, K, Xlist, Glist,Ylist) ! This subroutine computes the terms of trade between any given seller-buyer pair
                                                       ! if buyers get all the surplus from trade

      IMPLICIT NONE
 
      REAL(8), DIMENSION(0:UpBound),      INTENT(IN)     :: V   ! value function after pairwise meeting
      INTEGER,                            INTENT(IN)     :: SW, BW& ! money holdings of the seller/buyer
                                                         &, K  ! maximum possible money transfer between the pair
      INTEGER, DIMENSION(1:(MaxPay+1)),     INTENT(OUT)    :: Xlist  ! the probability measure to represent the lottery money payment
      REAL(8), DIMENSION(1:(MaxPay+1)),     INTENT(OUT)    :: Glist,Ylist 
      
      INTEGER                                            :: PCount ! To count all possible money transfer
      REAL(8), DIMENSION(0:MIN(K,MaxPay))                    :: DiffVS,DiffVB& ! Buyer's and Seller's change of value under different determinant money payment
                                                          &,TotDiffVB  ! Buyer's surplus from trade
      
      REAL(8)                                            ::  PreviousMax 
      INTEGER                                            :: OPDDisc,ccc
       

      DO PCount= 0, MIN(K,MaxPay)
          
          DiffVS(PCount) = Beta*(V(SW+PCount) - V(SW))
          DiffVB(PCount) = Beta*(V(BW-PCount) - V(BW))
          
      END DO
      
      Xlist = -1  
      
      TotDiffVB = DiffVB + ((TechoCoef*DiffVS+UtilEps)**(1.0-Sigma)-(UtilEps)**(1.0-Sigma))/(1.0-Sigma)
      !TotDiffVB = DiffVB + MultiCoef*(LOG(TechoCoef*DiffVS+HoProCoef)-LOG(HoProCoef)) 
      
      
      OPDDisc = MAXLOC(TotDiffVB,1) - 1 !  OPDDisc is the optimal money payment 
      
      PreviousMax =  MAXVAL(TotDiffVB,1)+1.0
      ccc = 1
      DO PCount= 0, MIN(K,MaxPay)
          
          Xlist(ccc) = MAXLOC(TotDiffVB,1,TotDiffVB<PreviousMax) - 1
          Glist(ccc) = MAXVAL(TotDiffVB,1,TotDiffVB<PreviousMax) 
          Ylist(ccc) = TechoCoef * DiffVS(Xlist(ccc))
          PreviousMax =  MAXVAL(TotDiffVB,1,TotDiffVB<PreviousMax) 
          ccc=ccc+1
      END DO
      !IF (Xlist(1)==MaxPay) WRITE(*,*) 'MaxPay'
     
   END SUBROUTINE pairoutput
   
 END MODULE DMMappings
     
 program PSSBaseline
 
  USE Variables
  USE DMMappings
  
  
IMPLICIT NONE

!-----------------------------------------------------------------------------
! Specifications
INTEGER                  ::  ipny,itwp,igrt,itst,k,i,j,i_pf,j_pf,i_den,Iter,b_pf,s_pf,pmtid,curroppmt,pmtcheck
REAL                     :: time_begin, time_end 
REAL                     :: check_lhs, check_rhs
REAL(8)                  :: RHSTemp(1:1,1:1),smallcoinsilver,tradegoodaccum,coinutilaccum,densityaccum,tempobj(5)
INTEGER                  :: cnt,cntd,maxct,maxuno,maxdos,maxtres,maxcuatro,maxcinco,maxseis,maxsiete, maxunodos
!!!!!!!!!!!
!REAL(8) :: tempobj(3),tempcoeflt(1:3,1:3),templtrhs(3),tempcoefe2(1,1:3), tempe2rhs(1),tempup(3),templw(3),tempx(3),tempfv, jingu, gujin(1,1:3)
!!!!!!!


CALL CPU_TIME(time_begin)
!-----------------------------------------------------------------------------
! Necessary house-keeping
 

RHSTemp = 1.0
   
 !MintCost  = 1.0*Epsilon*((0.04*SQRT(REAL(Deno)*SmallUnit)+UtilEps)**(1.0-SigmaJewe)-(UtilEps)**(1.0-SigmaJewe))/(1.0-SigmaJewe)
 !CarryCost = 0.1*Epsilon*((0.0025*(REAL(Deno(2))*SmallUnit)+UtilEps)**(1.0-SigmaJewe)-(UtilEps)**(1.0-SigmaJewe))/(1.0-SigmaJewe)
 CarryCost = 1.0e-5 

CALL kickstart
 
!-----------------------------------------------------------------------------
 
!WRITE(*,*) NumPortfoB,NumPortfoS

!WRITE(*,*) shape(Portfo)

WRITE(*,*) shape(BeginValue)

!WRITE(*,*) BeginValue
smallcoinsilver = REAL(Deno(1))*SmallUnit
WRITE(*,*) 'CC is: ',CarryCost
!WRITE(*,*) 'MC is: ',MintCost
!WRITE(*,*) 'JC is: ',JeweConsump(Deno(1)),JeweConsump(Deno(1))/(1.0-Beta)
!WRITE(*,*) check_lhs,'>=',check_rhs

WRITE(*,*) 'M is: ',DOT_PRODUCT((/(i,i=0, UpBound)/),BeginDist) 
!WRITE(*,*) 'UPBound is'
!WRITE(*,*) UBVec
DiffBV = 1.0;  DiffBD = 1.1 ; DiffBuyV = 1.1;DiffSelV = 1.1;  DiffMD = 1.0 
!CarryCostVec = MATMUL(Portfo(:,1:NumDeno),CarryCost)
!WRITE(*,*) JeweUtil
 
 
!-----------------------------------------------------------------------------
! Now we start the iterations
 open (unit = 1 , file = "RunTime_Log")
!open (unit = 2 , file = "MidValuebb")
!open (unit = 3 , file = "BeginDistbb")
 
 ! BeginValueNew = BeginValueOld; MidValueNew = MidValueOld
 ! BeginDistNew  = BeginDistOld;  MidDistNew  = MidDistOld
Iter = 0
 
DO WHILE (MAX(DiffBD,DiffBuyV,DiffSelV)>Toler) !((DiffBV>Toler).OR.(DiffMV>Toler).OR.(DiffBD>Toler).OR.(DiffMD>Toler))!
    IF (Iter>MaxIter) THEN
        WRITE(*,*) 'Failed to converge after',MaxIter,'iterations'
        EXIT
    END IF
    !MintPofoChoiceOld = MintPofoChoice
    !MidDistOld=MidDist  
  
    BeginDist  = BeginDistNew; BuyerValue = BuyerValueNew; SellerValue = SellerValueNew
    ! BeginValue = BeginValueNew; MidDist  = MidDistNew
    MintGainBuyer  = 0.0 ; MintGainSeller  = 0.0
    !TrsnMatMint = 0.0;  TrsnMatDM = IdenMat;    DMGain = 0.0;   
    !DMPayMat = 0.0;     DMYMat    = 0.0;        DMCoinMat = 0
    !IndivMint =0.0;     IndivMelt   = 0.0
    
!-----------------------------------------------------------------------------
! Firstly we deal with the minting phase
    !MidDist = 0.0
    BuyerDist=0.0; SellerDist=0.0
    !MintVol = 0.0
    !MintVolMat=0.0;MeltVolMat=0.0
    !MintPofoChoice = 0
    !$OMP PARALLEL PRIVATE(BuyerDistLocal,SellerDistLocal )
    BuyerDistLocal = 0.0; SellerDistLocal = 0.0
    !MintVolLocal = 0.0
    !$OMP DO PRIVATE(i_pf,MintObjBuyer,MintObjSeller,MintPofoBuyer,MintPofoSeller,j_pf,MintGainBuyer,MintGainSeller )
    
    DO i_pf=0,UpBound         ! For an agent with silver quant i_pf
        MintObjBuyer = 0.0;  MintPofoBuyer = 1
        MintObjSeller = 0.0; MintPofoSeller = 1
        
        Do j_pf=1,NumPortfoB
            IF (PortfoSilvB(j_pf)==(i_pf)) THEN
                MintGainBuyer  = BuyerValue(j_pf)  + JeweUtilB(j_pf) - DOT_PRODUCT(PortfoB(j_pf,1:NumDeno),CarryCost) 
                !MintGainSeller = SellerValue(j_pf) + JeweUtil(j_pf) - DOT_PRODUCT(Portfo(j_pf,1:NumDeno),CarryCost) 
                !MintGain = MidValue(j_pf) + JeweUtil(j_pf) - DOT_PRODUCT(Portfo(j_pf,1:NumDeno),CarryCost) 
                IF (MintGainBuyer>MintObjBuyer) THEN
                    MintPofoBuyer = j_pf
                    MintObjBuyer = MintGainBuyer
                END IF
                  
            END IF
            
        END DO
        MintPofoChoiceB(i_pf) = MintPofoBuyer
        
        Do j_pf=1,NumPortfoS
            IF (PortfoSilvS(j_pf)==(i_pf)) THEN
                !MintGainBuyer  = BuyerValue(j_pf)  + JeweUtil(j_pf) - DOT_PRODUCT(Portfo(j_pf,1:NumDeno),CarryCost) 
                MintGainSeller = SellerValue(j_pf) + JeweUtilS(j_pf) - DOT_PRODUCT(PortfoS(j_pf,1:NumDeno),CarryCost) 
                !MintGain = MidValue(j_pf) + JeweUtil(j_pf) - DOT_PRODUCT(Portfo(j_pf,1:NumDeno),CarryCost) 
                 
                IF (MintGainSeller>MintObjSeller) THEN
                    MintPofoSeller = j_pf
                    MintObjSeller = MintGainSeller
                END IF 
            END IF 
        END DO
  
        MintPofoChoiceS(i_pf) = MintPofoSeller 
         
        BeginValue(i_pf) =   0.5*(MintObjBuyer + MintObjSeller)!- DOT_PRODUCT(Portfo(MintPofo,1:NumDeno),CarryCost) 
        BuyerDistLocal(MintPofoBuyer)   = BuyerDistLocal(MintPofoBuyer) + BeginDist(i_pf) 
        SellerDistLocal(MintPofoSeller) = SellerDistLocal(MintPofoSeller) + BeginDist(i_pf) 
    END DO
    !$OMP END DO  
    !$OMP CRITICAL
    BuyerDist  = BuyerDist  + BuyerDistLocal
    SellerDist = SellerDist + SellerDistLocal 
    !$OMP END CRITICAL
    !$OMP END PARALLEL
    
!-----------------------------------------------------------------------------
! Secondly we deal with the trading phase  Portfo(BuyerID,1:NumDeno)  
 
    BeginDistNew=0.0
    DO j_pf = 1,NumPortfoB
        BuyerValueNew(j_pf) = Beta*BeginValue(PortfoSilvB(j_pf))
        !SellerValueNew(j_pf) = Beta*BeginValue(PortfoSilv(j_pf))
        BeginDistNew(PortfoSilvB(j_pf)) = BeginDistNew(PortfoSilvB(j_pf)) + 0.5*( BuyerDist(j_pf) )
    END DO
    
    DO j_pf = 1,NumPortfoS
        !BuyerValueNew(j_pf) = Beta*BeginValue(PortfoSilv(j_pf))
        SellerValueNew(j_pf) = Beta*BeginValue(PortfoSilvS(j_pf))
        BeginDistNew(PortfoSilvS(j_pf)) = BeginDistNew(PortfoSilvS(j_pf)) + 0.5*( SellerDist(j_pf) )
    END DO
    
     Welfare=&
     &  DOT_PRODUCT(BuyerDist, (JeweUtilB - MATMUL(PortfoB(:,1:NumDeno),CarryCost))) + &
     &  DOT_PRODUCT(SellerDist,(JeweUtilS - MATMUL(PortfoS(:,1:NumDeno),CarryCost)))
     CoinCirculation = 0.0
     AvgPaymt = 0.0
     MintVol = 0.0
     TotOutput = 0.0
     Mass = 0.0
     PWPaymentDist =0.0; PWPriceDist = 0.0
     AvgPrice = 0.0
    !$OMP PARALLEL PRIVATE(BeginDistLocal,BuyerValueLocal ,CoinCirculationLocal,AvgPaymtLocal,MintVolLocal,TotOutputLocal,MassLocal,PWPaymentDistLocal,PWPriceDistLocal,AvgPriceLocal, WelfareLocal)
    BeginDistLocal = 0.0
    BuyerValueLocal  = 0.0!; SellerValueLocal  = 0.0
    CoinCirculationLocal = 0.0
     MintVolLocal = 0.0
     AvgPaymtLocal = 0.0
     TotOutputLocal = 0.0
     MassLocal = 0.0
     AvgPriceLocal = 0.0
     PWPaymentDistLocal = 0.0; PWPriceDistLocal = 0.0
     WelfareLocal = 0.0
     
    !$OMP DO PRIVATE(BuyerWlth,SellerWlth,PWPaymentRank,PWBuyerGainRank,PWOutputRank ,i,j ,pmtid,pmtcheck,curroppmt,TradeCoin ,MintVolIndivB,MintVolIndivS,Util,Disutil)
     
    DO BuyerWlth = 1,UpBound
        
        DO SellerWlth = 0,(UpBound-1)
            ! IF (MidDist(SellerID)<1e-10) GO TO 10 
             
            CALL pairoutput(BeginValue, SellerWlth, BuyerWlth, MIN(BuyerWlth,(UpBound-SellerWlth)) , PWPaymentRank,PWBuyerGainRank,PWOutputRank )
            
            DO i = 1,IsoWlthPofoNumB(BuyerWlth)
              DO j= 1,IsoWlthPofoNumS(SellerWlth)
                 pmtid = 0 ;pmtcheck=0
                 DO WHILE (pmtcheck==0)  ! The pmtid-th optimal payment is not feasible
                     pmtid =pmtid+1
                     
                     IF (PWPaymentRank(pmtid)<=MaxPay) THEN
                         IF (PWPaymentSpace(IsoWlthPofoListB(BuyerWlth,i),IsoWlthPofoListS(SellerWlth,j),PWPaymentRank(pmtid))==.TRUE.) THEN
                             pmtcheck=1
                              
                         END IF
                     END IF
                       
                 END DO 
                
                 curroppmt = PWPaymentRank(pmtid)
                 TradeCoin = PWPaymentDetail(IsoWlthPofoListB(BuyerWlth,i),IsoWlthPofoListS(SellerWlth,j),curroppmt,:)
                  IF (curroppmt>0) THEN
                      
                      MassLocal = MassLocal + BuyerDist(IsoWlthPofoListB(BuyerWlth,i))*SellerDist(IsoWlthPofoListS(SellerWlth,j))
                      
                      AvgPriceLocal = AvgPriceLocal + BuyerDist(IsoWlthPofoListB(BuyerWlth,i))*SellerDist(IsoWlthPofoListS(SellerWlth,j))*SmallUnit*REAL(curroppmt)/PWOutputRank(pmtid)
                      
                      Util = ((PWOutputRank(pmtid)*(1.0-LucasTax)+UtilEps)**(1.0-Sigma)-(UtilEps)**(1.0-Sigma))/(1.0-Sigma)
                      Disutil = PWOutputRank(pmtid)/TechoCoef
                      WelfareLocal = WelfareLocal + (Util-Disutil)*BuyerDist(IsoWlthPofoListB(BuyerWlth,i))*SellerDist(IsoWlthPofoListS(SellerWlth,j)) 
                      PWPriceDistLocal(MIN((FLOOR(SmallUnit*REAL(curroppmt)/(PWOutputRank(pmtid)*PriceGridSize))+1),PriceGridNumber)) = &
                          & PWPriceDistLocal(MIN((FLOOR(SmallUnit*REAL(curroppmt)/(PWOutputRank(pmtid)*PriceGridSize))+1),PriceGridNumber))+&
                          & BuyerDist(IsoWlthPofoListB(BuyerWlth,i))*SellerDist(IsoWlthPofoListS(SellerWlth,j))
                  END IF
                  PWPaymentDistLocal(curroppmt) = PWPaymentDistLocal(curroppmt) + BuyerDist(IsoWlthPofoListB(BuyerWlth,i))*SellerDist(IsoWlthPofoListS(SellerWlth,j))
                  
                  AvgPaymtLocal = AvgPaymtLocal + BuyerDist(IsoWlthPofoListB(BuyerWlth,i))*SellerDist(IsoWlthPofoListS(SellerWlth,j))*SmallUnit*REAL(curroppmt)
                  CoinCirculationLocal = CoinCirculationLocal + BuyerDist(IsoWlthPofoListB(BuyerWlth,i))*SellerDist(IsoWlthPofoListS(SellerWlth,j))*REAL(abs(TradeCoin))/NumSpec
                  TotOutputLocal = TotOutputLocal + BuyerDist(IsoWlthPofoListB(BuyerWlth,i))*SellerDist(IsoWlthPofoListS(SellerWlth,j))*PWOutputRank(pmtid)/NumSpec
                 ! Buyer's minting next period, whose post-PM wealth is (BuyerWlth-curroppmt)
                  MintVolIndivB = 0.5*SmallUnit* Deno*&
                     &           (((PortfoB(MintPofoChoiceB(BuyerWlth-curroppmt),1:NumDeno)- (PortfoB(IsoWlthPofoListB(BuyerWlth,i),1:NumDeno)-TradeCoin))     &
                     &         +abs(PortfoB(MintPofoChoiceB(BuyerWlth-curroppmt),1:NumDeno)- (PortfoB(IsoWlthPofoListB(BuyerWlth,i),1:NumDeno)-TradeCoin)))/2  & ! If he's still a buyer next period
                     &         +  ((PortfoS(MintPofoChoiceS(BuyerWlth-curroppmt),1:NumDeno)- (PortfoB(IsoWlthPofoListB(BuyerWlth,i),1:NumDeno)-TradeCoin))     &
                     &         +abs(PortfoS(MintPofoChoiceS(BuyerWlth-curroppmt),1:NumDeno)- (PortfoB(IsoWlthPofoListB(BuyerWlth,i),1:NumDeno)-TradeCoin)))/2 )   ! If he's  a seller next period
                 
                 ! Seller's minting next period, whose post-PM wealth is (SellerWlth+curroppmt)
                 MintVolIndivS = 0.5*SmallUnit* Deno*&
                     &           (((PortfoS(MintPofoChoiceS(SellerWlth+curroppmt),1:NumDeno)-(PortfoS(IsoWlthPofoListS(SellerWlth,j),1:NumDeno)+TradeCoin))     &
                     &         +abs(PortfoS(MintPofoChoiceS(SellerWlth+curroppmt),1:NumDeno)-(PortfoS(IsoWlthPofoListS(SellerWlth,j),1:NumDeno)+TradeCoin)))/2  & ! If he's still a seller next period
                     &         +  ((PortfoB(MintPofoChoiceB(SellerWlth+curroppmt),1:NumDeno)-(PortfoS(IsoWlthPofoListS(SellerWlth,j),1:NumDeno)+TradeCoin))     &
                     &         +abs(PortfoB(MintPofoChoiceB(SellerWlth+curroppmt),1:NumDeno)-(PortfoS(IsoWlthPofoListS(SellerWlth,j),1:NumDeno)+TradeCoin)))/2 )   ! If he's  a buyer next period
                 
                 MintVolLocal = MintVolLocal + (MintVolIndivB+MintVolIndivS)*BuyerDist(IsoWlthPofoListB(BuyerWlth,i))*SellerDist(IsoWlthPofoListS(SellerWlth,j))/NumSpec
                 
                 BeginDistLocal(SellerWlth)  = BeginDistLocal(SellerWlth)   - BuyerDist(IsoWlthPofoListB(BuyerWlth,i))*SellerDist(IsoWlthPofoListS(SellerWlth,j))/NumSpec 
                 BeginDistLocal(BuyerWlth)   = BeginDistLocal(BuyerWlth)    - BuyerDist(IsoWlthPofoListB(BuyerWlth,i))*SellerDist(IsoWlthPofoListS(SellerWlth,j))/NumSpec 

                 BeginDistLocal(SellerWlth+curroppmt)= BeginDistLocal(SellerWlth+curroppmt) + BuyerDist(IsoWlthPofoListB(BuyerWlth,i))*SellerDist(IsoWlthPofoListS(SellerWlth,j))/NumSpec 
                 BeginDistLocal(BuyerWlth-curroppmt) = BeginDistLocal(BuyerWlth-curroppmt)  + BuyerDist(IsoWlthPofoListB(BuyerWlth,i))*SellerDist(IsoWlthPofoListS(SellerWlth,j))/NumSpec 
            
                 BuyerValueLocal(IsoWlthPofoListB(BuyerWlth,i)) = BuyerValueLocal(IsoWlthPofoListB(BuyerWlth,i)) + SellerDist(IsoWlthPofoListS(SellerWlth,j))*PWBuyerGainRank(pmtid) 
                  
                 
              END DO
            END DO
               
        END DO 
    END DO
     !$OMP END  DO
    
    !$OMP CRITICAL
    BeginDistNew = BeginDistNew + BeginDistLocal
    BuyerValueNew = BuyerValueNew + BuyerValueLocal
    
    CoinCirculation = CoinCirculation + CoinCirculationLocal
    AvgPaymt = AvgPaymt + AvgPaymtLocal
    AvgPrice = AvgPrice + AvgPriceLocal
    Mass  = Mass  + MassLocal 
    MintVol = MintVol + MintVolLocal
    TotOutput = TotOutput + TotOutputLocal
    PWPaymentDist = PWPaymentDist + PWPaymentDistLocal
    PWPriceDist = PWPriceDist + PWPriceDistLocal
    
    Welfare = Welfare + WelfareLocal
    !$OMP END CRITICAL
    !$OMP END PARALLEL 
    PWPriceDist = PWPriceDist/Mass
    AvgPaymt = AvgPaymt/Mass
    AvgPrice = AvgPrice/Mass
    TotOutput = TotOutput/Mass
    CoinCirculation = CoinCirculation/Mass
    Welfare = Welfare/(1.0-beta) 
    !MidValueNew  = MidValueNew- CarryCostVec!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
  
    DiffBD = SQRT(SUM( (BeginDistNew-BeginDist)*(BeginDistNew-BeginDist) ))
    DiffBuyV = SQRT(SUM( (BuyerValueNew-BuyerValue )*(BuyerValueNew-BuyerValue) ))
    DiffSelV = SQRT(SUM( (SellerValueNew-SellerValue )*(SellerValueNew-SellerValue ) ))
    Iter = Iter + 1
    IF (MOD(Iter,1000)==   0) THEN
        WRITE(*,*) 'Iterations passed    :', Iter
        WRITE(*,*)  DiffBD,DiffBuyV,DiffSelV
        WRITE(*,*) 'TotMint',SUM(MintVol)!,'TradeObjDiff',TradeObjDiff,Twins,DMCoinMat(Twins(1),Twins(2),:),DMCoinMatSec(Twins(1),Twins(2),:)
        !WRITE(*,*)  MintPofoChoice-MintPofoChoiceOld
        WRITE(*,*) ''
         WRITE (1,*) 'Iterations passed    :', Iter
         WRITE (1,*)  DiffBD,DiffBuyV,DiffSelV
         WRITE (1,*) ' ' 
    END IF
    !IF (Iter>500) WRITE(*,*) MintPofoChoice(38),MintPofoChoice(40),MintPofoChoice(42) 
END DO
!
! End of the iterations
!-----------------------------------------------------------------------------
 
!BeginValue = BeginValueNew;  MidDist  = MidDistNew
BeginDist  = BeginDistNew; BuyerValue = BuyerValueNew; SellerValue = SellerValueNew
!TotOutput = DOT_PRODUCT((MATMUL(MidDist,DMYMat)),MidDist)
CALL CPU_TIME(time_end)
 
  WRITE(*,*) 'TotMint1',SUM(MintVol)
MintVol = MintVol +  0.5*BeginDist(UpBound)*SmallUnit* Deno*&
 &           (((PortfoB(MintPofoChoiceB(UpBound),1:NumDeno)- PortfoS(MintPofoChoiceS(UpBound),1:NumDeno))     &
 &         +abs(PortfoB(MintPofoChoiceB(UpBound),1:NumDeno)- PortfoS(MintPofoChoiceS(UpBound),1:NumDeno)))/2  & ! If he's a buyer next period
 &         )/NumSpec   
  WRITE(*,*) 'TotMint2',SUM(MintVol)           
TotMint = SUM(MintVol)
WRITE(*,*) 'Time Elapsed    :', time_end-time_begin,' seconds'

IF (Iter<=MaxIter) THEN
    WRITE(*,*) 'Converged after :', Iter,'iterations.'
END IF

IF (StoreResults==1) THEN
  open (unit = 11 , file = "BeginValue_Temp")
  open (unit = 12 , file = "BuyerValue_Temp")
  open (unit = 13 , file = "BeginDist_Temp")
  open (unit = 14 , file = "BuyerDist_Temp")
  open (unit = 15 , file = "SellerValue_Temp")
  open (unit = 16 , file = "SellerDist_Temp") 
  open (unit = 17 , file = "MintVol_Temp")
  open (unit = 18 , file = "CoinJewelryStock_Temp")
  open (unit = 19 , file = "CoinCirc_Temp")
  open (unit = 20 , file = "PWPaymentDist_Temp")
  open (unit = 21 , file = "PWPriceDist_Temp")
 DO i=1,NumPortfoB 
     
         WRITE (12,*) BuyerValue(i)
         !WRITE (15,*) SellerValue(i)
         WRITE (14,*) BuyerDist(i)
         !WRITE (16,*) SellerDist(i)
                 
 END DO
  DO i=1,NumPortfoS 
     
         !WRITE (12,*) BuyerValue(i)
         WRITE (15,*) SellerValue(i)
         !WRITE (14,*) BuyerDist(i)
         WRITE (16,*) SellerDist(i)
                 
 END DO
 DO i=0,UpBound
         WRITE (11,*) BeginValue(i)
         !WRITE (12,*) MidValue(i)
         WRITE (13,*) BeginDist(i)
         !WRITE (14,*) MidDist(i)
        ! WRITE (28,*) MintPofoChoice(i)
                 
 END DO
 
 !WRITE (15,*) (time_end-time_begin)
 !WRITE (16,*)  Iter 
 
DO i=1,NumDeno+1
    IF (i<=(NumDeno)) THEN 
       WRITE (17,*)  MintVol(i)
     ! WRITE (18,*)  MeltVol(i)
       WRITE (18,*)  0.5*(DOT_PRODUCT(BuyerDist,PortfoB(:,i))+DOT_PRODUCT(SellerDist,PortfoS(:,i)))
       WRITE (19,*)  CoinCirculation(i)
    ELSE
       WRITE (18,*)  0.5*(DOT_PRODUCT(BuyerDist,PortfoB(:,i))+DOT_PRODUCT(SellerDist,PortfoS(:,i)))
    END IF
        
END DO
  DO i=0,MaxPay 
     
         WRITE (20,*) PWPaymentDist(i)
                 
  END DO
  
  DO i=1,PriceGridNumber 
     
         WRITE (21,*) PWPriceDist(i)
                 
  END DO
 !WRITE (19,*)  NumPortfo
END IF

 
WRITE(*,*) ''
WRITE(*,50,advance='no') NumDeno
WRITE(*,*) ''
WRITE(*,*) 'Silver content in different coins and jewelry is:'
WRITE(*,*) ''
WRITE(*,*) 'Coin 1','Coin 2 ','Jewelry'
WRITE(*,*) (/Deno,JeweSize/)*SmallUnit 
WRITE(*,*) ''
WRITE(*,*) 'Buyer and Seller Stock of different coins and jewelry is:'
WRITE(*,*)  MATMUL(BuyerDist,PortfoB)
WRITE(*,*) ''
WRITE(*,*)  MATMUL(SellerDist,PortfoS)
WRITE(*,*) ''
WRITE(*,*) 'Total Stock of different coins and jewelry is:'
WRITE(*,*) (0.5*( MATMUL(BuyerDist,PortfoB)+MATMUL(SellerDist,PortfoS)))*(/Deno,JeweSize/)*SmallUnit
WRITE(*,*) ''
!WRITE(*,*) 'Minting Volume of different coins and jewelry is:'
!WRITE(*,*) MintVol
!WRITE(*,*) ''
!WRITE(*,*) 'Melting Volume of different coins and jewelry is:'
!WRITE(*,*) MeltVol
WRITE(*,*) ''
WRITE(*,*) 'Total Stock of Silver by Buyers is:'
WRITE(*,*) DOT_PRODUCT(MATMUL(BuyerDist,PortfoB),(/Deno,JeweSize/))*SmallUnit,SUM(BuyerDist)
WRITE(*,*) ''
WRITE(*,*) 'Total Stock of Silver by Sellers is:'
WRITE(*,*) DOT_PRODUCT(MATMUL(SellerDist,PortfoS),(/Deno,JeweSize/))*SmallUnit,SUM(SellerDist)
WRITE(*,*) ''
WRITE(*,*) DOT_PRODUCT(BeginDist,(/(i,i=0,UpBound)/)) 
WRITE(*,*) ''
WRITE(*,*) 'Coins Circulation are'
 WRITE(*,*) NumSpec*CoinCirculation*Deno*SmallUnit
WRITE(*,*) ''
 WRITE(*,*) 'Avg Paymt&Price is:'
 WRITE(*,*) AvgPaymt,AvgPrice
 WRITE(*,*) ''
 WRITE(*,*) 'Total Output is:'
 WRITE(*,*) NumSpec*TotOutput
   WRITE(*,*) ''
 WRITE(*,*) '# of trading pairs is:'
 WRITE(*,*) Mass
WRITE(*,*) ''
 WRITE(*,*) 'Minting Volume is:'
 WRITE(*,*) MintVol
 
WRITE(*,*) ''
 WRITE(*,*) 'Total Minting Volume is:'
 WRITE(*,*) TotMint
   WRITE(*,*) ''
 WRITE(*,*) 'Ex-ante Welfare is :'
 WRITE(*,*) DOT_PRODUCT(BeginValue, BeginDist),0.5*Welfare,0.5*(DOT_PRODUCT(BuyerValue, BuyerDist)+DOT_PRODUCT(SellerValue, SellerDist))&
     &+ 0.5* DOT_PRODUCT(BuyerDist,(JeweUtilB -  MATMUL(PortfoB(:,1:NumDeno),CarryCost)))  &
     &+ 0.5* DOT_PRODUCT(SellerDist,(JeweUtilS - MATMUL(PortfoS(:,1:NumDeno),CarryCost))) 
  
WRITE(*,*) ''

 
 !WRITE(*,*) BeginDist
50  format('This is the steady-state equilibrium with',I2,' denominations ')
 
 

 
!WRITE(*,*) JeweUtil
WRITE(*,*) ' ' 
 cnt= 0; cntd=0
maxct=0;maxuno=0;maxdos=0;maxtres=0;maxunodos=0
maxcuatro=0;maxcinco=0!;maxseis=0!;maxsiete=0
DO i=1,NumPortfoB
    cntd=cntd+1
    IF ( BuyerDist(i) >0.0) THEN
        cnt = cnt+1
         
         IF (DOT_PRODUCT(PortfoB(i,1:NumDeno),Deno)>maxct) THEN
             maxct = DOT_PRODUCT(PortfoB(i,1:NumDeno),Deno)
         END IF
         IF (DOT_PRODUCT(PortfoB(i,1:2),Deno(1:2))>maxunodos) THEN
             maxunodos = DOT_PRODUCT(PortfoB(i,1:2),Deno(1:2))
         END IF
         IF  (PortfoB(i,1)>maxuno) THEN
             maxuno = PortfoB(i,1)
         END IF
         IF  (PortfoB(i,2)>maxdos) THEN
             maxdos = PortfoB(i,2)
         END IF
         IF  (PortfoB(i,3)>maxtres) THEN
             maxtres = PortfoB(i,3)
         END IF
         IF  (PortfoB(i,4)>maxcuatro) THEN
             maxcuatro = PortfoB(i,4)
         END IF
         IF  (PortfoB(i,5)>maxcinco) THEN
             maxcinco = PortfoB(i,5)
         END IF
  
         !IF  (PortfoB(i,7)>maxsiete) THEN
         !    maxsiete = PortfoB(i,7)
         !END IF
    END IF
!    PofoNomCnt(PortfoSilv(i))=PofoNomCnt(PortfoSilv(i))+1
 
END DO
 WRITE(*,*) 'nominal portfo number:',cntd
 WRITE(*,*) 'effective portfo number:',cnt
WRITE(*,*) 'maxct:',maxct,maxunodos
 WRITE(*,*) maxuno,maxdos,maxtres,maxcuatro,maxcinco!,maxseis!,maxsiete
  WRITE(*,*) '^Buyer,vSeller---------'
  cnt= 0; cntd=0
maxct=0;maxuno=0;maxdos=0;maxtres=0;maxunodos=0
maxcuatro=0;maxcinco=0!;maxseis=0!;maxsiete=0
DO i=1,NumPortfoS
    cntd=cntd+1
    IF ( SellerDist(i) >0.0) THEN
        cnt = cnt+1
         
         IF (DOT_PRODUCT(PortfoS(i,1:NumDeno),Deno)>maxct) THEN
             maxct = DOT_PRODUCT(PortfoS(i,1:NumDeno),Deno)
         END IF
         IF (DOT_PRODUCT(PortfoS(i,1:2),Deno(1:2))>maxunodos) THEN
             maxunodos = DOT_PRODUCT(PortfoS(i,1:2),Deno(1:2))
         END IF
         IF  (PortfoS(i,1)>maxuno) THEN
             maxuno = PortfoS(i,1)
         END IF
         IF  (PortfoS(i,2)>maxdos) THEN
             maxdos = PortfoS(i,2)
         END IF
         IF  (PortfoS(i,3)>maxtres) THEN
             maxtres = PortfoS(i,3)
         END IF
         IF  (PortfoS(i,4)>maxcuatro) THEN
             maxcuatro = PortfoS(i,4)
         END IF
         IF  (PortfoS(i,5)>maxcinco) THEN
             maxcinco = PortfoS(i,5)
         END IF
    
         !IF  (PortfoS(i,7)>maxsiete) THEN
        !     maxsiete = PortfoS(i,7)
         !END IF
    END IF
!    PofoNomCnt(PortfoSilv(i))=PofoNomCnt(PortfoSilv(i))+1
 
END DO
 WRITE(*,*) 'nominal portfo number:',cntd
 WRITE(*,*) 'effective portfo number:',cnt
WRITE(*,*) 'maxct:',maxct,maxunodos
 WRITE(*,*) maxuno,maxdos,maxtres,maxcuatro,maxcinco!,maxseis!,maxsiete

! -----------------------------------------------------
!
!
CONTAINS

! -----------------------------------------------------
!    This function returns the utility of goods consumption.
! -----------------------------------------------------

   REAL(8)  FUNCTION  GoodsConsump(GoodsNumber)
      IMPLICIT NONE

      REAL(8), INTENT(IN) :: GoodsNumber

       GoodsConsump = ((GoodsNumber+UtilEps)**(1.0-Sigma)-(UtilEps)**(1.0-Sigma))/(1.0-Sigma)
      !GoodsConsump =  MultiCoef*(LOG(GoodsNumber+HoProCoef)-LOG(HoProCoef)) 
   END FUNCTION  GoodsConsump

! -----------------------------------------------------
!    This function returns the utility of jewelry consumption
! -----------------------------------------------------

   REAL(8) FUNCTION  JeweConsump(JeweQuant)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: JeweQuant

      !JeweConsump = Epsilon*((REAL(JeweQuant)*SmallUnit+UtilEps)**(1.0-SigmaJewe)-(UtilEps)**(1.0-SigmaJewe))/(1.0-SigmaJewe)
       JeweConsump   = Epsilon * JeweCoef*REAL(JeweQuant)!*SmallUnit
   END FUNCTION  JeweConsump

    end program PSSBaseline





