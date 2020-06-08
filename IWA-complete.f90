program IWA
implicit none
    real River_Q, Discharge_Q, ALLRiver_n, ALLRiver_I,ALLRiver_B, ALLRiver_H, ALLRiver_L, ALLRiver_Temp           
    real, dimension(1,22) ::  River
    real, dimension(1,22) ::  Discharge
    real AirTemp, Sunlight, SavinovCon, Cloud
    real, dimension(28,22) :: Chemsp
    real ALLRiver_Q, ALLRiver_V, ALLRiver_A, ALLRiver_R, ALLRiver_E, ALLRiver_TankV
    integer Div_Total, Time_Total
    real KKeqw, KKeq1, KKeq2, KKeqN, KKeqP, KKeqSO               
    real SunlightSd
    real,allocatable,dimension(:,:,:) :: ALLRiver                      
    integer :: Div_Pointer = 0, Cal_Pointer = 0, Item_Pointer = 0, Runge_Pointer = 0
    real, dimension(22,1) :: TempPointer, Tempval_Inf   
    real, dimension(22,4)  :: RungeK
    real :: Kdeath_ALG = 0.1, Kdeath_CON = 0.05, Kgrowth_ALG = 2.0, Kgrowth_CON = 0.0002, Kgrowth_Haer = 2.0, &
            Kgrowth_Anox = 1.6, Kgrowth_N1 = 0.8, Kgrowth_N2 = 1.1, Khyd = 3.0, Kresp_ALG = 0.1, Kresp_CON = 0.05, &
            Kresp_Haer = 0.2, Kresp_Anox = 0.1, Kresp_N1 = 0.05, Kresp_N2 = 0.05, Keq1 = 0.001, Keq2 = 0.0001, &
            Keqw = 0.0001, KeqN = 0.0001, KeqP = 0.0001, KeqSO = 0.00000002, KHPO4_ALG = 0.02, KHPO4_Haer = 0.02, &
            KHPO4_Anox = 0.02, KHPO4_N1 = 0.02, KHPO4_N2 = 0.02, KN_ALG = 0.1, KNH4_ALG = 0.1, KNH_aer = 0.2, &
            KNH4_N1 = 0.5, K1 = 500.0, KNO3_Anox = 0.5, KNO2_Anox = 0.2, KNO2_N2 = 0.5, KO2_ALG = 0.2, KO2_CON = 0.5, &
            KO2_Haer = 0.2, KO2_N1 = 0.5, KO2_N2 = 0.5, KS_Haer = 2.0, KS_Anox = 2.0, Beta_ALG = 0.046, Beta_CON = 0.08, &
            Beta_H = 0.07, Beta_HYD= 0.07, Beta_N1 = 0.098, Beta_N2 = 0.069, Temp0 = 20.0
    real :: Cal_Total = 1.0, Time_Stride = 0.001  ![d]
                     
    open(unit=99, file='99_RivST.csv',status='old'); read(99, '()')
    open(unit=98, file='98_RivWQ.csv',status='old'); read(98, '()')
    open(unit=97, file='97_DisWQ.csv',status='old'); read(97, '()')
    open(unit=96, file='96_Matrix.csv',status='old')
    open(unit=95, file='95_CliST.csv',status='old'); read(95, '()')
    open(unit=10, file='Output.csv',status='replace')
          
    read (99,*) River_Q, Discharge_Q, ALLRiver_n, ALLRiver_I, ALLRiver_B, ALLRiver_H, &
                ALLRiver_L, ALLRiver_Temp       
    read (98,*) River(1, :)         
    read (97,*) Discharge(1, :)                                                                     
    read (96, *) Chemsp(1,1), Chemsp(1,2), Chemsp(1,3), Chemsp(1,4), Chemsp(1,5), Chemsp(1,6), Chemsp(1,7), Chemsp(1,8), & 
                Chemsp(1,9), Chemsp(1,10), Chemsp(1,11), Chemsp(1,12), Chemsp(1,13), Chemsp(1,14), Chemsp(1,15), Chemsp(1,16), &
                Chemsp(1,17), Chemsp(1,18), Chemsp(1,19), Chemsp(1,20), Chemsp(1,21), Chemsp(1,22), &              
                Chemsp(2,1), Chemsp(2,2), Chemsp(2,3), Chemsp(2,4), Chemsp(2,5), Chemsp(2,6), Chemsp(2,7), Chemsp(2,8), &
                Chemsp(2,9), Chemsp(2,10), Chemsp(2,11), Chemsp(2,12), Chemsp(2,13), Chemsp(2,14), Chemsp(2,15), Chemsp(2,16), &
                Chemsp(2,17), Chemsp(2,18), Chemsp(2,19), Chemsp(2,20), Chemsp(2,21), Chemsp(2,22), &                        
                Chemsp(3,1), Chemsp(3,2), Chemsp(3,3), Chemsp(3,4), Chemsp(3,5), Chemsp(3,6), Chemsp(3,7), Chemsp(3,8), &
                Chemsp(3,9), Chemsp(3,10), Chemsp(3,11), Chemsp(3,12), Chemsp(3,13), Chemsp(3,14), Chemsp(3,15), Chemsp(3,16), &
                Chemsp(3,17), Chemsp(3,18), Chemsp(3,19), Chemsp(3,20), Chemsp(3,21), Chemsp(3,22), &  
                Chemsp(4,1), Chemsp(4,2), Chemsp(4,3), Chemsp(4,4), Chemsp(4,5), Chemsp(4,6), Chemsp(4,7), Chemsp(4,8), &
                Chemsp(4,9), Chemsp(4,10), Chemsp(4,11), Chemsp(4,12), Chemsp(4,13), Chemsp(4,14), Chemsp(4,15), Chemsp(4,16), &
                Chemsp(4,17), Chemsp(4,18), Chemsp(4,19), Chemsp(4,20), Chemsp(4,21), Chemsp(4,22), &  
                Chemsp(5,1), Chemsp(5,2), Chemsp(5,3), Chemsp(5,4), Chemsp(5,5), Chemsp(5,6), Chemsp(5,7), Chemsp(5,8), &
                Chemsp(5,9), Chemsp(5,10), Chemsp(5,11), Chemsp(5,12), Chemsp(5,13), Chemsp(5,14), Chemsp(5,15), Chemsp(5,16), &
                Chemsp(5,17), Chemsp(5,18), Chemsp(5,19), Chemsp(5,20), Chemsp(5,21), Chemsp(5,22), &                        
                Chemsp(6,1), Chemsp(6,2), Chemsp(6,3), Chemsp(6,4), Chemsp(6,5), Chemsp(6,6), Chemsp(6,7), Chemsp(6,8), &
                Chemsp(6,9), Chemsp(6,10), Chemsp(6,11), Chemsp(6,12), Chemsp(6,13), Chemsp(6,14), Chemsp(6,15), Chemsp(6,16), &
                Chemsp(6,17), Chemsp(6,18), Chemsp(6,19), Chemsp(6,20), Chemsp(6,21), Chemsp(6,22), &  
                Chemsp(7,1), Chemsp(7,2), Chemsp(7,3), Chemsp(7,4), Chemsp(7,5), Chemsp(7,6), Chemsp(7,7), Chemsp(7,8), &
                Chemsp(7,9), Chemsp(7,10), Chemsp(7,11), Chemsp(7,12), Chemsp(7,13), Chemsp(7,14), Chemsp(7,15), Chemsp(7,16), &
                Chemsp(7,17), Chemsp(7,18), Chemsp(7,19), Chemsp(7,20), Chemsp(7,21), Chemsp(7,22), &  
                Chemsp(8,1), Chemsp(8,2), Chemsp(8,3), Chemsp(8,4), Chemsp(8,5), Chemsp(8,6), Chemsp(8,7), Chemsp(8,8), &
                Chemsp(8,9), Chemsp(8,10), Chemsp(8,11), Chemsp(8,12), Chemsp(8,13), Chemsp(8,14), Chemsp(8,15), Chemsp(8,16), &
                Chemsp(8,17), Chemsp(8,18), Chemsp(8,19), Chemsp(8,20), Chemsp(8,21), Chemsp(8,22), &                        
                Chemsp(9,1), Chemsp(9,2), Chemsp(9,3), Chemsp(9,4), Chemsp(9,5), Chemsp(9,6), Chemsp(9,7), Chemsp(9,8), &
                Chemsp(9,9), Chemsp(9,10), Chemsp(9,11), Chemsp(9,12), Chemsp(9,13), Chemsp(9,14), Chemsp(9,15), Chemsp(9,16), &
                Chemsp(9,17), Chemsp(9,18), Chemsp(9,19), Chemsp(9,20), Chemsp(9,21), Chemsp(9,22), &  
                Chemsp(10,1), Chemsp(10,2), Chemsp(10,3), Chemsp(10,4), Chemsp(10,5), Chemsp(10,6), Chemsp(10,7), Chemsp(10,8), &
                Chemsp(10,9), Chemsp(10,10), Chemsp(10,11), Chemsp(10,12), Chemsp(10,13), Chemsp(10,14), Chemsp(10,15), & 
                Chemsp(10,16), Chemsp(10,17), Chemsp(10,18), Chemsp(10,19), Chemsp(10,20), Chemsp(10,21), Chemsp(10,22), &  
                Chemsp(11,1), Chemsp(11,2), Chemsp(11,3), Chemsp(11,4), Chemsp(11,5), Chemsp(11,6), Chemsp(11,7), Chemsp(11,8), &
                Chemsp(11,9), Chemsp(11,10), Chemsp(11,11), Chemsp(11,12), Chemsp(11,13), Chemsp(11,14), Chemsp(11,15), &
                Chemsp(11,16), Chemsp(11,17), Chemsp(11,18), Chemsp(11,19), Chemsp(11,20), Chemsp(11,21), Chemsp(11,22), &                        
                Chemsp(12,1), Chemsp(12,2), Chemsp(12,3), Chemsp(12,4), Chemsp(12,5), Chemsp(12,6), Chemsp(12,7), Chemsp(12,8), &
                Chemsp(12,9), Chemsp(12,10), Chemsp(12,11), Chemsp(12,12), Chemsp(12,13), Chemsp(12,14), Chemsp(12,15), &
                Chemsp(12,16), Chemsp(12,17), Chemsp(12,18), Chemsp(12,19), Chemsp(12,20), Chemsp(12,21), Chemsp(12,22), &  
                Chemsp(13,1), Chemsp(13,2), Chemsp(13,3), Chemsp(13,4), Chemsp(13,5), Chemsp(13,6), Chemsp(13,7), Chemsp(13,8), &
                Chemsp(13,9), Chemsp(13,10), Chemsp(13,11), Chemsp(13,12), Chemsp(13,13), Chemsp(13,14), Chemsp(13,15), &
                Chemsp(13,16), Chemsp(13,17), Chemsp(13,18), Chemsp(13,19), Chemsp(13,20), Chemsp(13,21), Chemsp(13,22), &  
                Chemsp(14,1), Chemsp(14,2), Chemsp(14,3), Chemsp(14,4), Chemsp(14,5), Chemsp(14,6), Chemsp(14,7), Chemsp(14,8), &
                Chemsp(14,9), Chemsp(14,10), Chemsp(14,11), Chemsp(14,12), Chemsp(14,13), Chemsp(14,14), Chemsp(14,15), &
                Chemsp(14,16), Chemsp(14,17), Chemsp(14,18), Chemsp(14,19), Chemsp(14,20), Chemsp(14,21), Chemsp(14,22), &                        
                Chemsp(15,1), Chemsp(15,2), Chemsp(15,3), Chemsp(15,4), Chemsp(15,5), Chemsp(15,6), Chemsp(15,7), Chemsp(15,8), &
                Chemsp(15,9), Chemsp(15,10), Chemsp(15,11), Chemsp(15,12), Chemsp(15,13), Chemsp(15,14), Chemsp(15,15), &
                Chemsp(15,16), Chemsp(15,17), Chemsp(15,18), Chemsp(15,19), Chemsp(15,20), Chemsp(15,21), Chemsp(15,22), &  
                Chemsp(16,1), Chemsp(16,2), Chemsp(16,3), Chemsp(16,4), Chemsp(16,5), Chemsp(16,6), Chemsp(16,7), Chemsp(16,8), &
                Chemsp(16,9), Chemsp(16,10), Chemsp(16,11), Chemsp(16,12), Chemsp(16,13), Chemsp(16,14), Chemsp(16,15), &
                Chemsp(16,16), Chemsp(16,17), Chemsp(16,18), Chemsp(16,19), Chemsp(16,20), Chemsp(16,21), Chemsp(16,22), &  
                Chemsp(17,1), Chemsp(17,2), Chemsp(17,3), Chemsp(17,4), Chemsp(17,5), Chemsp(17,6), Chemsp(17,7), Chemsp(17,8), &
                Chemsp(17,9), Chemsp(17,10), Chemsp(17,11), Chemsp(17,12), Chemsp(17,13), Chemsp(17,14), Chemsp(17,15), &
                Chemsp(17,16), Chemsp(17,17), Chemsp(17,18), Chemsp(17,19), Chemsp(17,20), Chemsp(17,21), Chemsp(17,22), &                        
                Chemsp(18,1), Chemsp(18,2), Chemsp(18,3), Chemsp(18,4), Chemsp(18,5), Chemsp(18,6), Chemsp(18,7), Chemsp(18,8), &
                Chemsp(18,9), Chemsp(18,10), Chemsp(18,11), Chemsp(18,12), Chemsp(18,13), Chemsp(18,14), Chemsp(18,15), &
                Chemsp(18,16), Chemsp(18,17), Chemsp(18,18), Chemsp(18,19), Chemsp(18,20), Chemsp(18,21), Chemsp(18,22), &  
                Chemsp(19,1), Chemsp(19,2), Chemsp(19,3), Chemsp(19,4), Chemsp(19,5), Chemsp(19,6), Chemsp(19,7), Chemsp(19,8), &
                Chemsp(19,9), Chemsp(19,10), Chemsp(19,11), Chemsp(19,12), Chemsp(19,13), Chemsp(19,14), Chemsp(19,15), &
                Chemsp(19,16), Chemsp(19,17), Chemsp(19,18), Chemsp(19,19), Chemsp(19,20), Chemsp(19,21), Chemsp(19,22), &  
                Chemsp(20,1), Chemsp(20,2), Chemsp(20,3), Chemsp(20,4), Chemsp(20,5), Chemsp(20,6), Chemsp(20,7), Chemsp(20,8), &
                Chemsp(20,9), Chemsp(20,10), Chemsp(20,11), Chemsp(20,12), Chemsp(20,13), Chemsp(20,14), Chemsp(20,15), &
                Chemsp(20,16), Chemsp(20,17), Chemsp(20,18), Chemsp(20,19), Chemsp(20,20), Chemsp(20,21), Chemsp(20,22), &                        
                Chemsp(21,1), Chemsp(21,2), Chemsp(21,3), Chemsp(21,4), Chemsp(21,5), Chemsp(21,6), Chemsp(21,7), Chemsp(21,8), &
                Chemsp(21,9), Chemsp(21,10), Chemsp(21,11), Chemsp(21,12), Chemsp(21,13), Chemsp(21,14), Chemsp(21,15), &
                Chemsp(21,16), Chemsp(21,17), Chemsp(21,18), Chemsp(21,19), Chemsp(21,20), Chemsp(21,21), Chemsp(21,22), &  
                Chemsp(22,1), Chemsp(22,2), Chemsp(22,3), Chemsp(22,4), Chemsp(22,5), Chemsp(22,6), Chemsp(22,7), Chemsp(22,8), &
                Chemsp(22,9), Chemsp(22,10), Chemsp(22,11), Chemsp(22,12), Chemsp(22,13), Chemsp(22,14), Chemsp(22,15), &
                Chemsp(22,16), Chemsp(22,17), Chemsp(22,18), Chemsp(22,19), Chemsp(22,20), Chemsp(22,21), Chemsp(22,22), &  
                Chemsp(23,1), Chemsp(23,2), Chemsp(23,3), Chemsp(23,4), Chemsp(23,5), Chemsp(23,6), Chemsp(23,7), Chemsp(23,8), &
                Chemsp(23,9), Chemsp(23,10), Chemsp(23,11), Chemsp(23,12), Chemsp(23,13), Chemsp(23,14), Chemsp(23,15), &
                Chemsp(23,16), Chemsp(23,17), Chemsp(23,18), Chemsp(23,19), Chemsp(23,20), Chemsp(23,21), Chemsp(23,22), &                        
                Chemsp(24,1), Chemsp(24,2), Chemsp(24,3), Chemsp(24,4), Chemsp(24,5), Chemsp(24,6), Chemsp(24,7), Chemsp(24,8), &
                Chemsp(24,9), Chemsp(24,10), Chemsp(24,11), Chemsp(24,12), Chemsp(24,13), Chemsp(24,14), Chemsp(24,15), &
                Chemsp(24,16), Chemsp(24,17), Chemsp(24,18), Chemsp(24,19), Chemsp(24,20), Chemsp(24,21), Chemsp(24,22), &  
                Chemsp(25,1), Chemsp(25,2), Chemsp(25,3), Chemsp(25,4), Chemsp(25,5), Chemsp(25,6), Chemsp(25,7), Chemsp(25,8), &
                Chemsp(25,9), Chemsp(25,10), Chemsp(25,11), Chemsp(25,12), Chemsp(25,13), Chemsp(25,14), Chemsp(25,15), &
                Chemsp(25,16), Chemsp(25,17), Chemsp(25,18), Chemsp(25,19), Chemsp(25,20), Chemsp(25,21), Chemsp(25,22), &  
                Chemsp(26,1), Chemsp(26,2), Chemsp(26,3), Chemsp(26,4), Chemsp(26,5), Chemsp(26,6), Chemsp(26,7), Chemsp(26,8), &
                Chemsp(26,9), Chemsp(26,10), Chemsp(26,11), Chemsp(26,12), Chemsp(26,13), Chemsp(26,14), Chemsp(26,15), &
                Chemsp(26,16), Chemsp(26,17), Chemsp(26,18), Chemsp(26,19), Chemsp(26,20), Chemsp(26,21), Chemsp(26,22), &                        
                Chemsp(27,1), Chemsp(27,2), Chemsp(27,3), Chemsp(27,4), Chemsp(27,5), Chemsp(27,6), Chemsp(27,7), Chemsp(27,8), &
                Chemsp(27,9), Chemsp(27,10), Chemsp(27,11), Chemsp(27,12), Chemsp(27,13), Chemsp(27,14), Chemsp(27,15), &
                Chemsp(27,16), Chemsp(27,17), Chemsp(27,18), Chemsp(27,19), Chemsp(27,20), Chemsp(27,21), Chemsp(27,22), &  
                Chemsp(28,1), Chemsp(28,2), Chemsp(28,3), Chemsp(28,4), Chemsp(28,5), Chemsp(28,6), Chemsp(28,7), Chemsp(28,8), &
                Chemsp(28,9), Chemsp(28,10), Chemsp(28,11), Chemsp(28,12), Chemsp(28,13), Chemsp(28,14), Chemsp(28,15), &
                Chemsp(28,16), Chemsp(28,17), Chemsp(28,18), Chemsp(28,19), Chemsp(28,20), Chemsp(28,21), Chemsp(28,22) 
    
    read (95,*) AirTemp, Sunlight, SavinovCon, Cloud            
                
    ALLRiver_Q = River_Q + Discharge_Q
    ALLRiver_A = ALLRiver_B * ALLRiver_H
    ALLRiver_R = ALLRiver_A / (2.0 * ALLRiver_H + ALLRiver_B)
    ALLRiver_V = (1 / ALLRiver_n) * ALLRiver_R ** 0.6666666667 * ALLRiver_I ** 0.5
    ALLRiver_E = 2.0 * sqrt(9.8 * ALLRiver_R * ( (ALLRiver_n ** 2.0 * ALLRiver_V ** 2.0) / ALLRiver_R**(1.3333333333) ) ) * &
                 ALLRiver_H * ( ALLRiver_B / ALLRiver_H )**(1.5)
    Div_Total = int((ALLRiver_V * ALLRiver_L) / (2.0 * ALLRiver_E) + 1.0)
    ALLRiver_TankV = ALLRiver_B * ALLRiver_H * ALLRiver_L / Div_Total
    
    KKeqw = 10.0**((-4470.99) / (273.15 + ALLRiver_Temp) + 12.0875 - 0.01706 * (273.15 + ALLRiver_Temp))
    KKeq1 = 10.0**(17.843 - (3404.71 / (273.15 + ALLRiver_Temp)) - 0.032786 * (273.15 + ALLRiver_Temp))
    KKeq2 = 10.0**(9.494-2902.39 / (273.15 + ALLRiver_Temp) - 0.02379 * (273.15 + ALLRiver_Temp))
    KKeqN = 10.0**(2.891 - 2727.0 / (273.15 + ALLRiver_Temp))
    KKeqP = 10.0**((-3.46) - (219.4 / (273.15 + ALLRiver_Temp)))
    KKeqSO = 12.0 * 40.0 * 10.0 **(19.87 - (3059 / (273.15 + ALLRiver_Temp)) - (0.04035 * (273.15 + ALLRiver_Temp))) 

    Sunlightsd = Sunlight * ( 1 - ( 1 - SavinovCon ) * Cloud )
	         
    if (Div_Total < 10) Then
        Div_Total = 10
        print *, 'Warning! Div_Total is less than 10 ! >>> Div_Total = 10'
    End if

    Time_Total = int(Cal_Total/Time_Stride)
	    	       
    allocate (ALLRiver(22, Time_Total, Div_Total))

    ! Initial conditions
    do Item_Pointer = 1, 22
        ALLRiver(Item_Pointer, 1, 1:Div_Total) = (River_Q * River(1, Item_Pointer) + &
                                                 Discharge_Q * Discharge(1, Item_Pointer)) / ALLRiver_Q
    end do


    ! Loop calculation
    do Div_Pointer = 1, Div_Total; do Cal_Pointer = 2, Time_Total  
        do Item_Pointer = 1, 22; do Runge_Pointer = 1, 4                    
            if (Runge_Pointer == 1) then
                TempPointer(:, 1) = ALLRiver(:, Cal_Pointer-1, Div_Pointer)
            else if (Runge_Pointer == 2) then
                TempPointer(:, 1) = ALLRiver(:, Cal_Pointer-1, Div_Pointer) + (RungeK(:,1) * Time_Stride / 2.0)
            else if (Runge_Pointer == 3) then
                TempPointer(:, 1) = ALLRiver(:, Cal_Pointer-1, Div_Pointer) + (RungeK(:,2)  * Time_Stride / 2.0)
            else if (Runge_Pointer == 4) then
                TempPointer(:, 1) = ALLRiver(:, Cal_Pointer-1, Div_Pointer) + RungeK(:,3)  * Time_Stride
            end if

            if (Div_Pointer == 1) then
                Tempval_Inf(:, 1) = ALLRiver(:, 1, Div_Pointer)                                                    
            else    
                Tempval_Inf(:, 1) = ALLRiver(:, Cal_Pointer-1, Div_Pointer-1)                                                    
            end if
                                                    
            RungeK(Item_Pointer, Runge_Pointer) = Time_Stride * (( ALLRiver_Q / ALLRiver_TankV) * &
                (Tempval_Inf(Item_Pointer, 1) - TempPointer(Item_Pointer, 1)) &
                + Chemsp(1,Item_Pointer) * Kgrowth_Haer * exp(Beta_H * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(1,1), KS_Haer) * Frac(TempPointer(9,1), KO2_Haer) * & 
                Frac(TempPointer(3,1) + TempPointer(4,1), KNH_aer) * &
                Frac(TempPointer(7,1) + TempPointer(8,1), KHPO4_Haer) * TempPointer(16, 1) &  
                + Chemsp(2,Item_Pointer) * Kgrowth_Haer * exp(Beta_H * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(1,1), KS_Haer) * Frac(TempPointer(9,1), KO2_Haer) * &
                Frac(TempPointer(3,1) + TempPointer(4,1), KNH_aer)  * &
                Frac(TempPointer(6,1), KNH_aer) * Frac(TempPointer(7,1) + TempPointer(8,1), KHPO4_Haer) * TempPointer(16,1) &  
                + Chemsp(3,Item_Pointer) * Kresp_Haer * exp(Beta_H * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(9,1), KO2_Haer) * TempPointer(16,1) &  
                + Chemsp(4,Item_Pointer) * Kgrowth_Anox * exp(Beta_H * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(1,1), KS_Anox) * Frac(TempPointer(9,1), KO2_Haer) * Frac(TempPointer(6,1), KNO3_Anox) * &
                Frac(TempPointer(7,1) + TempPointer(8,1), KHPO4_Anox) * TempPointer(16,1) & 
                + Chemsp(5,Item_Pointer) * Kgrowth_Anox * exp(Beta_H * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(1,1), KS_Anox) * Frac(TempPointer(9,1), KO2_Haer) * Frac(TempPointer(5,1), KNO2_Anox) * &
                Frac(TempPointer(7,1) + TempPointer(8,1), KHPO4_Anox) * TempPointer(16,1) &    
                + Chemsp(6,Item_Pointer) * Kresp_Anox * exp(Beta_H * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(9,1), KO2_Haer) * Frac(TempPointer(6,1), KNO3_Anox) * TempPointer(16,1) & 
                + Chemsp(7,Item_Pointer) * Kgrowth_N1 * exp(Beta_N1 * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(9,1), KO2_N1) * Frac(TempPointer(3,1) + TempPointer(4,1), KNH4_N1) *&
                Frac(TempPointer(7,1) + TempPointer(8,1), KHPO4_N1) * TempPointer(17,1) &  
                + Chemsp(8,Item_Pointer) * Kresp_N1 * exp(Beta_N1 * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(9,1), KO2_N1) * TempPointer(17,1) & 
                + Chemsp(9,Item_Pointer) * Kgrowth_N2 * exp(Beta_N2 * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(9,1), KO2_N2) * Frac(TempPointer(5,1), KNO2_N2) * &
                Frac(TempPointer(7,1) + TempPointer(8,1), KHPO4_N2) * TempPointer(18,1) &  
                + Chemsp(10,Item_Pointer) * Kresp_N2 * exp(Beta_N2 * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(9,1), KO2_N2) * TempPointer(18,1) &  	                            
                + Chemsp(11,Item_Pointer) * Kgrowth_ALG * exp(Beta_ALG * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(3,1) + TempPointer(4,1) + TempPointer(6,1), KN_ALG) * &
                Frac(TempPointer(3,1) + TempPointer(4,1), KNH4_ALG) * &
                Frac(TempPointer(7,1) + TempPointer(8,1), KHPO4_ALG) * (Sunlightsd / K1) * exp(1 - (Sunlightsd/K1)) * &
                TempPointer(19,1) & 
                + Chemsp(12,Item_Pointer) * Kgrowth_ALG * exp(Beta_ALG * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(3,1) + TempPointer(4,1) + TempPointer(6,1), KN_ALG) * &
                Frac(TempPointer(3,1) + TempPointer(4,1), KNH4_ALG) * &
                Frac(TempPointer(7,1) + TempPointer(8,1), KHPO4_ALG) * (Sunlightsd / K1) * exp(1 - (Sunlightsd / K1)) * &
                TempPointer(19,1) &  
                + Chemsp(13,Item_Pointer) * Kresp_ALG * exp(Beta_ALG * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(9,1), KO2_ALG) * TempPointer(19,1) & 
                + Chemsp(14,Item_Pointer) * Kdeath_ALG * exp(Beta_ALG * (ALLRiver_Temp - Temp0)) * TempPointer(19,1) & 
                + Chemsp(15,Item_Pointer) * Kgrowth_CON * exp(Beta_CON * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(9,1), KO2_CON) * TempPointer(19,1) * TempPointer(20,1) & 
                + Chemsp(16,Item_Pointer) * Kgrowth_CON * exp(Beta_CON * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(9,1), KO2_CON) * TempPointer(21,1) * TempPointer(20,1) & 
                + Chemsp(17,Item_Pointer) * Kgrowth_CON * exp(Beta_CON * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(9,1), KO2_CON) * TempPointer(16, 1) * TempPointer(20,1) &  
                + Chemsp(18,Item_Pointer) * Kgrowth_CON * exp(Beta_CON * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(9,1), KO2_CON) * TempPointer(17,1) * TempPointer(20,1) & 
                + Chemsp(19,Item_Pointer) * Kgrowth_CON * exp(Beta_CON * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(9,1), KO2_CON) * TempPointer(18,1) * TempPointer(20,1) &   
                + Chemsp(20,Item_Pointer) * Kresp_CON * exp(Beta_CON * (ALLRiver_Temp - Temp0)) * &
                Frac(TempPointer(9,1), KO2_CON) * TempPointer(20,1) & 		                            
                + Chemsp(21,Item_Pointer) * Kdeath_CON * exp(Beta_CON * (ALLRiver_Temp - Temp0)) * TempPointer(20,1) & 
                + Chemsp(22,Item_Pointer) * Khyd * exp(Beta_HYD * (ALLRiver_Temp - Temp0)) * TempPointer(21,1) &  
                + Chemsp(23,Item_Pointer) * Keq1 * (TempPointer(10,1) - TempPointer(13,1) * TempPointer(11,1) / KKeq1) & 
                + Chemsp(24,Item_Pointer) * Keq2 * (TempPointer(11,1) - TempPointer(13,1) * TempPointer(12,1) / KKeq2) & 
                + Chemsp(25,Item_Pointer) * Keqw * (1.0 - TempPointer(13,1) * TempPointer(14,1) / KKeqw) & 
                + Chemsp(26,Item_Pointer) * KeqN * (TempPointer(3,1) - TempPointer(13,1) * TempPointer(4,1) / KKeqN) & 
                + Chemsp(27,Item_Pointer) * KeqP * (TempPointer(8,1) - TempPointer(13,1) * TempPointer(7,1) / KKeqP) &   
                + Chemsp(28,Item_Pointer) * KeqSO * (1.0 - TempPointer(15, 1) * TempPointer(12,1) / KKeqSO))
            end do !Runge_Pointer
                                                                            
            ALLRiver(Item_Pointer, Cal_Pointer,Div_Pointer) = ALLRiver(Item_Pointer, Cal_Pointer-1,Div_Pointer) + &
                (RungeK(Item_Pointer,1) + RungeK(Item_Pointer,2)*2.0 + RungeK(Item_Pointer,3)*2.0 + RungeK(Item_Pointer,4))/6.0  
        end do !Item_Pointer        
                                                                                        
        ! Writing data in a csv file                
        if (mod(Cal_Pointer, 100).eq. 0) then
            write(10, 200) Div_Pointer, Cal_Pointer*Time_Stride, ALLRiver(:, Cal_Pointer,Div_Pointer)
        end if 
        
    end do; end do !Div_counter and Cal_counter
    
    close (unit=99); close (unit=98); close (unit=97); close (unit=95); close (unit=96); close (unit=10)

200 format(I8, 22(F10.4, ','), F10.4)    

contains

    real function Frac(x,y)
        real x,y
        
        Frac = (x) / (x + y)
        
        if (x + y == 0) then
            Frac = 0
        end if 

    end function Frac
                    
end program IWA