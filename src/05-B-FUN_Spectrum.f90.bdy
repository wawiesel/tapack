SELECT CASE(Method)
 CASE('EXP1')
  
  SELECT CASE(Scheme)
   CASE('GREYSCALE')
    RGBA = RGBA_Exp1( ABS(X) , ro    = (/+2._KIND_R,-1._KIND_R,+1._KIND_R,+0._KIND_R,+1.0_KIND_R/) , &
                               gamma = (/+2._KIND_R,-1._KIND_R,+1._KIND_R,+0._KIND_R,+1.0_KIND_R/) , &
                               beta  = (/+2._KIND_R,-1._KIND_R,+1._KIND_R,+0._KIND_R,+1.0_KIND_R/) , &
                               alpha = (/+2._KIND_R,-1._KIND_R,+1._KIND_R,+0._KIND_R,+1.0_KIND_R/) )

   CASE('ECTOPLASM')
    RGBA = RGBA_Exp1( ABS(X) , ro    = (/+50._KIND_R,-0.5_KIND_R,+0.5_KIND_R,+0.1_KIND_R,+1.0_KIND_R/) , &
                               gamma = (/+3.0_KIND_R,-0.2_KIND_R,+0.2_KIND_R,+0.5_KIND_R,+1.0_KIND_R/) , &
                               beta  = (/+3.0_KIND_R,-0.8_KIND_R,+0.8_KIND_R,+0.9_KIND_R,+1.0_KIND_R/) , &
                               alpha = (/+2._KIND_R,-1._KIND_R,+1._KIND_R,+0._KIND_R,+1.0_KIND_R/) )
   
   CASE('SONOMA')
    RGBA = RGBA_Exp1( ABS(X) , ro    = (/+5.0_KIND_R,-0.3_KIND_R,+0.3_KIND_R,+0.0_KIND_R,+2.0_KIND_R/) , &
                               gamma = (/+3.0_KIND_R,+0.0_KIND_R,-0.3_KIND_R,+0.0_KIND_R,+1.0_KIND_R/) , &
                               beta  = (/+5.0_KIND_R,+0.0_KIND_R,-0.9_KIND_R,+0.0_KIND_R,+2.0_KIND_R/) , &
                               alpha = (/+2._KIND_R,-1._KIND_R,+1._KIND_R,+0._KIND_R,+1.0_KIND_R/) )

   CASE('ICY')
    RGBA = RGBA_Exp1( ABS(X) , ro    = (/+2.0_KIND_R,-0.9_KIND_R,+0.9_KIND_R,+0.0_KIND_R,+1.0_KIND_R/) , &
                               gamma = (/+2.0_KIND_R,-0.5_KIND_R,+0.5_KIND_R,+0.7_KIND_R,+1.0_KIND_R/) , &
                               beta  = (/+1.0_KIND_R,+1.0_KIND_R,+0.0_KIND_R,+0.0_KIND_R,+0.0_KIND_R/) , &
                               alpha = (/+2._KIND_R,-1._KIND_R,+1._KIND_R,+0._KIND_R,+1.0_KIND_R/) )
                          
   CASE('SUNSET')
    RGBA = RGBA_Exp1( ABS(X) , ro    = (/100._KIND_R,-0.1_KIND_R,+0.1_KIND_R,+0.0_KIND_R,+1.0_KIND_R/) , &
                               gamma = (/ 10._KIND_R,-0.5_KIND_R,+0.5_KIND_R,+0.5_KIND_R,+2.0_KIND_R/) , &
                               beta  = (/ 10._KIND_R,-0.5_KIND_R,+0.5_KIND_R,+1.0_KIND_R,+2.0_KIND_R/) , &
                               alpha = (/+2._KIND_R,-1._KIND_R,+1._KIND_R,+0._KIND_R,+1.0_KIND_R/) )

   CASE DEFAULT
    RGBA = RGBA_Exp1( ABS(X) , ro , gamma , beta , alpha )

  END SELECT
   
 CASE('HOT')
  RGBA = RGBA_Hot( ABS(X) )

 CASE DEFAULT
  RGBA = RGBA_Random( ABS(X) )

END SELECT
