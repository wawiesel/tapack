!! Get $R^{2}$ equivalent of $Z$ SCLS.
real_SCLS(1,:) =  REAL(SCLS)
real_SCLS(2,:) = AIMAG(SCLS)

!! Get $R^{2}$ equivalent of $Z$ SCL.
real_SCL(1) = REAL(SCL)
real_SCL(2) = AIMAG(SCL)

!! Call the real routine.
loc = NEARLOC(real_SCLS,real_SCL)