!set brightness to full
RGBA(4) = 1.00_KIND_R

!yellow->orange
IF( X<0.30_KIND_R )THEN
 RGBA(1) = 1.00_KIND_R
 RGBA(2) = 1.00_KIND_R - X
 RGBA(3) = 0.00_KIND_R

!orange->red/orange
ELSE IF( X<0.80_KIND_R )THEN
 RGBA(1) = 0.95_KIND_R 
 RGBA(2) = 1.00_KIND_R - 1.00_KIND_R*X
 RGBA(3) = 0.00_KIND_R

!red/orange->red
ELSE IF( X<0.90_KIND_R )THEN
 RGBA(1) = 0.88_KIND_R + 1.20_KIND_R*(X-0.80_KIND_R)
 RGBA(2) = 0.10_KIND_R - 1.00_KIND_R*(X-0.80_KIND_R)
 RGBA(3) = 0.00_KIND_R

!pink
ELSE
 RGBA(1) = 1.000_KIND_R
 RGBA(2) = 0.075_KIND_R
 RGBA(3) = 0.580_KIND_R
 !intensify the pinks
 RGBA = RGBA*X**2
END IF
