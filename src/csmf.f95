SUBROUTINE csmf(res,mat,ker,mxsize,mysize,oxsize,oysize,ksize,ksizecount)
  IMPLICIT NONE
  INTEGER, INTENT(in) :: mxsize,mysize,oxsize,oysize,ksize,ksizecount
  INTEGER :: i, j, k, l
  DOUBLE PRECISION, DIMENSION(mxsize,mysize), INTENT(out) :: res
  DOUBLE PRECISION, DIMENSION(mxsize,mysize), INTENT(in) :: mat
  DOUBLE PRECISION, DIMENSION(ksize,ksize), INTENT(in) :: ker
  FORALL (i = 1:mxsize, j = 1:mysize)
     res(i,j) = 0.0
  END FORALL
  DO i = (1+ksizecount),(oxsize+ksizecount)
     DO j = (1+ksizecount),(oysize+ksizecount)
        DO k = -(ksize/2),(ksize/2)
           DO l = -(ksize/2),(ksize/2)
              res(i,j) = res(i,j) + ker(k+ksizecount+1,l+ksizecount+1)*mat(i+k,j+l)
           END DO
        END DO
     END DO
  END DO
END SUBROUTINE csmf
