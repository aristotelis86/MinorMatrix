! Module to store the functions producing the minor matrix.
   
   module minor_matrix_mod
   
   implicit none
   
   private
   
   public minor_matrix
   
   interface minor_matrix
      module procedure minor_int
      module procedure minor_real
   end interface
   
   contains 
   
   ! ========================================================================
   ! INTEGERS
   function minor_int(i, j, A) result(B)
   
   integer, intent(in)     :: i, j
   integer, intent(in)     :: A(:,:)
   integer, allocatable    :: B(:,:)
   
   integer                 :: rows, cols
   integer, allocatable    :: mrows(:), mcols(:)
   integer                 :: irow, icol, k
   integer                 :: upbound_d1, lobound_d1
   integer                 :: upbound_d2, lobound_d2
   
   upbound_d1 = ubound(A,1)
   lobound_d1 = lbound(A,1)
   upbound_d2 = ubound(A,2)
   lobound_d2 = lbound(A,2)
   
   rows = size(A,1)
   cols = size(A,2)
   
   allocate(B(rows-1,cols-1))
   B = -99999
   
   if ((i < lobound_d1) .or.  (i > upbound_d1)) then
      write(*,*) 'ERROR!: The row id you entered is outside the matrix bounds...'
      return
   else if ((j < lobound_d2) .or.  (j > upbound_d2)) then
      write(*,*) 'ERROR!: The column id you entered is outside the matrix bounds...'
      return
   end if
   
   allocate(mrows(rows-1), mcols(cols-1))
   
   k = 1
   do irow = 1, rows
      if (irow /= i) then
         mrows(k) = irow
         k = k + 1
      end if
   end do
   
   k = 1
   do icol = 1, cols
      if (icol /= j) then
         mcols(k) = icol
         k = k + 1
      end if
   end do
   
   do irow = 1, size(mrows)
      do icol = 1, size(mcols)
         B(irow,icol) = A(mrows(irow), mcols(icol))
      end do
   end do
   
   end function minor_int
   
   ! ========================================================================
   ! REALS
   function minor_real(i, j, A) result(B)
   
   integer, intent(in)     :: i, j
   real, intent(in)        :: A(:,:)
   real, allocatable       :: B(:,:)
   
   integer                 :: rows, cols
   integer, allocatable    :: mrows(:), mcols(:)
   integer                 :: irow, icol, k
   integer                 :: upbound_d1, lobound_d1
   integer                 :: upbound_d2, lobound_d2
   
   upbound_d1 = ubound(A,1)
   lobound_d1 = lbound(A,1)
   upbound_d2 = ubound(A,2)
   lobound_d2 = lbound(A,2)
   
   rows = size(A,1)
   cols = size(A,2)
   
   allocate(B(rows-1,cols-1))
   B = -99999.
   
   if ((i < lobound_d1) .or.  (i > upbound_d1)) then
      write(*,*) 'ERROR!: The row id you entered is outside the matrix bounds...'
      return
   else if ((j < lobound_d2) .or.  (j > upbound_d2)) then
      write(*,*) 'ERROR!: The column id you entered is outside the matrix bounds...'
      return
   end if
   
   allocate(mrows(rows-1), mcols(cols-1))
   
   k = 1
   do irow = 1, rows
      if (irow /= i) then
         mrows(k) = irow
         k = k + 1
      end if
   end do
   
   k = 1
   do icol = 1, cols
      if (icol /= j) then
         mcols(k) = icol
         k = k + 1
      end if
   end do
   
   do irow = 1, size(mrows)
      do icol = 1, size(mcols)
         B(irow,icol) = A(mrows(irow), mcols(icol))
      end do
   end do
   
   end function minor_real
   
   
   
   end module
   
