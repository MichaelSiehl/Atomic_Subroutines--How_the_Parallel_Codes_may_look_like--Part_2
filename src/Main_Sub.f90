! https://github.com/MichaelSiehl/Atomic_Subroutines--How_the_Parallel_Codes_may_look_like--Part_2

module Main_Sub
!
contains
!
!**********
!
subroutine Entry_Main_Sub
  !
  use OOOPimma_admImageManager
  !
  implicit none
  !
  call OOOPimma_Start (OOOPimmaImageManager_1) ! start the ImageManager on all images
  !
end subroutine Entry_Main_Sub
!
!**********
!
end module Main_Sub
