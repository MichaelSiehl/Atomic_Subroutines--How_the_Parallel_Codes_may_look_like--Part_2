! https://github.com/MichaelSiehl/Atomic_Subroutines--How_the_Parallel_Codes_may_look_like--Part_2

module OOOEerro_admError

use OOOGglob_Globals

implicit none
!___________________________________________________________
!
private
!
public :: OOOEerroc_AddObject
!___________________________________________________________
!
type, public :: OOOEerroc_colError
  private
  !
end type OOOEerroc_colError
!___________________________________________________________

contains
!
!___________________________________________________________
!
subroutine OOOEerroc_AddObject (Collection, chrErrorDescription, &
  intErrorType)
  type (OOOEerroc_colError), intent (inout) :: Collection
  character(kind=OOOGglob_kcha,len=*), intent(in) :: chrErrorDescription
  integer(kind=OOOGglob_kint), intent(in) :: intErrorType
  character(kind=OOOGglob_kcha,len=OOOGglob_Len200) :: chrSelection
  !
  !
write(*,*) "CurrentProc: ", OOOGglob_chrCurrentProcedure
!
write(*,*) "ErrDescr: ", trim(chrErrorDescription)
!
write(*,*)  "ErrTyp: ", intErrorType
!
write(*,*)
write(*,*) "CallingProc: ", trim(OOOGglob_chrCallingProcedure)
!
write(*,*) "ReturningProc: ", trim(OOOGglob_chrReturningProcedure)
!
print *
write(*,*) "     *** An error occured ! *** "
print *, ' Please select: '
print *
print *, ' x  - Exit Program, or'
print *, ' c  - Continue Execution'
print *
!
print *, ' -> '
read *, chrSelection
print *
chrSelection = trim(chrSelection)
select case (chrSelection)
  case ('x', 'X')
  !
      error stop ! end of execution
  !
end select
!
end subroutine OOOEerroc_AddObject
!___________________________________________________________
!
end module OOOEerro_admError
