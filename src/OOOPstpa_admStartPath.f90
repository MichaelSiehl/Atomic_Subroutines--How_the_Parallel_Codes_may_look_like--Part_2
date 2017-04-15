! https://github.com/MichaelSiehl/Atomic_Subroutines--How_the_Parallel_Codes_may_look_like--Part_2

module OOOPstpa_admStartPath
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPstpa
!********************************************************
! Abstract Data Type (ADT):         OOOPstpa_adtStartPath
! Abstract Data Type Module (adm):  OOOPstpa_admStartPath.f90
!********************************************************
! Purpose:                    StartPath-Object
! Language:                   mainly Fortran 95 with Fortran 2008 coarrays
! Programmer:                 Michael Siehl
! Date:                       November 2016
!********************************************************
! Naming Conventions:
!
!  for scalar members:
!                             m: object member
!                             S: setter, G: getter,
!                             S_atomic: the setter operates on atomic values using atomic_define and SYNC MEMORY
!                             G_check_atomic: the getter only checks local PGAS memory for specific values atomically
!
!  for array members:
!                             A: array
!                             mA: array member
!                             SA: set array, GA: get array,
!
!  for elements of array members:
!                             SAElement: set only one array element
!                             GAElement: get only one array element
!
!                             99: signals a static array member which has an upper array bound
!                                 larger than necessary; the upper bound is given by a global parameter
!
!  other naming conventions:
!                             _CA: coarray routine / coarray declaration
!                             _SYNC_: synchronization routine
!
!                             Enum: enumeration
!
!                             OO: public (outer) scope (the two leading namespace letters)
!                             II: private (inner) scope
!                             UU: sub-object
!********************************************************
!___________________________________________________________

use OOOGglob_Globals
use OOOEerro_admError
!
!___________________________________________________________

implicit none
!___________________________________________________________

private
!___________________________________________________________
!
!*****************************
! access routines for scalar *
! and static array members:  *
!*****************************
public :: OOOPstpaS_chrPath, OOOPstpaG_chrPath

!___________________________________________________________
!
!********************
!** Program Logic: **
!********************
public :: OOOPstpa_LoadPath

!___________________________________________________________
!
!*********************
!** Error Handling: **
!*********************
!
private :: IIstpa_ErrorHandler

!___________________________________________________________
!
!***************************************
!***  Type Definition: *****************
!***************************************
type, public :: OOOPstpa_adtStartPath
  private
  !*****
  character(kind=OOOGglob_kcha,len=OOOGglob_Len200) :: m_chrPath = ""
  type (OOOEerroc_colError) :: m_UUerrocError ! Error-Collection
  !
end type OOOPstpa_adtStartPath
!__________________________________________________________
!


contains




!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************************
! access routines for scalar   *
! and static array members:    *
!*******************************
!___________________________________________________________

subroutine OOOPstpaS_chrPath (Object, chrPath)
  type (OOOPstpa_adtStartPath), intent (inout) :: Object
  character(kind=OOOGglob_kcha,len=*), intent (in) :: chrPath
                                                                call OOOGglob_subSetProcedures ("OOOPstpaS_chrPath")
  Object % m_chrPath = chrPath
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPstpaS_chrPath
!**********
subroutine OOOPstpaG_chrPath (Object, chrPath)
  type (OOOPstpa_adtStartPath), intent (in) :: Object
  character(kind=OOOGglob_kcha,len=*), intent (out) :: chrPath
                                                                call OOOGglob_subSetProcedures ("OOOPstpaG_chrPath")
  chrPath = Object % m_chrPath
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPstpaG_chrPath
!__________________________________________________________







!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************
! Program Logic:   *
!*******************
!___________________________________________________________
!
subroutine OOOPstpa_LoadPath (Object)
  ! method, loads the start-Path from file
  type (OOOPstpa_adtStartPath), intent (inout) :: Object
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenStatus = 'OLD'
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenAccess  = 'SEQUENTIAL'
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenForm = 'FORMATTED'
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenPosition = 'REWIND'
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenAction = 'read'
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenBlank = 'NULL'
  !
  ! for inquire:
  logical(kind=OOOGglob_klog) :: logExist
  integer(OOOGglob_kint) :: intRecl = 0
  !
  integer(OOOGglob_kint) :: FileStatus = 0 ! File-error-status
  character(kind=OOOGglob_kcha,len=OOOGglob_Len200) :: chrPathAndFileName = ""
  integer(OOOGglob_kint) :: FileUnit = 0
  !
                                                                call OOOGglob_subSetProcedures ("OOOPstpa_LoadPath")
  !
  chrPathAndFileName = 'start.txt' ! the file must be in the program directory
  FileUnit = OOOGglob_FileUnitA
  !
  ! check the existence of the file:
  inquire (file=chrPathAndFileName, exist=logExist)
  if (.NOT.logExist) then
    ! File does not exist
                                                                call IIstpa_ErrorHandler (Object, &
                                                                  "inquire: File does not exist", &
                                                                  OOOGglob_warning, OOOGglob_NoErrNumber)
                                                                call OOOGglob_subResetProcedures
    return
  end if
  !
  open (unit=FileUnit, iostat=FileStatus, file=trim(chrPathAndFileName), &
      status=trim(OpenStatus), access=trim(OpenAccess), form=trim(OpenForm), &
      position=trim(OpenPosition), action=trim(OpenAction), &
      blank=trim(OpenBlank), delim='APOSTROPHE')
                                                                if (FileStatus .NE. 0) then
                                                                  call IIstpa_ErrorHandler (Object, "File-Open-error", &
                                                                    OOOGglob_error, FileStatus)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if

  !
  read (unit=FileUnit, fmt=*, iostat=FileStatus) Object % m_chrPath
                                                                if (FileStatus .NE. 0) then
                                                                  call IIstpa_ErrorHandler (Object, "File-Read-error 1", &
                                                                    OOOGglob_error, FileStatus)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
 !
  close (unit=FileUnit, iostat=FileStatus, status='KEEP')
                                                                if (FileStatus .NE. 0) then
                                                                  call IIstpa_ErrorHandler (Object, "File-Close-error", &
                                                                    OOOGglob_error, FileStatus)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
 !
                                                                call OOOGglob_subResetProcedures
 !
end subroutine OOOPstpa_LoadPath
!___________________________________________________________






!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Error Handling:   *************************
!**********************************************************
!__________________________________________________________
!Private
subroutine IIstpa_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT-Routines
  type(OOOPstpa_adtStartPath), intent(inout) :: Object
  character(kind=1, len=*), intent(in) :: chrErrorDescription
  integer(OOOGglob_kint), intent(in) :: intErrorType ! 1=warning, 2=Severe System error
  integer(OOOGglob_kint), intent(in) :: intErrorNumber ! Run Time error Number (e.g. status)
  call OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
end subroutine IIstpa_ErrorHandler
!__________________________________________________________



end module OOOPstpa_admStartPath
