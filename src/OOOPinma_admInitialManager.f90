! https://github.com/MichaelSiehl/Atomic_Subroutines--How_the_Parallel_Codes_may_look_like--Part_2

module OOOPinma_admInitialManager
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPinma
!********************************************************
! Abstract Data Type (ADT):         OOOPinma_adtInitialManager
! Abstract Data Type Module (adm):  OOOPinma_admInitialManager.f90
!********************************************************
! Purpose:                    InitialManager-Object
! Language:                   mainly Fortran 95 with Fortran 2008 coarrays
! Programmer:                 Michael Siehl
! Date:                       March 2017
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
use OOOPstpa_admStartPath ! to read the start path from file
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
public :: OOOPinmaS_intNumberOfTeamManagers, OOOPinmaG_intNumberOfTeamManagers
!
!
!*******************
! ADT-Management: **
!*******************
public :: OOOPinma_StructureConstructor
!___________________________________________________________
!
!********************
!** Program Logic: **
!********************
public :: OOOPinma_Start
private :: IIinma_LoadTeamManagers, IIinma_ActivateTeamManagerImages
!___________________________________________________________
!
!*********************
!** Error Handling: **
!*********************
private :: IIinma_ErrorHandler
!___________________________________________________________
!
!*********************
!**  Enumerations:  **
!*********************

!___________________________________________________________
!
!***************************************
!***  Type Definition: *****************
!***************************************
type, public :: OOOPinma_adtInitialManager
  private
  !****
  integer(OOOGglob_kint) :: m_intNumberOfTeamManagers = 0
  !***** array containing the image numbers of the TeamManager images:
  integer(OOOGglob_kint), dimension (1:OOOGglob_TeamManagers_UpperBound) :: mA_intTeamManagerImages99
  !***** array containing the file names of the TeamMembers files for each TeamManager:
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40), dimension (1:OOOGglob_TeamManagers_UpperBound) :: mA_chrTeamMembersFiles99
  !*****
  type (OOOEerroc_colError) :: m_UUerrocError ! error-Collection
  !
end type OOOPinma_adtInitialManager
!__________________________________________________________
!
!****************************************************
!***  Corresponding Local Object Declaration:  ******
!****************************************************
!***
type (OOOPinma_adtInitialManager), public, save :: OOOPinmaInitialManager_1
!
!___________________________________________________________




contains


!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************************
! access routines for scalar   *
! and static array members:    *
!*******************************
!__________________________________________________________
!
subroutine OOOPinmaS_intNumberOfTeamManagers (Object, intNumberOfTeamManagers)
  type (OOOPinma_adtInitialManager), intent (inout) :: Object
  integer(OOOGglob_kint), intent (in) :: intNumberOfTeamManagers
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures ("OOOPinmaS_intNumberOfTeamManagers")
                                                                !
                                                                if (intNumberOfTeamManagers > OOOGglob_TeamManagers_UpperBound) then
                                                                  call IIinma_ErrorHandler (Object, "to many elements", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
  !
  Object % m_intNumberOfTeamManagers = intNumberOfTeamManagers
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPinmaS_intNumberOfTeamManagers
!**********
subroutine OOOPinmaG_intNumberOfTeamManagers (Object, intNumberOfTeamManagers)
  type (OOOPinma_adtInitialManager), intent (in) :: Object
  integer(OOOGglob_kint), intent (out) :: intNumberOfTeamManagers
                                                                call OOOGglob_subSetProcedures ("OOOPinmaG_intNumberOfTeamManagers")
  intNumberOfTeamManagers = Object % m_intNumberOfTeamManagers
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPinmaG_intNumberOfTeamManagers
!__________________________________________________________


!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************
! ADT-Management:  *
!*******************
!___________________________________________________________

subroutine OOOPinma_StructureConstructor (Object)
  ! structure constructor
  type (OOOPinma_adtInitialManager), intent (inout) :: Object
  !
                                                                call OOOGglob_subSetProcedures ("OOOPinma_StructureConstructor")
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPinma_StructureConstructor
!___________________________________________________________






!##################################################################################################
!##################################################################################################
!##################################################################################################

!*******************
! Program Logic:   *
!*******************
!___________________________________________________________

subroutine OOOPinma_Start (Object)
  use OOOPimsc_admImageStatus_CA ! access corresponding coarrays to
                                 ! communicate with remote or local PGAS memory
  type (OOOPinma_adtInitialManager), intent (inout) :: Object
  !
                                                                call OOOGglob_subSetProcedures ("OOOPinma_Start")
  !
  call OOOPinma_StructureConstructor (Object)
  !
  !*******
  call IIinma_LoadTeamManagers (Object) ! from TeamManagers.txt
  !
  ! activate the TeamManagers on the (remote) images as given by TeamManagers.txt:
  call IIinma_ActivateTeamManagerImages (Object)
  !
  ! communicate with local PGAS memory to finish image execution:
  call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, OOOPimscEnum_ImageActivityFlag % &
                                    ExecutionFinished, this_image(), logExecuteSyncMemory = .false.)
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPinma_Start
!___________________________________________________________
!
subroutine IIinma_LoadTeamManagers (Object)
  ! method, loads the data from TeamManagers.txt
  type (OOOPinma_adtInitialManager), intent (inout) :: Object
  !
  type (OOOPstpa_adtStartPath) :: UUStartPath1
  !
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenStatus = 'OLD'
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenAccess  = 'SEQUENTIAL'
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenForm = 'FORMATTED'
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenPosition = 'REWIND'
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenAction = 'read'
  character(kind=OOOGglob_kcha,len=OOOGglob_Len40) :: OpenBlank = 'NULL'
  !
  character(kind=OOOGglob_kcha,len=OOOGglob_Len200) :: chrStartPath
  !
  ! for inquire:
  logical(kind=OOOGglob_klog) :: logExist
  integer(OOOGglob_kint) :: intRecl = 0
  !
  integer(OOOGglob_kint) :: FileUnit = 0
  integer(OOOGglob_kint) :: FileStatus = 0 ! File-error-status
  character(kind=OOOGglob_kcha,len=OOOGglob_Len200) :: chrPathAndFileName = ""
  integer(OOOGglob_kint) :: intCounter ! do loop counter
  integer(OOOGglob_kint) :: intNumberOfTeamManagers
  !
                                                                call OOOGglob_subSetProcedures ("IIinma_LoadTeamManagers")
  !
  FileUnit = OOOGglob_FileUnitA
  !
  call OOOPstpa_LoadPath (UUStartPath1)
  call OOOPstpaG_chrPath (UUStartPath1, chrStartPath)
  !
  chrPathAndFileName = trim(chrStartPath) // 'TeamManagers.txt'
  !
  open (unit=FileUnit, iostat=FileStatus, file=trim(chrPathAndFileName), &
      status=trim(OpenStatus), access=trim(OpenAccess), form=trim(OpenForm), &
      position=trim(OpenPosition), action=trim(OpenAction), &
      blank=trim(OpenBlank), delim='APOSTROPHE')
                                                                !
                                                                if (FileStatus /= 0) then
                                                                  call IIinma_ErrorHandler (Object, "File Open-error", &
                                                                    OOOGglob_error, FileStatus)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
  !
  read (unit=FileUnit, fmt=*, iostat=FileStatus) intNumberOfTeamManagers
                                                                !
                                                                if (FileStatus /= 0) then
                                                                  call IIinma_ErrorHandler (Object, "File read-Error 1", &
                                                                    OOOGglob_error, FileStatus)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
  !
  call OOOPinmaS_intNumberOfTeamManagers (Object, intNumberOfTeamManagers) ! contains error handling
  !
  ! read the TeamManagerImages and there TeamMembers file name from the TeamManagers.txt file
  do intCounter = 1, Object % m_intNumberOfTeamManagers
    read (unit=FileUnit, fmt=*, iostat=FileStatus) Object % mA_intTeamManagerImages99 (intCounter), &
      Object % mA_chrTeamMembersFiles99 (intCounter)
    !
                                                                if (FileStatus /= 0) then
                                                                  ! FileStatus error
                                                                  call IIinma_ErrorHandler (Object, "File read-Error 2", &
                                                                    OOOGglob_error, FileStatus)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
  end do
  !
  close (unit=FileUnit, iostat=FileStatus, status='KEEP')
                                                                if (FileStatus /= 0) then
                                                                  call IIinma_ErrorHandler (Object, "File-Close-error", &
                                                                    OOOGglob_error, FileStatus)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
  !
                                                                call OOOGglob_subResetProcedures
  !
end subroutine IIinma_LoadTeamManagers
!___________________________________________________________
!
subroutine IIinma_ActivateTeamManagerImages (Object)
  !!!  synchronization counterpart routine  !!!!
  !!!  for IIimma_SYNC_CheckActivityFlag    !!!!
  ! activate the TeamManagers on the (remote) images (as given by TeamManagers.txt)
  !
  ! use OOOPimmc_admImageManager_CA ! access corresponding coarrays to
  use OOOPimsc_admImageStatus_CA  ! communicate with remote or local PGAS memory
  !
  type (OOOPinma_adtInitialManager), intent (in) :: Object
  integer(OOOGglob_kint), dimension (Object % m_intNumberOfTeamManagers) :: intA_RemoteImageNumbers
  integer(OOOGglob_kint) :: intNumberOfTeamManagerImages
  integer(OOOGglob_kint) :: intCounter
  !
                                                          call OOOGglob_subSetProcedures ("IIinma_ActivateTeamManagerImages")
  !
  intNumberOfTeamManagerImages = Object % m_intNumberOfTeamManagers
  !
  ! fill the intA_RemoteImageNumbers array with the (remote) image numbers:
  do intCounter = 1, intNumberOfTeamManagerImages
    ! (Object % mA_intTeamManagerImages99(intCounter) gives the (remote) image number):
    intA_RemoteImageNumbers(intCounter) = Object % mA_intTeamManagerImages99(intCounter)
  end do
  !***************************************************************
  ! this starts the segment synchronization  routine on image 1:
  call OOOPimsc_SynchronizeSegmentOrdering_CA (OOOPimscImageStatus_CA_1, intNumberOfTeamManagerImages, intA_RemoteImageNumbers)
  !
                                                                call OOOGglob_subResetProcedures
end subroutine IIinma_ActivateTeamManagerImages
!___________________________________________________________





!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Error Handling:   *************************
!**********************************************************
!__________________________________________________________

!Private
subroutine IIinma_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT-Routines
  type(OOOPinma_adtInitialManager), intent(inout) :: Object
  character(kind=1, len=*), intent(in) :: chrErrorDescription
  integer(OOOGglob_kint), intent(in) :: intErrorType ! 1=warning, 2=Severe System error
  integer(OOOGglob_kint), intent(in) :: intErrorNumber ! Run Time error Number (e.g. status)
  call OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
end subroutine IIinma_ErrorHandler
!__________________________________________________________



end module OOOPinma_admInitialManager
