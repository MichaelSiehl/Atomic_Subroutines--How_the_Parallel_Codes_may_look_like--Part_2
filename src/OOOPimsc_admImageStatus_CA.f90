! https://github.com/MichaelSiehl/Atomic_Subroutines--How_the_Parallel_Codes_may_look_like--Part_2

module OOOPimsc_admImageStatus_CA
!************************************************
! Namespace: OOOP - Parallel
!************************************************
! Abstact Data Type Short Name: OOOPimsc
!********************************************************
! Abstract Data Type (ADT):         OOOPimsc_adtImageStatus_CA
! Abstract Data Type Module (adm):  OOOPimsc_admImageStatus_CA.f90
!********************************************************
! Purpose:                    ImageStatus_CA-Object
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
use, intrinsic :: iso_fortran_env
!___________________________________________________________

implicit none
!___________________________________________________________

private
!__________________________________________________________
!
! access routines for scalar
! and static array members:
public :: OOOPimscSAElement_atomic_intImageActivityFlag99_CA, &
                             OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA
!***
! logic codes:
public :: OOOPimsc_subSyncMemory
public :: OOOPimsc_SynchronizeSegmentOrdering_CA
private :: OOOPimsc_SynchronizeTheInvolvedImages_CA
public :: OOOPimsc_Start_SegmentSynchronization_CA
!***
! coarray ADT management:
public :: OOOPimsc_StructureConstructor_CA
!___________________________________________________________
!
!***************************
!****  Error Handling:  ****
!***************************
!***
! local ADT:
private :: IIimsc_ErrorHandler
!***
! coarray ADT:
private :: IIimsc_ImageNumberBoundError_CA
!___________________________________________________________
!
!************************
!****  Enumerations:  ***
!************************
!***  ImageActivityFlag:
type, private :: OOOPimsc_DontUse1
  integer(kind=OOOGglob_kint) :: Enum_StepWidth ! = 1000000
  integer(kind=OOOGglob_kint) :: InitialWaiting ! = 2000000
  integer(kind=OOOGglob_kint) :: TeamManager ! = 3000000
  integer(kind=OOOGglob_kint) :: TeamMember ! = 4000000
  integer(kind=OOOGglob_kint) :: ExecutionFinished ! = 5000000
  integer(kind=OOOGglob_kint) :: InitiateSegmentSynchronization ! = 6000000
  integer(kind=OOOGglob_kint) :: WaitForSegmentSynchronization ! = 7000000
  integer(kind=OOOGglob_kint) :: ContinueSegmentSynchronization ! = 8000000
  integer(kind=OOOGglob_kint) :: SendetCurrentSegmentNumber ! = 9000000
  integer(kind=OOOGglob_kint) :: DoSegmentSynchronization ! = 10000000
  integer(kind=OOOGglob_kint) :: FinishedSegmentSynchronization ! = 11000000
  integer(kind=OOOGglob_kint) :: Enum_MaxValue ! = 12000000
end type OOOPimsc_DontUse1
!
type (OOOPimsc_DontUse1), public, parameter :: OOOPimscEnum_ImageActivityFlag &
     = OOOPimsc_DontUse1 (1000000,2000000,3000000,4000000,5000000,6000000,7000000,8000000, 9000000, &
                           10000000, 11000000, 12000000)
!___________________________________________________________
!
!***************************************
!***  Type Definition: *****************
!***************************************
type, public :: OOOPimsc_adtImageStatus_CA
  private
  !*****
  integer(atomic_int_kind), dimension (1:OOOGglob_NumImages_UpperBound, 1:2) :: &
                  mA_atomic_intImageActivityFlag99 = OOOPimscEnum_ImageActivityFlag % InitialWaiting
  !*****
  integer(atomic_int_kind), dimension (1:OOOGglob_NumImages_UpperBound) :: mA_atomic_intImageSyncMemoryCount99 = 0
  !*****
  type (OOOEerroc_colError) :: m_UUerrocError ! error collection
  !
end type OOOPimsc_adtImageStatus_CA
!___________________________________________________________
!
!****************************************************
!***  Corresponding Coarray Declaration:  ***********
!****************************************************
!***
type (OOOPimsc_adtImageStatus_CA), public, codimension[*], save :: OOOPimscImageStatus_CA_1
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
!**********
subroutine OOOPimsc_PackEnumValue_ImageActivityFlag (Object_CA, intEnumValue, intAdditionalValue, intPackedEnumValue)
  ! pack the both integer input arguments into a single integer scalar
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intEnumValue
  integer(OOOGglob_kint), intent (in) :: intAdditionalValue
  integer(OOOGglob_kint), intent (out) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intEnum_StepWidth
  integer(OOOGglob_kint) :: status
  !
                                                                call OOOGglob_subSetProcedures &
                                                              ("OOOPimsc_PackEnumValue_ImageActivityFlag")
  !
                                                                ! check if intAdditionalValue argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intEnum_StepWidth = OOOPimscEnum_ImageActivityFlag % &
                                                                  Enum_StepWidth
                                                                if (intAdditionalValue >= intEnum_StepWidth) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intAdditionalValue is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
  !
  intPackedEnumValue = intEnumValue + intAdditionalValue
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_PackEnumValue_ImageActivityFlag
!
!**********
subroutine OOOPimsc_UnpackEnumValue (intPackedEnumValue, intEnum_StepWidth, &
                             intUnpackedEnumValue, intUnpackedAdditionalValue)
  ! unpack the integer enum value into two integer scalars
  integer(OOOGglob_kint), intent (in) :: intPackedEnumValue
  integer(OOOGglob_kint), intent (in) :: intEnum_StepWidth
  integer(OOOGglob_kint), intent (out) :: intUnpackedEnumValue
  integer(OOOGglob_kint), intent (out) :: intUnpackedAdditionalValue
  !
                                                                call OOOGglob_subSetProcedures &
                                                                   ("OOOPimsc_UnpackEnumValue")
  !
  intUnpackedAdditionalValue = mod(intPackedEnumValue, intEnum_StepWidth)
  !
  intUnpackedEnumValue = intPackedEnumValue - intUnpackedAdditionalValue
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_UnpackEnumValue
!
!**********
!__________________________________________________________
!
subroutine OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                                intImageNumber, intArrayIndex, logExecuteSyncMemory)
  ! set an array element atomically
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
!  type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intImageActivityFlag
  integer(OOOGglob_kint), intent (in) :: intImageNumber ! the (remote) image number
  integer(OOOGglob_kint), optional, intent (in) :: intArrayIndex
  integer(OOOGglob_kint) :: intArrIndex
  integer(OOOGglob_kint) :: intMaxVal
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimscSAElement_atomic_intImageActivityFlag99_CA")
  !
  if (present(intArrayIndex)) then
                                                                ! check if intArrayIndex argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intMaxVal = OOOGglob_NumImages_UpperBound
                                                                if (intArrayIndex .gt. intMaxVal) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intArrayIndex is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
    intArrIndex = intArrayIndex
  else ! default:
    intArrIndex = intImageNumber
  end if
  !
  !
  if (present(logExecuteSyncMemory)) then
    logSyncMemoryExecution = logExecuteSyncMemory
  else ! default:
    logSyncMemoryExecution = .true.
  end if
  !
  !
  if (intImageNumber == this_image()) then ! local atomic define
    ! don't execute sync memory for local atomic_define:
    call atomic_define(Object_CA % mA_atomic_intImageActivityFlag99(intArrIndex,1), intImageActivityFlag)
  !
  else ! remote atomic define
                                                                ! check if the image number is valid:
                                                                if (IIimsc_ImageNumberBoundError_CA &
                                                                (Object_CA, intImageNumber)) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "no valid image number", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
    ! execute sync memory for remote atomic_define:
    if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory

! the following generates 'error #8583: COARRAY argument of ATOMIC_DEFINE/ATOMIC_REF intrinsic subroutine shall be a coarray.'
! with ifort 18 beta:
    call atomic_define(Object_CA [intImageNumber] % mA_atomic_intImageActivityFlag99(intArrIndex,1), intImageActivityFlag)

! the following generates the correct 'error #6360: A scalar-valued argument is required in this context.'
! with ifort 18 beta:
!    call atomic_define(Object_CA [intImageNumber] % mA_atomic_intImageActivityFlag99, intImageActivityFlag)

! the following does not generate an error with ifort 18 beta:
!    call atomic_define(Object_CA % mA_atomic_intImageActivityFlag99(intArrIndex,1), intImageActivityFlag)

!
  end if
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimscSAElement_atomic_intImageActivityFlag99_CA
!
!**********
!
logical(OOOGglob_klog) function OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA (Object_CA, intCheckImageActivityFlag, &
                                                          intArrayIndex, intAdditionalAtomicValue, logExecuteSyncMemory)
  ! check array element atomically
  ! in order to hide the sync memory statement herein, this Getter does not allow
  ! to access the member directly, but instead does only allow to check the atomic member
  ! for specific values (this Getter is intented for synchronizations)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
 ! type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intCheckImageActivityFlag
  integer(OOOGglob_kint), optional, intent (in) :: intArrayIndex
  integer(OOOGglob_kint) :: intArrIndex
  integer(OOOGglob_kint) :: intMaxVal
  integer(OOOGglob_kint), optional, intent (out) :: intAdditionalAtomicValue
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intEnum_StepWidth
  integer(OOOGglob_kint) :: intUnpackedEnumValue
  integer(OOOGglob_kint) :: intUnpackedAdditionalValue
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: status = 0 ! error status
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA")
  !
  OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA = .false.
  !
  if (present(intArrayIndex)) then
                                                                ! check if intArrayIndex argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intMaxVal = OOOGglob_NumImages_UpperBound
                                                                if (intArrayIndex .gt. intMaxVal) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intArrayIndex is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
    intArrIndex = intArrayIndex
  else ! default:
    intArrIndex = this_image()
  end if
  !
  if (present(logExecuteSyncMemory)) then
    logSyncMemoryExecution = logExecuteSyncMemory
  else ! default:
    logSyncMemoryExecution = .true.
  end if
  !
  ! access an array element in local PGAS memory atomically:
  call atomic_ref(intImageActivityFlag, Object_CA % mA_atomic_intImageActivityFlag99(intArrIndex,1))
  ! unpack the intImageActivityFlag value:
  intPackedEnumValue = intImageActivityFlag
  intEnum_StepWidth = OOOPimscEnum_ImageActivityFlag % Enum_StepWidth
  call OOOPimsc_UnpackEnumValue (intPackedEnumValue, intEnum_StepWidth, &
                             intUnpackedEnumValue, intUnpackedAdditionalValue)
  intImageActivityFlag = intUnpackedEnumValue
  !
  if (intCheckImageActivityFlag == intImageActivityFlag) then
    if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
    OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA = .true.
  end if
  !
  if (present(intAdditionalAtomicValue)) then
    intAdditionalAtomicValue = intUnpackedAdditionalValue
  end if
  !
                                                                call OOOGglob_subResetProcedures
end function OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA
!
!**********
!
subroutine OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA (Object_CA, intCheckImageActivityFlag, &
                  intNumberOfImages, intA_RemoteImageNumbers, logArrayIndexIsThisImage, &
                  intA_RemoteImageAndItsAdditionalAtomicValue, logExecuteSyncMemory)
  ! This routine is for atomic bulk synchronization (among the executing image and one or more remote images)
  ! using a spin-wait loop synchronizaton. Thus, the procedure implements a customized synchronization
  ! routine using atomic subroutines and the sync memory statement. Ordered execution segments among the involved images
  ! are not required.
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intCheckImageActivityFlag
  integer(OOOGglob_kint), intent (in) :: intNumberOfImages ! these are the number of involved remote images
  integer(OOOGglob_kint), dimension (1:intNumberOfImages), intent (in) :: intA_RemoteImageNumbers
  logical(OOOGglob_klog), optional, intent (in) :: logArrayIndexIsThisImage
  logical(OOOGglob_klog) :: logArrIndexIsThisImage
  integer(OOOGglob_kint) :: intArrIndex
  integer(OOOGglob_kint), optional, dimension (1:intNumberOfImages, 1:2), intent (out) :: &
                                                       intA_RemoteImageAndItsAdditionalAtomicValue
  integer(OOOGglob_kint) :: intCount
  integer(OOOGglob_kint) :: intImageNumber
  logical(OOOGglob_klog), dimension (1:intNumberOfImages) :: logA_CheckImageStates
  integer(OOOGglob_kint) :: intAtomicValue = 0
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: intMaxVal
  integer(OOOGglob_kint) :: status = 0 ! error status
  !
                                                                call OOOGglob_subSetProcedures &
                                            ("OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA")
  !
  !**********************************************************************
  !****
  if (present(logArrayIndexIsThisImage)) then
    logArrIndexIsThisImage = logArrayIndexIsThisImage
  else ! default:
    logArrIndexIsThisImage = .false.
  end if
  !****
  if (present(logExecuteSyncMemory)) then
    logSyncMemoryExecution = logExecuteSyncMemory
  else ! default:
    logSyncMemoryExecution = .true.
  end if
  !****
  ! initialize the array elements with .false.:
  logA_CheckImageStates = .false.
  !
  !**********************************************************************
  ! wait until all the involved remote image(s) do signal that they are in state intCheckImageActivityFlag
  ! spin-wait loop synchronization:
  do
    do intCount = 1, intNumberOfImages
      !
      intImageNumber = intA_RemoteImageNumbers(intCount)
      intArrIndex = intImageNumber ! but:
        if (logArrIndexIsThisImage) intArrIndex = this_image()
      if (intImageNumber .ne. this_image()) then ! (synchronization is only required between distinct images)
        if (.not. logA_CheckImageStates(intCount)) then ! check is only required if the remote image is not already
                                                        ! in state intCheckImageActivityFlag:
          !
          if (OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, &
                           intCheckImageActivityFlag, intArrayIndex = intArrIndex, &
                           intAdditionalAtomicValue = intAtomicValue, logExecuteSyncMemory = .false.)) then
            logA_CheckImageStates(intCount) = .true. ! the remote image is in state intCheckImageActivityFlag
            !
            if (present(intA_RemoteImageAndItsAdditionalAtomicValue)) then
            ! save the remote image number together with its sent AdditionalAtomicValue:
              intA_RemoteImageAndItsAdditionalAtomicValue(intCount,1) = intImageNumber
              intA_RemoteImageAndItsAdditionalAtomicValue(intCount,2) = intAtomicValue
            end if
          end if
        end if
      end if
    end do
    !
    if (all(logA_CheckImageStates)) then ! all involved remote images are in state intCheckImageActivityFlag
      if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
      exit ! exit the do loop if all involved remote images are in state
                                         ! intCheckImageActivityFlag
    end if
    ! (be aware: due to the first if statement, this would be error prone in real world programming,
    !  but it is safe for this example program)
  end do
  !
  !**********************************************************************
  !
                                                                call OOOGglob_subResetProcedures
  !
end subroutine OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA
!
!__________________________________________________________
!
!**********
subroutine OOOPimsc_subSyncMemory (Object_CA)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  ! encapsulates access to SYNC MEMORY
                                                                call OOOGglob_subSetProcedures &
                                                                 ("OOOPimsc_subSyncMemory")
  sync memory
  ! increment the ImageSyncMemoryCount to track the execution segment order
  ! on the executing image:
  call OOOPimscSAElement_atomic_incr_intImageSyncMemoryCount99_CA (Object_CA)
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_subSyncMemory
!
!**********
! private:
subroutine OOOPimscSAElement_atomic_incr_intImageSyncMemoryCount99_CA (Object_CA)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint) :: intTemp
  integer(OOOGglob_kint) :: status = 0 ! error status
integer(OOOGglob_kint) :: test
  !
                                                                call OOOGglob_subSetProcedures &
                                                            ("OOOPimscSAElement_atomic_incr_intImageSyncMemoryCount99_CA")
  !
  ! increment (by 1) the ImageSyncMemoryCount member atomically on the executing image only:
  ! every image uses its own array index (this_image())
  !
  ! Fortran 2015 syntax:
  !call atomic_add(Object_CA % mA_atomic_intImageSyncMemoryCount99(this_image()), 1)
  !
  ! Fortran 2008 syntax:
  call atomic_ref(intTemp, Object_CA % mA_atomic_intImageSyncMemoryCount99 (this_image()))
  intTemp = intTemp + 1
  ! don't execute sync memory for local atomic_define:
  call atomic_define(Object_CA % mA_atomic_intImageSyncMemoryCount99 (this_image()), intTemp)
!
! test:
call atomic_ref(test, Object_CA % mA_atomic_intImageSyncMemoryCount99 (this_image()))
write(*,*) 'entering execution segment', test, 'on image', this_image()
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimscSAElement_atomic_incr_intImageSyncMemoryCount99_CA
!**********
! private:
subroutine OOOPimscGAElement_atomic_intImageSyncMemoryCount99_CA (Object_CA, intSyncMemoryCount, intArrayIndex) !, &
!                                                                        intArrayIndexForLocalArrayAccess)
  ! get only one array element on the executing image
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (out) :: intSyncMemoryCount
  integer(OOOGglob_kint), optional, intent (in) :: intArrayIndex
  integer(OOOGglob_kint) :: intArrIndex
  integer(OOOGglob_kint) :: intMaxVal
  integer(OOOGglob_kint) :: status = 0 ! error status
!  integer(OOOGglob_kint), optional, intent (in) :: intArrayIndexForLocalArrayAccess ! 161201
  !
                                                                call OOOGglob_subSetProcedures &
                                                                  ("OOOPimscGAElement_atomic_intImageSyncMemoryCount99_CA")
  if (present(intArrayIndex)) then
                                                                ! check if intArrayIndex argument is to large:
                                                                ! ToDo: check if it is negative
                                                                intMaxVal = OOOGglob_NumImages_UpperBound
                                                                if (intArrayIndex .gt. intMaxVal) then
                                                                  call IIimsc_ErrorHandler (Object_CA, &
                                                                       "intArrayIndex is to large", &
                                                                    OOOGglob_error, status)
                                                                  call OOOGglob_subResetProcedures
                                                                  return
                                                                end if
                                                                !
    intArrIndex = intArrayIndex
  else ! default:
    intArrIndex = this_image() ! ToDo: this subroutine should only be used with this default
  end if
  ! get the array element:
  call atomic_ref(intSyncMemoryCount, Object_CA % mA_atomic_intImageSyncMemoryCount99 (intArrIndex))
  ! no SYNC MEMORY statement here, because this call to atomic_ref is not used for synchronization and
  ! thus, this is not an atomic checker routine
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimscGAElement_atomic_intImageSyncMemoryCount99_CA
!*************
!___________________________________________________________
!##########################################################################################
! The parallel logic codes:  ##############################################################
!##########################################################################################
!___________________________________________________________
!
subroutine OOOPimsc_SynchronizeSegmentOrdering_CA (Object_CA, intNumberOfImages,intA_RemoteImageNumbers)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intNumberOfImages
  integer(OOOGglob_kint), dimension (intNumberOfImages), intent (in) :: intA_RemoteImageNumbers
  !
                                                                call OOOGglob_subSetProcedures &
                                                            ("OOOPimsc_SynchronizeSegmentOrdering_CA")
  !
  call OOOPimsc_SynchronizeTheInvolvedImages_CA (Object_CA, intNumberOfImages,intA_RemoteImageNumbers)
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_SynchronizeSegmentOrdering_CA
!___________________________________________________________
! private
subroutine OOOPimsc_SynchronizeTheInvolvedImages_CA (Object_CA, intNumberOfImages,intA_RemoteImageNumbers)
  ! This routine is for stearing the execution segment synchronization (i.e. restoring of segment ordering)
  ! among a number of involved remote images. To do so, this routine gets executed on a separate coarray image
  ! (on image 1 with this example)
  !
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intNumberOfImages ! these are the number of involved remote images
  integer(OOOGglob_kint), dimension (intNumberOfImages), intent (in) :: intA_RemoteImageNumbers
  integer(OOOGglob_kint) :: status = 0 ! error status
  integer(OOOGglob_kint) :: intCount
  integer(OOOGglob_kint) :: intImageNumber
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint) :: intSetFromImageNumber = 0
  logical(OOOGglob_klog), dimension (intNumberOfImages) :: logA_CheckImageStates
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intCurrentSegmentCount = 0
  integer(OOOGglob_kint), dimension (1:intNumberOfImages, 1:2) :: intA_RemoteImageAndSegmentCounts
  integer(OOOGglob_kint) :: intMaxSegmentCount
  integer(OOOGglob_kint), dimension (1) :: intA_MaxSegmentCountLocation ! the array index
  integer(OOOGglob_kint), dimension (1) :: intA_ImageNumberWithMaxSegmentCount
  integer(OOOGglob_kint) :: intLocalSyncMemoryCount
  !
                                                                call OOOGglob_subSetProcedures &
                                                            ("OOOPimsc_SynchronizeTheInvolvedImages_CA")
  !
  !************************************************
  ! (1) initiate segment synchronization on the involved remote images:
  ! (counterpart synchronization routine is IIimma_SYNC_CheckActivityFlag)
  !
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % InitiateSegmentSynchronization
  ! pack the ImageActivityFlag enumeration together with this_image():
  call OOOPimsc_PackEnumValue_ImageActivityFlag (Object_CA, intImageActivityFlag, this_image(), intPackedEnumValue)
  !
  call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
  do intCount = 1, intNumberOfImages
    !
    intImageNumber = intA_RemoteImageNumbers(intCount)
    if (intImageNumber .ne. this_image()) then ! (synchronization is only required between distinct images)
    ! initiate the segment synchronization on the involved remote images:
      ! send the packed enum value atomically to the remote image (intImageNumber):
      ! (counterpart synchronization routine is IIimma_SYNC_CheckActivityFlag)
      call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intPackedEnumValue, &
            intImageNumber, logExecuteSyncMemory = .false.) ! do not execute SYNC MEMORY
    end if
  end do
  !
  !************************************************
  ! (2) wait until all the involved remote image(s) do signal that they are in state WaitForSegmentSynchronization:
  ! (counterpart routine is OOOPimsc_Start_SegmentSynchronization_CA)
  !
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % WaitForSegmentSynchronization
  ! spin-wait loop synchronization:
  call OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                         intNumberOfImages, intA_RemoteImageNumbers)
  !
  !**********************************************************************
  ! (3) set the involved remote images to state ContinueSegmentSynchronization:
  ! (counterpart synchronization routine is OOOPimsc_WaitForSegmentSynchronization_CA)
  !
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % ContinueSegmentSynchronization
  ! pack the ImageActivityFlag enumeration together with this_image():
  call OOOPimsc_PackEnumValue_ImageActivityFlag (Object_CA, intImageActivityFlag, this_image(), intPackedEnumValue)
  !
  call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
  do intCount = 1, intNumberOfImages
    !
    intImageNumber = intA_RemoteImageNumbers(intCount)
    if (intImageNumber .ne. this_image()) then ! (synchronization is only required between distinct images)
    ! to continue the segment synchronization on the involved remote images:
      ! send the packed enum value atomically to the remote image (intImageNumber):
      call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intPackedEnumValue, &
            intImageNumber, logExecuteSyncMemory = .false.) ! do not execute SYNC MEMORY
    end if
  end do
  !
  !**********************************************************************
  ! (4) wait until all the involved remote image(s) do signal that they are in state SendetCurrentSegmentNumber:
  ! (counterpart routine is OOOPimsc_WaitForSegmentSynchronization_CA)
  !
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % SendetCurrentSegmentNumber
  ! spin-wait loop synchronization:
  call OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                         intNumberOfImages, intA_RemoteImageNumbers, &
                         intA_RemoteImageAndItsAdditionalAtomicValue = intA_RemoteImageAndSegmentCounts)
  !
  !**********************************************************************
  ! (5) get the max segment (sync memory) count (only the remote images):
  !
  intMaxSegmentCount = maxval(intA_RemoteImageAndSegmentCounts(:,2))
write(*,*)'MaxSegmentCount (MaxSyncMemoryCount): ', intMaxSegmentCount
  intA_MaxSegmentCountLocation = maxloc(intA_RemoteImageAndSegmentCounts(:,2))
  intA_ImageNumberWithMaxSegmentCount = intA_RemoteImageAndSegmentCounts (intA_MaxSegmentCountLocation,1)
write(*,*)'ImageNumberWithMaxSegmentCount: ', intA_ImageNumberWithMaxSegmentCount
  !
  !**********************************************************************
  ! (5a) get the segment (sync memory) count on this image (not required for this example program):
  call OOOPimscGAElement_atomic_intImageSyncMemoryCount99_CA (Object_CA, intLocalSyncMemoryCount)
  !
  !**********************************************************************
  ! (6) initiate that the remote images do restore segment ordering:
  ! (restore the segment order among the remote images only for this example)
  ! to do so, set the involved remote images to state DoSegmentSynchronization:
  ! (counterpart synchronization routine is OOOPimsc_DoSegmentSynchronization_CA)
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % DoSegmentSynchronization
  !
  ! increment intMaxSegmentCount by 1 because the remote images will execute an
  ! additional sync memory statement when receiving the atomic value from the do loop below:
  intMaxSegmentCount = intMaxSegmentCount + 1
  !
  ! pack the ImageActivityFlag enumeration together with the intMaxSegmentCount:
  call OOOPimsc_PackEnumValue_ImageActivityFlag (Object_CA, intImageActivityFlag, intMaxSegmentCount, intPackedEnumValue)
  !
  call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
  !
  do intCount = 1, intNumberOfImages
    !
    intImageNumber = intA_RemoteImageNumbers(intCount)
    if (intImageNumber .ne. this_image()) then ! (synchronization is only required between distinct images)
    ! to execute the segment synchronization on the involved remote images:
      ! send the packed enum value atomically to the remote image (intImageNumber):
      call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intPackedEnumValue, &
            intImageNumber, logExecuteSyncMemory = .false.) ! do not execute SYNC MEMORY
    end if
  end do
  !
  !**********************************************************************
  ! (7) wait until all the involved remote image(s) do signal that they are in state FinishedSegmentSynchronization:
  ! (counterpart routine is OOOPimsc_DoSegmentSynchronization_CA)
  !
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % FinishedSegmentSynchronization
  ! spin-wait loop synchronization:
  call OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                         intNumberOfImages, intA_RemoteImageNumbers, &
                         intA_RemoteImageAndItsAdditionalAtomicValue = intA_RemoteImageAndSegmentCounts)

do intCount = 1, intNumberOfImages
  write(*,*) 'remote image number and its intCurrentSegmentCount:',intA_RemoteImageAndSegmentCounts(intCount,1:2)
end do
  !
  !**********************************************************************
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_SynchronizeTheInvolvedImages_CA
!___________________________________________________________
!
!#####################################################################################################################
!#####################################################################################################################
! From here, the routines are called from the ImageManager objects:
!___________________________________________________________
!
! public
subroutine OOOPimsc_Start_SegmentSynchronization_CA (Object_CA, intSetFromImageNumber)
  ! this routine starts the segment synchronization (restoring) on the involved inages
  ! (the involved images (not image 1) will execute this)
  ! (counterpart synchronization routine is IIimma_SYNC_CheckActivityFlag)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intSetFromImageNumber ! this is the remote image number (image 1)
                                                               ! which initiated the synchronization
  integer(OOOGglob_kint) :: status = 0 ! error status
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intRemoteImageNumber
  !
                                                                call OOOGglob_subSetProcedures &
                                                            ("OOOPimsc_Start_SegmentSynchronization_CA")
  !
  ! *********************************************************************
  ! start the segment synchronization (restoring) on the involved images:
  !
  intRemoteImageNumber = intSetFromImageNumber
  !
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % WaitForSegmentSynchronization
  !
  ! pack the ImageActivityFlag together with this_image():
  call OOOPimsc_PackEnumValue_ImageActivityFlag (Object_CA, intImageActivityFlag, this_image(), intPackedEnumValue)
  !
  ! signal to the remote image (image 1) that this image is now in state 'WaitForSegmentSychronization':
  ! (counterpart synchronization routine is OOOPimsc_SynchronizeTheInvolvedImages_CA)
  call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intPackedEnumValue, &
                         intRemoteImageNumber, intArrayIndex = this_image(), logExecuteSyncMemory = .true.)
  !
  call OOOPimsc_WaitForSegmentSynchronization_CA (Object_CA, intSetFromImageNumber) ! (the routine is below)
  !
  call OOOPimsc_DoSegmentSynchronization_CA (Object_CA, intSetFromImageNumber) ! (the routine is below)
  !
  ! finish execution on the executing image (only for this example and to avoid an error stop statement
  ! to terminate execution):
  call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, OOOPimscEnum_ImageActivityFlag % &
                                    ExecutionFinished, this_image(), logExecuteSyncMemory = .false.)
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_Start_SegmentSynchronization_CA
!__________________________________________________________
! public
subroutine OOOPimsc_WaitForSegmentSynchronization_CA (Object_CA, intSetFromImageNumber)
  ! Current image status is 'WaitForSegmentSynchronization',
  ! code execution on this image will be stopped until it is set to
  ! state 'ContinueSegmentSynchronization'.
  !
  ! Then, this routine will transmit the SyncMemoryCount value
  ! to the remote image through array index 'SetFromImageNumber'.
  ! Image status turns into 'SendetCurrentSegmentNumber'.
  ! (the involved images (not image 1) will execute this)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intSetFromImageNumber ! this is the remote image number (image 1)
                                                               ! which initiated the synchronization
  integer(OOOGglob_kint) :: status = 0 ! error status
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intRemoteImageNumber
  integer(OOOGglob_kint) :: intSyncMemoryCount
  integer(OOOGglob_kint) :: intNumberOfImages
  integer(OOOGglob_kint), dimension (1:1) :: intA_RemoteImageNumbers
  !
                                                                call OOOGglob_subSetProcedures &
                                                            ("OOOPimsc_WaitForSegmentSynchronization_CA")
  !
  intRemoteImageNumber = intSetFromImageNumber
  !**********************************************************************
  ! (1) wait until image state is remotely set to value ContinueSegmentSynchronization
  ! spin-wait loop synchronization:
  ! (conterpart routine is step 3 in OOOPimsc_SynchronizeTheInvolvedImages_CA)
  !
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % ContinueSegmentSynchronization
  intNumberOfImages = 1
  intA_RemoteImageNumbers(1) = intSetFromImageNumber
  ! spin-wait loop synchronization:
  call OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                         intNumberOfImages, intA_RemoteImageNumbers, logArrayIndexIsThisImage = .true.)
  !
  !**********************************************************************
  ! (2) send the current intSyncMemoryCount on this image to the remote image:
  ! (conterpart synchronization routine is step 4 in OOOPimsc_SynchronizeTheInvolvedImages_CA)
  !
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % SendetCurrentSegmentNumber
  !
  ! pack the intImageActivityFlag together with the current segment number:
  ! (a) get the SyncMemoryCount on this image:
  call OOOPimscGAElement_atomic_intImageSyncMemoryCount99_CA (Object_CA, intSyncMemoryCount)
  ! (b) increment it by one because of the following call to OOOPimscSAElement_atomic_intImageActivityFlag99_CA
  !     (which does execute SYNC MEMORY)
  intSyncMemoryCount = intSyncMemoryCount + 1
  ! (c) pack the Enum value with the SyncMemoryCount value (segment number):
  call OOOPimsc_PackEnumValue_ImageActivityFlag (Object_CA, intImageActivityFlag, intSyncMemoryCount, intPackedEnumValue)
  !
  ! signal to the remote image (image 1) that this image is now in state 'SendetCurrentSegmentNumber'
  ! and transmit also the current SyncMemoryCount within the same packed enum value:
  call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intPackedEnumValue, &
                         intRemoteImageNumber, intArrayIndex = this_image(), logExecuteSyncMemory = .true.)
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_WaitForSegmentSynchronization_CA
!__________________________________________________________
! public
subroutine OOOPimsc_DoSegmentSynchronization_CA (Object_CA, intSetFromImageNumber)
  ! Current image status is 'SendetCurrentSegmentNumber',
  ! code execution on this image will be stopped until it is set to
  ! state 'DoSegmentSynchronization'.
  !
  ! Then, this routine will transmit the SyncMemoryCount value
  ! to the remote image through array index 'SetFromImageNumber'.
  ! Image status turns into 'SendetCurrentSegmentNumber'.
  ! (the involved images (not image 1) will execute this)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intSetFromImageNumber ! this is the remote image number (image 1)
                                                               ! which initiated the synchronization
  integer(OOOGglob_kint) :: status = 0 ! error status
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint) :: intNumberOfImages
  integer(OOOGglob_kint), dimension (1:1) :: intA_RemoteImageNumbers
  integer(OOOGglob_kint), dimension (1:1, 1:2) :: intA_RemoteImageAndMaxSegmentCount
  integer(OOOGglob_kint) :: intMaxSegmentCount
  integer(OOOGglob_kint) :: intNumberOfSyncMemoryStatementsToExecute
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intRemoteImageNumber
  integer(OOOGglob_kint) :: intSyncMemoryCount
  integer(OOOGglob_kint) :: intCount
  !
                                                                call OOOGglob_subSetProcedures &
                                                            ("OOOPimsc_DoSegmentSynchronization_CA")
  !
  intRemoteImageNumber = intSetFromImageNumber
  !**********************************************************************
  ! (1) wait until image state is remotely set to value 'DoSegmentSynchronization'
  ! spin-wait loop synchronization:
  ! (conterpart routine is step 6 in OOOPimsc_SynchronizeTheInvolvedImages_CA)
  !
  !
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % DoSegmentSynchronization
  intNumberOfImages = 1
  intA_RemoteImageNumbers(1) = intSetFromImageNumber
  ! spin-wait loop synchronization:
  call OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA (Object_CA, intImageActivityFlag, &
                         intNumberOfImages, intA_RemoteImageNumbers, logArrayIndexIsThisImage = .true., &
                         intA_RemoteImageAndItsAdditionalAtomicValue = intA_RemoteImageAndMaxSegmentCount)
  !
  intMaxSegmentCount = intA_RemoteImageAndMaxSegmentCount(1,2)
  !
  !**********************************************************************
  ! (2) restore the segment order (sync memory count) on the involved images (this image):
  !
  ! (a) get the SyncMemoryCount on this image:
  call OOOPimscGAElement_atomic_intImageSyncMemoryCount99_CA (Object_CA, intSyncMemoryCount)
  !
  ! (b) change the segment order only if this_image has a lower sync memory count as intMaxSegmentCount:
  if (intMaxSegmentCount .gt. intSyncMemoryCount) then
    intNumberOfSyncMemoryStatementsToExecute = intMaxSegmentCount - intSyncMemoryCount
    ! restore the segment order (among the involved images) for this image:
    do intCount = 1, intNumberOfSyncMemoryStatementsToExecute
      !
      call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
      !
    end do
call OOOPimscGAElement_atomic_intImageSyncMemoryCount99_CA (Object_CA, intSyncMemoryCount)
write(*,*) 'segment order restored to value x on image y:',intSyncMemoryCount ,this_image()
  end if
  !
  !************************************************************************
  ! (3) send the current intSyncMemoryCount on this image to the remote image:
  ! (counterpart synchronization routine is step 7 in OOOPimsc_SynchronizeTheInvolvedImages_CA)
  !
  intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % FinishedSegmentSynchronization
  ! pack the intImageActivityFlag together with the current segment number (sync memory count):
  ! (1) get the SyncMemoryCount on this image:
  call OOOPimscGAElement_atomic_intImageSyncMemoryCount99_CA (Object_CA, intSyncMemoryCount)
  ! (2) increment it by 1 because of the follow call to OOOPimscSAElement_atomic_intImageActivityFlag99_CA
  !     (which does execute SYNC MEMORY)
  intSyncMemoryCount = intSyncMemoryCount + 1
  ! (3) pack the enum value together with the SyncMemoryCount:
  call OOOPimsc_PackEnumValue_ImageActivityFlag (Object_CA, intImageActivityFlag, intSyncMemoryCount, intPackedEnumValue)
  !
  ! signal to the remote image (image 1) that this image is now in state 'FinishedSegmentSynchronization'
  call OOOPimscSAElement_atomic_intImageActivityFlag99_CA (Object_CA, intPackedEnumValue, &
                         intRemoteImageNumber, intArrayIndex = this_image(), logExecuteSyncMemory = .true.)
  !**********************************************************************
  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_DoSegmentSynchronization_CA
!__________________________________________________________



!##################################################################################################
!##################################################################################################
!##################################################################################################


!**************************
! coarray ADT management: *
!**************************
!___________________________________________________________
!
!
subroutine OOOPimsc_StructureConstructor_CA (Object_CA)
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  !
                                                                call OOOGglob_subSetProcedures ("OOOPimsc_StructureConstructor_CA")

  !
                                                                call OOOGglob_subResetProcedures
end subroutine OOOPimsc_StructureConstructor_CA
!___________________________________________________________


!##################################################################################################
!##################################################################################################
!##################################################################################################

!**********************************************************
!*************  Error Handling:   *************************
!**********************************************************
!__________________________________________________________
!
!Private
subroutine IIimsc_ErrorHandler (Object, chrErrorDescription, &
  intErrorType, intErrorNumber)
  ! ErrorHandler for the ADT and CA routines
  type(OOOPimsc_adtImageStatus_CA), intent(inout) :: Object
  character(kind=1, len=*), intent(in) :: chrErrorDescription
  integer(OOOGglob_kint), intent(in) :: intErrorType ! 1=warning, 2=Severe System error
  integer(OOOGglob_kint), intent(in) :: intErrorNumber ! Run Time error Number (e.g. status)
  call OOOEerroc_AddObject (Object % m_UUerrocError, chrErrorDescription, &
    intErrorType)
end subroutine IIimsc_ErrorHandler
!__________________________________________________________
!
!Private
logical(OOOGglob_klog) function IIimsc_ImageNumberBoundError_CA (Object_CA, intImageNumber)
  ! error handling routine
  ! checks if the image number does exist
  type(OOOPimsc_adtImageStatus_CA), codimension[*], intent(inout) :: Object_CA
  integer(OOOGglob_kint), intent(in) :: intImageNumber
  !
  IIimsc_ImageNumberBoundError_CA = .false.
  !
  if (intImageNumber .lt. 1) then ! image number is too small
    IIimsc_ImageNumberBoundError_CA = .true.
  end if
  !
  if (intImageNumber .gt. num_images()) then ! image number is too large
    IIimsc_ImageNumberBoundError_CA = .true.
  end if
  !
end function IIimsc_ImageNumberBoundError_CA
!__________________________________________________________

end module OOOPimsc_admImageStatus_CA
