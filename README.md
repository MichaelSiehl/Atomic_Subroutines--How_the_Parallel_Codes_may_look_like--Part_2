# Atomic_Subroutines--How_the_Parallel_Codes_may_look_like--Part_2: Implementing_a_customized_synchronization
Fortran 2008 coarray programming with unordered execution segments (user-defined ordering) - Atomic Subroutines: How the parallel logic codes may look like - Part 2: Implementing a customized synchronization

# Overview
This GitHub repository contains an example program with an implementation of a customized synchronization, programmed as a procedure, using Fortran 2008 atomic subroutines and SYNC MEMORY statement. Such a customized synchronization, programmed as a procedure, was described in Modern Fortran explained, appendix B.10.1. There, however, they express strong doubts and that the programmer will not be able to ensure that such a customized synchronization will work correctly on all implementations. Personally, I can only guess where these doubts are derived from: a main reasoning behind them might be the limitation of atomic subroutines to only allow for single scalar (integer) values with them. Nevertheless, we use a simple programming 'trick' that allows to transmit more than just a single value with a single call to atomic_define / atomic_ref. We use that and other simple programming techniques to make more efficient and safe use of atomic subroutines:<br />
https://github.com/MichaelSiehl/Atomic_Subroutines--Using_Integers_Efficiently<br />
https://github.com/MichaelSiehl/Atomic_Subroutines--Using_Coarray_Arrays_to_Allow_for_Safe_Remote_Communication<br />
https://github.com/MichaelSiehl/Atomic_Subroutines--How_to_Encapsulate_Access_to_Them<br />

The example program does restore segment ordering among a number of coarray images, using Fortran 2008 source code. For a thorough description see the GitHub repository containing the first version of this example program: https://github.com/MichaelSiehl/Atomic_Subroutines--How_the_Parallel_Codes_may_look_like--Part_1 .

The following explanations do focus on the impkementation of the customized synchronizaton procedure and thus, on the changes from the first version of the program.

# Implementing a customized synchronization, programmed as a procedure
The following procedure 'OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA' is a first simple implementation of a customized synchronization using a spin-wait loop. As simple as it is, it is still very flexible and does already allow to replace all other spin-wait loops of our example program: The first version of the example program did contain 5 distinct spin-wait loops for the distinct synchronizations: 3 were executed on image 1, and 2 where executed on images 2, 3, and 4 resp. The customized synchronization procedure does replace all of these. Thus, this second version of the example program does contain a single spin-wait loop only, for all the kinds of synchronizations that we need within our example program. This makes maintaining the spin-wait loop synchronization later on a snap. (The current spin-wait loop is far away from what we will need with real-world parallel programming).


```fortran
subroutine OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA (Object_CA, intCheckImageActivityFlag, &
                  intNumberOfImages, intA_RemoteImageNumbers, logArrayIndexIsThisImage, &
                  intA_RemoteImageAndItsAdditionalAtomicValue, logExecuteSyncMemory)
  ! This routine is for atomic bulk synchronization (among the executing image and one or more remote images)
  ! using a spin-wait loop synchronizaton. Thus, the procedure implements a customized synchronization
  ! routine using atomic subroutines and the sync memory statement. Ordered execution segments among the involved images
  ! are not required.
  type (OOOPimsc_adtImageStatus_CA), codimension[*], volatile, intent (inout) :: Object_CA
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

```
