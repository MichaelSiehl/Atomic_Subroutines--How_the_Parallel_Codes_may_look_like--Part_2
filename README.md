# Atomic_Subroutines--How_the_Parallel_Codes_may_look_like--Part_2: Implementing_a_customized_synchronization
Fortran 2008 coarray programming with unordered execution segments (user-defined ordering) - Atomic Subroutines: How the parallel logic codes may look like - Part 2: Implementing a customized synchronization

# Note
The content of this Github repository is still experimental.<br />

To my current understanding, the core solutions herein are very similar to the solution of the ABA problem with the Compare-And-Swap (CAS) hardware implementation for atomic operations on x86 computers. See the following links for a description of the ABA problem:<br />
https://jfdube.wordpress.com/2011/11/30/understanding-atomic-operations/<br />
https://en.wikipedia.org/wiki/Compare-and-swap<br />
https://en.wikipedia.org/wiki/ABA_problem<br />

Nevertheless, our solution for solving the ABA-style problems herein may differ somewhat from the hardware-related solutions: Instead, we use two simple programming techniques:<br />
(1)<br />
Compared to the ABA solution, we just use an ordinary integer scalar to store an integer-based enumeration value (this is similar to the appended increment counter of the ABA solution) together with the main atomic value (which I call 'additional atomic value' in the repository's source code): https://github.com/MichaelSiehl/Atomic_Subroutines--Using_Integers_Efficiently.<br />
Nevertheless, we do not use that programming technique directly to solve an ABA-style problem here, but merely to implement the sophisticated synchronization methods herein. (Also, the integer-based enumeration helps to make the parallel logic codes more readable, compared to a simple increment counter).<br />
(2)<br />
To prevent ABA-style problems herein, we use a simple array technique: https://github.com/MichaelSiehl/Atomic_Subroutines--Using_Coarray_Arrays_to_Allow_for_Safe_Remote_Communication:<br />
OpenCoarrays does allow to use remote atomic_define together with a single (scalar) array element of an integer array component (of a derived type coarray). (The 'remote' here is not supported by ifort 18 beta update 1 yet).<br />
Together with the above ABA-like solution, we can safely synchronize the value of each distinct (scalar) array element atomically.<br />
This might also be the promising solution for synchronizing whole integer arrays atomically later on. And if we can process whole integer arrays atomically (and since integer is a very general data type that can in principle be used to store other data types with it), there might be no restrictions for implementing even more sophisticated algorithms based entirely on atomics (and thus, with user-defined segment ordering).

# Overview
This GitHub repository contains an example program with an implementation of a customized synchronization, programmed as a procedure, using Fortran 2008 atomic subroutines and SYNC MEMORY statement. Such a customized synchronization, programmed as a procedure, was described in Modern Fortran explained, appendix B.10.1. There, however, they express strong doubts and that the programmer will not be able to ensure that such a customized synchronization will work correctly on all implementations. Personally, I can only guess where these doubts are derived from: a main reasoning behind them might be the limitation of atomic subroutines to only allow for single scalar (integer) values with them. Nevertheless, we use a simple programming 'trick' that allows to transmit more than just one single value with a single call to atomic_define / atomic_ref. We use that and other simple programming techniques to make more efficient and safe use of atomic subroutines:<br />
https://github.com/MichaelSiehl/Atomic_Subroutines--Using_Integers_Efficiently<br />
https://github.com/MichaelSiehl/Atomic_Subroutines--Using_Coarray_Arrays_to_Allow_for_Safe_Remote_Communication<br />
https://github.com/MichaelSiehl/Atomic_Subroutines--How_to_Encapsulate_Access_to_Them<br />

The example program does restore segment ordering among a number of coarray images, using Fortran 2008 source code. For a thorough description see the GitHub repository containing the first version of this example program: https://github.com/MichaelSiehl/Atomic_Subroutines--How_the_Parallel_Codes_may_look_like--Part_1 .

The following explanations do focus on the implementation of the customized synchronizaton procedure and thus, on the changes from the first version of the program.

# Implementing a customized synchronization, programmed as a procedure
The following procedure 'OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA' is a first simple implementation of a customized synchronization using a spin-wait loop. As simple as it is, it is still very flexible and does already allow to replace all other spin-wait loops of our example program: The first version of the example program did contain 5 distinct spin-wait loops for the distinct synchronizations: three were executed on image 1, and two where executed on images 2, 3, and 4 resp. The customized synchronization procedure does replace all of these. Thus, this second version of the example program does contain a single spin-wait loop only, for all the kinds of synchronizations that we need within our example program. This makes it a snap to maintain the spin-wait loop synchronization later on. (The current spin-wait loop is far away from what we will need with real-world parallel programming). Compared to the Fortran 2008/2015 build-in synchronization methods, the resulting functionality of this customized synchronization method is quite unique and does allow for user-defined segment ordering. Much more sophisticated customized synchronization methods can easily be programmed in a similar manner.


```fortran
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
    ! (be aware: due to the second if statement in the loop, this would be error prone in real world programming,
    !  but it is safe for this example program)
  end do
  !
  !**********************************************************************
  !
                                                                call OOOGglob_subResetProcedures
  !
end subroutine OOOPimscSpinWaitBulkSync_atomic_intImageActivityFlag99_CA

```

The adapted parallel logic codes do not contain any spin-wait loop any more:

```fortran
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
  integer(OOOGglob_kint) :: intSetFromImageNumber
  logical(OOOGglob_klog), dimension (intNumberOfImages) :: logA_CheckImageStates
  integer(OOOGglob_kint) :: intPackedEnumValue
  integer(OOOGglob_kint) :: intCurrentSegmentCount
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

```

```fortran
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

```

```fortran
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

```
