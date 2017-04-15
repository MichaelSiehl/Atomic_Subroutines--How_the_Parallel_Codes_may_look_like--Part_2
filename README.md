# Atomic_Subroutines--How_the_Parallel_Codes_may_look_like--Part_2: Implementing_a_customized_synchronization
Fortran 2008 coarray programming with unordered execution segments (user-defined ordering) - Atomic Subroutines: How the parallel logic codes may look like - Part 2: Implementing a customized synchronization

# Overview
This GitHub repository contains an example program with an implementation of a customized synchronization, programmed as a procedure, using Fortran 2008 atomic subroutines and SYNC MEMORY statement. Such a customized synchronization, programmed as a procedure, was described in Modern Fortran explained, appendix B.10.1. There, however, they express strong doubts and that the programmer will not be able to ensure that such a customized synchronization will work correctly on all implementations. Personally, I can only guess where these doubts are derived from: a main reasoning behind them might be the limitation of atomic subroutines to only allow for single scalar (integer) values with them. Nevertheless, we use a simple programming 'trick' that allows to transmit more than just a single value within a single call to atomic_define / atomic_ref. We use that and other simple programming techniques to make more efficient and safe use of atomic subroutines:<br />
https://github.com/MichaelSiehl/Atomic_Subroutines--Using_Integers_Efficiently<br />
https://github.com/MichaelSiehl/Atomic_Subroutines--Using_Coarray_Arrays_to_Allow_for_Safe_Remote_Communication<br />
https://github.com/MichaelSiehl/Atomic_Subroutines--How_to_Encapsulate_Access_to_Them<br />

The example program does restore segment ordering among a number of coarray images, using Fortran 2008 source code. For a thorough description see the GitHub repository containing the first version of this example program: https://github.com/MichaelSiehl/Atomic_Subroutines--How_the_Parallel_Codes_may_look_like--Part_1 .

The following explanations do focus on the impkementation of the customized synchronizaton procedure and thus, on the changes from the first version of the program.
