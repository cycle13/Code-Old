---
layout:     post
title:      "一些Fortran报错信息与解决办法"
subtitle:   "Fortran error messages and solutions"
date:       2017-05-30
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Fortran
    - 编译器
---

# SIGSEGV, segmentation fault occurred

80% of the time, simply increasing your stacksize fixes the problem. OR just compile with `-heap-arrays`. That takes care of 80% of the cases. Next, isolate the fault with a stack traceback `-O2` (or `-O3`) `-g` `-traceback` on both compile and link. Yes, you can add `-g` to get symbolic information on optimized code as long as you also include a `-Ox` option. Next, make sure you're not doing something silly in your code (array bounds violations, for example)...other options... `-g` `-traceback` `-check all` `-fp-stack-check` That finds 99% of these. The remaining 1%, possible compiler bug. Tar up the code, open a problem report at premier.intel.com, attach the file, include instructions on how to build and run.

# Adjustable array dimension error

Upon entry to a subprogram, one of the following errors was detected during the evaluation of dimensioning information:
An upper-dimension bound was less than a lower-dimension bound. The dimensions implied an array that was larger than addressable memory.

# Attempt to access non-existent record

A direct-access READ or FIND statement attempted to access beyond the end of a relative file (or a sequential file on disk with fixed-length records) or access a record that was previously deleted in a relative file.

# Array index out of bounds (SIGTRAP)

Break exception generated a SIGTRAP signal (described in signal(3)). Core dump file created.

The cause is an array subscript that is outside the dimensioned boundaries of that array. Try recompiling using the -check bounds option (perhaps with the f77_dump_flag environment variable set) or examine the core dump file to determine the source code in error.

# Array index out of bounds for index n (SIGTRAP)

Break exception generated a SIGTRAP signal (described in signal(3)). Core dump file created.

The cause is an array subscript that is outside the dimensioned boundaries of the array index n. Try recompiling using the -check bounds option (perhaps with the f77_dump_flag environment variable set) or examine the core dump file to determine the source code in error.

# BACKSPACE error

An error condition was detected during execution of a BACKSPACE statement.

# Cannot overwrite existing file

Specified file xxx already exists when OPEN statement specified STATUS='NEW' (create new file) using I/O unit x. Make sure correct file name, directory path, unit, and so forth were specified in the source program. Decide whether to: Rename or remove the existing file before rerunning the program. Modify the source file to specify different file specification, I/O unit, or OPEN statement STATUS='UNKNOWN'.

# Cannot stat file

Attempted stat operation on the indicated file failed. Make sure correct file and unit were specified.

# CLOSE error

An error condition was detected by the DEC Fortran RTL I/O system during execution of a CLOSE statement.

# DELETE error

An error condition was detected by the DEC Fortran RTL I/O system during execution of a DELETE statement.

# Divide by zero check (SIGTRAP)

Break exception generated a SIGTRAP signal (described in signal(3)). Core dump file created. Examine core dump file for possible cause.

# Duplicate file specifications

Multiple attempts were made to specify file attributes without an intervening close operation. A DEFINE FILE statement was followed by another DEFINE FILE statement or an OPEN statement.

# ENDFILE error

One of the following conditions occurred:

- The file was not a sequential organization file with variable-length records.

- The file was not opened for sequential or append access.

- An unformatted file did not contain segmented records.

- The DEC Fortran RTL I/O system detected an error during execution of an ENDFILE statement.

# End-of-file during read

One of the following conditions occurred:

- A DEC Fortran RTL I/O system end-of-file condition was encountered during execution of a READ statement that did not contain an END, ERR, or IOSTAT specification.

- An end-of-file record written by the ENDFILE statement was encountered during execution of a READ statement that did not contain an END, ERR, or IOSTAT specification.

- An attempt was made to read past the end of an internal file character string or array during execution of a READ statement that did not contain an END, ERR, or IOSTAT specification.

# Error during read

The DEC Fortran RTL I/O system detected an error condition during execution of a READ statement.

# Error during write

The DEC Fortran RTL I/O system detected an error condition during execution of a WRITE statement.

# File name specification error

The file name was specified erroneously.

# File not found

A file with the specified name could not be found during an open operation.

# FIND error

The DEC Fortran RTL I/O system detected an error condition during execution of a FIND statement.

# Floating overflow in math library

A floating-point overflow condition was detected during execution of a math library procedure.

# Floating underflow in math library

A floating-point underflow condition was detected during execution of a math library procedure. The result returned was zero.

# Format/variable-type mismatch

An attempt was made either to read or write a real variable with an integer field descriptor (I or L), or to read or write an integer or logical variable with a real field descriptor (D, E, F, or G).

# Formatted I/O to unit open for unformatted transfers

Attempted formatted I/O (such as list-directed or namelist I/O) to a unit where the OPEN statement indicated the file was unformatted (FORM keyword). Check that the correct unit (file) was specified.

If the FORM keyword was not specified in the OPEN statement and the file should contain formatted data, specify FORM='FORMATTED' in the OPEN statement. Otherwise, if appropriate, use unformatted I/O.

# Inconsistent file organization

The file organization specified in an OPEN statement did not match the organization of the existing file.

# Inconsistent OPEN/CLOSE parameters

Specifications in an OPEN or CLOSE statement were inconsistent. Some invalid combinations follow:

- READONLY with STATUS='NEW' or STATUS='SCRATCH'

- ACCESS='APPEND' with READONLY, STATUS='NEW' or STATUS='SCRATCH'

- DISPOSE='SAVE', 'PRINT', or 'SUBMIT' with STATUS='SCRATCH'

- DISPOSE='DELETE' with READONLY

# Inconsistent record length

An attempt was made to open a direct access file without specifying a record length.

# Inconsistent record type

The RECORDTYPE value in an OPEN statement did not match the record type attribute of the existing file that was opened.

# Infinite format loop

The format associated with an I/O statement that included an I/O list had no field descriptors to use in transferring those values.

# Input conversion error

During a formatted input operation, an invalid character was detected in an input field, or the input value overflowed the range representable in the input variable. The value of the variable was set to zero.

# Input record too long

A record was read that exceeded the explicit or default record length specified when the file was opened. To read the file, use an OPEN statement with a RECL= value (record length) of the appropriate size.

# Input statement requires too much data

An unformatted READ statement attempted to read more data than existed in the record being read.

# Insufficient virtual memory

The DEC Fortran RTL was unable to acquire additional virtual memory from the operating system. Users of the C and Korn shells may be able to overcome this problem by increasing the per-process data limit using the limit (C shell) or ulimit (Korn shell) commands. For more information, see the csh(1) and ksh(1) reference pages.

If the maximum per-process data size is already allocated, increase the value of the maxdsiz parameter in the system's configuration file. Note that edits to the configuration file do not take effect until the operating system kernel has been rebuilt, and the system has been rebooted. For more information, see the doconfig(1) reference page and your operating system guide to system configuration.

# Integer overflow

During an arithmetic operation, an integer value exceeded byte, word, or longword range. The result of the operation was the correct low-order part. See your DEC Fortran user manual for ranges of the various integer data types.

# Integer zero divide

During an integer arithmetic operation, an attempt was made to divide by zero. The result of the operation was set to the dividend, which is equivalent to division by 1.

# Internal consistency check failure

Internal severe error. Please check that the program is correct. Recompile if an error exists in the program.

If this error persists, submit an SPR.

# Invalid argument to Fortran Run-Time Library

The compiler passed an invalid or improperly coded argument to the DEC Fortran RTL. This can occur if the compiler is newer than the RTL in use.

# Invalid argument to math library

One of the mathematical procedures detected an invalid argument value.

# Invalid logical unit number

A logical unit number greater than or less than zero was used in an I/O statement.

# Invalid reference to variable in NAMELIST input

One of the following conditions occurred:

- The variable was not a member of the namelist group.

- An attempt was made to subscript the scalar variable.

- A subscript of the array variable was out-of-bounds.

- An array variable was specified with too many or too few subscripts for the variable.

- An attempt was made to specify a substring of a non- character variable or array name.

- A substring specifier of the character variable was out-of-bounds.

- A subscript or substring specifier of the variable was not an integer constant.

- An attempt was made to specify a substring using an unsubscripted array variable.

# Kernel breakpoint (SIGTRAP)

Break exception generated a SIGTRAP signal (described in signal(3)). Core dump file created.

Examine core dump for possible cause.

# Keyword value error in OPEN statement

An improper value was specified for an OPEN or CLOSE statement keyword requiring a value.

# List-directed I/O syntax error

The data in a list-directed input record had an invalid format, or the type of the constant was incompatible with the corresponding variable. The value of the variable was unchanged.

# Logarithm of zero or negative value

An attempt was made to take the logarithm of zero or a negative number. The result returned was the reserved operand, -0.

# Mixed file access modes

An attempt was made to use any of the following combinations:

- Formatted and unformatted operations on the same unit.

- An invalid combination of access modes on a unit, such as direct and sequential.

- A DEC Fortran RTL I/O statement on a logical unit that was opened by a program coded in another language.

# No such device

A pathname included an invalid or unknown device name when an OPEN operation was attempted.

# Not a Fortran-specific error

An error occurred in the user program or in the RTL that was not a DEC Fortran-specific error.

# Not taken branch delay emulation (SIGTRAP)

Break exception generated a SIGTRAP signal (described in signal(3)). Core dump file created.

Examine core dump for possible cause.

# OPEN or DEFINE FILE required

A direct access READ, WRITE, or FIND, statement was attempted for a file when no DEFINE FILE or OPEN statement with ACCESS='DIRECT' was performed for that file.

# Open failure

An error was detected by the DEC Fortran RTL I/O system while attempting to open a file in an OPEN, INQUIRE, or other I/O statement. This message is issued when the error condition is not one of the more common conditions for which specific error messages are provided. It can occur if an OPEN operation is attempted for one of the following files:

- A segmented file that was not on a disk or a raw magnetic tape. 
- A standard I/O file that had been closed.

# Operation requires seek ability

Attempted an operation on a file that requires the ability to perform seeks on that file. Make sure the correct unit, directory path, and file were specified.

# Output statement overflows record

An output statement attempted to transfer more data than would fit in the maximum record size.

# Overflow check (SIGTRAP)

Break exception generated a SIGTRAP signal (described in signal(3)). Core dump file created.

The cause is an integer overflow. Try recompiling using the `-check overflow` option (perhaps with the f77_dump_flag environment variable set) or examine the core dump file to determine the source code in error.

# Pathname error

A pathname (or file name) given to an OPEN or INQUIRE statement was not acceptable to the DEC Fortran RTL I/O system.

# Permission to access file denied, unit x, file xxx

Check the mode (protection) of the specified file. Make sure the correct file was being accessed. Change the protection, specified file, or process used before rerunning program.

# Record number outside range

A direct access READ, WRITE, or FIND statement specified a record number outside the range specified when the file was opened.

# Recursive I/O operation

While processing an I/O statement for a logical unit, another I/O operation on the same logical unit was attempted, such as a function subprogram that performs I/O to the same logical unit was referenced in an expression in an I/O list or variable format expression.

# REWIND error

One of the following conditions occurred:

- The file was not a sequential file.

- The file was not opened for sequential or append access.

- The DEC Fortran RTL I/O system detected an error condition during execution of a REWIND statement.

# Segmented record format error

An invalid segmented record control data word was detected in an unformatted sequential file. The file was probably either created with RECORDTYPE='FIXED' or 'VARIABLE' in effect, or was created by a program written in a language other than Fortran.

# Significance lost in math library

The magnitude of an argument or the magnitude of the ratio of the arguments to a math library function was so large that all significance in the result was lost. The result returned was the reserved operand, -0.

# Square root of negative value

An argument required the evaluation of the square root of a negative value. The result returned was the reserved operand, -0.

# Subscript out of range

An array reference was detected outside the declared array bounds.

# Syntax error in format

A syntax error was encountered while the RTL was processing a format stored in an array or character variable.

# Syntax error in NAMELIST input

The syntax of input to a namelist READ statement was incorrect.

# Taken branch delay emulation (SIGTRAP)

Break exception generated a SIGTRAP signal (described in signal(3)). Core dump file created.

Examine core dump for possible cause.

# Too many records in I/O statement

An attempt was made to do one of the following:

- Read or write more than one record with an ENCODE or DECODE statement. 
- Write more records than existed.

# Too many values for NAMELIST variable

An attempt was made to assign too many values to a variable during a namelist READ statement.

# Undefined exponentiation

An exponentiation that is mathematically undefined was attempted, for example, 0.**0. The result returned for floating-point operations was the reserved operand, -0, and for integer operations, zero.

# Unformatted I/O to unit open for formatted transfers

Attempted unformatted I/O to a unit where the OPEN statement indicated the file was formatted (FORM keyword). Check that the correct unit (file) was specified.

If the FORM keyword was not specified in the OPEN statement and the file should contain unformatted data, specify FORM='UNFORMATTED' in the OPEN statement. Otherwise, if appropriate, use formatted I/O (such as list-directed or namelist I/O).

# Unit already open

A DEFINE FILE statement specified a logical unit that was already opened.

# Unit not connected

The specified unit was not open at the time of the attempted I/O operation. Check if correct unit number was specified. If appropriate, use an OPEN statement to explicitly open the file (associates the file with the unit number).

# User breakpoint (SIGTRAP)

Break exception generated a SIGTRAP signal (described in signal(3)). Core dump file created.

Examine core dump for possible cause.

# User single step (SIGTRAP)

Break exception generated a SIGTRAP signal (described in signal(3)). Core dump file created.

Examine core dump for possible cause.

# Variable format expression value error

The value of a variable format expression was not within the range acceptable for its intended use; for example, a field width was less than or equal to zero. A value of 1 was assumed, except for a P edit descriptor, for which a value of zero was assumed.

# Write to READONLY file

A write operation was attempted to a file that was declared READONLY in the OPEN statement that is currently in effect.

# Wrong number of arguments

An improper number of arguments was used to call a math library procedure.

Should be corrected. The program may continue execution, but the output from this execution may be incorrect.
The error messages follow (in alphabetical order).

# Floating divide by zero

During a floating-point arithmetic operation, an attempt was made to divide by zero.

# Floating invalid

During an arithmetic operation, the floating-point value generated resulted in an invalid format (not representable for that data type).

# Floating overflow

During an arithmetic operation, a floating-point value exceeded the largest representable value for that data type. See your DEC Fortran user manual for ranges of the various data types.

# Floating point exception

A floating-point exception occurred. Core dump file created. Possible causes include divide by zero, overflow, or an invalid operation, such as subtraction of infinite values, multiplication of zero by infinity (without signs), division of zero by zero or infinity by infinity, and conversion of floating-point to fixed-point format when an overflow prevents conversion.

# Floating underflow

During an arithmetic operation, a floating-point value became less than the smallest representable value for that data type. On RISC systems (depending on the values of the f77 command -fpe option), the underflowed result was either set to zero or allowed to gradually underflow. On AXP systems, the underflowed result is set to zero. See your DEC Fortran user manual for ranges of the various data types.

# Fortran abort routine called

The program called abort to terminate the program.

# IOT trap signal

Core dump file created. Examine core dump for possible cause of this IOT signal.

# Output conversion error

During a formatted output operation, the value of a particular number could not be output in the specified field length without loss of significant digits. When this situation is encountered, the field is filled with asterisks.

# Process interrupted (SIGINT)

The process received the signal SIGINT. Determine source of this interrupt signal (described in signal(3)).

# Process killed (SIGTERM)

The process received the signal SIGTERM. Determine source of this software termination signal (described in signal(3)).

# Process quit (SIGQUIT)

The process received the signal SIGQUIT. Core dump file created. Determine source of this quit signal (described in signal(3)).

Should be investigated. The program continues execution, but the output from this execution may be incorrect.
The warning messages follow (in alphabetical order):

# Could not open message catalog: formsg.cat

The DEC Fortran message file was not found on this system. See your DEC Fortran user manual for more information.

For informational purposes only. Unless it accompanies another message, the program continues.
The informational messages follow (in alphabetical order).

# Check environment variable NLSPATH and protection of path-name/for_msg.dat

The DEC Fortran message file was not found on this system. For more information, see your DEC Fortran user manual or your DEC Fortran installation guide.

# Check location/protection of NLS and /usr/lib/formsg.dat

The DEC Fortran message file was not found on this system. See your DEC Fortran user manual for more information.

# nn floating divide-by-zero traps

The total number of floating-point divide-by-zero traps encountered during program execution was nn. This summary message appears at program completion.

# nn floating invalid traps

The total number of floating-point invalid data traps encountered during program execution was nn. This summary message appears at program completion.

# nn floating overflow traps

The total number of floating-point overflow traps encountered during program execution was nn. This summary message appears at program completion.

# Floating-point conversion failed

The attempted unformatted read or write of non-native floating-point data failed. A non-native floating-point value either exceeded the allowable maximum value for the equivalent native format and was set equal to invalid, or the value was infinity (plus or minus), not a number (NaN), or otherwise invalid and was set to invalid. Very small numbers are set to zero (0). This could be caused by the specified non-native floating-point format not matching the floating-point format found in the specified file.

Make sure the correct file was specified. Make sure the record layout matches the format DEC Fortran is expecting. Check that the correct non-native floating-point data format was specified, as described in your DEC Fortran user manual.

# nn floating underflow traps

The total number of floating-point underflow traps encountered during program execution was nn. This summary message appears at program completion.

# Format syntax error at or near xx

Check the statement containing xx, a character substring from the format string, for a format syntax error. For information about FORMAT statements, refer to the "DEC Fortran Language Reference Manual".

# Fortran error message number is nnn

The DEC Fortran message file was not found on this system. For information about the message file location and the error text that corresponds to the listed error number, nnn, see your DEC Fortran user manual.