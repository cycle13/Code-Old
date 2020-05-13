---
layout:     post
title:      "C/Fortran混合编程"
subtitle:   "Mixed language programming based on C and Fortran"
date:       2016-12-21
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - 编译器
    - C/C++
    - Fortran
---

> C/Fortran混合编程有两种模式：C为主程序或Fortran为主程序。在C为主程序时，只能采用 gcc系列（包含intel编译器等等）编译C程序，f77编译Fortran程序，然后用f77连接；但在Fortran为主程序时，可以采用gcc或 pgcc（PGI系列）编译C程序，f77或pgf77或pgf90编译Fortran程序，然后采用f77或pgf77或pgf90连接，连接时用的软件必须与编译Fortran时的一致。

## 注意事项

在传递数组时必须注意以下三点：<br/>
1. 在C与Fortran中数组存放顺序不同，因此，传送后必须进行转置；<br/>
2. 在C中，形参中的数组元素不能采用动态数组，即不能用malloc分配内存；<br/>
3. 在C与Fortran中传递的数组大小、类型必须完全相同、对应；<br/>
4. 在C中数组元素下标从0开始，而在Fortran中则从1开始

## 范例

<pre class="prettyprint lang-c linenums">
Fortran为主程序调用C子程序的例子如下:

program test
external get3d
dimension u(12,11,10)
                                                                                                
ii=20
x=200.0
call get3d(imax,jmax,kmax,delx,bb,ii,x)
                                                                                                
write(*,*) 'in F:', imax,jmax,kmax,delx,ii,x
do k=1,kmax
  do j=1,jmax
    do i=1,imax
      write(*,*) i,j,k,u(k,j,i)
    enddo
  enddo
enddo      
                                                                                                      
end
-------------------------------------------
#include 
#include 
                                                                                                      
void get3d_(int *LEN,int *DEP,int *HIG,float *deltaX, \
float u[12][11][10],int *ii,float *x)
{
   int i,j,k;
                                                                                                      
   *LEN=12;
   *DEP=11;
   *HIG=10;
   *deltaX=1.0;
                                                                                                      
   for (k = 0; k < *HIG; k++) {
     for (j = 0; j < *DEP; j++) {
       for (i = 0; i < *LEN; i++) {
         u[i][j][k] = (float)(i+j+k);
         printf("in C1: %f\n",u[i][j][k]);
       }
     }
   }
                                                                                                      
   printf("%d %f %d %f\n",*LEN,*DEP,*HIG,*deltaX,*ii,*x);
}      

=========================================================
C为主程序调用Fortran子程序的例子如下:
#include 
#include 
int main()
{                                                                                                      
   extern void get3d_(int *LEN,float *deltaX,float bb[10],int *j,float *x);
                                                                                                      
   int i,j,LEN;
   float x,deltaX,bb[10];
                                                                                                      
   LEN=325;
   deltaX = 1.0;
                                                                                                      
   for(i=0;i<10;i++)
     {
        bb[i]=i;
        printf("in C1: %f\n",bb[i]);
     }
   printf("in C1: %d %f\n",LEN,deltaX);
   get3d_(&LEN,&deltaX,bb,&j,&x);
   printf("in C2: %d %f %d %f\n",LEN,deltaX,j,x);
   for(i=0;i<10;i++){
     printf("in C2: %f\n",bb[i]);
   }
}
-------------------------------------------
subroutine get3d(imax,delx,bb,j,x)
dimension bb(10)
                                                                                                
j=20
x=200.0
do i=1,10
  bb(i)=bb(i)+1
enddo
                                                                                                
write(*,*) 'in F:', imax,delx,j,x
write(*,*) 'in F:', bb
                                                                                                
end
=========================================================
C调用Fortran:
#include <stdio.h> 
void sub_fortran_(int *,float *,double *); 
double function_fortran_(double *); 
int main() { 
int num_int; 
float num_float; 
double num_double; 
double num; 
num_int=3; 
num_float=5.0; 
sub_fortran_(&num_int,&num_float,&num_double); 
num=function_fortran_(&num_double); 
printf("num_int=%d\nnum_float=%f\nnum_double=%f\nnum=%f", \
num_int,num_float,num_double,num); 
return 0; }
-------------------------------------------
Fortran90:
subroutine Sub_Fortran(NumInt,NumFloat,NumDouble) 
implicit none 
integer :: NumInt 
real :: NumFloat 
real(8) :: NumDouble 
NumDouble=NumFloat**NumInt 
end subroutine 
real(8) function Function_Fortran(NumDouble) 
implicit none 
real(8) :: NumDouble 
Function_Fortran=sqrt(NumDouble) 
end function
-------------------------------------------
Fortran2003:
subroutine Sub_Fortran(NumInt,NumFloat,NumDouble) 
use ISO_C_BINDING 
implicit none 
integer(c_int) :: NumInt 
real(c_float) :: NumFloat 
real(c_double) :: NumDouble 
NumDouble=NumFloat**NumInt 
end subroutine 
real(c_double) function Function_Fortran(NumDouble) 
use ISO_C_BINDING 
implicit none 
real(c_double) :: NumDouble 
Function_Fortran=sqrt(NumDouble) 
end function
-------------------------------------------
链接方法:
gcc –o main.o –c main.c
gfortran –o sub.o –c sub.f90
gcc –o main.exe main.o sub.o
或者直接  
gcc –o main.exe main.c sub.f90
=========================================================
Fortran调用C:
Fortran90:
program main 
implicit none 
interface 
subroutine sub_c(n1,n2,n3) 
integer :: n1 
real :: n2 
real(8) :: n3 
end subroutine 
real(8) function func_c(n3) 
real(8) :: n3 
end function 
end interface 
integer :: n1 
real :: n2 
real(8) :: n3,n4 
n1=3 
n2=5.0 
call sub_c(n1,n2,n3) 
n4=func_c(n3) 
write(*,*) "n1=",n1 
write(*,*) "n2=",n2 
write(*,*) "n3=",n3 
write(*,*) "n4=",n4 
end program
-------------------------------------------
Fortran2003:
program main 
use ISO_C_BINDING 
implicit none 
interface 
subroutine sub_c(n1,n2,n3) 
use ISO_C_BINDING 
integer(c_int) :: n1 
real(c_float) :: n2 
real(c_double) :: n3 
end subroutine 
real(c_double) function func_c(n3) 
use ISO_C_BINDING 
real(c_double) :: n3 
end function 
end interface 
integer(c_int) :: n1 
real(c_float) :: n2 
real(c_double) :: n3,n4 
n1=3 
n2=5.0 
call sub_c(n1,n2,n3) 
n4=func_c(n3) 
write(*,*) "n1=",n1 
write(*,*) "n2=",n2 
write(*,*) "n3=",n3 
write(*,*) "n4=",n4 
end program
-------------------------------------------
#include < math.h > 
void sub_c_(int *,float *,double *); 
double func_c_(double *); 
void sub_c_(int *n1,float *n2,double *n3) 
{ *n3=pow(*n2,*n1); } 
double func_c_(double *n3) 
{ double n4; n4=sqrt(*n3); return n4; }
-------------------------------------------
链接方式:
gcc –o sub.o sub.c
gfortran –o main.o main.f90
gfortran –o main.exe main.o sub.o
或是直接
gfortran –o main.exe main.f90 sub.c
</pre>