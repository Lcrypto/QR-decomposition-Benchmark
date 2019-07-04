/*
  Copyright 2012  Vasiliy Usatyuk.
  This program is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  You should have received a copy of the GNU General Public License along
  with this program; see the file COPYING.  If not, write to the Free
  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
  02111-1307, USA.

*/
#include <cuda_runtime.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <string.h>
#include <conio.h>
#include <iostream>
#include <fstream>

// Timers
#ifdef _WIN32
#include <windows.h>
#include <psapi.h>
#else
#include <sys/time.h>
#endif

#include <cula_lapack.h>

// Intel MKL
#include "mkl_lapack.h"
#ifdef _MSC_VER
#   ifdef _WIN64
#       pragma comment(lib, "mkl_intel_lp64.lib")
#   else
#       pragma comment(lib, "mkl_intel_c.lib")
#   endif
#   pragma comment(lib, "libiomp5mt.lib")
#   pragma comment(lib, "mkl_intel_thread.lib")
#   pragma comment(lib, "mkl_core.lib")
#endif
#include "qr.h"


int imax(int a, int b);
int imin(int a, int b);
double getHighResolutionTime(void);
void fillRandomSingle(int m, int, float* a, float min, float max);
void fillRandomDouble(int m, int, double* a, double min, double max);
int strcmp_ci(const char* left, const char* right);
void initializeMkl();


void printHeader(const char* title);
void printProblemSize(int n);
void printRuntime(double t);
void printSpeedup(double culaTime, double mklTime);
void printCulaError(culaStatus status);

culaStatus benchSgeqrf(int n);
culaStatus benchSgetrf(int n);
culaStatus benchSgels(int n);
culaStatus benchSgglse(int n);
culaStatus benchSgesvd(int n);
culaStatus benchSgesv(int n);
culaStatus benchDgeqrf(int n);

#ifdef CULA_PREMIUM
culaStatus benchSsyev(int n);
culaStatus benchDgeqrf(int n);
culaStatus benchDgetrf(int n);
culaStatus benchDgels(int n);
culaStatus benchDgglse(int n);
culaStatus benchDgesvd(int n);
culaStatus benchDgesv(int n);
culaStatus benchDsyev(int n);
#endif

typedef culaStatus (*BenchmarkFunctionPointer)(int);



// Main

int main(int argc, char** argv)
{
//********************************************************************************
	
	int numSpecified = 0;
    int numToRun = 0;
    char group = 'N';
    int negate = 0;
    int rangeArg = 0;
    int validArg = 0;
    culaStatus status;
	

//********************************************************************************
	printf("Copyright 2012 Vasiliy Usatyuk.\n");
	printf("Bechmark of serial and paralell QR Factoring algorithm runing on GPU and CPU.\nParallel QR use Intel MKL and NVIDIA CUBLAS version of Lapack's function SGEQRF.Paralell QR Factoring algorithm based on Givens rotation algorithm.\nSerial algorithm use Modified Gram-Schmidt (MGS) method.\n");
	//myfile.open ("d:\\result.txt");
    //int start = 6400;
    //int stop = 12100;
    //int step = 1600;
	int start = 10;
    int stop = 1000;
	int step = 50;
    //int start = 30;
    //int stop = 100;
    //int step = 20;
	
	


    if(start < 0  || stop < 0 || start > stop || step <= 0)
    {
        printf("Invalid start, stop, or step size\n");
       
        return EXIT_FAILURE;
    }

 
    // Initialize CUDA
    printf("Initialize NVIDIA CUDA...\n");
    status = culaInitialize();
    // Early exit if CULA fails to initialize (no GPU, etc)
    if(status)
    {
        printf("CUDA failed to initialized.\nUpdate your GPU driver or/and check GPU for support 2.1 compute capability.\n");
        _getch();
		return EXIT_FAILURE;
    }
	printf("CUDA Start OK...\n");

	  int idevice = 0;
   
    
    //
    //  initialize
    //
    if( cudaSetDevice( idevice ) != cudaSuccess )
    {
        printf( "Failed to set device %d\n", idevice );
        return 1;
    }
    
    struct cudaDeviceProp prop;
    cudaGetDeviceProperties( &prop, idevice );
    printf( "Device: %s, %.0f MHz clock, %.0f MB memory.\n", prop.name, prop.clockRate/1000.f, prop.totalGlobalMem/1024.f/1024.f );
    // Initialize MKL
    printf("Initialize Intel Math Kernel Library...\n");
    initializeMkl();
	printf("MKL - It's Alive!!!\n");
    printf("\n");
    printf("--CUDA vs MKL QR Parallel Factoring and column-major(fast) Gramm-Schmidt--\n");
	printf("\n");
	// Benchmarks
    printHeader();
    for (int n=start; n<=stop; n+=step)
        {
       
			status =  benchSgeqrf(n);
		
		benchMGS(n);
		printf("\n");
		//status = benchDgeqrf(n);
        //if(status != culaNoError)
        //break;
        }

       
    // Shutdown
    culaShutdown();
	_getch();
	 
	//myfile <<  "\n After QR reduction\n";
	//printMat(m,n,a_cula);
  

    // Check CULA errors
  

   

    return EXIT_SUCCESS;
}

