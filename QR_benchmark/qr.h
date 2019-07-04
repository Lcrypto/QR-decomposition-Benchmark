#include <cula_lapack.h>
#include <stdlib.h>
#include <float.h>
// Cross platform high resolution timer
#ifdef _WIN32
double getHighResolutionTime(void)
{
    double freq;
    double seconds;
    LARGE_INTEGER end_time;
    LARGE_INTEGER performance_frequency_hz;

    QueryPerformanceCounter(&end_time);
    QueryPerformanceFrequency(&performance_frequency_hz);

    seconds = (double) end_time.QuadPart;
    freq = (double) performance_frequency_hz.QuadPart;
    seconds /= freq;

    return seconds;
}
#else
double getHighResolutionTime(void)
{
    struct timeval tod;

    gettimeofday(&tod, NULL);
    double time_seconds = (double) tod.tv_sec + ((double) tod.tv_usec / 1000000.0);
    return time_seconds;
}
#endif
using namespace std;
	ofstream myfile;
void printCulaError(culaStatus status);


void printHeader()
{
  
    printf("Size of Square Mat    CUDA (s)    MKL (s)   Speedup    Serial MGramm-Schmidt (s)   \n");
    printf("------------------   ---------- ---------- ---------- --------------------------  \n");
    fflush(stdout);
}

void printProblemSize(int n)
{
    printf("%10.0d       ", n);
    fflush(stdout);
}

void printRuntime(double t)
{
    printf("%11.4f", t);
    fflush(stdout);
}

void printSpeedup(double cula_time, double mkl_time)
{
    printf("  %10.4f",  mkl_time/cula_time);
    fflush(stdout);
}


// Min / Max
int imax(int a, int b)
{
    return (((a) > (b)) ? (a) : (b));
}

int imin(int a, int b)
{
    return (((a) < (b)) ? (a) : (b));
}

//
// Quasi-random data generater
//

void fillRandomSingle(int m, int n, float* a, float min, float max)
{
    
	
	int i, j;
	 
    srand(777);

    for (j=0; j<m; j++)
    {
        for (i=0; i<n; i++)
        {
            a[j*n+i] = min + (max-min) * rand()/RAND_MAX;
        
			//myfile <<" "<< a[j*n+i]<< " "; 
        }
			//myfile <<endl;
    }

}
void printMat(int m, int n,float * a)
{
    int i, j;
myfile <<" "<< "Matrix size is "<<m<<"x"<<n<<endl;
 

    for (j=0; j<m; j++)
    {
        for (i=0; i<n; i++)
        {
           myfile <<" "<< a[j*n+i]<< " "; 
        }
			myfile <<endl;
    }
}
/*
typedef float * vector_t;

vector_t GetVectorMulByScalar( float afSrc[], int uiVecIdx, int nItems, float fScalar ){
	vector_t afResult =(vector_t) malloc (sizeof(float)*nItems);

	if(  afResult&& nItems>0 && uiVecIdx>=0 && fScalar!=0){
		for(int i=0; i<nItems;i++)
		afResult[i] = afSrc[ nItems*uiVecIdx+i ]*fScalar;
	}
	return afResult;
}

vector_t GetVectorDifference( float afSrc[], int uiVec1Idx, int uiVec2Idx, int nItems ){
	vector_t afResult =(vector_t) malloc (sizeof(float)*nItems);
	
	if( afSrc != NULL && afResult != NULL && nItems > 0 && uiVec1Idx >= 0 && uiVec2Idx >= 0 )
	{
		for( int i=0; i < nItems; i++ )
			afResult[i] = afSrc[ nItems * uiVec1Idx + i ] - afSrc[ nItems * uiVec2Idx + i ];
	}
	return afResult;
}




typedef float vecnorm_t;
//typedef float 
vecnorm_t GetMatrixVectorNorm( float afSrc[], int uiVecIdx, int nItems ){
	float fresult=-1;
	if( afSrc != NULL && nItems>0 && uiVecIdx>=0){
	float summ=0;
	for(int i=0; i<nItems;i++)
	summ+=afSrc[nItems*uiVecIdx+i]*afSrc[nItems*uiVecIdx+i];
	fresult=(float) sqrt(summ);
	}
	return fresult;
}
typedef float scalarproduct_t;

scalarproduct_t GetScalarProduct( float afSrc[], int uiVec1Idx, int uiVec2Idx, int nItems ){
	float fresult=FLT_MAX;
	if( afSrc != NULL && nItems>0 && uiVec1Idx>=0 && uiVec2Idx>=0){
	float summ=0;
	for(int i=0; i<nItems;i++)
	summ+=afSrc[nItems*uiVec1Idx+i]*afSrc[nItems*uiVec2Idx+i];
	fresult=summ;
	}
	return fresult;
}


vector_t GetVectorDivideByScalar( float afSrc[], int uiVecIdx, int nItems, float fScalar ){
	vector_t afResult =(vector_t) malloc (sizeof(float)*nItems);

	if(  afResult&& nItems>0 && uiVecIdx>=0 && fScalar!=0){
	for(int i=0; i<nItems;i++)
	afResult[i]=afSrc[nItems*uiVecIdx+i]/fScalar;
	}
	return afResult;
}


//Get Vectors Product which have nItems size
vector_t GetVectorProduct( float afSrc[], int uiVec1Idx, int uiVec2Idx, int nItems ){
	vector_t afResult =(vector_t) malloc (sizeof(float)*nItems);
	if( afSrc && afResult&& nItems>0 && uiVec1Idx>=0 && uiVec2Idx>=0){
	for(int i=0; i<nItems;i++)
	afResult[i]=afSrc[nItems*uiVec1Idx+i]*afSrc[nItems*uiVec2Idx+i];
	}
	return afResult;
}

// "float *" represent linear two dimmension array
float * ModGram_Schmidt(float * B, int n, int m){
	float * pBT=NULL;
	pBT= (float*) malloc(n*m*sizeof(float)); // ortogonal B from Modified Gram-Schmidt
	float * pR=NULL;
	pR=(float*) malloc(n*m*sizeof(float)); // R from Modified Gram-Schmidt
	float * pQ=NULL;
	pQ=(float*) malloc(n*m*sizeof(float)); // Q from Modified Gram-Schmidt
	if( pBT && pR && pQ ) {
		memmove(pBT, B, sizeof(float)*n*m);
		memset(pR,0,n*m*sizeof(float));
		memset(pQ,0,n*m*sizeof(float));
		for(int i=0; i<m;i++ )
		{
			pR[i*i+i]=GetMatrixVectorNorm( pBT, i, m );
			float * paf_qi = GetVectorDivideByScalar( pBT, i, m,  pR[i*i+i] ); 
			memcpy( pQ + i*i, paf_qi, m ); 
			
			for( int j=i+1; j<n; j++){
				paf_qi = (float*)realloc( paf_qi, 2*m*sizeof(float) );
				/// realloc размен€ю на malloc фтв free
				memcpy( paf_qi + m*sizeof(float), pBT + i*m*sizeof(float), m*sizeof(float) );
				pR[i*i+j] = GetScalarProduct( paf_qi, 0, 1, m );
			
				float * afMul = GetVectorMulByScalar( paf_qi, 0, m, pR[i*i+j] );			
				afMul = (float*) realloc( afMul, 2*m*sizeof(float) );
				memcpy( afMul + m*sizeof(float), pBT + i*m*sizeof(float), m*sizeof(float) );  
				float * afDiff = GetVectorDifference( afMul, 1, 0, m );
				realloc( afMul, 0 );
				memcpy( pBT + i*m*sizeof(float), afDiff, m*sizeof(float) );
				realloc( afDiff, 0 ); 
			}// for : j
			
			realloc( paf_qi, 0 );
		}//for : i
	}

	return pR;

}
*/
float VectorNORM(int VecSize,float *pV){
	float Summ=0;
	for(int i=0; i<VecSize;i++)
	Summ+=pV[i]*pV[i];
	return sqrt(Summ);

}
float ScalarVectorProduct(int VecSize,float *pV1,float *pV2){
	float Summ=0;
	for(int i=0; i<VecSize;i++)
	Summ+=pV1[i]*pV2[i];
	return Summ;

}
float * VectorMultScalar(int VecSize,float *pV,float Scalar){
	for(int i=0; i<VecSize;i++)
	pV[i]=pV[i]*Scalar;
	return pV;

}
float * VectorDivide(int VecSize,float *pV, float divider){
	
	for(int i=0; i<VecSize;i++)
	pV[i]=pV[i]/divider;
	return	pV;
}
void VectorProduct(int VecSize,float *pV1,float *pV2,float *pResultV){
	
	for(int i=0; i<VecSize;i++)
	pResultV[i]=pV1[i]*pV2[i];

}
void VectorSubstraction(int VecSize,float *pV1,float *pV2,float *pResultV){
	
	for(int i=0; i<VecSize;i++)
	pResultV[i]=pV1[i]-pV2[i];

}

void EqualofVectorofVector(int m, int n, float **B_is,float **equal_C){
	for(int i=0; i<m;i++)
	for(int j=0; j<n;j++)
	B_is[i][j]=equal_C[i][j];
}

void GetVectFromAray(int index, int VecSize, float **B,float * pVector){
	for(int j=0;j<VecSize;j++)
	pVector[j]=B[index][j];
}
void SetVectInAray(int index, int VecSize, float **B,float * pVector){
	for(int j=0;j<VecSize;j++)
	B[index][j]=pVector[j];
}	
void GetScalarFromAray(int m, int n, float **B,float &Scalar){
	
	Scalar=B[m][n];
}


//m x n array
void ModGram_SchmidtSingle(int m, int n, float **pB,float **pBT, float **pR,float **pQ ){
	float *b_temp=(float *) malloc(n*sizeof(float));
	float *q_temp=(float *) malloc(n*sizeof(float));
	if(!b_temp||!q_temp) printf("Temper variable allocation error\n");
	else{

	for(int i=0;i<n; i++){
	GetVectFromAray(i,n,pB,b_temp);
	SetVectInAray(i,n,pBT,b_temp);  //BT(i)=B(I)
	}
	for(int i=0; i<n;i++){
		GetVectFromAray(i,n,pBT,b_temp); 
		pR[i][i]=VectorNORM(n,b_temp); //R(i,i)=NORM(BT(i))
		SetVectInAray(i,n,pQ,VectorDivide(n,b_temp,pR[i][i])); //q(i)=BT(i)/R(i,i)
		for(int j=i+1; j<n;j++){
		
		GetVectFromAray(i,n,pB,b_temp);
		GetVectFromAray(i,n,pQ,q_temp);
		pR[i][j] =ScalarVectorProduct(n,b_temp,q_temp); //R(i,j)=<q(i),b(i)>
		GetVectFromAray(i,n,pBT,b_temp); //Take BT(i) vector
		//BT(i)=BT(i)-R(i,j)*q(i));
		VectorSubstraction(n,b_temp,VectorMultScalar(n,q_temp,pR[i][j]),b_temp);
		SetVectInAray(i,n,pBT,b_temp);
		}
	}
	
	
	free(b_temp);
	free(q_temp);
	}
}


void fillRandomSingleStatic( int m,  int n, float **pD, float min, float max)
{
    int i, j;

    srand(1);

    for (i=0; i<m; i++)
    {
        for (j=0; j<n; j++)
        {
            pD[i][j] =(float) min + (max-min) * rand()/RAND_MAX;
			//printf( " %f",  a[j*n+i]); 
        }
			//printf( "/n");
	}
}


void fillRandomDouble(int m, int n, double* a, double min, double max)
{
    int i, j;

    srand(1);

    for (j=0; j<m; j++)
    {
        for (i=0; i<n; i++)
        {
            a[j*n+i] = min + (max-min) * rand()/RAND_MAX;
			//printf( " %f",  a[j*n+i]); 
        }
			//printf( "/n");
	}
}



void benchMGS(int n){

	double start_time, end_time, gramm_time;
	int m = n;
	float **pB=(float **) malloc(m*sizeof(float*));
	for(int i=0;i<m; i++) pB[i]=(float *) malloc(n*sizeof(float));
	float **pBT=(float **) malloc(m*sizeof(float*));
	for(int i=0;i<m; i++) pBT[i]=(float *) malloc(n*sizeof(float));
	float **pR=(float **) malloc(m*sizeof(float*));
	for(int i=0;i<m; i++) pR[i]=(float *) malloc(n*sizeof(float));
	float **pQ=(float **) malloc(m*sizeof(float*));
	for(int i=0;i<m; i++) pQ[i]=(float *) malloc(n*sizeof(float));

if(!pB||!pBT||!pR||!pQ) printf(" Host side allocation error.\n");
else
{
	fillRandomSingleStatic(m, n, pB, -999999999999.0f, 99999999999.0f);
	fillRandomSingleStatic(m, n, pBT, -9999999999.0f, 999999999999.0f);
	fillRandomSingleStatic(m, n, pR, -99999999999.0f, 999999999.0f);
	fillRandomSingleStatic(m, n, pQ, -999999999999.0f, 9999999.0f);
	/*  for(int i=0;i<m;i++){
		for(int j=0;j<n;j++){
		printf(" %f", pB[i][j]);
	 }

	printf("\n");
	
	  }
	  */
	start_time = getHighResolutionTime();
     ModGram_SchmidtSingle(m, n, pB,pBT,pR,pQ);

    end_time = getHighResolutionTime();
	  gramm_time = end_time - start_time;
	  printf("        %8.4f\n",gramm_time);
	/*  
	  for(int i=0;i<m;i++){
		for(int j=0;j<n;j++){
		printf(" %f", pR[i][j]);
	 }

	printf("\n");
	
	  }
	  */
	  
	
	//memory free area
	for(int i=0;i<m; i++) free(pB[i]);
	free(pB);
	
	for(int i=0;i<m; i++) free(pBT[i]);
	free(pBT);
	for(int i=0;i<m; i++) free(pR[i]);
	free(pR);
	for(int i=0;i<m; i++) free(pQ[i]);
	free(pQ);

}
}

culaStatus benchSgeqrf(int n)
{
   int m = n;
    int lda = m;
    int k = imin(m,n);
    int info = 0;
    int lwork = -1;

    float* a_cula = NULL;
    float* a_mkl = NULL;
    float* tau_cula = NULL;
    float* tau_mkl = NULL;
    float* work_mkl = NULL;

    double start_time, end_time;
    double cula_time, mkl_time;
    culaStatus status = culaNoError;

    printProblemSize(n);

    a_cula = (float*) malloc(lda*n*sizeof(float));
    a_mkl = (float*) malloc(lda*n*sizeof(float));
    tau_cula = (float*) malloc(k*sizeof(float));
    tau_mkl = (float*) malloc(k*sizeof(float));
    work_mkl = (float*) malloc(1*sizeof(float));

    if(!a_cula || !a_mkl || !tau_cula || !tau_mkl || !work_mkl)
    {
        printf(" Host side allocation error.\n");
        status = culaInsufficientMemory;
        goto endBenchSgeqrf;
    }

    // Add some random data
    fillRandomSingle(lda, n, a_cula, -9999999999999999.0f, 9999999999999999.0f);
    //myfile <<  "\n Before reduction \n";
	//printMat(m,n,a_cula);
	memcpy(a_mkl, a_cula, lda*n*sizeof(float));

    // Run CULA's version
    start_time = getHighResolutionTime();
    culaSgeqrf
		(m, n, a_cula, lda, tau_cula);

    end_time = getHighResolutionTime();
	//myfile <<  "\n After QR reduction\n";
	//printMat(m,n,a_cula);
    cula_time = end_time - start_time;

    // Check CULA errors
    status = culaGetLastStatus();
    if(status)
    {
        printCulaError(status);
        goto endBenchSgeqrf;
    }

    printRuntime(cula_time);

    // Prepare MKL benchmark
    sgeqrf(&m, &n, a_mkl, &lda, tau_mkl, work_mkl, &lwork, &info);   // Worksize query
    lwork = (int) work_mkl[0];
    free(work_mkl);
    work_mkl = (float*) malloc(lwork*sizeof(float));

    if(!work_mkl)
    {
        printf(" Host side allocation error.\n");
        status = culaInsufficientMemory;
        goto endBenchSgeqrf;
    }

    // Run MKL's version
    start_time = getHighResolutionTime();
    sgeqrf(&m, &n, a_mkl, &lda, tau_mkl, work_mkl, &lwork, &info);
    end_time = getHighResolutionTime();
    mkl_time = end_time - start_time;

    if(info != 0)
    {
        printf(" Intel MKL Error: (%d)\n", info);
        goto endBenchSgeqrf;
    }

    printRuntime(mkl_time);
    printSpeedup(cula_time, mkl_time);

endBenchSgeqrf:
    free(a_cula);
    free(tau_cula);
    free(a_mkl);
    free(tau_mkl);
    free(work_mkl);
    return status;

}

/*#ifdef CULA_PREMIUM
culaStatus benchDgeqrf(int n)
{
    int m = n;
    int lda = m;
    int k = imin(m,n);
    int info = 0;
    int lwork = -1;

    double* a_cula = NULL;
    double* a_mkl = NULL;
    double* tau_cula = NULL;
    double* tau_mkl = NULL;
    double* work_mkl = NULL;

    double start_time, end_time;
    double cula_time, mkl_time;
    culaStatus status = culaNoError;

    printProblemSize(n);

    a_cula = (double*) malloc(lda*n*sizeof(double));
    a_mkl = (double*) malloc(lda*n*sizeof(double));
    tau_cula = (double*) malloc(k*sizeof(double));
    tau_mkl = (double*) malloc(k*sizeof(double));
    work_mkl = (double*) malloc(1*sizeof(double));

    if(!a_cula || !a_mkl || !tau_cula || !tau_mkl || !work_mkl)
    {
        printf(" Host side allocation error.\n");
        status = culaInsufficientMemory;
        goto endBenchDgeqrf;
    }

    // Add some random data
    fillRandomDouble(lda, n, a_cula, -10.0f, 10.0f);
    memcpy(a_mkl, a_cula, lda*n*sizeof(double));

    // Run CULA's version
    start_time = getHighResolutionTime();
    culaDgeqrf(m, n, a_cula, lda, tau_cula);
    end_time = getHighResolutionTime();
    cula_time = end_time - start_time;

    // Check CULA errors
    status = culaGetLastStatus();
    if(status)
    {
        printCulaError(status);
        goto endBenchDgeqrf;
    }

    printRuntime(cula_time);

    // Prepare MKL benchmark
    dgeqrf(&m, &n, a_mkl, &lda, tau_mkl, work_mkl, &lwork, &info);   // Worksize query
    lwork = (int) work_mkl[0];
    free(work_mkl);
    work_mkl = (double*) malloc(lwork*sizeof(double));

    if(!work_mkl)
    {
        printf(" Host side allocation error.\n");
        status = culaInsufficientMemory;
        goto endBenchDgeqrf;
    }

    // Run MKL's version
    start_time = getHighResolutionTime();
    dgeqrf(&m, &n, a_mkl, &lda, tau_mkl, work_mkl, &lwork, &info);
    end_time = getHighResolutionTime();
    mkl_time = end_time - start_time;

    if(info != 0)
    {
        printf(" Intel MKL Error: (%d)\n", info);
        goto endBenchDgeqrf;
    }

    printRuntime(mkl_time);
    printSpeedup(cula_time, mkl_time);

endBenchDgeqrf:
    free(a_cula);
    free(tau_cula);
    free(a_mkl);
    free(tau_mkl);
    free(work_mkl);
    return status;
}
#endif
*/
/*
culaStatus benchSgetrf(int n)
{
    int m = n;
    int lda = m;
    int k = imin(m,n);
    int info = 0;

    float* a_cula = NULL;
    float* a_mkl = NULL;
    int* ipiv_cula = NULL;
    int* ipiv_mkl = NULL;

    double start_time, end_time;
    double cula_time, mkl_time;
    culaStatus status = culaNoError;

    printProblemSize(n);

    a_cula = (float*) malloc(lda*n*sizeof(float));
    a_mkl = (float*) malloc(lda*n*sizeof(float));
    ipiv_cula = (int*) malloc(k*sizeof(int));
    ipiv_mkl = (int*) malloc(k*sizeof(int));

    if(!a_cula || !a_mkl || !ipiv_cula || !ipiv_mkl)
    {
        printf(" Host side allocation error.\n");
        status = culaInsufficientMemory;
        goto endBenchSgetrf;
    }

    // Add some random data
    fillRandomSingle(lda, n, a_cula, 1.0f, 2.0f);
    memcpy(a_mkl, a_cula, lda*n*sizeof(float));

    // Run CULA's version
    start_time = getHighResolutionTime();
    culaSgetrf(m, n, a_cula, lda, ipiv_cula);
    end_time = getHighResolutionTime();
    cula_time = end_time - start_time;

    // Check CULA errors
    status = culaGetLastStatus();
    if(status)
    {
        printCulaError(status);
        goto endBenchSgetrf;
    }

    printRuntime(cula_time);

    // Run MKL's version
    start_time = getHighResolutionTime();
    sgetrf(&m, &n, a_mkl, &lda, ipiv_mkl, &info);
    end_time = getHighResolutionTime();
    mkl_time = end_time - start_time;

    if(info != 0)
    {
        printf(" Intel MKL Error: (%d)\n", info);
        goto endBenchSgetrf;
    }

    printRuntime(mkl_time);
    printSpeedup(cula_time, mkl_time);

endBenchSgetrf:
    free(a_cula);
    free(ipiv_cula);
    free(a_mkl);
    free(ipiv_mkl);
	
myfile.close();
    return status;
}
*/

void initializeMkl()
{}

void printCulaError(culaStatus status)
{
    char buf[256];

    if(!status)
        return;

    culaGetErrorInfoString(status, culaGetErrorInfo(), buf, sizeof(buf));
    printf("\n%s\n", buf);
}

