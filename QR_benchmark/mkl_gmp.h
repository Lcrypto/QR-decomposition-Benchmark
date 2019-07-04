/*******************************************************************************
!                             INTEL CONFIDENTIAL
!   Copyright(C) 2006-2010 Intel Corporation. All Rights Reserved.
!   The source code contained  or  described herein and all documents related to
!   the source code ("Material") are owned by Intel Corporation or its suppliers
!   or licensors.  Title to the  Material remains with  Intel Corporation or its
!   suppliers and licensors. The Material contains trade secrets and proprietary
!   and  confidential  information of  Intel or its suppliers and licensors. The
!   Material  is  protected  by  worldwide  copyright  and trade secret laws and
!   treaty  provisions. No part of the Material may be used, copied, reproduced,
!   modified, published, uploaded, posted, transmitted, distributed or disclosed
!   in any way without Intel's prior express written permission.
!   No license  under any  patent, copyright, trade secret or other intellectual
!   property right is granted to or conferred upon you by disclosure or delivery
!   of the Materials,  either expressly, by implication, inducement, estoppel or
!   otherwise.  Any  license  under  such  intellectual property  rights must be
!   express and approved by Intel in writing.
!
!*******************************************************************************
!  Content:
!      Intel(R) Math Kernel Library (MKL) interface for Multiple-Precision Arithmetic
!******************************************************************************/

#ifndef __MKL_GMP_H__
#define __MKL_GMP_H__

#define gmp_version "MKL Multi-Precision Library v.1.0.0"

#ifdef __cplusplus
extern "C" {
#endif

/* let stddef.h define size_t */
#define __need_size_t
#if defined (__cplusplus)
#include <cstddef>
#else
#include <stddef.h>
#endif
#undef __need_size_t

#if defined (_FILE_DEFINED) || defined (_STDIO_H)
#define _MKL_GMP_FILE
#endif

typedef int mp_size_t;
#if defined(_WIN64)
typedef unsigned long long mp_limb_t;
typedef signed long long mp_signed_limb_t;
#else
typedef unsigned long int mp_limb_t;
typedef signed long int mp_signed_limb_t;
#endif

typedef mp_limb_t * mp_ptr;
typedef const mp_limb_t * mp_srcptr;

typedef struct
{
	int _mp_alloc;      /* number of limbs allocated by the library in a native way */
	mp_size_t _mp_size; /* sign of this field indicates the sign of the big number */
                        /* the absolute value is the number of limbs in the big number */
	mp_limb_t * _mp_d;  /* points to limbs allocated for the big number */
} __mpz_struct;
typedef __mpz_struct mpz_t[1];

typedef struct
{
	struct __mkl_gmp_randstate_t * rs;
} __gmp_randstate_struct;
typedef __gmp_randstate_struct gmp_randstate_t[1];

typedef enum
{
	GMP_RAND_ALG_DEFAULT = 0,
	GMP_RAND_ALG_LC = GMP_RAND_ALG_DEFAULT
} gmp_randalg_t;

#define MKL_GMP_API extern

/* Init, clear and realloc functions */
#define mpz_init __gmpz_init
MKL_GMP_API void mpz_init (mpz_t integer);
#define mpz_init2 __gmpz_init2
MKL_GMP_API void mpz_init2 (mpz_t integer, unsigned long n);
#define mpz_clear __gmpz_clear
MKL_GMP_API void mpz_clear (mpz_t integer);
#define mpz_realloc2 __gmpz_realloc2
MKL_GMP_API void mpz_realloc2 (mpz_t integer, unsigned long n);
#define mpz_array_init __gmpz_array_init
MKL_GMP_API void mpz_array_init (mpz_t integer_array[], size_t array_size, mp_size_t fixed_num_bits);
#define mpz_realloc _mpz_realloc
#define _mpz_realloc __gmpz_realloc
MKL_GMP_API void * _mpz_realloc (mpz_t integer, mp_size_t new_alloc);

/* Set and swap functions */
#define mpz_set __gmpz_set
MKL_GMP_API void mpz_set (mpz_t rop, mpz_t op);
#define mpz_set_ui __gmpz_set_ui
MKL_GMP_API void mpz_set_ui (mpz_t rop, unsigned long int op);
#define mpz_set_si __gmpz_set_si
MKL_GMP_API void mpz_set_si (mpz_t rop, signed long int op);
#define mpz_set_d __gmpz_set_d
MKL_GMP_API void mpz_set_d (mpz_t rop, double op);

#define mpz_set_str __gmpz_set_str
MKL_GMP_API int mpz_set_str (mpz_t rop, char * str, int base);
#define mpz_swap __gmpz_swap
MKL_GMP_API void mpz_swap (mpz_t rop1, mpz_t rop2);

/* Combined Init/Set functions */
#define mpz_init_set __gmpz_init_set
MKL_GMP_API void mpz_init_set (mpz_t rop, mpz_t op);
#define mpz_init_set_ui __gmpz_init_set_ui
MKL_GMP_API void mpz_init_set_ui (mpz_t rop, unsigned long int op);
#define mpz_init_set_si __gmpz_init_set_si
MKL_GMP_API void mpz_init_set_si (mpz_t rop, signed long int op);
#define mpz_init_set_d __gmpz_init_set_d
MKL_GMP_API void mpz_init_set_d (mpz_t rop, double op);
#define mpz_init_set_str __gmpz_init_set_str
MKL_GMP_API void mpz_init_set_str (mpz_t rop, char * str, int base);

/* Conversion functions */
#define mpz_get_ui __gmpz_get_ui
MKL_GMP_API unsigned long int mpz_get_ui (mpz_t rop);
#define mpz_get_si __gmpz_get_si
MKL_GMP_API signed long int mpz_get_si (mpz_t rop);
#define mpz_get_d __gmpz_get_d
MKL_GMP_API double mpz_get_d (mpz_t rop);
#define mpz_get_d_2exp __gmpz_get_d_2exp
MKL_GMP_API double mpz_get_d_2exp (signed long int * exp, mpz_t rop);
#define mpz_get_str __gmpz_get_str
MKL_GMP_API char * mpz_get_str (char *str, int base, mpz_t op);
#define mpz_getlimbn __gmpz_getlimbn
MKL_GMP_API mp_limb_t mpz_getlimbn (mpz_t op, mp_size_t n);

/* Arithmetic functions */
#define mpz_add __gmpz_add
MKL_GMP_API void mpz_add (mpz_t rop, mpz_t op1, mpz_t op2);
#define mpz_add_ui __gmpz_add_ui
MKL_GMP_API void mpz_add_ui (mpz_t rop, mpz_t op1, unsigned long int op2);

#define mpz_sub __gmpz_sub
MKL_GMP_API void mpz_sub (mpz_t rop, mpz_t op1, mpz_t op2);
#define mpz_sub_ui __gmpz_sub_ui
MKL_GMP_API void mpz_sub_ui (mpz_t rop, mpz_t op1, unsigned long int op2);
#define mpz_ui_sub __gmpz_ui_sub
MKL_GMP_API void mpz_ui_sub (mpz_t rop, unsigned long int op1, mpz_t op2);

#define mpz_mul __gmpz_mul
MKL_GMP_API void mpz_mul (mpz_t rop, mpz_t op1, mpz_t op2);
#define mpz_mul_si __gmpz_mul_si
MKL_GMP_API void mpz_mul_si (mpz_t rop, mpz_t op1, long int op2);
#define mpz_mul_ui __gmpz_mul_ui
MKL_GMP_API void mpz_mul_ui (mpz_t rop, mpz_t op1, unsigned long int op2);

#define mpz_addmul __gmpz_addmul
MKL_GMP_API void mpz_addmul (mpz_t rop, mpz_t op1, mpz_t op2);
#define mpz_addmul_ui __gmpz_addmul_ui
MKL_GMP_API void mpz_addmul_ui (mpz_t rop, mpz_t op1, unsigned long int op2);

#define mpz_submul __gmpz_submul
MKL_GMP_API void mpz_submul (mpz_t rop, mpz_t op1, mpz_t op2);
#define mpz_submul_ui __gmpz_submul_ui
MKL_GMP_API void mpz_submul_ui (mpz_t rop, mpz_t op1, unsigned long int op2);

#define mpz_mul_2exp __gmpz_mul_2exp
MKL_GMP_API void mpz_mul_2exp (mpz_t rop, mpz_t op1, unsigned long int op2);

#define mpz_neg __gmpz_neg
MKL_GMP_API void mpz_neg (mpz_t rop, mpz_t op);

#define mpz_abs __gmpz_abs
MKL_GMP_API void mpz_abs (mpz_t rop, mpz_t op);

/* Division functions */
#define mpz_cdiv_q __gmpz_cdiv_q
MKL_GMP_API void mpz_cdiv_q (mpz_t q, mpz_t n, mpz_t d);
#define mpz_cdiv_r __gmpz_cdiv_r
MKL_GMP_API void mpz_cdiv_r (mpz_t r, mpz_t n, mpz_t d);
#define mpz_cdiv_qr __gmpz_cdiv_qr
MKL_GMP_API void mpz_cdiv_qr (mpz_t q, mpz_t r, mpz_t n, mpz_t d);
#define mpz_cdiv_q_ui __gmpz_cdiv_q_ui
MKL_GMP_API unsigned long int mpz_cdiv_q_ui (mpz_t q, mpz_t n, unsigned long int d);
#define mpz_cdiv_r_ui __gmpz_cdiv_r_ui
MKL_GMP_API unsigned long int mpz_cdiv_r_ui (mpz_t r, mpz_t n, unsigned long int d);
#define mpz_cdiv_qr_ui __gmpz_cdiv_qr_ui
MKL_GMP_API unsigned long int mpz_cdiv_qr_ui (mpz_t q, mpz_t r, mpz_t n, unsigned long int d);
#define mpz_cdiv_ui __gmpz_cdiv_ui
MKL_GMP_API unsigned long int mpz_cdiv_ui (mpz_t n, unsigned long int d);
#define mpz_cdiv_q_2exp __gmpz_cdiv_q_2exp
MKL_GMP_API void mpz_cdiv_q_2exp (mpz_t q, mpz_t n, unsigned long int b);
#define mpz_cdiv_r_2exp __gmpz_cdiv_r_2exp
MKL_GMP_API void mpz_cdiv_r_2exp (mpz_t r, mpz_t n, unsigned long int b);

#define mpz_fdiv_q __gmpz_fdiv_q
MKL_GMP_API void mpz_fdiv_q (mpz_t q, mpz_t n, mpz_t d);
#define mpz_fdiv_r __gmpz_fdiv_r
MKL_GMP_API void mpz_fdiv_r (mpz_t r, mpz_t n, mpz_t d);
#define mpz_fdiv_qr __gmpz_fdiv_qr
MKL_GMP_API void mpz_fdiv_qr (mpz_t q, mpz_t r, mpz_t n, mpz_t d);
#define mpz_fdiv_q_ui __gmpz_fdiv_q_ui
#define mpz_div_ui mpz_fdiv_q_ui
MKL_GMP_API unsigned long int mpz_fdiv_q_ui (mpz_t q, mpz_t n, unsigned long int d);
#define mpz_fdiv_r_ui __gmpz_fdiv_r_ui
MKL_GMP_API unsigned long int mpz_fdiv_r_ui (mpz_t r, mpz_t n, unsigned long int d);
#define mpz_fdiv_qr_ui __gmpz_fdiv_qr_ui
MKL_GMP_API unsigned long int mpz_fdiv_qr_ui (mpz_t q, mpz_t r, mpz_t n, unsigned long int d);
#define mpz_fdiv_ui __gmpz_fdiv_ui
MKL_GMP_API unsigned long int mpz_fdiv_ui (mpz_t n, unsigned long int d);
#define mpz_fdiv_q_2exp __gmpz_fdiv_q_2exp
MKL_GMP_API void mpz_fdiv_q_2exp (mpz_t q, mpz_t n, unsigned long int b);
#define mpz_fdiv_r_2exp __gmpz_fdiv_r_2exp
MKL_GMP_API void mpz_fdiv_r_2exp (mpz_t r, mpz_t n, unsigned long int b);

#define mpz_tdiv_q __gmpz_tdiv_q
MKL_GMP_API void mpz_tdiv_q (mpz_t q, mpz_t n, mpz_t d);
#define mpz_tdiv_r __gmpz_tdiv_r
MKL_GMP_API void mpz_tdiv_r (mpz_t r, mpz_t n, mpz_t d);
#define mpz_tdiv_qr __gmpz_tdiv_qr
MKL_GMP_API void mpz_tdiv_qr (mpz_t q, mpz_t r, mpz_t n, mpz_t d);
#define mpz_tdiv_q_ui __gmpz_tdiv_q_ui
MKL_GMP_API unsigned long int mpz_tdiv_q_ui (mpz_t q, mpz_t n, unsigned long int d);
#define mpz_tdiv_r_ui __gmpz_tdiv_r_ui
MKL_GMP_API unsigned long int mpz_tdiv_r_ui (mpz_t r, mpz_t n, unsigned long int d);
#define mpz_tdiv_qr_ui __gmpz_tdiv_qr_ui
MKL_GMP_API unsigned long int mpz_tdiv_qr_ui (mpz_t q, mpz_t r, mpz_t n, unsigned long int d);
#define mpz_tdiv_ui __gmpz_tdiv_ui
MKL_GMP_API unsigned long int mpz_tdiv_ui (mpz_t n, unsigned long int d);
#define mpz_tdiv_q_2exp __gmpz_tdiv_q_2exp
MKL_GMP_API void mpz_tdiv_q_2exp (mpz_t q, mpz_t n, unsigned long int b);
#define mpz_tdiv_r_2exp __gmpz_tdiv_r_2exp
MKL_GMP_API void mpz_tdiv_r_2exp (mpz_t r, mpz_t n, unsigned long int b);

#define mpz_mod __gmpz_mod
MKL_GMP_API void mpz_mod (mpz_t r, mpz_t n, mpz_t d);
#define mpz_mod_ui __gmpz_mod_ui
MKL_GMP_API unsigned long int mpz_mod_ui (mpz_t r, mpz_t n, unsigned long int d);

#define mpz_divexact __gmpz_divexact
MKL_GMP_API void mpz_divexact (mpz_t q, mpz_t n, mpz_t d);
#define mpz_divexact_ui __gmpz_divexact_ui
MKL_GMP_API void mpz_divexact_ui (mpz_t q, mpz_t n, unsigned long d);

#define mpz_divisible_p __gmpz_divisible_p
MKL_GMP_API int mpz_divisible_p (mpz_t n, mpz_t d);
#define mpz_divisible_ui_p __gmpz_divisible_ui_p
MKL_GMP_API int mpz_divisible_ui_p (mpz_t n, unsigned long int d);
#define mpz_divisible_2exp_p __gmpz_divisible_2exp_p
MKL_GMP_API int mpz_divisible_2exp_p (mpz_t n, unsigned long int b);

#define mpz_congruent_p __gmpz_congruent_p
MKL_GMP_API int mpz_congruent_p (mpz_t n, mpz_t c, mpz_t d);
#define mpz_congruent_ui_p __gmpz_congruent_ui_p
MKL_GMP_API int mpz_congruent_ui_p (mpz_t n, unsigned long int c, unsigned long int d);
#define mpz_congruent_2exp_p __gmpz_congruent_2exp_p
MKL_GMP_API int mpz_congruent_2exp_p (mpz_t n, mpz_t c, unsigned long int b);

/* Exponentiaion functions */
#define mpz_powm __gmpz_powm
MKL_GMP_API void mpz_powm (mpz_t rop, mpz_t base, mpz_t exp, mpz_t mod);
#define mpz_powm_ui __gmpz_powm_ui
MKL_GMP_API void mpz_powm_ui (mpz_t rop, mpz_t base, unsigned long int exp, mpz_t mod);

#define mpz_pow_ui __gmpz_pow_ui
MKL_GMP_API void mpz_pow_ui (mpz_t rop, mpz_t base, unsigned long int exp);
#define mpz_ui_pow_ui __gmpz_ui_pow_ui
MKL_GMP_API void mpz_ui_pow_ui (mpz_t rop, unsigned long int base, unsigned long int exp);

/* Root extractoin functions */
#define mpz_root __gmpz_root
MKL_GMP_API int mpz_root (mpz_t rop, mpz_t op, unsigned long int n);
#define mpz_sqrt __gmpz_sqrt
MKL_GMP_API void mpz_sqrt (mpz_t rop, mpz_t op);
#define mpz_sqrtrem __gmpz_sqrtrem
MKL_GMP_API void mpz_sqrtrem (mpz_t rop1, mpz_t rop2, mpz_t op);
#define mpz_perfect_power_p __gmpz_perfect_power_p
MKL_GMP_API int mpz_perfect_power_p (mpz_t ro);
#define mpz_perfect_square_p __gmpz_perfect_square_p
MKL_GMP_API int mpz_perfect_square_p (mpz_t ro);

/* Number theoretic functions */
#define mpz_probab_prime_p __gmpz_probab_prime_p
MKL_GMP_API int mpz_probab_prime_p (mpz_t n, int reps);
#define mpz_nextprime __gmpz_nextprime
MKL_GMP_API void mpz_nextprime (mpz_t rop, mpz_t op);

#define mpz_gcd __gmpz_gcd
MKL_GMP_API void mpz_gcd (mpz_t rop, mpz_t op1, mpz_t op2);
#define mpz_gcd_ui __gmpz_gcd_ui
MKL_GMP_API unsigned long int mpz_gcd_ui (mpz_t rop, mpz_t op1, unsigned long int op2);
#define mpz_gcdext __gmpz_gcdext
MKL_GMP_API void mpz_gcdext (mpz_t g, mpz_t s, mpz_t t, mpz_t a, mpz_t b);
#define mpz_lcm __gmpz_lcm
MKL_GMP_API void mpz_lcm (mpz_t rop, mpz_t op1, mpz_t op2);
#define mpz_lcm_ui __gmpz_lcm_ui
MKL_GMP_API void mpz_lcm_ui (mpz_t rop, mpz_t op1, unsigned long int op2);
#define mpz_invert __gmpz_invert
MKL_GMP_API int mpz_invert (mpz_t rop, mpz_t op1, mpz_t op2);
#define mpz_jacobi __gmpz_jacobi
MKL_GMP_API int mpz_jacobi (mpz_t a, mpz_t b);
#define mpz_legendre mpz_jacobi
#define mpz_kronecker mpz_jacobi
#define mpz_kronecker_si __gmpz_kronecker_si
MKL_GMP_API int mpz_kronecker_si (mpz_t a, long b);
#define mpz_kronecker_ui __gmpz_kronecker_ui
MKL_GMP_API int mpz_kronecker_ui (mpz_t a, unsigned long b);
#define mpz_si_kronecker __gmpz_si_kronecker
MKL_GMP_API int mpz_si_kronecker (long a, mpz_t b);
#define mpz_ui_kronecker __gmpz_ui_kronecker
MKL_GMP_API int mpz_ui_kronecker (unsigned long a, mpz_t b);
#define mpz_remove __gmpz_remove
MKL_GMP_API unsigned long int mpz_remove (mpz_t rop, mpz_t op, mpz_t f);
#define mpz_fac_ui __gmpz_fac_ui
MKL_GMP_API void mpz_fac_ui (mpz_t rop, unsigned long int op);
#define mpz_bin_ui __gmpz_bin_ui
MKL_GMP_API void mpz_bin_ui (mpz_t rop, mpz_t n, unsigned long int k);
#define mpz_bin_uiui __gmpz_bin_uiui
MKL_GMP_API void mpz_bin_uiui (mpz_t rop, unsigned long int n, unsigned long int k);
#define mpz_fib_ui __gmpz_fib_ui
MKL_GMP_API void mpz_fib_ui (mpz_t fn, unsigned long int n);
#define mpz_fib2_ui __gmpz_fib2_ui
MKL_GMP_API void mpz_fib2_ui (mpz_t fn, mpz_t fnsub1, unsigned long int n);
#define mpz_lucnum_ui __gmpz_lucnum_ui
MKL_GMP_API void mpz_lucnum_ui (mpz_t ln, unsigned long int n);
#define mpz_lucnum2_ui __gmpz_lucnum2_ui
MKL_GMP_API void mpz_lucnum2_ui (mpz_t ln, mpz_t lnsub1, unsigned long int n);

/* Comparison functions */
#define mpz_cmp __gmpz_cmp
MKL_GMP_API int mpz_cmp (mpz_t op1, mpz_t op2);
#define mpz_cmp_d __gmpz_cmp_d
MKL_GMP_API int mpz_cmp_d (mpz_t op1, double op2);
#define mpz_cmp_si __gmpz_cmp_si
MKL_GMP_API int mpz_cmp_si (mpz_t op1, signed long int op2);
#define mpz_cmp_ui __gmpz_cmp_ui
MKL_GMP_API int mpz_cmp_ui (mpz_t op1, unsigned long int op2);
#define mpz_cmpabs __gmpz_cmpabs
MKL_GMP_API int mpz_cmpabs (mpz_t op1, mpz_t op2);
#define mpz_cmpabs_d __gmpz_cmpabs_d
MKL_GMP_API int mpz_cmpabs_d (mpz_t op1, double op2);
#define mpz_cmpabs_ui __gmpz_cmpabs_ui
MKL_GMP_API int mpz_cmpabs_ui (mpz_t op1, unsigned long int op2);
#define mpz_sgn __gmpz_sgn
MKL_GMP_API int mpz_sgn (mpz_t op);

/* Logical and bit manipulation functions */
#define mpz_and __gmpz_and
MKL_GMP_API void mpz_and (mpz_t rop, mpz_t op1, mpz_t op2);
#define mpz_ior __gmpz_ior
MKL_GMP_API void mpz_ior (mpz_t rop, mpz_t op1, mpz_t op2);
#define mpz_xor __gmpz_xor
MKL_GMP_API void mpz_xor (mpz_t rop, mpz_t op1, mpz_t op2);
#define mpz_com __gmpz_com
MKL_GMP_API void mpz_com (mpz_t rop, mpz_t op);
#define mpz_popcount __gmpz_popcount
MKL_GMP_API unsigned long int mpz_popcount (mpz_t op);
#define mpz_hamdist __gmpz_hamdist
MKL_GMP_API unsigned long int mpz_hamdist (mpz_t op1, mpz_t op2);
#define mpz_scan0 __gmpz_scan0
MKL_GMP_API unsigned long int mpz_scan0 (mpz_t op, unsigned long int starting_bit);
#define mpz_scan1 __gmpz_scan1
MKL_GMP_API unsigned long int mpz_scan1 (mpz_t op, unsigned long int starting_bit);
#define mpz_setbit __gmpz_setbit
MKL_GMP_API void mpz_setbit (mpz_t rop, unsigned long int bit_index);
#define mpz_clrbit __gmpz_clrbit
MKL_GMP_API void mpz_clrbit (mpz_t rop, unsigned long int bit_index);
#define mpz_tstbit __gmpz_tstbit
MKL_GMP_API int mpz_tstbit (mpz_t op, unsigned long int bit_index);

/* Input and output functions */
#define mpz_out_str __gmpz_out_str
#define mpz_inp_str __gmpz_inp_str
#define mpz_out_raw __gmpz_out_raw
#define mpz_inp_raw __gmpz_inp_raw
#if defined(_MKL_GMP_FILE)
MKL_GMP_API size_t mpz_out_str (FILE * stream, int base, mpz_t op);
MKL_GMP_API size_t mpz_inp_str (mpz_t rop, FILE * stream, int base);
MKL_GMP_API size_t mpz_out_raw (FILE * stream, mpz_t op);
MKL_GMP_API size_t mpz_inp_raw (mpz_t rop, FILE * stream);
#endif /* defined(_MKL_GMP_FILE) */

/**
Random number functions
*/
/* Random state initialization */
#define gmp_randinit_default __gmp_randinit_default
MKL_GMP_API void gmp_randinit_default (gmp_randstate_t state);
#define gmp_randinit_lc_2exp __gmp_randinit_lc_2exp
MKL_GMP_API void gmp_randinit_lc_2exp (gmp_randstate_t state, mpz_t a, unsigned long c, unsigned long m2exp);
#define gmp_randinit_lc_2exp_size __gmp_randinit_lc_2exp_size
MKL_GMP_API int gmp_randinit_lc_2exp_size (gmp_randstate_t state, unsigned long size);
#define gmp_randinit __gmp_randinit
MKL_GMP_API void gmp_randinit (gmp_randstate_t state, gmp_randalg_t alg, ...); /* obsolete */
#define gmp_randclear __gmp_randclear
MKL_GMP_API void gmp_randclear (gmp_randstate_t state);
/* Random state seeding */
#define gmp_randseed __gmp_randseed
MKL_GMP_API void gmp_randseed (gmp_randstate_t state, mpz_t seed);
#define gmp_randseed_ui __gmp_randseed_ui
MKL_GMP_API void gmp_randseed_ui (gmp_randstate_t state, unsigned long int seed);

#define mpz_urandomb __gmpz_urandomb
MKL_GMP_API void mpz_urandomb (mpz_t rop, gmp_randstate_t state, unsigned long int n);
#define mpz_urandomm __gmpz_urandomm
MKL_GMP_API void mpz_urandomm (mpz_t rop, gmp_randstate_t state, mpz_t n);
#define mpz_rrandomb __gmpz_rrandomb
MKL_GMP_API void mpz_rrandomb (mpz_t rop, gmp_randstate_t state, unsigned long int n);
#define mpz_random __gmpz_random
MKL_GMP_API void mpz_random (mpz_t rop, mp_size_t max_size); /* obsolete */
#define mpz_random2 __gmpz_random2
MKL_GMP_API void mpz_random2 (mpz_t rop, mp_size_t max_size); /* obsolete */

/**
Integer import and export
*/
#define mpz_import __gmpz_import
MKL_GMP_API void mpz_import (mpz_t rop, size_t count, int order, int size, int endian, size_t nails, const void * op);
#define mpz_export __gmpz_export
MKL_GMP_API void * mpz_export (void * rop, size_t * countp, int order, int size, int endian, size_t nails, mpz_t op);

/* Miscelaneous functions */
#define mpz_fits_ulong_p __gmpz_fits_ulong_p
MKL_GMP_API int mpz_fits_ulong_p (mpz_t op);
#define mpz_fits_slong_p __gmpz_fits_slong_p
MKL_GMP_API int mpz_fits_slong_p (mpz_t op);
#define mpz_fits_uint_p __gmpz_fits_uint_p
MKL_GMP_API int mpz_fits_uint_p (mpz_t op);
#define mpz_fits_sint_p __gmpz_fits_sint_p
MKL_GMP_API int mpz_fits_sint_p (mpz_t op);
#define mpz_fits_ushort_p __gmpz_fits_ushort_p
MKL_GMP_API int mpz_fits_ushort_p (mpz_t op);
#define mpz_fits_sshort_p __gmpz_fits_sshort_p
MKL_GMP_API int mpz_fits_sshort_p (mpz_t op);

#define mpz_odd_p(op) (mpz_get_ui(op) & 1)
#define mpz_even_p(op) (! mpz_odd_p(op))

#define mpz_size __gmpz_size
MKL_GMP_API size_t mpz_size (mpz_t op);
#define mpz_sizeinbase __gmpz_sizeinbase
MKL_GMP_API size_t mpz_sizeinbase (mpz_t op, int base);
/* ******************* Integer *********************** */

#define gmp_fprintf __gmp_fprintf
#if defined(_MKL_GMP_FILE)
/* @@@ FIXME: We supress formatted output for now */
MKL_GMP_API int gmp_fprintf (FILE *fp, const char *fmt, mpz_t z);
#endif

#define mpn_add_1 __gmpn_add_1
MKL_GMP_API mp_limb_t mpn_add_1 (mp_limb_t * rp, const mp_limb_t * s1p, mp_size_t n, mp_limb_t s2limb);
#define mpn_add_n __gmpn_add_n
MKL_GMP_API mp_limb_t mpn_add_n (mp_limb_t * rp, const mp_limb_t * s1p, const mp_limb_t * s2p, mp_size_t n);
/* requires n1 >= n2 */
#define mpn_add(rp, s1p, n1, s2p, n2) mpn_add_1((rp)+(n2), (s1p)+(n2), (n1)-(n2), mpn_add_n(rp, s1p, s2p, n2))
#define mpn_sub_1 __gmpn_sub_1
MKL_GMP_API mp_limb_t mpn_sub_1 (mp_limb_t * rp, const mp_limb_t * s1p, mp_size_t n, mp_limb_t s2limb);
#define mpn_sub_n __gmpn_sub_n
MKL_GMP_API mp_limb_t mpn_sub_n (mp_limb_t * rp, const mp_limb_t * s1p, const mp_limb_t * s2p, mp_size_t n);
/* requires n1 >= n2 */
#define mpn_sub(rp, s1p, n1, s2p, n2) mpn_sub_1((rp)+(n2), (s1p)+(n2), (n1)-(n2), mpn_sub_n(rp, s1p, s2p, n2))
#define mpn_addmul_1 __gmpn_addmul_1
MKL_GMP_API mp_limb_t mpn_addmul_1(mp_limb_t * rop, const mp_limb_t * op1, mp_size_t sz, mp_limb_t op2);
#define mpn_submul_1 __gmpn_submul_1
MKL_GMP_API mp_limb_t mpn_submul_1(mp_limb_t * rop, const mp_limb_t * op1, mp_size_t sz, mp_limb_t op2);
#define mpn_cmp __gmpn_cmp
MKL_GMP_API int mpn_cmp (const mp_limb_t * s1p, const mp_limb_t * s2p, mp_size_t n);
#define mpn_mul __gmpn_mul
MKL_GMP_API void mpn_mul (mp_limb_t * rp, const mp_limb_t * s1p, mp_size_t n1, const mp_limb_t * s2p, mp_size_t n2);
#define mpn_mul_n __gmpn_mul_n
MKL_GMP_API void mpn_mul_n (mp_limb_t * rp, const mp_limb_t * s1p, const mp_limb_t * s2p, mp_size_t n);

#ifdef __cplusplus
}
#endif

#endif /* __MKL_GMP_H__ */
