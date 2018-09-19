/* 
 * CS:APP Data Lab 
 * 
 * <Please put your name and userid here>
 *  Lee Woojoo 	cs20170478
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting an integer by more
     than the word size.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   *int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implent floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operators (! ~ & ^ | + << >>)
     that you are allowed to use for your implementation of the function. 
     The max operator count is checked by dlc. Note that '=' is not 
     counted; you may use as many of these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */


#endif
/* Copyright (C) 1991-2014 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
/* 
 * bitAnd - x&y using only ~ and | 
 *   Example: bitAnd(6, 5) = 4
 *   Legal ops: ~ |
 *   Max ops: 8
 *   Rating: 1
 */
int bitAnd(int x, int y) {
 /*use the de morgan's law for logic with the fact (x and y) = ~(x nand y)*/
 int nand = ~x | ~y;
 return ~nand;
}
/* 
 * getByte - Extract byte n from word x
 *   Bytes numbered from 0 (LSB) to 3 (MSB)
 *   Examples: getByte(0x12345678,1) = 0x56
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2
 */
int getByte(int x, int n) {
 /*first bring target to LSB position and then truncate it other than LSB */
 int lsB = 0xFF;
 int shift_amount= n<<3;	// this amounts to n*8 where 8 = (# of bit in a byte)
 int shifted_x = x>>shift_amount;
 return shifted_x & lsB;
}
/* 
 * logicalShift - shift x to the right by n, using a logical shift
 *   Can assume that 0 <= n <= 31
 *   Examples: logicalShift(0x87654321,4) = 0x08765432
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3 
 */
int logicalShift(int x, int n) {
 /*Pick up the most significant bit(msb) of x and then add it to x>>n(arithmatic shift) in a proper digit position
   Here, note that 1+~n = -n 
 */
 int arith_shift = x >> n;
 int msbit = (x>>31) & 0x01;			// make it into the form 0x00....0b where b= 0 or 1
 int sol_dummy = msbit << (32 + (1+~n));	

 sol_dummy = (sol_dummy>>1)<<1;			// exception handling for n=0 so that we make sol_dummy = 0x00; if n!=0, this doesn't matter.
 return arith_shift + sol_dummy;		// this eliminates augmented msb by adding the same bit at a proper position where augmentation begins

}
/*
 * bitCount - returns count of number of 1's in word
 *   Examples: bitCount(5) = 2, bitCount(7) = 3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 40
 *   Rating: 4
 */
int bitCount(int x) {
/*use masks(=dum) step by step to gather all 1s in given x*/
 int quad_01=0x55;			// 0x01010101
 int di_doub_1=0x33;			// 0x00110011
 int quad_1 = 0x0f;			// 0x00001111
 int octa_1 = 0xff;			// 0x11111111

 /*dums are just 32bit numbers where the numbers we defined above are uniformly distributed respectively*/
 
 int dum1_half = (quad_01<<8) +quad_01;
 int dum1 = (dum1_half << 16) + dum1_half;
 int dum2_half = (di_doub_1 << 8 ) +di_doub_1;
 int dum2 = (dum2_half <<16) + dum2_half;
 int dum3_half = (quad_1 <<8) + quad_1;
 int dum3 = (dum3_half <<16) +dum3_half;
 int dum4 = (octa_1 <<16) +octa_1;
 int dum5 = (octa_1<<8) + octa_1;

 /*dums function as masks*/

 int unitby_2 = ((x>>1)&dum1) + (x & dum1);			// implies how many 1s are in each unit;
 int unitby_4 = ((unitby_2>>2) & dum2)  + (unitby_2 & dum2);
 int unitby_8 = ((unitby_4>>4) & dum3) + (unitby_4 & dum3);
 int unitby_16 = ((unitby_8 >>8) & dum4) + (unitby_8 & dum4);  
 return ((unitby_16 >> 16) & dum5) + (unitby_16 &dum5) ;	// ==unitby_32 (total)
}

/* 
 * bang - Compute !x without using !
 *   Examples: bang(3) = 0, bang(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4 
 */
int bang(int x) {
 /*Observe that at least one of the most significant bit (msbit) of x and -x would be 1 unless x==0 
   due to the sign issue : 0 == 0x00000000 == -0;
 */
 int neg_x = 1+~x;
 int msbit_x = (x>>31)&0x01;	int msbit_neg_x = (neg_x >>31)&0x01;
 int ethr_0_or_1 = msbit_x | msbit_neg_x;		// this would be either 0x01(if x!=0) or 0x00(1f x==0)
 return (ethr_0_or_1 +1) & 0x01; 			// to make it either 0x00(if x!=0) or 0x01(if x==0);
}

/* 
 * tmin - return minimum two's complement integer 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 4
 *   Rating: 1
 */
int tmin(void) {
/*The unique 1 is on the leftmost(32nd) side*/
 int ten_mill = 0x80;		// 0x10000000
 return ten_mill <<24;
}
/* 
 * fitsBits - return 1 if x can be represented as an 
 *  n-bit, two's complement integer.
 *   1 <= n <= 32
 *   Examples: fitsBits(5,3) = 0, fitsBits(-4,3) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int fitsBits(int x, int n) {
 /*Note that by the 2's complement int mechanism, this issue depends on the bits other than the last (n-1) bits; call it 'others' 
   If 0 and 1 are mixed in 'others', x would be fail to fit. 
 */
 int n_minus_1 = n + (~1+1);
 int others=x >> n_minus_1;
 int lsbit_of_others= others & 0x01;
 // negative case handling : if others =111...11, x must be fitted (consider x=-4, n=3); so we should evade this;
 int zero_or_not = others + lsbit_of_others;		// => either  0x00(fit) or not(fail).		

 return !(zero_or_not);
}
/* 
 * divpwr2 - Compute x/(2^n), for 0 <= n <= 30
 *  Round toward zero
 *   Examples: divpwr2(15,1) = 7, divpwr2(-33,4) = -2
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int divpwr2(int x, int n) {
 /*The point is that the right shift(>>) brings rounding toward -infinity, so we  have to concern x<0 case
   Also, since rounding doesn't occur when 2^n(2 power n) divides x, we just need to concern the last n-bit of x 
 */
 int ten_mill = 0x80;
 int T_min = ten_mill << 24;			//0x80000000
 int msbit_in_place = x & T_min;
 int msbit = (msbit_in_place >>31) & 0x01;

 int all_one = ~0x00;
 int n_lsbits = x & ~(all_one <<n);		//the last n-bits of x; if this doesn't equal to 0, x must not be divided by 2^n

 int divpwr_shift = x>>n;
 int aux_factor = msbit & !!n_lsbits;		//if x<0(i.e. msbit==1) and rounding occurs(i.e. n_lsbits !=0), then aux_factor==1;
 return divpwr_shift + aux_factor;		//This prevents round toward -inf by simply adding 1; Else, aux_factor doesn't affect   
}
/* 
 * negate - return -x 
 *   Example: negate(1) = -1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int negate(int x) {
 /*This is classic issue considering x + ~x = -1 = 0xffffffff*/ 
  return 1+ (~x);
}
/* 
 * isPositive - return 1 if x > 0, return 0 otherwise 
 *   Example: isPositive(-1) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 3
 */
int isPositive(int x) {
 /*Just follow the comments*/
 int T_min = 0x80<<24;					// 0x80000000
 int ethr_0_or_neg_1 = (x & T_min) >>31; 		// if x>=0, this would be 0; if x<0, this would be -1
 int zero_trap = !(x | 0x00);				// exception handling : if x==0, this would be 1; otherwise, this would be 0;
 return !(ethr_0_or_neg_1 + zero_trap);			// the inner sum would be either zero(if x>0) or nonzero(if x<=0)	
}
/* 
 * isLessOrEqual - if x <= y  then return 1, else return 0 
 *   Example: isLessOrEqual(4,5) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isLessOrEqual(int x, int y) {
 /* We can determine this by considering the following three cases
  1) x<0 and y>=0
  2) x-y <0 when x and y have the same signs (then unnecessary over(under)flow doesn't occur)
  3) x=y
*/
 int T_min = 0x80 <<24;
 int x_minus_y = x + (1 + ~y);

 int sign_of_x = ((x & T_min)>>31)&0x01;			// if x>=0, this would be 0; otherwise, 1
 int sign_of_y = ((y & T_min)>>31)&0x01;
 int sign_of_x_minus_y = ((x_minus_y & T_min)>>31)&0x01;	
 int same_signs = !(sign_of_x ^ sign_of_y);			// if sign(x)=sign(y), then this would be 1; otherwise, 0; 
 // we want 1) or 2) or 3) to hold. i.e. to return 1 (resp). 
 return ((sign_of_x)& !(sign_of_y)) | (sign_of_x_minus_y & same_signs) | !(x_minus_y);			
}
/*
 * ilog2 - return floor(log base 2 of x), where x > 0
 *   Example: ilog2(16) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 90
 *   Rating: 4
 */
int ilog2(int x) {
 /*The point is to get the greatest nonzero bit of x and its position (i.e. the exact num)
  To do this,  we shall copy the bit from its original position to the least significant bit position
  so that the result would be partitioned into fore 0s and rear 1s;
  Then we can estimate the original position of 1 (which we desire) by counting the number of rear 1s except leading 1;   
*/ 
 // Since we're going to count the number of 1s, we take analogous process as in bitCount(x) above
 int T_min=0x80<<24;
 int quad_01=0x55;
 int di_doub_1=0x33;
 int quad_1=0x0f;
 int octa_1=0xff;

 int hemidum1=(quad_01 <<8) +quad_01;
 int dum1 = (hemidum1<<16)+hemidum1;
 int hemidum2=(di_doub_1<<8)+di_doub_1;
 int dum2 =(hemidum2<<16)+hemidum2;
 int hemidum3=(quad_1<<8)+quad_1;
 int dum3=(hemidum3<<16)+hemidum3;
 int dum4=(octa_1<<16)+octa_1;
 int dum5=(octa_1<<8)+octa_1;

 int first_copy = x |(x>>1);
 int second_copy = first_copy | (first_copy>>2);
 int third_copy = second_copy | (second_copy>>4);
 int fourth_copy = third_copy | (third_copy >>8);
 int final_copy = fourth_copy | (fourth_copy>>16);		//this would be enough (w/o repitition) since we have at most 32-bits.
 
 int only_greatest_nonzero=((final_copy+1)>>1);
 int pos_ogn = only_greatest_nonzero & (~T_min);		// exception handling: x=T_Min ;since x is never negative, we can always drop sign_bit;
 int consisting_ones=pos_ogn + (1+~1);				// == only_greatest_nonzero -1; we omitted the greatest nonzero bit from final_copy.
 
 // (Observe that ilog2(0x80)==ilog2(2^7)==7 and 0x80 brings 0xff (# of 1s : 8) after copying; so we should omit the leading 1)  
 
 int unit_2=((consisting_ones>>1)&dum1) + (consisting_ones&dum1);
 int unit_4=((unit_2>>2)&dum2)+(unit_2&dum2);
 int unit_8=((unit_4>>4)&dum3)+(unit_4&dum3);
 int unit_16=((unit_8>>8)&dum4)+(unit_8&dum4);
 return ((unit_16>>16)&dum5)+(unit_16&dum5);			
 // this returns the answer we want since the position of the greatest nonzero bit(say, kth from the right) implies ilog2 (as k-1);    
}

/* 
 * float_neg - Return bit-level equivalent of expression -f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned float_neg(unsigned uf) {
 /*If x is NaN, return uf itself. Otherwise, change the sign bit and then return*/ 
 unsigned sign_one = 0x80000000; 				//the sign part of floating point set to be 1  
 int octa_one= 0xFF;
 int exp_part_mask = octa_one<<23;				//0x78f00000 exactly for exp part of float point
 int exp_part = uf & exp_part_mask;			
 int frac_part_mask = ~(sign_one | exp_part_mask);		//=0x007fffff  where the number of 1 is 23 ; exactly for frac part of float point
 int frac_part = uf & frac_part_mask;

 if ((exp_part == exp_part_mask) && (frac_part || 0x00)){				// x == NaN (exp ==11111111 and frac != 0)
  return uf;				
 }
 return uf+ sign_one;
}
/* 
 * float_i2f - Return bit-level equivalent of expression (float) x
 *   Result is returned as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point values.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_i2f(int x) {
 /*The idea is that we first seperately get s(sign), exp, frac part
  and then for frac part, we get guard, round, sticky bits to round it as given precision. (Note: frac part has 23bit)  
 */
 
 // for some initiation, refer to float_neg(x) function above.
 int x_backup=x;
 int sgn =0x80000000;	unsigned fin_sgn=0;
 int sign_part = x & sgn;
 int E =0;
 unsigned bias = 0x7f;				//=127 for single precision
 unsigned exp=0x7f800000;
 unsigned  frac_mask =0x007fffff;   
 unsigned frac=0;
 int temp_val=0x00;	
 int temp_po_1=0x00; 	int temp_po_2=0x00;		//those are temporary values/positions
 int g=0; int r=0; int s=0;				// guard/round/sticky bit
 
 
 if (x==sgn){				//exception handling : x==T_min
  return 0xcf000000;			//so that exp_part = 10011110 = 158 = bias + 31;
 }
 if (x==0){				//exception handling : x==0; just return 0, itself.
  return 0;
 }
 
 if (sign_part == sgn){
  x= 1+~x;				//we'll unifiy to nonnegative case. Determine the sign as fin_sgn.
  x_backup=x;
  fin_sgn=sgn;
 }
 else{
  fin_sgn=0;
 }

 while (x&0xfffffffe){				//try to calculate E by shifting until onl`y one digit left; 0xfffffffe==~0x01
  x=x>>1;
  E+=1;
 }
 x=x_backup;
 exp=E+bias;					//Here's an exp value

 temp_val = E+0xffffffe9;						//E-23 to check precision for frac part
 
 if (E<23){								//where we don't need to round
  frac = (x<<(~temp_val+1))&frac_mask;					//use 23-E=-(temp_val) to get frac part;
 }
 else{									//where the rounding needs; first, we don't round up
  frac=(x>>temp_val)&frac_mask;						
 
  temp_po_1 = 0x01<<temp_val;						//to get guard bit (that would be the least significant bit of the result)	
  temp_po_2 = temp_po_1>>1;
  g= x& temp_po_1;  r=x&(temp_po_2); s=x&(temp_po_2+0xffffffff); 
  frac = frac + (r && (s||g));							//Rounding up (Note: s is all remaining bits under r)

 // how this works: note that rounding up occurs when r bit is 1 and either (s bits are nonzero (round off)) or (s bits are 0 but g bit is 1 (round to even))  
 } 

 return fin_sgn+(exp<<23)+frac;
}
/* 
 * float_twice - Return bit-level equivalent of expression 2*f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_twice(unsigned uf) {
 /*Note that 2 has s==0, E==1 and frac==0. Assume, for general case, uf is normalized.
  If each part of uf is s0, E0, and frac0, the result should be s==0, E==E0+1,and frac==frac*0==0 (resp) 
 */
 unsigned T_min=0x80<<24;		//just the name is T_min. not signed!
 unsigned exp_mask=0xff<<23;
 unsigned frac_mask = ~(T_min+exp_mask);
 unsigned exp_part=uf & exp_mask;
 unsigned frac_part =uf &frac_mask;
 unsigned sign_part=uf&T_min;

 
 if (exp_part==(0xff<<23)){		// special values where exp==11111111 
  return uf;
 }
 if (exp_part==0x00){			// denormalized values(exp==0); then we should operate for frac part(times 2)
  frac_part = (frac_part <<1);		// overflow is okay since adding 1 to exp_part is natural computation
 }					
 else{					// normalized
  exp_part +=(0x01<<23);		// this amounts to E0+1;
  exp_part =exp_part&exp_mask;
 }
  return sign_part+exp_part+frac_part;
}
