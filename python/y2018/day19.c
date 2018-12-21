#include <stdio.h>
#include <stdlib.h>

long generate_r2() {
  long r2 = 0, r5 = 0;
  r2 = 2;                       /* 17 */
  r2 *= r2;                     /* 18 */
  r2 *= 19;                     /* 19 */
  r2 *= 11;                     /* 20 */
  r5 += 3;                      /* 21 */
  r5 *= 22;                     /* 22 */
  r5 += 3;                      /* 23 */
  r2 += r5;                     /* 24 */
  r5 = 27;                      /* 27 */
  r5 *= 28;                     /* 28 */
  r5 += 29;                     /* 29 */
  r5 *= 30;                     /* 30 */
  r5 *= 14;                     /* 31 */
  r5 *= 32;                     /* 32 */
  r2 += r5;                     /* 33 */
  return r2;
}

int main(int argc, char **argv) {
  long r0 = 0;
  long r1 = 0, r2 = 0, r5 = 0;
  r2 = generate_r2();
  printf("r2 = %ld\n", r2);

  for (r1 = 1; r1 <= r2; ++r1)
    if ((r2 % r1) == 0)
      r0 += r1;
  printf("sum of factors = %ld\n", r0);
  
  exit(0);
  return 0;
}
