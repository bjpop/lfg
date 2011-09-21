#include "sprng.h"
#include "sprng_cpp.h"

void printRandInts(Sprng *rng, int num)
{
   int i;
   //rng->print_sprng();
   for (i = 0; i < num; i++)
   {
      printf ("%d\n", rng->get_rn_int());
   }
}

void printRandFlts(Sprng *rng, int num)
{
   int i;
   //rng->print_sprng();
   for (i = 0; i < num; i++)
   {
      printf ("%f\n", rng->get_rn_flt());
   }
}



void printRandDoubles(Sprng *rng, int num)
{
   int i;
   //rng->print_sprng();
   for (i = 0; i < num; i++)
   {
      printf ("%.16lf\n", rng->get_rn_dbl());
   }
}

int main (void)
{
   int seed = 42;
   Sprng * gen1;
   gen1 = SelectType(SPRNG_LFG);
   gen1->init_sprng(0, 1, seed, 0);
   printRandInts(gen1, 100);
   printRandFlts(gen1, 100);
   printRandDoubles(gen1, 100);
   return 0;
}
