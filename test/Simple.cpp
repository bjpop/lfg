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
   Sprng * gen0, **gens;
   gen0 = SelectType(SPRNG_LFG);
   gen0->init_sprng(0, 1, seed, 0);
   gen0->spawn_sprng(2, &gens);
   printRandInts(gen0, 100);
   //printRandFlts(gen0, 100);
   //printRandDoubles(gen0, 100);
   printRandInts(gens[0], 100);
   printRandInts(gens[1], 100);
   return 0;
}
