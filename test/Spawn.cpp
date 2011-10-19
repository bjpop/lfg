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
      printf ("%.20lf\n", rng->get_rn_dbl());
   }
}

void spawnTest (Sprng *gen, int depth)
{
   int newDepth;
   Sprng **gens;
   if (depth <= 0)
      return;
   printRandInts(gen, 100);
   printRandDoubles(gen, 100);
   gen->spawn_sprng(2, &gens);
   newDepth = depth - 1;
   spawnTest(gens[0], newDepth);
   spawnTest(gens[1], newDepth);
}

int main (void)
{
   Sprng *gen;
   int seed = 42;
   int depth = 6;
   gen = SelectType(SPRNG_LFG);
   gen->init_sprng(0, 1, seed, 0);
   spawnTest(gen, depth);
   return 0;
}
