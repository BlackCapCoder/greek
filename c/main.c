#include <stdio.h>
#include <stdbool.h>


// ----------

#ifdef __GNUC__

#define ASSUME(cond) \
  do { if (!(cond)) __builtin_unreachable (); } while (0);

#elif __CLANG__

#define ASSUME(cond) \
  __builtin_assume (cond);

#endif


#define ASSUME_INBOUND(needle,hay) \
  ASSUME (needle >= 0); \
  ASSUME (needle <  sizeof (hay));

// ----------

typedef bool
Bit;

typedef enum
{
  X, Y, Z
}
__attribute__ ((__packed__))
Trit;


// ----------


typedef struct
{
  Trit buf [4096 * 8 * 2];
  int  ptr; // This is significantly faster than a pointer

} Tape;


// It is not true in general that `ptr` is inbounds,
// but forcing the compiler to accept it gives us
// a very significant speed boost!


__attribute__ ((pure))
inline const
Trit read_tape (const Tape * tape)
{
  ASSUME_INBOUND (tape->ptr, tape->buf);

  return tape->buf [tape->ptr];
}

inline
void write_tape (Trit val, Tape * tape)
{
  ASSUME_INBOUND (tape->ptr, tape->buf);

  tape->buf [tape->ptr] = val;
}

// ----------


typedef struct
{
  Bit  state : 1;
  Tape tape;

} Machine;


// Machine states
#define A false
#define B true

// Directions
#define L false
#define R true


// ----------

typedef struct
{
  const Bit  state  : 1;
  const Trit symbol : 2;
  const Bit  dir    : 1;
}
const
Trans;


//   | A  | B
// ============
// X | BYR  AZL
// Y | AZL  BZR
// Z | AYL  AXR
//
const Trans system0 []
=
  { {B, Y, R}, {A, Z, L}
  , {A, Z, L}, {B, Z, R}
  , {A, Y, L}, {A, X, R}
  };


__attribute__ ((pure))
inline const
Trans lookup (Bit st, Trit sym)
{
  const int ix = (sym << 1) | st;

  ASSUME_INBOUND (ix, system0);

  return system0 [ix];
}

inline
void transition (Trans trans, Machine * m)
{
  m->state = trans.state;
  write_tape (trans.symbol, &m->tape);

  /* m->tape.ptr += trans.dir*2 - 1; */

  if (trans.dir)
    m->tape.ptr--;
  else
    m->tape.ptr++;
}

inline
void step (Machine * m)
{
  transition (lookup (m->state, read_tape (&m->tape)), m);
}

// ----------

/* void trace (Machine * m) */
/* { */
/*   putchar (m->state ? 'B' : 'A'); */
/*  */
/*   switch (read_tape (&m->tape)) */
/*   { */
/*     case (X): putchar ('X'); break; */
/*     case (Y): putchar ('Y'); break; */
/*     case (Z): putchar ('Z'); break; */
/*   } */
/*  */
/*   printf (":%d\n", m->tape.ptr); */
/* } */

bool run (void)
{
  Machine m = { A, {{}, sizeof (m.tape.buf) >> 1} };

  for (int i = 0; i < 800000000; i++)
  {
    step (&m);
  }

  return m.state;
}


int main (void)
{
  return run ();
}

