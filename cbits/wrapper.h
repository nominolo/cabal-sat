#ifndef __SOLVER_WRAPPER__
#define __SOLVER_WRAPPER__

typedef struct solver solver_t;
typedef int lbool;

solver_t *minisat_new_solver(void);
void      minisat_delete_solver(solver_t *);

int       minisat_new_var(solver_t *s, int polarity, int dvar);

// Variables must have been returned via new_var from the same solver!
//
// Literals:  0 - undefined
//            1 - variable 0, positive
//           -1 - variable 0, negated
//   in general:  n = variable |n| - 1, positive (if n > 0)
//                n = variable |n| - 1, negated (if n < 0)

// Arrays of literals (int *) are zero-terminated.

int       minisat_add_clause1(solver_t *s, int l1);
int       minisat_add_clause2(solver_t *s, int l1, int l2);
int       minisat_add_clause3(solver_t *s, int l1, int l2, int l3);
int       minisat_add_clause(solver_t *s, int *ls);

int       minisat_solve(solver_t *s, int do_simp, int *assumptions);

lbool     minisat_model_value(solver_t *s, int var);

#endif
