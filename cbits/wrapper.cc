#include "core/Solver.h"
#include "simp/SimpSolver.h"

extern "C" {
#include "wrapper.h"
}

using namespace Minisat;

struct solver : public Solver {

  vec<Lit> tmp;
  
  solver() : Solver() { }
  ~solver() {}

};

static inline Lit fromDimacs(int l) {
  int isNegative = l < 0;
  return mkLit( (isNegative ? -l : l) - 1, isNegative);
}

static inline int toDimacs(Lit l) {
  Var v = var(l) + 1;
  return sign(l) ? -v : v;
}

extern "C" {

  solver_t *minisat_new_solver(void) { return new solver();
  }
  void minisat_delete_solver(solver *s) { delete s; }

  int minisat_new_var(solver *s, int polarity, int dvar) {
    return s->newVar(polarity != 0, dvar != 0);
  }

  int minisat_add_clause1(solver *s, int l1) {
    return (int)s->addClause(fromDimacs(l1));
  }

  int minisat_add_clause2(solver *s, int l1, int l2) {
    // printf("addClause(%d, %d)\n", l1, l2);
    // Lit p1 = fromDimacs(l1);
    // Lit p2 = fromDimacs(l2);
    // printf("         (%d,%d, %d,%d)\n", sign(p1), var(p1), sign(p2), var(p2));
    return (int)s->addClause(fromDimacs(l1), fromDimacs(l2));
  }

  int minisat_add_clause3(solver *s, int l1, int l2, int l3) {
    return (int)s->addClause(fromDimacs(l1), fromDimacs(l2), fromDimacs(l3));
  }
  
  int minisat_add_clause(solver_t *s, int *ls /* zero-terminated */) {
    s->tmp.clear();
    for (int i = 0; ls[i] != 0; i++) {
      s->tmp.push(fromDimacs(ls[i]));
    }
    return (int)s->addClause(s->tmp);
  }

  int minisat_solve(solver_t *s, int do_simp, int *assumptions) {
    // printf("clauses: %d\n", s->nClauses());
    s->tmp.clear();
    for (int i = 0; assumptions[i] != 0; i++) {
      s->tmp.push(fromDimacs(assumptions[i]));
    }
    return s->solve(s->tmp); // , do_simp);
  }

  int minisat_model_value(solver_t *s, int v) {
    return toInt(s->model[v]);
  }
}
