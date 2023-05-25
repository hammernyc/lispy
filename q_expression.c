/**
 * compile with: cc -std=c99 -Wall q_expression.c mpc.c -ledit -o q_expression
 */

#include <stdio.h>
#include <stdlib.h>
#include "mpc.h"

#define LASSERT(args, cond, err) \
  if (!(cond)) { lval_del(args); return lval_err(err); }

#define LASSERT_PASSED_EMPTY(lval) \
  LASSERT(lval, lval->cell[0]->count != 0, "function passed {}.");

#define LASSERT_NUM_ARGS(lval, num) \
  LASSERT(lval, lval->count == num, "function passed incorrect number of arguments.");

typedef struct lval {
  int type;
  double num;
  char* err;
  char* sym;
  int count;
  struct lval** cell;
} lval;

lval* builtin(lval* a, char* func);
lval* builtin_op(lval*, char*);

enum { LVAL_ERR, LVAL_NUM, LVAL_SYM, LVAL_SEXPR, LVAL_QEXPR };

#ifdef _WIN32

static char buffer[2048];

char* readline(char* prompt) {
  fputs(prompt, stdout);
  fgets(buffer, 2048, stdin);
  char* cpy = malloc(strlen(buffer)+1);
  strcpy(cpy, buffer);
  cpy[strlen(cpy)-1] = '\0';
  return cpy;
}

void add_history(char* unused) {}

#else
#include <editline/readline.h>
#endif

/* Returns a pointer to a new number lval */ 
lval* lval_num(double x) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_NUM;
  v->num = x;
  return v;
}

/* Returns a pointer to a new error lval */ 
lval* lval_err(char* m) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_ERR;
  v->err = malloc(strlen(m) + 1);
  strcpy(v->err, m);
  return v;
}

/* Returns a pointer to a new symbol lval */ 
lval* lval_sym(char* s) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_SYM;
  v->sym = malloc(strlen(s) + 1);
  strcpy(v->sym, s);
  return v;
}

/* Returns a pointer to a new empty sexpr lval */
lval* lval_sexpr(void) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_SEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

/* Returns a pointer to a new empty qexpr lval */
lval* lval_qexpr(void) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_QEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

void lval_del(lval* v) {
  switch (v->type) {
    case LVAL_NUM: 
      break;
    case LVAL_ERR:
      free(v->err);
      break;
    case LVAL_SYM:
      free(v->sym);
      break;
    case LVAL_SEXPR:
    case LVAL_QEXPR:
      for (int i = 0; i < v->count; i++) {
        lval_del(v->cell[i]);
      }
      free(v->cell);
      break;
  }
  free(v);
}

lval* lval_add(lval* v, lval* x) {
  v->count++;
  v->cell = realloc(v->cell, sizeof(lval*) * v->count);
  v->cell[v->count-1] = x;
  return v;
}

lval* lval_pop(lval* v, int i) {
  lval* x = v->cell[i];

  /* shift memory after the item at `i` back one slot */
  memmove(&v->cell[i], &v->cell[i+1], sizeof(lval*) * (v->count-i-1));

  v->count--;
  
  /* Reallocate the memory used */
  v->cell = realloc(v->cell, sizeof(lval*) * v->count); 
  return x;
}

lval* lval_take(lval* v, int i) {
  lval* x = lval_pop(v, i);
  lval_del(v);
  return x;
}

void lval_print(lval* v);

void lval_expr_print(char open, lval* v, char close) {
  putchar(open);
  for (int i = 0; i < v->count; i++) {
    lval_print(v->cell[i]);
    /* Don't print trailing space if last element */
    if (i != (v->count-1)) {
      putchar(' ');
    }
  }
  putchar(close);
}

void lval_print(lval* v) {
  switch (v->type) {
    case LVAL_NUM: 
      printf("%g", v->num);
      break;
    case LVAL_ERR: 
      printf("Error: %s", v->err);
      break;
    case LVAL_SYM: 
      printf("%s", v->sym);
      break;
    case LVAL_SEXPR:
      lval_expr_print('(', v, ')');
      break;
    case LVAL_QEXPR:
      lval_expr_print('{', v, '}');
      break;
  }
}

void lval_println(lval* v) {
  lval_print(v);
  putchar('\n');
}

lval* builtin_head(lval* a) {
  LASSERT_NUM_ARGS(a, 1);
  LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
    "Function 'head' passed incorrect type!");  
  LASSERT_PASSED_EMPTY(a);

  lval* v = lval_take(a, 0);
  while (v->count > 1) { lval_del(lval_pop(v, 1)); }
  return v;
}

lval* builtin_tail(lval* a) {
  LASSERT_NUM_ARGS(a, 1);
  LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
    "Function 'tail' passed incorrect type!");
  LASSERT_PASSED_EMPTY(a);


  lval* v = lval_take(a, 0);
  lval_del(lval_pop(v, 0));
  return v;
}

lval* builtin_list(lval* a) {
  a->type = LVAL_QEXPR;
  return a;
}

lval* lval_eval(lval* v);

lval* lval_eval_expr(lval* v) {
  /* eval children */
  for (int i = 0; i < v->count; i++) {
    v->cell[i] = lval_eval(v->cell[i]);
  }

  /* check errors */
  for (int i = 0; i < v-> count; i++) {
    if (v->cell[i]->type == LVAL_ERR) {
      return lval_take(v, i);
    }
  }

  /* handle empty expression */
  if (!v->count) {
    return v;
  }

  /* handle single expression */
  if (v->count == 1) {
    return lval_take(v, 0);
  }

  /* ensure first el is symbol */
  lval* f = lval_pop(v, 0);
  if (f->type != LVAL_SYM) {
    lval_del(f);
    lval_del(v);
    return lval_err("S-expression must begin with a symbol.");
  }

  /* call builtin with operator */
  lval* result = builtin(v, f->sym);
  lval_del(f);
  return result;
}

lval* lval_eval(lval* v) {
  if (v->type == LVAL_SEXPR) {
    return lval_eval_expr(v);
  }
  return v;
}


lval* builtin_eval(lval* a) {
  LASSERT_NUM_ARGS(a, 1);
  LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
    "Function 'eval' passed incorrect type!");

  lval* x = lval_take(a, 0);
  x->type = LVAL_SEXPR;
  return lval_eval(x);
}

lval* lval_join(lval* x, lval* y) {
  /* For each cell in 'y' add it to 'x' */
  while (y->count) {
    x = lval_add(x, lval_pop(y, 0));
  }

  /* Delete the empty 'y' and return 'x' */
  lval_del(y);
  return x;
}

lval* builtin_join(lval* a) {
  // note - does not evaluate args before checking if is q-expr
  for (int i = 0; i < a->count; i++) {
    LASSERT(a, a->cell[i]->type == LVAL_QEXPR,
      "Function 'join' passed incorrect type.");
  }

  lval* x = lval_pop(a, 0);

  while (a->count) {
    x = lval_join(x, lval_pop(a, 0));
  }

  lval_del(a);
  return x;
}

lval* builtin_cons(lval* a) {
  LASSERT_NUM_ARGS(a, 2);
  LASSERT(a, a->cell[1]->type == LVAL_QEXPR,
    "Can only cons values onto q-expressions");

  lval* x = lval_add(lval_qexpr(), lval_pop(a,0)); 

  x = lval_join(x, lval_pop(a, 0));
  lval_del(a);
  return x;
}

lval* builtin_init(lval* a) {
  LASSERT_NUM_ARGS(a, 1);
  LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
    "Function 'init' passed incorrect type!");
  LASSERT_PASSED_EMPTY(a);

  lval* v = lval_take(a, 0);
  lval_del(lval_pop(v, v->count - 1));
  return v;
}

lval* builtin_len(lval* a) {
  LASSERT_NUM_ARGS(a, 1);
  LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
    "Can only find length of q-expressions");

  lval* x = lval_num(a->cell[0]->count);

  lval_del(a);
  return x;
}


lval* builtin_op(lval* a, char* op) {
  /* ensure all arguments are numbers*/
  for (int i = 0; i < a->count; i++) {
    if (a->cell[i]->type != LVAL_NUM) {
      lval_del(a);
      return lval_err("Cannot operate on non-number!");
    }
  }
  lval* x = lval_pop(a, 0);

  /* If no arguments and sub then perform unary negation */
  if ((strcmp(op, "-") == 0) && a->count == 0) {
    x->num = -x->num;
  }
  while (a->count > 0) {
    lval* y = lval_pop(a, 0);

    if (strcmp(op, "+") == 0) {
      x->num += y->num;
    }
    if (strcmp(op, "-") == 0) {
      x->num -= y->num;
    }
    if (strcmp(op, "*") == 0) {
      x->num *= y->num;
    }
    if (strcmp(op, "/") == 0) {
      if (y->num == 0) {
        lval_del(x);
        x = lval_err("Division By Zero!");
      } else {
       x->num /= y->num;
      }
    }
    if (strcmp(op, "%") == 0) {
      if (y->num == 0) {
        lval_del(x);
        x = lval_err("Division By Zero!");
      } else {
        x->num = (long) x->num % (long) y->num;
      }
    }
    if (strcmp(op, "^") == 0) {
      x->num = pow(x->num, y->num);
    }
    if (strcmp(op, "min") == 0) {
      x->num = x->num < y-> num ? x->num : y->num;
    }
    if (strcmp(op, "max") == 0) {
      x->num = x->num > y-> num ? x->num : y->num;
    }
    lval_del(y);
  }

  lval_del(a);
  return x;
}

lval* builtin(lval* a, char* func) {
  if (strcmp("list", func) == 0) return builtin_list(a);
  if (strcmp("head", func) == 0) return builtin_head(a);
  if (strcmp("tail", func) == 0) return builtin_tail(a);
  if (strcmp("join", func) == 0) return builtin_join(a);
  if (strcmp("eval", func) == 0) return builtin_eval(a);
  if (strcmp("cons", func) == 0) return builtin_cons(a);
  if (strcmp("init", func) == 0) return builtin_init(a);
  if (strcmp("len", func) == 0) return builtin_len(a);
  if (strcmp("min", func) == 0)  return builtin_op(a, func);
  if (strcmp("max", func) == 0)  return builtin_op(a, func);
  if (strstr("+-/*%^", func)) return builtin_op(a, func);
  lval_del(a);
  return lval_err(func);
}


lval* lval_read_num(mpc_ast_t* t) {
  errno = 0;
  double x = strtod(t->contents, NULL);
  return errno != ERANGE ?
    lval_num(x) : lval_err("invalid number");
}

lval* lval_read(mpc_ast_t* t) {
  // printf("debug : %s\n", t->tag);

  if (strstr(t->tag, "number")) {
    return lval_read_num(t);
  }
  if (strstr(t->tag, "symbol")) {
    return lval_sym(t->contents);
  }
  /* If root (>) or sexpr then create empty list */
  lval* x = NULL;
  if (strcmp(t->tag, ">") == 0) {
    x = lval_sexpr();
  } 
  if (strstr(t->tag, "sexpr"))  {
    x = lval_sexpr();
  }
  if (strstr(t->tag, "qexpr"))  {
    x = lval_qexpr();
  }
  
  /* Fill this list with any valid expression contained within */
  for (int i = 0; i < t->children_num; i++) {
    if (strcmp(t->children[i]->contents, "(") == 0) continue;
    if (strcmp(t->children[i]->contents, ")") == 0) continue;
    if (strcmp(t->children[i]->contents, "{") == 0) continue;
    if (strcmp(t->children[i]->contents, "}") == 0) continue;
    if (strcmp(t->children[i]->tag,  "regex") == 0) continue;
    x = lval_add(x, lval_read(t->children[i]));
  }
  
  return x;
}

int main(int argc, char** argv) {
  mpc_parser_t* Int = mpc_new("int");
  mpc_parser_t* Double = mpc_new("double");
  mpc_parser_t* Number = mpc_new("number");
  mpc_parser_t* Symbol = mpc_new("symbol");
  mpc_parser_t* Sexpr  = mpc_new("sexpr");
  mpc_parser_t* Qexpr  = mpc_new("qexpr");
  mpc_parser_t* Expr   = mpc_new("expr");
  mpc_parser_t* Lispy  = mpc_new("lispy");
  
  mpca_lang(MPCA_LANG_DEFAULT,
    "                                                      \
      int    : /-?[0-9]+/ ;                                \
      double  : /-?[0-9]*\\.[0-9]+/ ;                      \
      number : <double> | <int> ;                          \
      symbol : \"list\" | \"head\" | \"tail\" | \"join\" | \
               \"eval\" | \"cons\" | \"len\"  | \"init\" | \
               '+' | '-' | '*' | '/' | '%' | '^' |         \
               \"min\" | \"max\" ;                         \
      sexpr  : '(' <expr>* ')' ;                           \
      qexpr  : '{' <expr>* '}' ;                           \
      expr   : <number> | <symbol> | <sexpr> | <qexpr> ;   \
      lispy  : /^/ <expr>* /$/ ;                           \
    ",
    Int, Double, Number, Symbol, Sexpr, Qexpr, Expr, Lispy);
  
  puts("Lispy Version 0.0.0.0.6");
  puts("Press Ctrl+c to Exit\n");
  
  while (1) {
    char* input = readline("$ ");
    add_history(input);
    
    mpc_result_t r;
    if (mpc_parse("<stdin>", input, Lispy, &r)) {
      lval* x = lval_eval(lval_read(r.output));
      lval_println(x);
      lval_del(x);
      mpc_ast_delete(r.output);
    } else {    
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }
    
    free(input);
  }
  
  mpc_cleanup(8, Int, Double, Number, Symbol, Sexpr, Qexpr,  Expr, Lispy);
  
  return 0;
}
