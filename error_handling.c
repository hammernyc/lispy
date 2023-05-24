/**
 * compile with: cc -std=c99 -Wall error-handling.c mpc.c -ledit -o error_handling
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mpc.h"

/* If we are compiling on Windows compile these functions */
#ifdef _WIN32
#include <string.h>

static char buffer[2048];

/* Fake readline function */
char* readline(char* prompt) {
  fputs(prompt, stdout);
  fgets(buffer, 2048, stdin);
  char* cpy = malloc(strlen(buffer)+1);
  strcpy(cpy, buffer);
  cpy[strlen(cpy)-1] = '\0';
  return cpy;
}

/* Fake add_history function */
void add_history(char* unused) {}

/* Otherwise include the editline headers */
#else
#include <editline/readline.h>
#endif

enum { LVAL_NUM, LVAL_ERR };
enum { LERR_DIV_ZERO, LERR_BAD_OP, LERR_BAD_NUM };

typedef struct {
  int type;
  double num;
  int err;
} lval;

lval lval_num(double x) {
  lval v;
  v.type = LVAL_NUM;
  v.num = x;
  return v;
}

lval lval_err(double x) {
  lval v;
  v.type = LVAL_ERR;
  v.num = x;
  return v;
}

void lval_print(lval v) {
  switch (v.type) {
    case LVAL_NUM:
      printf("%g", v.num);
      break;
    case LVAL_ERR:
      switch (v.err) {
        case LERR_DIV_ZERO:
          printf("Error: Division by zero!");
          break;
        case LERR_BAD_OP:
          printf("Error: Invalid operator!");
          break;
        case LERR_BAD_NUM:
          printf("Error: Invalid number");
          break;
      }
      break;
  }
}

void lval_println(lval v) { lval_print(v); putchar('\n'); }

/* use operator string to se which operation to perform */
lval eval_op(lval x, char* op, lval y) {
  if (x.type == LVAL_ERR) { return x; }
  if (y.type == LVAL_ERR) { return y; }

  if (strcmp(op, "+") == 0) {
    return lval_num(x.num + y.num);
  }
  if (strcmp(op, "-") == 0) {
    return lval_num(x.num - y.num);
  }
  if (strcmp(op, "*") == 0) {
    return lval_num(x.num * y.num);
  }
  if (strcmp(op, "/") == 0) {
    return y.num == 0 ?
      lval_err(LERR_DIV_ZERO) :
      lval_num(x.num / y.num);
  }
  if (strcmp(op, "%") == 0)   {
    return y.num == 0 ?
      lval_err(LERR_DIV_ZERO) :
      lval_num((int) x.num % (int) y.num);
  }
  if (strcmp(op, "^") == 0)   {
    return lval_num(pow(x.num, y.num));
  }
  if (strcmp(op, "min") == 0) {
    return lval_num(fminl(x.num, y.num));
  }
  if (strcmp(op, "max") == 0) {
    return lval_num(fmaxl(x.num, y.num));
  }

  return lval_err(LERR_BAD_OP);
}

lval eval(mpc_ast_t* t) {
    if (strstr(t->tag, "number")) {
      errno = 0;
      double x = strtol(t->contents, NULL, 10);
      return errno != ERANGE ? lval_num(x) : lval_err(LERR_BAD_NUM);
    }

    /* the operator is always second child, after '('*/
    char *op = t->children[1]->contents;

    lval x = eval(t->children[2]);


    /* special case: '-' works as unary operator */
    if (strcmp(op, "-") == 0 && t->children_num == 4) {
      x = eval_op(lval_num(0), op, x);
    }  
  
    int i = 3;
    while (strstr(t->children[i]->tag, "expr")) {
        x = eval_op(x, op, eval(t->children[i++]));
    }

  return x;
}

int main(int argc, char** argv) {
    /* Create Some Parsers */
    mpc_parser_t* Number   = mpc_new("number");
    mpc_parser_t* Operator = mpc_new("operator");
    mpc_parser_t* Expr     = mpc_new("expr");
    mpc_parser_t* Lispy    = mpc_new("lispy");

    /* Define them with the following Language */
    mpca_lang(MPCA_LANG_DEFAULT,
      "                                                           \
        number   : /-?[0-9]+/ | /-?[0-9]*.[0-9]+/ ;               \
        operator : '+' |'-' | '*' | '/' | '%' | '^' |             \
                    \"min\" | \"max\" ;                           \
        expr     : <number> | '(' <operator> <expr>+ ')' ;        \
        lispy    : /^/ <operator> <expr>+ /$/ ;                   \
      ",
      Number, Operator, Expr, Lispy);


    /* Print Version and Exit Information */
    puts("Lispy Version 0.0.0.0.1");
    puts("Press Ctrl+c to Exit\n");

    while (1) {
        /* Output prompt and get input */
        char* input = readline("$ ");

        /* Add input to history */
        add_history(input);

        /* Attempt to Parse the user Input */
        mpc_result_t r;
        if (mpc_parse("<stdin>", input, Lispy, &r)) {
          lval_println(eval(r.output));
          mpc_ast_delete(r.output);
        } else {
          mpc_err_print(r.error);
          mpc_err_delete(r.error);
        }

        free(input);
    }

    /* Undefine and Delete our Parsers */
    mpc_cleanup(4, Number, Operator, Expr, Lispy);
    return 0;
}
