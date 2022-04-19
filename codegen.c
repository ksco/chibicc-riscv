#include "chibicc.h"

static int depth;

static void push(void) {
  printf("  sd a0,-%d(sp)\n", depth * 8);
  depth++;
}

static void pop(char *arg) {
  depth--;
  printf("  ld %s,-%d(sp)\n", arg, depth * 8);
}

static void gen_expr(Node *node) {
  switch (node->kind) {
  case ND_NUM:
    printf("  li a0,%d\n", node->val);
    return;
  case ND_NEG:
    gen_expr(node->lhs);
    printf("  neg a0,a0\n");
    return;
  }

  gen_expr(node->rhs);
  push();
  gen_expr(node->lhs);
  pop("a1");

  switch (node->kind) {
  case ND_ADD:
    printf("  add a0,a0,a1\n");
    return;
  case ND_SUB:
    printf("  sub a0,a0,a1\n");
    return;
  case ND_MUL:
    printf("  mul a0,a0,a1\n");
    return;
  case ND_DIV:
    printf("  div a0,a0,a1\n");
    return;
  case ND_EQ:
    printf("  sub a0,a0,a1\n");
    printf("  seqz a0,a0\n");
    return;
  case ND_NE:
    printf("  sub a0,a0,a1\n");
    printf("  snez a0,a0\n");
    return;
  case ND_LT:
    printf("  slt a0,a0,a1\n");
    return;
  case ND_LE:
    printf("  sgt a0,a0,a1\n");
    printf("  xori a0,a0,1\n");
    return;
  }

  error("invalid expression");
}

void codegen(Node *node) {
  printf("  .globl main\n");
  printf("main:\n");

  gen_expr(node);
  printf("  ret\n");

  assert(depth == 0);
}