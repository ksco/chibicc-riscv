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

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
static void gen_addr(Node *node) {
  if (node->kind == ND_VAR) {
    int offset = (node->name - 'a' + 1) * 8;
    printf("  addi a0,s0,%d\n", -offset);
    return;
  }

  error("not an lvalue");
}

// Generate code for a given node.
static void gen_expr(Node *node) {
  switch (node->kind) {
  case ND_NUM:
    printf("  li a0,%d\n", node->val);
    return;
  case ND_NEG:
    gen_expr(node->lhs);
    printf("  neg a0,a0\n");
    return;
  case ND_VAR:
    gen_addr(node);
    printf("  ld a0,0(a0)\n");
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    push();
    gen_expr(node->rhs);
    pop("a1");
    printf("  sd a0,0(a1)\n");
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

static void gen_stmt(Node *node) {
  if (node->kind == ND_EXPR_STMT) {
    gen_expr(node->lhs);
    return;
  }

  error("invalid statement");
}

void codegen(Node *node) {
  printf("  .globl main\n");
  printf("main:\n");

  // Prologue
  printf("  addi sp,sp,-224\n");
  printf("  sd s0,216(sp)\n");
  printf("  addi s0,sp,208\n");

  for (Node *n = node; n; n = n->next) {
    gen_stmt(n);
    assert(depth == 0);
  }

  printf("  ld s0,216(sp)\n");
  printf("  addi sp,sp,224\n");
  printf("  ret\n");
}