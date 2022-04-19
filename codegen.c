#include "chibicc.h"

static int depth;

static void push(void) {
  printf("  sd a0,0(sp)\n");
  printf("  addi sp,sp,-8\n");
  depth++;
}

static void pop(char *arg) {
  printf("  addi sp,sp,8\n");
  printf("  ld %s,0(sp)\n", arg);
  depth--;
}

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
static int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
static void gen_addr(Node *node) {
  if (node->kind == ND_VAR) {
    printf("  addi a0,s0,%d\n", node->var->offset);
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
  switch (node->kind) {
  case ND_RETURN:
    gen_expr(node->lhs);
    printf("  j .L.return\n");
    return;
  case ND_EXPR_STMT:
    gen_expr(node->lhs);
    return;
  }

  error("invalid statement");
}

// Assign offsets to local variables.
static void assign_lvar_offsets(Function *prog) {
  int offset = 0;
  for (Obj *var = prog->locals; var; var = var->next) {
    var->offset = -offset;
    offset += 8;
  }
  prog->stack_size = align_to(offset, 16);
}

void codegen(Function *prog) {
  assign_lvar_offsets(prog);

  printf("  .globl main\n");
  printf("main:\n");

  // Prologue
  printf("  addi sp,sp,-%d\n", prog->stack_size + 16);
  printf("  sd s0,%d(sp)\n", prog->stack_size + 8);
  printf("  addi s0,sp,%d\n", prog->stack_size);

  for (Node *n = prog->body; n; n = n->next) {
    gen_stmt(n);
    assert(depth == 0);
  }

  printf(".L.return:\n");
  printf("  ld s0,%d(sp)\n", prog->stack_size + 8);
  printf("  addi sp,sp,%d\n", prog->stack_size + 16);
  printf("  ret\n");
}