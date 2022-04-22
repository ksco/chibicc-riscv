#include "chibicc.h"

static FILE *output_file;
static int depth;
static char *argreg[] = {"a0", "a1", "a2", "a3", "a4", "a5"};
static Obj *current_fn;

static void gen_expr(Node *node);
static void gen_stmt(Node *node);

static void println(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_file, fmt, ap);
  va_end(ap);
  fprintf(output_file, "\n");
}

static int count(void) {
  static int i = 1;
  return i++;
}

static void push(void) {
  println("  addi sp,sp,-8");
  println("  sd a0,0(sp)");
  depth++;
}

static void pop(char *arg) {
  println("  ld %s,0(sp)", arg);
  println("  addi sp,sp,8");
  depth--;
}

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
static void gen_addr(Node *node) {
  switch (node->kind) {
  case ND_VAR:
    if (node->var->is_local) {
      // Local variable
      println("  addi a0,s0,%d", node->var->offset - node->var->ty->size);
    } else {
      // Global variable
      println("  lui a0,%%hi(%s)", node->var->name);
      println("  addi a0,a0,%%lo(%s)", node->var->name);
    }

    return;
  case ND_DEREF:
    gen_expr(node->lhs);
    return;
  case ND_COMMA:
    gen_expr(node->lhs);
    gen_addr(node->rhs);
    return;
  case ND_MEMBER:
    gen_addr(node->lhs);
    println("  addi a0,a0,%d", node->member->offset);
    return;
  }

  error_tok(node->tok, "not an lvalue");
}

// Load a value from where a0 is pointing to.
static void load(Type *ty) {
  if (ty->kind == TY_ARRAY || ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    // If it is an array, do not attempt to load a value to the
    // register because in general we can't load an entire array to a
    // register. As a result, the result of an evaluation of an array
    // becomes not the array itself but the address of the array.
    // This is where "array is automatically converted to a pointer to
    // the first element of the array in C" occurs.
    return;
  }

  // When we load a char or a short value to a register, we always
  // extend them to the size of int, so we can assume the lower half of
  // a register always contains a valid value. The upper half of a
  // register for char, short and int may contain garbage. When we load
  // a long value to a register, it simply occupies the entire register.
  if (ty->size == 1)
    println("  lb a0,0(a0)");
  else if (ty->size == 2)
    println("  lh a0,0(a0)");
  else if (ty->size == 4)
    println("  lw a0,0(a0)");
  else
    println("  ld a0,0(a0)");
}

// Store a0 to an address that the stack top is pointing to.
static void store(Type *ty) {
  pop("a1");

  if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    for (int i = 0; i < ty->size; i++) {
      println("  lb a4,%d(a0)", i);
      println("  sb a4,%d(a1)", i);
    }
    return;
  }

  if (ty->size == 1)
    println("  sb a0,0(a1)");
  else if (ty->size == 2)
    println("  sh a0,0(a1)");
  else if (ty->size == 4)
    println("  sw a0,0(a1)");
  else
    println("  sd a0,0(a1)");
}

enum { I8, I16, I32, I64 };

static int getTypeId(Type *ty) {
  switch (ty->kind) {
  case TY_CHAR:
    return I8;
  case TY_SHORT:
    return I16;
  case TY_INT:
    return I32;
  }
  return I64;
}

// The table for type casts
static char i32i8[] = "  andi a0,a0,0xff";
static char i32i16[] = "  slliw a0,a0,16\n  sraiw a0,a0,16";
static char i32i64[] = "  sext.w a0,a0";

static char *cast_table[][10] = {
  {NULL,  NULL,   NULL, i32i64}, // i8
  {i32i8, NULL,   NULL, i32i64}, // i16
  {i32i8, i32i16, NULL, i32i64}, // i32
  {i32i8, i32i16, NULL, NULL},   // i64
};

static void cast(Type *from, Type *to) {
  if (to->kind == TY_VOID)
    return;

  if (to->kind == TY_BOOL) {
    println("  snez a0,a0");
    return;
  }

  int t1 = getTypeId(from);
  int t2 = getTypeId(to);
  if (cast_table[t1][t2])
    println(cast_table[t1][t2]);
}

// Generate code for a given node.
static void gen_expr(Node *node) {
  println("  .loc 1 %d", node->tok->line_no);

  switch (node->kind) {
  case ND_NULL_EXPR:
    return;
  case ND_NUM:
    println("  li a0,%ld", node->val);
    return;
  case ND_NEG:
    gen_expr(node->lhs);
    println("  neg a0,a0");
    return;
  case ND_VAR:
  case ND_MEMBER:
    gen_addr(node);
    load(node->ty);
    return;
  case ND_DEREF:
    gen_expr(node->lhs);
    load(node->ty);
    return;
  case ND_ADDR:
    gen_addr(node->lhs);
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    push();
    gen_expr(node->rhs);
    store(node->ty);
    return;
  case ND_STMT_EXPR:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    return;
  case ND_COMMA:
    gen_expr(node->lhs);
    gen_expr(node->rhs);
    return;
  case ND_CAST:
    gen_expr(node->lhs);
    cast(node->lhs->ty, node->ty);
    return;
  case ND_MEMZERO: {
    int offset = node->var->offset;
    for (int i = 0; i < node->var->ty->size; i++) {
      offset -= sizeof(char);
      println("  sb zero,%d(s0)", offset);
    }
    return;
  }
  case ND_COND: {
    int c = count();
    gen_expr(node->cond);
    println("  beqz a0,.L.else.%d", c);
    gen_expr(node->then);
    println("  j .L.end.%d", c);
    println(".L.else.%d:", c);
    gen_expr(node->els);
    println(".L.end.%d:", c);
    return;
  }
  case ND_NOT:
    gen_expr(node->lhs);
    println("  seqz a0,a0");
    return;
  case ND_BITNOT:
    gen_expr(node->lhs);
    println("  not a0,a0");
    return;
  case ND_LOGAND: {
    int c = count();
    gen_expr(node->lhs);
    println("  beqz a0,.L.false.%d", c);
    gen_expr(node->rhs);
    println("  beqz a0,.L.false.%d", c);
    println("  li a0,1");
    println("  j .L.end.%d", c);
    println(".L.false.%d:", c);
    println("  li a0,0");
    println(".L.end.%d:", c);
    return;
  }
  case ND_LOGOR: {
    int c = count();
    gen_expr(node->lhs);
    println("  bne a0,zero,.L.true.%d", c);
    gen_expr(node->rhs);
    println("  bne a0,zero,.L.true.%d", c);
    println("  li a0,0");
    println("  j .L.end.%d", c);
    println(".L.true.%d:", c);
    println("  li a0,1");
    println(".L.end.%d:", c);
    return;
  }
  case ND_FUNCALL: {
    int nargs = 0;
    for (Node *arg = node->args; arg; arg = arg->next) {
      gen_expr(arg);
      push();
      nargs++;
    }

    for (int i = nargs - 1; i >= 0; i--)
      pop(argreg[i]);

    println("  call %s", node->funcname);
    return;
  }
  }

  gen_expr(node->rhs);
  push();
  gen_expr(node->lhs);
  pop("a1");

  char* suffix = node->lhs->ty->kind == TY_LONG || node->lhs->ty->base
               ? "" : "w";
  switch (node->kind) {
  case ND_ADD:
    println("  add%s a0,a0,a1", suffix);
    return;
  case ND_SUB:
    println("  sub%s a0,a0,a1", suffix);
    return;
  case ND_MUL:
    println("  mul%s a0,a0,a1", suffix);
    return;
  case ND_DIV:
    println("  div%s a0,a0,a1", suffix);
    return;
  case ND_MOD:
    println("  rem%s a0,a0,a1", suffix);
    return;
  case ND_BITAND:
    println("  and a0,a0,a1");
    return;
  case ND_BITOR:
    println("  or a0,a0,a1");
    return;
  case ND_BITXOR:
    println("  xor a0,a0,a1");
    return;
  case ND_EQ:
    println("  sub a0,a0,a1");
    println("  seqz a0,a0");
    return;
  case ND_NE:
    println("  sub a0,a0,a1");
    println("  snez a0,a0");
    return;
  case ND_LT:
    println("  slt a0,a0,a1");
    return;
  case ND_LE:
    println("  sgt a0,a0,a1");
    println("  xori a0,a0,1");
    return;
  case ND_SHL:
    println("  sll%s a0,a0,a1", suffix);
    return;
  case ND_SHR:
    println("  sra%s a0,a0,a1", suffix);
    return;
  }

  error_tok(node->tok, "invalid expression");
}

static void gen_stmt(Node *node) {
  println("  .loc 1 %d", node->tok->line_no);
  switch (node->kind) {
  case ND_IF: {
    int c = count();
    gen_expr(node->cond);
    println("  beqz a0,.L.else.%d", c);
    gen_stmt(node->then);
    println("  j .L.end.%d", c);
    println(".L.else.%d:", c);
    if (node->els)
      gen_stmt(node->els);
    println(".L.end.%d:", c);
    return;
  }
  case ND_FOR: {
    int c = count();
    if (node->init)
      gen_stmt(node->init);
    println(".L.begin.%d:", c);
    if (node->cond) {
      gen_expr(node->cond);
      println("  beqz a0,%s", node->brk_label);
    }
    gen_stmt(node->then);
    println("%s:", node->cont_label);
    if (node->inc)
      gen_expr(node->inc);
    println("  j .L.begin.%d", c);
    println("%s:", node->brk_label);
    return;
  }
  case ND_SWITCH:
    gen_expr(node->cond);

    for (Node *n = node->case_next; n; n = n->case_next) {
      println("  li a4,%ld", n->val);
      println("  beq a0,a4,%s", n->label);
    }

    if (node->default_case)
      println("  j %s", node->default_case->label);

    println("  j %s", node->brk_label);
    gen_stmt(node->then);
    println("%s:", node->brk_label);
    return;
  case ND_CASE:
    println("%s:", node->label);
    gen_stmt(node->lhs);
    return;
  case ND_BLOCK:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    return;
  case ND_GOTO:
    println("  j %s", node->unique_label);
    return;
  case ND_LABEL:
    println("%s:", node->unique_label);
    gen_stmt(node->lhs);
    return;
  case ND_RETURN:
    gen_expr(node->lhs);
    println("  j .L.return.%s", current_fn->name);
    return;
  case ND_EXPR_STMT:
    gen_expr(node->lhs);
    return;
  }

  error_tok(node->tok, "invalid statement");
}

// Assign offsets to local variables.
static void assign_lvar_offsets(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function)
      continue;

    int offset = 0;
    for (Obj *var = fn->locals; var; var = var->next) {
      offset = align_to(offset, var->ty->align);
      var->offset = -offset;
      offset += var->ty->size;
    }
    fn->stack_size = align_to(offset, 16);
  }
}

static void emit_data(Obj *prog) {
  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function || !var->is_definition)
      continue;

    println("  .globl %s", var->name);
    println("  .align %d", var->ty->align);

    if (var->init_data) {
      println("  .data");
      println("%s:", var->name);

      Relocation *rel = var->rel;
      int pos = 0;
      while (pos < var->ty->size) {
        if (rel && rel->offset == pos) {
          println("  .quad %s%+ld", rel->label, rel->addend);
          rel = rel->next;
          pos += 8;
        } else {
          println("  .byte %d", var->init_data[pos++]);
        }
      }
      continue;
    }

    println("  .bss");
    println("%s:", var->name);
    println("  .zero %d", var->ty->size);
  }
}

static void store_gp(int r, int offset, int sz) {
  switch (sz) {
  case 1:
    println("  sb %s,%d(s0)", argreg[r], offset - sz);
    return;
  case 2:
    println("  sh %s,%d(s0)", argreg[r], offset - sz);
    return;
  case 4:
    println("  sw %s,%d(s0)", argreg[r], offset - sz);
    return;
  case 8:
    println("  sd %s,%d(s0)", argreg[r], offset - sz);
    return;
  }
  unreachable();
}

static void emit_text(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;

    if (fn->is_static)
      println("  .local %s", fn->name);
    else
      println("  .globl %s", fn->name);

    println("  .text");
    println("%s:", fn->name);
    current_fn = fn;

    // Prologue
    println("  addi sp,sp,-%d", fn->stack_size + 16);
    println("  sd ra,%d(sp)", fn->stack_size + 8);
    println("  sd s0,%d(sp)", fn->stack_size);
    println("  addi s0,sp,%d", fn->stack_size);

    // Save passed-by-register arguments to the stack
    int i = 0;
    for (Obj *var = fn->params; var; var = var->next)
      store_gp(i++, var->offset, var->ty->size);

    // Emit code
    gen_stmt(fn->body);
    assert(depth == 0);

    // Epilogue
    println(".L.return.%s:", fn->name);
    println("  ld ra,%d(sp)", fn->stack_size + 8);
    println("  ld s0,%d(sp)", fn->stack_size);
    println("  addi sp,sp,%d", fn->stack_size + 16);
    println("  ret");
  }
}

void codegen(Obj *prog, FILE *out) {
  output_file = out;

  assign_lvar_offsets(prog);
  emit_data(prog);
  emit_text(prog);
}
