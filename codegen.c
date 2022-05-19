#include "chibicc.h"

#define GP_MAX 8
#define FP_MAX 8

static FILE *output_file;
static int depth;
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

static void pop(int reg) {
  println("  ld a%d,0(sp)", reg);
  println("  addi sp,sp,8");
  depth--;
}

static void pushf(void) {
  println("  addi sp,sp,-8");
  println("  fsd fa0,0(sp)");
  depth++;
}

static void popf(int reg) {
  println("  fld fa%d,0(sp)", reg);
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
    // Local variable
    if (node->var->is_local) {
      println("  li t1,%d", node->var->offset - node->var->ty->size);
      println("  add a0,s0,t1");
      return;
    }

    // Function
    if (node->ty->kind == TY_FUNC) {
      int c = count();
      println(".L.b1_%d:", c);
      println("  auipc a0,%%pcrel_hi(%s)", node->var->name);
      println("  addi a0,a0,%%pcrel_lo(.L.b1_%d)", c);
      return;
    }

    // Global variable
    println("  lui a0,%%hi(%s)", node->var->name);
    println("  addi a0,a0,%%lo(%s)", node->var->name);
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
  default: break;
  }

  error_tok(node->tok, "not an lvalue");
}

// Load a value from where a0 is pointing to.
static void load(Type *ty) {
  switch (ty->kind) {
  case TY_ARRAY:
  case TY_STRUCT:
  case TY_UNION:
  case TY_FUNC:
    // If it is an array, do not attempt to load a value to the
    // register because in general we can't load an entire array to a
    // register. As a result, the result of an evaluation of an array
    // becomes not the array itself but the address of the array.
    // This is where "array is automatically converted to a pointer to
    // the first element of the array in C" occurs.
    return;
  case TY_FLOAT:
    println("  flw fa0,0(a0)");
    return;
  case TY_DOUBLE:
    println("  fld fa0,0(a0)");
    return;
  default: break;
  }

  char *suffix = ty->is_unsigned ? "u" : "";

  // When we load a char or a short value to a register, we always
  // extend them to the size of int, so we can assume the lower half of
  // a register always contains a valid value. The upper half of a
  // register for char, short and int may contain garbage. When we load
  // a long value to a register, it simply occupies the entire register.
  if (ty->size == 1)
    println("  lb%s a0,0(a0)", suffix);
  else if (ty->size == 2)
    println("  lh%s a0,0(a0)", suffix);
  else if (ty->size == 4)
    println("  lw a0,0(a0)", suffix);
  else
    println("  ld a0,0(a0)");
}

// Store a0 to an address that the stack top is pointing to.
static void store(Type *ty) {
  pop(1);

  switch (ty->kind) {
  case TY_STRUCT:
  case TY_UNION:
    for (int i = 0; i < ty->size; i++) {
      println("  lb a4,%d(a0)", i);
      println("  sb a4,%d(a1)", i);
    }
    return;
  case TY_FLOAT:
    println("  fsw fa0,0(a1)");
    return;
  case TY_DOUBLE:
    println("  fsd fa0,0(a1)");
    return;
  default: break;
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

static void cmp_zero(Type *ty) {
    switch (ty->kind) {
    case TY_FLOAT:
      println("  fmv.w.x fa1,zero");
      println("  feq.s a0,fa0,fa1");
      return;
    case TY_DOUBLE:
      println("  fmv.d.x fa1,zero");
      println("  feq.d a0,fa0,fa1");
      return;
    default: break;
    }

    println("  seqz a0,a0");
}

enum { I8, I16, I32, I64, U8, U16, U32, U64, F32, F64 };

static int getTypeId(Type *ty) {
  switch (ty->kind) {
  case TY_CHAR:
    return ty->is_unsigned ? U8 : I8;
  case TY_SHORT:
    return ty->is_unsigned ? U16 : I16;
  case TY_INT:
    return ty->is_unsigned ? U32 : I32;
  case TY_LONG:
    return ty->is_unsigned ? U64 : I64;
  case TY_FLOAT:
    return F32;
  case TY_DOUBLE:
    return F64;
  default:
    return U64;;
  }
}

// The table for type casts
static char i32i8[] = "  slliw a0,a0,24\n  sraiw a0,a0,24";
static char i32u8[] = "  andi a0,a0,0xff";
static char i32i16[] = "  slliw a0,a0,16\n  sraiw a0,a0,16";
static char i32u16[] = "  slli a0,a0,48\n  srli a0,a0,48";

static char u64i32[] = "  slli a0,a0,32\n  srli a0,a0,32";

static char i32f32[] = "  fcvt.s.w fa0,a0";
static char i32f64[] = "  fcvt.d.w fa0,a0";

static char u32f32[] = "  fcvt.s.wu fa0,a0";
static char u32f64[] = "  fcvt.d.wu fa0,a0";

static char i64f32[] = "  fcvt.s.l fa0,a0";
static char i64f64[] = "  fcvt.d.l fa0,a0";

static char u64f32[] = "  fcvt.s.lu fa0,a0";
static char u64f64[] = "  fcvt.d.lu fa0,a0";

static char f32i8[] = "  fcvt.w.s a0,fa0,rtz\n  andi a0,a0,0xff";
static char f32u8[] = "  fcvt.wu.s a0,fa0,rtz\n  andi a0,a0,0xff";
static char f32i16[] =
  "  fcvt.w.s a0,fa0,rtz\n"
  "  slliw a0,a0,16\n"
  "  sraiw a0,a0,16\n";
static char f32u16[] =
  "  fcvt.wu.s a0,fa0,rtz\n"
  "  slli a0,a0,48\n"
  "  srli a0,a0,48\n";
static char f32i32[] = "  fcvt.w.s a0,fa0,rtz";
static char f32u32[] = "  fcvt.wu.s a0,fa0,rtz";
static char f32i64[] = "  fcvt.l.s a0,fa0,rtz";
static char f32u64[] = "  fcvt.lu.s a0,fa0,rtz";
static char f32f64[] = "  fcvt.d.s fa0,fa0";

static char f64i8[] = "  fcvt.w.d a0,fa0,rtz\n  andi a0,a0,0xff";
static char f64u8[] = "  fcvt.wu.d a0,fa0,rtz\n  andi a0,a0,0xff";
static char f64i16[] =
  "  fcvt.w.d a0,fa0,rtz\n"
  "  slliw a0,a0,16\n"
  "  sraiw a0,a0,16\n";
static char f64u16[] =
  "  fcvt.wu.d a0,fa0,rtz\n"
  "  slli a0,a0,48\n"
  "  srli a0,a0,48\n";
static char f64i32[] = "  fcvt.w.d a0,fa0,rtz";
static char f64u32[] = "  fcvt.wu.d a0,fa0,rtz";
static char f64f32[] = "  fcvt.s.d fa0,fa0";
static char f64i64[] = "  fcvt.l.d a0,fa0,rtz";
static char f64u64[] = "  fcvt.lu.d a0,fa0,rtz";

static char *cast_table[][10] = {
  // i8   i16     i32     i64     u8     u16     u32     u64     f32     f64
  {NULL,  NULL,   NULL,   NULL, i32u8, i32u16, NULL,   NULL, i32f32, i32f64}, // i8
  {i32i8, NULL,   NULL,   NULL, i32u8, i32u16, NULL,   NULL, i32f32, i32f64}, // i16
  {i32i8, i32i16, NULL,   NULL, i32u8, i32u16, NULL,   NULL, i32f32, i32f64}, // i32
  {i32i8, i32i16, u64i32, NULL, i32u8, i32u16, u64i32, NULL, i64f32, i64f64}, // i64

  {i32i8, NULL,   NULL,   NULL, NULL,  NULL,   NULL,   NULL, i32f32, i32f64}, // u8
  {i32i8, i32i16, NULL,   NULL, i32u8, NULL,   NULL,   NULL, i32f32, i32f64}, // u16
  {i32i8, i32i16, NULL,   NULL, i32u8, i32u16, NULL,   NULL, u32f32, u32f64}, // u32
  {i32i8, i32i16, u64i32, NULL, i32u8, i32u16, u64i32, NULL, u64f32, u64f64}, // u64

  {f32i8, f32i16, f32i32, f32i64, f32u8, f32u16, f32u32, f32u64, NULL,   f32f64}, // f32
  {f64i8, f64i16, f64i32, f64i64, f64u8, f64u16, f64u32, f64u64, f64f32, NULL},   // f64
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

static bool has_flonum(Type *ty, int lo, int hi, int offset) {
  if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    for (Member *mem = ty->members; mem; mem = mem->next)
      if (!has_flonum(mem->ty, lo, hi, offset + mem->offset))
        return false;
    return true;
  }

  if (ty->kind == TY_ARRAY) {
    for (int i = 0; i < ty->array_len; i++)
      if (!has_flonum(ty->base, lo, hi, offset + ty->base->size * i))
        return false;
    return true;
  }

  return offset < lo || hi <= offset || is_flonum(ty);
}

static bool has_flonum1(Type *ty) {
  return has_flonum(ty, 0, 8, 0);
}

static bool has_flonum2(Type *ty) {
  return has_flonum(ty, 8, 16, 0);
}

static void push_struct(Type *ty) {
  int sz = align_to(ty->size, 8);
  println("  addi sp,sp,%d", -sz);
  depth += sz / 8;

  for (int i = 0; i < ty->size; i++) {
    println("  lb t3,%d(a0)", i);
    println("  sb t3,%d(sp)", i);
  }
}

static void push_args2(Node *args, bool first_pass) {
  if (!args)
    return;

  push_args2(args->next, first_pass);

  if ((first_pass && !args->pass_by_stack) || (!first_pass && args->pass_by_stack))
    return;

  gen_expr(args);
  switch (args->ty->kind) {
  case TY_STRUCT:
  case TY_UNION:
    push_struct(args->ty);
    break;
  case TY_FLOAT:
  case TY_DOUBLE:
    pushf();
    break;
  default:
    push();
  }
}

static int push_args(Node *args) {
  int stack = 0, gp = 0, fp = 0;

  for (Node *arg = args; arg; arg = arg->next) {
    Type *ty = arg->ty;

    switch (ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      if (ty->size > 16) {
        arg->pass_by_stack = true;
        stack += align_to(ty->size, 8) / 8;
      } else {
        bool fp1 = has_flonum1(ty);
        bool fp2 = has_flonum2(ty);

        if (fp + fp1 + fp2 < FP_MAX && gp + !fp1 + !fp2 < GP_MAX) {
          fp = fp + fp1 + fp2;
          gp = gp + !fp1 + !fp2;
        } else {
          arg->pass_by_stack = true;
          stack += align_to(ty->size, 8) / 8;
        }
      }
      break;
    case TY_FLOAT:
    case TY_DOUBLE:
      if (fp >= FP_MAX && gp > GP_MAX) {
        arg->pass_by_stack = true;
        stack++;
      } else if (fp < FP_MAX) {
        fp++;
      } else {
        gp++;
      }
      break;
    default:
      if (gp++ >= GP_MAX) {
        arg->pass_by_stack = true;
        stack++;
      }
    }
  }

  push_args2(args, true);
  push_args2(args, false);

  return stack;
}

// Generate code for a given node.
static void gen_expr(Node *node) {
  println("  .loc %d %d", node->tok->file->file_no, node->tok->line_no);

  switch (node->kind) {
  case ND_NULL_EXPR:
    return;
  case ND_NUM: {
    union { float f32; double f64; uint32_t u32; uint64_t u64; } u;

    switch (node->ty->kind) {
    case TY_FLOAT:
      u.f32 = node->fval;
      println("  li a0,%u  # float %f", u.u32, node->fval);
      println("  fmv.w.x fa0,a0");
      return;
    case TY_DOUBLE:
      u.f64 = node->fval;
      println("  li a0,%lu  # float %f", u.u64, node->fval);
      println("  fmv.d.x fa0,a0");
      return;
    default: break;
    }

    println("  li a0,%ld", node->val);
    return;
  }
  case ND_NEG:
    gen_expr(node->lhs);

    switch (node->ty->kind) {
    case TY_FLOAT:
      println("  fneg.s fa0,fa0");
      return;
    case TY_DOUBLE:
      println("  fneg.d fa0,fa0");
      return;
    default: break;
    }

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
      println("  li t1,%d", offset);
      println("  add t1,t1,s0");
      println("  sb zero,0(t1)", offset);
    }
    return;
  }
  case ND_COND: {
    int c = count();
    gen_expr(node->cond);
    cmp_zero(node->cond->ty);
    println("  bne a0,zero,.L.else.%d", c);
    gen_expr(node->then);
    println("  j .L.end.%d", c);
    println(".L.else.%d:", c);
    gen_expr(node->els);
    println(".L.end.%d:", c);
    return;
  }
  case ND_NOT:
    gen_expr(node->lhs);
    cmp_zero(node->lhs->ty);
    return;
  case ND_BITNOT:
    gen_expr(node->lhs);
    println("  not a0,a0");
    return;
  case ND_LOGAND: {
    int c = count();
    gen_expr(node->lhs);
    cmp_zero(node->lhs->ty);
    println("  bne a0,zero,.L.false.%d", c);
    gen_expr(node->rhs);
    cmp_zero(node->rhs->ty);
    println("  bne a0,zero,.L.false.%d", c);
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
    cmp_zero(node->lhs->ty);
    println("  beqz a0,.L.true.%d", c);
    gen_expr(node->rhs);
    cmp_zero(node->rhs->ty);
    println("  beqz a0,.L.true.%d", c);
    println("  li a0,0");
    println("  j .L.end.%d", c);
    println(".L.true.%d:", c);
    println("  li a0,1");
    println(".L.end.%d:", c);
    return;
  }
  case ND_FUNCALL: {
    int stack_args = push_args(node->args);
    gen_expr(node->lhs);
    println("  mv t2,a0");

    int fp = 0, gp = 0;
    Type* cur_param = node->func_ty->params;
    for (Node *arg = node->args; arg; arg = arg->next) {
      if (node->func_ty->is_variadic && cur_param == NULL) {
        if (gp < GP_MAX) pop(gp++);
        continue;
      }
      cur_param = cur_param->next;
      Type *ty = arg->ty;

      switch (ty->kind) {
      case TY_STRUCT:
      case TY_UNION:
        if (ty->size > 16)
          continue;

        bool fp1 = has_flonum1(ty);
        bool fp2 = has_flonum2(ty);

        if (fp + fp1 + fp2 < FP_MAX && gp + !fp1 + !fp2 < GP_MAX) {
          if (fp1)
            popf(fp++);
          else
            pop(gp++);

          if (ty->size > 8) {
            if (fp2)
              popf(fp++);
            else
              pop(gp++);
          }
        }
        break;
      case TY_FLOAT:
      case TY_DOUBLE:
        if (fp < FP_MAX) {
          popf(fp++);
        } else if (gp < GP_MAX) {
          pop(gp++);
        }
        break;
      default:
        if (gp < GP_MAX) pop(gp++);
      }
    }

    println("  jalr t2");

    if (stack_args) {
      depth -= stack_args;
      println("  addi sp,sp,%d", stack_args * 8);
    }

    // It looks like the most significant 48 or 56 bits in a0 may
    // contain garbage if a function return type is short or bool/char,
    // respectively. We clear the upper bits here.
    switch (node->ty->kind) {
    case TY_BOOL:
      println("  andi a0,a0,0xff");
    case TY_CHAR:
      if (node->ty->is_unsigned) {
        println("  andi a0,a0,0xff");
      } else {
        println("  slliw a0,a0,24");
        println("  sraiw a0,a0,24");
      }
      return;
    case TY_SHORT:
      if (node->ty->is_unsigned) {
        println("  slli a0,a0,48");
        println("  srli a0,a0,48");
      } else {
        println("  slliw a0,a0,16");
        println("  sraiw a0,a0,16");
      }
      return;
    default: break;
    }
    return;
  default: break;
  }
  }

  if (is_flonum(node->lhs->ty)) {
    gen_expr(node->rhs);
    pushf();
    gen_expr(node->lhs);
    popf(1);

    char *suffix = (node->lhs->ty->kind == TY_FLOAT) ? "s" : "d";

    switch (node->kind) {
    case ND_ADD:
      println("  fadd.%s fa0,fa0,fa1", suffix);
      return;
    case ND_SUB:
      println("  fsub.%s fa0,fa0,fa1", suffix);
      return;
    case ND_MUL:
      println("  fmul.%s fa0,fa0,fa1", suffix);
      return;
    case ND_DIV:
      println("  fdiv.%s fa0,fa0,fa1", suffix);
      return;
    case ND_EQ:
      println("  feq.%s a0,fa0,fa1", suffix);
      return;
    case ND_NE:
      println("  feq.%s a0,fa0,fa1", suffix);
      println("  seqz a0,a0");
      return;
    case ND_LT:
      println("  flt.%s a0,fa0,fa1", suffix);
      return;
    case ND_LE:
      println("  fle.%s a0,fa0,fa1", suffix);
      return;
    default: break;
    }

    error_tok(node->tok, "invalid expression");
  }

  gen_expr(node->rhs);
  push();
  gen_expr(node->lhs);
  pop(1);

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
    if (node->ty->is_unsigned) {
      println("  divu%s a0,a0,a1", suffix);
    } else {
      println("  div%s a0,a0,a1", suffix);
    }
    return;
  case ND_MOD:
    if (node->ty->is_unsigned) {
      println("  remu%s a0,a0,a1", suffix);
    } else {
      println("  rem%s a0,a0,a1", suffix);
    }
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
    if (node->lhs->ty->is_unsigned) {
      println("  sltu a0,a0,a1");
    } else {
      println("  slt a0,a0,a1");
    }
    return;
  case ND_LE:
    if (node->lhs->ty->is_unsigned) {
      println("  sgtu a0,a0,a1");
    } else {
      println("  sgt a0,a0,a1");
    }
    println("  xori a0,a0,1");
    return;
  case ND_SHL:
    println("  sll%s a0,a0,a1", suffix);
    return;
  case ND_SHR:
    if (node->lhs->ty->is_unsigned) {
      println("  srl%s a0,a0,a1", suffix);
    } else {
      println("  sra%s a0,a0,a1", suffix);
    }
    return;
  default: break;
  }

  error_tok(node->tok, "invalid expression");
}

static void gen_stmt(Node *node) {
  println("  .loc %d %d", node->tok->file->file_no, node->tok->line_no);
  switch (node->kind) {
  case ND_IF: {
    int c = count();
    gen_expr(node->cond);
    cmp_zero(node->cond->ty);
    println("  bne a0,zero,.L.else.%d", c);
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
      cmp_zero(node->cond->ty);
      println("  bne a0,zero,%s", node->brk_label);
    }
    gen_stmt(node->then);
    println("%s:", node->cont_label);
    if (node->inc)
      gen_expr(node->inc);
    println("  j .L.begin.%d", c);
    println("%s:", node->brk_label);
    return;
  }
  case ND_DO: {
    int c = count();
    println(".L.begin.%d:", c);
    gen_stmt(node->then);
    println("%s:", node->cont_label);
    gen_expr(node->cond);
    cmp_zero(node->cond->ty);
    println("  beqz a0,.L.begin.%d", c);
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
    if (node->lhs)
      gen_expr(node->lhs);
    println("  j .L.return.%s", current_fn->name);
    return;
  case ND_EXPR_STMT:
    gen_expr(node->lhs);
    return;
  default: break;
  }

  error_tok(node->tok, "invalid statement");
}

// Assign offsets to local variables.
static void assign_lvar_offsets(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function)
      continue;

    int top = 16;
    int bottom = 0;

    int gp = 0, fp = 0;

    // Assign offsets to pass-by-stack parameters.
    for (Obj *var = fn->params; var; var = var->next) {
      if (is_flonum(var->ty)) {
        if (fp < FP_MAX) {
          fp++;
          continue;
        } else if(gp < GP_MAX) {
          gp++;
          continue;
        }
      } else {
        if (gp++ < GP_MAX)
          continue;
      }

      top = align_to(top, 8);
      var->offset = top + var->ty->size;
      top += var->ty->size;
    }


    // Assign offsets to pass-by-register parameters and local variables.
    for (Obj *var = fn->locals; var; var = var->next) {
      if (var->offset)
        continue;

      bottom = align_to(bottom, var->align);
      var->offset = -bottom;
      bottom += var->ty->size;
    }

    fn->stack_size = align_to(bottom, 16);
  }
}

static void emit_data(Obj *prog) {
  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function || !var->is_definition)
      continue;

    if (var->is_static)
      println("  .local %s", var->name);
    else
      println("  .globl %s", var->name);

    println("  .align %d", (int)log2(var->align));

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

static void store_fp(int r, int offset, int sz) {
  println("  li t1,%d", offset - sz);
  println("  add t1,t1,s0");
  switch (sz) {
  case 4:
    println("  fsw fa%d, 0(t1)", r, offset);
    return;
  case 8:
    println("  fsd fa%d, 0(t1)", r, offset);
    return;
  }
  unreachable();
}

static void store_gp(int r, int offset, int sz) {
  println("  li t1,%d", offset - sz);
  println("  add t1,t1,s0");
  switch (sz) {
  case 1:
    println("  sb a%d,0(t1)", r);
    return;
  case 2:
    println("  sh a%d,0(t1)", r);
    return;
  case 4:
    println("  sw a%d,0(t1)", r);
    return;
  case 8:
    println("  sd a%d,0(t1)", r);
    return;
  }
  printf("WTF %d\n", sz);
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
    println("  sd ra,-8(sp)");
    println("  sd s0,-16(sp)");
    println("  addi s0,sp,-16");
    println("  li t1,-%d", fn->stack_size + 16);
    println("  add sp,sp,t1");

    // Save passed-by-register arguments to the stack
    int fp = 0, gp = 0;
    for (Obj *var = fn->params; var; var = var->next) {
      if (var->offset > 0) {
        continue;
      }

      // __va_area__
      if (var->ty->kind == TY_ARRAY) {
        int offset = var->offset - var->ty->size;
        while (gp < GP_MAX) {
          offset += 8;
          store_gp(gp++, offset, 8);
        }
      } else if (is_flonum(var->ty)) {
        if (fp < FP_MAX) {
          store_fp(fp++, var->offset, var->ty->size);
        } else {
          store_gp(gp++, var->offset, var->ty->size);
        }
      } else {
        store_gp(gp++, var->offset, var->ty->size);
      }
    }

    // Emit code
    gen_stmt(fn->body);
    assert(depth == 0);

    // Epilogue
    println(".L.return.%s:", fn->name);
    println("  li t1,%d", fn->stack_size + 16);
    println("  add sp,sp,t1");
    println("  ld ra,-8(sp)");
    println("  ld s0,-16(sp)");
    println("  ret");
  }
}

void codegen(Obj *prog, FILE *out) {
  output_file = out;

  File **files = get_input_files();
  for (int i = 0; files[i]; i++)
    println("  .file %d \"%s\"", files[i]->file_no, files[i]->name);

  assign_lvar_offsets(prog);
  emit_data(prog);
  emit_text(prog);
}
