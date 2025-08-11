///
/// @file
/// @brief eForth - C++ vector-based object-threaded implementation
///
///====================================================================
#include <sstream>                     /// iostream, stringstream
#include <cstring>
#include "ceforth.h"

using namespace std;
///
///> Forth VM state variables
///
Code           root("forth", false);  ///< global dictionary
FV<Code*>      nspace;                ///< namespace stack
FV<Code*>      *dict = &root.vt;      ///< current namespace
Code           *last;                 ///< last word cached
Code           noname((XT)NULL);
///
///> macros to reduce verbosity (but harder to single-step debug)
///
#define VAR(i_w)     (*((*dict)[(int)((i_w) & 0xffff)]->pf[0]->q.data()+((i_w) >> 16)))
#define STR(i_w)     (                                      \
        EQ(i_w, UINT(-DU1))                                 \
        ? vm.pad.c_str()                                    \
        : (*dict)[(i_w) & 0xffff]->pf[(i_w) >> 16]->name    \
        )
#define BASE         ((U8*)&VAR(vm.id << 16))
#define DICT_PUSH(c) (dict->push(last=(Code*)(c)))
#define DICT_POP()   (delete dict->pop(),last=(*dict)[-1])
#define ADD_W(w)     (last->append((Code*)w))
#define BLAST        (nspace[-2]->vt[-1])              /** last word before branching */
#define BTGT         ((Bran*)(BLAST->pf[-1]))          /** branching target           */
#define BRAN(p)      ((p).merge(last->pf))             /** add branching code         */
#define BEND()       (delete dict->pop(),last=BLAST)   /** pop branching tmp off dict */
#define NEST(pf)     for (auto w : (pf)) w->nest(vm)
#define UNNEST()     throw 0

void dstat(const char *prefix, VM &vm) {
    for (int i=vm.compile; i>0; --i) printf(">> ");
    int sz = (int)nspace.size();
    printf("%s ns.sz=%d ", prefix, sz);
    for (int i=0; i<sz; i++) {
        Code *ns = nspace[i];
        printf("[%d]%s.vt[%ld] ", i, ns->name, ns->vt.size());
    }
    printf("last=%s.pf[%ld] compile=%d\n", last->name, last->pf.size(), vm.compile);
}
void _enscope(const char *s, VM &vm, Code *c) {
    dstat(s, vm);
    DICT_PUSH(c);                  /// create new word
    nspace.push(last);             /// store current namespace
    dict = &last->vt;              /// new word's vt keeps new namespace
    vm.compile++;
    dstat("after", vm);
}
void _descope(const char *s, VM &vm) {
    dstat(s, vm);
    nspace.pop();                  /// restore outer namespace
    dict = &nspace[-1]->vt;
    last = nspace[-vm.compile]->vt[-1];
    --vm.compile;
    dstat("after", vm);
}
///
///> Forth Dictionary Assembler
/// @note:
///    1. Dictionary construction sequence
///       * Code rom[] in statically build in compile-time
///       * vector<Code*> dict is populated in forth_init, i.e. first thing in main()
///    2. Macro CODE/IMMD use __COUNTER__ for array token/index can potetially
///       make the dictionary static but need to be careful the
///       potential issue comes with it.
///    3. a degenerated lambda becomes a function pointer
///
const Code rom[] {               ///< Forth dictionary
    CODE("bye",    forth_quit()),
    ///
    /// @defgroup ALU ops
    /// @{
    CODE("+",      TOS += SS.pop()),
    CODE("-",      TOS =  SS.pop() - TOS),
    CODE("*",      TOS *= SS.pop()),
    CODE("/",      TOS =  SS.pop() / TOS),
    CODE("mod",    TOS =  INT(MOD(SS.pop(), TOS))),          /// ( a b -- c ) c integer, see fmod
    CODE("*/",     TOS =  (DU2)SS.pop() * SS.pop() / TOS),   /// ( a b c -- d ) d=a*b / c (float) 
    CODE("/mod",   DU  n = SS.pop();                         /// ( a b -- c d ) c=a%b, d=int(a/b)
                   DU  t = TOS;
                   DU  m = MOD(n, t);
                   SS.push(m); TOS = INT(n / t)),
    CODE("*/mod",  DU2 n = (DU2)SS.pop() * SS.pop();         /// ( a b c -- d e ) d=(a*b)%c, e=(a*b)/c
                   DU2 t = TOS;
                   DU  m = MOD(n, t);
                   SS.push(m); TOS = INT(n / t)),
    CODE("and",    TOS = UINT(TOS) & UINT(SS.pop())),
    CODE("or",     TOS = UINT(TOS) | UINT(SS.pop())),
    CODE("xor",    TOS = UINT(TOS) ^ UINT(SS.pop())),
    CODE("abs",    TOS =  ABS(TOS)),
    CODE("negate", TOS =  -TOS),
    CODE("invert", TOS =  ~UINT(TOS)),
    CODE("rshift", TOS =  UINT(SS.pop()) >> UINT(TOS)),
    CODE("lshift", TOS =  UINT(SS.pop()) << UINT(TOS)),
    CODE("max",    DU n=SS.pop(); TOS = (TOS>n) ? TOS : n),
    CODE("min",    DU n=SS.pop(); TOS = (TOS<n) ? TOS : n),
    CODE("2*",     TOS *= 2),
    CODE("2/",     TOS /= 2),
    CODE("1+",     TOS += 1),
    CODE("1-",     TOS -= 1),
#if USE_FLOAT
    CODE("fmod",   TOS = MOD(SS.pop(), TOS)),             /// -3.5 2 fmod => -1.5
    CODE("f>s",    TOS = INT(TOS)),                       /// 1.9 => 1, -1.9 => -1
#else
    CODE("f>s",     /* do nothing */),
#endif // USE_FLOAT
    /// @}
    /// @defgroup Logic ops
    /// @{
    CODE("0=",     TOS = BOOL(ZEQ(TOS))),
    CODE("0<",     TOS = BOOL(LT(TOS, DU0))),
    CODE("0>",     TOS = BOOL(GT(TOS, DU0))),
    CODE("=",      TOS = BOOL(EQ(SS.pop(), TOS))),
    CODE(">",      TOS = BOOL(GT(SS.pop(), TOS))),
    CODE("<",      TOS = BOOL(LT(SS.pop(), TOS))),
    CODE("<>",     TOS = BOOL(!EQ(SS.pop(), TOS))),
    CODE(">=",     TOS = BOOL(!LT(SS.pop(), TOS))),
    CODE("<=",     TOS = BOOL(!GT(SS.pop(), TOS))),
    CODE("u<",     TOS = BOOL(UINT(SS.pop()) < UINT(TOS))),
    CODE("u>",     TOS = BOOL(UINT(SS.pop()) > UINT(TOS))),
    /// @}
    /// @defgroup Data Stack ops
    /// @brief - opcode sequence can be changed below this line
    /// @{
    CODE("dup",    PUSH(TOS)),
    CODE("drop",   TOS=SS.pop()),  /// note: SS.pop() != POP()
    CODE("swap",   DU n = SS.pop(); PUSH(n)),
    CODE("over",   PUSH(SS[-2])),
    CODE("rot",    DU n = SS.pop(); DU m = SS.pop(); SS.push(n); PUSH(m)),
    CODE("-rot",   DU n = SS.pop(); DU m = SS.pop(); PUSH(m);  PUSH(n)),
    CODE("pick",   TOS = SS[-TOS]),
    CODE("nip",    SS.pop()),
    CODE("?dup",   if (TOS != DU0) PUSH(TOS)),
    /// @}
    /// @defgroup Data Stack ops - double
    /// @{
    CODE("2dup",   PUSH(SS[-2]); PUSH(SS[-2])),
    CODE("2drop",  SS.pop(); TOS=SS.pop()),
    CODE("2swap",  DU n = SS.pop(); DU m = SS.pop(); DU l = SS.pop();
                   SS.push(n); PUSH(l); PUSH(m)),
    CODE("2over",  PUSH(SS[-4]); PUSH(SS[-4])),
    /// @}
    /// @defgroup Return Stack ops
    /// @{
    CODE(">r",     RS.push(POP())),
    CODE("r>",     PUSH(RS.pop())),
    CODE("r@",     PUSH(RS[-1])),
    /// @}
    /// @defgroup IO ops
    /// @{
    CODE("base",   PUSH(vm.id << 16)),   /// dict[0]->pf[0]->q[id] used for base
    CODE("decimal",dot(RDX, *BASE=10)),
    CODE("hex",    dot(RDX, *BASE=16)),
    CODE("bl",     PUSH(0x20)),
    CODE("cr",     dot(CR)),
    CODE(".",      dot(DOT,  POP())),
    CODE("u.",     dot(UDOT, POP())),
    CODE(".r",     IU w = POPI(); dotr(w, POP(), *BASE)),
    CODE("u.r",    IU w = POPI(); dotr(w, POP(), *BASE, true)),
    CODE("type",   POP(); U32 i_w=POPI(); pstr(STR(i_w))),
    CODE("key",    PUSH(key())),
    CODE("emit",   dot(EMIT, POP())),
    CODE("space",  dot(SPCS, DU1)),
    CODE("spaces", dot(SPCS, POP())),
    /// @}
    /// @defgroup Literal ops
    /// @{
    IMMD("(",      scan(')')),
    IMMD(".(",     pstr(scan(')'))),
    IMMD("\\",     scan('\n')),
    IMMD("s\"",
         const char *s = word('"'); if (!s) return;
         if (vm.compile) {
             ADD_W(new Str(s+1, last->token, (int)last->pf.size()));
         }
         else {
             vm.pad = s;                             /// copy string onto pad
             PUSH(-DU1); PUSH(STRLEN(s));            /// -1 = pad, len
         }),
    IMMD(".\"",
         const char *s = word('"'); if (!s) return;
         if (vm.compile) ADD_W(new Str(s+1));
         else            pstr(s)),
    /// @}
    /// @defgroup Branching ops
    /// @brief - if...then, if...else...then
    ///     dict[-2]->pf[0,1,2,...,-1] as *last
    ///                              \--->pf[...] if  <--+ merge
    ///                               \-->p1[...] else   |
    ///     dict[-1]->pf[...] as *tmp -------------------+
    /// @{
    IMMD("if",
         if (!vm.compile) _enscope("BEGIN", vm, new Bran(_begin));
         Bran *b = new Bran(_if);
         ADD_W(b);
         _enscope("IF", vm, b)),
    IMMD("else",
         _descope("done IF", vm);
         Bran *b = new Bran(_else);
         ADD_W(b);
         _enscope("ELSE", vm, b)),
    IMMD("then",
         _descope("THEN", vm);
         ADD_W(new Bran(_then));
         if (strcmp(last->name,"begin")==0) {
             noname.pf.clear();
             noname.pf.merge((*dict)[-1]->pf);
             _descope("BEGIN", vm);
//             DICT_POP();
             PUSH(-DU1);
         }),
    IMMD("end", _descope("END", vm)),
    /// @}
    /// @defgroup Loops
    /// @brief  - begin...again, begin...f until, begin...f while...repeat
    /// @{
    IMMD("begin",
         Bran *b = new Bran(_begin);
         ADD_W(b);
         _enscope("begin", vm, b)),
    IMMD("while",
         Bran *b = new Bran(_while);
         ADD_W(b);
         _enscope("while", vm, b)),
    IMMD("until",  _descope("until", vm)),
    IMMD("repeat", _descope("repeat", vm)),
    /// @}
    /// @defgrouop FOR loops
    /// @brief  - for...next, for...aft...then...next
    /// @{
    IMMD("for",
         ADD_W(new Bran(_toi));
         Bran *b = new Bran(_for);
         ADD_W(b);
         _enscope("for", vm, b)),
    IMMD("next", _descope("next", vm)),
    /// @}
    /// @defgrouop DO loops
    /// @brief  - do...loop, do..leave..loop
    /// @{
    IMMD("do",
         ADD_W(new Bran(_toi2));               ///< ( limit first -- )
         Bran *b = new Bran(_loop);
         ADD_W(b);
         _enscope("do", vm, b)),
    CODE("i",      PUSH(vm.i[-1])),
    CODE("j",      PUSH(vm.i[-2])),
    CODE("leave",  UNNEST()),                  /// * exit loop
    IMMD("loop",   _descope("loop", vm)),
    /// @}
    /// @defgrouop Compiler ops
    /// @{
    IMMD("[",      --vm.compile),
    IMMD("]",      vm.compile++),
    IMMD(":",      _enscope(":", vm, new Code(word()))),
    IMMD(";",      _descope(";", vm)),
    IMMD("constant",
         DICT_PUSH(new Code(word()));
         ADD_W(new Lit(POP()))),
    IMMD("variable",
         DICT_PUSH(new Code(word()));
         Code *w = ADD_W(new Var(DU0));
         w->pf[0]->token = w->token),
    IMMD("immediate", last->immd = 1),
    CODE("exit",   UNNEST()),           /// -- (exit from word)
    /// @}
    /// @defgroup metacompiler
    /// @brief - dict is directly used, instead of shield by macros
    /// @{
    CODE("exec",  (*dict)[POPI()]->nest(vm)),                    /// w --
    IMMD("create",
         DICT_PUSH(new Code(word()));
         Code *w = ADD_W(new Var(DU0));
         w->pf[0]->token = w->token;
         w->pf[0]->q.pop()),
    IMMD("does>",
         ADD_W(new Bran(_does));
         last->pf[-1]->token = last->token),                     /// keep WP
    CODE("to",                                                   /// n --
         const Code *w = find(word()); if (!w) return;
         VAR(w->token) = POP()),                                 /// update value
    CODE("is",                                                   /// w -- 
         Code *w = (Code*)find(word()); if (!w) return;          /// defered word
         IU i = POPI();  if (i >= (IU)dict->size()) return;      /// like this word
         Code *src = i >= DU0 ? &noname : (*dict)[i];
         w->pf.clear();                                          /// clear out w
         w->xt = src->xt;                                        /// built-in word
         w->pf.merge(src->pf)),                                  /// merge colon word
    /// @}
    /// @defgroup Memory Access ops
    /// @{
    CODE("@",       U32 i_w = POPI(); PUSH(VAR(i_w))),           /// a -- n
    CODE("!",       U32 i_w = POPI(); VAR(i_w) = POP()),         /// n a -- 
    CODE("+!",      U32 i_w = POPI(); VAR(i_w) += POP()),
    CODE("?",       U32 i_w = POPI(); dot(DOT, VAR(i_w))),
    CODE(",",       last->pf[0]->q.push(POP())),
    CODE("cells",   { /* for backward compatible */ }),          /// array index, inc by 1
    CODE("allot",   U32 n = POPI();                              /// n --
         for (U32 i=0; i<n; i++) last->pf[0]->q.push(DU0)),
    ///> Note:
    ///>   allot allocate elements in a word's q[] array
    ///>   to access, both indices to word itself and to q array are needed
    ///>   'th' a word that compose i_w, a 32-bit value, the 16 high bits
    ///>   serves as the q index and lower 16 lower bit as word index
    ///>   so a variable (array with 1 element) can be access as usual
    ///>
    CODE("th",      U32 i = POPI() << 16; TOS = UINT(TOS) | i),  /// w i -- i_w
    /// @}
#if DO_MULTITASK
    /// @defgroup Multitasking ops
    /// @}
    CODE("task",                                                /// w -- task_id
         IU w = POPI();                                         ///< dictionary index
         if ((*dict)[w]->xt) pstr("  ?colon word only\n");
         else PUSH(task_create(w))),                            /// create a task starting on pfa
    CODE("rank",    PUSH(vm.id)),                               /// ( -- n ) thread id
    CODE("start",   task_start(POPI())),                        /// ( task_id -- )
    CODE("join",    vm.join(POPI())),                           /// ( task_id -- )
    CODE("lock",    vm.io_lock()),                              /// wait for IO semaphore
    CODE("unlock",  vm.io_unlock()),                            /// release IO semaphore
    CODE("send",    IU t = POPI(); vm.send(t, POPI())),         /// ( v1 v2 .. vn n tid -- ) pass values onto task's stack
    CODE("recv",    vm.recv()),                                 /// ( -- v1 v2 .. vn ) waiting for values passed by sender
    CODE("bcast",   vm.bcast(POPI())),                          /// ( v1 v2 .. vn -- )
    CODE("pull",    IU t = POPI(); vm.pull(t, POPI())),         /// ( tid n -- v1 v2 .. vn )
    /// @}
#endif // DO_MULTITASK    
    /// @defgroup Debug ops
    /// @{
    CODE("abort",   TOS = -DU1; SS.clear(); RS.clear()),        /// clear ss, rs
    CODE("here",    PUSH(last->token)),
    CODE("'",
         const Code *w = find(word()); if (w) PUSH(w->token)),
    CODE(".s",      ss_dump(vm, true)),                         /// dump parameter stack
    CODE("words",   words(*vm.base)),                           /// display word lists
    IMMD("see",
         const Code *w = find(word());
         if (w) see(*w, *vm.base);
         dot(CR)),
    CODE("dict",    dict_dump(*vm.base)),                       /// display dictionary
    CODE("dump",                                                /// ' xx 1 dump
         IU n = POPI(); mem_dump(POPI(), n, *vm.base)), 
    CODE("depth",   PUSH(SS.size())),                           /// data stack depth
    /// @}
    /// @defgroup OS ops
    /// @{
    IMMD("include", load(vm, word())),                          /// include an OS file
    CODE("included",                                            /// include a file (programmable)
         POP(); U32 i_w = POPI(); load(vm, STR(i_w))),
    CODE("mstat",   mem_stat()),                                /// display memory stat
    CODE("clock",   PUSH(millis())),                            /// get system clock in msec
    CODE("rnd",     PUSH(RND())),                               /// get a random number
    CODE("ms",      IU i = POPI(); delay(i)),                   /// n -- delay n msec
    IMMD("forget",
         const Code *w = find(word()); if (!w) return;
         int   t = MAX((int)w->token, (int)find("boot")->token + 1);
         for (int i=(int)dict->size(); i>t; i--) DICT_POP()),
    CODE("boot",
         int t = find("boot")->token + 1;
         for (int i=(int)dict->size(); i>t; i--) DICT_POP())
};
///====================================================================
///
///> Code Class constructors
///
Code::Code(const char *s, const char *d, XT fp, U32 a)    ///> primitive word
    : name(s), desc(d), xt(fp), attr(a) {}
Code::Code(const char *s, bool n) {                       ///< new colon word
    const Code *w = find(s);                              /// * scan the dictionary
    name  = w ? w->name : (new string(s))->c_str();       /// * copy the name
    desc  = "";
    xt    = w ? w->xt : NULL;
    token = n ? dict->size() : 0;
    printf(" => new Code(%s)=%p token=%d\n", s, w ? w : this, token);
    if (n && w) pstr("reDef?");                           /// * warn word redefined
}
///
///> Forth inner interpreter
///
void Code::nest(VM &vm) {
//    vm.set_state(NEST);                /// * this => lock, major slow down
    vm.state = NEST;                     /// * racing? No, helgrind says so
    if (xt) { xt(vm, *this); return; }   /// * run primitive word

    for (int i=0; i < (int)pf.size(); i++) {
        try         { pf[i]->nest(vm); } /// * execute recursively
        catch (...) { break; }
        printf("%-3x: RS=%d, SS=%d I=%d %s\n",
            i, (int)vm.rs.size(), (int)vm.ss.size(), (int)vm.i.size(), pf[i]->name);
    }
}
///====================================================================
///
///> Primitive Functions
///
void _str( VM &vm, Code &c)  {
    if (!c.token) pstr(c.name);
    else { PUSH(c.token); PUSH(strlen(c.name)); }
}
void _lit( VM &vm, Code &c) { PUSH(c.q[0]);  }
void _var( VM &vm, Code &c) { PUSH(c.token); }
void _toi( VM &vm, Code &c) { vm.i.push(POP()); }
void _toi2(VM &vm, Code &c) { vm.i.push(POP()); vm.i.push(POP()); }
void _if(  VM &vm, Code &c) { vm.i.push(POP()); if (!ZEQ(vm.i[0])) NEST(c.pf); }
void _else(VM &vm, Code &c) { if (ZEQ(vm.i[0])) NEST(c.pf); }
void _then( VM &vm, Code &c){ vm.i.pop(); }
void _for( VM &vm, Code &c) {                  ///> for..next
    try {
        do {
            NEST(c.pf);
        } while ((vm.i[-1]-=DU1) >=0);         /// * for..next only
    }
    catch (...) {}                             /// handle EXIT
    vm.i.pop();
}
void _loop(VM &vm, Code &c) {                  ///> do..loop
    try { 
        do {
            NEST(c.pf);
        } while ((vm.i[-2]+=DU1) < vm.i[-1]);  /// increment counter
    }
    catch (...) {}                             /// handle LEAVE
    vm.i.pop(); vm.i.pop();                    /// pop off counters
}
void _begin(VM &vm, Code &c){                  ///> begin.while.repeat, begin.until
    while (true) {
        NEST(c.pf);                            /// * begin..
    }
}
void _while(VM &vm, Code &c) { if (POPI()) NEST(c.pf); }
void _end( VM &vm, Code &c) {}
void _does(VM &vm, Code &c) {
    bool hit = false;
    for (auto w : (*dict)[c.token]->pf) {
        if (hit) ADD_W(w);                     /// copy rest of pf
        if (STRCMP(w->name, "does>")==0) hit = true;
    }
    UNNEST();                                  /// exit caller
}
///====================================================================
///
///> Forth outer interpreter
///
const Code* find_ns(const char *ns) {
    auto ncolon = [](char *p) {
        int i = 0;
        for (i=0; (p=strchr(p, ':'))!=NULL; i++, p++);
        return i;
    };
    int n = ncolon((char*)ns);
    printf("ncolon=%d, find_ns %s ns.size=%ld\n", n, ns, nspace.size());
    for (int i = 0; i < (int)nspace.size(); i++) {
        printf("  ns[%d]=%s\n", i, nspace[i]->name);
        if (STRCMP(ns, nspace[i]->name)==0) return nspace[i];
    }
    return NULL;                               ///< global namespace
}
const Code *find(const char *s) {              ///> scan dictionary, last to first
    if (strchr(s, ':') > s) return find_ns(s);     ///< search by namespace
    for (int j = nspace.size() - 1; j >= 0; --j) { ///< search from leaf
        FV<Code*> &d = nspace[j]->vt;
        printf("find %s in [%d]%s.vt[%ld] => ", s, j, nspace[j]->name, d.size());
        for (int i = (int)d.size() - 1; i >= 0; --i) {
            if (STRCMP(s, d[i]->name)==0) {
                printf("[%d]%s\n", i, d[i]->name);
                return d[i];
            }
        }
    }
    printf("not found\n");
    return NULL;                               /// * word not found
}

DU parse_number(const char *s, int b) {
    switch (*s) {                              ///> base override
    case '%': b = 2;  s++; break;
    case '&':   
    case '#': b = 10; s++; break;
    case '$': b = 16; s++; break;
    }
    char *p;
    errno = 0;                                 ///> clear overflow flag
#if USE_FLOAT
    DU n = (b==10)
        ? static_cast<DU>(strtof(s, &p))
        : static_cast<DU>(strtol(s, &p, b));
#else
    DU n = static_cast<DU>(strtol(s, &p, b));
#endif
    if (errno || *p != '\0') throw runtime_error("");
    return n;
}

void forth_core(VM &vm, const char *idiom) {
    Code *w = (Code*)find(idiom);     ///< find the word named idiom in dict
    if (w) {                          /// * word found?
        if (vm.compile && !w->immd)   /// * are we compiling new word?
            ADD_W(w);                 /// * append word ptr to it
        else w->nest(vm);             /// * execute forth word
        return;
    }
    DU  n = parse_number(idiom, *vm.base);  ///< try as a number, throw exception
    if (vm.compile)                   /// * are we compiling new word?
        ADD_W(new Lit(n));            /// * append numeric literal to it
    else PUSH(n);                     /// * add value to data stack
}
///====================================================================
///
///> Forth VM - interface to outside world
///
void forth_init() {
    static bool init = false;         ///< singleton
    if (init) return;

    const int sz = (int)(sizeof(rom))/(sizeof(Code));
    dict->reserve(sz * 2);            /// * pre-allocate vector

    for (const Code &c : rom) {       /// * populate the dictionary
        DICT_PUSH(&c);                /// * ROM => RAM
    }
    nspace.push(&root);               ///< initialize namespace stack
/*
    const Code *ns = find_ns();
    printf("=> ns=%s size=%ld\n", ns ? ns->name : "not found", ns ? ns->vt.size() : 0);

    ns = find_ns("root");
    printf("=> ns=%s size=%ld\n", ns ? ns->name : "not found", ns ? ns->vt.size() : 0);

    const Code *c = find("boot");
    printf("c=%s token=%d\n", c ? c->name : "not found", c ? c->token : 0);
*/
    uvar_init();                      /// * initialize user area
    t_pool_init();                    /// * initialize thread pool
    VM &vm0   = vm_get(0);            ///< main thread
    vm0.state = HOLD;
}

void forth_teardown() {
    t_pool_stop();
    dict->clear();
}

int forth_vm(const char *line, void(*hook)(int, const char*)) {
    VM &vm = vm_get(0);               ///< main thread
    fout_setup(hook);                 /// * init output stream
    fin_setup(line);                  /// * refresh buffer if not resuming

    string idiom;
    while (fetch(idiom)) {            /// * read a word from line
        const char *s = idiom.c_str();
        try {
            vm.set_state(QUERY);
            forth_core(vm, s);        /// * send to Forth core
        }
        catch (exception &e) {
            pstr(s); pstr("?"); pstr(e.what(), CR);
            vm.compile = 0;
            scan('\n');               /// * exhaust input line
        }
    }
    ss_dump(vm);
    
    return vm.state==STOP;
}
