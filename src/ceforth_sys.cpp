///
/// @file
/// @brief eForth - System Dependent Interface
///
///====================================================================
#include <sstream>                     /// iostream, stringstream
#include <cstring>
#include "ceforth.h"
using namespace std;

extern FV<Code*> *dict;
///
///> I/O streaming interface
///
istringstream   fin;                   ///< forth_in
ostringstream   fout;                  ///< forth_out
void (*fout_cb)(int, const char*);     ///< forth output callback functi
int load_dp = 0;                       ///< load depth control
///====================================================================
///
///> IO functions
///
void fin_setup(const char *line) {
    fout.str("");                      /// * clean output buffer
    fin.clear();                       /// * clear input stream error bit if any
    fin.str(line);                     /// * reload user command into input stream
}
void fout_setup(void (*hook)(int, const char*)) {
    auto cb = [](int, const char *rst) { printf("%s", rst); };
    fout_cb = hook ? hook : cb;        ///< serial output hook up
}
const char *scan(char c) {
    static string s;                   ///< temp str, static prevents reclaim
    getline(fin, s, c);                ///< scan fin for char c
    return s.c_str();                  ///< return the reference
}
const char *word(char delim) {         ///> read next idiom form input stream
    static string s;                   ///< temp str, static prevents reclaim
    delim ? getline(fin, s, delim) : (fin >> s);

    if (s.size()) return s.c_str();    ///< return a new copy of string

    pstr(" ?str");
    return NULL;
}
int fetch(string &idiom) {             ///> read an idiom from input stream
    return !(fin >> idiom)==0;
}
char key() { return word()[0]; }
void load(VM &vm, const char *fn) {    ///> include script from stream
    load_dp++;                         /// * increment depth counter
    void (*cb)(int, const char*) = fout_cb;  ///< keep output function
    string in; getline(fin, in);             ///< keep input buffers
    fout << ENDL;                      /// * flush output

    vm.rs.push(vm.state);              /// * save context
    vm.set_state(NEST);
    forth_include(fn);                 /// * send script to VM
    vm.set_state(static_cast<vm_state>(vm.rs.pop()));
    
    fout_cb = cb;                      /// * restore output cb
    fin.clear(); fin.str(in);          /// * restore input
    --load_dp;                         /// * decrement depth counter
}
void spaces(int n) { for (int i = 0; i < n; i++) fout << " "; }
void dot(io_op op, DU v) {
    switch (op) {
    case RDX:   fout << setbase(UINT(v));               break;
    case CR:    fout << ENDL;                           break;
    case DOT:   fout << v << " ";                       break;
    case UDOT:  fout << static_cast<U32>(v) << " ";     break;
    case EMIT:  { char b = (char)UINT(v); fout << b; }  break;
    case SPCS:  spaces(UINT(v));                        break;
    default:    fout << "unknown io_op=" << op << ENDL; break;
    }
}
void dotr(int w, DU v, int b, bool u) {
    fout << setbase(b) << setw(w)
         << (u ? static_cast<U32>(v) : v);
}
void pstr(const char *str, io_op op) {
    fout << str;
    if (op==CR) { fout << ENDL; }
}
///====================================================================
///
///> Debug functions
///
void ss_dump(VM &vm, bool forced) {       ///> display data stack and ok promt
    if (load_dp) return;                  /// * skip when including file
#if DO_WASM    
    if (!forced) { fout << "ok" << ENDL; return; }
#endif // DO_WASM
    char buf[34];
    auto rdx = [&buf](DU v, int b) {      ///> display v by radix
#if USE_FLOAT
        DU t, f = modf(v, &t);            ///< integral, fraction
        if (ABS(f) > DU_EPS) {
		    snprintf(buf, 32, "%0.6g", v);
            return buf;
        }
#endif // USE_FLOAT
        int i = 33;  buf[i]='\0';         /// * C++ can do only 8,10,16
        int dec = b==10;
        U32 n   = dec ? UINT(ABS(v)) : UINT(v);  ///< handle negative
        do {
            U8 d = (U8)MOD(n, b);  n /= b;
            buf[--i] = d > 9 ? (d-10)+'a' : d+'0';
        } while (n && i);
        if (dec && v < DU0) buf[--i]='-';
        return &buf[i];
    };
    if (vm.compile) {
        for (int i=0; i < vm.compile; i++) fout << "> ";
    }
    else {
        SS.push(TOS);
        for (DU v : SS) { fout << rdx(v, *vm.base) << ' '; }
        TOS = SS.pop();
        fout << "ok ";
    }
    fout << FLUSH;
}
typedef uintptr_t UFP;
void _see(const Code &c, int dp) {               ///< disassemble a colon word
    static const FV<Code*> nil = {};
    auto hdr = [](const string &s, int dp) {
        if (dp && s != "\t") { fout << ENDL; }    ///> newline control (skip branching \t)
        while (dp--) { fout << "  "; } fout << s; ///> indentation control
    };
    auto pp = [](const FV<Code*> &pf, int dp) {   ///> recursive dump with indent
        if (dp > 2) return;                       /// * non-recursive
        for (auto w : pf) _see(*w, dp + 1);
    };
    auto pq = [](FV<DU> &q) {
        for (DU i : q) fout << i << (q.size() > 1 ? " " : "");
    };
    auto pm = [](const Code &c, int dp) {          ///> recursive dump with indent
        for (auto w : c.vt) {
            if (w->is_bran) continue;
            fout << ENDL;
            for (int i=0; i<dp; i++) fout << "  "; ///> indentation control
            fout << ":" << w->name;
        }
    };
    string sn(c.name);

    if (c.xt==_str) {
        sn = (c.token ? "s\" " : ".\" ") + sn + "\""; hdr(sn, dp);
    }
    else if (c.xt == _var) { hdr("var ", dp); pq((*Var::qv)[c.token]); }
    else if (c.xt == _lit) { hdr("lit ", dp); fout << c.lit; }
    else {
        hdr(sn, dp); pp(c.pf, dp);
        pm(c, dp+1);
    }
}
void see(const Code &c, int base) {
    if (c.xt) fout << "  ->{ " << c.desc << "; }";
    else {
        fout << ": "; _see(c, 0); fout << " ;";
    }
}
void words(int base) {                    ///> display word list
    const int WIDTH = 60;
    int x = 0;
    fout << setbase(16) << setfill('0');
    for (auto w : *dict) {
#if CC_DEBUG > 1
        fout << setw(4) << w->token << "> "
             << (UFP)w << ' '
             << setw(8) << static_cast<U32>((UFP)w->xt)
             << (w->is_str ? '"' : ':') << (w->immd ? '*' : ' ')
             << w->name << "  " << ENDL;
#else // !CC_DEBUG
        fout << "  " << w->name;
        x += ((int)strlen(w->name) + 2);
        if (x > WIDTH) { fout << ENDL; x = 0; }
#endif // CC_DEBUG
    }
    fout << setfill(' ') << setbase(base) << ENDL;
}
///====================================================================
///
///> System statistics - for heap, stack, external memory debugging
///
void dict_dump(int base) {
    fout << setbase(16) << ENDL;
    int i=0;
    for (auto c : *dict) {
        fout << setfill('0') << setw(3) << i++
             << "> name=" << setw(8) << (UFP)c->name
             << ", xt="   << setw(8) << (UFP)c->xt
             << ", attr=" << setw(8) << c->attr
             << " "       << c->name << ENDL;
    }
    fout << setbase(base) << setfill(' ') << setw(-1);
}
void _dump(Code *c, int dp) {
    auto pp = [](const char *s, FV<Code*> &pf, int dp) {
        if (pf.size()==0) return;
        int i = dp;
        if (dp) { fout << ENDL; }                ///> newline control
        while (i--) { fout << "  "; }            ///> indentation control
        fout << s << "[" << pf.size() << "] { ";
        for (auto w : pf) {
            if (w->xt == _str) {
                fout << (w->token ? "s\" " : ".\" ") << w->name << "\"";
            }
            else if (w->token) fout << w->token;
            else if (w->xt == _var) {
                fout << "q{ ";
                for (auto v : (*Var::qv)[w->token]) fout << v << " ";
                fout << "}";
            }
            else fout << (w->name[0]=='\t' ? "\\t" : w->name);
            fout << " ";
            _dump(w, dp + 1);
        }
        fout << "} ";
    };
    pp("pf", c->pf, dp);
}
void mem_dump(IU w0, IU n, int base) {           ///> ' xx 1 dump
    fout << setbase(16) << setfill('0');
    auto cx = dict->begin() + w0 + n;
    for (auto c = dict->begin() + w0; c != cx; c++) {
        fout << setw(4) << (int)(c - dict->begin()) << ": ";
        if ((*c)->xt) fout << "built-in";        ///< primitives
        else          _dump(*c, 1);              ///< colon wordsa
        fout << ENDL;
    }
    fout << setbase(base) << setfill(' ');
}
///====================================================================
