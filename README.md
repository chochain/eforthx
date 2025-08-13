# eForthX - eForth evolved

This is an evolution and experimental work trying to modernize eForth. It is spun off from [eForth](https://github.com/chochain/eforth) and is very much a work in progress.

Though Forth's VOCABULARY provides much greater flexibility than the scoping/namespace of other languages, to harness the power, it requires to understand an extra set of manipulation words. With background from imperative language (i.e. C/C++, Java, ...) and possibly hindered by the lack of the said words in Dr. Ting's eForth, it took me a few years to even get to them not to mention to gel my mental picture of it. See Bill Ragsdale's SVFIG [video](https://www.youtube.com/watch?v=wjppiefvc_U).

Hinted by Sean Pringle's [Rethinking Forth](https://github.com/seanpringle/reforth) and Travis Bemann's wonderful [zeptoforth](https://github.com/tabemann/zeptoforth), I twist my own arm to bring the new scoping constructs into the eForth I've done. Just to feel and to learn!

My own guide-line is not to pollute Forth language's simplicity and elegance nor deviate too far away from the common idiomatic usage. We'll see.

## Nested Module (or sub-words)
### master - vector implementation
```
struct Code {
    ...
    FV<Code*> vt;           /* each word has an extra lookup table */
    ...

Code      root("forth");    /* global word-list aka FORTH    */
FV<Code*> VS                /* vocabulary stack aka ROOT     */
FV<Code*> dict = &root.vt;  /* current namespace aka CONTEXT */
Code      *last;            /* last word cached, aka CURRENT */ 

Code Example
    : X ... ;           \ define X
    : Y                 \ define Y
        : a ... ;          \ nested word a
        : b ... ;          \ nested word b
        a b X ... ;     \ Y's code which calls a b and outer X
    ^ Y # a             \ call Y::a
    Y:a                 \ or in short (Forther might not like this)
```

### 50x - linear memory
```
    +------+-----+-----+------+----------+-----------------+
    | LINK | PFA | NSA | LAST | name-str | code/parameters |
    +------+-----+-----+------+----------+-----------------+

     0          0          0    <= NSA (namespace address)
      \          \          \
    <--[ W1 ] <-- [ W2 ] <-- [ W3 ] <-- LAST (word linked-list)
             \          \          \
              \         NSA         [ A ] <-- [ B ] <-- [ C ] <-- W3.LAST
              NSA         \
                \          [ A ] <-- [ B ] <-- [ X ] <-- W2.LAST
                 \
                  [ A ] <-- [ B ] <-- W1.LAST
                                 \
                                  [ A ] <-- [ X ] <-- [ Y ] <-- W1B.LAST
```

## Smart Compilation
Anton Ertl said "state-smartness is evil"! [see](http://www.euroforth.org/ef98/ertl98.pdf). But, for now, let's live through it to see what's the good and bad of it.
```
    : xx 2 for i . next ;
    
    3 for xx next                   \ eForthX can behave like a scripting language
    => 2 1 0 2 1 0 2 1 0 2 1 0 ok   \ without using :noname
```

## Simplified Control Structures
```
    0= if ... end
    3 > if ... else ... end
    
    3 for ... end
    10 0 do ... end
    
    begin
    ...
    3 = while ... leave
    4 = while ... leave
    0 = until ... leave
    ...
    repeat
```   

## Local variables/constants
```
    : xx
      variable x
      : x++ 1 x +! ;
      x++ x++ x @ . ;
    : yy
      [ 123 ] constant y
      3 y + ;
```      
   
## eForthX Internals
   TODO
   
## Source Code Directories

    + ~/src       - multi-threaded, dynamic vector-based, object threading
    + ~/platform  - platform specific code for C++, ESP32, Windows, and WASM

## References

## Revision History

* CC: 20250806: branch off from [eForth](https://github.com/chochain/eforth)
    + Refactor
    ```
        - add Code::vt (virtual table for nested node)
        - add Code::desc
        - move see to _sys (implementation)
        - change to FV<Code*> dict (12% faster than FV<Code>)
        - add FV<Code*> nspace (namespace stack in compilation)
        - update find to walk nspace outward
        - IMMD for all words with Code(word()), cannot break out from input stream
        - add FLUSH macro, along with ENDL (ok prompt is now inline)
        - add _else, _while, _end, _then, _next, _noname Branching primitives
        - add _enscope/_descope pair for scoping control
        - add VM::i (third stack) for loop counts and locals (future)
        - rename _tor,_tor2 => _toi, _toi2
        - drop Bran::p1,p2, by scoping envelopes now
        - use BASE_NODE instead of hardcoded [0]
    ```
