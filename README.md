# eForthX - eForth evolved

This is an evolution and experimental work trying to modernize eForth. It is spun off from [eForth](https://github.com/chochain/eforth) and is very much a work in progress.

Hinted by Sean Pringle's [Rethinking Forth](https://github.com/seanpringle/reforth) and Travis Bemann's wornderful [zeptoforth](https://github.com/tabemann/zeptoforth). 

Hopefully, not to polute Forth language's simplicy and elegance nor diviate from the common idiomatic usage. Here's the list of proposed change. 

## Nested Module (or sub-words)

    FV<Code*> nspace - global namespace stack

    Code:: FV<Code*> vt - virtual table for current namespace
    
    Code Node: 
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
                                  
## Simplified Control Structures
## Smart Compilation

## eForthX Internals

## Source Code Directories

    + ~/src       - multi-threaded, dynamic vector-based, object threading
    + ~/platform  - platform specific code for C++, ESP32, Windows, and WASM
    + ~/orig      - archive from Dr. Ting and my past works
    +    /33b     - refactored ceForth_33, separate ASM from VM (used in eForth1 for Adruino UNO)
    +    /ting    - ceForth source codes collaborated with Dr. Ting
    +    /esp32   - esp32forth source codes collaborated with Dr. Ting
    +    /40x     - my experiments, refactor _40 into vector-based subroutine-threaded, with 16-bit offset
    +    /50x     - my experiments, add multi-threading to _40

## Benchmark and Tuning

### Desktop PC - 10K*10K cycles on 3.2GHz AMD**

#### v0.2 ~/src/ceforth, multi-threading capable, dynamic vector, object threading

## References

## Revision History

* CC: 20250806: branch off from [eForth](https://github.com/chochain/eforth)
    + Refactor
        - add Code::vt (virtual table for nested node)
        - add Code::desc
        - move see to _sys (implementation)
        - change to FV<Code*> dict (12% faster than FV<Code>)
        - add FV<Code*> nspace (namespace stack in compilation)
        - update find to walk nspace outward
        - IMMD for all words with Code(word()), cannot break out from input stream
        - add FLUSH macro, along with ENDL (ok prompt is now inline)
        - add _else, _while, _end, _then, _next, _noname Branching primtives
        - add _enscope/_descope pair for scoping control
        - add VM::i (third stack) for loop counts and locals (future)
        - rename _tor,_tor2 => _toi, _toi2
        - drop Bran::p1,p2, by scoping envelopes now
        - use BASE_NODE instead of hardcoded [0]
