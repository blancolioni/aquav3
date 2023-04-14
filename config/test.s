.code

start:
    sethi %0, #16        ; load register 0 with about a million
    ld    %1, %0         ; save a copy in register 1
    geta  %2, text1      ; get the address of text1

.text

text1: string "Calculating processor speed"

.code
    call 1, put_line     ; write to terminal (1 argument)
    call get_tod         ; get seconds since midnight (32 bit floating point, no arguments, result in %3)

_1: 
    sub %1, %1, #1       ; run some instructions
    bne _1               ; to take up some time

    call get_tod         ; get new seconds in %4
    subf %3, %4, %3      ; find the time span between the two measurements
    divf %3, %3, %0      ; divide by number of loops
    divf %2, %2, #2      ; divide by instructions per loop
    int %2,%2            ; convert to integer

    ld   %5, %2          ; put instructions per second into %5
    call 1, put_int      ; write it
    geta %5, text2       ; load %5 with a message

.text
text2: string " bogoMIPS"

    call 1, put_line     ; write that
    halt
