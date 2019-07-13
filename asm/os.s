        exit  = $00
        wrch  = $01
        rdch  = $02
        ;rdln  = $03
        hcli  = $04
        hbyte = $05
        file  = $06
        args  = $07
        bget  = $08
        bput  = $09
        gbpb  = $0a
        find  = $0b
        word  = $0c

.macro  host arg
        .byte   $02, arg
.endmacro

        ; .segment "CODE"
        .segment "UTILITY"
language:
        ;.byte    $40
        ;.byte    $bf
        ;inc      $8000
        ;jmp      $c558
        ;host 1
        lda       #<atomcli
        sta       $206
        lda       #>atomcli
        sta       $207
        rts
atomcli:
        host      hcli
        rts

        .segment "OS"
start:
        lda     #<dowrch
        sta     $20e ; XXX
        lda     #>dowrch
        sta     $20f
        lda     #$01
        jmp     language


wrdec:
        LDX #$FF
        SEC               ; Prepare for subtraction
wrdec100:
        INX
        SBC #100
        BCS wrdec100  ; Count how many 100s
        ADC #100
        JSR wrdecdigit    ; Print the 100s
        LDX #$FF
        SEC               ; Prepare for subtraction
wrdec10:
        INX
        SBC #10
        BCS wrdec10    ; Count how many 10s
        ADC #10
        JSR wrdecdigit     ; Print the 10s
        TAX                        ; Pass 1s into X
wrdecdigit:
        PHA
        TXA                    ; Save A, pass digit to A
        ORA #'0'
        JSR oswrch     ; Convert to character and print it
        PLA
        RTS                    ; Restore A and return

wrhexdigit:
        pha
        cmp     #9
        bcc     wrhex1
        clc                     ; XXX I know what carry is
        adc     #'A'-('9'+1)
wrhex1:
        clc
        adc     #'0'
        jsr     oswrch
        pla
        rts
wrhex:
        pha
        clc
        ror     a
        lsr     a
        lsr     a
        lsr     a
        jsr     wrhex1
        pla
        jmp     wrhex1

wrstr:  
        stx     $80
        sty     $81
        ldy     #0
wrstr2:
        lda     ($80), y
        beq     wrstr1
        host    wrch
        iny
        jmp     wrstr2
wrstr1: jsr     osnewl
        rts

donewl:
        lda     #$0a
        host    wrch
        clc
        rts
doasci:
        cmp     #$0d
        beq     donewl
        host    wrch
        clc
        rts
dowrch:
        host    wrch
        clc
        rts

dogbpb:
        host    gbpb
        clc
        rts

dofind:
        host    find
        clc
        rts

dobput:
        host    bput
        clc
        rts

dobget:
        host    bget
        clc
        rts

doargs:
        host    args
        clc
        rts

dofile:
        host    file
        clc
        rts

dordch:
        host    rdch
        clc
        rts

doword:
        host    word
        clc
        rts

dobyte:
        cmp     #$02
        bne     notselistrm
        jmp     selistrm
notselistrm:
        cmp     #$03
        bne     notselostrm
        jmp     selostrm
notselostrm:
        cmp     #$76
        bne     notkbdled
        jmp     kbdled
notkbdled:
        cmp     #$80
        bne     notadval
        jmp     adval
notadval:
        cmp     #$8b
        bne     notopt
        jmp     opt
notopt:
        cmp     #$ca
        bne     notkbdstatus
        jmp     kbdstatus
notkbdstatus:
        cmp     #$da
        bne     notvdulen
        jmp     vdulen
notvdulen:
        cmp     #$fe
        bne     notavblram
        jmp     avblram
notavblram:
        cmp     #$a8
        bne     notgetromptr
        jmp     getromptr
notgetromptr:
        cmp     #$90
        bne     nottv
        jmp     tv
nottv:
        cmp     #$ff
        bne     notrdopts
        jmp     rdopts
notrdopts:
        cmp     #212
        bne     notbellenv
        jmp     bellenv
notbellenv:
        cmp     #213
        bne     notbellfreq
        jmp     bellfreq
notbellfreq:
        cmp     #214
        bne     notbelldur
        jmp     belldur
notbelldur:
        cmp     #253
        bne     notrdbreak
        jmp     rdbreak
notrdbreak:
        ; Failure
        host    hbyte
        rts

        sta     $82
        txa
        sta     $83
        tya
        sta     $84
        ldx     #<errbyte
        ldy     #>errbyte
        jsr     wrstr
        lda     $82
        jsr     wrdec
        lda     #','
        host    wrch
        lda     $83
        jsr     wrdec
        lda     #','
        host    wrch
        lda     $84
        jsr     wrdec
        jsr     osnewl
        host    exit
errbyte:    .asciiz "Bad OSBYTE call:"

        ; What kind of reset happened last?
        ; I always say power up.

rdbreak:
        ldx     #1
        ldy     #0
        clc
        rts

        ; Read/write bell dur etc.

belldur:
        ldx     #0
        ldy     #0
        clc
        rts

        ; Read/write bell freq etc.

bellfreq:
        ldx     #0
        ldy     #0
        clc
        rts

        ; Read/write bell envelope etc.

bellenv:
        ldx     #0
        ldy     #0
        clc
        rts

; Read startup options
; Not used except to suggest we're working in MODE 7.

rdopts:
        ldx     #7
        ldy     #0
        clc
        rts

; Controls TV interlacing.
; Not useful here.

tv:
        ldx     #0
        ldy     #0
        clc
        rts

; Get address of base of extended vectors.
; These allow vectors like BRKV to point into
; paged ROMS.
; For now I'm using the hard coded address $0D80
; as the base for these.

getromptr:
        ldx     #$80
        ldy     #$0D
        clc
        rts

avblram:
        ldx     #$80 ; What is "next location"? AUG p.245
        clc
        rts
opt:
        clc
        rts

adval:
        pha
        txa
        cmp     #$ff
        beq     keybuf
        ; no adval
        ldx     #<erradval
        ldy     #>erradval
        jsr     wrstr
        host    exit
erradval:   .asciiz "Bad ADVAL call"

keybuf:
        pla
        ldx     #$00
        clc
        rts

docli:
        host    hcli
        rts

ack:
        ldx     $ff
        clc
        rts
;oshwm:
;        ldx     #$00
;        ldy     #$0e
;        lda     #$80
;        clc
;        rts

;himem:
;        ldx     #$00
;        ldy     #$80
;        lda     #$80
;        clc
;        rts

;rdhi:
;        pha
;        lda     #$ff
;        tax
;        tay
;        pla
;        clc
;        rts

selostrm:
        pha
        lda     #$00
        tax
        pla
        clc
        rts

selistrm:
        pha
        lda     #$00
        tax
        pla
        clc
        rts

kbdled:
        lda     #0
        clc
        rts

kbdstatus:
        lda     #0
        clc
        rts
vdulen:
        ldx     #$00
        ldy     #$00
        clc
        rts

handlebrk:
        sta     $0D00
        pla
        sta     $0D01 ; P
        pla
        sta     $0D02 ; lo
        pla
        sta     $0D03 ; hi
        lda     $0D02
        sec
        sbc     #1
        sta     $fd
        lda     $0D03
        sbc     #0
        sta     $fe
        lda     $0D03
        pha
        lda     $0D02
        pha
        lda     $0D01
        pha
        lda     $0D00
        jmp     ($202)
        ;jmp $b402 ; XXX

ebrk:
        jmp     ($0D83) ; XXX Should switch to ROM in 0D85

        .segment "EOSVECTORS"
euserv: jmp     euserv              ; XXX
ebrkv:  jmp     ebrk
   
        .segment "OSVECTORS"
osfind: jmp dofind
osgbpb: jmp dogbpb
osbput: jmp dobput
osbget: jmp dobget
osargs: jmp doargs
osfile: jmp dofile
osrdch: jmp dordch
osasci: jmp doasci
        .byte   $0
osnewl: jmp donewl
        .byte   $0,$0,$0,$0
oswrch: jmp dowrch
osword: jmp doword
osbyte: jmp dobyte
oscli:  jmp     docli
        .segment "VECTORS"
nmiv:
        .word   $aaaa
resetv:
        .word   start
irqv:
brkv:
        .word   handlebrk
        .end
