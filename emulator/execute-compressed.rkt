#lang rosette/safe

(require
  "fmt.rkt"
  "machine.rkt"
  "parameters.rkt"
  "instr.rkt"
  "c-instr.rkt")

; Decode and execute all of the binary instructions and instruction as list

(define (adj-reg reg)
  (bvadd (zero-extend reg (bitvector 5)) (bv #x8 5)))

; Execute a 32 bit instruction
(define (execute-compressed m b_instr)
  (define opcode (extract 1 0 b_instr))
  (define funct3 (extract 15 13 b_instr))

  ; // Compressed instructions have 3-bit field for popular registers, which correspond to
  ; // registers x8 to x15.
  (cond
    [(bveq opcode (bv 0 2))
     (cond
       [(bveq funct3 (bv 0 3))
        ; c.addi4spn
        ; Expands to addi rd, x2, nzuimm, where rd=rd'+8.
        (define adj-rd (adj-reg (extract 4 2 b_instr)))
        (define nzuimm 
          (zero-extend
            (concat
              (extract 5 4 b_instr)
              (extract 9 6 b_instr)
              (extract 2 2 b_instr)
              (extract 3 3 b_instr)
              (bv #b00 2))
            (bitvector 12)))
        (cond
          [(bveq nzuimm (bv 0 12))
           'illegal-instruction]
          [else
           (c.addi-instr adj-rd (bv 2 5) nzuimm)])]
       [(bveq funct3 (bv 1 3))
        ; c.fld
        ; Expands to fld rd, offset(rs1), where rd=rd'+8 and rs1=rs1'+8.
        ; TODO: Implement Regular FLD First

        ; let rd = ((inst >> 2) & 0x7) + 8;
        ; let rs1 = ((inst >> 7) & 0x7) + 8;
        ; // offset[5:3|7:6] = isnt[12:10|6:5]
        ; let offset = ((inst << 1) & 0xc0) // imm[7:6]
        ;     | ((inst >> 7) & 0x38); // imm[5:3]
        ; let val = f64::from_bits(
        ;     self.read(self.xregs.read(rs1).wrapping_add(offset), DOUBLEWORD)?,
        ; );
        ; self.fregs.write(rd, val);
        (list 'c.fld)]
       [(bveq funct3 (bv 2 3))
        ; // c.lw
        ; // Expands to lw rd, offset(rs1), where rd=rd'+8 and rs1=rs1'+8.
        (define adj-rd (extract 4 2 b_instr))
        (define adj-rs1 (extract 9 7 b_instr))
        (define offset
          (zero-extend
            (concat
              (extract 5 5 b_instr)
              (extract 12 10 b_instr)
              (extract 6 6 b_instr)
              (bv #x00 2))
          (bitvector 64)))
        (define addr (bvadd offset (get-gprs-i (machine-gprs m) adj-rs1)))
        (lw-instr adj-rd addr)]
       [(bveq funct3 (bv 3 3))
        ; c.ld
        ; Expands to ld rd, offset(rs1), where rd=rd'+8 and rs1=rs1'+8.
        (define adj-rd (adj-reg (extract 4 2)))
        (define adj-rs1 (adj-reg (extract 9 7)))
        (define offset
          (zero-extend 
            (concat
              (extract 6 5 b_instr)
              (extract 12 10 b_instr)
              (bv #x000 3))
            (bitvector 64)))
        (define addr (bvadd offset (get-gprs-i (machine-gprs m) adj-rs1)))
        (ld-instr adj-rd addr)]
       [(bveq funct3 (bv #x4 3))
        ; // Reserved.
        ; panic!("reserved");
        ; TODO: Figure out what to do here
        (list 'illegal-instruction)]
       [(bveq funct3 (bv #x5 3))
        ; c.fsd
        ; Expands to fsd rs2, offset(rs1), where rs2=rs2'+8 and rs1=rs1'+8.
        ; TODO: Implement regular FSD

        ; let rs2 = ((inst >> 2) & 0x7) + 8;
        ; let rs1 = ((inst >> 7) & 0x7) + 8;
        ; // offset[5:3|7:6] = isnt[12:10|6:5]
        ; let offset = ((inst << 1) & 0xc0) // imm[7:6]
        ;     | ((inst >> 7) & 0x38); // imm[5:3]
        ; let addr = self.xregs.read(rs1).wrapping_add(offset);
        ; self.write(addr, self.fregs.read(rs2).to_bits() as u64, DOUBLEWORD)?;
        (list 'c.fsd)]
       [(bveq funct3 (bv #x6 3))
        ; c.sw
        ; Expands to sw rs2, offset(rs1), where rs2=rs2'+8 and rs1=rs1'+8.
        (define adj-rs2 (adj-reg (extract 4 2 b_instr)))
        (define adj-rs1 (adj-reg (extract 9 7 b_instr)))
        (define offset
          (zero-extend
            (concat
              (extract 5 5 b_instr)
              (extract 6 6 b_instr)
              (extract 12 10 b_instr)
              (bv #b00 2))
          (bitvector 64)))
        ; let addr = self.xregs.read(rs1).wrapping_add(offset);
        (define addr (bvadd offset (get-gprs-i (machine-gprs m) adj-rs1)))
        (sw-instr adj-rs2 addr)]
       [(bveq funct3 (bv #x7 3))
        ; c.sd
        ; Expands to sd rs2, offset(rs1), where rs2=rs2'+8 and rs1=rs1'+8.
        (define adj-rs2 (adj-reg (extract 4 2 b_instr)))
        (define adj-rs1 (adj-reg (extract 9 7 b_instr)))
        ; // offset[5:3|7:6] = isnt[12:10|6:5]
        (define offset
          (zero-extend
            (concat
              (extract 6 5 b_instr)
              (extract 9 7 b_instr)
              (bv #b000 3))))
        (define addr (bvadd offset (get-gprs-i (machine-gprs m))))
        ; let addr = self.xregs.read(rs1).wrapping_add(offset);
        ; self.write(addr, self.xregs.read(rs2), DOUBLEWORD)?
        (list 'c.sd)]
       [else
        'illegal-instruction])]
    [(bveq opcode (bv 1 2))
     (cond
       [(bveq funct3 (bv #x0 3))
        ; c.addi
        ; Expands to addi rd, rd, nzimm.
        ; inst_count!(self, "c.addi");

        ; let rd = (inst >> 7) & 0x1f;
        ; // nzimm[5|4:0] = inst[12|6:2]
        ; let mut nzimm = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f);
        ; // Sign-extended.
        ; nzimm = match (nzimm & 0x20) == 0 {
        ;     true => nzimm,
        ;     false => (0xc0 | nzimm) as i8 as i64 as u64,
        ; };
        ; if rd != 0 {
        ;     self.xregs
        ;         .write(rd, self.xregs.read(rd).wrapping_add(nzimm));
        ; }
        (list 'c.addi)]
       [(bveq funct3 (bv #x1 3))
        ; // c.addiw
        ; // Expands to addiw rd, rd, imm
        ; // "The immediate can be zero for C.ADDIW, where this corresponds to sext.w
        ; // rd"
        ; inst_count!(self, "c.addiw");

        ; let rd = (inst >> 7) & 0x1f;
        ; // imm[5|4:0] = inst[12|6:2]
        ; let mut imm = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f);
        ; // Sign-extended.
        ; imm = match (imm & 0x20) == 0 {
        ;     true => imm,
        ;     false => (0xc0 | imm) as i8 as i64 as u64,
        ; };
        ; if rd != 0 {
        ;     self.xregs.write(
        ;         rd,
        ;         self.xregs.read(rd).wrapping_add(imm) as i32 as i64 as u64,
        ;     );
        ; }
        (list 'c.addiw)]
       [(bveq funct3 (bv #x2 3))
        ; // c.li
        ; // Expands to addi rd, x0, imm.
        ; inst_count!(self, "c.li");

        ; let rd = (inst >> 7) & 0x1f;
        ; // imm[5|4:0] = inst[12|6:2]
        ; let mut imm = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f);
        ; // Sign-extended.
        ; imm = match (imm & 0x20) == 0 {
        ;     true => imm,
        ;     false => (0xc0 | imm) as i8 as i64 as u64,
        ; };
        ; if rd != 0 {
        ;     self.xregs.write(rd, imm);
        ; }
        (list 'c.li)]
       [(bveq funct3 (bv #x3 3))
        (define rd (extract 11 7 b_instr))
        (cond
          [(bveq rd (bv #x0 5))
           (list 'nop)]
          [(bveq rd (bv #x2 5))
           ; // c.addi16sp
           ; // Expands to addi x2, x2, nzimm
           ; inst_count!(self, "c.addi16sp");
 
           ; // nzimm[9|4|6|8:7|5] = inst[12|6|5|4:3|2]
           ; let mut nzimm = ((inst >> 3) & 0x200) // nzimm[9]
           ;     | ((inst >> 2) & 0x10) // nzimm[4]
           ;     | ((inst << 1) & 0x40) // nzimm[6]
           ;     | ((inst << 4) & 0x180) // nzimm[8:7]
           ;     | ((inst << 3) & 0x20); // nzimm[5]
           ; nzimm = match (nzimm & 0x200) == 0 {
           ;     true => nzimm,
           ;     // Sign-extended.
           ;     false => (0xfc00 | nzimm) as i16 as i32 as i64 as u64,
           ; };
           ; if nzimm != 0 {
           ;     self.xregs.write(2, self.xregs.read(2).wrapping_add(nzimm));
           (list 'c.addi16sp)]
          [else
           ; // c.lui
           ; // Expands to lui rd, nzimm.
           ; inst_count!(self, "c.lui");

           ; // nzimm[17|16:12] = inst[12|6:2]
           ; let mut nzimm = ((inst << 5) & 0x20000) | ((inst << 10) & 0x1f000);
           ; // Sign-extended.
           ; nzimm = match (nzimm & 0x20000) == 0 {
           ;     true => nzimm,
           ;     false => (0xfffc0000 | nzimm) as i32 as i64 as u64,
           ; };
           ; if nzimm != 0 {
           ;     self.xregs.write(rd, nzimm);
           (list 'c.lui)])]
       [(bveq funct3 (bv #x4 3))
        (define funct2 (extract 11 10 b_instr))
        (cond
          [(bveq funct2 (bv #x0 2))
           ; // c.srli
           ; // Expands to srli rd, rd, shamt, where rd=rd'+8.
           ; inst_count!(self, "c.srli");
           
           ; let rd = ((inst >> 7) & 0b111) + 8;
           ; // shamt[5|4:0] = inst[12|6:2]
           ; let shamt = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f);
           ; self.xregs.write(rd, self.xregs.read(rd) >> shamt);
           (list 'c.srli)]
          [(bveq funct2 (bv #x1 2))
           ; // c.srai
           ; // Expands to srai rd, rd, shamt, where rd=rd'+8.
           ; inst_count!(self, "c.srai");
           
           ; let rd = ((inst >> 7) & 0b111) + 8;
           ; // shamt[5|4:0] = inst[12|6:2]
           ; let shamt = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f);
           ; self.xregs
           ;     .write(rd, ((self.xregs.read(rd) as i64) >> shamt) as u64);
           (list 'c.sraw)]
          [(bveq funct2 (bv #x2 2))
          ;  // c.andi
          ; // Expands to andi rd, rd, imm, where rd=rd'+8.
          ; inst_count!(self, "c.andi");

          ; let rd = ((inst >> 7) & 0b111) + 8;
          ; // imm[5|4:0] = inst[12|6:2]
          ; let mut imm = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f);
          ; // Sign-extended.
          ; imm = match (imm & 0x20) == 0 {
          ;     true => imm,
          ;     false => (0xc0 | imm) as i8 as i64 as u64,
          ; };
          ; self.xregs.write(rd, self.xregs.read(rd) & imm);
          (list 'c.andi)]
          [(bveq funct2 (bv #x3 2))
           (define imm2 (extract 6 5 b_instr))
           (define imm1 (extract 12 12 b_instr))
           (cond
             [(and (bveq imm1 (bv #x0 1) (bveq imm2 (bv #x0 2))))
              ; // c.sub
              ; // Expands to sub rd, rd, rs2, rd=rd'+8 and rs2=rs2'+8.
              ; inst_count!(self, "c.sub");

              ; let rd = ((inst >> 7) & 0b111) + 8;
              ; let rs2 = ((inst >> 2) & 0b111) + 8;
              ; self.xregs.write(
              ;     rd,
              ;     self.xregs.read(rd).wrapping_sub(self.xregs.read(rs2)),
              ;    );
              (list 'c.sub)]
             [(and (bveq imm1 (bv #x0 1) (bveq imm2 (bv #x1 2))))
              ; // c.xor
              ; // Expands to xor rd, rd, rs2, rd=rd'+8 and rs2=rs2'+8.
              ; inst_count!(self, "c.xor");

              ; let rd = ((inst >> 7) & 0b111) + 8;
              ; let rs2 = ((inst >> 2) & 0b111) + 8;
              ; self.xregs
              ;     .write(rd, self.xregs.read(rd) ^ self.xregs.read(rs2));
              (list 'c.xor)
              ]
             [(and (bveq imm1 (bv #x0 1) (bveq imm2 (bv #x2 2))))
              ; // c.or
              ; // Expands to or rd, rd, rs2, rd=rd'+8 and rs2=rs2'+8.
              ; inst_count!(self, "c.or");

              ; let rd = ((inst >> 7) & 0b111) + 8;
              ; let rs2 = ((inst >> 2) & 0b111) + 8;
              ; self.xregs
              ;     .write(rd, self.xregs.read(rd) | self.xregs.read(rs2));
              (list 'c.or)]
             [(and (bveq imm1 (bv #x0 1) (bveq imm2 (bv #x3 2))))
              ; // c.and
              ; // Expands to and rd, rd, rs2, rd=rd'+8 and rs2=rs2'+8.
              ; inst_count!(self, "c.and");

              ; let rd = ((inst >> 7) & 0b111) + 8;
              ; let rs2 = ((inst >> 2) & 0b111) + 8;
              ; self.xregs
              ;     .write(rd, self.xregs.read(rd) & self.xregs.read(rs2));
              (list 'c.and)]
             [(and (bveq imm1 (bv #x1 1) (bveq imm2 (bv #x0 2))))
              ; // c.subw
              ; // Expands to subw rd, rd, rs2, rd=rd'+8 and rs2=rs2'+8.
              ; inst_count!(self, "c.subw");

              ; let rd = ((inst >> 7) & 0b111) + 8;
              ; let rs2 = ((inst >> 2) & 0b111) + 8;
              ; self.xregs.write(
              ;     rd,
              ;     self.xregs.read(rd).wrapping_sub(self.xregs.read(rs2))
              ;         as i32
              ;         as i64
              ;         as u64,
              (list 'c.subw)]
             [(and (bveq imm1 (bv #x1 1) (bveq imm2 (bv #x1 2))))
              ; // c.addw
              ; // Expands to addw rd, rd, rs2, rd=rd'+8 and rs2=rs2'+8.
              ; inst_count!(self, "c.addw");

              ; let rd = ((inst >> 7) & 0b111) + 8;
              ; let rs2 = ((inst >> 2) & 0b111) + 8;
              ; self.xregs.write(
              ;     rd,
              ;     self.xregs.read(rd).wrapping_add(self.xregs.read(rs2))
              ;         as i32
              ;         as i64
              ;         as u64,
              ; );
              (list 'c.addw)]
             [else
              'illegal-instruction])]
          [else
           'illegal-instruction])]
       [(bveq funct3 (bv #x5 3))
        ; // c.j
        ; // Expands to jal x0, offset.
        ; inst_count!(self, "c.j");

        ; // offset[11|4|9:8|10|6|7|3:1|5] = inst[12|11|10:9|8|7|6|5:3|2]
        ; let mut offset = ((inst >> 1) & 0x800) // offset[11]
        ;     | ((inst << 2) & 0x400) // offset[10]
        ;     | ((inst >> 1) & 0x300) // offset[9:8]
        ;     | ((inst << 1) & 0x80) // offset[7]
        ;     | ((inst >> 1) & 0x40) // offset[6]
        ;     | ((inst << 3) & 0x20) // offset[5]
        ;     | ((inst >> 7) & 0x10) // offset[4]
        ;     | ((inst >> 2) & 0xe); // offset[3:1]
        ;                            // Sign-extended.
        ; offset = match (offset & 0x800) == 0 {
        ;     true => offset,
        ;     false => (0xf000 | offset) as i16 as i64 as u64,
        ; };
        ; self.pc = self.pc.wrapping_add(offset).wrapping_sub(2);
        (list 'c.j)]
       [(bveq funct3 (bv #x6 3))
        ; // c.beqz
        ; // Expands to beq rs1, x0, offset, rs1=rs1'+8.
        ; inst_count!(self, "c.beqz");

        ; let rs1 = ((inst >> 7) & 0b111) + 8;
        ; // offset[8|4:3|7:6|2:1|5] = inst[12|11:10|6:5|4:3|2]
        ; let mut offset = ((inst >> 4) & 0x100) // offset[8]
        ;     | ((inst << 1) & 0xc0) // offset[7:6]
        ;     | ((inst << 3) & 0x20) // offset[5]
        ;     | ((inst >> 7) & 0x18) // offset[4:3]
        ;     | ((inst >> 2) & 0x6); // offset[2:1]
        ;                            // Sign-extended.
        ; offset = match (offset & 0x100) == 0 {
        ;     true => offset,
        ;     false => (0xfe00 | offset) as i16 as i64 as u64,
        ; };
        ; if self.xregs.read(rs1) == 0 {
        ;     self.pc = self.pc.wrapping_add(offset).wrapping_sub(2);
        ; }
        (list 'c.beqz)]
       [(bveq funct3 (bv #x7 3))
        ; // c.bnez
        ; // Expands to bne rs1, x0, offset, rs1=rs1'+8.
        ; inst_count!(self, "c.bnez");

        ; let rs1 = ((inst >> 7) & 0b111) + 8;
        ; // offset[8|4:3|7:6|2:1|5] = inst[12|11:10|6:5|4:3|2]
        ; let mut offset = ((inst >> 4) & 0x100) // offset[8]
        ;     | ((inst << 1) & 0xc0) // offset[7:6]
        ;     | ((inst << 3) & 0x20) // offset[5]
        ;     | ((inst >> 7) & 0x18) // offset[4:3]
        ;     | ((inst >> 2) & 0x6); // offset[2:1]
        ;                            // Sign-extended.
        ; offset = match (offset & 0x100) == 0 {
        ;     true => offset,
        ;     false => (0xfe00 | offset) as i16 as i64 as u64,
        ; };
        ; if self.xregs.read(rs1) != 0 {
        ;     self.pc = self.pc.wrapping_add(offset).wrapping_sub(2);
        ; }
        (list 'c.bnez)]
       [else
        'illegal-instruction])]
    [(bveq opcode (bv 2 2))
     (cond
       [(bveq funct3 (bv #x0 3))
        ; // c.slli
        ; // Expands to slli rd, rd, shamt.
        ; inst_count!(self, "c.slli");

        ; let rd = (inst >> 7) & 0x1f;
        ; // shamt[5|4:0] = inst[12|6:2]
        ; let shamt = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f);
        ; if rd != 0 {
        ;     self.xregs.write(rd, self.xregs.read(rd) << shamt);
        ; }
        (list 'c.slli)]
       [(bveq funct3 (bv #x1 3))
        ; // c.fldsp
        ; // Expands to fld rd, offset(x2).
        ; inst_count!(self, "c.fldsp");

        ; let rd = (inst >> 7) & 0x1f;
        ; // offset[5|4:3|8:6] = inst[12|6:5|4:2]
        ; let offset = ((inst << 4) & 0x1c0) // offset[8:6]
        ;     | ((inst >> 7) & 0x20) // offset[5]
        ;     | ((inst >> 2) & 0x18); // offset[4:3]
        ; let val =
        ;     f64::from_bits(self.read(self.xregs.read(2) + offset, DOUBLEWORD)?);
        ; self.fregs.write(rd, val);
        (list 'c.fldsp)]
       [(bveq funct3 (bv #x2 3))
        ; // c.lwsp
        ; // Expands to lw rd, offset(x2).
        ; inst_count!(self, "c.lwsp");

        ; let rd = (inst >> 7) & 0x1f;
        ; // offset[5|4:2|7:6] = inst[12|6:4|3:2]
        ; let offset = ((inst << 4) & 0xc0) // offset[7:6]
        ;     | ((inst >> 7) & 0x20) // offset[5]
        ;     | ((inst >> 2) & 0x1c); // offset[4:2]
        ; let val = self.read(self.xregs.read(2).wrapping_add(offset), WORD)?;
        ; self.xregs.write(rd, val as i32 as i64 as u64);
        (list 'c.lwsp)]
       [(bveq funct3 (bv #x3 3))
        ; // c.ldsp
        ; // Expands to ld rd, offset(x2).
        ; inst_count!(self, "c.ldsp");

        ; let rd = (inst >> 7) & 0x1f;
        ; // offset[5|4:3|8:6] = inst[12|6:5|4:2]
        ; let offset = ((inst << 4) & 0x1c0) // offset[8:6]
        ;     | ((inst >> 7) & 0x20) // offset[5]
        ;     | ((inst >> 2) & 0x18); // offset[4:3]
        ; let val = self.read(self.xregs.read(2).wrapping_add(offset), DOUBLEWORD)?;
        ; self.xregs.write(rd, val);
        (list 'c.ldsp)]
       [(bveq funct3 (bv #x4 3))
        ; match ((inst >> 12) & 0x1, (inst >> 2) & 0x1f) {
        ;     (0, 0) => {
        ;         // c.jr
        ;         // Expands to jalr x0, 0(rs1).
        ;         inst_count!(self, "c.jr");

        ;         let rs1 = (inst >> 7) & 0x1f;
        ;         if rs1 != 0 {
        ;             self.pc = self.xregs.read(rs1);
        ;         }
        ;     }
        ;     (0, _) => {
        ;         // c.mv
        ;         // Expands to add rd, x0, rs2.
        ;         inst_count!(self, "c.mv");

        ;         let rd = (inst >> 7) & 0x1f;
        ;         let rs2 = (inst >> 2) & 0x1f;
        ;         if rs2 != 0 {
        ;             self.xregs.write(rd, self.xregs.read(rs2));
        ;         }
        ;     }
        ;     (1, 0) => {
        ;         let rd = (inst >> 7) & 0x1f;
        ;         if rd == 0 {
        ;             // c.ebreak
        ;             // Expands to ebreak.
        ;             inst_count!(self, "c.ebreak");

        ;             return Err(Exception::Breakpoint);
        ;         } else {
        ;             // c.jalr
        ;             // Expands to jalr x1, 0(rs1).
        ;             inst_count!(self, "c.jalr");

        ;             let rs1 = (inst >> 7) & 0x1f;
        ;             // Don't add 2 because the pc already moved on.
        ;             let t = self.pc;
        ;             self.pc = self.xregs.read(rs1);
        ;             self.xregs.write(1, t);
        ;         }
        ;     }
        ;     (1, _) => {
        ;         // c.add
        ;         // Expands to add rd, rd, rs2.
        ;         inst_count!(self, "c.add");

        ;         let rd = (inst >> 7) & 0x1f;
        ;         let rs2 = (inst >> 2) & 0x1f;
        ;         if rs2 != 0 {
        ;             self.xregs.write(
        ;                 rd,
        ;                 self.xregs.read(rd).wrapping_add(self.xregs.read(rs2)),
        ;             );
        ;         }
        ;     }
        ;     (_, _) => {
        ;         return Err(Exception::IllegalInstruction);
        ;     }
        ; }
        (list 'TODO)]
       [(bveq funct3 (bv #x5 3))
        ; // c.fsdsp
        ; // Expands to fsd rs2, offset(x2).
        ; inst_count!(self, "c.fsdsp");

        ; let rs2 = (inst >> 2) & 0x1f;
        ; // offset[5:3|8:6] = isnt[12:10|9:7]
        ; let offset = ((inst >> 1) & 0x1c0) // offset[8:6]
        ;     | ((inst >> 7) & 0x38); // offset[5:3]
        ; let addr = self.xregs.read(2).wrapping_add(offset);
        ; self.write(addr, self.fregs.read(rs2).to_bits(), DOUBLEWORD)?;
        (list 'c.fsdsp)]
       [(bveq funct3 (bv #x6 3))
        ; // c.swsp
        ; // Expands to sw rs2, offset(x2).
        ; inst_count!(self, "c.swsp");

        ; let rs2 = (inst >> 2) & 0x1f;
        ; // offset[5:2|7:6] = inst[12:9|8:7]
        ; let offset = ((inst >> 1) & 0xc0) // offset[7:6]
        ;     | ((inst >> 7) & 0x3c); // offset[5:2]
        ; let addr = self.xregs.read(2).wrapping_add(offset);
        ; self.write(addr, self.xregs.read(rs2), WORD)?;
        (list 'c.swsp)]
       [(bveq funct3 (bv #x7 3))
        ; // c.sdsp
        ; // Expands to sd rs2, offset(x2).
        ; inst_count!(self, "c.sdsp");

        ; let rs2 = (inst >> 2) & 0x1f;
        ; // offset[5:3|8:6] = isnt[12:10|9:7]
        ; let offset = ((inst >> 1) & 0x1c0) // offset[8:6]
        ;     | ((inst >> 7) & 0x38); // offset[5:3]
        ; let addr = self.xregs.read(2).wrapping_add(offset);
        ; self.write(addr, self.xregs.read(rs2), DOUBLEWORD)?;
        (list 'c.sdsp)]
       [else
        'illegal-instruction])]
    [else
     'illegal-instruction]))
(provide execute-compressed)

; example: add x5, x6, x7
; (define b_instr (bv #b00000000011100110000001010110011 32))
; (define b_instr (bv #x11111111 32))
; (define instr (execute b_instr))
; (printf "~a~n" instr)
