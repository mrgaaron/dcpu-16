#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

static uint16_t literals[0x20] = {
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
    0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
    0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
};

typedef struct _machine {
	//registers
	uint16_t registers[8];
	//stack pointer
	uint16_t SP;
	//program counter
	uint16_t PC;
	//overflow register
	uint16_t O;
	//active memory - spec specifies 0x10000 words of ram
	uint16_t RAM[65536];
} machine;

typedef void (*op_func) (machine*, uint16_t, uint16_t);
typedef void (*extended_op_func) (machine*, uint16_t);

uint16_t *get_dcpu_value(machine *m, uint16_t opcode) {
    switch (opcode) {
        case 0x00: case 0x01: case 0x02: case 0x03:
        case 0x04: case 0x05: case 0x06: case 0x07:
            return m->registers + opcode;
        case 0x08: case 0x09: case 0x0a: case 0x0b:
        case 0x0c: case 0x0d: case 0x0e: case 0x0f:
            return m->RAM + m->registers[opcode & 7];
        case 0x10: case 0x11: case 0x12: case 0x13:
        case 0x14: case 0x15: case 0x16: case 0x17:
            //the mask at the end ensures the returned address is 16-bit
            return m->RAM + 
                    ((m->RAM[m->PC++] + m->registers[opcode & 7]) & 0xffff);
        case 0x18:
            return m->RAM + m->SP++;
        case 0x19:
            return m->RAM + m->SP;
        case 0x1a:
            return m->RAM + --(m->SP);
        case 0x1b:
            return &m->SP;
        case 0x1c:
            return &m->PC;
        case 0x1d:
            return &m->O;
        case 0x1e:
            return m->RAM + m->RAM[m->PC++];
        case 0x1f:
            return m->RAM + m->PC++;
        default:
            return literals + (opcode & 0x1F);
    };
};

inline void do_set_op(machine *m, uint16_t a, uint16_t b) {
    uint16_t *mem_loc = get_dcpu_value(m, a);
    uint16_t *b_val = get_dcpu_value(m, b);  
    
    *mem_loc = *b_val;
};

inline void do_add_op(machine *m, uint16_t a, uint16_t b) {
    uint32_t res;
    uint16_t *a_val = get_dcpu_value(m, a);
    uint16_t *b_val = get_dcpu_value(m, b);
    
    res = *a_val + *b_val;
    *a_val = res & 0xFF;
    m->O = res >> 16;
}

inline void do_sub_op(machine *m, uint16_t a, uint16_t b) {
    uint32_t res;
    uint16_t *a_val = get_dcpu_value(m, a);
    uint16_t *b_val = get_dcpu_value(m, b);
    
    res = *a_val - *b_val;
    *a_val = res & 0xFF;
    m->O = res >> 16;
}

inline void do_mul_op(machine *m, uint16_t a, uint16_t b) {
    uint16_t *a_val = get_dcpu_value(m, a);
    uint16_t *b_val = get_dcpu_value(m, b);
    
    *a_val = (*a_val) * (*b_val);
    m->O = (*a_val >> 16) & 0xFFFF;
}

inline void do_div_op(machine *m, uint16_t a, uint16_t b) {
    uint16_t *a_val = get_dcpu_value(m, a);
    uint16_t *b_val = get_dcpu_value(m, b);
    uint16_t res;
    
    res = (*a_val) / (*b_val);
    m->O = ((*a_val >> 16) / (*b_val)) & 0xFFFF;
    *a_val = res;
}

inline void do_mod_op(machine *m, uint16_t a, uint16_t b) {
    uint16_t *a_val = get_dcpu_value(m, a);
    uint16_t *b_val = get_dcpu_value(m, b);
    
    if (*b_val == 0) {
        *a_val = 0;
    } else {
        *a_val = (*a_val) % (*b_val);
    }
}

inline void do_shiftl_op(machine *m, uint16_t a, uint16_t b) {
    uint16_t *a_val = get_dcpu_value(m, a);
    uint16_t *b_val = get_dcpu_value(m, b);
    uint16_t res;
    
    res = (*a_val) << (*b_val);
    m->O = (((*a_val) << (*b_val)) >> 16) & 0xFFFF;
    *a_val = res;
}

inline void do_shiftr_op(machine *m, uint16_t a, uint16_t b) {
    uint16_t *a_val = get_dcpu_value(m, a);
    uint16_t *b_val = get_dcpu_value(m, b);
    uint16_t res;
    
    res = (*a_val) >> (*b_val);
    m->O = (((*a_val) << (*b_val)) >> 16) & 0xFFFF;
    *a_val = res;
}

inline void do_bitand_op(machine *m, uint16_t a, uint16_t b) {
    uint16_t *a_val = get_dcpu_value(m, a);
    uint16_t *b_val = get_dcpu_value(m, b);
    
    *a_val = (*a_val) & (*b_val);
}

inline void do_bitor_op(machine *m, uint16_t a, uint16_t b) {
    uint16_t *a_val = get_dcpu_value(m, a);
    uint16_t *b_val = get_dcpu_value(m, b);
    
    *a_val = (*a_val) | (*b_val);
}

inline void do_bitxor_op(machine *m, uint16_t a, uint16_t b) {
    uint16_t *a_val = get_dcpu_value(m, a);
    uint16_t *b_val = get_dcpu_value(m, b);
    
    *a_val = (*a_val) ^ (*b_val);
}

inline void do_ife_op(machine *m, uint16_t a, uint16_t b) {
    uint16_t *a_val = get_dcpu_value(m, a);
    uint16_t *b_val = get_dcpu_value(m, b);
    
    if ((*a_val) != (*b_val)) {
        m->PC += 1;
    }
}

inline void do_ifn_op(machine *m, uint16_t a, uint16_t b) {
    uint16_t *a_val = get_dcpu_value(m, a);
    uint16_t *b_val = get_dcpu_value(m, b);
    
    if ((*a_val) == (*b_val)) {
        m->PC += 1;
    }
}

inline void do_ifg_op(machine *m, uint16_t a, uint16_t b) {
    uint16_t *a_val = get_dcpu_value(m, a);
    uint16_t *b_val = get_dcpu_value(m, b);
    
    if ((*a_val) > (*b_val)) {
        m->PC += 1;
    }
}

inline void do_ifb_op(machine *m, uint16_t a, uint16_t b) {
    uint16_t *a_val = get_dcpu_value(m, a);
    uint16_t *b_val = get_dcpu_value(m, b);
    
    if (((*a_val) & (*b_val)) != 0) {
        m->PC += 1;
    }
}

op_func dispatch_table[15] = {
    &do_set_op, &do_add_op, &do_sub_op, &do_mul_op,
    &do_div_op, &do_mod_op, &do_shiftl_op, &do_shiftr_op,
    &do_bitand_op, &do_bitor_op, &do_bitxor_op, &do_ife_op,
    &do_ifn_op, &do_ifg_op, &do_ifb_op
};

inline void dummy(machine *m, uint16_t a) {
    return;
}

inline void do_jsr_op(machine *m, uint16_t a) {
    //this function performs the push
    uint16_t *stack_loc = get_dcpu_value(m, 0x1A);
    *stack_loc = m->PC;
    m->PC = a;
}

extended_op_func extended_dispatch_table[2] = {
    &dummy, &do_jsr_op
};

void execute_instruction(machine *m, uint16_t op) {
    uint16_t a, opcode, f_index;
    a = (op >> 4) & 0x3F;
    opcode = op & 0xF;
    
    //if the lower 4 bits of the instruction are unset, this is
    //an "extended" instruction, otherwise just execute a normal
    //instruction
    if (opcode != 0) {
        //gracefully shut down if the opcode would be out of bounds
        //on the dispatch table
        if (opcode > 15) {
            fprintf(stderr, "ILLEGAL OPCODE: %x", opcode);
            exit(1);
        }
        
        uint16_t b = (op >> 10);
        f_index = (op & 0xF) - 1;
        op_func f = dispatch_table[f_index];
        f(m, a, b);
    } else {
        if (opcode > 1) {
            fprintf(stderr, "ILLEGAL OPCODE: %x", opcode);
            exit(1);
        }
        
        f_index = (op & 0xF) - 1;
        extended_op_func f = extended_dispatch_table[f_index];
        f(m, a);
    }
    
};

void load_into_memory(machine* m, const char* path) {
    FILE *fh;
    char buffer[16];
    uint16_t counter = 0;
    
    if (!(fh = fopen(path, "r"))) {
        fprintf(stderr, "Unable to open file: %s\n", path);
        exit(1);
    }
    
    while (!feof(fh) && fgets(buffer, 16, fh)) {
        m->RAM[counter] = strtoul(buffer, 0, 16);
        counter++;
    }
    
    fclose(fh);
}

void init_machine(machine *m) {
    memset(&m, 0, sizeof(m));
}


int main(int argc, char** argv) {
    machine m;
    init_machine(&m);
    
    uint16_t *reg = get_dcpu_value(&m, 0x06);
    uint16_t *overflow = get_dcpu_value(&m, 0x1d);
    
    execute_instruction(&m, 0xa861);
    fprintf(stdout, "REGISTER I: %d\n", *reg);
    execute_instruction(&m, 0x8463);
    fprintf(stdout, "REGISTER I: %d\n", *reg);
    fprintf(stdout, "OVERFLOW: %d\n", *overflow);
    
    return 0;
};
