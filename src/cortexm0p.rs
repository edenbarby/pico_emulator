use crate::utils::bit_is_set;

pub mod memory;

enum InstructionSignal {
    Branch,
    SupervisorCall,
    Breakpoint,
    SendEvent,
    WaitForEvent,
    WaitForInterrupt,
    Yeild,
    DataMemoryBarrier,
    DataSynchronizationBarrier,
    InstructionSynchronizationBarrier,
}
enum InstructionError {
    Undefined,
    Unpredictable,
}
//#[derive(PartialEq)]
#[derive(Clone, Copy)]
enum ShiftType {
    Lsl,
    Lsr,
    Asr,
    Ror,
    // RRX, // RRX is not supported by ARMv6-M.
}

type InstructionResult = Result<Option<InstructionSignal>, InstructionError>;

fn logical_shift_left(x: u32, shift: u32) -> (u32, bool) {
    (x << shift, bit_is_set(x, 32 - shift))
}

fn logical_shift_right(x: u32, shift: u32) -> (u32, bool) {
    (x >> shift, bit_is_set(x, shift - 1))
}

fn arithmetic_shift_right(x: u32, shift: u32) -> (u32, bool) {
    // Right shift of a signed type results in an arithmetic shift under the hood
    // (extends the sign bit).
    (((x as i32) >> shift) as u32, bit_is_set(x, shift - 1))
}

fn rotate_right(x: u32, shift: u32) -> (u32, bool) {
    let result = x.rotate_right(shift);
    (result, bit_is_set(result, 31))
}

fn decode_imm_shift(shift_type: ShiftType, imm: u32) -> u32 {
    match shift_type {
        ShiftType::Lsl => imm,
        ShiftType::Lsr | ShiftType::Asr => {
            if imm == 0 {
                32
            } else {
                imm
            }
        }
        ShiftType::Ror => {
            if imm == 0 {
                panic!("RRX is not supported by ARMv6-M.")
            } else {
                imm
            }
        }
    }
}

fn shift_c(value: u32, shift_type: ShiftType, amount: u32, carry_in: bool) -> (u32, bool) {
    if amount == 0 {
        (value, carry_in)
    } else {
        match shift_type {
            ShiftType::Lsl => logical_shift_left(value, amount),
            ShiftType::Lsr => logical_shift_right(value, amount),
            ShiftType::Asr => arithmetic_shift_right(value, amount),
            ShiftType::Ror => rotate_right(value, amount),
        }
    }
}

fn add_u32(lhs: u32, rhs: u32, carry: bool) -> (u32, bool) {
    let (a, b) = lhs.overflowing_add(rhs);
    let (c, d) = a.overflowing_add(carry as u32);
    (c, b || d)
}

fn add_i32(lhs: i32, rhs: i32, carry: bool) -> (i32, bool) {
    let (a, b) = lhs.overflowing_add(rhs);
    let (c, d) = a.overflowing_add(carry as i32);
    (c, b != d)
}

fn add_with_carry(x: u32, y: u32, carry_in: bool) -> (u32, bool, bool) {
    let (result, carry_out) = add_u32(x, y, carry_in);
    let (_, overflow) = add_i32(x as i32, y as i32, carry_in);

    (result, carry_out, overflow)
}

fn sign_extend(x: u32, width: u32) -> i32 {
    let shift = 32 - width;
    ((x << shift) as i32) >> shift
}
#[derive(PartialEq)]
enum StackType {
    Main,
    Process,
}
#[derive(PartialEq)]
enum ExecutionMode {
    Thread,
    Handler,
}
pub struct Cpu {
    general_purpose_registers: [u32; 13],
    stack_pointer_main: u32,
    stack_pointer_process: u32,
    link_register: u32,
    program_counter: u32,
    negative: bool,
    zero: bool,
    carry: bool,
    overflow: bool,
    exception_number: u32,
    thumb_mode_is_enabled: bool,
    prioritizable_interrupts_are_disabled: bool,
    thread_mode_is_unprivileged: bool,
    stack_pointer_select: StackType,
    execution_mode: ExecutionMode,
}

impl Cpu {
    pub fn new(memory: &memory::Memory) -> Cpu {
        const RESET_STACK_POINTER_ADDRESS: u32 = 0x0000_0000;
        const RESET_PROGRAM_COUNTER_ADDRESS: u32 = 0x0000_0004;

        let initial_stack_pointer =
            memory.read_u32(RESET_STACK_POINTER_ADDRESS).unwrap() & 0xFFFF_FFFC;
        let reset_routine_address_and_thumb_bit =
            memory.read_u32(RESET_PROGRAM_COUNTER_ADDRESS).unwrap();

        // VTOR = 0
        Cpu {
            general_purpose_registers: [0; 13],
            stack_pointer_main: initial_stack_pointer,
            stack_pointer_process: 0,
            link_register: 0,
            program_counter: reset_routine_address_and_thumb_bit & 0xFFFF_FFFE,
            negative: false,
            zero: false,
            carry: false,
            overflow: false,
            exception_number: 0,
            thumb_mode_is_enabled: (reset_routine_address_and_thumb_bit & 0b1) == 0b1,
            prioritizable_interrupts_are_disabled: false,
            thread_mode_is_unprivileged: false,
            stack_pointer_select: StackType::Main,
            execution_mode: ExecutionMode::Thread,
        }
    }

    pub fn step(&mut self, memory: &mut memory::Memory) {
        let program_counter = self.read_current_instruction_address();
        let instruction0 = memory.read_u16(program_counter).unwrap();

        println!(
            "0x{0:08X} => 0x{1:04X} (0b{1:016b})",
            program_counter, instruction0
        );
        if program_counter == 0x1902 {
            println!();
        }

        let mut program_counter_increments = 1;
        let instruction_jump_table_index = (instruction0 >> 8) & 0b1111_1111;
        let instruction_result = match instruction_jump_table_index {
            0b00000000..=0b00000111 => self.lsl_immediate_t1(instruction0),
            0b00001000..=0b00001111 => self.lsr_immediate_t1(instruction0),
            0b00010000..=0b00010111 => self.asr_immediate_t1(instruction0),
            0b00011000..=0b00011001 => self.add_register_t1(instruction0),
            0b00011010..=0b00011011 => self.sub_register_t1(instruction0),
            0b00011100..=0b00011101 => self.add_immediate_t1(instruction0),
            0b00011110..=0b00011111 => self.sub_immediate_t1(instruction0),
            0b00100000..=0b00100111 => self.mov_immediate_t1(instruction0),
            0b00101000..=0b00101111 => self.cmp_immediate_t1(instruction0),
            0b00110000..=0b00110111 => self.add_immediate_t2(instruction0),
            0b00111000..=0b00111111 => self.sub_immediate_t2(instruction0),
            0b01000000..=0b01000000 => {
                if (instruction0 & Self::LSL_REGISTER_T1_MSK) == Self::LSL_REGISTER_T1_VAL {
                    self.lsl_register_t1(instruction0)
                } else if (instruction0 & Self::EOR_REGISTER_T1_MSK) == Self::EOR_REGISTER_T1_VAL {
                    self.eor_register_t1(instruction0)
                } else if (instruction0 & Self::AND_REGISTER_T1_MSK) == Self::AND_REGISTER_T1_VAL {
                    self.and_register_t1(instruction0)
                } else if (instruction0 & Self::LSR_REGISTER_T1_MSK) == Self::LSR_REGISTER_T1_VAL {
                    self.lsr_register_t1(instruction0)
                } else {
                    panic!()
                }
            }
            0b01000001..=0b01000001 => {
                if (instruction0 & Self::SBC_REGISTER_T1_MSK) == Self::SBC_REGISTER_T1_VAL {
                    self.sbc_register_t1(instruction0)
                } else if (instruction0 & Self::ADC_REGISTER_T1_MSK) == Self::ADC_REGISTER_T1_VAL {
                    self.adc_register_t1(instruction0)
                } else if (instruction0 & Self::ROR_REGISTER_T1_MSK) == Self::ROR_REGISTER_T1_VAL {
                    self.ror_register_t1(instruction0)
                } else if (instruction0 & Self::ASR_REGISTER_T1_MSK) == Self::ASR_REGISTER_T1_VAL {
                    self.asr_register_t1(instruction0)
                } else {
                    panic!()
                }
            }
            0b01000010..=0b01000010 => {
                if (instruction0 & Self::RSB_IMMEDIATE_T1_MSK) == Self::RSB_IMMEDIATE_T1_VAL {
                    self.rsb_immediate_t1(instruction0)
                } else if (instruction0 & Self::CMP_REGISTER_T1_MSK) == Self::CMP_REGISTER_T1_VAL {
                    self.cmp_register_t1(instruction0)
                } else if (instruction0 & Self::TST_REGISTER_T1_MSK) == Self::TST_REGISTER_T1_VAL {
                    self.tst_register_t1(instruction0)
                } else if (instruction0 & Self::CMN_REGISTER_T1_MSK) == Self::CMN_REGISTER_T1_VAL {
                    self.cmn_register_t1(instruction0)
                } else {
                    panic!()
                }
            }
            0b01000011..=0b01000011 => {
                if (instruction0 & Self::ORR_REGISTER_T1_MSK) == Self::ORR_REGISTER_T1_VAL {
                    self.orr_register_t1(instruction0)
                } else if (instruction0 & Self::MVN_REGISTER_T1_MSK) == Self::MVN_REGISTER_T1_VAL {
                    self.mvn_register_t1(instruction0)
                } else if (instruction0 & Self::BIC_REGISTER_T1_MSK) == Self::BIC_REGISTER_T1_VAL {
                    self.bic_register_t1(instruction0)
                } else if (instruction0 & Self::MUL_T1_MSK) == Self::MUL_T1_VAL {
                    self.mul_t1(instruction0)
                } else {
                    panic!()
                }
            }
            0b01000100..=0b01000100 => self.add_register_t2(instruction0),
            0b01000101..=0b01000101 => self.cmp_register_t2(instruction0),
            0b01000110..=0b01000110 => self.mov_register_t1(instruction0),
            0b01000111..=0b01000111 => {
                if (instruction0 & Self::BX_T1_MSK) == Self::BX_T1_VAL {
                    self.bx_t1(instruction0)
                } else if (instruction0 & Self::BLX_REGISTER_T1_MSK) == Self::BLX_REGISTER_T1_VAL {
                    self.blx_register_t1(instruction0)
                } else {
                    panic!()
                }
            }
            0b01001000..=0b01001111 => self.ldr_literal_t1(instruction0, memory),
            0b01010000..=0b01010001 => self.str_register_t1(instruction0, memory),
            0b01010010..=0b01010011 => self.strh_register_t1(instruction0, memory),
            0b01010100..=0b01010101 => self.strb_register_t1(instruction0, memory),
            0b01010110..=0b01010111 => self.ldrsb_register_t1(instruction0, memory),
            0b01011000..=0b01011001 => self.ldr_register_t1(instruction0, memory),
            0b01011010..=0b01011011 => self.ldrh_register_t1(instruction0, memory),
            0b01011100..=0b01011101 => self.ldrb_register_t1(instruction0, memory),
            0b01011110..=0b01011111 => self.ldrsh_register_t1(instruction0, memory),
            0b01100000..=0b01100111 => self.str_immediate_t1(instruction0, memory),
            0b01101000..=0b01101111 => self.ldr_immediate_t1(instruction0, memory),
            0b01110000..=0b01110111 => self.strb_immediate_t1(instruction0, memory),
            0b01111000..=0b01111111 => self.ldrb_immediate_t1(instruction0, memory),
            0b10000000..=0b10000111 => self.strh_immediate_t1(instruction0, memory),
            0b10001000..=0b10001111 => self.ldrh_immediate_t1(instruction0, memory),
            0b10010000..=0b10010111 => self.str_immediate_t2(instruction0, memory),
            0b10011000..=0b10011111 => self.ldr_immediate_t2(instruction0, memory),
            0b10100000..=0b10100111 => self.adr_t1(instruction0),
            0b10101000..=0b10101111 => self.add_sp_plus_immediate_t1(instruction0),
            0b10110000..=0b10110000 => {
                if (instruction0 & Self::ADD_SP_PLUS_IMMEDIATE_T2_MSK)
                    == Self::ADD_SP_PLUS_IMMEDIATE_T2_VAL
                {
                    self.add_sp_plus_immediate_t2(instruction0)
                } else if (instruction0 & Self::SUB_SP_MINUS_IMMEDIATE_T1_MSK)
                    == Self::SUB_SP_MINUS_IMMEDIATE_T1_VAL
                {
                    self.sub_sp_minus_immediate_t1(instruction0)
                } else {
                    panic!()
                }
            }
            0b10110010..=0b10110010 => {
                if (instruction0 & Self::UXTH_T1_MSK) == Self::UXTH_T1_VAL {
                    self.uxth_t1(instruction0)
                } else if (instruction0 & Self::SXTB_T1_MSK) == Self::SXTB_T1_VAL {
                    self.sxtb_t1(instruction0)
                } else if (instruction0 & Self::UXTB_T1_MSK) == Self::UXTB_T1_VAL {
                    self.uxtb_t1(instruction0)
                } else if (instruction0 & Self::SXTH_T1_MSK) == Self::SXTH_T1_VAL {
                    self.sxth_t1(instruction0)
                } else {
                    panic!()
                }
            }
            0b10110100..=0b10110101 => self.push_t1(instruction0, memory),
            0b10110110..=0b10110110 => self.cps_t1(instruction0),
            0b10111010..=0b10111010 => {
                if (instruction0 & Self::REVSH_T1_MSK) == Self::REVSH_T1_VAL {
                    self.revsh_t1(instruction0)
                } else if (instruction0 & Self::REV_T1_MSK) == Self::REV_T1_VAL {
                    self.rev_t1(instruction0)
                } else if (instruction0 & Self::REV16_T1_MSK) == Self::REV16_T1_VAL {
                    self.rev16_t1(instruction0)
                } else {
                    panic!()
                }
            }
            0b10111100..=0b10111101 => self.pop_t1(instruction0, memory),
            0b10111110..=0b10111110 => self.bkpt_t1(),
            0b10111111..=0b10111111 => {
                if (instruction0 & Self::WFI_T1_MSK) == Self::WFI_T1_VAL {
                    self.wfi_t1()
                } else if (instruction0 & Self::SEV_T1_MSK) == Self::SEV_T1_VAL {
                    self.sev_t1()
                } else if (instruction0 & Self::YEILD_T1_MSK) == Self::YEILD_T1_VAL {
                    self.yeild_t1()
                } else if (instruction0 & Self::NOP_T1_MSK) == Self::NOP_T1_VAL {
                    self.nop_t1()
                } else if (instruction0 & Self::WFE_T1_MSK) == Self::WFE_T1_VAL {
                    self.wfe_t1()
                } else {
                    panic!()
                }
            }
            0b11000000..=0b11000111 => self.stm_t1(instruction0, memory),
            0b11001000..=0b11001111 => self.ldm_t1(instruction0, memory),
            0b11010000..=0b11011111 => self.b_t1(instruction0),
            0b11100000..=0b11100111 => self.b_t2(instruction0),
            0b11110000..=0b11110111 => {
                // instruction_was_32bit = true;
                // self.increment_program_counter();
                let instruction1 = memory
                    .read_u16(self.read_current_instruction_address() + 2)
                    .unwrap();
                // self.increment_program_counter();
                program_counter_increments += 1;

                if (instruction1 & Self::UDF_T2_1_MSK) == Self::UDF_T2_1_VAL {
                    self.udf_t2()
                } else if (instruction1 & Self::BL_T1_1_MSK) == Self::BL_T1_1_VAL {
                    self.bl_t1(instruction0, instruction1)
                } else if (instruction1 & Self::ISB_T1_1_MSK) == Self::ISB_T1_1_VAL {
                    self.isb_t1()
                } else if (instruction1 & Self::MRS_T1_1_MSK) == Self::MRS_T1_1_VAL {
                    self.mrs_t1(instruction1)
                } else if (instruction1 & Self::DMB_T1_1_MSK) == Self::DMB_T1_1_VAL {
                    self.dmb_t1()
                } else if (instruction1 & Self::DSB_T1_1_MSK) == Self::DSB_T1_1_VAL {
                    self.dsb_t1()
                } else if (instruction1 & Self::MSR_REGISTER_T1_1_MSK)
                    == Self::MSR_REGISTER_T1_1_VAL
                {
                    self.msr_register_t1(instruction0, instruction1)
                } else {
                    panic!()
                }
            }
            _ => panic!(),
        };

        match instruction_result {
            Ok(instruction_signal_opt) => {
                if let Some(instruction_signal) = instruction_signal_opt {
                    match instruction_signal {
                        InstructionSignal::Branch => {
                            // If we branch, the PC will be pointing at exactly the new address we
                            // want to execute so don't increment it!
                            program_counter_increments = 0;
                        }
                        InstructionSignal::SupervisorCall => panic!("todo"),
                        InstructionSignal::Breakpoint => panic!("todo"),
                        InstructionSignal::SendEvent => panic!("todo"),
                        InstructionSignal::WaitForEvent => panic!("todo"),
                        InstructionSignal::WaitForInterrupt => panic!("todo"),
                        InstructionSignal::Yeild => panic!("todo"),
                        InstructionSignal::DataMemoryBarrier => panic!("todo"),
                        InstructionSignal::DataSynchronizationBarrier => {
                            panic!("todo")
                        }
                        InstructionSignal::InstructionSynchronizationBarrier => {
                            panic!("todo")
                        }
                    }
                }
            }
            Err(instruction_error) => match instruction_error {
                InstructionError::Undefined => panic!("todo"),
                InstructionError::Unpredictable => panic!("todo"),
            },
        }

        self.increment_program_counter(program_counter_increments);
    }

    fn register_read(&self, index: u16) -> u32 {
        match index {
            0..=12 => self.general_purpose_registers[index as usize],
            13 => self.read_stack_pointer(),
            14 => self.link_register,
            15 => self.read_program_counter(),
            _ => panic!(),
        }
    }
    fn register_write(&mut self, index: u16, value: u32) {
        match index {
            0..=12 => self.general_purpose_registers[index as usize] = value,
            13 => self.write_stack_pointer(value),
            14 => self.link_register = value,
            15 => panic!("Use branch to change PC."),
            _ => panic!(),
        }
    }
    fn read_stack_pointer(&self) -> u32 {
        match self.stack_pointer_select {
            StackType::Main => self.stack_pointer_main,
            StackType::Process => self.stack_pointer_process,
        }
    }
    fn write_stack_pointer(&mut self, new_stack_pointer: u32) {
        match self.stack_pointer_select {
            StackType::Main => self.stack_pointer_main = new_stack_pointer,
            StackType::Process => self.stack_pointer_process = new_stack_pointer,
        }
    }
    fn select_stack_pointer(&mut self, choice: StackType) {
        self.stack_pointer_select = choice;
    }
    fn read_current_instruction_address(&self) -> u32 {
        self.program_counter
    }
    fn read_program_counter(&self) -> u32 {
        self.program_counter + 4
    }
    fn read_word_aligned_program_counter(&mut self) -> u32 {
        self.read_program_counter() & 0xFFFF_FFFC
    }
    fn increment_program_counter(&mut self, half_words: u32) {
        self.program_counter += 2 * half_words;
    }
    fn branch_write_pc(&mut self, address: u32) {
        self.program_counter = address & 0xFFFF_FFFE;
    }
    fn bx_write_pc(&mut self, address: u32) {
        if (self.execution_mode == ExecutionMode::Handler) && (((address >> 28) & 0b1111) == 0b1111)
        {
            self.exception_return();
        } else {
            self.thumb_mode_is_enabled = address & 0b1 == 1;
            self.program_counter = address & 0xFFFF_FFFE;
        }
    }
    fn blx_write_pc(&mut self, address: u32) {
        self.thumb_mode_is_enabled = address & 0b1 == 1;
        self.program_counter = address & 0xFFFF_FFFE;
    }
    fn load_write_pc(&mut self, address: u32) {
        self.bx_write_pc(address);
    }
    fn alu_write_pc(&mut self, address: u32) {
        self.branch_write_pc(address);
    }
    fn exception_return(&mut self) {
        panic!("Not yet implemented!")
    }
    fn current_mode_is_priviledged(&mut self) -> bool {
        (self.execution_mode == ExecutionMode::Handler) || (!self.thread_mode_is_unprivileged)
    }

    const ADC_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const ADC_REGISTER_T1_VAL: u16 = 0b0100_0001_0100_0000;
    fn adc_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let rdn = instruction0 & 0b111;
        let rm = (instruction0 >> 3) & 0b111;

        let op1 = self.register_read(rdn);
        let op2 = self.register_read(rm);

        let (result, carry, overflow) = add_with_carry(op1, op2, self.carry);

        self.register_write(rdn, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;
        self.overflow = overflow;

        Ok(None)
    }

    // const ADD_IMMEDIATE_T1_MSK: u16 = 0b1111_1110_0000_0000;
    // const ADD_IMMEDIATE_T1_VAL: u16 = 0b0001_1100_0000_0000;
    fn add_immediate_t1(&mut self, instruction0: u16) -> InstructionResult {
        let rd = instruction0 & 0b111;
        let rn = (instruction0 >> 3) & 0b111;
        let imm3 = ((instruction0 >> 6) & 0b111) as u32;

        let op1 = self.register_read(rn);

        let (result, carry, overflow) = add_with_carry(op1, imm3, false);

        self.register_write(rd, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;
        self.overflow = overflow;

        Ok(None)
    }

    // const ADD_IMMEDIATE_T2_MSK: u16 = 0b1111_1000_0000_0000;
    // const ADD_IMMEDIATE_T2_VAL: u16 = 0b0011_0000_0000_0000;
    fn add_immediate_t2(&mut self, instruction0: u16) -> InstructionResult {
        let imm8 = (instruction0 & 0b1111_1111) as u32;
        let rdn = (instruction0 >> 8) & 0b111;

        let op1 = self.register_read(rdn);

        let (result, carry, overflow) = add_with_carry(op1, imm8, false);

        self.register_write(rdn, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;
        self.overflow = overflow;

        Ok(None)
    }

    // const ADD_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
    // const ADD_REGISTER_T1_VAL: u16 = 0b0001_1000_0000_0000;
    fn add_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let rd = instruction0 & 0b111;
        let rn = (instruction0 >> 3) & 0b111;
        let rm = (instruction0 >> 6) & 0b111;

        let op1 = self.register_read(rn);
        let op2 = self.register_read(rm);

        let (result, carry, overflow) = add_with_carry(op1, op2, false);

        self.register_write(rd, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;
        self.overflow = overflow;

        Ok(None)
    }

    // These two instructions are made redundant by the third.
    // const ADD_SP_PLUS_REGISTER_T1_MSK: u16 = 0b1111_1111_0111_1000;
    // const ADD_SP_PLUS_REGISTER_T1_VAL: u16 = 0b0100_0100_0110_1000;
    // const ADD_SP_PLUS_REGISTER_T2_MSK: u16 = 0b1111_1111_1000_0111;
    // const ADD_SP_PLUS_REGISTER_T2_VAL: u16 = 0b0100_0100_1000_0101;
    // const ADD_REGISTER_T2_MSK: u16 = 0b1111_1111_0000_0000;
    // const ADD_REGISTER_T2_VAL: u16 = 0b0100_0100_0000_0000;
    fn add_register_t2(&mut self, instruction0: u16) -> InstructionResult {
        let rdn = ((instruction0 >> 4) & 0b1000) | (instruction0 & 0b111);
        let rm = (instruction0 >> 3) & 0b1111;

        if (rdn == 15) && (rm == 15) {
            return Err(InstructionError::Unpredictable);
        }

        let op1 = self.register_read(rdn);
        let op2 = self.register_read(rm);

        let (result, _, _) = add_with_carry(op1, op2, false);

        if rdn == 15 {
            self.alu_write_pc(result);
            Ok(Some(InstructionSignal::Branch))
        } else {
            self.register_write(rdn, result);
            Ok(None)
        }
    }

    // const ADD_SP_PLUS_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
    // const ADD_SP_PLUS_IMMEDIATE_T1_VAL: u16 = 0b1010_1000_0000_0000;
    fn add_sp_plus_immediate_t1(&mut self, instruction0: u16) -> InstructionResult {
        let imm8 = (instruction0 & 0b1111_1111) as u32;
        let rd = (instruction0 >> 8) & 0b111;

        let op1 = self.read_stack_pointer();
        let op2 = imm8 << 2;

        let (result, _, _) = add_with_carry(op1, op2, false);

        self.register_write(rd, result);

        Ok(None)
    }
    const ADD_SP_PLUS_IMMEDIATE_T2_MSK: u16 = 0b1111_1111_1000_0000;
    const ADD_SP_PLUS_IMMEDIATE_T2_VAL: u16 = 0b1011_0000_0000_0000;
    fn add_sp_plus_immediate_t2(&mut self, instruction0: u16) -> InstructionResult {
        let imm7 = (instruction0 & 0b111_1111) as u32;

        let op1 = self.read_stack_pointer();
        let op2 = imm7 << 2;

        let (result, _, _) = add_with_carry(op1, op2, false);

        self.write_stack_pointer(result);

        Ok(None)
    }

    // const ADR_T1_MSK: u16 = 0b1111_1000_0000_0000;
    // const ADR_T1_VAL: u16 = 0b1010_0000_0000_0000;
    fn adr_t1(&mut self, instruction0: u16) -> InstructionResult {
        let imm8 = (instruction0 & 0b1111_1111) as u32;
        let rd = (instruction0 >> 8) & 0b111;

        let op1 = self.read_word_aligned_program_counter();
        let op2 = imm8 << 2;

        let (result, _, _) = add_with_carry(op1, op2, false);

        self.register_write(rd, result);

        Ok(None)
    }

    const AND_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const AND_REGISTER_T1_VAL: u16 = 0b0100_0000_0000_0000;
    fn and_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let rdn = instruction0 & 0b111;
        let rm = (instruction0 >> 3) & 0b111;

        let op1 = self.register_read(rdn);
        let op2 = self.register_read(rm);

        let result = op1 & op2;

        self.register_write(rdn, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = false;

        Ok(None)
    }

    // const ASR_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
    // const ASR_IMMEDIATE_T1_VAL: u16 = 0b0001_0000_0000_0000;
    fn asr_immediate_t1(&mut self, instruction0: u16) -> InstructionResult {
        let rd = instruction0 & 0b111;
        let rm = (instruction0 >> 3) & 0b111;
        let imm5 = ((instruction0 >> 6) & 0b1_1111) as u32;

        let op1 = self.register_read(rm);
        let shift_t = ShiftType::Asr;
        let shift_n = decode_imm_shift(shift_t, imm5);

        let (result, carry) = shift_c(op1, shift_t, shift_n, self.carry);

        self.register_write(rd, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;

        Ok(None)
    }

    const ASR_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const ASR_REGISTER_T1_VAL: u16 = 0b0100_0001_0000_0000;
    fn asr_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let rdn = instruction0 & 0b111;
        let rm = (instruction0 >> 3) & 0b111;

        let op1 = self.register_read(rdn);
        let op2 = self.register_read(rm);
        let shift_t = ShiftType::Asr;
        let shift_n = op2 & 0b1111_1111;

        let (result, carry) = shift_c(op1, shift_t, shift_n, self.carry);

        self.register_write(rdn, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;

        Ok(None)
    }

    // More overlapping instructions. Branch T1 covers the SVC T1 and Undefined T1
    // instructions.
    // const SVC_T1_MSK: u16 = 0b1111_1111_0000_0000;
    // const SVC_T1_VAL: u16 = 0b1101_1111_0000_0000;
    // const UDF_T1_MSK: u16 = 0b1111_1111_0000_0000;
    // const UDF_T1_VAL: u16 = 0b1101_1110_0000_0000;
    // const B_T1_MSK: u16 = 0b1111_0000_0000_0000;
    // const B_T1_VAL: u16 = 0b1101_0000_0000_0000;
    fn b_t1(&mut self, instruction0: u16) -> InstructionResult {
        let imm8 = instruction0 & 0b1111_1111;
        let cond = (instruction0 >> 8) & 0b1111;

        let branch = match cond {
            // Equal
            0b0000 => self.zero,
            // Not equal
            0b0001 => !self.zero,
            // Carry set
            0b0010 => self.carry,
            // Carry clear
            0b0011 => !self.carry,
            // Minus, negative
            0b0100 => self.negative,
            // Plus, postive or zero
            0b0101 => !self.negative,
            // Overflow
            0b0110 => self.overflow,
            // No overflow
            0b0111 => !self.overflow,
            // Unsigned higher
            0b1000 => self.carry && !self.zero,
            // Unsigned lower or same
            0b1001 => !self.carry || self.zero,
            // Signed greater than or equal
            0b1010 => self.negative == self.overflow,
            // Signed less than
            0b1011 => self.negative != self.overflow,
            // Signed greater than
            0b1100 => !self.zero && (self.negative == self.overflow),
            // Signed less than or equal
            0b1101 => self.zero && (self.negative != self.overflow),
            0b1110 => return Err(InstructionError::Undefined),
            0b1111 => return Ok(Some(InstructionSignal::SupervisorCall)),
            _ => panic!(),
        };

        if branch {
            let imm32 = sign_extend(imm8 as u32, 8) << 1;
            let address = self
                .read_program_counter()
                .checked_add_signed(imm32)
                .unwrap();
            self.branch_write_pc(address);

            Ok(Some(InstructionSignal::Branch))
        } else {
            Ok(None)
        }
    }

    // const B_T2_MSK: u16 = 0b1111_1000_0000_0000;
    // const B_T2_VAL: u16 = 0b1110_0000_0000_0000;
    fn b_t2(&mut self, instruction0: u16) -> InstructionResult {
        let imm11 = instruction0 & 0b111_1111_1111;

        let imm32 = sign_extend(imm11 as u32, 11) << 1;
        let address = self
            .read_program_counter()
            .checked_add_signed(imm32)
            .unwrap();
        self.branch_write_pc(address);

        Ok(Some(InstructionSignal::Branch))
    }

    const BIC_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const BIC_REGISTER_T1_VAL: u16 = 0b0100_0011_1000_0000;
    fn bic_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let rdn = instruction0 & 0b111;
        let rm = (instruction0 >> 3) & 0b111;

        let op1 = self.register_read(rdn);
        let op2 = self.register_read(rm);

        let result = op1 & !op2;

        self.register_write(rm, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;

        Ok(None)
    }

    // const BKPT_T1_MSK: u16 = 0b1111_1111_0000_0000;
    // const BKPT_T1_VAL: u16 = 0b1011_1110_0000_0000;
    fn bkpt_t1(&mut self) -> InstructionResult {
        Ok(Some(InstructionSignal::Breakpoint))
    }

    // const BL_T1_0_MSK: u16 = 0b1111_1000_0000_0000;
    // const BL_T1_0_VAL: u16 = 0b1111_0000_0000_0000;
    const BL_T1_1_MSK: u16 = 0b1101_0000_0000_0000;
    const BL_T1_1_VAL: u16 = 0b1101_0000_0000_0000;
    fn bl_t1(&mut self, instruction0: u16, instruction1: u16) -> InstructionResult {
        let imm11 = (instruction1 & 0b111_1111_1111) as u32;
        let j2 = ((instruction1 >> 11) & 0b1) as u32;
        let j1 = ((instruction1 >> 13) & 0b1) as u32;
        let imm10 = (instruction0 & 0b11_1111_1111) as u32;
        let s = ((instruction0 >> 10) & 0b1) as u32;

        let i1 = (!(j1 ^ s)) & 0b1;
        let i2 = (!(j2 ^ s)) & 0b1;
        let imm32 = sign_extend(
            (s << 24) | (i1 << 23) | (i2 << 22) | (imm10 << 12) | (imm11 << 1),
            25,
        );

        let next_instr_addr = self.read_program_counter();
        self.link_register = next_instr_addr | 0b1;
        let address = next_instr_addr.checked_add_signed(imm32).unwrap();
        self.branch_write_pc(address);

        Ok(Some(InstructionSignal::Branch))
    }

    const BLX_REGISTER_T1_MSK: u16 = 0b1111_1111_1000_0111;
    const BLX_REGISTER_T1_VAL: u16 = 0b0100_0111_1000_0000;
    fn blx_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let rm = (instruction0 >> 3) & 0b111;

        if rm == 15 {
            return Err(InstructionError::Unpredictable);
        }

        let target = self.register_read(rm);
        let next_instr_addr = self.read_program_counter() - 2;
        self.link_register = next_instr_addr | 0b1;
        self.blx_write_pc(target);

        Ok(Some(InstructionSignal::Branch))
    }

    const BX_T1_MSK: u16 = 0b1111_1111_1000_0111;
    const BX_T1_VAL: u16 = 0b0100_0111_0000_0000;
    fn bx_t1(&mut self, instruction0: u16) -> InstructionResult {
        let rm = (instruction0 >> 3) & 0b1111;

        if rm == 15 {
            return Err(InstructionError::Unpredictable);
        }

        let address = self.register_read(rm);
        self.bx_write_pc(address);

        Ok(Some(InstructionSignal::Branch))
    }

    const CMN_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const CMN_REGISTER_T1_VAL: u16 = 0b0100_0010_1100_0000;
    fn cmn_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let rn = instruction0 & 0b111;
        let rm = (instruction0 >> 3) & 0b111;

        let op1 = self.register_read(rn);
        let op2 = self.register_read(rm);

        let (result, carry, overflow) = add_with_carry(op1, op2, false);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;
        self.overflow = overflow;

        Ok(None)
    }

    // const CMP_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
    // const CMP_IMMEDIATE_T1_VAL: u16 = 0b0010_1000_0000_0000;
    fn cmp_immediate_t1(&mut self, instruction0: u16) -> InstructionResult {
        let imm8 = (instruction0 & 0b1111_1111) as u32;
        let rn = (instruction0 >> 8) & 0b111;

        let op1 = self.register_read(rn);

        let (result, carry, overflow) = add_with_carry(op1, !imm8, true);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;
        self.overflow = overflow;

        Ok(None)
    }

    const CMP_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const CMP_REGISTER_T1_VAL: u16 = 0b0100_0010_1000_0000;
    fn cmp_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let rn = instruction0 & 0b111;
        let rm = (instruction0 >> 3) & 0b111;

        let op1 = self.register_read(rn);
        let op2 = self.register_read(rm);

        let (result, carry, overflow) = add_with_carry(op1, !op2, true);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;
        self.overflow = overflow;

        Ok(None)
    }

    // const CMP_REGISTER_T2_MSK: u16 = 0b1111_1111_0000_0000;
    // const CMP_REGISTER_T2_VAL: u16 = 0b0100_0101_0000_0000;
    fn cmp_register_t2(&mut self, instruction0: u16) -> InstructionResult {
        let rn = ((instruction0 >> 4) & 0b1000) | (instruction0 & 0b111);
        let rm = (instruction0 >> 3) & 0b1111;

        if (rn < 8) && (rm < 8) {
            return Err(InstructionError::Unpredictable);
        }
        if (rn == 15) || (rm == 15) {
            return Err(InstructionError::Unpredictable);
        }

        let op1 = self.register_read(rn);
        let op2 = self.register_read(rm);

        let (result, carry, overflow) = add_with_carry(op1, !op2, true);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;
        self.overflow = overflow;

        Ok(None)
    }

    // const CPS_T1_MSK: u16 = 0b1111_1111_1110_1111;
    // const CPS_T1_VAL: u16 = 0b1011_0110_0110_0010;
    fn cps_t1(&mut self, instruction0: u16) -> InstructionResult {
        let im = ((instruction0 >> 4) & 0b1) == 1;

        if self.current_mode_is_priviledged() {
            self.prioritizable_interrupts_are_disabled = im;
        }

        Ok(None)
    }

    const EOR_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const EOR_REGISTER_T1_VAL: u16 = 0b0100_0000_0100_0000;
    fn eor_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let rdn = instruction0 & 0b111;
        let rm = (instruction0 >> 3) & 0b111;

        let op1 = self.register_read(rdn);
        let op2 = self.register_read(rm);

        let result = op1 ^ op2;

        self.register_write(rdn, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;

        Ok(None)
    }

    // const LDM_T1_MSK: u16 = 0b1111_1000_0000_0000;
    // const LDM_T1_VAL: u16 = 0b1100_1000_0000_0000;
    fn ldm_t1(&mut self, instruction0: u16, memory: &mut memory::Memory) -> InstructionResult {
        let register_list = instruction0 & 0b1111_1111;
        let rn = (instruction0 >> 8) & 0b111;

        if register_list == 0 {
            return Err(InstructionError::Unpredictable);
        }

        let wback = ((register_list >> rn) & 0b1) == 0;
        let mut address = self.register_read(rn);

        for rd in 0..8 {
            if ((register_list >> rd) & 0b1) == 1 {
                self.register_write(rd, memory.read_u32(address).unwrap());
                address = address.checked_add(4).unwrap();
            }
        }

        if wback {
            self.register_write(rn, address);
        }

        Ok(None)
    }

    // const LDR_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
    // const LDR_IMMEDIATE_T1_VAL: u16 = 0b0110_1000_0000_0000;
    fn ldr_immediate_t1(
        &mut self,
        instruction0: u16,
        memory: &mut memory::Memory,
    ) -> InstructionResult {
        let rt = instruction0 & 0b111;
        let rn = (instruction0 >> 3) & 0b111;
        let imm5 = (instruction0 >> 6) & 0b1_1111;

        let address = self.register_read(rn) + (imm5 << 2) as u32;

        self.register_write(rt, memory.read_u32(address).unwrap());

        Ok(None)
    }

    // const LDR_IMMEDIATE_T2_MSK: u16 = 0b1111_1000_0000_0000;
    // const LDR_IMMEDIATE_T2_VAL: u16 = 0b1001_1000_0000_0000;
    fn ldr_immediate_t2(
        &mut self,
        instruction0: u16,
        memory: &mut memory::Memory,
    ) -> InstructionResult {
        let imm8 = instruction0 & 0b1111_1111;
        let rt = (instruction0 >> 8) & 0b111;

        let address = self.read_stack_pointer() + (imm8 << 2) as u32;

        self.register_write(rt, memory.read_u32(address).unwrap());

        Ok(None)
    }

    // const LDR_LITERAL_T1_MSK: u16 = 0b1111_1000_0000_0000;
    // const LDR_LITERAL_T1_VAL: u16 = 0b0100_1000_0000_0000;
    fn ldr_literal_t1(
        &mut self,
        instruction0: u16,
        memory: &mut memory::Memory,
    ) -> InstructionResult {
        let imm8 = (instruction0 & 0b1111_1111) as u32;
        let rt = (instruction0 >> 8) & 0b111;

        let base = self.read_word_aligned_program_counter();
        let address = base + (imm8 << 2);

        self.register_write(rt, memory.read_u32(address).unwrap());

        Ok(None)
    }

    // const LDR_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
    // const LDR_REGISTER_T1_VAL: u16 = 0b0101_1000_0000_0000;
    fn ldr_register_t1(
        &mut self,
        instruction0: u16,
        memory: &mut memory::Memory,
    ) -> InstructionResult {
        let rt = instruction0 & 0b111;
        let rn = (instruction0 >> 3) & 0b111;
        let rm = (instruction0 >> 6) & 0b111;

        let address = self.register_read(rn) + self.register_read(rm);

        self.register_write(rt, memory.read_u32(address).unwrap());

        Ok(None)
    }

    // const LDRB_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
    // const LDRB_IMMEDIATE_T1_VAL: u16 = 0b0111_1000_0000_0000;
    fn ldrb_immediate_t1(
        &mut self,
        instruction0: u16,
        memory: &mut memory::Memory,
    ) -> InstructionResult {
        let rt = instruction0 & 0b111;
        let rn = (instruction0 >> 3) & 0b111;
        let imm5 = (instruction0 >> 6) & 0b1_1111;

        let address = (self.register_read(rn)) + imm5 as u32;

        self.register_write(rt, memory.read_u8(address).unwrap() as u32);

        Ok(None)
    }

    // const LDRB_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
    // const LDRB_REGISTER_T1_VAL: u16 = 0b0101_1100_0000_0000;
    fn ldrb_register_t1(
        &mut self,
        instruction0: u16,
        memory: &mut memory::Memory,
    ) -> InstructionResult {
        let rt = instruction0 & 0b111;
        let rn = (instruction0 >> 3) & 0b111;
        let rm = (instruction0 >> 6) & 0b111;

        let address = self.register_read(rn) + self.register_read(rm);

        self.register_write(rt, memory.read_u8(address).unwrap() as u32);

        Ok(None)
    }

    // const LDRH_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
    // const LDRH_IMMEDIATE_T1_VAL: u16 = 0b1000_1000_0000_0000;
    fn ldrh_immediate_t1(
        &mut self,
        instruction0: u16,
        memory: &mut memory::Memory,
    ) -> InstructionResult {
        let rt = instruction0 & 0b111;
        let rn = (instruction0 >> 3) & 0b111;
        let imm5 = (instruction0 >> 6) & 0b1_1111;

        let address = (self.register_read(rn)) + (imm5 << 1) as u32;

        self.register_write(rt, memory.read_u16(address).unwrap() as u32);

        Ok(None)
    }

    // const LDRH_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
    // const LDRH_REGISTER_T1_VAL: u16 = 0b0101_1010_0000_0000;
    fn ldrh_register_t1(
        &mut self,
        instruction0: u16,
        memory: &mut memory::Memory,
    ) -> InstructionResult {
        let rt = instruction0 & 0b111;
        let rn = (instruction0 >> 3) & 0b111;
        let rm = (instruction0 >> 6) & 0b111;

        let address = self.register_read(rn) + self.register_read(rm);

        self.register_write(rt, memory.read_u16(address).unwrap() as u32);

        Ok(None)
    }

    // const LDRSB_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
    // const LDRSB_REGISTER_T1_VAL: u16 = 0b0101_0110_0000_0000;
    fn ldrsb_register_t1(
        &mut self,
        instruction0: u16,
        memory: &mut memory::Memory,
    ) -> InstructionResult {
        let rt = instruction0 & 0b111;
        let rn = (instruction0 >> 3) & 0b111;
        let rm = (instruction0 >> 6) & 0b111;

        let address = self.register_read(rn) + self.register_read(rm);

        let data = memory.read_u8(address).unwrap() as i8;
        let data_sign_extended = data as i32;
        self.register_write(rt, data_sign_extended as u32);

        Ok(None)
    }

    // const LDRSH_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
    // const LDRSH_REGISTER_T1_VAL: u16 = 0b0101_1110_0000_0000;
    fn ldrsh_register_t1(
        &mut self,
        instruction0: u16,
        memory: &mut memory::Memory,
    ) -> InstructionResult {
        let rt = instruction0 & 0b111;
        let rn = (instruction0 >> 3) & 0b111;
        let rm = (instruction0 >> 6) & 0b111;

        let base = self.register_read(rn);
        let address = base + self.register_read(rm);

        let data = memory.read_u16(address).unwrap() as i16;
        let data_sign_extended = data as i32;
        self.register_write(rt, data_sign_extended as u32);

        Ok(None)
    }

    // More overlapping instructions. LSL immediate T1 covers all MOV register T2
    // instructions.
    // const MOV_REGISTER_T2_MSK: u16 = 0b1111_1111_1100_0000;
    // const MOV_REGISTER_T2_VAL: u16 = 0b0000_0000_0000_0000;
    // const LSL_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
    // const LSL_IMMEDIATE_T1_VAL: u16 = 0b0000_0000_0000_0000;
    fn lsl_immediate_t1(&mut self, instruction0: u16) -> InstructionResult {
        let rd = instruction0 & 0b111;
        let rm = (instruction0 >> 3) & 0b111;
        let imm5 = (instruction0 >> 6) & 0b1_1111;

        let d = rd;
        let m = rm;

        let shift_t = ShiftType::Lsl;
        let shift_n = decode_imm_shift(shift_t, imm5 as u32);
        let op1 = self.register_read(m);

        let (result, carry) = shift_c(op1, shift_t, shift_n, self.carry);

        self.register_write(d, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;

        Ok(None)
    }

    const LSL_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const LSL_REGISTER_T1_VAL: u16 = 0b0100_0000_1000_0000;
    fn lsl_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let rdn = instruction0 & 0b111;
        let rm = (instruction0 >> 3) & 0b111;

        let d = rdn;
        let n = d;
        let m = rm;

        let shift_t = ShiftType::Lsl;
        let shift_n = self.register_read(m) & 0b1111_1111;
        let op1 = self.register_read(n);

        let (result, carry) = shift_c(op1, shift_t, shift_n, self.carry);

        self.register_write(d, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;

        Ok(None)
    }

    // const LSR_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
    // const LSR_IMMEDIATE_T1_VAL: u16 = 0b0000_1000_0000_0000;
    fn lsr_immediate_t1(&mut self, instruction0: u16) -> InstructionResult {
        let rd = instruction0 & 0b111;
        let rm = (instruction0 >> 3) & 0b111;
        let imm5 = (instruction0 >> 6) & 0b1_1111;

        let d = rd;
        let m = rm;

        let shift_t = ShiftType::Lsr;
        let shift_n = decode_imm_shift(shift_t, imm5 as u32);
        let op1 = self.register_read(m);

        let (result, carry) = shift_c(op1, shift_t, shift_n, self.carry);

        self.register_write(d, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;

        Ok(None)
    }

    const LSR_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const LSR_REGISTER_T1_VAL: u16 = 0b0100_0000_1100_0000;
    fn lsr_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let rdn = instruction0 & 0b111;
        let rm = (instruction0 >> 3) & 0b111;

        let d = rdn;
        let n = d;
        let m = rm;

        let shift_t = ShiftType::Lsr;
        let shift_n = self.register_read(m) & 0b1111_1111;
        let op1 = self.register_read(n);

        let (result, carry) = shift_c(op1, shift_t, shift_n, self.carry);

        self.register_write(d, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;

        Ok(None)
    }

    // const MOV_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
    // const MOV_IMMEDIATE_T1_VAL: u16 = 0b0010_0000_0000_0000;
    fn mov_immediate_t1(&mut self, instruction0: u16) -> InstructionResult {
        let imm8 = instruction0 & 0b1111_1111;
        let rd = (instruction0 >> 8) & 0b111;

        let d = rd;
        let imm32 = imm8 as u32;

        let result = imm32;

        self.register_write(d, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;

        Ok(None)
    }

    // const MOV_REGISTER_T1_MSK: u16 = 0b1111_1111_0000_0000;
    // const MOV_REGISTER_T1_VAL: u16 = 0b0100_0110_0000_0000;
    fn mov_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let d = ((instruction0 >> 4) & 0b1000) | (instruction0 & 0b111);
        let m = (instruction0 >> 3) & 0b1111;

        let result = self.register_read(m);

        if d == 15 {
            self.branch_write_pc(result);
            Ok(Some(InstructionSignal::Branch))
        } else {
            self.register_write(d, result);
            Ok(None)
        }
    }

    const MUL_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const MUL_T1_VAL: u16 = 0b0100_0011_0100_0000;
    fn mul_t1(&mut self, instruction0: u16) -> InstructionResult {
        let d = instruction0 & 0b111;
        let n = (instruction0 >> 3) & 0b111;
        let m = d;

        let op1 = self.register_read(n) as i32;
        let op2 = self.register_read(m) as i32;

        let result = op1 * op2;

        self.register_write(d, result as u32);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;

        Ok(None)
    }

    const MVN_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const MVN_REGISTER_T1_VAL: u16 = 0b0100_0011_1100_0000;
    fn mvn_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let d = instruction0 & 0b111;
        let m = (instruction0 >> 3) & 0b111;

        let result = !self.register_read(m);

        self.register_write(d, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;

        Ok(None)
    }

    const NOP_T1_MSK: u16 = 0b1111_1111_1111_1111;
    const NOP_T1_VAL: u16 = 0b1011_1111_0000_0000;
    fn nop_t1(&mut self) -> InstructionResult {
        Ok(None)
    }

    const ORR_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const ORR_REGISTER_T1_VAL: u16 = 0b0100_0011_0000_0000;
    fn orr_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let d = instruction0 & 0b111;
        let n = d;
        let m = (instruction0 >> 3) & 0b111;

        let op1 = self.register_read(n);
        let op2 = self.register_read(m);

        let result = op1 | op2;

        self.register_write(d, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;

        Ok(None)
    }

    // const POP_T1_MSK: u16 = 0b1111_1110_0000_0000;
    // const POP_T1_VAL: u16 = 0b1011_1100_0000_0000;
    fn pop_t1(&mut self, instruction0: u16, memory: &mut memory::Memory) -> InstructionResult {
        let register_list = instruction0 & 0b1111_1111;
        let p = bit_is_set(instruction0, 8);

        if register_list == 0 {
            return Err(InstructionError::Unpredictable);
        }

        let mut address = self.read_stack_pointer();

        for register in 0..8 {
            if ((register_list >> register) & 0b1) == 1 {
                let result = memory.read_u32(address).unwrap();
                self.register_write(register, result);
                address = address.checked_add(4).unwrap();
            }
        }

        if p {
            let result = memory.read_u32(address).unwrap();
            self.load_write_pc(result);
            address = address.checked_add(4).unwrap();
            self.write_stack_pointer(address);
            Ok(Some(InstructionSignal::Branch))
        } else {
            self.write_stack_pointer(address);
            Ok(None)
        }
    }

    // const PUSH_T1_MSK: u16 = 0b1111_1110_0000_0000;
    // const PUSH_T1_VAL: u16 = 0b1011_0100_0000_0000;
    fn push_t1(&mut self, instruction0: u16, memory: &mut memory::Memory) -> InstructionResult {
        let register_list = instruction0 & 0b1111_1111;
        let m = bit_is_set(instruction0, 8);

        if register_list == 0 {
            return Err(InstructionError::Unpredictable);
        }

        let mut address = self.read_stack_pointer();

        if m {
            address = address.checked_sub(4).unwrap();
            assert_eq!(
                memory.write_u32(address, self.link_register),
                memory::MemoryResult::Ok(())
            );
        };

        for register in (0..8).rev() {
            if ((register_list >> register) & 0b1) == 1 {
                address = address.checked_sub(4).unwrap();
                let value = self.register_read(register);
                assert_eq!(
                    memory.write_u32(address, value),
                    memory::MemoryResult::Ok(())
                );
            }
        }

        self.write_stack_pointer(address);

        Ok(None)
    }

    const REV_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const REV_T1_VAL: u16 = 0b1011_1010_0000_0000;
    fn rev_t1(&mut self, instruction0: u16) -> InstructionResult {
        let d = instruction0 & 0b111;
        let m = (instruction0 >> 3) & 0b111;

        let result = self.register_read(m).swap_bytes();
        self.register_write(d, result);

        Ok(None)
    }

    const REV16_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const REV16_T1_VAL: u16 = 0b1011_1010_0100_0000;
    fn rev16_t1(&mut self, instruction0: u16) -> InstructionResult {
        let d = instruction0 & 0b111;
        let m = (instruction0 >> 3) & 0b111;

        let op1 = self.register_read(m);
        let result = ((op1 & 0xFF00_0000) >> 8)
            | ((op1 & 0x00FF_0000) << 8)
            | ((op1 & 0x0000_FF00) >> 8)
            | ((op1 & 0x0000_00FF) << 8);

        self.register_write(d, result);

        Ok(None)
    }

    const REVSH_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const REVSH_T1_VAL: u16 = 0b1011_1010_1100_0000;
    fn revsh_t1(&mut self, instruction0: u16) -> InstructionResult {
        let d = instruction0 & 0b111;
        let m = (instruction0 >> 3) & 0b111;

        let op1 = self.register_read(m);
        let result = ((op1 & 0x0000_FF00) >> 8) | ((op1 & 0x0000_00FF) << 8);
        let signed = sign_extend(result, 16);

        self.register_write(d, signed as u32);

        Ok(None)
    }

    const ROR_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const ROR_REGISTER_T1_VAL: u16 = 0b0100_0001_1100_0000;
    fn ror_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let d = instruction0 & 0b111;
        let n = d;
        let m = (instruction0 >> 3) & 0b111;

        let shift_t = ShiftType::Ror;
        let shift_n = self.register_read(m) & 0b1111_1111;
        let op1 = self.register_read(n);

        let (result, carry) = shift_c(op1, shift_t, shift_n, self.carry);

        self.register_write(d, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;

        Ok(None)
    }

    const RSB_IMMEDIATE_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const RSB_IMMEDIATE_T1_VAL: u16 = 0b0100_0010_0100_0000;
    fn rsb_immediate_t1(&mut self, instruction0: u16) -> InstructionResult {
        let d = instruction0 & 0b111;
        let n = (instruction0 >> 3) & 0b111;

        let op1 = self.register_read(n);

        let (result, carry, overflow) = add_with_carry(!op1, 0, true);

        self.register_write(d, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;
        self.overflow = overflow;

        Ok(None)
    }

    const SBC_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const SBC_REGISTER_T1_VAL: u16 = 0b0100_0001_1000_0000;
    fn sbc_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let d = instruction0 & 0b111;
        let n = d;
        let m = (instruction0 >> 3) & 0b111;

        let op1 = self.register_read(n);
        let op2 = self.register_read(m);

        let (result, carry, overflow) = add_with_carry(op1, !op2, self.carry);

        self.register_write(d, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;
        self.overflow = overflow;

        Ok(None)
    }

    const SEV_T1_MSK: u16 = 0b1111_1111_1111_1111;
    const SEV_T1_VAL: u16 = 0b1011_1111_0100_0000;
    fn sev_t1(&mut self) -> InstructionResult {
        Ok(Some(InstructionSignal::SendEvent))
    }

    // const STM_T1_MSK: u16 = 0b1111_1000_0000_0000;
    // const STM_T1_VAL: u16 = 0b1100_0000_0000_0000;
    fn stm_t1(&mut self, instruction0: u16, memory: &mut memory::Memory) -> InstructionResult {
        let register_list = instruction0 & 0b1111_1111;
        let n = (instruction0 >> 8) & 0b111;

        if register_list == 0 {
            return Err(InstructionError::Unpredictable);
        }

        assert!(!bit_is_set(register_list, n));

        let mut address = self.register_read(n);

        for register in 0..8 {
            if ((register_list >> register) & 0b1) == 1 {
                let result = self.register_read(register);
                assert_eq!(
                    memory.write_u32(address, result),
                    memory::MemoryResult::Ok(())
                );
                address = address.checked_add(4).unwrap();
            }
        }

        self.register_write(n, address);

        Ok(None)
    }

    // const STR_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
    // const STR_IMMEDIATE_T1_VAL: u16 = 0b0110_0000_0000_0000;
    fn str_immediate_t1(
        &mut self,
        instruction0: u16,
        memory: &mut memory::Memory,
    ) -> InstructionResult {
        let t = instruction0 & 0b111;
        let n = (instruction0 >> 3) & 0b111;
        let imm5 = (instruction0 >> 6) & 0b1_1111;

        let imm32 = (imm5 as u32) << 2;
        let address = self.register_read(n) + imm32;

        assert_eq!(
            memory.write_u32(address, self.register_read(t)),
            memory::MemoryResult::Ok(())
        );

        Ok(None)
    }

    // const STR_IMMEDIATE_T2_MSK: u16 = 0b1111_1000_0000_0000;
    // const STR_IMMEDIATE_T2_VAL: u16 = 0b1001_0000_0000_0000;
    fn str_immediate_t2(
        &mut self,
        instruction0: u16,
        memory: &mut memory::Memory,
    ) -> InstructionResult {
        let imm8 = instruction0 & 0b1111_1111;
        let t = (instruction0 >> 8) & 0b111;

        let imm32 = (imm8 as u32) << 2;
        let address = self.read_stack_pointer() + imm32;

        assert_eq!(
            memory.write_u32(address, self.register_read(t)),
            memory::MemoryResult::Ok(())
        );

        Ok(None)
    }

    // const STR_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
    // const STR_REGISTER_T1_VAL: u16 = 0b0101_0000_0000_0000;
    fn str_register_t1(
        &mut self,
        instruction0: u16,
        memory: &mut memory::Memory,
    ) -> InstructionResult {
        let t = instruction0 & 0b111;
        let n = (instruction0 >> 3) & 0b111;
        let m = (instruction0 >> 6) & 0b111;

        let address = self.register_read(n) + self.register_read(m);
        assert_eq!(
            memory.write_u32(address, self.register_read(t)),
            memory::MemoryResult::Ok(())
        );

        Ok(None)
    }

    // const STRB_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
    // const STRB_IMMEDIATE_T1_VAL: u16 = 0b0111_0000_0000_0000;
    fn strb_immediate_t1(
        &mut self,
        instruction0: u16,
        memory: &mut memory::Memory,
    ) -> InstructionResult {
        let t = instruction0 & 0b111;
        let n = (instruction0 >> 3) & 0b111;
        let imm32 = ((instruction0 >> 6) & 0b1_1111) as u32;

        let address = self.register_read(n) + imm32;
        assert!(memory
            .write_u8(address, self.register_read(t) as u8)
            .is_ok());

        Ok(None)
    }

    // const STRB_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
    // const STRB_REGISTER_T1_VAL: u16 = 0b0101_0100_0000_0000;
    fn strb_register_t1(
        &mut self,
        instruction0: u16,
        memory: &mut memory::Memory,
    ) -> InstructionResult {
        let t = instruction0 & 0b111;
        let n = (instruction0 >> 3) & 0b111;
        let m = (instruction0 >> 6) & 0b111;

        let address = self.register_read(n) + self.register_read(m);
        assert!(memory
            .write_u8(address, self.register_read(t) as u8)
            .is_ok());

        Ok(None)
    }

    // const STRH_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
    // const STRH_IMMEDIATE_T1_VAL: u16 = 0b1000_0000_0000_0000;
    fn strh_immediate_t1(
        &mut self,
        instruction0: u16,
        memory: &mut memory::Memory,
    ) -> InstructionResult {
        let t = instruction0 & 0b111;
        let n = (instruction0 >> 3) & 0b111;
        let imm32 = ((instruction0 >> 6) & 0b1_1111) as u32;

        let address = self.register_read(n) + imm32;
        assert!(memory
            .write_u16(address, self.register_read(t) as u16)
            .is_ok());

        Ok(None)
    }

    // const STRH_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
    // const STRH_REGISTER_T1_VAL: u16 = 0b0101_0010_0000_0000;
    fn strh_register_t1(
        &mut self,
        instruction0: u16,
        memory: &mut memory::Memory,
    ) -> InstructionResult {
        let t = instruction0 & 0b111;
        let n = (instruction0 >> 3) & 0b111;
        let m = (instruction0 >> 6) & 0b111;

        let address = self.register_read(n) + self.register_read(m);
        assert!(memory
            .write_u16(address, self.register_read(t) as u16)
            .is_ok());

        Ok(None)
    }

    // const SUB_IMMEDIATE_T1_MSK: u16 = 0b1111_1110_0000_0000;
    // const SUB_IMMEDIATE_T1_VAL: u16 = 0b0001_1110_0000_0000;
    fn sub_immediate_t1(&mut self, instruction0: u16) -> InstructionResult {
        let d = instruction0 & 0b111;
        let n = (instruction0 >> 3) & 0b111;
        let imm32 = ((instruction0 >> 6) & 0b111) as u32;

        let op1 = self.register_read(n);

        let (result, carry, overflow) = add_with_carry(op1, !imm32, true);

        self.register_write(d, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;
        self.overflow = overflow;

        Ok(None)
    }

    // const SUB_IMMEDIATE_T2_MSK: u16 = 0b1111_1000_0000_0000;
    // const SUB_IMMEDIATE_T2_VAL: u16 = 0b0011_1000_0000_0000;
    fn sub_immediate_t2(&mut self, instruction0: u16) -> InstructionResult {
        let d = (instruction0 >> 8) & 0b111;
        let n = d;
        let imm32 = (instruction0 & 0b1111_1111) as u32;

        let op1 = self.register_read(n);
        let (result, carry, overflow) = add_with_carry(op1, !imm32, true);
        self.register_write(d, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;
        self.overflow = overflow;

        Ok(None)
    }

    // const SUB_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
    // const SUB_REGISTER_T1_VAL: u16 = 0b0001_1010_0000_0000;
    fn sub_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let d = instruction0 & 0b111;
        let n = (instruction0 >> 3) & 0b111;
        let m = (instruction0 >> 6) & 0b111;

        let op1 = self.register_read(n);
        let op2 = self.register_read(m);
        let (result, carry, overflow) = add_with_carry(op1, !op2, true);
        self.register_write(d, result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;
        self.overflow = overflow;

        Ok(None)
    }

    const SUB_SP_MINUS_IMMEDIATE_T1_MSK: u16 = 0b1111_1111_1000_0000;
    const SUB_SP_MINUS_IMMEDIATE_T1_VAL: u16 = 0b1011_0000_1000_0000;
    fn sub_sp_minus_immediate_t1(&mut self, instruction0: u16) -> InstructionResult {
        let imm32 = ((instruction0 & 0b111_1111) as u32) << 2;

        let op1 = self.read_stack_pointer();
        let (result, carry, overflow) = add_with_carry(op1, !imm32, true);
        self.write_stack_pointer(result);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;
        self.carry = carry;
        self.overflow = overflow;

        Ok(None)
    }
    const SXTB_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const SXTB_T1_VAL: u16 = 0b1011_0010_0100_0000;
    fn sxtb_t1(&mut self, instruction0: u16) -> InstructionResult {
        let d = instruction0 & 0b111;
        let m = (instruction0 >> 3) & 0b111;

        let op1 = self.register_read(m);
        let signed = ((op1 as i8) as i32) as u32;
        self.register_write(d, signed);

        Ok(None)
    }

    const SXTH_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const SXTH_T1_VAL: u16 = 0b1011_0010_0000_0000;
    fn sxth_t1(&mut self, instruction0: u16) -> InstructionResult {
        let d = instruction0 & 0b111;
        let m = (instruction0 >> 3) & 0b111;

        let op1 = self.register_read(m);
        let signed = ((op1 as i16) as i32) as u32;
        self.register_write(d, signed);

        Ok(None)
    }

    const TST_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const TST_REGISTER_T1_VAL: u16 = 0b0100_0010_0000_0000;
    fn tst_register_t1(&mut self, instruction0: u16) -> InstructionResult {
        let n = instruction0 & 0b111;
        let m = (instruction0 >> 3) & 0b111;

        let result = self.register_read(n) & self.register_read(m);

        self.negative = (result >> 31) == 1;
        self.zero = result == 0;

        Ok(None)
    }

    const UXTB_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const UXTB_T1_VAL: u16 = 0b1011_0010_1100_0000;
    fn uxtb_t1(&mut self, instruction0: u16) -> InstructionResult {
        let d = instruction0 & 0b111;
        let m = (instruction0 >> 3) & 0b111;

        self.register_write(d, self.register_read(m) & 0x0000_00FF);

        Ok(None)
    }

    const UXTH_T1_MSK: u16 = 0b1111_1111_1100_0000;
    const UXTH_T1_VAL: u16 = 0b1011_0010_1000_0000;
    fn uxth_t1(&mut self, instruction0: u16) -> InstructionResult {
        let d = instruction0 & 0b111;
        let m = (instruction0 >> 3) & 0b111;

        self.register_write(d, self.register_read(m) & 0x0000_FFFF);

        Ok(None)
    }

    const WFE_T1_MSK: u16 = 0b1111_1111_1111_1111;
    const WFE_T1_VAL: u16 = 0b1011_1111_0010_0000;
    fn wfe_t1(&mut self) -> InstructionResult {
        Ok(Some(InstructionSignal::WaitForEvent))
    }
    const WFI_T1_MSK: u16 = 0b1111_1111_1111_1111;
    const WFI_T1_VAL: u16 = 0b1011_1111_0011_0000;
    fn wfi_t1(&mut self) -> InstructionResult {
        Ok(Some(InstructionSignal::WaitForInterrupt))
    }
    const YEILD_T1_MSK: u16 = 0b1111_1111_1111_1111;
    const YEILD_T1_VAL: u16 = 0b1011_1111_0001_0000;
    fn yeild_t1(&mut self) -> InstructionResult {
        Ok(Some(InstructionSignal::Yeild))
    }

    // const DMB_T1_0_MSK: u16 = 0b1111_1111_1111_1111;
    // const DMB_T1_0_VAL: u16 = 0b1111_0011_1011_1111;
    const DMB_T1_1_MSK: u16 = 0b1111_1111_1111_0000;
    const DMB_T1_1_VAL: u16 = 0b1000_1111_0101_0000;
    fn dmb_t1(&mut self) -> InstructionResult {
        Ok(Some(InstructionSignal::DataMemoryBarrier))
    }

    // const DSB_T1_0_MSK: u16 = 0b1111_1111_1111_1111;
    // const DSB_T1_0_VAL: u16 = 0b1111_0011_1011_1111;
    const DSB_T1_1_MSK: u16 = 0b1111_1111_1111_0000;
    const DSB_T1_1_VAL: u16 = 0b1000_1111_0100_0000;
    fn dsb_t1(&mut self) -> InstructionResult {
        Ok(Some(InstructionSignal::DataSynchronizationBarrier))
    }

    // const ISB_T1_0_MSK: u16 = 0b1111_1111_1111_1111;
    // const ISB_T1_0_VAL: u16 = 0b1111_0011_1011_1111;
    const ISB_T1_1_MSK: u16 = 0b1111_1111_1111_0000;
    const ISB_T1_1_VAL: u16 = 0b1000_1111_0110_0000;
    fn isb_t1(&mut self) -> InstructionResult {
        Ok(Some(InstructionSignal::InstructionSynchronizationBarrier))
    }

    // const MRS_T1_0_MSK: u16 = 0b1111_1111_1111_1111;
    // const MRS_T1_0_VAL: u16 = 0b1111_0011_1110_1111;
    const MRS_T1_1_MSK: u16 = 0b1111_0000_0000_0000;
    const MRS_T1_1_VAL: u16 = 0b1000_0000_0000_0000;
    fn mrs_t1(&mut self, instruction1: u16) -> InstructionResult {
        let sysm = instruction1 & 0b1111_1111;
        let d = (instruction1 >> 8) & 0b1111;

        if (d == 13) || (d == 15) {
            return Err(InstructionError::Unpredictable);
        }

        if !matches!(sysm, 0..=3 | 5..=9 | 16 | 20) {
            return Err(InstructionError::Unpredictable);
        }

        let mut result = 0u32;

        match (sysm >> 3) & 0b1_1111 {
            0b0_0000 => {
                if bit_is_set(sysm, 0) {
                    result |= self.exception_number & 0b1_1111_1111;
                }
                // Thumb bit always reads a 0.
                // if bit_is_set(sysm, 1) && self.thumb_mode_is_enabled {
                //     result |= 1 << 24;
                // }
                if !bit_is_set(sysm, 2) {
                    result |= ((self.overflow as u32) << 28)
                        | ((self.carry as u32) << 29)
                        | ((self.zero as u32) << 30)
                        | ((self.negative as u32) << 31);
                }
            }
            0b0_0001 => {
                if self.current_mode_is_priviledged() {
                    match sysm & 0b111 {
                        0b000 => result = self.stack_pointer_main,
                        0b001 => result = self.stack_pointer_process,
                        _ => panic!(),
                    }
                }
            }
            0b0_0010 => match sysm & 0b111 {
                0b000 => {
                    if self.current_mode_is_priviledged()
                        && self.prioritizable_interrupts_are_disabled
                    {
                        result = 1;
                    }
                }
                0b100 => {
                    if self.thread_mode_is_unprivileged {
                        result |= 1;
                    }
                    if self.stack_pointer_select == crate::cortexm0p::StackType::Process {
                        result |= 1 << 1;
                    }
                }
                _ => panic!(),
            },
            _ => panic!(),
        }

        self.register_write(d, result);

        Ok(None)
    }

    // const MSR_REGISTER_T1_0_MSK: u16 = 0b1111_1111_1111_0000;
    // const MSR_REGISTER_T1_0_VAL: u16 = 0b1111_0011_1000_0000;
    const MSR_REGISTER_T1_1_MSK: u16 = 0b1111_1111_0000_0000;
    const MSR_REGISTER_T1_1_VAL: u16 = 0b1000_1000_0000_0000;
    fn msr_register_t1(&mut self, instruction0: u16, instruction1: u16) -> InstructionResult {
        let n = instruction0 & 0b1111;
        let sysm = instruction1 & 0b1111_1111;

        if (n == 13) || (n == 15) {
            return Err(InstructionError::Unpredictable);
        }

        if !matches!(sysm, 0..=3 | 5..=9 | 16 | 20) {
            return Err(InstructionError::Unpredictable);
        }

        let value = self.register_read(n);

        match (sysm >> 3) & 0b1_1111 {
            0b0_0000 => {
                if !bit_is_set(sysm, 2) {
                    self.overflow = bit_is_set(value, 28);
                    self.carry = bit_is_set(value, 29);
                    self.zero = bit_is_set(value, 30);
                    self.negative = bit_is_set(value, 31);
                }
            }
            0b0_0001 => {
                if self.current_mode_is_priviledged() {
                    match sysm & 0b111 {
                        0b000 => self.stack_pointer_main = value & 0xFFFF_FFFC,
                        0b001 => self.stack_pointer_process = value & 0xFFFF_FFFC,
                        _ => panic!(),
                    }
                }
            }
            0b0_0010 => {
                if self.current_mode_is_priviledged() {
                    match sysm & 0b111 {
                        0b000 => self.prioritizable_interrupts_are_disabled = bit_is_set(value, 0),
                        0b100 => {
                            self.thread_mode_is_unprivileged = bit_is_set(value, 0);
                            if self.execution_mode == crate::cortexm0p::ExecutionMode::Thread {
                                if bit_is_set(value, 1) {
                                    self.select_stack_pointer(crate::cortexm0p::StackType::Process);
                                } else {
                                    self.select_stack_pointer(crate::cortexm0p::StackType::Main);
                                }
                            }
                        }
                        _ => panic!(),
                    }
                }
            }
            _ => panic!(),
        }

        Ok(None)
    }

    // const UDF_T2_0_MSK: u16 = 0b1111_1111_1111_0000;
    // const UDF_T2_0_VAL: u16 = 0b1111_0111_1111_0000;
    const UDF_T2_1_MSK: u16 = 0b1111_0000_0000_0000;
    const UDF_T2_1_VAL: u16 = 0b1010_0000_0000_0000;
    fn udf_t2(&mut self) -> InstructionResult {
        Err(InstructionError::Undefined)
    }
}
