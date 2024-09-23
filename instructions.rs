const ADC_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
const ADC_REGISTER_T1_VAL: u16 = 0b0100_0001_0100_0000;
fn adc_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const ADD_IMMEDIATE_T1_MSK: u16 = 0b1111_1110_0000_0000;
const ADD_IMMEDIATE_T1_VAL: u16 = 0b0001_1100_0000_0000;
fn add_immediate_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const ADD_IMMEDIATE_T2_MSK: u16 = 0b1111_1000_0000_0000;
const ADD_IMMEDIATE_T2_VAL: u16 = 0b0011_0000_0000_0000;
fn add_immediate_t2(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const ADD_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
const ADD_REGISTER_T1_VAL: u16 = 0b0001_1000_0000_0000;
fn add_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const ADD_REGISTER_T2_MSK: u16 = 0b1111_1111_0000_0000;
const ADD_REGISTER_T2_VAL: u16 = 0b0100_0100_0000_0000;
fn add_register_t2(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const ADD_SP_PLUS_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
const ADD_SP_PLUS_IMMEDIATE_T1_VAL: u16 = 0b1010_1000_0000_0000;
fn add_sp_plus_immediate_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const ADD_SP_PLUS_IMMEDIATE_T2_MSK: u16 = 0b1111_1111_1000_0000;
const ADD_SP_PLUS_IMMEDIATE_T2_VAL: u16 = 0b1011_0000_0000_0000;
fn add_sp_plus_immediate_t2(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const ADD_SP_PLUS_REGISTER_T1_MSK: u16 = 0b1111_1111_0111_1000;
const ADD_SP_PLUS_REGISTER_T1_VAL: u16 = 0b0100_0100_0110_1000;
fn add_sp_plus_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const ADD_SP_PLUS_REGISTER_T2_MSK: u16 = 0b1111_1111_1000_0111;
const ADD_SP_PLUS_REGISTER_T2_VAL: u16 = 0b0100_0100_1000_0101;
fn add_sp_plus_register_t2(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const ADR_T1_MSK: u16 = 0b1111_1000_0000_0000;
const ADR_T1_VAL: u16 = 0b1010_0000_0000_0000;
fn adr_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const AND_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
const AND_REGISTER_T1_VAL: u16 = 0b0100_0000_0000_0000;
fn and_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const ASR_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
const ASR_IMMEDIATE_T1_VAL: u16 = 0b0001_0000_0000_0000;
fn asr_immediate_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const ASR_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
const ASR_REGISTER_T1_VAL: u16 = 0b0100_0001_0000_0000;
fn asr_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const B_T1_MSK: u16 = 0b1111_0000_0000_0000;
const B_T1_VAL: u16 = 0b1101_0000_0000_0000;
fn b_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const B_T2_MSK: u16 = 0b1111_1000_0000_0000;
const B_T2_VAL: u16 = 0b1110_0000_0000_0000;
fn b_t2(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const BIC_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
const BIC_REGISTER_T1_VAL: u16 = 0b0100_0011_1000_0000;
fn bic_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const BKPT_T1_MSK: u16 = 0b1111_1111_0000_0000;
const BKPT_T1_VAL: u16 = 0b1011_1110_0000_0000;
fn bkpt_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const BL_T1_MSK: u16 = 0b1111_1000_0000_00001101000000000000;
const BL_T1_VAL: u16 = 0b1111_0000_0000_00001101000000000000;
fn bl_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const BLX_REGISTER_T1_MSK: u16 = 0b1111_1111_1000_0111;
const BLX_REGISTER_T1_VAL: u16 = 0b0100_0111_1000_0000;
fn blx_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const BX_T1_MSK: u16 = 0b1111_1111_1000_0111;
const BX_T1_VAL: u16 = 0b0100_0111_0000_0000;
fn bx_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const CMN_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
const CMN_REGISTER_T1_VAL: u16 = 0b0100_0010_1100_0000;
fn cmn_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const CMP_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
const CMP_IMMEDIATE_T1_VAL: u16 = 0b0010_1000_0000_0000;
fn cmp_immediate_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const CMP_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
const CMP_REGISTER_T1_VAL: u16 = 0b0100_0010_1000_0000;
fn cmp_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const CMP_REGISTER_T2_MSK: u16 = 0b1111_1111_0000_0000;
const CMP_REGISTER_T2_VAL: u16 = 0b0100_0101_0000_0000;
fn cmp_register_t2(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const CPS_T1_MSK: u16 = 0b1111_1111_1110_1111;
const CPS_T1_VAL: u16 = 0b1011_0110_0110_0010;
fn cps_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const DMB_T1_MSK: u16 = 0b1111_1111_1111_11111111111111110000;
const DMB_T1_VAL: u16 = 0b1111_0011_1011_11111000111101010000;
fn dmb_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const DSB_T1_MSK: u16 = 0b1111_1111_1111_11111111111111110000;
const DSB_T1_VAL: u16 = 0b1111_0011_1011_11111000111101000000;
fn dsb_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const EOR_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
const EOR_REGISTER_T1_VAL: u16 = 0b0100_0000_0100_0000;
fn eor_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const ISB_T1_MSK: u16 = 0b1111_1111_1111_11111111111111110000;
const ISB_T1_VAL: u16 = 0b1111_0011_1011_11111000111101100000;
fn isb_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const LDM_T1_MSK: u16 = 0b1111_1000_0000_0000;
const LDM_T1_VAL: u16 = 0b1100_1000_0000_0000;
fn ldm_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const LDR_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
const LDR_IMMEDIATE_T1_VAL: u16 = 0b0110_1000_0000_0000;
fn ldr_immediate_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const LDR_IMMEDIATE_T2_MSK: u16 = 0b1111_1000_0000_0000;
const LDR_IMMEDIATE_T2_VAL: u16 = 0b1001_1000_0000_0000;
fn ldr_immediate_t2(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const LDR_LITERAL_T1_MSK: u16 = 0b1111_1000_0000_0000;
const LDR_LITERAL_T1_VAL: u16 = 0b0100_1000_0000_0000;
fn ldr_literal_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const LDR_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
const LDR_REGISTER_T1_VAL: u16 = 0b0101_1000_0000_0000;
fn ldr_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const LDRB_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
const LDRB_IMMEDIATE_T1_VAL: u16 = 0b0111_1000_0000_0000;
fn ldrb_immediate_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const LDRB_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
const LDRB_REGISTER_T1_VAL: u16 = 0b0101_1100_0000_0000;
fn ldrb_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const LDRH_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
const LDRH_IMMEDIATE_T1_VAL: u16 = 0b1000_1000_0000_0000;
fn ldrh_immediate_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const LDRH_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
const LDRH_REGISTER_T1_VAL: u16 = 0b0101_1010_0000_0000;
fn ldrh_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const LDRSB_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
const LDRSB_REGISTER_T1_VAL: u16 = 0b0101_0110_0000_0000;
fn ldrsb_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const LDRSH_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
const LDRSH_REGISTER_T1_VAL: u16 = 0b0101_1110_0000_0000;
fn ldrsh_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const LSL_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
const LSL_IMMEDIATE_T1_VAL: u16 = 0b0000_0000_0000_0000;
fn lsl_immediate_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const LSL_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
const LSL_REGISTER_T1_VAL: u16 = 0b0100_0000_1000_0000;
fn lsl_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const LSR_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
const LSR_IMMEDIATE_T1_VAL: u16 = 0b0000_1000_0000_0000;
fn lsr_immediate_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const LSR_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
const LSR_REGISTER_T1_VAL: u16 = 0b0100_0000_1100_0000;
fn lsr_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const MOV_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
const MOV_IMMEDIATE_T1_VAL: u16 = 0b0010_0000_0000_0000;
fn mov_immediate_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const MOV_REGISTER_T1_MSK: u16 = 0b1111_1111_0000_0000;
const MOV_REGISTER_T1_VAL: u16 = 0b0100_0110_0000_0000;
fn mov_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const MOV_REGISTER_T2_MSK: u16 = 0b1111_1111_1100_0000;
const MOV_REGISTER_T2_VAL: u16 = 0b0000_0000_0000_0000;
fn mov_register_t2(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const MRS_T1_MSK: u16 = 0b1111_1111_1111_11111111000000000000;
const MRS_T1_VAL: u16 = 0b1111_0011_1110_11111000000000000000;
fn mrs_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const MSR_REGISTER_T1_MSK: u16 = 0b1111_1111_1111_00001111111100000000;
const MSR_REGISTER_T1_VAL: u16 = 0b1111_0011_1000_00001000100000000000;
fn msr_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const MUL_T1_MSK: u16 = 0b1111_1111_1100_0000;
const MUL_T1_VAL: u16 = 0b0100_0011_0100_0000;
fn mul_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const MVN_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
const MVN_REGISTER_T1_VAL: u16 = 0b0100_0011_1100_0000;
fn mvn_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const NOP_T1_MSK: u16 = 0b1111_1111_1111_1111;
const NOP_T1_VAL: u16 = 0b1011_1111_0000_0000;
fn nop_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const ORR_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
const ORR_REGISTER_T1_VAL: u16 = 0b0100_0011_0000_0000;
fn orr_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const POP_T1_MSK: u16 = 0b1111_1110_0000_0000;
const POP_T1_VAL: u16 = 0b1011_1100_0000_0000;
fn pop_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const PUSH_T1_MSK: u16 = 0b1111_1110_0000_0000;
const PUSH_T1_VAL: u16 = 0b1011_0100_0000_0000;
fn push_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const REV_T1_MSK: u16 = 0b1111_1111_1100_0000;
const REV_T1_VAL: u16 = 0b1011_1010_0000_0000;
fn rev_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const REV16_T1_MSK: u16 = 0b1111_1111_1100_0000;
const REV16_T1_VAL: u16 = 0b1011_1010_0100_0000;
fn rev16_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const REVSH_T1_MSK: u16 = 0b1111_1111_1100_0000;
const REVSH_T1_VAL: u16 = 0b1011_1010_1100_0000;
fn revsh_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const ROR_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
const ROR_REGISTER_T1_VAL: u16 = 0b0100_0001_1100_0000;
fn ror_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const RSB_IMMEDIATE_T1_MSK: u16 = 0b1111_1111_1100_0000;
const RSB_IMMEDIATE_T1_VAL: u16 = 0b0100_0010_0100_0000;
fn rsb_immediate_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const SBC_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
const SBC_REGISTER_T1_VAL: u16 = 0b0100_0001_1000_0000;
fn sbc_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const SEV_T1_MSK: u16 = 0b1111_1111_1111_1111;
const SEV_T1_VAL: u16 = 0b1011_1111_0100_0000;
fn sev_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const STM_T1_MSK: u16 = 0b1111_1000_0000_0000;
const STM_T1_VAL: u16 = 0b1100_0000_0000_0000;
fn stm_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const STR_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
const STR_IMMEDIATE_T1_VAL: u16 = 0b0110_0000_0000_0000;
fn str_immediate_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const STR_IMMEDIATE_T2_MSK: u16 = 0b1111_1000_0000_0000;
const STR_IMMEDIATE_T2_VAL: u16 = 0b1001_0000_0000_0000;
fn str_immediate_t2(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const STR_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
const STR_REGISTER_T1_VAL: u16 = 0b0101_0000_0000_0000;
fn str_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const STRB_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
const STRB_IMMEDIATE_T1_VAL: u16 = 0b0111_0000_0000_0000;
fn strb_immediate_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const STRB_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
const STRB_REGISTER_T1_VAL: u16 = 0b0101_0100_0000_0000;
fn strb_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const STRH_IMMEDIATE_T1_MSK: u16 = 0b1111_1000_0000_0000;
const STRH_IMMEDIATE_T1_VAL: u16 = 0b1000_0000_0000_0000;
fn strh_immediate_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const STRG_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
const STRG_REGISTER_T1_VAL: u16 = 0b0101_0010_0000_0000;
fn strg_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const SUB_IMMEDIATE_T1_MSK: u16 = 0b1111_1110_0000_0000;
const SUB_IMMEDIATE_T1_VAL: u16 = 0b0001_1110_0000_0000;
fn sub_immediate_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const SUB_IMMEDIATE_T2_MSK: u16 = 0b1111_1000_0000_0000;
const SUB_IMMEDIATE_T2_VAL: u16 = 0b0011_1000_0000_0000;
fn sub_immediate_t2(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const SUB_REGISTER_T1_MSK: u16 = 0b1111_1110_0000_0000;
const SUB_REGISTER_T1_VAL: u16 = 0b0001_1010_0000_0000;
fn sub_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const SUB_SP_MINUS_IMMEDIATE_T1_MSK: u16 = 0b1111_1111_1000_0000;
const SUB_SP_MINUS_IMMEDIATE_T1_VAL: u16 = 0b1011_0000_1000_0000;
fn sub_sp_minus_immediate_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const SVC_T1_MSK: u16 = 0b1111_1111_0000_0000;
const SVC_T1_VAL: u16 = 0b1101_1111_0000_0000;
fn svc_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const SXTB_T1_MSK: u16 = 0b1111_1111_1100_0000;
const SXTB_T1_VAL: u16 = 0b1011_0010_0100_0000;
fn sxtb_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const SXTH_T1_MSK: u16 = 0b1111_1111_1100_0000;
const SXTH_T1_VAL: u16 = 0b1011_0010_0000_0000;
fn sxth_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const TST_REGISTER_T1_MSK: u16 = 0b1111_1111_1100_0000;
const TST_REGISTER_T1_VAL: u16 = 0b0100_0010_0000_0000;
fn tst_register_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const UDF_T1_MSK: u16 = 0b1111_1111_0000_0000;
const UDF_T1_VAL: u16 = 0b1101_1110_0000_0000;
fn udf_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const UDF_T2_MSK: u16 = 0b1111_1111_1111_00001111000000000000;
const UDF_T2_VAL: u16 = 0b1111_0111_1111_00001010000000000000;
fn udf_t2(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const UXTB_T1_MSK: u16 = 0b1111_1111_1100_0000;
const UXTB_T1_VAL: u16 = 0b1011_0010_1100_0000;
fn uxtb_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const UXTH_T1_MSK: u16 = 0b1111_1111_1100_0000;
const UXTH_T1_VAL: u16 = 0b1011_0010_1000_0000;
fn uxth_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const WFE_T1_MSK: u16 = 0b1111_1111_1111_1111;
const WFE_T1_VAL: u16 = 0b1011_1111_0010_0000;
fn wfe_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const WFI_T1_MSK: u16 = 0b1111_1111_1111_1111;
const WFI_T1_VAL: u16 = 0b1011_1111_0011_0000;
fn wfi_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}
const YEILD_T1_MSK: u16 = 0b1111_1111_1111_1111;
const YEILD_T1_VAL: u16 = 0b1011_1111_0001_0000;
fn yeild_t1(instruction: u16, cpu: &mut Cpu, memory: &mut Memory) {}

} else if (instruction0 & ADC_REGISTER_T1_MSK) == ADC_REGISTER_T1_VAL {
adc_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & ADD_IMMEDIATE_T1_MSK) == ADD_IMMEDIATE_T1_VAL {
add_immediate_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & ADD_IMMEDIATE_T2_MSK) == ADD_IMMEDIATE_T2_VAL {
add_immediate_t2(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & ADD_REGISTER_T1_MSK) == ADD_REGISTER_T1_VAL {
add_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & ADD_REGISTER_T2_MSK) == ADD_REGISTER_T2_VAL {
add_register_t2(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & ADD_SP_PLUS_IMMEDIATE_T1_MSK) == ADD_SP_PLUS_IMMEDIATE_T1_VAL {
add_sp_plus_immediate_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & ADD_SP_PLUS_IMMEDIATE_T2_MSK) == ADD_SP_PLUS_IMMEDIATE_T2_VAL {
add_sp_plus_immediate_t2(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & ADD_SP_PLUS_REGISTER_T1_MSK) == ADD_SP_PLUS_REGISTER_T1_VAL {
add_sp_plus_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & ADD_SP_PLUS_REGISTER_T2_MSK) == ADD_SP_PLUS_REGISTER_T2_VAL {
add_sp_plus_register_t2(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & ADR_T1_MSK) == ADR_T1_VAL {
adr_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & AND_REGISTER_T1_MSK) == AND_REGISTER_T1_VAL {
and_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & ASR_IMMEDIATE_T1_MSK) == ASR_IMMEDIATE_T1_VAL {
asr_immediate_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & ASR_REGISTER_T1_MSK) == ASR_REGISTER_T1_VAL {
asr_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & B_T1_MSK) == B_T1_VAL {
b_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & B_T2_MSK) == B_T2_VAL {
b_t2(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & BIC_REGISTER_T1_MSK) == BIC_REGISTER_T1_VAL {
bic_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & BKPT_T1_MSK) == BKPT_T1_VAL {
bkpt_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & BL_T1_MSK) == BL_T1_VAL {
bl_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & BLX_REGISTER_T1_MSK) == BLX_REGISTER_T1_VAL {
blx_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & BX_T1_MSK) == BX_T1_VAL {
bx_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & CMN_REGISTER_T1_MSK) == CMN_REGISTER_T1_VAL {
cmn_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & CMP_IMMEDIATE_T1_MSK) == CMP_IMMEDIATE_T1_VAL {
cmp_immediate_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & CMP_REGISTER_T1_MSK) == CMP_REGISTER_T1_VAL {
cmp_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & CMP_REGISTER_T2_MSK) == CMP_REGISTER_T2_VAL {
cmp_register_t2(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & CPS_T1_MSK) == CPS_T1_VAL {
cps_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & DMB_T1_MSK) == DMB_T1_VAL {
dmb_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & DSB_T1_MSK) == DSB_T1_VAL {
dsb_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & EOR_REGISTER_T1_MSK) == EOR_REGISTER_T1_VAL {
eor_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & ISB_T1_MSK) == ISB_T1_VAL {
isb_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & LDM_T1_MSK) == LDM_T1_VAL {
ldm_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & LDR_IMMEDIATE_T1_MSK) == LDR_IMMEDIATE_T1_VAL {
ldr_immediate_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & LDR_IMMEDIATE_T2_MSK) == LDR_IMMEDIATE_T2_VAL {
ldr_immediate_t2(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & LDR_LITERAL_T1_MSK) == LDR_LITERAL_T1_VAL {
ldr_literal_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & LDR_REGISTER_T1_MSK) == LDR_REGISTER_T1_VAL {
ldr_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & LDRB_IMMEDIATE_T1_MSK) == LDRB_IMMEDIATE_T1_VAL {
ldrb_immediate_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & LDRB_REGISTER_T1_MSK) == LDRB_REGISTER_T1_VAL {
ldrb_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & LDRH_IMMEDIATE_T1_MSK) == LDRH_IMMEDIATE_T1_VAL {
ldrh_immediate_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & LDRH_REGISTER_T1_MSK) == LDRH_REGISTER_T1_VAL {
ldrh_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & LDRSB_REGISTER_T1_MSK) == LDRSB_REGISTER_T1_VAL {
ldrsb_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & LDRSH_REGISTER_T1_MSK) == LDRSH_REGISTER_T1_VAL {
ldrsh_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & LSL_IMMEDIATE_T1_MSK) == LSL_IMMEDIATE_T1_VAL {
lsl_immediate_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & LSL_REGISTER_T1_MSK) == LSL_REGISTER_T1_VAL {
lsl_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & LSR_IMMEDIATE_T1_MSK) == LSR_IMMEDIATE_T1_VAL {
lsr_immediate_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & LSR_REGISTER_T1_MSK) == LSR_REGISTER_T1_VAL {
lsr_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & MOV_IMMEDIATE_T1_MSK) == MOV_IMMEDIATE_T1_VAL {
mov_immediate_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & MOV_REGISTER_T1_MSK) == MOV_REGISTER_T1_VAL {
mov_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & MOV_REGISTER_T2_MSK) == MOV_REGISTER_T2_VAL {
mov_register_t2(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & MRS_T1_MSK) == MRS_T1_VAL {
mrs_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & MSR_REGISTER_T1_MSK) == MSR_REGISTER_T1_VAL {
msr_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & MUL_T1_MSK) == MUL_T1_VAL {
mul_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & MVN_REGISTER_T1_MSK) == MVN_REGISTER_T1_VAL {
mvn_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & NOP_T1_MSK) == NOP_T1_VAL {
nop_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & ORR_REGISTER_T1_MSK) == ORR_REGISTER_T1_VAL {
orr_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & POP_T1_MSK) == POP_T1_VAL {
pop_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & PUSH_T1_MSK) == PUSH_T1_VAL {
push_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & REV_T1_MSK) == REV_T1_VAL {
rev_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & REV16_T1_MSK) == REV16_T1_VAL {
rev16_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & REVSH_T1_MSK) == REVSH_T1_VAL {
revsh_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & ROR_REGISTER_T1_MSK) == ROR_REGISTER_T1_VAL {
ror_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & RSB_IMMEDIATE_T1_MSK) == RSB_IMMEDIATE_T1_VAL {
rsb_immediate_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & SBC_REGISTER_T1_MSK) == SBC_REGISTER_T1_VAL {
sbc_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & SEV_T1_MSK) == SEV_T1_VAL {
sev_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & STM_T1_MSK) == STM_T1_VAL {
stm_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & STR_IMMEDIATE_T1_MSK) == STR_IMMEDIATE_T1_VAL {
str_immediate_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & STR_IMMEDIATE_T2_MSK) == STR_IMMEDIATE_T2_VAL {
str_immediate_t2(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & STR_REGISTER_T1_MSK) == STR_REGISTER_T1_VAL {
str_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & STRB_IMMEDIATE_T1_MSK) == STRB_IMMEDIATE_T1_VAL {
strb_immediate_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & STRB_REGISTER_T1_MSK) == STRB_REGISTER_T1_VAL {
strb_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & STRH_IMMEDIATE_T1_MSK) == STRH_IMMEDIATE_T1_VAL {
strh_immediate_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & STRG_REGISTER_T1_MSK) == STRG_REGISTER_T1_VAL {
strg_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & SUB_IMMEDIATE_T1_MSK) == SUB_IMMEDIATE_T1_VAL {
sub_immediate_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & SUB_IMMEDIATE_T2_MSK) == SUB_IMMEDIATE_T2_VAL {
sub_immediate_t2(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & SUB_REGISTER_T1_MSK) == SUB_REGISTER_T1_VAL {
sub_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & SUB_SP_MINUS_IMMEDIATE_T1_MSK) == SUB_SP_MINUS_IMMEDIATE_T1_VAL {
sub_sp_minus_immediate_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & SVC_T1_MSK) == SVC_T1_VAL {
svc_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & SXTB_T1_MSK) == SXTB_T1_VAL {
sxtb_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & SXTH_T1_MSK) == SXTH_T1_VAL {
sxth_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & TST_REGISTER_T1_MSK) == TST_REGISTER_T1_VAL {
tst_register_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & UDF_T1_MSK) == UDF_T1_VAL {
udf_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & UDF_T2_MSK) == UDF_T2_VAL {
udf_t2(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & UXTB_T1_MSK) == UXTB_T1_VAL {
uxtb_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & UXTH_T1_MSK) == UXTH_T1_VAL {
uxth_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & WFE_T1_MSK) == WFE_T1_VAL {
wfe_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & WFI_T1_MSK) == WFI_T1_VAL {
wfi_t1(instruction0, &mut cpu, &mut memory);
} else if (instruction0 & YEILD_T1_MSK) == YEILD_T1_VAL {
yeild_t1(instruction0, &mut cpu, &mut memory);

