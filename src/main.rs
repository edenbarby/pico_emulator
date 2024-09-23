mod cortexm0p;
#[allow(dead_code)]
mod regs;
mod utils;

struct ClocksPeripheral {
    registers: std::collections::HashMap<u32, u32>,
    start: u32,
    end: u32,
}

impl ClocksPeripheral {
    const CLK_REF_CTRL: u32 = regs::CLOCKS_BASE + regs::CLOCKS_CLK_REF_CTRL_OFFSET;
    const CLK_REF_SELECTED: u32 = regs::CLOCKS_BASE + regs::CLOCKS_CLK_REF_SELECTED_OFFSET;
    const CLK_SYS_CTRL: u32 = regs::CLOCKS_BASE + regs::CLOCKS_CLK_SYS_CTRL_OFFSET;
    const CLK_SYS_SELECTED: u32 = regs::CLOCKS_BASE + regs::CLOCKS_CLK_SYS_SELECTED_OFFSET;

    pub fn new() -> Self {
        let mut registers = std::collections::HashMap::<u32, u32>::new();

        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_GPOUT0_CTRL_OFFSET,
            0x0000_0000,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_GPOUT0_DIV_OFFSET,
            0x0000_0100,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_GPOUT0_SELECTED_OFFSET,
            0x0000_0001,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_GPOUT1_CTRL_OFFSET,
            0x0000_0000,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_GPOUT1_DIV_OFFSET,
            0x0000_0100,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_GPOUT1_SELECTED_OFFSET,
            0x0000_0001,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_GPOUT2_CTRL_OFFSET,
            0x0000_0000,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_GPOUT2_DIV_OFFSET,
            0x0000_0100,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_GPOUT2_SELECTED_OFFSET,
            0x0000_0001,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_GPOUT3_CTRL_OFFSET,
            0x0000_0000,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_GPOUT3_DIV_OFFSET,
            0x0000_0100,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_GPOUT3_SELECTED_OFFSET,
            0x0000_0001,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_REF_CTRL_OFFSET,
            0x0000_0000,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_REF_DIV_OFFSET,
            0x0000_0100,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_REF_SELECTED_OFFSET,
            0x0000_0001,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_SYS_CTRL_OFFSET,
            0x0000_0000,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_SYS_DIV_OFFSET,
            0x0000_0100,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_SYS_SELECTED_OFFSET,
            0x0000_0001,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_PERI_CTRL_OFFSET,
            0x0000_0000,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_PERI_SELECTED_OFFSET,
            0x0000_0001,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_USB_CTRL_OFFSET,
            0x0000_0000,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_USB_DIV_OFFSET,
            0x0000_0100,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_USB_SELECTED_OFFSET,
            0x0000_0001,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_ADC_CTRL_OFFSET,
            0x0000_0000,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_ADC_DIV_OFFSET,
            0x0000_0100,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_ADC_SELECTED_OFFSET,
            0x0000_0001,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_RTC_CTRL_OFFSET,
            0x0000_0000,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_RTC_DIV_OFFSET,
            0x0000_0100,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_RTC_SELECTED_OFFSET,
            0x0000_0001,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_SYS_RESUS_CTRL_OFFSET,
            0x0000_00FF,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_CLK_SYS_RESUS_STATUS_OFFSET,
            0x0000_0000,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_FC0_REF_KHZ_OFFSET,
            0x0000_0000,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_FC0_MIN_KHZ_OFFSET,
            0x0000_0000,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_FC0_MAX_KHZ_OFFSET,
            0x01FF_FFFF,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_FC0_DELAY_OFFSET,
            0x0000_0001,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_FC0_INTERVAL_OFFSET,
            0x0000_0008,
        );
        registers.insert(regs::CLOCKS_BASE + regs::CLOCKS_FC0_SRC_OFFSET, 0x0000_0000);
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_FC0_STATUS_OFFSET,
            0x0000_0000,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_FC0_RESULT_OFFSET,
            0x0000_0000,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_WAKE_EN0_OFFSET,
            0xFFFF_FFFF,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_WAKE_EN1_OFFSET,
            0x0000_7FFF,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_SLEEP_EN0_OFFSET,
            0xFFFF_FFFF,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_SLEEP_EN1_OFFSET,
            0x0000_7FFF,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_ENABLED0_OFFSET,
            0x0000_0000,
        );
        registers.insert(
            regs::CLOCKS_BASE + regs::CLOCKS_ENABLED1_OFFSET,
            0x0000_0000,
        );
        registers.insert(regs::CLOCKS_BASE + regs::CLOCKS_INTR_OFFSET, 0x0000_0000);
        registers.insert(regs::CLOCKS_BASE + regs::CLOCKS_INTE_OFFSET, 0x0000_0000);
        registers.insert(regs::CLOCKS_BASE + regs::CLOCKS_INTF_OFFSET, 0x0000_0000);
        registers.insert(regs::CLOCKS_BASE + regs::CLOCKS_INTS_OFFSET, 0x0000_0000);

        Self {
            registers,
            start: regs::CLOCKS_BASE,
            end: regs::CLOCKS_BASE + regs::CLOCKS_INTS_OFFSET + 4,
        }
    }
}

impl cortexm0p::memory::MemoryRegion for ClocksPeripheral {
    fn start(&self) -> u32 {
        self.start
    }
    fn end(&self) -> u32 {
        self.end
    }
    fn contains(&self, address_with_alias: u32) -> bool {
        let address = cortexm0p::memory::strip_device_address_alias(address_with_alias);
        self.registers.contains_key(&address)
    }
    fn read_u8(&self, _address_with_alias: u32) -> cortexm0p::memory::MemoryResult<u8> {
        // TODO see 2.1.4 in RP2040 datasheet
        Err(cortexm0p::memory::MemoryError::AccessBadWidth)
    }
    fn read_u16(&self, address_with_alias: u32) -> cortexm0p::memory::MemoryResult<u16> {
        if !utils::address_is_half_word_aligned(address_with_alias) {
            Err(cortexm0p::memory::MemoryError::AccessUnaligned)
        } else {
            Err(cortexm0p::memory::MemoryError::AccessBadWidth)
        }
    }
    fn read_u32(&self, address_with_alias: u32) -> cortexm0p::memory::MemoryResult<u32> {
        if !utils::address_is_word_aligned(address_with_alias) {
            Err(cortexm0p::memory::MemoryError::AccessUnaligned)
        } else {
            let address = cortexm0p::memory::strip_device_address_alias(address_with_alias);
            let value = self.registers[&address];
            println!(
                "ClocksPeripheral read_u32 0x{0:08X} 0x{1:08X}/0b{1:032b}/{1}",
                address, value
            );
            Ok(value)
        }
    }
    fn write_u8(
        &mut self,
        _address_with_alias: u32,
        _value: u8,
    ) -> cortexm0p::memory::MemoryResult<()> {
        Err(cortexm0p::memory::MemoryError::AccessBadWidth)
    }
    fn write_u16(
        &mut self,
        address_with_alias: u32,
        _value: u16,
    ) -> cortexm0p::memory::MemoryResult<()> {
        if !utils::address_is_half_word_aligned(address_with_alias) {
            Err(cortexm0p::memory::MemoryError::AccessUnaligned)
        } else {
            Err(cortexm0p::memory::MemoryError::AccessBadWidth)
        }
    }
    fn write_u32(
        &mut self,
        address_with_alias: u32,
        value: u32,
    ) -> cortexm0p::memory::MemoryResult<()> {
        if !utils::address_is_word_aligned(address_with_alias) {
            Err(cortexm0p::memory::MemoryError::AccessUnaligned)
        } else {
            let address = cortexm0p::memory::strip_device_address_alias(address_with_alias);

            println!(
                "ClocksPeripheral write_u32 0x{0:08X} 0x{1:08X}/0b{1:032b}/{1}",
                address, value
            );

            match address {
                Self::CLK_REF_CTRL => {
                    let register_old = self.registers[&address];
                    let register_new = cortexm0p::memory::modify_device_register(
                        address_with_alias,
                        register_old,
                        value,
                    );
                    self.registers.insert(address, register_new);

                    let glitchless_clk_src = register_new & 0b11;
                    self.registers
                        .insert(Self::CLK_REF_SELECTED, 1 << glitchless_clk_src);
                }
                Self::CLK_SYS_CTRL => {
                    let register_old = self.registers[&address];
                    let register_new = cortexm0p::memory::modify_device_register(
                        address_with_alias,
                        register_old,
                        value,
                    );
                    self.registers.insert(address, register_new);

                    let glitchless_clk_src = register_new & 0b11;
                    self.registers
                        .insert(Self::CLK_SYS_SELECTED, 1 << glitchless_clk_src);
                }
                _ => {
                    //self.registers.insert(address, value);
                }
            }

            Ok(())
        }
    }
}

fn main() {
    let mut memory = cortexm0p::memory::Memory::new();

    const BOOT_ROM_START: u32 = 0x0000_0000;
    const BOOT_ROM_LEN: u32 = 0x0000_4000; // 16KiB
    const FLASH_START: u32 = 0x1000_0000;
    const FLASH_LEN: u32 = 0x0020_0000; // 2MiB
    const SRAM_START: u32 = 0x2000_0000;
    const SRAM_LEN: u32 = 0x0004_2000; // 256KiB + 8KiB
    memory.add_memory_region(Box::new(cortexm0p::memory::Normal::new(
        BOOT_ROM_START,
        BOOT_ROM_LEN,
    )));
    memory.add_memory_region(Box::new(cortexm0p::memory::Normal::new(
        FLASH_START,
        FLASH_LEN,
    )));
    memory.add_memory_region(Box::new(cortexm0p::memory::Normal::new(
        SRAM_START, SRAM_LEN,
    )));

    // Load the bootrom seperately because it isn't part of the compilation output
    // on account of being baked into the chip.
    {
        let path = std::path::PathBuf::from("/home/eden/repo/vm/bootrom.bin");
        let file_data = std::fs::read(path).unwrap();
        for (address_offset, byte) in file_data.into_iter().enumerate() {
            let address = BOOT_ROM_START + (address_offset as u32);
            assert_eq!(
                memory.write_u8(address, byte),
                cortexm0p::memory::MemoryResult::Ok(())
            );
        }
    }

    // Load all loadable segments into memory using their physical addresses.
    // Loadable segments look like the only things we care about (includes .data
    // .text and not .bss) and the physical address appears to map to their
    // locations at startup (.data is in flash at startup, and gets loaded into SRAM
    // during initialisation).
    {
        let path = std::path::PathBuf::from("/home/eden/repo/vm/raveshades.elf");
        let file_data = std::fs::read(path).unwrap();
        let slice = file_data.as_slice();
        let file_as_elf = elf::ElfBytes::<elf::endian::AnyEndian>::minimal_parse(slice).unwrap();
        for segment_header in file_as_elf.segments().unwrap() {
            if segment_header.p_type == 1 {
                let segment_address: usize = segment_header.p_paddr.try_into().unwrap();
                let segment_data = file_as_elf.segment_data(&segment_header).unwrap();

                if !segment_data.is_empty() {
                    for (address_offset, &byte) in segment_data.iter().enumerate() {
                        let address = (segment_address + address_offset) as u32;
                        assert_eq!(
                            memory.write_u8(address, byte),
                            cortexm0p::memory::MemoryResult::Ok(())
                        );
                    }
                }
            }
        }
    }

    memory.add_memory_region(Box::new(cortexm0p::memory::DeviceZeros::new(
        "XIP",
        regs::XIP_CTRL_BASE,
        regs::XIP_CTRL_BASE + regs::XIP_STREAM_FIFO_OFFSET,
    )));
    memory.add_memory_region(Box::new(cortexm0p::memory::DeviceZeros::new(
        "SSI",
        regs::XIP_SSI_BASE,
        regs::XIP_SSI_BASE + regs::SSI_TXD_DRIVE_EDGE_OFFSET,
    )));
    memory.add_memory_region(Box::new(cortexm0p::memory::DeviceZeros::new(
        "APB",
        regs::SYSINFO_BASE,
        regs::CLOCKS_BASE - 4,
    )));
    // Need to emulate system and reference clock selection for the bootrom to
    // continue.
    memory.add_memory_region(Box::new(ClocksPeripheral::new()));
    {
        // The bootrom queries this peripheral to check that PADS_BANK0 is out of reset
        // at startup and will hang if it isn't.
        let mut registers = std::collections::HashMap::<u32, u32>::new();
        // Reset value is 1 for bits <24:0>, but we set them to 0 to represent that all
        // peripherals are not in reset.
        registers.insert(regs::RESETS_BASE + regs::RESETS_RESET_OFFSET, 0x0000_0000);
        // We don't really care about these values at the moment (a value of 1 means
        // that the watchdog will reset the corresponding peripheral when triggered).
        registers.insert(regs::RESETS_BASE + regs::RESETS_WDSEL_OFFSET, 0x0000_0000);
        // Reset value is 0 for bits <24:0>, but we set them to 1 to represent that all
        // peripherals are not in reset.
        registers.insert(
            regs::RESETS_BASE + regs::RESETS_RESET_DONE_OFFSET,
            0xFFFF_FFFF,
        );
        memory.add_memory_region(Box::new(cortexm0p::memory::DeviceFixed::new(
            "Subsystem Reset Controller",
            registers,
        )));
    }
    memory.add_memory_region(Box::new(cortexm0p::memory::DeviceZeros::new(
        "APB",
        regs::PSM_BASE,
        regs::TBMAN_BASE - 4,
    )));
    {
        // Make sure the chip knows it's running on an ASIC not an FPGA, haha.
        let mut registers = std::collections::HashMap::<u32, u32>::new();
        registers.insert(regs::TBMAN_BASE, 0b0000_0000_0000_0000_0000_0000_0000_0001);
        memory.add_memory_region(Box::new(cortexm0p::memory::DeviceFixed::new(
            "Testbench Manager",
            registers,
        )));
    }
    memory.add_memory_region(Box::new(cortexm0p::memory::DeviceZeros::new(
        "DMA",
        regs::DMA_BASE,
        regs::DMA_BASE + regs::DMA_CH11_DBG_TCR_OFFSET,
    )));
    memory.add_memory_region(Box::new(cortexm0p::memory::DeviceZeros::new(
        "USB",
        regs::USBCTRL_BASE,
        regs::USBCTRL_REGS_BASE + regs::USB_INTS_OFFSET,
    )));
    memory.add_memory_region(Box::new(cortexm0p::memory::DeviceZeros::new(
        "PIO0",
        regs::PIO0_BASE,
        regs::PIO0_BASE + regs::PIO_IRQ1_INTS_OFFSET,
    )));
    memory.add_memory_region(Box::new(cortexm0p::memory::DeviceZeros::new(
        "PIO1",
        regs::PIO1_BASE,
        regs::PIO1_BASE + regs::PIO_IRQ1_INTS_OFFSET,
    )));
    memory.add_memory_region(Box::new(cortexm0p::memory::DeviceZeros::new(
        "SIO",
        regs::SIO_BASE,
        regs::SIO_BASE + regs::SIO_SPINLOCK31_OFFSET,
    )));
    memory.add_memory_region(Box::new(cortexm0p::memory::DeviceZeros::new(
        "PPB",
        regs::PPB_BASE,
        regs::PPB_BASE + regs::M0PLUS_MPU_RASR_OFFSET,
    )));

    let mut core0 = cortexm0p::Cpu::new(&memory);

    loop {
        core0.step(&mut memory);
    }
}
