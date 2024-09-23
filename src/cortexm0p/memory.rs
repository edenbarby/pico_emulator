use crate::utils::address_is_half_word_aligned;
use crate::utils::address_is_word_aligned;

#[derive(Debug, PartialEq)]
pub enum MemoryError {
    RegionUnaligned,
    RegionOverlapping,
    AccessUnaligned,
    AccessBadAddress,
    AccessBadWidth,
}

pub type MemoryResult<T> = Result<T, MemoryError>;

pub struct Memory {
    memory_regions: Vec<Box<dyn MemoryRegion>>,
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            memory_regions: Vec::new(),
        }
    }
    pub fn add_memory_region(&mut self, new_memory_region: Box<dyn MemoryRegion>) {
        for existing_memory_region in &self.memory_regions {
            assert!(is_disjoint(
                existing_memory_region.as_ref(),
                new_memory_region.as_ref()
            ));
        }

        self.memory_regions.push(new_memory_region);
    }
    fn find(&self, address: u32) -> Option<usize> {
        self.memory_regions
            .iter()
            .position(|mr| mr.contains(address))
    }
    fn read<F, T>(&self, address: u32, func: F) -> MemoryResult<T>
    where
        F: Fn(&Box<dyn MemoryRegion>) -> MemoryResult<T>,
    {
        match self.find(address) {
            Some(idx) => func(&self.memory_regions[idx]),
            None => Err(MemoryError::AccessBadAddress),
        }
    }
    fn write<F>(&mut self, address: u32, func: F) -> MemoryResult<()>
    where
        F: Fn(&mut Box<dyn MemoryRegion>) -> MemoryResult<()>,
    {
        match self.find(address) {
            Some(idx) => func(&mut self.memory_regions[idx]),
            None => Err(MemoryError::AccessBadAddress),
        }
    }
    pub fn read_u8(&self, address: u32) -> MemoryResult<u8> {
        self.read(address, |mr| mr.read_u8(address))
    }
    pub fn read_u16(&self, address: u32) -> MemoryResult<u16> {
        self.read(address, |mr| mr.read_u16(address))
    }
    pub fn read_u32(&self, address: u32) -> MemoryResult<u32> {
        self.read(address, |mr| mr.read_u32(address))
    }
    pub fn write_u8(&mut self, address: u32, value: u8) -> MemoryResult<()> {
        self.write(address, |mr| mr.write_u8(address, value))
    }
    pub fn write_u16(&mut self, address: u32, value: u16) -> MemoryResult<()> {
        self.write(address, |mr| mr.write_u16(address, value))
    }
    pub fn write_u32(&mut self, address: u32, value: u32) -> MemoryResult<()> {
        self.write(address, |mr| mr.write_u32(address, value))
    }
}

fn is_disjoint(lhs: &dyn MemoryRegion, rhs: &dyn MemoryRegion) -> bool {
    (lhs.end() <= rhs.start()) || (rhs.end() <= lhs.start())
}

pub trait MemoryRegion {
    fn start(&self) -> u32;
    fn end(&self) -> u32;
    fn contains(&self, address: u32) -> bool;
    fn read_u8(&self, address: u32) -> MemoryResult<u8>;
    fn read_u16(&self, address: u32) -> MemoryResult<u16>;
    fn read_u32(&self, address: u32) -> MemoryResult<u32>;
    fn write_u8(&mut self, address: u32, value: u8) -> MemoryResult<()>;
    fn write_u16(&mut self, address: u32, value: u16) -> MemoryResult<()>;
    fn write_u32(&mut self, address: u32, value: u32) -> MemoryResult<()>;
}

pub struct Normal {
    start: u32, // location of memory region in "physical" memory
    end: u32,
    buf: Box<[u8]>,
}

impl Normal {
    pub fn new(start: u32, len: u32) -> Self {
        assert!(len > 0);
        assert!(address_is_word_aligned(start));
        assert!(address_is_word_aligned(len));
        Self {
            start,
            end: start + len,
            buf: vec![0; len as usize].into_boxed_slice(),
        }
    }
}

impl MemoryRegion for Normal {
    fn start(&self) -> u32 {
        self.start
    }
    fn end(&self) -> u32 {
        self.end
    }
    fn contains(&self, address: u32) -> bool {
        (self.start <= address) && (address < self.end)
    }
    fn read_u8(&self, address: u32) -> MemoryResult<u8> {
        let offset = (address - self.start) as usize;
        Ok(self.buf[offset])
    }
    fn read_u16(&self, address: u32) -> MemoryResult<u16> {
        match address_is_half_word_aligned(address) {
            true => {
                let start = (address - self.start) as usize;
                let end = start + 2;
                let bytes: &[u8; 2] = self.buf[start..end].try_into().unwrap();
                Ok(u16::from_le_bytes(*bytes))
            }
            false => Err(MemoryError::AccessUnaligned),
        }
    }
    fn read_u32(&self, address: u32) -> MemoryResult<u32> {
        match address_is_word_aligned(address) {
            true => {
                let start = (address - self.start) as usize;
                let end = start + 4;
                let bytes: &[u8; 4] = self.buf[start..end].try_into().unwrap();
                Ok(u32::from_le_bytes(*bytes))
            }
            false => Err(MemoryError::AccessUnaligned),
        }
    }
    fn write_u8(&mut self, address: u32, value: u8) -> MemoryResult<()> {
        let offset = (address - self.start) as usize;
        self.buf[offset] = value;
        Ok(())
    }
    fn write_u16(&mut self, address: u32, value: u16) -> MemoryResult<()> {
        match address_is_half_word_aligned(address) {
            true => {
                let start = (address - self.start) as usize;
                let end = start + 2;
                self.buf[start..end].copy_from_slice(&value.to_le_bytes());
                Ok(())
            }
            false => Err(MemoryError::AccessUnaligned),
        }
    }
    fn write_u32(&mut self, address: u32, value: u32) -> MemoryResult<()> {
        match address_is_word_aligned(address) {
            true => {
                let start = (address - self.start) as usize;
                let end = start + 4;
                self.buf[start..end].copy_from_slice(&value.to_le_bytes());
                Ok(())
            }
            false => Err(MemoryError::AccessUnaligned),
        }
    }
}

// impl core::fmt::Display for Normal {
//     fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
//         write!(
//             f,
//             "Normal 0x{0:0>8X} 0x{1:0>8X} 0x{2:0>8X}/{2}",
//             self.start,
//             self.end,
//             self.buf.len()
//         )
//     }
// }

// pub enum RegisterAccessMode {
//     Normal,
//     XorBitmask,
//     SetBitmask,
//     ClearBitmask,
// }
pub fn strip_device_address_alias(address: u32) -> u32 {
    const DEVICE_ADDRESS_ALIAS_MASK: u32 = !(0b11 << 12);
    address & DEVICE_ADDRESS_ALIAS_MASK
    // (
    //     address & mask,
    //     match (address >> 12) & 0b11 {
    //         0b00 => RegisterAccessMode::Normal,
    //         0b01 => RegisterAccessMode::XorBitmask,
    //         0b10 => RegisterAccessMode::SetBitmask,
    //         _ => RegisterAccessMode::ClearBitmask,
    //     },
    // )
}

pub fn modify_device_register(
    address_with_alias: u32,
    register_value: u32,
    write_value: u32,
) -> u32 {
    let write_mode = (address_with_alias >> 12) & 0b11;
    match write_mode {
        0b00 => {
            // Normal
            write_value
        }
        0b01 => {
            // XOR Bitmask
            register_value ^ write_value
        }
        0b10 => {
            // Set Bitmask
            register_value | write_value
        }
        _ => {
            // Clear Bitmask
            register_value & !write_value
        }
    }
}

pub struct DeviceFixed {
    label: String,
    registers: std::collections::HashMap<u32, u32>,
    start: u32,
    end: u32,
}

impl DeviceFixed {
    pub fn new(label: &str, registers_values: std::collections::HashMap<u32, u32>) -> Self {
        assert!(!registers_values.is_empty());

        let mut start = u32::MAX;
        let mut end = u32::MIN;
        for &k in registers_values.keys() {
            assert!(address_is_word_aligned(k));
            if k < start {
                start = k;
            }
            if k >= end {
                end = k;
            }
        }

        Self {
            label: String::from(label),
            registers: registers_values,
            start,
            end,
        }
    }
}

impl MemoryRegion for DeviceFixed {
    fn start(&self) -> u32 {
        self.start
    }
    fn end(&self) -> u32 {
        self.end
    }
    fn contains(&self, address: u32) -> bool {
        let address_without_alias = strip_device_address_alias(address);
        self.registers.contains_key(&address_without_alias)
    }
    fn read_u8(&self, _address: u32) -> MemoryResult<u8> {
        // TODO see 2.1.4 in RP2040 datasheet
        Err(MemoryError::AccessBadWidth)
    }
    fn read_u16(&self, address: u32) -> MemoryResult<u16> {
        if !address_is_half_word_aligned(address) {
            Err(MemoryError::AccessUnaligned)
        } else {
            Err(MemoryError::AccessBadWidth)
        }
    }
    fn read_u32(&self, address: u32) -> MemoryResult<u32> {
        if !address_is_word_aligned(address) {
            Err(MemoryError::AccessUnaligned)
        } else {
            let address_without_alias = strip_device_address_alias(address);
            let value = self.registers[&address_without_alias];
            println!(
                "DeviceFixed({0}) read_u32 0x{1:08X} 0x{2:08X}/0b{2:032b}/{2}",
                self.label, address_without_alias, value
            );
            Ok(value)
        }
    }
    fn write_u8(&mut self, _address: u32, _value: u8) -> MemoryResult<()> {
        Err(MemoryError::AccessBadWidth)
    }
    fn write_u16(&mut self, address: u32, _value: u16) -> MemoryResult<()> {
        if !address_is_half_word_aligned(address) {
            Err(MemoryError::AccessUnaligned)
        } else {
            Err(MemoryError::AccessBadWidth)
        }
    }
    fn write_u32(&mut self, address: u32, value: u32) -> MemoryResult<()> {
        if !address_is_word_aligned(address) {
            Err(MemoryError::AccessUnaligned)
        } else {
            let address_without_alias = strip_device_address_alias(address);
            println!(
                "DeviceFixed({0}) write_u32 0x{1:08X} 0x{2:08X}/0b{2:032b}/{2}",
                self.label, address_without_alias, value
            );
            Ok(())
        }
    }
}

// impl core::fmt::Display for DeviceFixed {
//     fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
//         let mut reg_addr_sorted = Vec::new();
//         for addr in self.reg_map.keys() {
//             match reg_addr_sorted.binary_search(&addr) {
//                 Ok(_) => {}
//                 Err(idx) => reg_addr_sorted.insert(idx, addr),
//             }
//         }

//         write!(f, "Device")?;
//         for addr in reg_addr_sorted {
//             write!(f, " 0x{0:0>8X}", addr)?;
//         }

//         core::fmt::Result::Ok(())
//     }
// }

pub struct DeviceZeros {
    label: String,
    start: u32,
    end: u32,
}

impl DeviceZeros {
    pub fn new(label: &str, first_register_address: u32, last_register_address: u32) -> Self {
        assert!(address_is_word_aligned(first_register_address));
        assert!(address_is_word_aligned(last_register_address));

        Self {
            label: String::from(label),
            start: first_register_address,
            end: last_register_address + 4,
        }
    }
}

impl MemoryRegion for DeviceZeros {
    fn start(&self) -> u32 {
        self.start
    }
    fn end(&self) -> u32 {
        self.end
    }
    fn contains(&self, address: u32) -> bool {
        let address_without_alias = strip_device_address_alias(address);
        (self.start <= address_without_alias) && (address_without_alias < self.end)
    }
    fn read_u8(&self, _address: u32) -> MemoryResult<u8> {
        Err(MemoryError::AccessBadWidth)
    }
    fn read_u16(&self, address: u32) -> MemoryResult<u16> {
        if !address_is_half_word_aligned(address) {
            Err(MemoryError::AccessUnaligned)
        } else {
            Err(MemoryError::AccessBadWidth)
        }
    }
    fn read_u32(&self, address: u32) -> MemoryResult<u32> {
        if address_is_word_aligned(address) {
            let address_without_alias = strip_device_address_alias(address);
            println!(
                "DeviceZeros({}) read_u32  0x{:08X}",
                self.label, address_without_alias
            );
            Ok(0)
        } else {
            Err(MemoryError::AccessUnaligned)
        }
    }
    fn write_u8(&mut self, _address: u32, _value: u8) -> MemoryResult<()> {
        Err(MemoryError::AccessBadWidth)
    }
    fn write_u16(&mut self, address: u32, _value: u16) -> MemoryResult<()> {
        if !address_is_half_word_aligned(address) {
            Err(MemoryError::AccessUnaligned)
        } else {
            Err(MemoryError::AccessBadWidth)
        }
    }
    fn write_u32(&mut self, address: u32, value: u32) -> MemoryResult<()> {
        if address_is_word_aligned(address) {
            let address_without_alias = strip_device_address_alias(address);
            println!(
                "DeviceZeros({0}) write_u32 0x{1:08X} 0x{2:08X}/0b{2:032b}/{2}",
                self.label, address_without_alias, value
            );
            Ok(())
        } else {
            Err(MemoryError::AccessUnaligned)
        }
    }
}
