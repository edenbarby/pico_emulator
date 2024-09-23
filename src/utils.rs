pub fn bit_is_set<T, U>(x: T, pos: U) -> bool
where
    T: std::convert::From<u8>
        + std::ops::Shr<U, Output = T>
        + std::ops::BitAnd<T, Output = T>
        + std::cmp::PartialEq<T>
        + std::marker::Copy,
    U:,
{
    let one = T::from(1);
    ((x >> pos) & one) == one
}

pub fn address_is_half_word_aligned<T>(address: T) -> bool
where
    T: std::convert::From<u8> + std::ops::BitAnd<T, Output = T> + std::cmp::PartialEq<T>,
{
    let mask = T::from(0b1);
    let zero = T::from(0);
    address & mask == zero
}

pub fn address_is_word_aligned<T>(address: T) -> bool
where
    T: std::convert::From<u8> + std::ops::BitAnd<T, Output = T> + std::cmp::PartialEq<T>,
{
    let mask = T::from(0b11);
    let zero = T::from(0);
    address & mask == zero
}
