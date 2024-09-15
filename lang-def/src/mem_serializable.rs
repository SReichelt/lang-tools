use std::{borrow::Cow, collections::VecDeque, fmt::Debug, ops::Range};

use compact_str::CompactString;
use smallvec::SmallVec;

pub use lang_def_derive::MemSerializable;

pub trait MemSerializable<Pos> {
    type Serialized: Clone + PartialEq + Debug + 'static;

    fn serialize(&self, relative_to: &Pos) -> Self::Serialized;
    fn deserialize(serialized: &Self::Serialized, relative_to: &Pos) -> Self;
}

#[macro_export]
macro_rules! impl_mem_serializable_self {
    ($T:ty) => {
        impl<Pos> $crate::mem_serializable::MemSerializable<Pos> for $T {
            type Serialized = $T;

            fn serialize(&self, _relative_to: &Pos) -> Self::Serialized {
                self.clone()
            }

            fn deserialize(serialized: &Self::Serialized, _relative_to: &Pos) -> Self {
                serialized.clone()
            }
        }
    };
}

pub use impl_mem_serializable_self;

impl_mem_serializable_self!(());
impl_mem_serializable_self!(bool);
impl_mem_serializable_self!(u8);
impl_mem_serializable_self!(u16);
impl_mem_serializable_self!(u32);
impl_mem_serializable_self!(u64);
impl_mem_serializable_self!(u128);
impl_mem_serializable_self!(usize);
impl_mem_serializable_self!(i8);
impl_mem_serializable_self!(i16);
impl_mem_serializable_self!(i32);
impl_mem_serializable_self!(i64);
impl_mem_serializable_self!(i128);
impl_mem_serializable_self!(isize);
impl_mem_serializable_self!(char);
impl_mem_serializable_self!(String);
impl_mem_serializable_self!(CompactString);

impl<'a, Pos> MemSerializable<Pos> for Cow<'a, str> {
    type Serialized = CompactString;

    fn serialize(&self, _relative_to: &Pos) -> Self::Serialized {
        CompactString::new(self)
    }

    fn deserialize(serialized: &Self::Serialized, _relative_to: &Pos) -> Self {
        Cow::Owned(serialized.to_string())
    }
}

macro_rules! impl_mem_serializable_tuple {
    ($($idx:tt $T:ident),+) => {
        impl<Pos, $($T: MemSerializable<Pos>,)+> MemSerializable<Pos> for ($($T,)+) {
            type Serialized = ($($T::Serialized,)+);

            fn serialize(&self, relative_to: &Pos) -> Self::Serialized {
                ($(self.$idx.serialize(relative_to),)+)
            }

            fn deserialize(serialized: &Self::Serialized, relative_to: &Pos) -> Self {
                ($($T::deserialize(&serialized.$idx, relative_to),)+)
            }
        }
    };
}

impl_mem_serializable_tuple!(0 T0);
impl_mem_serializable_tuple!(0 T0, 1 T1);
impl_mem_serializable_tuple!(0 T0, 1 T1, 2 T2);
impl_mem_serializable_tuple!(0 T0, 1 T1, 2 T2, 3 T3);
impl_mem_serializable_tuple!(0 T0, 1 T1, 2 T2, 3 T3, 4 T4);
impl_mem_serializable_tuple!(0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5);
impl_mem_serializable_tuple!(0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6);
impl_mem_serializable_tuple!(0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7);
impl_mem_serializable_tuple!(0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8);
impl_mem_serializable_tuple!(0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9);
impl_mem_serializable_tuple!(0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10);
impl_mem_serializable_tuple!(0 T0, 1 T1, 2 T2, 3 T3, 4 T4, 5 T5, 6 T6, 7 T7, 8 T8, 9 T9, 10 T10, 11 T11);

impl<Pos, T: MemSerializable<Pos>> MemSerializable<Pos> for Option<T> {
    type Serialized = Option<T::Serialized>;

    fn serialize(&self, relative_to: &Pos) -> Self::Serialized {
        self.as_ref().map(|obj| obj.serialize(relative_to))
    }

    fn deserialize(serialized: &Self::Serialized, relative_to: &Pos) -> Self {
        serialized
            .as_ref()
            .map(|obj| T::deserialize(obj, relative_to))
    }
}

impl<Pos, T: MemSerializable<Pos>> MemSerializable<Pos> for Box<T> {
    type Serialized = Box<T::Serialized>;

    fn serialize(&self, relative_to: &Pos) -> Self::Serialized {
        Box::new(self.as_ref().serialize(relative_to))
    }

    fn deserialize(serialized: &Self::Serialized, relative_to: &Pos) -> Self {
        Box::new(T::deserialize(serialized, relative_to))
    }
}

impl<Pos, T: MemSerializable<Pos>> MemSerializable<Pos> for Vec<T> {
    type Serialized = Vec<T::Serialized>;

    fn serialize(&self, relative_to: &Pos) -> Self::Serialized {
        self.iter()
            .map(|item| item.serialize(relative_to))
            .collect()
    }

    fn deserialize(serialized: &Self::Serialized, relative_to: &Pos) -> Self {
        serialized
            .iter()
            .map(|item| T::deserialize(item, relative_to))
            .collect()
    }
}

impl<Pos, T: MemSerializable<Pos>, const LEN: usize> MemSerializable<Pos> for SmallVec<[T; LEN]> {
    type Serialized = SmallVec<[T::Serialized; LEN]>;

    fn serialize(&self, relative_to: &Pos) -> Self::Serialized {
        self.iter()
            .map(|item| item.serialize(relative_to))
            .collect()
    }

    fn deserialize(serialized: &Self::Serialized, relative_to: &Pos) -> Self {
        serialized
            .iter()
            .map(|item| T::deserialize(item, relative_to))
            .collect()
    }
}

impl<Pos, T: MemSerializable<Pos>> MemSerializable<Pos> for VecDeque<T> {
    type Serialized = SmallVec<[T::Serialized; 1]>;

    fn serialize(&self, relative_to: &Pos) -> Self::Serialized {
        self.iter()
            .map(|item| item.serialize(relative_to))
            .collect()
    }

    fn deserialize(serialized: &Self::Serialized, relative_to: &Pos) -> Self {
        serialized
            .iter()
            .map(|item| T::deserialize(item, relative_to))
            .collect()
    }
}

impl<Pos, T: MemSerializable<Pos>> MemSerializable<Pos> for Range<T> {
    type Serialized = Range<T::Serialized>;

    fn serialize(&self, relative_to: &Pos) -> Self::Serialized {
        self.start.serialize(relative_to)..self.end.serialize(relative_to)
    }

    fn deserialize(serialized: &Self::Serialized, relative_to: &Pos) -> Self {
        T::deserialize(&serialized.start, relative_to)..T::deserialize(&serialized.end, relative_to)
    }
}
