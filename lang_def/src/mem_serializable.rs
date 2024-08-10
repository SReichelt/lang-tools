use std::{borrow::Cow, collections::VecDeque, fmt::Debug, ops::Range};

use smallvec::SmallVec;

pub trait MemSerializable<Position> {
    type Serialized: Clone + PartialEq + Debug + 'static;

    fn serialize(&self, relative_to: &Position) -> Self::Serialized;
    fn deserialize(serialized: &Self::Serialized, relative_to: &Position) -> Self;
}

#[macro_export]
macro_rules! impl_mem_serializable_self {
    ($T:tt) => {
        impl<Position> MemSerializable<Position> for $T {
            type Serialized = $T;

            fn serialize(&self, _relative_to: &Position) -> Self::Serialized {
                self.clone()
            }

            fn deserialize(serialized: &Self::Serialized, _relative_to: &Position) -> Self {
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

impl<'a, Position> MemSerializable<Position> for Cow<'a, str> {
    type Serialized = String;

    fn serialize(&self, _relative_to: &Position) -> Self::Serialized {
        self.as_ref().to_owned()
    }

    fn deserialize(serialized: &Self::Serialized, _relative_to: &Position) -> Self {
        Cow::Owned(serialized.clone())
    }
}

macro_rules! impl_mem_serializable_tuple {
    ($($idx:tt $T:ident),+) => {
        impl<Position, $($T: MemSerializable<Position>),+> MemSerializable<Position> for ($($T),+) {
            type Serialized = ($($T::Serialized),+);

            fn serialize(&self, relative_to: &Position) -> Self::Serialized {
                ($(self.$idx.serialize(relative_to)),+)
            }

            fn deserialize(serialized: &Self::Serialized, relative_to: &Position) -> Self {
                ($($T::deserialize(&serialized.$idx, relative_to)),+)
            }
        }
    };
}

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

impl<Position, T: MemSerializable<Position>> MemSerializable<Position> for Option<T> {
    type Serialized = Option<T::Serialized>;

    fn serialize(&self, relative_to: &Position) -> Self::Serialized {
        self.as_ref().map(|obj| obj.serialize(relative_to))
    }

    fn deserialize(serialized: &Self::Serialized, relative_to: &Position) -> Self {
        serialized
            .as_ref()
            .map(|obj| T::deserialize(obj, relative_to))
    }
}

impl<Position, T: MemSerializable<Position>> MemSerializable<Position> for Box<T> {
    type Serialized = Box<T::Serialized>;

    fn serialize(&self, relative_to: &Position) -> Self::Serialized {
        Box::new(self.as_ref().serialize(relative_to))
    }

    fn deserialize(serialized: &Self::Serialized, relative_to: &Position) -> Self {
        Box::new(T::deserialize(serialized, relative_to))
    }
}

impl<Position, T: MemSerializable<Position>> MemSerializable<Position> for Vec<T> {
    type Serialized = Vec<T::Serialized>;

    fn serialize(&self, relative_to: &Position) -> Self::Serialized {
        self.iter()
            .map(|item| item.serialize(relative_to))
            .collect()
    }

    fn deserialize(serialized: &Self::Serialized, relative_to: &Position) -> Self {
        serialized
            .iter()
            .map(|item| T::deserialize(item, relative_to))
            .collect()
    }
}

impl<Position, T: MemSerializable<Position>, const LEN: usize> MemSerializable<Position>
    for SmallVec<[T; LEN]>
{
    type Serialized = SmallVec<[T::Serialized; LEN]>;

    fn serialize(&self, relative_to: &Position) -> Self::Serialized {
        self.iter()
            .map(|item| item.serialize(relative_to))
            .collect()
    }

    fn deserialize(serialized: &Self::Serialized, relative_to: &Position) -> Self {
        serialized
            .iter()
            .map(|item| T::deserialize(item, relative_to))
            .collect()
    }
}

impl<Position, T: MemSerializable<Position>> MemSerializable<Position> for VecDeque<T> {
    type Serialized = SmallVec<[T::Serialized; 1]>;

    fn serialize(&self, relative_to: &Position) -> Self::Serialized {
        self.iter()
            .map(|item| item.serialize(relative_to))
            .collect()
    }

    fn deserialize(serialized: &Self::Serialized, relative_to: &Position) -> Self {
        serialized
            .iter()
            .map(|item| T::deserialize(item, relative_to))
            .collect()
    }
}

impl<Position, T: MemSerializable<Position>> MemSerializable<Position> for Range<T> {
    type Serialized = Range<T::Serialized>;

    fn serialize(&self, relative_to: &Position) -> Self::Serialized {
        self.start.serialize(relative_to)..self.end.serialize(relative_to)
    }

    fn deserialize(serialized: &Self::Serialized, relative_to: &Position) -> Self {
        T::deserialize(&serialized.start, relative_to)..T::deserialize(&serialized.end, relative_to)
    }
}
