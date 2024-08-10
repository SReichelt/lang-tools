use std::ptr::{eq, NonNull};

/// A helper data structure that logically consists of a pair of mutable references, in a way that
/// does not require a lifetime parameter. Instances of [`TempRefPair`] cannot be created directly
/// but only via [`TempRefPair::with_temp_ref_pair`], which executes a closure that takes a mutable
/// reference to a [`TempRefPair`]. Within the closure, the pair reference can be decomposed into
/// two mutable references, but those cannot outlive the closure.
///
/// The main use case is to pass a combination of two references to a function that only takes a
/// single reference (to a generic type).
pub struct TempRefPair<T1: ?Sized, T2: ?Sized>(NonNull<T1>, NonNull<T2>);

impl<T1: ?Sized, T2: ?Sized> TempRefPair<T1, T2> {
    /// Calls `f` with a mutable reference to a [`TempRefPair`] instance that combines `t1` and
    /// `t2`, and returns the value returned by `f`.
    ///
    /// # Panics
    ///
    /// Panics if `f` modifies the internal state of the object that was passed to it (more
    /// precisely, if `f` returns after modifying the object). As no [`TempRefPair`] method actually
    /// modifies the state, the only way to trigger this panic from safe code is by calling
    /// [`std::mem::swap`] with two [`TempRefPair`] instances.
    pub fn with_temp_ref_pair<R>(t1: &mut T1, t2: &mut T2, f: impl FnOnce(&mut Self) -> R) -> R {
        let t1_ptr = NonNull::from(t1);
        let t2_ptr = NonNull::from(t2);
        let mut temp = TempRefPair(t1_ptr, t2_ptr);
        let result = f(&mut temp);
        if !(eq(temp.0.as_ptr(), t1_ptr.as_ptr()) && eq(temp.1.as_ptr(), t2_ptr.as_ptr())) {
            panic!("TempRefPair instance was modified; this is not allowed because it violates safety guarantees");
        }
        result
    }

    /// Decomposes a shared reference to a [`TempRefPair`] into shared references to its two
    /// components.
    pub fn get_refs(&self) -> (&T1, &T2) {
        // SAFETY: see `get_refs_mut`
        unsafe { (self.0.as_ref(), self.1.as_ref()) }
    }

    /// Decomposes a mutable reference to a [`TempRefPair`] into mutable references to its two
    /// components.
    pub fn get_refs_mut(&mut self) -> (&mut T1, &mut T2) {
        // SAFETY:
        //
        // The only way to construct a `TempRefPair` is via `with_temp_ref_pair`, and the client
        // receives a reference that is only valid within the closure passed to
        // `with_temp_ref_pair`. This guarantees that the two references which were used to
        // construct `self` are still in scope, i.e. from the client's point of view the objects are
        // still mutably borrowed, but within `with_temp_ref_pair` the borrow is no longer used.
        //
        // The easy case is that `self` has not been modified since we created it. Since
        // `TempRefPair` cannot be cloned, we can then safely create new borrows with the lifetime
        // of `self`, as that lifetime will definitely end before the corresponding
        // `with_temp_ref_pair` call returns.
        // However, we cannot prevent the client from swapping two `TempRefPair` instances, which
        // effectively swaps their lifetimes, so that when the inner `with_temp_ref_pair` closure
        // returns, mutable references returned from `get_refs_mut` may still exist. We detect this
        // case and panic before returning from `with_temp_ref_pair`, preventing the situation where
        // they would be aliased by the references that were passed to `with_temp_ref_pair`.
        //
        // Note that detection after the closure returns is sufficient. Within the closure, the
        // standard Rust safety mechanisms ensure that no aliased references can be obtained, even
        // when `TempRefPair` instances are swapped.
        unsafe { (self.0.as_mut(), self.1.as_mut()) }
    }
}

#[cfg(test)]
mod tests {
    use std::mem::swap;

    use super::*;

    #[test]
    fn temp_ref_pair_ok() {
        let mut a = 42;
        let mut b = 23;
        let sum = TempRefPair::with_temp_ref_pair(&mut a, &mut b, |pair| {
            let (a_ref, b_ref) = pair.get_refs_mut();
            assert_eq!(*a_ref, 42);
            assert_eq!(*b_ref, 23);
            *a_ref += 1;
            *b_ref -= 1;
            *a_ref + *b_ref
        });
        assert_eq!(a, 43);
        assert_eq!(b, 22);
        assert_eq!(sum, 43 + 22);
    }

    #[test]
    #[should_panic]
    fn temp_ref_pair_illegal_swap() {
        let mut a = 42;
        let mut b = 23;
        TempRefPair::with_temp_ref_pair(&mut a, &mut b, |pair| {
            let mut a2 = 42;
            let mut b2 = 23;
            TempRefPair::with_temp_ref_pair(&mut a2, &mut b2, |pair2| {
                swap(pair, pair2);
            });
        });
    }
}
