//! MIT License
//!
//! Copyright (c) 2020 Hilmar Gústafsson
//!
//! Permission is hereby granted, free of charge, to any person obtaining a copy
//! of this software and associated documentation files (the "Software"), to deal
//! in the Software without restriction, including without limitation the rights
//! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//! copies of the Software, and to permit persons to whom the Software is
//! furnished to do so, subject to the following conditions:
//!
//! The above copyright notice and this permission notice shall be included in all
//! copies or substantial portions of the Software.
//!
//! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//! SOFTWARE.
#![feature(iterator_fold_self)]

use std::collections::HashSet;

/// A Lattice is a partial order (T, F) which is both a Lower and an Upper Semilattice.
#[derive(Clone, Debug)]
pub struct Lattice<T, F>
where
    F: Fn(&T, &T) -> bool,
{
    set: HashSet<T>,
    relation: F,
}

impl<T, F> Lattice<T, F>
where
    F: Fn(&T, &T) -> bool,
{
    pub fn new(set: HashSet<T>, relation: F) -> Self {
        Self { set, relation }
    }

    fn relation(&self, a: &T, b: &T) -> bool {
        (self.relation)(a, b)
    }
}

/// The Join or Upper Semilattice.
pub trait JoinSemilattice<'a, T> {
    /// Find the least upper bound.
    fn join(&self, a: &'a T, b: &'a T) -> T;
    fn bottom(&self) -> T;
}

/// The Meet or Lower Semilattice.
pub trait MeetSemilattice<'a, T> {
    /// Find the greatest lower bound for the doubleton.
    fn meet(&self, a: &'a T, b: &'a T) -> T;
    fn top(&self) -> T;
}

impl<'a, T, F> JoinSemilattice<'a, T> for Lattice<T, F>
where
    T: Clone,
    F: Fn(&T, &T) -> bool,
{
    fn join(&self, a: &'a T, b: &'a T) -> T {
        self.set
            .iter()
            .filter(|el| self.relation(a, el) && self.relation(b, el))
            .fold_first(|a, b| if self.relation(a, b) { a } else { b })
            .unwrap()
            .clone()
    }

    fn bottom(&self) -> T {
        self.set
            .iter()
            .fold_first(|a, b| if self.relation(a, b) { a } else { b })
            .unwrap()
            .clone()
    }
}

impl<'a, T, F> MeetSemilattice<'a, T> for Lattice<T, F>
where
    T: Clone,
    F: Fn(&T, &T) -> bool,
{
    fn meet(&self, a: &'a T, b: &'a T) -> T {
        self.set
            .iter()
            .filter(|el| self.relation(el, a) && self.relation(el, b))
            .fold_first(|a, b| if self.relation(b, a) { a } else { b })
            .unwrap()
            .clone()
    }

    fn top(&self) -> T {
        self.set
            .iter()
            .fold_first(|a, b| if self.relation(b, a) { a } else { b })
            .unwrap()
            .clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn powerset_lattice() {
        let mut set: HashSet<Vec<i32>> = HashSet::new();
        set.insert(vec![1, 2, 3]);
        set.insert(vec![1, 2]);
        set.insert(vec![1, 3]);
        set.insert(vec![2, 3]);
        set.insert(vec![1]);
        set.insert(vec![2]);
        set.insert(vec![3]);
        set.insert(vec![]);

        let lattice = Lattice::new(set, |a: &Vec<i32>, b: &Vec<i32>| {
            a.iter().all(|el| b.contains(el))
        });

        // For the powerset lattice, join should equal the union operation
        assert_eq!(lattice.join(&vec![1], &vec![2]), vec![1, 2]);
        assert_eq!(lattice.join(&vec![1, 3], &vec![2]), vec![1, 2, 3]);

        // For the powerset lattice, meet should equal the intersection operation
        assert_eq!(lattice.meet(&vec![1], &vec![2]), vec![]);
        assert_eq!(lattice.meet(&vec![1, 2], &vec![2, 3]), vec![2]);

        assert_eq!(lattice.top(), vec![1, 2, 3]);
        assert_eq!(lattice.bottom(), vec![]);
    }

    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Clone, Debug)]
    enum Sign {
        Top,
        Plus,
        Minus,
        EmptySet,
        Bottom,
    }

    #[test]
    fn sign_lattice() {
        let mut set: HashSet<Sign> = HashSet::new();
        set.insert(Sign::Plus);
        set.insert(Sign::Minus);
        set.insert(Sign::EmptySet);
        set.insert(Sign::Top);
        set.insert(Sign::Bottom);

        let lattice = Lattice::new(set, |a: &Sign, b: &Sign| {
            if b == &Sign::Top {
                true
            } else if b == &Sign::Plus || b == &Sign::Minus || b == &Sign::EmptySet {
                if a == b || a == &Sign::Bottom {
                    true
                } else {
                    false
                }
            } else {
                false
            }
        });

        // For the powerset lattice, join should equal the union operation
        assert_eq!(lattice.join(&Sign::Plus, &Sign::Minus), Sign::Top);
        assert_eq!(lattice.join(&Sign::Bottom, &Sign::Plus), Sign::Plus);

        // For the powerset lattice, meet should equal the intersection operation
        assert_eq!(lattice.meet(&Sign::Plus, &Sign::Minus), Sign::Bottom);
        assert_eq!(lattice.meet(&Sign::Top, &Sign::Plus), Sign::Plus);

        assert_eq!(lattice.top(), Sign::Top);
        assert_eq!(lattice.bottom(), Sign::Bottom);
    }
}
