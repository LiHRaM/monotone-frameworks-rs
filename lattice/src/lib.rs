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

use std::{collections::HashSet, fmt::Debug};

/// A Lattice is a partial order (T, F) which is both a Lower and an Upper Semilattice.
pub struct Lattice<T: BinaryRelation> {
    set: HashSet<T>,
}

impl<T: BinaryRelation> Lattice<T> {
    /// Creating a lattice object fails when the set and relational operator fail to form a valid lattice.
    /// In concrete terms, this occurs when any doubleton (x, y) fails to either meet or join.
    pub fn try_new(set: HashSet<T>) -> Option<Self> {
        let lattice = Self { set };
        if lattice.try_all() {
            Some(lattice)
        } else {
            None
        }
    }

    /// For a lattice to be valid, each doubleton must have a meet and a join.
    /// The top and bottom must also be unique, i.e. not "equal" in terms of the relation to anything else.
    /// Ideally, this would be a `const fn` which would error at compile time. Alas.
    fn try_all(&self) -> bool {
        for a in &self.set {
            for b in &self.set {
                if self.try_join(a, b).is_none() {
                    eprintln!("{:?} cannot join {:?}", a, b);
                    return false;
                }
                if self.try_meet(a, b).is_none() {
                    eprintln!("{:?} cannot meet {:?}", a, b);
                    return false;
                }
            }
        }

        let top = self.top();
        let bottom = self.bottom();

        self.set.iter().all(|el| {
            // There is a single top
            (el.relation(&top) && (el == &top || !top.relation(el)))
            // There is a single bottom
                && (bottom.relation(el) && (el == &bottom || !el.relation(&bottom)))
        })
    }

    fn try_join(&self, a: &T, b: &T) -> Option<&T> {
        self.set
            .iter()
            .filter(|el| a.relation(el) && b.relation(el))
            .fold_first(|a, b| if a.relation(b) { a } else { b })
    }

    fn try_meet(&self, a: &T, b: &T) -> Option<&T> {
        self.set
            .iter()
            .filter(|el| el.relation(a) && el.relation(b))
            .fold_first(|a, b| if b.relation(a) { a } else { b })
    }
}

/// The second element of the pair that makes a Lattice.
pub trait BinaryRelation
where
    Self: Debug + Clone + PartialEq,
{
    fn relation(&self, other: &Self) -> bool;
}

/// The Join or Upper Semilattice.
pub trait JoinSemilattice<T: BinaryRelation> {
    /// Find the least upper bound.
    fn join(&self, a: &T, b: &T) -> T;
    fn bottom(&self) -> T;
}

/// The Meet or Lower Semilattice.
pub trait MeetSemilattice<T: BinaryRelation> {
    /// Find the greatest lower bound for the doubleton.
    fn meet(&self, a: &T, b: &T) -> T;
    fn top(&self) -> T;
}

impl<T: BinaryRelation> JoinSemilattice<T> for Lattice<T> {
    fn join(&self, a: &T, b: &T) -> T {
        self.try_join(a, b).unwrap().clone()
    }

    fn bottom(&self) -> T {
        self.set
            .iter()
            .fold_first(|a, b| if a.relation(b) { a } else { b })
            .unwrap()
            .clone()
    }
}

impl<T: BinaryRelation> MeetSemilattice<T> for Lattice<T> {
    fn meet(&self, a: &T, b: &T) -> T {
        self.try_meet(a, b).unwrap().clone()
    }

    fn top(&self) -> T {
        self.set
            .iter()
            .fold_first(|a, b| if b.relation(a) { a } else { b })
            .unwrap()
            .clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use maplit::hashset;

    #[test]
    fn powerset_lattice() {
        let set = hashset! {
            vec![1, 2, 3],
            vec![1, 2],
            vec![1, 3],
            vec![2, 3],
            vec![1],
            vec![2],
            vec![3],
            vec![]
        };

        impl BinaryRelation for Vec<i32> {
            fn relation(&self, other: &Self) -> bool {
                self.iter().all(|el| other.contains(el))
            }
        }

        let lattice = Lattice::try_new(set).unwrap();

        // For the powerset lattice, join should equal the union operation
        assert_eq!(lattice.join(&vec![1], &vec![2]), vec![1, 2]);
        assert_eq!(lattice.join(&vec![1, 3], &vec![2]), vec![1, 2, 3]);

        // For the powerset lattice, meet should equal the intersection operation
        assert_eq!(lattice.meet(&vec![1], &vec![2]), vec![]);
        assert_eq!(lattice.meet(&vec![1, 2], &vec![2, 3]), vec![2]);

        assert_eq!(lattice.top(), vec![1, 2, 3]);
        assert_eq!(lattice.bottom(), vec![]);
    }

    #[test]
    fn two_tops() {
        let set: HashSet<Vec<i32>> = hashset![vec![1, 2, 3], vec![2, 3, 4]];
        assert!(Lattice::try_new(set).is_none());
    }

    #[test]
    fn two_bottoms() {
        let set = hashset![vec![1, 2], vec![1], vec![2]];
        assert!(Lattice::try_new(set).is_none());
    }

    #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Clone, Debug)]
    enum Sign {
        Top,
        Plus,
        Minus,
        EmptySet,
        Bottom,
    }

    impl BinaryRelation for Sign {
        fn relation(&self, other: &Self) -> bool {
            match (self, other) {
                (Sign::Top, Sign::Top) => true,
                (Sign::Top, _) => false,
                (Sign::Plus, Sign::Top) => true,
                (Sign::Plus, Sign::Plus) => true,
                (Sign::Plus, _) => false,
                (Sign::Minus, Sign::Top) => true,
                (Sign::Minus, Sign::Minus) => true,
                (Sign::Minus, _) => false,
                (Sign::EmptySet, Sign::Top) => true,
                (Sign::EmptySet, Sign::EmptySet) => true,
                (Sign::EmptySet, _) => false,
                (Sign::Bottom, _) => true,
            }
        }
    }

    #[test]
    fn sign_lattice() {
        let set = hashset! {
            Sign::Top,
            Sign::Plus,
            Sign::Minus,
            Sign::EmptySet,
            Sign::Bottom
        };

        let lattice = Lattice::try_new(set).unwrap();

        assert_eq!(lattice.join(&Sign::Top, &Sign::Bottom), Sign::Top);
        assert_eq!(lattice.join(&Sign::Plus, &Sign::Minus), Sign::Top);

        assert_eq!(lattice.meet(&Sign::Top, &Sign::Bottom), Sign::Bottom);
        assert_eq!(lattice.meet(&Sign::Plus, &Sign::EmptySet), Sign::Bottom);

        assert_eq!(lattice.top(), Sign::Top);
        assert_eq!(lattice.bottom(), Sign::Bottom);
    }
}
