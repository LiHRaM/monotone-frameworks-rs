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

use std::collections::HashSet;

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
    fn relation(&self, a: &T, b: &T) -> bool {
        (self.relation)(a, b)
    }
}

/// The Join or Upper Semilattice.
/// Every doubleton has a least upper bound.
pub trait JoinSemilattice<'a, T> {
    fn join(&self, a: &'a T, b: &'a T) -> Option<T>;
    fn bottom(&self) -> &'a T;
}

/// The Meet or Lower Semilattice.
/// every doubleton has a greatest lower bound.
pub trait MeetSemilattice<'a, T> {
    fn meet(&self, a: &'a T, b: &'a T) -> Option<T>;
    fn top(&self) -> &'a T;
}

impl<'a, T, F> JoinSemilattice<'a, T> for Lattice<T, F>
where
    T: Clone,
    F: Fn(&T, &T) -> bool,
{
    /// Find the least upper bound for a and b.
    fn join(&self, a: &'a T, b: &'a T) -> Option<T> {
        let bound = self
        .set
        .iter()
        .filter(|el| {
            self.relation(a, el) && 
            self.relation(b, el)
        })
        .fold_first(|a, b| {
            if self.relation(a, b) { a } else { b }
        });
        Some((*bound.unwrap()).clone())
    }

    fn bottom(&self) -> &'a T {
        todo!()
    }
}

impl<'a, T, F> MeetSemilattice<'a, T> for Lattice<T, F>
where
    T: Clone,
    F: Fn(&T, &T) -> bool,
{
    /// Find the greatest lower bound for a and b.
    fn meet(&self, a: &'a T, b: &'a T) -> Option<T> {
        let bound = self
        .set
        .iter()
        .filter(|el| {
            self.relation(el, a) && 
            self.relation(el, b)
        })
        .fold_first(|a, b| {
            if self.relation(b, a) { a } else { b }
        });
        Some((*bound.unwrap()).clone())
    }

    fn top(&self) -> &'a T {
        todo!()
    }
}

#[test]
fn powerset_lattice()
{
    let mut set: HashSet<Vec<i32>> = HashSet::new();
    set.insert(vec![1, 2, 3]);
    set.insert(vec![1, 2]);
    set.insert(vec![1, 3]);
    set.insert(vec![2, 3]);
    set.insert(vec![1]);
    set.insert(vec![2]);
    set.insert(vec![3]);
    set.insert(vec![]);
    let subset_eq = |a: &Vec<i32>, b: &Vec<i32>| {
        a.iter().all(|el| b.contains(el))
    };

    let lattice = Lattice {
        set,
        relation: subset_eq
    };

    // For the powerset lattice, join should equal the union operation
    assert_eq!(lattice.join(&vec![1], &vec![2]), Some(vec![1, 2]));
    assert_eq!(lattice.join(&vec![1, 3], &vec![2]), Some(vec![1, 2, 3]));

    // For the powerset lattice, meet should equal the intersection operation
    assert_eq!(lattice.meet(&vec![1], &vec![2]), Some(vec![]));
    assert_eq!(lattice.meet(&vec![1, 2], &vec![2, 3]), Some(vec![2]));
}