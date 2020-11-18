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

use lattice::{BinaryRelation, JoinSemilattice, Lattice};

pub trait Node<T: BinaryRelation + Clone>
where
    Self: Clone,
{
    fn set_value(&mut self, value: T);
    fn get_value(&self) -> T;
    fn transfer(&self) -> T;
    fn successors(&self) -> Vec<Box<Self>>;
}

pub struct MonotoneFramework;

/// Use the worklist algorithm to traverse the nodes,
impl MonotoneFramework {
    pub fn worklist<N: Node<T>, T: BinaryRelation>(
        lattice: Lattice<T>,
        mut nodes: Vec<N>,
    ) -> Vec<N> {
        let bottom = lattice.bottom();
        for node in nodes.iter_mut() {
            node.set_value(bottom.clone());
        }
        let mut worklist = nodes.to_vec();
        while let Some(node) = worklist.pop() {
            let new_value = node.transfer();
            for mut next in node.successors() {
                let old_value = next.get_value();
                if !new_value.relation(&old_value) {
                    next.set_value(lattice.join(&old_value, &new_value));
                    worklist.push(*next.clone());
                }
            }
        }
        nodes
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lattice::Lattice;
    use lattice_sign::Sign;
    use maplit::hashset;

    #[test]
    fn sign_analysis() {
        let lattice = Lattice::try_new(hashset![
            Sign::Top,
            Sign::Bottom,
            Sign::Plus,
            Sign::Minus,
            Sign::EmptySet,
        ])
        .unwrap();

        todo!();
        // let nodes;
        // let signs = MonotoneFramework::worklist(lattice, nodes);
    }
}
