use lattice::BinaryRelation;

#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Clone, Debug)]
pub enum Sign {
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

#[cfg(test)]
mod tests {
    use super::*;
    use maplit::hashset;
    use lattice::{JoinSemilattice, Lattice, MeetSemilattice};
    

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