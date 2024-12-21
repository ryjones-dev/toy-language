use super::{source_range::SourceRange, Identifier, Type};

/// Represents a member in a struct definition.
///
/// This is almost identical to a [`Variable`], except that the type is required,
/// and they serve different semantic purposes.
#[derive(Debug, Clone, Eq)]
pub(crate) struct StructMember {
    name: Identifier,
    ty: Type,
}

impl StructMember {
    pub(super) fn new(name: Identifier, ty: Type) -> Self {
        Self { name, ty }
    }

    pub(crate) fn name(&self) -> &Identifier {
        &self.name
    }

    pub(crate) fn get_type(&self) -> &Type {
        &self.ty
    }

    pub(crate) fn get_type_mut(&mut self) -> &mut Type {
        &mut self.ty
    }

    pub(crate) fn into_type(self) -> Type {
        self.ty
    }

    /// Returns a [`SourceRange`] from the start of the name to the end of the type.
    pub(crate) fn source(&self) -> SourceRange {
        self.name.source().combine(self.ty.source())
    }

    pub(crate) fn is_discarded(&self) -> bool {
        self.name.is_discarded()
    }
}

// The type should not affect equivalence or hashing
impl PartialEq for StructMember {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl std::hash::Hash for StructMember {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

/// A composite type that is composed of a number of members.
///
/// These members can be any type except for the struct type itself.
/// Members can be accessed with a dot syntax (struct.member).
#[derive(Debug, Clone, Eq)]
pub(crate) struct Struct {
    name: Identifier,
    members: Vec<StructMember>,
    source: SourceRange,
}

impl Struct {
    pub(super) fn new(name: Identifier, members: Vec<StructMember>, source: SourceRange) -> Self {
        Self {
            name,
            members,
            source,
        }
    }

    pub(crate) fn name(&self) -> &Identifier {
        &self.name
    }

    pub(crate) fn members(&self) -> &Vec<StructMember> {
        &self.members
    }

    pub(crate) fn members_mut(&mut self) -> &mut Vec<StructMember> {
        &mut self.members
    }

    pub(crate) fn into_members(self) -> Vec<StructMember> {
        self.members
    }

    pub(crate) fn source(&self) -> SourceRange {
        self.source
    }

    pub(crate) fn is_discarded(&self) -> bool {
        self.name.is_discarded()
    }
}

// The source range should not affect equivalence or hashing.
// Only the name is used for equivalence and hashing.
impl PartialEq for Struct {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl std::hash::Hash for Struct {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl std::fmt::Display for Struct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
