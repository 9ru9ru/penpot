#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Group {
    pub masked: bool,
    // The mask hides by brightness (white shows, black hides) instead of
    // by opacity.
    pub luminance: bool,
}
