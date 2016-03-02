# Setup up chpath shell function for non-interactive/all shells

chpath () {
    . $(chpath-bin -s "$@")
}
