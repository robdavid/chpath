# Commands to add to a login shell to initially set up paths for a user
chpath () {
    . $(chpath-bin -s "$@")
}
export chpath

if [ -e "$HOME/.chpath" ]
then
    chpath directory "$HOME"
fi
