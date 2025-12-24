Interface your file system as a Set rather than as a Tree

> [!CAUTION]
> This is a new application and has not been tested extensively. There may be bugs
> Please report any / all issues you encounter !

## Features

Supported operations are `union` (by default) and `intersection` (`lmhk get tag1 tag2 -n`)

## Examples

An example integration of Lumberhack with `lf` :
```sh
cmd lumberhack_tag ${{
    set -f
    oldIFS=$IFS
    IFS=" "
    declare -a tags=()
    read -a tags
    IFS=$oldIFS
    for f in $fx; do
        lmhk tag "$f" "${tags[@]}"
    done
}}

cmd lumberhack_untag ${{
    set -f
    oldIFS=$IFS
    IFS=" "
    declare -a tags=()
    read -a tags
    IFS=$oldIFS
    for f in $fx; do
        lmhk untag "$f" "${tags[@]}"
    done
}}

map t lumberhack_tag
map T lumberhack_untag
```

## Implementation details

The application writes to `~/.local/share/lumberhack`. Adding a file to Lumberhack creates a hardlink in `lumberhack/store` (used for quickly retrieving a file for the associated inode)

If a file has been deleted from the file system and only exists within Lumberhack, it's interpreted as a "zombie" file. These can be viewed and removed with the corresponding commands (see lmhk --help)
