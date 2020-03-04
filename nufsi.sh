#!/bin/sh

# lowercase package name (nuget is case insensitive, but stores
# using lowercase on disk)
package=$(echo "$1" | tr '[:upper:]' '[:lower:]')


package_dll_paths() {
    find ~/.nuget -path "*${package}*dll"
}

# fsharpi executable comes from Mono, meaning netcoreapp
# is not an option
package_best_framework() {
    grep -E "net[0-9][0-9]|netstandard[0-9].[0-9]" | sort -k 9 -t /
}

package_best_version() {
    sort -V -s -k 7 -t / | tail -n 1
}

package_fsharpi_reference() {
    sed -E 's/^(.*)$/#r "\1" ;;/'
}

package_show_reference() {
    echo "To load this package into F# Interactive, paste this:"
    echo ""
    package_dll_paths \
        | package_best_framework | package_best_version \
        | package_fsharpi_reference
}

package_nuget_install() {
    nuget install "$package"
    echo ""
    echo "Install complete."
}

package_verify_installed() {
    if [ -z "$(package_dll_paths)" ]; then
        echo "Package not installed. Install it now? [y/N]"
        read -r answer
        if [ "$answer" != y ]; then
            return 1
        else
            package_nuget_install
        fi
    fi
}

if package_verify_installed; then
    package_show_reference
fi
