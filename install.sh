#!/bin/sh
# mcs installer - downloads the prebuilt binary for your platform onto your PATH.
#
#   curl -fsSL https://raw.githubusercontent.com/mmurakaru/mcs/main/install.sh | sh
#
# No OCaml toolchain needed. Environment overrides:
#   MCS_VERSION      release tag to install (default: newest release, incl. pre-releases)
#   MCS_INSTALL_DIR  install directory (default: $HOME/.local/bin)
set -eu

repo="mmurakaru/mcs"
install_dir="${MCS_INSTALL_DIR:-$HOME/.local/bin}"

# --- detect platform -> release asset name ---
os="$(uname -s)"
arch="$(uname -m)"
case "$os" in
  Darwin)
    case "$arch" in
      arm64) target="macos-arm64" ;;
      x86_64) target="macos-x64" ;;
      *) echo "mcs: unsupported macOS architecture: $arch" >&2; exit 1 ;;
    esac ;;
  Linux)
    case "$arch" in
      x86_64 | amd64) target="linux-x64" ;;
      *) echo "mcs: unsupported Linux architecture: $arch" >&2; exit 1 ;;
    esac ;;
  *)
    echo "mcs: unsupported OS '$os' - build from source instead (dune build)" >&2
    exit 1 ;;
esac

# --- resolve the release tag ---
if [ -n "${MCS_VERSION:-}" ]; then
  tag="$MCS_VERSION"
else
  # newest tag in the releases list (includes pre-releases)
  tag="$(curl -fsSL "https://api.github.com/repos/$repo/releases" \
    | grep '"tag_name"' | head -1 | sed -E 's/.*"tag_name"[[:space:]]*:[[:space:]]*"([^"]+)".*/\1/')"
fi
if [ -z "${tag:-}" ]; then
  echo "mcs: could not determine a release version - set MCS_VERSION explicitly" >&2
  exit 1
fi

url="https://github.com/$repo/releases/download/$tag/mcs-$target"
echo "mcs: installing $tag ($target) -> $install_dir/mcs"

tmp="$(mktemp)"
if ! curl -fSL "$url" -o "$tmp"; then
  echo "mcs: download failed - $url" >&2
  rm -f "$tmp"
  exit 1
fi
chmod +x "$tmp"
# strip the macOS quarantine bit so Gatekeeper doesn't block the unsigned binary
if [ "$os" = "Darwin" ]; then
  xattr -d com.apple.quarantine "$tmp" 2>/dev/null || true
fi
mkdir -p "$install_dir"
mv "$tmp" "$install_dir/mcs"

echo "mcs: installed $("$install_dir/mcs" --version 2>/dev/null || echo "$tag")"
case ":$PATH:" in
  *":$install_dir:"*) ;;
  *)
    echo "mcs: note - $install_dir is not on your PATH. Add it, e.g.:"
    echo "  export PATH=\"$install_dir:\$PATH\"" ;;
esac
