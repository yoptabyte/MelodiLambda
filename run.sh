#!/usr/bin/env bash
set -e

echo "🎵 Starting Melodiλ..."
echo ""

# Check if .env exists
if [ ! -f .env ]; then
  echo "❌ .env file not found!"
  echo ""
  echo "Create .env file with:"
  echo "TELEGRAM_BOT_TOKEN=your_token_here"
  echo ""
  echo "Or copy from example:"
  echo "cp .env.example .env"
  exit 1
fi

# Run everything inside nix develop environment
echo "📦 Entering Nix environment and building project..."
nix develop --command bash -c '
  echo "✓ In Nix environment"
  echo "✓ yt-dlp: $(which yt-dlp || echo "not found")"
  echo "✓ ffmpeg: $(which ffmpeg || echo "not found")"
  
  echo ""
  echo "📦 Building project..."
  stack --nix build
  
  echo ""
  echo "🚀 Starting bot..."
  # Get the binary path and run it directly with current PATH
  BINARY=$(stack --nix path --local-install-root)/bin/melodilambda
  echo "Running: $BINARY"
  exec "$BINARY"
'
