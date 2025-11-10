#!/usr/bin/env bash
set -e

echo "🎵 Запуск Melodiλ..."
echo ""

# Check if .env exists
if [ ! -f .env ]; then
  echo "❌ Файл .env не найден!"
  echo ""
  echo "Создайте файл .env с содержимым:"
  echo "TELEGRAM_BOT_TOKEN=ваш_токен_здесь"
  echo ""
  echo "Или скопируйте из примера:"
  echo "cp .env.example .env"
  exit 1
fi

# Run everything inside nix develop environment
echo "📦 Вход в Nix окружение и сборка проекта..."
nix develop --command bash -c '
  echo "✓ В Nix окружении"
  echo "✓ yt-dlp: $(which yt-dlp || echo "не найден")"
  echo "✓ ffmpeg: $(which ffmpeg || echo "не найден")"
  
  echo ""
  echo "📦 Сборка проекта..."
  stack --nix build
  
  echo ""
  echo "🚀 Запуск бота..."
  # Get the binary path and run it directly with current PATH
  BINARY=$(stack --nix path --local-install-root)/bin/melodilambda
  echo "Запуск: $BINARY"
  exec "$BINARY"
'
