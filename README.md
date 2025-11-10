# 🎵 Melodiλ - YouTube to Audio Telegram Bot

A Haskell-based Telegram bot for converting YouTube videos to audio tracks.

## ✨ Features

- 🎧 Download audio from YouTube
- 🔄 Automatic conversion to MP3
- 📤 Send tracks directly to Telegram
- 🔥 Built with Nix for reproducible builds
- ⚡ Asynchronous request processing
- 🛡️ File size limit (50 MB)
- 🔢 Concurrent download control (max 3)

## 📋 Requirements

- Nix with flakes support
- Telegram account

## 🚀 Quick Start

### 1. Install Nix with Flakes

If you don't have Nix installed:

```bash
# Install Nix
sh <(curl -L https://nixos.org/nix/install) --daemon

# Enable flakes (add to ~/.config/nix/nix.conf or /etc/nix/nix.conf)
experimental-features = nix-command flakes
```

### 2. Create Telegram Bot

1. Open Telegram and find [@BotFather](https://t.me/BotFather)
2. Send command `/newbot`
3. Enter bot name (e.g., `Melodilambda Music Bot`)
4. Enter bot username (must end with `bot`, e.g., `melodilambda_bot`)
5. Get your bot token (looks like `123456789:ABCdefGHIjklMNOpqrsTUVwxyz`)

**Important:** Save the token in a secure place!

### 3. Clone and Setup

```bash
# Clone the repository
git clone <your-repo-url>
cd Melodiλ

# Create .env file
cp .env.example .env
```

Edit `.env` and insert your token:

```bash
TELEGRAM_BOT_TOKEN=123456789:ABCdefGHIjklMNOpqrsTUVwxyz
```

### 4. Run the Bot

```bash
./run.sh
```

That's it! The script will:
- ✅ Enter Nix development environment
- ✅ Download all dependencies (yt-dlp, ffmpeg, GHC, etc.)
- ✅ Build the project with Stack
- ✅ Run the bot

**On any other machine with Nix:**
Just repeat steps 3-4. Everything is reproducible!

## 📱 Usage

1. Find your bot in Telegram by username
2. Press `/start`
3. Send a YouTube video link:
   ```
   https://youtu.be/nOJSmXSFCWk
   ```
4. Wait for conversion
5. Receive the audio file, which you can:
   - Listen directly in Telegram
   - Download to your device
   - Forward to friends

## 🔧 Development

### Development Environment

```bash
# Enter Nix development shell
nix develop

# Inside the shell, you have access to:
# - stack (build tool)
# - ghc (Haskell compiler)
# - haskell-language-server (LSP)
# - hlint (linter)
# - ormolu (formatter)
# - yt-dlp & ffmpeg (runtime deps)

# Build
stack --nix build

# Run
stack --nix exec melodilambda
```

### Project Structure

```
Melodiλ/
├── src/
│   └── Main.hs           # Main bot code
├── downloads/            # Temporary files (auto-created)
├── flake.nix             # Nix flake configuration
├── stack.yaml            # Stack configuration
├── package.yaml          # Haskell dependencies
├── run.sh                # Run script
└── README.md             # Documentation
```

## ⚙️ Configuration

### Environment Variables

| Variable | Description | Required |
|----------|-------------|----------|
| `TELEGRAM_BOT_TOKEN` | Bot token from @BotFather | ✅ |

### Limits

- **Max file size:** 50 MB (Telegram limitation)
- **Concurrent downloads:** 3 simultaneously
- **Audio format:** MP3 (quality: 9/10, where 0 = best)

### Customization

Edit `src/Main.hs`:

```haskell
-- Change concurrent download limit
if count >= 3  -- Change to desired number

-- Change audio quality (0-9, where 0 = best)
"--audio-quality 9"  -- Change to desired value

-- Change format
"--audio-format mp3"  -- Options: mp3, m4a, opus, vorbis, wav
```

After changes, rebuild:
```bash
./run.sh
```

## 🐛 Troubleshooting

### Bot doesn't respond

1. Check token in `.env`
2. Verify bot is running (check terminal output)
3. Test with a simple YouTube link

### Error: "File too large"

The YouTube video is too long. Try:
- A shorter video
- Lower quality settings

### Error: "yt-dlp failed"

1. Check that the link is valid
2. Video might be unavailable or deleted
3. Check your internet connection

### Slow downloads

This is normal for large files. Depends on:
- Internet speed
- Video size
- YouTube server load

## 📚 Tech Stack

- **Language:** Haskell (GHC 9.4)
- **Framework:** telegram-bot-simple
- **Downloader:** yt-dlp
- **Converter:** ffmpeg
- **Build System:** Nix + Stack
- **Package Manager:** Nix Flakes

## 🤝 Contributing

Pull requests are welcome! For major changes, please open an issue first.

## 📄 License

BSD-3-Clause

## 🎯 Roadmap

- [ ] Playlist support
- [ ] Support for other platforms (SoundCloud, Spotify)

## 💡 FAQ

**Q: Can I download playlists?**  
A: Not yet, only individual videos. Feature in development.

**Q: What's the maximum video length?**  
A: Limited by file size (50 MB). Usually ~30-60 minutes depending on quality.

**Q: Does the bot save my requests?**  
A: No, all files are deleted after sending.

**Q: Can I use the bot commercially?**  
A: Yes, but respect content copyright.

## 🌟 Why Nix?

- ✅ **Reproducible builds** - Works the same on any machine
- ✅ **No dependency hell** - All dependencies managed automatically
- ✅ **Isolated environment** - Doesn't pollute your system
- ✅ **Easy deployment** - Just `./run.sh` on any machine with Nix

---

Made with ❤️ and λ
