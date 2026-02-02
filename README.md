# 🎵 Melodiλ - YouTube to Audio Telegram Bot

A Haskell-based Telegram bot for converting YouTube videos to audio tracks.

## ✨ Features

- 🎧 Download audio from YouTube with **best quality**
- 🔄 Automatic conversion to MP3 (320kbps)
- 📤 Send tracks directly to Telegram
- 🖼️ **Automatic thumbnail extraction** from YouTube
- 📝 **Video title as audio metadata**
- 🔥 Built with Nix for reproducible builds
- ⚡ Asynchronous request processing
- 🛡️ File size limit (50 MB)
- 🔢 Concurrent download control (max 3)
- 🚀 **Faster downloads** with concurrent fragments

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
## Building for Windows
### Option 1: GitHub Actions (Recommended)
The easiest way to get a Windows executable is to use the provided GitHub Actions workflow.
1. Push your code to GitHub.
2. Go to the **Actions** tab in your repository.
3. Click on the latest **Windows Build** run.
4. Download the `melodilambda-windows-bundle` artifact.
5. Extract the archive. It will contain `melodilambda.exe`, `yt-dlp.exe`, and `ffmpeg.exe` ready to go.

### Option 2: Nix Cross-Compilation (Experimental)
You can try to cross-compile from Linux using Nix, but this may be unstable due to GHC/MinGW mismatches:
```bash
nix build .#windows
```

**Note:** For both methods, you will need to download `yt-dlp.exe` and `ffmpeg.exe` separately and place them in the same directory as the bot executable.

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
- **Audio format:** MP3
- **Audio quality:** 0 (best quality, ~320kbps)
- **Metadata:** Embedded title and thumbnail

### Customization

Edit `src/Main.hs`:

```haskell
-- Change concurrent download limit (line 73)
if count >= 3  -- Change to desired number

-- Change audio quality (line 147)
"--audio-quality 0"  -- 0 = best, 9 = worst

-- Change format (line 146)
"--audio-format mp3"  -- Options: mp3, m4a, opus, vorbis, wav

-- Disable thumbnail embedding (line 148)
-- "--embed-thumbnail"  -- Comment out this line

-- Disable metadata (line 149)
-- "--add-metadata"  -- Comment out this line
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

### Error: "yt-dlp failed"

1. Check that the link is valid
2. Video might be unavailable or deleted
3. Check your internet connection
4. If the error mentions `Failed to resolve` / `Name or service not known`, that's DNS (not yt-dlp): check `cat /etc/resolv.conf` and `getent hosts www.youtube.com` in the same environment where the bot runs
4. If you see `HTTP Error 403: Forbidden` or SABR-related warnings, update `yt-dlp` (it breaks frequently):
   - Nix: run `nix flake update` (or update channels) and rebuild
   - Or set `MELODILAMBDA_YTDLP=/path/to/yt-dlp` to use a newer binary
   - Optional: set `MELODILAMBDA_YTDLP_ARGS="--extractor-args youtube:player_client=android"` to force a more compatible YouTube client

## 📚 Tech Stack

- **Language:** Haskell (GHC 9.4)
- **Framework:** telegram-bot-simple
- **Downloader:** yt-dlp
- **Converter:** ffmpeg
- **Build System:** Nix + Stack
- **Package Manager:** Nix

## 📄 License

MIT License - see [LICENSE](LICENSE) file for details

## 🎯 Roadmap

- [ ] Playlist support
- [ ] Support for other platforms (SoundCloud, Spotify)
- [x] Best audio quality
- [x] Thumbnail extraction
- [x] Video title metadata
- [x] Faster downloads

---

Made with ❤️ and λ
