# Worduel

Worduel is a 1v1 Wordle duel playable from the comfort of discord!
Use the /challenge command to challenge another player to a match.
Once they accept, each player will be DMed a secret word.
Guess your opponent's word to win!

![alt text](https://github.com/rexapex/Worduel-Discord/blob/master/res/readme_screenshot.png?raw=true)

## Setup

1. Create a bot on the Discord developer portal.

2. Upload the png files in the res directory as emojis via the Emojis tab.

3. Copy the emoji IDs into the getGreenSquare, getYellowSquare, and getRedSquare functions in the Game.hs source file.

4. Use stack run to start the bot.

5. Use the /challenge <player> command to challenge someone to a game.
