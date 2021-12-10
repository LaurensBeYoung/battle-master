# Cruel Battle

This is a small game called Cruel Battle.
The main body of the game is written in haskell and uses the bricks framework to make it have a lovely command line UI.
This project was started as a course project for the CSE 230 course at UCSD 2021fall. We wanted to build this project to better understand haskell programming, and to use what we have to design something interesting.

## 1. installation

Installing this little game is very easy. As long as you've installed stack before, you can do this.
```shell
git clone https://github.com/LaurensBeYoung/battle-master/tree/main
cd battle-master
stack install battle
```
After this, the game has been successfully installed on your computer. Now all you need to do is
```shell
stack run battle
```
and you are ready to play the game.


## 2. game rules


This game is a two player game. Player 1 uses the keyboard 'w', 'a', 's', 'd' to control the direction and player 2 uses the keyboard up, down, left and right.
The two players are in competition and only one player can win.
Players have the following three goals.
1. to eat as many money bags as possible
2. avoid being hit by the falling stream of fire from the sky as much as possible
3. never touch the river in the middle of the map. That would be a direct defeat, allowing the other player to win.
   
<br />

Each time a player is hit by a flame, the player's score is reduced by one, and each time a player grabs a money bag, the player's score is increased by one.
If any player is the first to reach 12 points, then he will win and the game is over.
Meanwhile, if any player hits the river, or if his score drops to 0, then he loses immediately, the other player wins, and the game is over.

<br />

Overall, you have to go straight to the goal and you have to be as fast as possible. At the same time, you have to avoid dangers and always keep in mind not to step into the forbidden areas. 

<br />

Have you gotten some insights from playing this game? We hope that real life is not as cruel as the rules of this game.

<br />

But this is exactly <strong>how we usually feel in our life</strong>. Endless competition, endless danger. Never be able to have a rest. Never be a win-win game. Isn't it?

<br />

Let's <strong>play the game, think about this, and end this.</strong>