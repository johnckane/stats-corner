---
title: Keeper Analysis
author: John Kane
date: '2015-09-12'
slug: 
categories:
  - 2015
  - draft
  - keepers
tags: 
  - 2015
  - draft
  - keepers
---



#### *Stats Corner* {#stats-corner .author}

There are three types of owners in our league:

1.  The owner who is plotting his keepers before the end of the previous
    regular season (Regan, Ready, Harrington, Olson, D’Skae).

2.  The owner who starts thinking sometime during the summer who he’s
    going to keep (Kane, McShane, Hartman, Higdon).

3.  The owner who decides the morning of the draft who he’s keeping
    (Thieneman, Matovina).

Whoops, I’m sorry four types:

4.  The owner who thinks he’s pulling a fast one on everyone by lying
    about who he’s keeping as if everyone has organized their draft
    strategy around Teddy Bridgewater being off the board, then ends up
    up paying a late fee for switching it after the deadline passes
    (Ola).

I bring this up because I have come to believe that keeper selection is
**the** most important decision you will make as an owner in any given
year. As a preview to the rest of the post consider this: Russell Wilson
cost D’Skae \$22 this year. My estimates for his draft value was
somewhere around \$58. Jeremy Hill cost him \$8, whereas my estimate for
what Jeremy Hill would have gone for was \$73. So D’Skae saved \$101
using his keepers this year. Effectively his draft cash was \$401, not
\$300. What was yours?

Things to keep in mind:

-   I have adjusted keeper values to reflect what they actually cost the
    owner. This means:
    -   Any alterations based on draft cash swapped in trades is not
        applied.
    -   Even if Hartman didn’t properly account for them in the final
        salary selection I will have done so here.
-   As the council-of-draft-attenders can attest, my predictions for
    QBs, especially the top ones, were underestimated. I haven’t (yet)
    corrected my model.
-   These values reflect what I think the players would have been
    drafted for, not what I think they’re actually worth.
-   Estimated draft values don’t take into account the “Desperation
    Factor” that goes into drafting guys, particularly QBs as the last
    of the top tier guys go off the board. How else do you explain Eli
    going for \$60 this year?

I will not give away too much of my methodology apart from saying I used
a [slightly more sophisticated
technique](https://en.wikipedia.org/wiki/Local_regression) than you
would have seen in your stats class.

<div id="best-and-worst-keeper-selections" class="section level2">

Best and Worst Keeper Selections
--------------------------------


#### The Top 10 Best Keeper Selections, Based on Dollars Saved

  Owner          Year   Player             Position   Estimated Cost   Kept Value   Savings
  -------------- ------ ------------------ ---------- ---------------- ------------ ---------
  Shokunbi       2012   Arian Foster       RB         107              16           91
  Hartman        2011   Michael Vick       QB         99               8            91
  Shokunbi       2011   Arian Foster       RB         94               9            85
  Thieneman      2013   Doug Martin        RB         101              17           84
  Shokunbi       2013   Arian Foster       RB         88               23           65
  Skrzyskewski   2015   Jeremy Hill        RB         73               8            65
  Regan          2014   Demaryius Thomas   WR         82               21           61
  Higdon         2012   Julio Jones        WR         82               23           59
  Shokunbi       2015   Eddie Lacy         RB         101              44           57
  Harrington     2012   Cam Newton         QB         64               8            56

Lot’s of Arian Foster in here. Ola certainly rode that \$2 pickup for a
long time. Also, shout out to Mike Vick.

#### The Top 10 Worst Keeper Selections, Based on Dollars Saved

  Owner        Year   Player               Position   Estimated Cost   Kept Value   Savings
  ------------ ------ -------------------- ---------- ---------------- ------------ ---------
  Regan        2013   Josh Freeman         QB         7                26           -19
  Thieneman    2012   Maurice Jones-Drew   RB         68               86           -18
  Higdon       2015   Philip Rivers        QB         32               49           -17
  Matovina     2013   Ben Roethlisberger   QB         14               28           -14
  Matovina     2015   Rob Gronkowski       TE         42               48           -6
  Matovina     2013   Andre Johnson        WR         49               54           -5
  Thieneman    2011   Wes Welker           WR         28               33           -5
  Harrington   2015   Cam Newton           QB         26               29           -3
  Ready        2015   LeSean McCoy         RB         60               63           -3
  Regan        2012   Aaron Rodgers        QB         99               100          -1

Yuck. Josh Freeman. Interesting and important to note that with the
exception of Freeman these picks are bad because the owners kept them at
values close to market rate. It’s an easy way to ensure you retain that
player, but in many cases you could get them for close to what it would
have cost to keep.


Cumulative Player Savings
-------------------------

Especially with the best selections, we see a few players show up time
and again. We can guess at this point which player has resulted in the
most cumulative savings for their owners, but what about the rest of the
field?

#### Highest Cumulative Savings by Player

  Player             Total Savings   Times Kept   \# of Different Owners
  ------------------ --------------- ------------ ------------------------
  Arian Foster       266             4            2
  LeSean McCoy       167             5            1
  Demaryius Thomas   163             4            1
  Julio Jones        152             4            1
  Michael Vick       152             3            1
  Jamaal Charles     150             4            2
  Marshawn Lynch     148             4            2
  Cam Newton         127             4            1
  Eddie Lacy         103             2            1
  Randall Cobb       95              3            1

What I find the most interesting here is that our \#2 cumulative savings
player, LeSean McCoy, also ranks as the 9th worth individual keeper
selection of all time. This year Ready paid \$3 above what he could have
expected to pay on the open market.

#### Lowest Cumulative Savings by Player

  Player               Total Savings   Times Kept   \# of Different Owners
  -------------------- --------------- ------------ ------------------------
  Maurice Jones-Drew   -18             1            1
  Philip Rivers        -17             1            1
  Andre Johnson        -5              1            1
  Wes Welker           1               2            1
  Aaron Rodgers        4               2            1
  Julius Thomas        5               1            1
  Cecil Shorts         6               1            1
  Malcom Floyd         6               1            1
  Matt Cassel          6               1            1
  Steve Smith          6               1            1

What I like about this table is that it has a lot of good players on it.
They just aren’t good keeper selections. It is kind of strange that Josh
Freeman, by far the worst one-time keeper selection of all time, isn’t
in the bottom 10 of cumulative savings. That’s because back in 2011
D’Skae kept him for \$7 when I have him estimated to go for \$44 (a \$37
savings). This means ol’ Josh Freeman has a lifetime cumulative keeper
savings of \$18.

Cumulative Owner Rankings
-------------------------

#### Owners Ranked by Total Savings

  Owner          Cumulative Savings
  -------------- --------------------
  Shokunbi       509
  Skrzyskewski   362
  Ready          301
  Hartman        273
  McShane        270
  Higdon         269
  Kane           235
  Regan          213
  Harrington     211
  Olson          200
  Thieneman      155
  Matovina       97

Ola leads the list here. Fueled by years of keeping Arain Foster and
Eddie Lacy.

Looking at Effective Draft Cash
-------------------------------

As I touched on at the top of this post, D’Skae effectively had \$401 to
work with for this year’s draft. How does that stack up to other guys
this year, or any team from the past four years? To attempt to give some
context for the ultimate performance of these teams I’ve included their
year-end ranking in proportional wins.

#### Most Effective Draft Cash

  Owner          Year   Effective Draft Cash   PW Rank
  -------------- ------ ---------------------- ---------
  Shokunbi       2012   430                    8
  Hartman        2011   426                    6
  Shokunbi       2013   415                    5
  Shokunbi       2011   405                    1
  Skrzyskewski   2015   401                    NA
  Higdon         2012   396                    5
  Thieneman      2013   394                    9
  McShane        2013   393                    8
  Shokunbi       2014   392                    7
  Hartman        2012   390                    2

Note D’Skae coming in that fifth spot. His PW Rank is TBD.

#### Least Effective Draft Cash

  Owner        Year   Effective Draft Cash   PW Rank
  ------------ ------ ---------------------- ---------
  Matovina     2013   281                    12
  Thieneman    2012   288                    6
  Thieneman    2011   295                    11
  Harrington   2015   297                    NA
  Hartman      2015   311                    NA
  Harrington   2011   313                    2
  Higdon       2015   314                    NA
  Matovina     2012   316                    12
  Olson        2012   318                    3
  Higdon       2011   319                    9

Note here how three 2015 teams are on the list. Only time will tell how
they fare.

#### The Relationship between Effective Draft Cash and PW

I included PW Rank in each of the above tables because I thought there
would be some relationship between the two. Neither of those tables seem
to indicate that there is. In fact, the correlation between Effective
Draft Cash and Season Ending PW Rank is rather weak, just about -0.2. So
just having the draft cash is one thing, knowing how to use it something
else entirely…

#### Oh Well. 2015 Effective Draft Cash Rankings

  Owner          Effective Draft Cash
  -------------- ----------------------
  Skrzyskewski   401
  Shokunbi       367
  Regan          360
  Kane           357
  Thieneman      350
  McShane        343
  Olson          330
  Matovina       329
  Ready          320
  Higdon         314
  Hartman        311
  Harrington     297

