---
title: Scoring the Draft
author: John Kane
date: '2015-09-05'
slug: 
categories:
  - 2015
  - draft
tags: 
  - 2015
  - draft
---

Welcome back to Stats Corner for the 2015 Season!

This place has a new look, a new location on the internet and new ideas.
I will no longer be restricted to message board posts. Bear with me as
the style evolves. Here in Week 1 I want to focus on content and content
only. Future posts should have a better style.

Scoring the Draft
-----------------

Coming out of Draft Day I’m sure most of you were feeling pretty good.
Minus D’Skae of course, he never likes his team. Can we quantify how
well everyone’s draft was? Of course we can. It’s pretty
straightforward: we cull the projections from various sites and join
them up to everyone’s roster. Recall that ESPN’s projections aren’t
great. For perspective, I also looked how CBSsportsline.com and Yahoo!
projected players to perform.

#### ~~Quick~~ notes on methodology

-   I assume you’re playing a QB at OP. God help you if you’re not.
-   I assume head coaches will score you 0 all season (probably not
    true).
-   I’m only scoring your starters: 2 QB’s, 2 RB’s, 3 WR’s, 1 TE, 1 D/ST
    and 1 K.
-   ESPN let me download projections unique to our league’s scoring. CBS
    and Yahoo did not. Therefore interpret their D/ST and K projections
    loosely. Still informative, but probably wrong.
-   I assume no halfback passes. I do assume my \#1 WR does
    [this](https://www.youtube.com/watch?v=bvS9fYHBRvo) again though.
    Just kidding.
-   No return touchdowns count toward individual players.
-   For CBS projections, no rushing yards by a WR.
-   RJ, I’m sorry but your drafting of David Cobb (he plays for the
    Titans everyone, including you RJ) was not counted. He didn’t appear
    in all draft projections so I had to leave him out. ESPN had him at
    26.6 points for the **year** so hopefully he wasn’t going to be your
    RB1. Or RB4.
-   Matovina you drafted Caleb Sturgis. Caleb Sturgis is not currently
    on an NFL roster. So I had to impute your Kicker projections with
    the level one unit below the next-to-worst kicker on the other 11
    teams. Though you won’t see it I named him “Replacement Level.”

### ESPN

Without further adieu, the results from ESPN. I threw in Z-scores
because they’ll be useful later.

    ## Source: local data frame [12 x 3]
    ## 
    ##           owner total_points           z
    ##           (chr)        (dbl)       (dbl)
    ## 1  Skrzyskewski       1775.1  1.34707343
    ## 2        Higdon       1761.7  1.08217260
    ## 3         Regan       1749.5  0.84099423
    ## 4    Harrington       1745.5  0.76191936
    ## 5      Matovina       1739.8  0.64923766
    ## 6         Olson       1723.1  0.31910007
    ## 7       McShane       1709.3  0.04629175
    ## 8          Kane       1690.4 -0.32733703
    ## 9       Hartman       1665.8 -0.81364751
    ## 10    Thieneman       1665.3 -0.82353187
    ## 11        Ready       1631.1 -1.49962204
    ## 12     Shokunbi       1626.9 -1.58265066

Before you go erasing names in the record book for most points in a
season, note how just about every team is projected to set a new record.
These are *probably* overestimates. Still fun though. Poor Ola.

### What about Yahoo! and CBSsportsline.com?

Because ESPN doesn’t have a great track record at making projections,
let’s look at two other sources. It’s not that Yahoo! and
CBSsportsline.com are any better necessarily, but a composite view has
been shown to be the most accurate. Because of things previously
mentioned I’ll use the z-scores for each of the projection systems to
make things an even playing field. The final metric will be the sum of
three z-scores. It’s essentially meaningless but useful for the
rankings.

    ##           owner espn_z yahoo_z  cbs_z total_z
    ## 1      Matovina  0.649   1.748  2.168   4.565
    ## 2  Skrzyskewski  1.347   1.190  0.715   3.252
    ## 3    Harrington  0.762   0.867  0.424   2.053
    ## 4         Regan  0.841   0.334 -0.048   1.127
    ## 5        Higdon  1.082  -0.032 -0.123   0.927
    ## 6         Olson  0.319   0.096  0.329   0.744
    ## 7       McShane  0.046   0.001  0.295   0.342
    ## 8       Hartman -0.814  -0.020  0.415  -0.419
    ## 9          Kane -0.327  -0.150 -0.498  -0.975
    ## 10        Ready -1.500  -0.974 -0.922  -3.396
    ## 11     Shokunbi -1.583  -1.511 -0.969  -4.063
    ## 12    Thieneman -0.824  -1.550 -1.785  -4.159

Well look what we have here! None other than Matovina coming out on top.
In fact, his starters rated out first overall in *both* Yahoo! and
CBSsportsline.com rankings. **And this is with a replacement level
kicker everyone**. Look out for Matovina, may he stay healthy.

### Scoring the Bench Warmers

> “I like depth.” - Brenden Regan

Fair enough Regan, and I suppose I can’t argue with that simple,
declarative sentence from draft night. But you can only play 10 and good
for you if your 4th WR is a stud, you only get points for your top 3.
Hopefully you play the right guy.

It wouldn’t be fair to simply add up bench points. People load up their
rosters in different ways. For example, Ola attempted to draft anyone
with an arm in the draft, whereas others went without a backup QB
(gulp). I’ll take your top backup at each position. I do not consider
kickers (no one has a backup) nor D/ST (only one team drafted a backup.
Good on you, McShane).

If you happened to not draft a backup QB, RB, WR or TE then I award you
no points, and may God have mercy on your soul.

Here they are, the bench rankings!

    ##           owner espn_z  cbs_z yahoo_z total_z
    ## 1         Olson  1.069  1.150   1.058   3.277
    ## 2      Shokunbi  0.907  0.771   0.859   2.537
    ## 3       McShane  0.801  0.866   0.825   2.492
    ## 4    Harrington  0.949  0.579   0.738   2.266
    ## 5       Hartman  0.483  0.753   0.545   1.781
    ## 6          Kane  0.191  0.636   0.272   1.099
    ## 7        Higdon  0.559 -0.130   0.491   0.920
    ## 8         Regan -0.037  0.413   0.248   0.624
    ## 9  Skrzyskewski -0.693 -0.809  -0.650  -2.152
    ## 10        Ready -0.855 -1.034  -0.944  -2.833
    ## 11    Thieneman -1.620 -1.491  -1.703  -4.814
    ## 12     Matovina -1.753 -1.701  -1.740  -5.194

Well now Matovina *really* better stay healhty. His bench is awful.
Thieneman and Ready enjoy that combo of being bottom 3 in starters and
bottom 3 in bench. I say look out for Himmy: third ranked starters and
4th ranked bench.

Good luck to everyone this season, remember this analysis is not your
destiny. And with Ola’s weak starting lineup and strong bench be
expecting maybe a few more trade texts than usual.

