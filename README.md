# TVRecommender
TvRecommender lists a bunch of todays prime time TV program from Austria. Based on your favorite actors, it can make recommendations for you.

## Dependencies
First of, I have used some external librarys to make my life easier.
Here is a list of them:
* [TagSoup](http://hackage.haskell.org/package/tagsoup)  - Parsing and extracting information from (possibly malformed) HTML/XML documents
* [HTTP Conduit](http://hackage.haskell.org/package/http-conduit) - HTTP client package with conduit interface and HTTPS support.

## How to use
For all __Linux__ or __macOS__ users:
Simply run this: `./app/main` in terminal

For __Windows__ users: maybe I will find some time to compile it for you... so in meantime you`ll have to do it yourself

One of the most important commands is `help`. Use it and read.

## Features
* List all available prime time movies in Austria
* Show detailed information of each single entry
* make a list of favorite actors
* get recommendations for movies based on your favorite actors
* it can clean your room (not really, but just imagine.. how cool would that be?)

## FAQ
> Heck, why is the program so slow at start?
>> That's because it uses only one core of your cpu to get the content from [Tele.at](https://www.tele.at/) and parsing it. Maybe, and only maybe, I will make improvements regarding this topic.

> Is there a way to get the TV Program of my country?
>> Sure there is, but I didn't take an effort to implement it, neither planning to do it in future.

> I found a bug. What should I do? Is there a bug bounty program?
>> Bug bounty? [insert "We don't do that here" meme]
>>
>> Anyway, feel free to let me know of that bug and pat your shoulder... well done, you did something good for the world

## Contributions
Everyone is welcome to __fork__ my project, __improve__ it and make a pull request. It is up to me whether I merge or not.

## Credits
* to myself, I'm genius :P just kidding, everybody is
* to [Tele.at](https://www.tele.at/) for providing all the information about the TV program
