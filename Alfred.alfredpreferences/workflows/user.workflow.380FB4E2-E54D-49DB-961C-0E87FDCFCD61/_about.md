###Overview
Luxinate's default menu contains several powerful options.
![01](http://s29.postimg.org/qb11c1g2f/Screen_Shot_2014_06_28_at_12_51_06_AM.png)<br>
After supplying a URL, you can select whether to download Video, Audio, or Both!
![02](http://s8.postimg.org/hgwngdfwl/image.png)<br>
There is also an action modifier available that allows you to download specific qualities and formats!
![03](http://s8.postimg.org/gjq87i4ed/image.png)<br>
All your download history is saved in case you want to monitor any of your previous downloads.
![04](http://s8.postimg.org/419mqx7et/image.png)<br>
There are several settings available for you to modify Luxinate according to your personal preferences.
![05](http://s8.postimg.org/rkldciwn9/image.png)<br>
Luxinate also supports downloading User's uploads and Playlists from several social sites.
![06](http://s8.postimg.org/r405q38w5/image.png)
![07](http://s8.postimg.org/840nq01j9/image.png)<br><hr>
###Additions
There's been a few things added since the last version that I think you might like.<br><br>
<h4 class="text-primary"><i>Hotkeys</i></h4>
You may remember the browser options in the previous version (_LuxChrome_ & _LuxSafari_). Well those options are gone now! In their stead we've created a hotkey which allows you to have a simple way to grab the frontmost URL from the current browser in view.<br><br>
<strong>Note:</strong> Keep in mind that _Firefox_ is a pain to work with in AppleScript. So we've substituted a small script to grab the URL last loaded from _Firefox_ when needed.
###Simplifications
Luxinate has been completely rewritten. That means we should throw out the broken features while we still can!
<h4 class="text-primary"><i>Removed Options</i></h4>
You may notice a few things missing from the previous versions. Mainly the YouTube and SoundCloud search features (_LuxYouTube_ & _LuxSoundCloud_). The main reason for the disappearance of these two options is the extra baggage they brought along. _LuxYouTube_ required far to many external calls to the YouTube API thus making the workflow slower than it should be. And _LuxSoundCloud_ not only required an entire locally installed API, but also was causing Luxinate to download media slower than it should have been.
###Features
A slew of new features have been added to Luxinate since the last release!<br>Check it out!<br><br>
<h4 class="text-primary">[_LuxePrisimPackageManager_](https://github.com/Ritashugisha/luxeprisim.dependency.pack)</h4>
In order to drastically decrease the download size of Luxinate, _LuxePrisimPackageManager_ has been developed to supply and manage any and all external dependencies required by workflows authored by Ritashugisha.<br><br>
At every runtime of Luxinate, _LuxePrisimPackageManager_ will quickly and silently check if there are missing workflow dependencies. If so, the package manager will grab the missing resources and extract them to their specific locations silently. This results in a 10MB workflow installation being shrunken down to 58KB installation with only a small addition in startup time during initial setup.<br><br>
<h4 class="text-primary">[_GlyphManager_](https://github.com/Ritashugisha/GlyphManager)</h4>
To counter the problem of annoyance concerning Luxinate's glyphs, _GlyphManager_ has been developed and placed at the root of all workflows by Ritashugisha.<br><br>_GlyphManager_ allows freedom for you to use your preferred theme. Instead of being forced to download a specific version of Luxinate to recieve a set of icons that contrast your theme, _GlyphManager_ will now switch between white and black icons automatically to accomadate your theme preference.<br><br>
<h4 class="text-primary">[_Iconic Icons_](https://useiconic.com/open/)</h4>
Along with _GlyphManager_, Luxinate's icons have been updated to a more modern minimal set. Open Iconic has be adapted to become Luxinate's new default.<br><br>
<h4 class="text-primary">[_YouTube-DL_](http://rg3.github.io/youtube-dl/)</h4>
_YouTube-DL_ has now been improved to the point where Luxinate can now support self updates. This allows for the newest downloading features and supported sites to be pushed automatically to your system.<br><br>
<h4 class="text-primary">[_CocoaDialog_](https://github.com/mstratman/cocoadialog)</h4>
In order to create a more user friendly workflow, _CocoaDialog_ has been implemented to spawn interactive GUI dialogs to manage your settings. By default _CocoaDialog_ will install directly to your `/Applications` folder, so to permanently provide you with this wonderful resource.<br><br>
<h4 class="text-primary">[_TerminalNotifier_](https://github.com/alloy/terminal-notifier)</h4>
So as to keep you up to date with when downloads start or finish, _TerminalNotifier_ has been adapted into Luxinate. _TerminalNotifier_ is a CLI based application that can control and spawn MacOSX notifications for your Notification Center. Just like _CocoaDialog_, _TerminalNotifier_ will be installed directly into your `/Applications` folder, to provide this resource for any future workflows that also use this dependency.<hr>