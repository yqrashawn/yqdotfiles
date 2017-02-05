
## Version 3.1, November 24, 2012

Well look at that, we're back to building our own libcurl again. Partly because it's nice to have control, but mostly because OS X v10.7 Lion's built-in libcurl crashes when checking certificates for FTPS. libcurl 7.28.0 is included pre-built as a dylib.

Michael Robinson added FTP support for deleting directories, and moving items.

## Version 3.0

For explicit SSL/TLS, specify NSURLRequest.curl_shouldVerifySSLCertificate. For implicit FTPS, use the ftps: URL scheme

## Version 2.0, December 10, 2008

Yes, I know it's been 4 years.  But I figured I'd collect the various intel-compatible modifications out there and bring them "home."

This version incorporates some goodies from Jonathan "Wolf" Rentzsch: -setPutFile:resumeUploadFromOffset: and the class ResumableHTTPUploader.

And it's an Xcode project, and builds fat binary.

I have taken out references to building your own CURL, and being 10.1-compatible.

## Version 1.9, 1 April 2004

• Wraps around curl 7.11.1

• Remove setProxy:port: method since CURLHandle uses the system proxy settings

• Retain proxy information retrieved with SCDynamicStoreCopyProxies until the CURLHandle is released.  (Thanks Eric Fung and others for discovery of problems)

• Added multipart post operations. (Thanks Andrew @ Kavasoft, http://www.kavasoft.com/)

• Refactor methods to access the response's HTTP Header as categories on NSString.  New methods to access header information:  headerString to cache and retrieve the entire header as a 7-bit string; headerMatchingKey: and headersMatchingKey:; allHTTPHeaderDicts, etc.

• Add HTTP Reason strings support, which were never implemented before.  Modified Tester to show the new values we support.  (Thanks to Quentin Hill)

• Get proxy information from system preferences for https & ftp as well as http (Thanks to Chris Silverberg)

• Fix HTTP header setting so that it's sent as Key: Value instead of Key:Value (Thanks to Chris Silverberg)

• Put in timeout methods and method to get the version of underlying curl (Thanks to Alan Pinstein)

## Version 1.8, 2 July 2003 (Never Released)

• Changed the runloop mode for threading from NSDefaultRunLoop to kCFRunLoopCommonModes so it will work in a modal dialog box (thanks to Quentin D. Carnicelli)

• Added setAcceptEncoding: and setAcceptCompression: (thanks to David Remahl) to CURLHandle+extras

• formatForHTTPUsingEncoding: now escapes ;:@&=/ characters that are found in the dictionaries; previously it was possible to make an incorrect URL if the keys or values in the dictionaries had characters that are part of the URL syntax.

• formatForHTTPUsingEncoding: now handles properly if the value for a key is a collection, e.g. responds to @selector(objectEnumerator).  (thanks to Doc O'Leary)

• release proxies created with SCDynamicStoreCopyProxies (thanks to Matt Reda)

• Tester:  Added Find panel (thanks to Stephen Steiner).  Note that the tester now contains software copyright Apple Computer.

## Version 1.7, 26 November 2002

• Wraps around curl 7.10.2.

• Add setStringOrNumberObject:forKey: so that you can specify an integer NSNumber as well as an NSString.  Retained setString:forKey: for backward compatibility.

• Adjust project for building under Mac OS X 10.2.  (Project needs to link to a few more libraries).  However, you will need to build curl under 10.1 if you wish your application to run under 10.1; see "DevNotes.txt".

• Added "curl_easy_setopt([self curl], CURLOPT_UPLOAD, 1L);" to setPutFile: to make FTP PUT easier (thanks to Mike Schrag).  Now you'd do something like this:
	NSURL *url = [NSURL URLWithString:@"ftp://username:password@192.168.1.4/remoteFileName.xxx"];
	CURLHandle *urlHandle = (CURLHandle *)[url URLHandleUsingCache:NO];
	[urlHandle setPutFile:@"/localFileName.yyy"];
	[urlHandle prepareAndPerformCurl];

• Added setLowSpeedTime, setLowSpeedLimit methods to deal with timeout issues (thanks to Steven W. Schuldt).

• Got SSL working, as long as you are building curl and CURLHandle under 10.2.  Well, it was working all along, but now you can go to an https: URL in CURLHandleTester.  Please see the notes at <http://curl.haxx.se/lxr/source/UPGRADE> -- by default, curl will complain if it couldn't verify the remote certificate from the default CA cert bundle that gets installed with curl.  You may need to provide your own CA cert bundle using the CURLOPT_CAPATH.  (Presently, there is no convenience method for this defined in CURLHandle.)


## Version 1.6, 5 July 2002

• Wraps around curl 7.9.8.

• Fix a bug in the tester project that failed to copy the framework into the application bundle, forcing the user to install the framework before running the tester.

• Remove Mac OS X 10.0.x support

• Stopped overriding failureReason; this caused problems in some cases.

• Removed carbon APIs, using system configuration framework instead.   This requires a change to curlHelloSignature:acceptAll:.  Thanks to Dominik Westner for this change.

• Add a script in the project for the framework to eliminate the "MH_DYLIB" error message people were getting.

## Version 1.5.2, 29 April 2002

• Fix to cookie handling.  The category method -[NSArray parsedCookies] returns a dictionary with the keys being the cookie names and the value being another dictionary of attributes (including value) of the cookie.  -[CURLHandle setRequestCookies:] take a dictionary of either that form, or a simple { name1 = value1; name2 = value2; } form.

## Version 1.5.1, 25 April 2002

• This will likely be the last version with Mac OS X 10.0 support.  We mean it this time!

• Wraps around curl-7.9.6.

• Significant Browser Cookie support improvement, thanks to Daniel Jalkut.

• dictionaryWithHTTPHeaders: has been replaced by arrayWithHTTPHeaders: to better handle multiple header lines with the same key.

• setHTTPHeaders: now adds to the dictionary of HTTP request headers, rather than replacing them.

• Tester program now works with cookies.

You can try it out if you have a Yahoo! account, for instance.  Check "Attempt to parse cookies", turn off the "Follow" checkbox, and enter the following URL (replacing your own account & password):

http://login.yahoo.com/config/login?login=[yahooAcct]&passwd=[yahooPass]

Copy the "Response Cookies" text and paste into the "Cookie Dict" field.  Then go to a regular Yahoo page URL; you may want to check "Render HTML":

http://quote.yahoo.com/

and verify that it shows your personalized information.

## Version 1.5, 11 April 2002

• Packaged CURLHandle as a standalone framework.  There are now two projects -- CURLHandle.pbproj, which is used for building the framework, and CURLHandleTester.pbproj, which builds the testing application.  

• The process for building an application that uses CURLHandle, and the process for building the framework itself, have changed; see the DevNotes.txt file.

• added CURLHandleCreatedNotification.  Post a notification when a CURLHandle is created.  That way, your app can be notified when a CURLHandle is created behind your back -- this is handy if you want to load and display a Web page that has images....

• added url method to get the NSURL from the CURLHandle

• Updated the DevNotes.txt file significantly to make it easier to build CURLHandleTester

• Added setPostString: in case your post isn't in dictionary form.

• Added a variation of setPostDictionary where you can specify the string encoding for the POST.

• added setAllowsProxy, whether to use the proxy settings or not

• Some leak cleanup (though there seems to be a leak with ports and run loops; this has been reported to Apple)

• Added setRange, setNoBody, setIfModSince to CURLHandle+Extras (thanks to Tom Waters)

• Added PUT abilities (thanks to Tom Waters) -- Here's a code sample; in URLHandleResourceDidFinishLoading you'll get an httpCode of 204 if it was successful.

	- (id)init:(NSString *)url
	{
		NSDate *filemod;
		mUrl = [[NSURL URLWithString:url] retain];
		mLocalPath = [[NSString stringWithFormat:@"%@%@", mDocumentRoot, [mUrl path]] retain];
		mHandle = [(CURLHandle *)[mUrl URLHandleUsingCache:NO] retain];
		[mHandle setUserAgent:@"MyApp/0.1 (Mac OS X)"];
		if (filemod = [[NSFileManager defaultManager] lastModTime:mLocalPath]) {
			[mHandle setIfModSince:filemod];
		}
		[mHandle setPutFile:mLocalPath];
		[mHandle setConnectionTimeout:4];
		[mHandle addClient:self];
		[mHandle loadInBackground];
	}




## Version 1.2, 12 December 2001

•  setProgressIndicator: to allow for indeterminate progress indicators in foreground loading. See the tester application for how to do a progress indicator from a background load.
•  Support for proxy settings from System Preferences, and authenticated proxy support as well.
•  curlHello now takes signature of app as parameter
•  setHTTPHeaders for setting custom HTTP request headers
•  setProxyUserIDAndPassword for proxy servers
•  New method for formatting a dictionary as an HTTP get/post request in a particular order or parameters
•  Rudimentary support for ftp:// URLs
•  Fixed a couple of crashing bugs, one where the data was strangely being released prematurely, another where the background thread would try to send a message to the foreground thread after it had been dealloc'd.
•  Updated bundled libcurl to be in sync with curl 7.9.2.


## Version 1.1, 11 November 2001

•  Fixed a lot of synchronization problems so that background cancelling now works. If the cancel flag has been set, and the foreground thread is messaged of more data (from a pending block from before the cancel), it isn't passed on.
•  Fixed caching; it now works (but be sure to use cachedHandleForURL to actually get the cached data on subsequent loads).
•  Took out the locks, which weren't needed
•  Make sure we don't attempt to kick off a background load if one is already in progress.


## Version 1.0, 14 October 2001

•  curlThreadBackgroundLoad Don't send DONE/BAD after an abort
•  beginLoadInBackground sets status to NSURLHandleLoadInProgress
•  Various debugging tests to make sure mStatus = status
•  initWithURL now sets result from some option calls that were being ignored before
•  prepareAndPerformCurl now clears the header buffer before loading
•  AutoDoc'ing
•  - (void)flushCachedData implemented for 10.0
•  Remove implicit ftp workings; not actually tested at all so I don't know what to expect. It still tries to handle https, though I don't know if that will work.

## Version 0.31, 8 Oct 2001

• Doesn't access NSURLHandle's private _data in Mac OS X 10.0; uses private ivar

## Version 0.3, 7 Oct 2001

• Major cleanup; works on Mac OS X 10.0

## Version 0.2, 2 Oct 2001

• First actual release; works on Mac OS X 10.1 only
